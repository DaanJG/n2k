# n2k.R: by Daan Gerla

library('magrittr')
library('rgdal')
library('Hmisc')
library('lubridate')
library('raster')
library('plyr')
library('stringr')
library('foreign')
library('xlsx')
library('readxl')
library('tidyr')

#===============================================================================
# Functies e.d.
#===============================================================================

get_variable   <- function(colheader) str_extract(colheader, '(\\b[:alpha:]+)')
get_deelgebied <- function(colheader) as.integer(str_extract(colheader, '(\\d+\\b)'))

colselect_ht0 = c('OBJECTID', 'N2K_NUMMER', 'N2K_GEBIED', 'PERC_TOT', 
                  'LEG_EENV', 'LEG_COMPL')
colselect_ht1 = c('HABTYPE', 'VEGTYPE', 'PERC', 'OPP')

merge_spd <- function(vars, spds) {
    eiv <- Reduce(cbind, spds)
    n <- names(eiv)
    eiv_cols <- n[grepl('EIV', n)]
    other_cols <- n[!grepl('\\.\\d', n)]
    eiv_other <- eiv[other_cols]
    eiv_eiv <- eiv[eiv_cols]
    names(eiv_eiv) <- vars
    eiv <- cbind(eiv_other, eiv_eiv)
    eiv@data[eiv@data == -9999.0] <- NA
    eiv$EIV <- NULL
    eiv
}

read_layers <- function(path, vars=c('GLG', 'GVG', 'pH', 'Trofie')) {
    layers <- ogrListLayers(path)
    layers <- mapply(function(var) layers[str_detect(layers, var)], vars)
    spds <- Map(function(layer) readOGR(path, layer, stringsAsFactors=F), layers)
    merge_spd(vars, spds)
}

weighted.mean.pH <- function(pH, w, na.rm=F) {
    H <- 10^-pH
    H.mean <- weighted.mean(H, w, na.rm=na.rm)
    -log(H.mean, 10)
}

intersect_eiv_hab <- function(eiv, hab) {

    # "polygon" below refers to habitat, i.e. a continous area of the same 
    # habitattype (or of mixed habitattype in some cases). These are the 
    # polygons in the SpatialPolygonsDataFrame "hab".
    
    hab$polygon <- rownames(hab@data)

    names(eiv)[names(eiv) == 'VEGTYPE'] <- 'VEGTYPE_eiv'

    eiv$PERC_TOT <- NULL
    
    # https://gis.stackexchange.com/a/140536/182236:
    #projection(eiv) <- projection(hab)
    intersection <- intersect(hab, eiv)
    intersection$opp <- area(intersection)
    
    intersection
}

reshape_mixed_habtyp <- function(
        df,
        c2=c("HABTYPE", "VEGTYPE", "PERC", "OPP", "ISHD", "KWAL", "OPM"),
        n=10,
        HABTYPE='HABTYPE') {
    c2 <- c(mapply(function(n) paste0(c2, n), 1:n))
    c1 <- names(df)[!(names(df) %in% c2)]
    d1 <- df[c1]     # the columns referring to gebied
    d2 <- df[c(c2)]  # the columns referring to deelgebieden
    f <- function(cols) {
        # Remove numeric suffix from column headers and indicate deelgebied in
        # an extra collumn in stead:
        d <- d2[cols]
        deelgebied <- get_deelgebied(names(d)[-1])[1]
        names(d) <- mapply(get_variable, names(d))
        cbind(d1, deelgebied=deelgebied, d)
        }
    dd <- Map(f, split(c2, mapply(get_deelgebied, c2)))
    deelgebied <- do.call(rbind, dd)
    # We can now remove unused data fields:
    deelgebied <- deelgebied[!is.na(deelgebied[[HABTYPE]]),]
    #deelgebied <- deelgebied[order(deelgebied$OBJECTID, deelgebied$deelgebied),]
    rownames(deelgebied) <- NULL
    deelgebied
}

#trofie2voedselrijkdom <- Vectorize(function(trofie) {
#    if (is.na(trofie)) return(NA_character_)
#    if (trofie < 2) return("zeer voedselarm")
#    if (trofie < 3) return("licht voedselrijk")
#    if (trofie < 4) return("matig voedselrijk-a")
#    if (trofie < 5) return("matig voedselrijk-b")
#    if (trofie < 6) return("zeer voedselrijk")
#    if (trofie < 7) return("uiterst voedselrijk")
#    return("uiterst voedselrijk")
#})

# Bovenstaand is wat ik heb gekregen van een ecoloog bij de provincie
# Overijssel. De classificatie komt echter niet overeen met wat in
# profieldocument voor habitattypen staat.  Ik neem over wat de leeswijzer voor
# de profieldocumenten staat, er van uit gaande dat ITERATIO 'trofie' gelijk
# staat aan productie in ton ds/ha.

trofie2voedselrijkdom <- Vectorize(function(trofie) {
    if (is.na(trofie)) return(NA_character_)
    if (trofie < 1.0)  return("zeer voedselarm")
    if (trofie < 2.5)  return("matig voedselarm")
    if (trofie < 4.5)  return("licht voedselrijk")
    if (trofie < 7.5)  return("matig voedselrijk-a")
    if (trofie < 11.)  return("matig voedselrijk-b")
    if (trofie < 15.)  return("zeer voedselrijk")
    if (trofie >= 15.) return("uiterst voedselrijk")
    #return(NULL)
})

pH2zuurgraad <- Vectorize(function(pH) {
    if (is.na(pH)) return(NA_character_)
    if (pH < 4)   return("zuur-b")
    if (pH < 4.5) return("zuur-a")
    if (pH < 5)   return("matig zuur-b")
    if (pH < 5.5) return("matig zuur-a")
    if (pH < 6)   return("zwak zuur-b")
    if (pH < 6.5) return("zwak zuur-a")
    if (pH < 7)   return("neutraal-b")
    if (pH < 7.5) return("neutraal-a")
    if (pH >= 7.5)return("basisch")
})

GVG2vochttoestand <- Vectorize(function(GVG, GLG=NA, droogteststress=NA) {
    # Tabel B1-2 Leeswijzer Natura 2000 profielendocument
    if (is.na(GVG)) return(NA_character_)
    if (GVG > 50)   return("diep water")
    if (GVG > 20) {
        if (is.na(GLG)) return(NA_character_) 
        if (GLG > 0)  return("ondiep permanent water")
        if (GLG < 0)  return("ondiep droogvallend water")
        if (GLG == 0) return("ondiep droogvallend water")
        #NOTE: GLG == 0 staat niet in tabel
    }
    if (GVG > 5)    return("'s winters inunderend")
    if (GVG > -10)  return("zeer nat")
    if (GVG > -25)  return("nat")
    if (GVG > -40)  return("zeer vochtig")
    # Dieper dan 40 cm:
    #if (is.na(droogteststress)) return(NA_character_)
    if (is.na(droogteststress)) return("vochtig tot droog")
    if (droogteststress <  14) return("vochtig")
    if (droogteststress <= 32) return("matig droog")
    if (droogteststress >  32) return("droog")
})

#abiotische_randvoorwaarden <- read_excel(paste0(root, '/data/abiotische_randvoorwaarden.xlsx'))

read_htkw <- function(xlsx=paste0('/data/BIJ12/HabitattypeKwaliteit.xlsx') {
    sheets <- excel_sheets(xlsx)
    sheets <- sheets[grep('AR', sheets)]
    sheets <- Map(function(sheet) read_excel(xlsx, sheet), sheets)
    nams <- sub('HabitattypeAR', '', names(sheets))
    nams <- sub('Overstromingsto', 'Overstromingstolerantie', nams) # fix typo
    nams <- sub('Voedslerijkdom', 'Voedselrijkdom', nams) # fix typo
    names(sheets) <- nams
    # 'licht voedselarm' is geen voedselrijkdomklasse:
    a <- names(sheets$Voedselrijkdom) == 'licht voedselarm'
    names(sheets$Voedselrijkdom)[a] <- 'licht voedselrijk'
    nams <- tolower(nams)
    reshape_tbl <- function(tbl, Variabele) {
        tbl <- gather(tbl, 'Waarde', 'Code', -(1:3))
        tbl$Variabele <- Variabele
        tbl
    }
    tbls <- Map(reshape_tbl, sheets, nams)
    tbl <- do.call(rbind, tbls)
    tbl$HABTYPE <- sub('_', '', tbl$HabitattypeEUCode)
    tbl$HABTYPE_NAAM <- tbl$HabitatSubTypeKorteNaam
    tbl <- tbl[order(tbl$HABTYPE),]
    tbl$Code <- sub('n.v.t$', 'n.v.t.', tbl$Code) # fix typo
    f <- function(x) {
        if (x == '2')   return('kern')
        if (x == '1,5') return('Kernbereik als het alleen de toplaag betreft')
        if (x == '1')   return('aanvullend')
        if (x == '0')   return('geen')
        return(as.character(NA))
    }
    tbl$Bereik <- Vectorize(f)(tbl$Code)
    #cols <- c(
    #    "HABTYPE",
    #    "HABTYPE_NAAM",
    #    "Variabele",
    #    "Waarde",
    #    "Code",
    #    "Bereik")
    #tbl[cols]
    tbl
}

abiotische_randvoorwaarden <- read_htkw()

bepaal_bereik <- Vectorize(function(habtype, variabele, waarde, tabel=abiotische_randvoorwaarden) {
    if (!habtype %in% tabel$HABTYPE) {
        warning('HABTYPE not in table')
        return(NA_character_)
        }
    if (is.na(waarde))
        return(NA_character_)
    if (waarde == 'vochtig tot droog') {
        # We hebben geen droogstresstrest metingen. Retourneer het meest
        # gunstige bereik:
        s <- tabel$HABTYPE==habtype & tabel$Variabele==variabele & tabel$Waarde %in% c('vochtig', 'matig droog', 'droog')
        Bereik <- tabel[s,]$Bereik
        if ('kern' %in% Bereik) return('kern')
        if ('aanvullend' %in% Bereik) return('aanvullend')
        return('geen')
    }
    s <- tabel$HABTYPE==habtype & tabel$Variabele==variabele & tabel$Waarde==waarde
    tabel[s,]$Bereik
})

ht_list <- abiotische_randvoorwaarden[c('HABTYPE', 'HABTYPE_NAAM')]
ht_list <- ht_list[!duplicated(ht_list),]

get_habveg <- function(eiv, hab, ht=ht_list, ...) {
    # Randvoorwaarden per intersectie van habitattype en vegetatietype:
    intersection <- intersect_eiv_hab(eiv, hab)
    habveg <- reshape_mixed_habtyp(intersection@data, ...)
    if ('HABTYPE' %in% names(list(...)))
        HABTYPE <- list(...)$HABTYPE
    else
        HABTYPE <- 'HABTYPE'
    #habveg$HABTYPE_NAAM <- mapply(function(h) ht$HABTYPE_NAAM[ht$HABTYPE == h], habveg[[HABTYPE]])
    habveg$zuurgraad <- pH2zuurgraad(habveg$pH)
    habveg$vochttoestand <- GVG2vochttoestand(habveg$GVG, habveg$GLG)
    habveg$voedselrijkdom <- trofie2voedselrijkdom(habveg$Trofie)
    habveg
}

get_randvoorwaarden <- function(habveg) {
    # Randvoorwaarden en bereik per habitattype en vegetatietype:
    randvoorwaarden <- list(
        zuurgraad     =ddply(habveg, .(N2K_GEBIED, HABTYPE, VEGTYPE_eiv, zuurgraad),      summarize, jaar=jaar, klasse=zuurgraad,      bereik=bepaal_bereik(HABTYPE, 'zuurgraad',      zuurgraad),      opp=opp),
        vochttoestand =ddply(habveg, .(N2K_GEBIED, HABTYPE, VEGTYPE_eiv, vochttoestand),  summarize, jaar=jaar, klasse=vochttoestand,  bereik=bepaal_bereik(HABTYPE, 'vochttoestand',  vochttoestand),  opp=opp),
        voedselrijkdom=ddply(habveg, .(N2K_GEBIED, HABTYPE, VEGTYPE_eiv, voedselrijkdom), summarize, jaar=jaar, klasse=voedselrijkdom, bereik=bepaal_bereik(HABTYPE, 'voedselrijkdom', voedselrijkdom), opp=opp))
    for (randvoorwaarde in c('zuurgraad', 'vochttoestand', 'voedselrijkdom')) {
        randvoorwaarden[[randvoorwaarde]]$randvoorwaarde <- randvoorwaarde
        randvoorwaarden[[randvoorwaarde]][[randvoorwaarde]] <- NULL
    }
    randvoorwaarden <- do.call(rbind, randvoorwaarden)
    rownames(randvoorwaarden) <- NULL
    randvoorwaarden[c('N2K_GEBIED', 'jaar', 'HABTYPE', 'VEGTYPE_eiv', 'randvoorwaarde', 'klasse', 'bereik', 'opp')]
}

get_randvoorwaarden_ht <- function(randvoorwaarden) {
    # Randvoorwaarden per habitattype:
    randvoorwaarden_ht <- ddply(
        randvoorwaarden, 
        .(N2K_GEBIED, HABTYPE, jaar, randvoorwaarde, bereik), 
        summarize, 
        opp=sum(opp))
    randvoorwaarden_ht
}

summarize_bereik <- function(bereik, opp) {
    # Als meer dan 50% van het oppervlakte binnen het kernbereik valt, dan 
    # valt het hele habitattype daarbinnen en wordt het habitatype toegewezen
    # aan 'kerbereik'. Valt het daarbuiten, maar valt meer dan 50% binnen het
    # aanvullend bereik, dan 'aanvullend'. Anders 'geen'. 
    bereik[is.na(bereik)] <- 'onbekend'
    opp <- opp/sum(opp)
    opp_kern <- sum(opp * (bereik == 'kern'))
    opp_aanv <- sum(opp * (bereik %in% c('kern', 'aanvullend')))
    opp_geen <- sum(opp * (bereik == 'geen'))
    if (opp_kern > 0.5)
        return('kern')
    if (opp_aanv > 0.5)
        return('aanvullend')
    if (opp_geen > 0.5)
        return('geen')
    return(NA_character_)
}    

get_bereik <- function(randvoorwaarden_ht) {
    # Bereik per habitattype en randvoorwaarde:
    ddply(randvoorwaarden_ht, .(N2K_GEBIED, HABTYPE, jaar, randvoorwaarde), summarize, 
          bereik=summarize_bereik(bereik, opp))
}

get_dekking <- function(hab, habveg, ...) {
    habtot <- reshape_mixed_habtyp(hab@data, ...)
    x <- ddply(habveg, .(N2K_GEBIED, HABTYPE), summarize, opp=sum(opp))
    y <- ddply(habtot, .(N2K_GEBIED, HABTYPE), summarize, opp=sum(OPP))
    dekking <- merge(x, y, by=c('N2K_GEBIED', 'HABTYPE'), all=T, suffixes=c('.dek', '.tot'))
    dekking$opp.dek[is.na(dekking$opp.dek)] <- 0.0
    dekking$dekking <- dekking$opp.dek / dekking$opp.tot
    dekking[c('N2K_GEBIED', 'HABTYPE', 'dekking')]
    }

remove_habtypes <- function(hab, habtypes, HABTYPE='HABTYPE') {
    rgx <- paste0(HABTYPE, '[[:digit:]]')
    cols <- names(hab)[grep(rgx, names(hab))]
    for (n in cols) {
        hab[[n]][hab[[n]] %in% habtypes] <- as.character(NA)
    }
    hab
}
