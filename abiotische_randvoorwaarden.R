# abiotische_randvoorwaarden.R: by Daan Gerla, d.gerla@overijssel.nl, daan.gerla@anteagroup.nl, daan@gerla.nl

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

root <- getwd()

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

trofie2voedselrijkdom <- Vectorize(function(trofie) {
    if (is.na(trofie)) return(NA_character_)
    if (trofie < 2) return("zeer voedselarm")
    if (trofie < 3) return("licht voedselrijk")
    if (trofie < 4) return("matig voedselrijk-a")
    if (trofie < 5) return("matig voedselrijk-b")
    if (trofie < 6) return("zeer voedselrijk")
    if (trofie < 7) return("uiterst voedselrijk")
    return("uiterst voedselrijk")
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
    if (GVG > 5)    return("’s winters inunderend")
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

xlsx <- paste0(root, '/data/BIJ12/HabitattypeKwaliteit.xlsx')
sheets <- excel_sheets(xlsx)
sheets <- sheets[grep('AR', sheets)]
sheets <- Map(function(sheet) read_excel(xlsx, sheet), sheets)
nams <- sub('HabitattypeAR', '', names(sheets))
nams <- sub('Overstromingsto', 'Overstromingstolerantie', nams) # fix typo
nams <- sub('Voedslerijkdom', 'Voedselrijkdom', nams) # fix typo
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
    if (x == '1')   return('aavullend')
    if (x == '0')   return('geen')
    return(as.character(NA))
}
tbl$Bereik <- Vectorize(f)(tbl$Code)
cols <- c(
    "HABTYPE",
    "HABTYPE_NAAM",
    "Variabele",
    "Waarde",
    "Code",
    "Bereik")
tbl[cols]

abiotische_randvoorwaarden <- tbl

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

habitattypen <- abiotische_randvoorwaarden[c('HABTYPE', 'HABTYPE_NAAM')]
habitattypen <- habitattypen[!duplicated(habitattypen),]

get_habveg <- function(eiv, hab, ht=habitattypen, ...) {
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
    randvoorwaarden_ht <- ddply(randvoorwaarden, 
        .(N2K_GEBIED, HABTYPE, jaar, randvoorwaarde, bereik), summarize, opp=sum(opp))
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

#===============================================================================
# N2K Habitattypen
#===============================================================================

setwd(root)

habitattypen <- readOGR('data/Habitattypenkaart_Overijssel', stringsAsFactors=F)

#N2K <- ddply(habitattypen, .(N2K_GEBIED), summarize, 
#             #deelgebieden=max(deelgebied), polygons=length(polygon), 
#             oppervlakte=sum(OPP))
#N2K$rapportage <- N2K$N2K_GEBIED %in% c(
rapporteer <- c(
     #"Weerribben",
     #"De Wieden",
     #"Uiterwaarden Zwarte Water en Vecht",
     #"Olde Maten & Veerslootslanden",
     "Vecht en Beneden Reggegebied",
     #"Engbertsdijksvenen",
     #"Boetelerveld",
     "Sallandse Heuvelrug",
     "Wierdense Veld",
     #"Borkeld",
     "Springendal & Dal van de Mosbeek",
     "Bergvennen & Brecklenkampse Veld",
     #"Achter de Voort, Agelerbroek & Voltherbroek",
     #"Lemselermaten",
     #"Dinkelland",
     #"Landgoederen Oldenzaal",
     "Lonnekermeer",
     "Buurserzand & Haaksbergerveen"#,
     #"Witte Veen",
     #"Aamsveen"
     )

#===============================================================================
# Initialiseer
#===============================================================================

EIV <- list()
HAB <- list()
BRK <- list()

##===============================================================================
## Vecht en Beneden Reggegebied
##===============================================================================
#
#N2K_gebied <- "Vecht en Beneden Reggegebied"
#setwd('data/Vecht_en_beneden_Reggegebied/')
#
## Beerzerveld
#
#setwd('Beerzerveld')
#
## Turboveg: 
#opname <- list(abundance   =read.dbf('TurbovegBeerzerveld2015/tvabund.dbf'),
#               habitat_type=read.dbf('TurbovegBeerzerveld2015/tvhabita.dbf'),
#               remarks     =read.dbf('TurbovegBeerzerveld2015/remarks.dbf'),
#               admin       =read.dbf('TurbovegBeerzerveld2015/TvAdmin.dbf'))
#head(opname$habitat_type)
#(type_turboveg <- as.character(opname$habitat_type$VELDTYPE))
#
#jaar <- year(as.Date(opname$habitat_type$DATE, format="%Y%m%d"))
##NOTE: verschillende jaren voor verschillende releves, slechts deel ligt in 
## gevraagde periode.
#
## Iteratio input vegetatietabel gemaakt door Jan-Willem Wolters:
#vegtab <- read.csv('Beerzerveld_ITERATIO_input.csv', sep='\t', stringsAsFactors=F)
#VELDTYPE <- unlist(vegtab[1,])
#names(VELDTYPE) <- NULL
#(type_iteratio <- VELDTYPE[3:(length(VELDTYPE) - 2)])
## Deze Iteratio input file is gemaakt in Turboveg met VELDTYPE als lokaal type.
#
# Vlakken:
#vlakken <- readOGR('.', stringsAsFactors=F)
#head(vlakken)
#any(duplicated((vlakken$OBJECTID_1))) # FALSE
#(type_vlakken <- unique(vlakken$LOKTYP1))
#unique(vlakken$SBB1)
## Het ziet er naar uit dat LOKTYP het beste correspondeert wat er gebruikt is 
## om de Iteratio vegetatie tabel te maken.
#v <- vlakken@data[c('OBJECTID_1', 'LOKTYP1', 'LOKTYP2', 'LOKTYP3', 'PERC1', 'PERC2', 'PERC3')]
#v1 <- v[c('OBJECTID_1', 'LOKTYP1', 'PERC1')]
#v2 <- v[c('OBJECTID_1', 'LOKTYP2', 'PERC2')]
#v3 <- v[c('OBJECTID_1', 'LOKTYP3', 'PERC3')]
#names(v1) <- names(v2) <-  names(v3) <- c('OBJECTID_1', 'Vegtype', 'Perc')
#vegtype <- rbind(v1, v2, v3)
#vegtype <- vegtype[order(vegtype$OBJECTID),]
#vegtype <- vegtype[vegtype$Perc != 0,]
#vegtype <- vegtype[!is.na(vegtype$Vegtype),]
#rownames(vegtype) <- NULL
#head(vegtype, 40)
#
#write.xlsx(vegtype, 'Vegetatievlakken_Beerzerveld.xlsx', row.names=F)
#
#unique(vlakken$JAAR)
## [1] "2017" "2015"
#### Run Iteratio ###
#
#eiv_Beerzerveld <- read_layers('iteratio_output')
#eiv$jaar <- NA_integer_
#
#setwd('..')
#
## Eerde
#
#setwd('Eerde')
#
#vegtab <- read.csv('Eerde_ITERATIO_input.csv', sep='\t', stringsAsFactors=F)
#VELDTYPE <- unlist(vegtab[1,])
#names(VELDTYPE) <- NULL
#(type_iteratio <- VELDTYPE[3:(length(VELDTYPE) - 2)])
#
#vlakken <- readOGR('.', stringsAsFactors=F)
#any(duplicated((vlakken$NUM_AFD))) # FALSE
## We moeten zelf een *numerieke" ID kolom toevoegen:
#vlakken$ID <- as.numeric(rownames(vlakken@data))
#dir.create('vlakken')
#writeOGR(vlakken, 'vlakken', 'vlakken', driver='ESRI Shapefile')
#vegtype <- data.frame(
#    ID=vlakken$ID,
#    Vegtype=vlakken$VEG_TYPE,
#    Perc=100
#    )
#write.xlsx(vegtype, 'vlakken/vlakken.xlsx', row.names=F)
#
#### Run Iteratio ###
#
#eiv_Eerde <- read_layers('iteratio_output')
#
#setwd('..')
#
## Lemelerberg en benenden Regge
#
#setwd('Lemelerberg_en_beneden_Regge')
#
#vegtab <- read.csv('lemelerberg_en_Beneden_Regge_Iteratio2_input.csv', sep='\t', stringsAsFactors=F)
#VELDTYPE <- unlist(vegtab[1,])
#names(VELDTYPE) <- NULL
#(type_iteratio <- VELDTYPE[3:(length(VELDTYPE) - 2)])
#
#vlakken <- readOGR('.', stringsAsFactors=F)
#any(duplicated(vlakken$OBJECTID))
#(type_vlakken <- unique(vlakken$LOKTYP1))
#
#v <- vlakken@data[c('OBJECTID', 'LOKTYP1', 'LOKTYP2', 'LOKTYP3', 'PERC1', 'PERC2', 'PERC3')]
#v1 <- v[c('OBJECTID', 'LOKTYP1', 'PERC1')]
#v2 <- v[c('OBJECTID', 'LOKTYP2', 'PERC2')]
#v3 <- v[c('OBJECTID', 'LOKTYP3', 'PERC3')]
#names(v1) <- names(v2) <-  names(v3) <- c('OBJECTID', 'Vegtype', 'Perc')
#vegtype <- rbind(v1, v2, v3)
#vegtype <- vegtype[order(vegtype$OBJECTID),]
#vegtype <- vegtype[vegtype$Perc != 0,]
#head(vegtype, 45)
#
#write.xlsx(vegtype, 'Vegetatievlakken_Lemelerberg_en_beneden_Regge.xlsx', row.names=F)
#
#### Run Iteratio ###
#
#eiv_LB_en_BR <- read_layers('iteratio_output', c('GLG', 'GVG', 'Indicatiewaarden', 'Trofie'))
#names(eiv_LB_en_BR)[names(eiv_LB_en_BR) == 'Indicatiewaarden'] <- 'pH'
#eiv_LB_en_BR %>% head
#
#setwd('..')
#
## Particuliere landgoederen
#
#setwd('Particuliere_landgoederen')
#
#vlakken <- readOGR('.', 'Vegetatievlakken', stringsAsFactors=F)
#head(vlakken)
#any(duplicated(vlakken$OBJECTID))
#(type_vlakken <- unique(vlakken$lok_typ1))
#
#v <- vlakken@data[c('OBJECTID', 'lok_typ1', 'lok_typ2', 'lok_typ3', 'dekperc1', 'dekperc2', 'dekperc3')]
#v1 <- v[c('OBJECTID', 'lok_typ1', 'dekperc1')]
#v2 <- v[c('OBJECTID', 'lok_typ2', 'dekperc2')]
#v3 <- v[c('OBJECTID', 'lok_typ3', 'dekperc3')]
#names(v1) <- names(v2) <-  names(v3) <- c('OBJECTID', 'Vegtype', 'Perc')
#vegtype <- rbind(v1, v2, v3)
##vegtype <- vegtype[order(vegtype$OBJECTID),]
#vegtype <- vegtype[vegtype$Perc != 0,]
#head(vegtype, 45)
#
#write.xlsx(vegtype, 'Vegetatievlakken.xlsx', row.names=F)
#
#### Run Iteratio ###
#
#eiv_landgoederen <- read_layers('.', 'GLG')
#eiv_landgoederen$GLG # all NA
#eiv_landgoederen$GVG <- NA_real_
#eiv_landgoederen$pH <- NA_real_
#eiv_landgoederen$Trofie <- NA_real_
#
#setwd('..')
#
## Samenvoegen:
#
#eiv <- Reduce(union, c(eiv_Beerzerveld, eiv_Eerde, eiv_LB_en_BR, eiv_landgoederen))
#EIV[[N2K_gebied]] <- eiv
##NOTE: Te veel verschillende jaren:
#EIV[[N2K_gebied]] <- NULL
#
#setwd('..')

#===============================================================================
# Sallandse Heuvelrug
#===============================================================================

setwd(root)

# Natuurmonumenten

N2K_gebied <- 'Sallandse Heuvelrug'
hab <- habitattypen[habitattypen$N2K_GEBIED == N2K_gebied, ]
path <- 'data/abiotische_randvoorwaarden_input/Sallandse_Heuvelrug/Natuurmonumenten'
eiv <- read_layers(path)

eiv$jaar <- 2015

habveg <- get_habveg(eiv, hab)
randvoorwaarden <- get_randvoorwaarden(habveg)
randvoorwaarden_ht <- get_randvoorwaarden_ht(randvoorwaarden)
bereik <- get_bereik(randvoorwaarden_ht)
dekking <- get_dekking(hab, habveg)
bereik <- merge(bereik, dekking)

EIV[[N2K_gebied]] <- eiv
HAB[[N2K_gebied]] <- habveg
BRK[[N2K_gebied]] <- bereik

##===============================================================================
## Wierdense Veld
##===============================================================================
#
#setwd(root)
#setwd('data/abiotische_randvoorwaarden_input/Wierdense_Veld')
#
## Turboveg: 
#opname <- list(abundance   =read.dbf('Turboveg_Wierdense_Veld_2015/tvabund.dbf'),
#               habitat_type=read.dbf('Turboveg_Wierdense_Veld_2015/tvhabita.dbf'),
#               remarks     =read.dbf('Turboveg_Wierdense_Veld_2015/remarks.dbf'),
#               admin       =read.dbf('Turboveg_Wierdense_Veld_2015/TvAdmin.dbf'))
#names(opname$habitat_type)
#(type_turboveg <- as.character(opname$habitat_type$ASSOCIA_01))
#
## Iteratio input vegetatietabel gemaakt door Jan-Willem Wolters:
#vegtab <- read.csv('Wierdense_Veld_Iteratio2_input.csv', sep='\t', stringsAsFactors=F)
#VELDTYPE <- unlist(vegtab[1,])
#names(VELDTYPE) <- NULL
#(type_iteratio <- VELDTYPE[3:(length(VELDTYPE) - 2)])
#
## Vlakken:
#vlakken <- readOGR('.', stringsAsFactors=F)
#head(vlakken)
#any(duplicated((vlakken$OBJECTID))) # FALSE
#(type_vlakken <- unique(vlakken$VVN1))
## Het ziet er naar uit dat VNN1 het beste correspondeert wat er gebruikt is 
## om de Iteratio vegetatie tabel te maken.
#v <- vlakken@data[c('OBJECTID', 'VVN1', 'VVN2', 'VVN3', 'PERC1', 'PERC2', 'PERC3')]
#v1 <- v[c('OBJECTID', 'VVN1', 'PERC1')]
#v2 <- v[c('OBJECTID', 'VVN2', 'PERC2')]
#v3 <- v[c('OBJECTID', 'VVN3', 'PERC3')]
#names(v1) <- names(v2) <-  names(v3) <- c('OBJECTID', 'Vegtype', 'Perc')
#vegtype <- rbind(v1, v2, v3)
#vegtype <- vegtype[order(vegtype$OBJECTID),]
#vegtype <- vegtype[vegtype$Perc != 0,]
#vegtype <- vegtype[!is.na(vegtype$Vegtype),]
#rownames(vegtype) <- NULL
#head(vegtype, 40)
#
#write.xlsx(vegtype, 'Vlakkenkaart_Wierdense_Veld.xlsx', row.names=F)
#
#### Run Iteratio ###
#
##ERROR: -9999

#===============================================================================
# Springendal & Dal van de Mosbeek
#===============================================================================

#NOTE: de twee delen van het gebied hebben verschillende jaren

#===============================================================================
# Bergvennen & Brecklenkampse Veld
#===============================================================================

setwd(root)

N2K_gebied <- 'Bergvennen & Brecklenkampse Veld'
path <- 'data/abiotische_randvoorwaarden_input/Bergvennen_Brecklenkampse_Veld'
gdb_path <- paste0(path, '/Habitattypenkaart.gdb')
hab <- readOGR(gdb_path)
hab$N2K_GEBIED <- N2K_gebied
for (n in names(hab)[grep('Htype[[:digit:]]', names(hab))]) {
    hab[[n]][grep('^$', hab[[n]])] <- as.character(NA)
}
    
eiv <- read_layers(path)

eiv$jaar <- NA_integer_

plot(hab)
plot(eiv, add=T, col='red')

c2 <- c('VVN', 'PERC', 'Htype', 'Kwal')
n <- 4
HABTYPE <- 'Htype'

habveg <- get_habveg(eiv, hab, c2=c2, n=n, HABTYPE=HABTYPE)
habveg$HABTYPE <- habveg$Htype

randvoorwaarden <- get_randvoorwaarden(habveg)

randvoorwaarden_ht <- get_randvoorwaarden_ht(randvoorwaarden)

bereik <- get_bereik(randvoorwaarden_ht)
hab$OPP <- area(hab)
dekking <- get_dekking(hab, habveg, c2=c2, n=n, HABTYPE=HABTYPE)
names(dekking)[names(dekking) == HABTYPE] <- 'HABTYPE'
bereik <- merge(bereik, dekking)

EIV[[N2K_gebied]] <- eiv
HAB[[N2K_gebied]] <- habveg
BRK[[N2K_gebied]] <- bereik

#===============================================================================
## Lonnekermeer
##===============================================================================
#
#setwd('data/abiotische_randvoorwaarden_input/Lonnekermeer')
## Turboveg: 
#opname <- list(abundance   =read.dbf('Lonnekermeer_2013/tvabund.dbf'),
#               habitat_type=read.dbf('Lonnekermeer_2013/tvhabita.dbf'),
#               remarks     =read.dbf('Lonnekermeer_2013/remarks.dbf'),
#               admin       =read.dbf('Lonnekermeer_2013/TvAdmin.dbf'))
#names(opname$habitat_type)
#(type_turboveg <- as.character(opname$habitat_type$VELDTYPE))
#
#jaar <- unique(year(as.Date(opname$habitat_type$DATE, format="%Y%m%d")))
##NOTE: verschillende jaren voor verschillende releves, slechts deel ligt in 
## gevraagde periode.
#
#vegtab <- read.csv('Lonnekermeer_2013_iteratio2_input.csv', sep='\t', stringsAsFactors=F)
#VELDTYPE <- unlist(vegtab[1,])
#names(VELDTYPE) <- NULL
#(type_iteratio <- VELDTYPE[3:(length(VELDTYPE) - 2)])
## Deze Iteratio input file is gemaakt in Turboveg met VELDTYPE als lokaal type.
#
# Vlakken:
#vlakken <- readOGR('.', stringsAsFactors=F)
#head(vlakken)
#any(duplicated((vlakken$OBJECTID_1))) # FALSE
#(type_vlakken <- unique(vlakken$LOKTYP1))
#unique(vlakken$SBB1)
## Het ziet er naar uit dat LOKTYP het beste correspondeert wat er gebruikt is 
## om de Iteratio vegetatie tabel te maken.
#v <- vlakken@data[c('OBJECTID_1', 'LOKTYP1', 'LOKTYP2', 'LOKTYP3', 'PERC1', 'PERC2', 'PERC3')]
#v1 <- v[c('OBJECTID_1', 'LOKTYP1', 'PERC1')]
#v2 <- v[c('OBJECTID_1', 'LOKTYP2', 'PERC2')]
#v3 <- v[c('OBJECTID_1', 'LOKTYP3', 'PERC3')]
#names(v1) <- names(v2) <-  names(v3) <- c('OBJECTID_1', 'Vegtype', 'Perc')
#vegtype <- rbind(v1, v2, v3)
#vegtype <- vegtype[order(vegtype$OBJECTID),]
#vegtype <- vegtype[vegtype$Perc != 0,]
#vegtype <- vegtype[!is.na(vegtype$Vegtype),]
#rownames(vegtype) <- NULL
#head(vegtype, 40)
#
#write.xlsx(vegtype, 'Vegetatievlakken_Beerzerveld.xlsx', row.names=F)
#
#unique(vlakken$JAAR)
## [1] "2017" "2015"
#### Run Iteratio ###
#setwd(root)
#
#N2K_gebied <- 'Lonnekermeer'
#hab <- habitattypen[habitattypen$N2K_GEBIED == N2K_gebied, ]
#path <- 'data/Lonnekermeer'
#eiv <- read_layers(path)
#
#eiv$jaar <- NA_integer_
#
#habveg <- get_habveg(eiv, hab)
#
#randvoorwaarden <- get_randvoorwaarden(habveg)
#
#randvoorwaarden_ht <- get_randvoorwaarden_ht(randvoorwaarden)
#
#bereik <- get_bereik(randvoorwaarden_ht)
#
#EIV[[N2K_gebied]] <- eiv
#HAB[[N2K_gebied]] <- habveg
#BRK[[N2K_gebied]] <- bereik

#===============================================================================
# Buurserzand & Haaksbergerveen
#===============================================================================

setwd(root)

N2K_gebied <- "Buurserzand & Haaksbergerveen"
hab <- habitattypen[habitattypen$N2K_GEBIED == N2K_gebied, ]
path <- 'data/abiotische_randvoorwaarden_input/Buurserzand_Haaksbergerveen'
eiv <- read_layers(path)

plot(hab)
plot(eiv, add=T, col='red')

mdb <- mdb.get(paste0(path, '/0820_Haaksbergerveen.mdb'))
jaar <- year(as.POSIXlt(mdb$PuntLocatieSoort$DATUM, 
                               format="%d/%m/%y %H:%M:%S", tz="CET"))
(jaar <- unique(jaar[!is.na(jaar)]))
eiv$jaar <- jaar

habveg <- get_habveg(eiv, hab)

randvoorwaarden <- get_randvoorwaarden(habveg)

randvoorwaarden_ht <- get_randvoorwaarden_ht(randvoorwaarden)

bereik <- get_bereik(randvoorwaarden_ht)
dekking <- get_dekking(hab, habveg)
bereik <- merge(bereik, dekking)

EIV[[N2K_gebied]] <- eiv
HAB[[N2K_gebied]] <- habveg
BRK[[N2K_gebied]] <- bereik

#===============================================================================
# Samen in een data.frame en sla op in bestand
#===============================================================================

#Habtype <- do.call(rbind, HAB)
#rownames(Habtype) <- NULL
#write.xlsx(Habtype, 'data/output/Habtype.xlsx', row.names=F, showNA=F)

Bereik <- do.call(rbind, BRK)
rownames(Bereik) <- NULL
write.xlsx(Bereik, 'data/abiotische_randvoorwaarden_output/Bereik.xlsx', row.names=F, showNA=F)
Bereik[Bereik$HABTYPE != 'H0000', ]

hbtp <- unique(Bereik$HABTYPE)
hbtp[!hbtp %in% abiotische_randvoorwaarden$HABTYPE]
