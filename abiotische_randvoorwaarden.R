root <- getwd()

source('R/n2k.R')

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
hab <- remove_habtypes(hab, c('H0000', 'H9999'))
path <- 'data/abiotische_randvoorwaarden_input/Sallandse_Heuvelrug/Natuurmonumenten'
eiv <- read_layers(path)

eiv$jaar <- 2015

plot(hab)
plot(eiv, add=T, col='red')

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

#EIV[[N2K_gebied]] <- eiv
#HAB[[N2K_gebied]] <- habveg
#BRK[[N2K_gebied]] <- bereik

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
