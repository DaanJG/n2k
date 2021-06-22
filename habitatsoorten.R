library('magrittr')
library('rgdal')
#library('raster')
library('maptools')
library('rgeos')
library('readxl')
library('lubridate')
library('plyr')
library('lme4')
library('ggplot2')

rrow <- function(dat, n=5) {
    smpl <- sample(1:nrow(dat), min(n, nrow(dat)), replace=F)
    smpl <- smpl[order(smpl)]
    dat[smpl,]
}

ogrListLayers('data/N2K/shp')
n2k <- readOGR('data/N2K/shp', 'B4_Natura_2000-gebiedenPolygon', stringsAsFactors=F)

plot(n2k)

gebieden <- c(
     "Weerribben",
     "De Wieden",
     "Uiterwaarden Zwarte Water en Vecht",
     "Olde Maten & Veerslootslanden",
     "Vecht- en Beneden-Reggegebied",
     "Engbertsdijksvenen",
     "Boetelerveld",
     "Sallandse Heuvelrug",
     "Wierdense Veld",
     "Borkeld",
     "Springendal & Dal van de Mosbeek",
     "Bergvennen & Brecklenkampse Veld",
     "Achter de Voort, Agelerbroek & Voltherbroek",
     "Lemselermaten",
     "Dinkelland",
     "Landgoederen Oldenzaal",
     "Lonnekermeer",
     "Buurserzand & Haaksbergerveen",
     "Witte Veen",
     "Aamsveen"
     )

all(gebieden %in% n2k$NAAM_N2K)

overijssel <- n2k[n2k$NAAM_N2K %in% gebieden, ]
plot(overijssel, add=T, col='red')

overijssel$IDs <- rep(1, length(overijssel))
u <- unionSpatialPolygons(overijssel, overijssel$IDs)
str(u, 1)
overijssel@polygons[[1]] %>% str(2)
u@polygons[[1]] %>% str(2)

u <- SpatialPolygonsDataFrame(u, data.frame(ID=1))

writeOGR(u, 'data/N2K/shp/', 'Overijssel_union', driver='ESRI Shapefile')

#===============================================================================
# Habitattypen
#===============================================================================

hab <- readOGR('data/Habitattypenkaart_Overijssel')
ht1 <- unique(c(as.matrix(hab@data[grepl('HABTYPE', names(hab))])))
(ht1 <- ht1[order(ht1)]) # Alle habitattypen in N2000-gebieden in Overijssel

(hab_uitvraag <- read_excel('uitvraag.xlsx', sheet='habitattypen'))
ht <- as.data.frame(hab_uitvraag[c('hbt_code', 'hbt_naam')][hab_uitvraag['bestuursorgaan'] == 'provincie Overijssel',])
(ht <- ht[!duplicated(ht$hbt_code),])
ht[order(ht$hbt_code),]
ht <- ht[!is.na(ht$hbt_code),]
rownames(ht) <- NULL
ht[order(ht$hbt_code),] # Alle habitattypen in Overijssel in de uitvraag

#===============================================================================
# Habitatsoorten
#===============================================================================

hs <- read_excel('uitvraag.xlsx', sheet='habitatsoorten')
hs <- hs[hs$bestuursorgaan == 'provincie Overijssel',] 
hs$soort_naam[hs$soort_naam == 'Zegge-korfslak'] <- 'Zeggekorfslak' # andere spelling in ndff
hs[1:5] %>% as.data.frame
soorten <- hs[c('soort_code', 'soort_naam')]
(soorten <- soorten[!duplicated(soorten), ])

#(hs2 <- read_excel('data/BIJ12/HabitattypeKwaliteit.xlsx', sheet='N2KGebied_HT_TypischeSoorten'))
#names(hs2)[2] <- 'N2000GebiedNaam'
#all(gebieden %in% hs2[['N2000GebiedNaam']])
#soorten2 <- hs2[hs2[['N2000GebiedNaam']] %in% gebieden, ]
#ddply(soorten2, c('N2000GebiedNaam', 'SoortNaamNederlandse'), summarize,
#      nrow=length(HabitattypeEUCode))
#soorten$soort_naam[!soorten$soort_naam %in% soorten2$SoortNaamNederlandse]
#unique(soorten2$SoortNaamNederlandse)      

path <- 'data/NDFF/NDFF-export_07-06-2021_14-58-10/NDFF-export_07-06-2021_14-58-09.xls'
ndff_xls <- read_excel(path)
nrow(ndff_xls)

path <- 'data/NDFF/NDFF-export_07-06-2021_14-57-53/'
ndff_ <- readOGR(path, stringsAsFactors=F)
ndff <- ndff_

overijssel@proj4string
ndff@proj4string

ndff_intersects_n2k <- gIntersects(overijssel, ndff, byid=T)
ndff_intersects_any <- apply(ndff_intersects_n2k, 1, any)

sum(apply(ndff_intersects_n2k, 1, sum) > 1)
# 37 waarnemingen liggen in meer dan 1 n2k gebied.

NAAM_N2K <- apply(ndff_intersects_n2k, 1, function(x) overijssel$NAAM_N2K[x])
ndff$NAAM_N2K  <- sapply(NAAM_N2K, function(x) x[1])
ndff$NAAM_N2K2 <- sapply(NAAM_N2K, function(x) ifelse(length(x) > 1, x[[2]], as.character(NA)))
ndff <- ndff[ndff_intersects_any, ]

s <- c(
    #"obs_uri"    
    "soort_ned", "soort_wet", "telondrwrp", "telmethode", "orig_aant",
    "aantal_min", "aantal_max", "eenheid", "centrumx", "centrumy", "area_m2",
    "loc_type", #"vervaagd", 
    "datm_start", "datm_stop", "datm_dkkng", #"stadium", "geslacht", "gedrag", 
    #"biotoop", "doodsoorzk", "verblfplts", "substraat",
    #"syntaxon", 
    "opp_dkkng", "protocol", #"detmethode", "databehrdr", 
    "dataeigenr"#, #"kwliteit", "srtgroepen", "wnb_vrl", "wnb_hrl", "wnb_andere", 
    #"ffwet1", "ffwet2", "ffwet3", "rodelijst", "zoid" "sessionid"
)

s <- c(s, 'NAAM_N2K')

ndff$datm_start <- as.Date(ndff$datm_start)
ndff$datm_stop  <- as.Date(ndff$datm_stop)
ndff$datm_dkkng <- as.logical(ndff$datm_dkkng)
ndff$jaar <- year(ndff$datm_start)
ndff$opp_dkkng <- as.logical(ndff$opp_dkkng)
ndff$protocol[ndff$protocol == 
    "01.201 Monitoring van amfibie\xebn in Nederland (NEM)"]  <- 
    "01.201 Monitoring van amfibieën in Nederland (NEM)"

table(duur <- ndff$datm_stop - ndff$datm_start)
ndff[s]@data[duur > 0, ]  %>% head
ndff[s]@data[duur > 0 & !ndff$datm_dkkng, ] %>% head

pfreq <- ndff$protocol %>% table %>% as.data.frame
(pfreq <- pfreq[rev(order(pfreq$Freq)),])
#                                                                       . Freq
# 12                         101.000 Losse waarnemingen van Waarneming.nl 6203
# 4                                       02.201 Geel schorpioenmos (NEM) 3764
# 18 12.205 Monitoring Beoordeling Natuurkwaliteit EHS - N2000 (SNL-2014) 1601
# 11                        100.000 Losse waarnemingen NDFF-invoerportaal 1389
# 8                               07.201 Landelijk Meetnet Libellen (NEM)  411
# 7                                04.006 Slakken van de Habitatrichtlijn  164
# 5                               03.201 Landelijk Meetnet Vlinders (NEM)   98
# 2              01.204 ANLb meetnet beleidsmonitoring Amfibien en Vissen   66
# 6                                 04.001 Kevers van de habitatrichtlijn   64
# 1                 01.201 Monitoring van amfibie\xebn in Nederland (NEM)   51
# 10           10.003 Gebiedsgerichte inventarisatie herpetofauna - RAVON   26
# 14                           106.000 Losse waarnemingen Staatsbosbeheer   21
# 13                          103.000 Losse waarnemingen Natuurmonumenten   20
# 16                                              12.007 Vegetatieopnamen   11
# 3                   02.000 Losse waarnemingen van mossen en korstmossen   10
# 17     12.202 Landelijk Meetnet Flora- Milieu- en Natuurkwaliteit (NEM)    5
# 19                                 13.000 Losse waarnemingen van vissen    1
# 15                         12.000 Losse waarnemingen vaatplanten FLORON    1
# 9                                    10.002 Environmental DNA-onderzoek    1
protocollen <- c(
    #"101.000 Losse waarnemingen van Waarneming.nl",
    "02.201 Geel schorpioenmos (NEM)",
    "12.205 Monitoring Beoordeling Natuurkwaliteit EHS - N2000 (SNL-2014)",
    #"100.000 Losse waarnemingen NDFF-invoerportaal",
    "07.201 Landelijk Meetnet Libellen (NEM)",
    "04.006 Slakken van de Habitatrichtlijn",
    "03.201 Landelijk Meetnet Vlinders (NEM)",
    "01.204 ANLb meetnet beleidsmonitoring Amfibien en Vissen",
    "04.001 Kevers van de habitatrichtlijn",
    "01.201 Monitoring van amfibieën in Nederland (NEM)",
    "10.003 Gebiedsgerichte inventarisatie herpetofauna - RAVON",
    #"106.000 Losse waarnemingen Staatsbosbeheer",
    #"103.000 Losse waarnemingen Natuurmonumenten",
    "12.007 Vegetatieopnamen",
    #"02.000 Losse waarnemingen van mossen en korstmossen",
    "12.202 Landelijk Meetnet Flora- Milieu- en Natuurkwaliteit (NEM)"#,
    #"13.000 Losse waarnemingen van vissen",
    #"12.000 Losse waarnemingen vaatplanten FLORON",
    #"10.002 Environmental DNA-onderzoek"
)

ndff <- ndff[ndff$protocol %in% protocollen, ]

table(orig_aant=ndff$orig_aant) %>% as.data.frame %>% head
table(aantal_min=ndff$aantal_min) %>% as.data.frame %>% head
table(aantal_min=ndff$aantal_max) %>% as.data.frame %>% head

unique(ndff$orig_aant)
ndff@data[s][ndff$orig_aant == 'F',] %>% head

#NOTE: GEEN nulwaarnemingen :(

table(ndff$protocol, ndff$soort_ned)

for (soort in unique(ndff$soort_ned)) {
    cat('\n')
    cat(soort)
    cat('\n')
    gebieden <- as.factor(unique(hs[hs$soort_naam==soort,]$n2k_naam))
    d.f. <- ndff@data[ndff$soort_ned == soort & ndff$NAAM_N2K %in% gebieden,]
    d.f.$NAAM_N2K <- factor(d.f.$NAAM_N2K, gebieden)
    d.f.$jaar <- factor(d.f.$jaar, 2013:2020)
    #d.f. <- d.f.[d.f.$protocol %in% protocollen, ]
    gebxjaar <- table(d.f.$NAAM_N2K, d.f.$jaar)
    protxjaar <- table(d.f.$protocol, d.f.$jaar)
    print(rbind(gebxjaar, protxjaar))
    cat('---\n')
}

protocol <- '12.205'
n <- ndff[s][grep(protocol, ndff$protocol), ]
nrow(n)
rrow(n@data)
plot(overijssel)
plot(n, col='red', add=T)

collumn <- 'orig_aant'
tbl <- table(aantal=ndff[[collumn]])[order(tbl)]
as.data.frame(tbl) %>% head(10)
#NOTE: hoewel NDFF voor protocollen wel nulwaarnemingen toestaat (zie
# https://www.ndff.nl/overdendff/validatie/protocollen/), zitten er geen
# nulwaarnemingen in de data, terwijl er wel veel tellingen met resultaat "1"
# zijn. Het lijkt mij dus onwaarschijnlijk dat er nooit eens "0" wordt
# waargenomen. Nulwaarnemingen zijn dus waarschijnlijk wel gedaan, maar niet
# opgenomen in de database.

root <- getwd()
setwd('data/NDFF/protocol')
ndff2 <- list()
for (protocol in protocollen) {
    if (protocol %in% dir())
        ndff2[[protocol]] <- readOGR(dsn=protocol)
}

d <- ndff2[[2]]
d@data %>% head
tbl <- table(d$area_m2, d$loc_type); rbind(head(tbl), tail(tbl))

snl <- ndff[grep('12.205', ndff$protocol),]
snl@data[s] %>% head
table(snl$soort_ned, snl$jaar)
snl[snl$soort_ned == 'Kamsalamander',]@data[s]
snl[snl$soort_ned == 'Drijvende waterweegbree',]@data[s]
setwd(root)

#===============================================================================
# Data rechtstreeks van de Vlinderstichting
#===============================================================================

vs <- read.csv('data/vlinderstichting/Tellingen_HR_soorten_Overijssel.csv',
                   sep=';', stringsAsFactors=FALSE)
vs$xc <- as.numeric(sub(',', '.', vs$xc))
vs$yc <- as.numeric(sub(',', '.', vs$yc))
vs$visit_startdate <- as.POSIXct(vs$visit_startdate, tz='CET')
vs$visit_enddate  <- as.POSIXct(vs$visit_enddate, tz='CET')
vs$visit_duration <- as.numeric((vs$visit_enddate - vs$visit_startdate) / 60)
vs$year <- year(vs$visit_startdate)

table(aantal=vs$TotAant) %>% as.data.frame %>% head(10) # Geen nulwaarnemingen

summ <- ddply(vs, .(countobject_id, xc, yc), summarize, 
              rows=length(TotAant),
              na =sum( is.na(TotAant)),
              obs=sum(!is.na(TotAant)),
              tot=na+obs)
summ$count %>% duplicated %>% any
# [1] FALSE
# Dit betekent dat elk countobject_id slechts 1 locatie heeft (xc, xy).
(dup <- summ[c('xc', 'yc')] %>% duplicated) %>% any
# [1] TRUE
# Dit betekent dat sommige locaties meer dan 1 countobject_id hebben.

(tbl <- table(object=vs$countobject_id, project=vs$project_id)) %>% head(55)
any(apply(tbl, 1, function(row) row %>% as.logical %>% sum > 1))
# [1] FALSE
# Dit betekent dat geen countobject in meer dan 1 project voorkomt.

vs$visit_id <- id(vs[c('countobject_id', 'xc', 'yc', 'visit_startdate', 'visit_enddate')], drop=T)
max(vs$visit_id)

table(soort=vs$species_name_nl, project=vs$project_id)
table(jaar=vs$year, project=vs$project_id)

for (project in sort(unique(vs$project_id))) {
    cat('\n')
    cat(paste('project', project))
    cat('\n')
    d.f. <- vs[vs$project_id == project, ]
    print(table(soort=d.f.$species_name_nl, jaar=d.f.$year))
}

dup_visits <- vs$visit_id[vs$visit_id %>% duplicated]
vs[vs$visit_id%in% dup_visits,]
cols <- c('project_id', 'countobject_id', 'visit_id', 'xc', 'yc', 'visit_startdate', 'visit_enddate', 'visit_duration')
visits <- vs[!duplicated(vs$visit_id), ][cols]
visits %>% head(55)

plot(vs$xc, vs$yc)

#===============================================================================
# Geel schorpioenmos
#===============================================================================

plot(overijssel)

gs <- ndff[grep('02.201', ndff$protocol), ]
gs <- gs[gs$NAAM_N2K == 'De Wieden' | !is.na(gs$NAAM_N2K2) & gs$NAAM_N2K2 == 'De Wieden', ]
nrow(gs)
plot(overijssel[overijssel$NAAM_N2K == 'De Wieden',])
plot(gs, col='red', border='red', add=T)
gs@data[s] %>% rrow
unique(gs$telmethode)
# [1] "Voorkomen" "aanwezig" 
unique(gs$loc_type)
# [1] "hok"  "vlak"
table(opp=gs$area_m2, gs$jaar)
# We gaan er vanuit dat het een foutje is dat sommige hokken niet 100 m2 zijn maar 
# 782,8427 m2.

ddply(gs@data, .(soort_ned, NAAM_N2K, jaar), summarize, totaal=sum(aantal_min))

#===============================================================================
# 04.006 Slakken van de Habitatrichtlijn
#===============================================================================

sl <- ndff[ndff$protocol == '04.006 Slakken van de Habitatrichtlijn', ]
sl[s]@data %>% rrow(20)
unique(sl$loc_type)
# [1] "punt"
unique(sl$area_m2)
# [1] 282.8427 282.8427
sl %>% rrow(1) %>% plot
sl %>% tail(5) %>% plot

#NOTE: Volgens https://www.anemoon.org/projecten/natura2000/habslak-project 
# wordt aanwezigheid de slakkensoorten in 10x10 km hokken bepaald.

#===============================================================================
# 01.204 ANLb meetnet beleidsmonitoring Amfibien en Vissen
#===============================================================================

av <- ndff[grep("01.204", ndff$protocol), ]
av@data[s] %>% rrow()
unique(av$soort_ned)
# [1] "Bittervoorn"          "Kleine modderkruiper" "Kamsalamander"        "Grote modderkruiper" 
unique(av$loc_type)
# [1] "lijn"
av %>% rrow(1) %>% plot
unique(av$telondrwrp)
# [1] "levend exemplaar"      "DNA-sporen in monster"
table(soort=av$soort_ned, onderwerp=av$telondrwrp)
#                       onderwerp
# soort                  DNA-sporen in monster levend exemplaar
#   Bittervoorn                              0               24
#   Grote modderkruiper                     20                0
#   Kamsalamander                            0                9
#   Kleine modderkruiper                     0               13
table(soort=av$soort_ned, jaar=av$jaar)
#                       jaar
# soort                  2016 2017 2018 2019 2020
#   Bittervoorn             0    1    2   11   10
#   Grote modderkruiper     0    0    0   10   10
#   Kamsalamander           6    3    0    0    0
#   Kleine modderkruiper    1    0    1    7    4
table(n2k=av$NAAM_N2K, soort=av$soort_ned)
#                                              soort
# n2k                                           Bittervoorn Grote modderkruiper Kamsalamander Kleine modderkruiper
#   Aamsveen                                              0                   0             6                    0
#   Achter de Voort, Agelerbroek & Voltherbroek           0                   0             3                    0
#   De Wieden                                            23                  18             0                   12
#   Uiterwaarden Zwarte Water en Vecht                    1                   2             0                    1

#===============================================================================
# 04.001 Kevers van de habitatrichtlijn
#===============================================================================

kv <- ndff[grep("04.001", ndff$protocol), ]
kv@data[s] %>% rrow()
table(kv$soort_ned, kv$jaar)
#                            
#                             2015 2016 2017 2018 2019
#   Gestreepte waterroofkever    0    0    0    0   26
#   Vliegend hert               23    6    6    2    1
table(kv$soort_ned, kv$loc_type)
#                            
#                             hok punt
#   Gestreepte waterroofkever   0   26
#   Vliegend hert              36    2
table(kv$area_m2, kv$loc_type)
#                   
#                    hok punt
#   70.7106781189214   0   28
#   10000             14    0
#   78284.271247458   20    0
#   1e+06              2    0
kv %>% rrow(1) %>% plot

#===============================================================================
# 01.201 Monitoring van amfibieën in Nederland (NEM)
#===============================================================================

am <- ndff[grep('01.201', ndff$protocol),]
am@data[s]
unique(am$soort_ned)
# [1] "Kamsalamander"
table(am$NAAM_N2K, am$jaar)
#                                              
#                                               2013 2014 2015 2016 2018
#   Achter de Voort, Agelerbroek & Voltherbroek    0    0    1    2    0
#   Buurserzand & Haaksbergerveen                 12    0    3    6    2
#   Landgoederen Oldenzaal                         5    0    4    0    0
#   Sallandse Heuvelrug                            1   12    0    0    0
#   Witte Veen                                     0    3    0    0    0
table(am$telmethode, am$loc_type)
#                          
#                           coord punt vlak
#   exact aantal               14    5    2
#   geschat aantal              0    1    0
#   presentieklasse (Ravon)     9   14    6

#===============================================================================
# 10.003 Gebiedsgerichte inventarisatie herpetofauna - RAVON"
#===============================================================================

hf <- ndff[grep('10.003', ndff$protocol),]
hf@data[s]
unique(hf$soort_ned)
# [1] "Kamsalamander"
table(hf$NAAM_N2K, hf$jaar)
#                         
#                          2016 2017 2019
#   Dinkelland                2    0    0
#   Landgoederen Oldenzaal    9    0    3
#   Witte Veen                0   12    0
hf %>% rrow(1) %>% plot

#===============================================================================
# 12.007 Vegetatieopnamen
#===============================================================================

vo <- ndff[grep('12.007', ndff$protocol),]
vo@data[s]
unique(vo$soort_ned)
# [1] "Bittervoorn"             "Drijvende waterweegbree"
unique(vo$NAAM_N2K)
# [1] "Buurserzand & Haaksbergerveen"
table(vo$soort_ned, vo$jaar)
#                          
#                           2017 2018
#   Bittervoorn                3    6
#   Drijvende waterweegbree    0    2

#===============================================================================
# 12.202 Landelijk Meetnet Flora- Milieu- en Natuurkwaliteit (NEM)
#===============================================================================

kw <- ndff[grep('12.202', ndff$protocol), ]
kw[s]@data
table(kw$NAAM_N2K, kw$jaar)
#             
#              2014 2016 2017
#   De Wieden     0    2    0
#   Weerribben    2    0    1
