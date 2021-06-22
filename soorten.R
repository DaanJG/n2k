library('readxl')

telling <- read.csv('data/NDFF-export_26-05-2021_15-51-55.csv', stringsAsFactors=F)

typsoort <- read_excel('data/typische_soorten.xlsx')

unique(telling$soort_ned)
qnique(telling$soort_wet)
typsoort[(typsoort[['Wetenschappelijke naam']] %in% telling$soort_wet),]

table(telling$loc_type)
head(telling, 1)
