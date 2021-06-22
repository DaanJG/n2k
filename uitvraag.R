library('readxl')
library('xlsx')
library('magrittr')
library('tidyr')
library('plyr')

xlsx <- 'data/uitvraag.xlsx'
form <- Map(function(x) read_excel(xlsx, sheet=x), excel_sheets(xlsx))
names(form)
# [1] "MetaData"        "algemeen"        "habitattypen"    "broedvogels"    
# [5] "nietbroedvogels" "habitatsoorten" 
A <- list(habitattypen=c('J', 'K', 'S', 'T', 'U', 'V', 'W', 'X'),
          habitatsoorten=c('G', 'I', 'K', 'M', 'O', 'Q', 'S', 'U', 
                           'H', 'J', 'L', 'N', 'P', 'R', 'T', 'V'),
          broedvogels=c('G', 'I', 'K', 'M', 'O', 'Q', 'S', 'U',
                        'H', 'J', 'L', 'N', 'P', 'R', 'T', 'V'),
          nietbroedvogels=c('I', 'K', 'M', 'O', 'Q', 'S', 'U', 'W',
                            'J', 'L', 'N', 'P', 'R', 'T', 'V', 'X'))

Overijssel <- Map(function(x) x[x$bestuursorgaan == 'provincie Overijssel' & !is.na(x$bestuursorgaan),], form[2:length(form)])
Borkeld    <- Map(function(x) x[x$n2k_naam == 'Borkeld' & !is.na(x$n2k_naam),], form[2:length(form)])

get_status <- function(df, tabel) {
    lttrs <- expand.grid(LETTERS, LETTERS)
    lttrs <- lttrs[order(lttrs$Var1, lttrs$Var2),]
    lttrs <- c(LETTERS, do.call('paste0', lttrs))
    status <- data.frame(
        rijen=(gv <- mapply(length,                        df)),
        ingevuld=(ig <- mapply(function(x) sum(!is.na(x)), df)),
        perc=round((ig/gv) * 100)
        )
    status$tabel <- tabel
    status$kolom <- lttrs[1:nrow(status)]
    status$naam <- rownames(status)
    status$in_aanpassing <- status$kolom %in% A[[tabel]]
    #status <- separate(statusO, naam, c('groep', NA), ':', remove=F, fill='right') %>% head(20)
    status <- status[c('tabel', 'kolom', 'naam', 'in_aanpassing', 'rijen', 'ingevuld', 'perc')]
    #status <- status[c('tabel', 'groep', 'kolom', 'naam', 'in_aanpassing', 'rijen', 'ingevuld', 'perc')]
    rownames(status) <- NULL
    status
}

rb <- function(...) rbind(..., make.row.names=FALSE)

statusO = do.call(rb, Map(get_status, Overijssel[names(A)], names(A)))
statusO[statusO$in_aanpassing,]
write.xlsx(statusO[statusO$in_aanpassing,], 'data/status_overijssel.xlsx', row.names=F)

statusB = do.call(rb, Map(get_status, Borkeld[names(A)], names(A)))
statusB[statusB$rijen > 0,]
write.xlsx(statusB[statusB$rijen > 0,], 'data/status_Borkeld.xlsx', row.names=F)

