### Hyena/Spatsoc - Association ====
# Alec Robitaille
# March 01 2019

### Packages ----
libs <- c('data.table', 'spatsoc', 'asnipe', 'igraph')
lapply(libs, require, character.only = TRUE)

### Import data ----
raw <- dir('data/raw-data', full.names = TRUE)

affil <- fread(raw[grepl('affil', raw)])
egos <- fread(raw[grepl('egos', raw)])

### Prep ----
affil[, sessiondate := as.IDate(sessiondate)]
affil[, grtTime := as.ITime(grtTime)]

### Observed ----
#TODO: how do we build affiliation networks?
