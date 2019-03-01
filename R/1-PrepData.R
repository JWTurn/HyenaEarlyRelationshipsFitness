### Hyena/Spatsoc ====
# Alec Robitaille


### Packages ----
libs <- c('data.table', 'spatsoc')
lapply(libs, require, character.only = TRUE)


### Import data ----
raw <- dir('data/raw-data', full.names = TRUE)

affil <- fread(raw[grepl('affil', raw)])
aggr <- fread(raw[grepl('agg', raw)])
asso <- fread(raw[grepl('asso', raw)])
egos <- fread(raw[grepl('egos', raw)])
