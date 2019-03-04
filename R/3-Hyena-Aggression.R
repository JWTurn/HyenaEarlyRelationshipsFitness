### Hyena/Spatsoc - Association ====
# Alec Robitaille
# March 01 2019

### Notes ----
# directed, average of behavior1 during period/AI during period

### Packages ----
libs <- c('data.table', 'spatsoc', 'asnipe', 'igraph')
lapply(libs, require, character.only = TRUE)

### Import data ----
raw <- dir('data/raw-data', full.names = TRUE)

aggr <- fread(raw[grepl('agg', raw)])
egos <- fread(raw[grepl('egos', raw)])

### Prep ----
aggr[, sessiondate := as.IDate(sessiondate)]
aggr[, aggressiontime := as.ITime(aggressiontime)]

### Observed ----
#TODO: how do we build aggression networks?

