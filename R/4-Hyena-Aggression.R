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
derived <- dir('data/derived-data', full.names = TRUE)

# Affiliation
aggre <- fread(raw[grepl('aggr', raw)], drop = 'V1')

# Life stages
life <- readRDS(derived[grepl('ego', derived)])


### Prep ----
aggr[, sessiondate := as.IDate(sessiondate)]
aggr[, aggressiontime := as.ITime(aggressiontime)]

### Observed ----
#TODO: how do we build aggression networks?

