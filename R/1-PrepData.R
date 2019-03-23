### Prep Hyena Data ====
# Alec Robitaille
# Started: March 01 2019

### Packages ----
libs <- c('data.table')
lapply(libs, require, character.only = TRUE)

### Import data ----
raw <- 'data/raw-data/'

# Life stages
life <- fread(paste0(raw, 'lifeperiods.csv'), drop = 'V1')

# Egos
egos <- fread(paste0(raw, 'egos_filtered.csv'), drop = 'V1')

# Affiliation
affil <- fread(paste0(raw, 'data_affiliations.csv'), drop = 'V1')

# Association
asso <- fread(paste0(raw, 'data_associations.csv'), drop = 'V1')

# Aggression
aggr <- fread(paste0(raw, 'data_aggressions.csv'), drop = 'V1')


### Prep data ----
## Filter egos
life <- life[ego %chin% egos$ego]

## Sub columns
# Keep only relevant columns
life <- life[, .(ego, period, period_start, period_end)]

# Cast session to an integer group column
asso[, group := .GRP, session]

aggr[, behavior1 := as.numeric(behavior1)]

## Date columns
asso[, sessiondate := as.IDate(sessiondate)]
asso[, yr := year(sessiondate)]

periods <- c('period_start', 'period_end')
life[, (periods) := lapply(.SD, as.IDate), .SDcols = (periods)]

affil[, sessiondate := as.IDate(sessiondate)]
affil[, yr := year(sessiondate)]

asso[, sessiondate := as.IDate(sessiondate)]
asso[, yr := year(sessiondate)]

aggr[, sessiondate := as.IDate(sessiondate)]
aggr[, aggressiontime := as.ITime(aggressiontime)]

## Typo
setnames(affil, 'll_reciever', 'll_receiver')

### Output ----
# Output to derived-data
saveRDS(life, 'data/derived-data/filtered-ego-lifestages.Rds')
saveRDS(asso, 'data/derived-data/prep-association.Rds')
saveRDS(affil, 'data/derived-data/prep-affiliation.Rds')
saveRDS(aggr, 'data/derived-data/prep-aggression.Rds')


###
message('=== DATA PREP COMPLETE ===')
