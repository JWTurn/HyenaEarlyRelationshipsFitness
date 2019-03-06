### Prep Hyena Data ====
# Alec Robitaille
# Started: March 05 2019

### Packages ----
libs <- c('data.table')
lapply(libs, require, character.only = TRUE)

### Import data ----
raw <- dir('data/raw-data', full.names = TRUE)

## Life stages
life <- fread(raw[grepl('lifeperiods.csv', raw)], drop = 'V1')

# Keep only relevant columns
life <- life[, .(ego, period, period_start, period_end)]


######
#TODO: which egos to filter out? (put it here)
######


## Network data
asso <- fread(raw[grepl('asso', raw)], drop = 'V1')
affil <- fread(raw[grepl('affil', raw)], drop = 'V1')
aggr <- fread(raw[grepl('aggressions.csv', raw)], drop = 'V1')

### Prep data ----
asso[, sessiondate := as.IDate(sessiondate)]
periods <- c('period_start', 'period_end')
life[, (periods) := lapply(.SD, as.IDate), .SDcols = (periods)]

setnames(affil, 'll_reciever', 'recip')

### Join association to life stages ----
allstages <- merge(asso, life,
									 by.x = 'hyena', by.y = 'ego',
									 all.x = TRUE, allow.cartesian = TRUE)
warning(asso[is.na(sessiondate), .N], ' NAs in sessiondate dropped')

# Not egos
allstages[!(hyena %in% life$ego), idlife := hyena]

# session matches a life period
allstages[between(sessiondate, period_start, period_end),
					idlife := paste0(hyena, '-', period)]

# where sessiondate doesn't match any period start/end
# checking if the sessiondate doesn't match any periods
allstages[, none := all(is.na(idlife)), .(sessiondate, hyena)]
allstages[(none), idlife := hyena]

assolife <- unique(allstages[!is.na(idlife), .(hyena, session, sessiondate, idlife)])

### Join affiliation to life stages ----
warning(affil[is.na(sessiondate), .N], ' NAs in sessiondate dropped')
## For solicitor --
allstages <- merge(affil, life,
									 by.x = 'aggressor', by.y = 'ego',
									 all.x = TRUE, allow.cartesian = TRUE)
# Not egos
allstages[!(aggressor %in% life$ego), idlife_solicitor := aggressor]

# session matches a life period
allstages[between(sessiondate, period_start, period_end),
					idlife_solicitor := paste0(aggressor, '-', period)]

# where sessiondate doesn't match any period start/end
# checking if the sessiondate doesn't match any periods
allstages[, none := all(is.na(idlife_solicitor)), .(sessiondate, aggressor)]
allstages[(none), idlife_solicitor := aggressor]

affillife <- unique(allstages[!is.na(idlife_solicitor), .SD,
															.SDcols = c(colnames(affil), 'idlife_solicitor')])

## For receiver --
allstages <- merge(affillife, life,
									 by.x = 'll_receiver', by.y = 'ego',
									 all.x = TRUE, allow.cartesian = TRUE)
# Not egos
allstages[!(ll_receiver %in% life$ego), idlife_receiver := ll_receiver]

# session matches a life period
allstages[between(sessiondate, period_start, period_end),
					idlife_receiver := paste0(ll_receiver, '-', period)]

# where sessiondate doesn't match any period start/end
# checking if the sessiondate doesn't match any periods
allstages[, none := all(is.na(idlife_receiver)), .(sessiondate, ll_receiver)]
allstages[(none), idlife_receiver := ll_receiver]

affillife <- unique(allstages[!is.na(idlife_receiver) & !is.na(idlife_solicitor), .SD, .SDcols = c(colnames(affil), 'idlife_receiver', 'idlife_solicitor')])

### Join aggression to life stages ----
warning(aggr[is.na(sessiondate), .N], ' NAs in sessiondate dropped')

## For aggressor --
allstages <- merge(aggr, life,
									 by.x = 'aggressor', by.y = 'ego',
									 all.x = TRUE, allow.cartesian = TRUE)
# Not egos
allstages[!(aggressor %in% life$ego), idlife_aggressor := aggressor]

# session matches a life period
allstages[between(sessiondate, period_start, period_end),
					idlife_aggressor := paste0(aggressor, '-', period)]

# where sessiondate doesn't match any period start/end
# checking if the sessiondate doesn't match any periods
allstages[, none := all(is.na(idlife_aggressor)), .(sessiondate, aggressor)]
allstages[(none), idlife_aggressor := aggressor]

aggrlife <- unique(allstages[!is.na(idlife_aggressor), .SD,
														 .SDcols = c(colnames(aggr), 'idlife_aggressor')])

## For recip --
allstages <- merge(aggrlife, life,
									 by.x = 'recip', by.y = 'ego',
									 all.x = TRUE, allow.cartesian = TRUE)
# Not egos
allstages[!(recip %in% life$ego), idlife_recip := recip]

# session matches a life period
allstages[between(sessiondate, period_start, period_end),
					idlife_recip := paste0(recip, '-', period)]

# where sessiondate doesn't match any period start/end
# checking if the sessiondate doesn't match any periods
allstages[, none := all(is.na(idlife_recip)), .(sessiondate, recip)]
allstages[(none), idlife_recip := recip]

aggrlife <- unique(allstages[!is.na(idlife_recip) & !is.na(idlife_aggressor), .SD, .SDcols = c(colnames(aggr), 'idlife_recip', 'idlife_aggressor')])

### Output ----
# Output to derived-data
saveRDS(assolife, 'data/derived-data/association-lifestages.Rds')
saveRDS(affillife, 'data/derived-data/affiliation-lifestages.Rds')
saveRDS(aggrlife, 'data/derived-data/aggression-lifestages.Rds')


### Extra ----
## Finding overlapping life periods:
assolife[duplicated(assolife[, .SD, .SDcols = colnames(asso)])]
affillife[duplicated(affillife[, .SD, .SDcols = colnames(affil)])]
aggrlife[duplicated(aggrlife[, .SD, .SDcols = colnames(aggr)])]
##
