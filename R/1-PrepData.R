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

setnames(affil, 'll_reciever', 'll_receiver')

### Join life stages to association data ----
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

### Join life stages to affiliation data ----
warning(affil[is.na(sessiondate), .N], ' NAs in sessiondate dropped')
## For solicitor --
allstages <- merge(affil, life,
									 by.x = 'll_solicitor', by.y = 'ego',
									 all.x = TRUE, allow.cartesian = TRUE)
# Not egos
allstages[!(ll_solicitor %in% life$ego), idlife_solicitor := ll_solicitor]

# session matches a life period
allstages[between(sessiondate, period_start, period_end),
					idlife_solicitor := paste0(ll_solicitor, '-', period)]

# where sessiondate doesn't match any period start/end
# checking if the sessiondate doesn't match any periods
allstages[, none := all(is.na(idlife_solicitor)), .(sessiondate, ll_solicitor)]
allstages[(none), idlife_solicitor := ll_solicitor]

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

## Merge all lifestages in 'life' to aggression data --
# TODO: do we want to merge the life stage of the aggressor or the recip?
allstages <- merge(aggr, life,
									 by.x = 'aggressor', by.y = 'ego',
									 allow.cartesian = TRUE)

# Compare sessiondate to period start+end
warning(aggr[is.na(sessiondate), .N], ' NAs in sessiondate dropped')
aggrlife <- allstages[between(sessiondate, period_start, period_end)]

### Output ----
# Check output data
check_dup <- function(DT) {
	if (anyDuplicated(DT) != 0) stop('duplicated rows found in ', deparse(substitute(DT)))
}
check_na <- function(DT, col = 'idlife') {
	if (DT[is.na(get(col)), .N] != 0) stop('NAs found in ', col, ' column of ', deparse(substitute(DT)))
}

check_dup(assolife)
check_na(assolife)
check_dup(affillife)
check_na(affillife)
check_dup(aggrlife)
check_na(aggrlife)

# Output to derived-data
saveRDS(assolife, 'data/derived-data/association-lifestages.Rds')
saveRDS(affillife, 'data/derived-data/affiliation-lifestages.Rds')
saveRDS(aggrlife, 'data/derived-data/aggression-lifestages.Rds')


### Extra ----
## Finding overlapping life periods:
assolife[duplicated(assolife[, .SD, .SDcols = colnames(asso)])]
affillife[duplicated(affillife[, .SD, .SDcols = colnames(affil)])]
##
