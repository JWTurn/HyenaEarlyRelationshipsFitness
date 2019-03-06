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

## Network data
asso <- fread(raw[grepl('asso', raw)], drop = 'V1')
affil <- fread(raw[grepl('affil', raw)], drop = 'V1')
aggr <- fread(raw[grepl('aggressions.csv', raw)], drop = 'V1')

### Prep data ----
asso[, sessiondate := as.IDate(sessiondate)]
periods <- c('period_start', 'period_end')
life[, (periods) := lapply(.SD, as.IDate), .SDcols = (periods)]

### Join life stages ----
## Merge all lifestages in 'life' to association data --
allstages <- merge(asso, life,
									 by.x = 'hyena', by.y = 'ego',
									 all.x = TRUE, allow.cartesian = TRUE)
warning(asso[is.na(sessiondate), .N], ' NAs in sessiondate dropped')

# Not egos
allstages[!(hyena %in% life$ego), idlife := hyena]

# session matches a life period
allstages[between(sessiondate, period_start, period_end),
					idlife := paste(hyena, '-', period)]

# where sessiondate doesn't match any period start/end
# checking if the sessiondate doesn't match any periods
allstages[, none := all(is.na(idlife)), .(sessiondate, hyena)]
allstages[(none), idlife := hyena]

assolife <- allstages[between(sessiondate, period_start, period_end), idlife := paste(hyena, '-', period)]

fsetdiff(assolife[, .SD, .SDcols = colnames(asso)], asso)

## Merge all lifestages in 'life' to affiliation data --
# TODO: do we want to merge the life stage of the solicitor or the receiver?
# not sure, the ego can be both the solicitor and the receiver which affects the in/out metrics.
#If it's arbitrary, I would just do the solicitor,
#but if it's a unique individual, then it should be the same in both columns, otherwise xxx-CD could interact with xxx, which would be itself
allstages <- merge(affil, life,
									 by.x = 'll_solicitor', by.y = 'ego',
									 allow.cartesian = TRUE)

# Compare sessiondate to period start+end
warning(affil[is.na(sessiondate), .N], ' NAs in sessiondate dropped')
affillife <- allstages[between(sessiondate, period_start, period_end)]

## Merge all lifestages in 'life' to aggression data --
# TODO: do we want to merge the life stage of the aggressor or the recip?
allstages <- merge(aggr, life,
									 by.x = 'aggressor', by.y = 'ego',
									 allow.cartesian = TRUE)

# Compare sessiondate to period start+end
warning(aggr[is.na(sessiondate), .N], ' NAs in sessiondate dropped')
aggrlife <- allstages[between(sessiondate, period_start, period_end)]

### Output ----
# Add idlife col
add_paste_id <- function(DT, xcol, ycol = 'period') {
	DT[, 'idlife' := paste(get(xcol), get(ycol), sep = '-')]
}
add_paste_id(assolife, 'hyena')
add_paste_id(affillife, 'll_solicitor')
add_paste_id(aggrlife, 'aggressor')

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
