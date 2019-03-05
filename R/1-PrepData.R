### Prep Data ====
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

### Join life stages ----
## Merge all lifestages in 'life' to association data --
allstages <- merge(asso, life,
									 by.x = 'hyena', by.y = 'ego',
									 allow.cartesian = TRUE)
# Compare sessiondate to period start+end
warning(asso[is.na(sessiondate), .N], ' NAs in sessiondate dropped')
assolife <- allstages[between(sessiondate, period_start, period_end)]

# Generate ID-life stage
assolife[, idlife := paste(hyena, period, sep = '-')]


## Merge all lifestages in 'life' to affiliation data --
# TODO: do we want to merge the life stage of the solicitor or the receiver?
allstages <- merge(affil, life,
									 by.x = 'll_solicitor', by.y = 'ego',
									 allow.cartesian = TRUE)

# Compare sessiondate to period start+end
warning(affil[is.na(sessiondate), .N], ' NAs in sessiondate dropped')
affillife <- allstages[between(sessiondate, period_start, period_end)]

# Generate ID-life stage
affillife[, idlife := paste(ll_solicitor, period, sep = '-')]

### Output ----
# Check output data
check_dup <- function(DT) {
	if (anyDuplicated(DT) != 0) stop('duplicated rows found in asso')
}
check_na <- function(DT, col = 'idlife') {
	if (DT[is.na(get(col)), .N] != 0) stop('NAs found in ', col)
}

check_dup(assolife); check_na(assolife)
check_dup(affillife); check_na(affillife)
check_dup(aggrlife); check_na(aggrlife)

# Output to derived-data
saveRDS(assolife, 'data/derived-data/association-lifestages.Rds')
saveRDS(affillife, 'data/derived-data/affiliation-lifestages.Rds')
saveRDS(aggrlife, 'data/derived-data/aggression-lifestages.Rds')
