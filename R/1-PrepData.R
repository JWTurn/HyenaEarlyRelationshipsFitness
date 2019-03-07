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
egos <- fread(raw[grepl('egos_filtered.csv', raw)])


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

# TODO: which egos to filter out?
life<-life[ego %chin% egos$ego] # these are just the
#setkey(life, period_start, period_end)
#drop <- foverlaps(life, life)[ego == i.ego & period != i.period]$ego

#asso <- asso[!(hyena %in% drop)]

#TODO: why?
setnames(affil, 'll_reciever', 'recip')

### Join association to life stages ----
# Merge all possible life stages to all egos (allow.cartesian), and retain all non-ego individuals (all.x)
allstages <- merge(asso, life,
									 by.x = 'hyena', by.y = 'ego',
									 all.x = TRUE, allow.cartesian = TRUE)
warning(asso[is.na(sessiondate), .N], ' NAs in sessiondate dropped')

# If not an ego, the 'idlife' column is just the name of the individual
allstages[!(hyena %in% life$ego), idlife := hyena]

# If the session time matches a period start-end, they are during that life stage
allstages[between(sessiondate, period_start, period_end),
					idlife := paste0(hyena, '-', period)]

# if ALL the rows that are leftover for an id/period date and time, then that individual must be outside of the range of their lifestages during that interaction. therefore idlife is just the ID
allstages[, none := all(is.na(idlife)), .(sessiondate, hyena)]
allstages[(none), idlife := hyena]

assolife <- unique(allstages[!is.na(idlife), .(hyena, session, sessiondate, idlife)])

warning('difference of ', nrow(asso) - nrow(assolife),
				' rows between input association and after merge with lifestages')



### Join affiliation to life stages ----
warning(affil[is.na(sessiondate), .N], ' NAs in sessiondate dropped')
## For solicitor --
# Merge all possible life stages to all egos (allow.cartesian), and retain all non-ego individuals (all.x)
allstages <- merge(affil, life,
									 by.x = 'll_solicitor', by.y = 'ego',
									 all.x = TRUE, allow.cartesian = TRUE)

# If not an ego, the 'idlife' column is just the name of the individual
allstages[!(ll_solicitor %in% life$ego), idlife_solicitor := ll_solicitor]

# If the session time matches a period start-end, they are during that life stage
allstages[between(sessiondate, period_start, period_end),
					idlife_solicitor := paste0(ll_solicitor, '-', period)]

# if ALL the rows that are leftover for an id/period date and time, then that individual must be outside of the range of their lifestages during that interaction. therefore idlife is just the ID
allstages[, none := all(is.na(idlife_solicitor)),
					.(sessiondate, grtTime, ll_solicitor)]
allstages[(none), idlife_solicitor := ll_solicitor]

# Drop duplicated rows from cartesian merge
affillife <- unique(allstages[!is.na(idlife_solicitor), .SD,
															.SDcols = c(colnames(affil), 'idlife_solicitor')])

## For receiver --
# Merge all possible life stages to all egos (allow.cartesian), and retain all non-ego individuals (all.x)
allstages <- merge(affillife, life,
									 by.x = 'recip', by.y = 'ego',
									 all.x = TRUE, allow.cartesian = TRUE)

# If not an ego, the 'idlife' column is just the name of the individual
allstages[!(recip %in% life$ego), idlife_receiver := recip]

# If the session time matches a period start-end, they are during that life stage
allstages[between(sessiondate, period_start, period_end),
					idlife_receiver := paste0(recip, '-', period)]

# if ALL the rows that are leftover for an id/period date and time, then that individual must be outside of the range of their lifestages during that interaction. therefore idlife is just the ID
allstages[, none := all(is.na(idlife_receiver)), .(sessiondate, recip)]
allstages[(none), idlife_receiver := recip]

# Drop duplicated rows from cartesian merge
affillife <- unique(allstages[!is.na(idlife_receiver) & !is.na(idlife_solicitor), .SD, .SDcols = c(colnames(affil), 'idlife_receiver', 'idlife_solicitor')])

warning('difference of ', nrow(affil) - nrow(affillife),
				' rows between input affiliation and after merge with lifestages')
# affil[, .N, by = .(sessiondate, grtTime, ll_solicitor, recip)][N > 1, .N]



### Join aggression to life stages ----
warning(aggr[is.na(sessiondate), .N], ' NAs in sessiondate dropped')

## For aggressor --
# Merge all possible life stages to all egos (allow.cartesian), and retain all non-ego individuals (all.x)
allstages <- merge(aggr, life,
									 by.x = 'aggressor', by.y = 'ego',
									 all.x = TRUE, allow.cartesian = TRUE)

# If not an ego, the 'idlife' column is just the name of the individual
allstages[!(aggressor %in% life$ego), idlife_aggressor := aggressor]

# If the session time matches a period start-end, they are during that life stage
allstages[between(sessiondate, period_start, period_end),
					idlife_aggressor := paste0(aggressor, '-', period)]

# if ALL the rows that are leftover for an id/period date and time, then that individual must be outside of the range of their lifestages during that interaction. therefore idlife is just the ID
allstages[, none := all(is.na(idlife_aggressor)), .(sessiondate, aggressor)]
allstages[(none), idlife_aggressor := aggressor]

# Drop duplicated rows from cartesian merge
aggrlife <- unique(allstages[!is.na(idlife_aggressor), .SD,
														 .SDcols = c(colnames(aggr), 'idlife_aggressor')])

## For recip --
# Merge all possible life stages to all egos (allow.cartesian), and retain all non-ego individuals (all.x)
allstages <- merge(aggrlife, life,
									 by.x = 'recip', by.y = 'ego',
									 all.x = TRUE, allow.cartesian = TRUE)

# If not an ego, the 'idlife' column is just the name of the individual
allstages[!(recip %in% life$ego), idlife_recip := recip]

# If the session time matches a period start-end, they are during that life stage
allstages[between(sessiondate, period_start, period_end),
					idlife_recip := paste0(recip, '-', period)]

# if ALL the rows that are leftover for an id/period date and time, then that individual must be outside of the range of their lifestages during that interaction. therefore idlife is just the ID
allstages[, none := all(is.na(idlife_recip)),
					.(sessiondate, aggressiontime, recip)]
allstages[(none), idlife_recip := recip]

# Drop duplicated rows from cartesian merge
aggrlife <- unique(
	allstages[!is.na(idlife_recip) & !is.na(idlife_aggressor),
						.SD,
						.SDcols = c(colnames(aggr), 'idlife_recip', 'idlife_aggressor')]
)

warning('difference of ', nrow(aggr) - nrow(aggrlife),
				' rows between input aggression and after merge with lifestages')
# affil[, .N, by = .(sessiondate, grtTime, ll_solicitor, recip)][N > 1, .N]



### Add multiple interactions in <= 1 minute back ----
## Affiliation
# Count number of rows for each date*time*solicitor*recip
affil[, nByMin := .N, .(sessiondate, grtTime, ll_solicitor, recip)]
affillife[, nByMin := .N, .(sessiondate, grtTime, ll_solicitor, recip)]

# Preserve colnames w/o nByMin
cols <- colnames(affil)[grep('nBy', colnames(affil), invert = TRUE)]

# Find the difference between input affil and merged affillife, where
affildif <- fsetdiff(affil[nByMin > 1, .SD, .SDcols = cols],
										 affillife[nByMin > 1, .SD, .SDcols = cols])

# Merge the life stage back on
mergedif <- merge(affildif, affillife, by = cols)

# Add the rows to the (affillife) merged life stages from above
affilout <- rbindlist(list(affillife, mergedif))

warning('difference of ', nrow(affil) - nrow(affilout),
				' rows between input association and after merge with lifestages')

## Aggression
# Count number of rows for each date*time*solicitor*recip
aggr[, nByMin := .N, .(sessiondate, aggressiontime, aggressor, recip)]
aggrlife[, nByMin := .N, .(sessiondate, aggressiontime, aggressor, recip)]

# Preserve colnames w/o nByMin
cols <- colnames(aggr)[grep('nBy', colnames(aggr), invert = TRUE)]

# Find the difference between input aggr and merged aggrlife, where
aggrdif <- fsetdiff(aggr[nByMin > 1, .SD, .SDcols = cols],
										 aggrlife[nByMin > 1, .SD, .SDcols = cols])

# Merge the life stage back on
mergedif <- merge(aggrdif, aggrlife, by = cols)

# Add the rows to the (aggrlife) merged life stages from above
aggrout <- rbindlist(list(aggrlife, mergedif))

warning('difference of ', nrow(aggr) - nrow(aggrout),
				' rows between input association and after merge with lifestages')


### Finding NAs ----
aggr[, cbind(.SD, rowSums(is.na(.SD)))][V2 > 0]
aggrout[, cbind(.SD, rowSums(is.na(.SD)))][V2 > 0]


### Finding duplicates ----
dup <- assolife[duplicated(assolife[, .SD, .SDcols = colnames(asso)])]
assodup <- assolife[dup[, .SD, .SDcols = colnames(asso)], on = colnames(asso)]

dup <- affilout[duplicated(affilout[, .SD, .SDcols = colnames(affil)])]
affildup <- affilout[dup[, .SD, .SDcols = colnames(affil)], on = colnames(affil)]

dup <- aggrout[duplicated(aggrout[, .SD, .SDcols = colnames(aggr)])]
aggrdup <- aggrout[dup[, .SD, .SDcols = colnames(aggr)], on = colnames(aggr)]

### Output ----
# Output to derived-data
saveRDS(assolife, 'data/derived-data/association-lifestages.Rds')
saveRDS(affilout, 'data/derived-data/affiliation-lifestages.Rds')
saveRDS(aggrout, 'data/derived-data/aggression-lifestages.Rds')


