### Prep Data ====
# Alec Robitaille
# Started: March 05 2019

### Packages ----
libs <- c('data.table')
lapply(libs, require, character.only = TRUE)

### Import data ----
raw <- dir('data/raw-data', full.names = TRUE)

# Life stages
life <- fread(raw[grepl('lifeperiod.csv', raw)], drop = 'V1')

# Network data
asso <- fread(raw[grepl('asso', raw)], drop = 'V1')
affil <- fread(raw[grepl('affil', raw)], drop = 'V1')
aggr <- fread(raw[grepl('aggressions.csv', raw)], drop = 'V1')

### Join life stages ----
## Set keys on lifestages
allstages <- merge(asso, life,
									 by.x = 'hyena', by.y = 'ego',
									 allow.cartesian = TRUE)
assolife <- allstages[between(sessiondate, period_start, period_end)]

### Output ----
rmCols <-
	c(
		'period_start',
		'period_end',
		'mom_alive',
		'prey_level',
		'ego_period_rank',
		"seq",
		"mom_period_rank",
		"clan_size",
		"sessions_count",
		"sessions_alone"
	)
assolife[, (rmCols) := NULL]
colnames(assolife)
