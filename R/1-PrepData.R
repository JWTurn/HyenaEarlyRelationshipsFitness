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
# Merge all lifestages in 'life' to association data
allstages <- merge(asso, life,
									 by.x = 'hyena', by.y = 'ego',
									 allow.cartesian = TRUE)
# Compare sessiondate to period start+end
assolife <- allstages[between(sessiondate, period_start, period_end)]

### Output ----
# Drop excess columns
rmColsAsso <- c('period_start', 'period_end', 'mom_alive',
								'prey_level', 'ego_period_rank', 'seq',
								'mom_period_rank', 'clan_size', 'sessions_count',
								'sessions_alone')
assolife[, (rmColsAsso) := NULL]

# Output to derived-data
saveRDS(assolife, 'data/derived-data/association-lifestages.Rds')
