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

### Prep data ----
# Keep only relevant columns
life <- life[, .(ego, period, period_start, period_end)]

periods <- c('period_start', 'period_end')
life[, (periods) := lapply(.SD, as.IDate), .SDcols = (periods)]

# Filter egos
life <- life[ego %chin% egos$ego]

### Output ----
# Output to derived-data
saveRDS(life, 'data/derived-data/filtered-ego-lifestages.Rds')
