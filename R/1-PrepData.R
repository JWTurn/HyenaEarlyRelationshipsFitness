### Prep Hyena Data ====
# Alec Robitaille
# Started: March 05 2019

### Packages ----
libs <- c('data.table')
lapply(libs, require, character.only = TRUE)

### Import data ----
raw <- dir('data/raw-data', full.names = TRUE)

# Life stages
life <- fread(raw[grepl('lifeperiods.csv', raw)], drop = 'V1')

# Egos
egos <- fread(raw[grepl('egos_filt', raw)])

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
