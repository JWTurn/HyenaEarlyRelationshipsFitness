### Hyena Network Randomizations ====
# Alec Robitaille
# Started: March 01 2019


### Hyena Affiliation Networks ====
# Alec Robitaille
# Started: March 01 2019

### Packages ----
libs <- c('data.table', 'spatsoc', 'asnipe', 'igraph', 'foreach')
lapply(libs, require, character.only = TRUE)

### Import data ----
derived <- dir('data/derived-data', full.names = TRUE)

# Life stages
life <- readRDS(derived[grepl('ego-life', derived)])

# Association
asso <- readRDS(derived[grepl('prep-asso', derived)])

# Aggression
aggr <- readRDS(derived[grepl('prep-aggr', derived)])

# Affiliation
affil <- readRDS(derived[grepl('prep-affil', derived)])

## Set column names
groupCol <- 'group'
idCol <- 'hyena'


### Merge network data together ----
keep <- colnames(asso)
assoaggr <- merge(
	asso, aggr, allow.cartesian = TRUE,
	by = 'session'
)
keep <- c(keep, 'aggressor', 'recip', 'behavior1')
DT <- merge(
	assoaggr, affil,
	by = 'session', allow.cartesian = TRUE
)
keep <- c(keep, 'll_solicitor', 'll_receiver')

# TODO: why are there 2000 rows where the session
DT <- DT[, .SD, .SDcols = keep]

