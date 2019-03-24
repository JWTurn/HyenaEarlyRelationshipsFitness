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

# NOTE: there are 75 sessions with mismatching sessiondates, and some mismatching years
DT[, sessiondate := sessiondate.x]
DT[, yr := yr.x]
DT <- DT[, .SD, .SDcols = keep]


### Fill NAs ----
# Where hyena is not either the aggressor or the recipient
DT[hyena != aggressor & hyena != recip,
	 c('aggressor', 'recip') := NA]

DT[hyena != ll_solicitor & hyena != ll_receiver,
	 c('ll_solicitor', 'll_receiver') := NA]

# TODO: use all AND unique to gather all individuals in each session, from association, affiliation and aggression
# TODO: fill with NAs wherever repeated (careful with direction and repeated affil/aggressions)
# TODO: randomize both columns of the directed networks

