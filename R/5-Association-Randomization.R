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

## Iterations
iterations <- 3

### Merge network data together ----
# Count the number of affiliations (edges) in each session
affil[, countAffil := .N, session]

# Count the number of individuals associating in each session
asso[, countAsso := .N, session]


# Merge all individuals observed in each session
#  from associations, onto affiliations
DT <- merge(
	affil, asso,
	by = 'session', allow.cartesian = TRUE
)
DT[, c('sessiondate', 'yr') := .(sessiondate.x, yr.x)]

randomizations.directed <- function(DT, id, count, by) {
	DT[, {
		size <- .SD[[2]][[1]]
		if (length(unique(.SD[[1]])) > size) {
			l <- sample(.SD[[1]], size = size)
			r <- sample(.SD[[1]], size = size)

			while (any(l == r)) {
				l <- sample(.SD[[1]], size = size)
				r <- sample(.SD[[1]], size = size)
			}
			list(ll_solicitor = l, ll_receiver = r)
		}
	}, by = by, .SDcols = c(id, count)]
}

lapply(seq(1, iterations), function(i) {
	rand <- randomizations.directed(
		DT, id = 'hyena', count = 'countAffil', by = 'session'
	)


})


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

