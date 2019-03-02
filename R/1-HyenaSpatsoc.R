### Hyena/Spatsoc ====
# Alec Robitaille
# March 01 2019

### Packages ----
libs <- c('data.table', 'spatsoc', 'asnipe', 'igraph')
lapply(libs, require, character.only = TRUE)

### Import data ----
raw <- dir('data/raw-data', full.names = TRUE)

affil <- fread(raw[grepl('affil', raw)])
aggr <- fread(raw[grepl('agg', raw)])
asso <- fread(raw[grepl('asso', raw)])
egos <- fread(raw[grepl('egos', raw)])

### Prep ----
affil[, sessiondate := as.IDate(sessiondate)]
affil[, grtTime := as.ITime(grtTime)]

aggr[, sessiondate := as.IDate(sessiondate)]
aggr[, aggressiontime := as.ITime(aggressiontime)]

asso[, sessiondate := as.IDate(sessiondate)]

egos[, period_start := as.IDate(period_start)]
egos[, period_end := as.IDate(period_end)]

### Association ----
# Cast session to an integer group column
asso[, group := .GRP, session]

# TODO: why nulls in sessiondate?
asso <- asso[!is.na(year(sessiondate))]
yearLs <- asso[, unique(year(sessiondate))]

netLs <- lapply(yearLs, function(yr) {
	# Build group by individual matrix
	gbiMtrx <- get_gbi(asso[year(sessiondate) == yr],
										 group = 'group', id = 'hyena')

	## Generate observed network
	#TODO: what association index?
	net <- get_network(gbiMtrx,
										 data_format = "GBI",
										 association_index = "SRI")
})

mets <- lapply(seq_along(netLs), function(n) {
	g <- graph_from_adjacency_matrix(netLs[[n]], 'undirected',
											 diag = FALSE, weighted = TRUE)

	#TODO: which network metrics?
	data.table(
		centrality = eigen_centrality(g)$vector,
		strength = strength(g),
		ID = names(degree(g)),
		yr = yearLs[[n]]
	)
})

association <- rbindlist(mets)
association
