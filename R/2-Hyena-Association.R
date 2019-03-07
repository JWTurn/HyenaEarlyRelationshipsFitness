### Hyena/Spatsoc - Association ====
# Alec Robitaille
# March 01 2019

### Notes ----
# undirected, Twice weight index

### Packages ----
libs <- c('data.table', 'spatsoc', 'asnipe', 'igraph', 'foreach')
lapply(libs, require, character.only = TRUE)

### Import data ----
derived <- dir('data/derived-data', full.names = TRUE)
raw <- dir('data/raw-data', full.names = TRUE)

asso <- readRDS(derived[grepl('association-life', derived)])

## Life stages
life <- fread(raw[grepl('lifeperiods.csv', raw)], drop = 'V1')

### Prep ----
# Date columns
asso[, sessiondate := as.IDate(sessiondate)]
asso[, yr := year(sessiondate)]

periods <- c('period_start', 'period_end')
life[, (periods) := lapply(.SD, as.IDate), .SDcols = (periods)]


# Cast session to an integer group column
asso[, group := .GRP, session]

# Keep only relevant columns
life <- life[, .(ego, period, period_start, period_end)]

groupCol <- 'group'
idCol <- 'hyena'

### For each ego*life stage ----
# life <- life[1:5]

# To avoid the merge dropping out sessiondate to sessiondate and sessiondate.i (matching period start and end), we'll add it as an extra column and disregard those later
asso[, idate := sessiondate]

# Loop through each ego's life stages
system.time(gbiLs <- lapply(seq(1, nrow(life)), function(i) {
	sub <- asso[life[i],
							on = .(sessiondate >= period_start,
										 sessiondate < period_end)]
	sub[, id := ifelse(hyena == life[i]$ego,
										 idlife, hyena)]
	get_gbi(sub, group = groupCol, id = 'id')
}))

doParallel::registerDoParallel()
system.time(gbiLs <- foreach(i = seq(1, nrow(life))) %dopar% {
	sub <- asso[life[i],
							on = .(sessiondate >= period_start,
										 sessiondate < period_end)]
	sub[, id := ifelse(hyena == life[i]$ego,
										 idlife, hyena)]
	get_gbi(sub, group = groupCol, id = 'id')
})

netLs <- lapply(gbiLs, FUN = get_network,
								data_format = "GBI",
								association_index = "HWI")

## Generate graph and calculate network metrics
mets <- lapply(seq_along(netLs), function(n) {
	g <- graph.adjacency(netLs[[n]], 'undirected',
											 diag = FALSE, weighted = TRUE)

	# TODO: Are these the network metrics you want? Add them here...
	# degree, outdegree, indegree, strength, outstrength, instrength, betweenness
	cbind(data.table(
		degree = degree(g),
		strength = strength(g),
		betweenness = betweenness(g, directed = FALSE),
		ID = names(degree(g))
	), life[n])
})

## Observed and random for all individuals across all iterations and years
out <- rbindlist(mets)
setnames(out, 'ID', idCol)

## Split observed and random
out[, observed := ifelse(iteration == 0, TRUE, FALSE)]

## Mean values for each individual and year, by observed/random
meanMets <- out[, lapply(.SD, mean), by = c(idCol, 'yr', 'observed'),
								.SDcols = colnames(out)[1:7]]

saveRDS(meanMets, 'data/derived-data/mean-mets-association.Rds')
