### Hyena Association Networks ====
# Alec Robitaille
# Started: March 01 2019

### Packages ----
libs <- c('data.table', 'spatsoc', 'asnipe', 'igraph', 'foreach', 'doParallel')
lapply(libs, require, character.only = TRUE)


### Import data ----
derived <- dir('data/derived-data', full.names = TRUE)

# Associations
asso <- readRDS(derived[grepl('prep-asso', derived)])

# Life stages
life <- readRDS(derived[grepl('ego-life', derived)])

## Set column names
groupCol <- 'group'
idCol <- 'hyena'

# Set up parallel with doParallel and foreach
doParallel::registerDoParallel()

### Count number of sessions and alone session ----
# Generate a GBI for each ego's life stage
nsesh <- rbindlist(foreach(i = seq(1, nrow(life))) %dopar% {
	sub <- asso[life[i],
							on = .(sessiondate >= period_start,
										 sessiondate < period_end)]
	ego <- sub$ego[[i]]
	period <- sub$period[[i]]

	uasso <- unique(sub[, .(hyena, session)])
	uasso[, nSession := .N, session]
	nsesh <- uasso[, .(nSession = .N, nAlone = sum(nSession == 1)), hyena]
	return(cbind(nsesh[hyena == ego], period))
})

### Make networks for each ego*life stage ----
life <- life[1:5]

# To avoid the merge dropping out sessiondate to sessiondate and sessiondate.i (matching period start and end), we'll add it as an extra column and disregard those later
asso[, idate := sessiondate]

# Generate a GBI for each ego's life stage
gbiLs <- foreach(i = seq(1, nrow(life))) %dopar% {
	sub <- asso[life[i],
							on = .(sessiondate >= period_start,
										 sessiondate < period_end)]

	# Filter out < 10
	get_gbi(sub[hyena %chin% sub[, .N, idCol][N > 10, get(idCol)]],
					groupCol, idCol)
}

# Calculate TWI
source('R/twi.R')
twiLs <- foreach(g = gbiLs) %dopar% {
	twi(g)
}

# Generate graph and calculate network metrics
mets <- foreach(n = seq_along(twiLs)) %dopar% {
	g <- graph.adjacency(twiLs[[n]], 'undirected',
											 diag = FALSE, weighted = TRUE)

	cbind(data.table(
		twi_degree = degree(g),
		twi_strength = strength(g),
		twi_betweenness = betweenness(g, directed = FALSE,
															weights = (1/E(g)$weight)),
		ID = names(degree(g))
	), life[n])
}

out <- rbindlist(mets)
setnames(out, 'ID', idCol)

out <- out[hyena == ego]

out <- merge(out, nsesh, by = c('hyena', 'period'))

### Output ----
saveRDS(out, 'data/derived-data/association-metrics.Rds')

###
message('=== ASSOCIATION NETWORK COMPLETE ===')
