### Hyena Association Networks ====
# Julie Turner and Alec Robitaille
# Revisions: April 02 2020

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
life.n <- merge(life, nsesh, by.x = c('ego', 'period'), by.y = c('hyena', 'period'))
life.n <- life.n[nSession>=50]

# To avoid the merge dropping out sessiondate to sessiondate and sessiondate.i (matching period start and end), we'll add it as an extra column and disregard those later
asso[, idate := sessiondate]



# Generate a GBI for each ego's life stage
gbiLs.50 <- foreach(i = seq(1, nrow(life.n))) %dopar% {
	sub <- asso[life.n[i],
							on = .(sessiondate >= period_start,
										 sessiondate < period_end)]
	sesh.50 <- sub[, .(session = sample(unique(session), 50))]
	sub.50 <- sub[session %in% sesh.50$session]

	# Filter out < 10
	get_gbi(sub[hyena %chin% sub[, .N, idCol][N > 10, get(idCol)]],
					groupCol, idCol)
}

# Calculate SRI
sriLs.50 <- foreach(g = gbiLs) %dopar% {
	get_network(g, 'GBI', 'SRI')
}

# Generate graph and calculate network metrics
mets.50 <- foreach(n = seq_along(sriLs)) %dopar% {
	g <- graph.adjacency(sriLs[[n]], 'undirected',
											 diag = FALSE, weighted = TRUE)

	w <- E(g)$weight
	cbind(data.table(
		sri_degree = degree(g),
		sri_strength = strength(g, weights = w),
		sri_betweenness = betweenness(g, directed = FALSE, weights = 1/w),
		ID = names(degree(g))
	), life[n])
}

out.50 <- rbindlist(mets.50)

setnames(out.50, 'ID', idCol)

out.50 <- out.50[hyena == ego]


######
out <- merge(out, nsesh, by = c('hyena', 'period'))




