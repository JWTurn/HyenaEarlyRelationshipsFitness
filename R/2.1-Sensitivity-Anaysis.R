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

asso.2011.50 <- asso[yr==2011 & hyena %in% life.sub.2011, .(session = sample(unique(session), 50)), by = .(hyena)]
asso.2010.50 <- asso[yr==2010 & hyena %in% life.sub.2010, .(session = sample(unique(session), 50)), by = .(hyena)]

ego.50 <- rbind(asso.2010.50, asso.2011.50)
ego.45 <- ego.50[, .(session = sample(unique(session), 45)), by = .(hyena)]
ego.40 <- ego.45[, .(session = sample(unique(session), 40)), by = .(hyena)]
ego.35 <- ego.40[, .(session = sample(unique(session)), 35), by = .(hyena)]
ego.30 <- ego.35[, .(session = sample(unique(session), 30)), by = .(hyena)]
ego.25 <- ego.30[, .(session = sample(unique(session), 25)), by = .(hyena)]
ego.20 <- ego.25[, .(session = sample(unique(session), 20)), by = .(hyena)]
ego.15 <- ego.20[, .(session = sample(unique(session), 15)), by = .(hyena)]
ego.10 <- ego.15[, .(session = sample(unique(session)), 10), by = .(hyena)]
ego.5 <- ego.10[, .(session = sample(unique(session), 5)), by = .(hyena)]

asso <- asso[session %in% unique(ego.50$session)]

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




