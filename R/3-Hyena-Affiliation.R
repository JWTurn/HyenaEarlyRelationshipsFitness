### Hyena/Spatsoc - Affiliation ====
# Alec Robitaille
# March 01 2019

### Notes ----
# directed, count of affiliations during period/AI during period


### Packages ----
libs <- c('data.table', 'spatsoc', 'asnipe', 'igraph', 'foreach')
lapply(libs, require, character.only = TRUE)

### Import data ----
raw <- dir('data/raw-data', full.names = TRUE)
derived <- dir('data/derived-data', full.names = TRUE)

# Affiliation
affil <- fread(raw[grepl('affil', raw)], drop = 'V1')

# Life stages
life <- readRDS(derived[grepl('ego', derived)])


### Prep ----
# Date columns
affil[, sessiondate := as.IDate(sessiondate)]
affil[, yr := year(sessiondate)]

periods <- c('period_start', 'period_end')
life[, (periods) := lapply(.SD, as.IDate), .SDcols = (periods)]

# Cast session to an integer group column
# affil[, group := .GRP, session]

# Keep only relevant columns
life <- life[, .(ego, period, period_start, period_end)]

groupCol <- 'group'
idCol <- 'hyena'


### Make networks for each ego*life stage ----
# Set up parallel with doParallel and foreach
doParallel::registerDoParallel()

life <- life[1:5]

# To avoid the merge dropping out sessiondate to sessiondate and sessiondate.i (matching period start and end), we'll add it as an extra column and disregard those later
affil[, idate := sessiondate]


# 1 = DT 154 edges

# Count number of (directed) affiliations between individuals
countAffil <- foreach(i = seq(1, nrow(life))) %dopar% {
	affil[life[i],
				on = .(sessiondate >= period_start,
							 sessiondate < period_end)][, .N, .(ll_solicitor, ll_reciever)]
}

# Generate a GBI for each ego's life stage
gbiLs <- foreach(i = seq(1, nrow(life))) %dopar% {
	sub <- asso[life[i],
							on = .(sessiondate >= period_start,
										 sessiondate < period_end)]

	get_gbi(sub[hyena %chin% sub[, .N, idCol][N > 10, get(idCol)]],
					groupCol, idCol)
}

# Calculate TWI
source('R/twi.R')
netLs <- foreach(g = gbiLs) %dopar% {
	twi(g)
}


# Generate list of networks
netLs <- foreach(g = gbiLs) %dopar% {
	get_network(g, data_format = "GBI", association_index = "HWI")
}

# Generate graph and calculate network metrics
mets <- foreach(n = seq_along(netLs)) %dopar% {
	g <- graph.adjacency(netLs[[n]], 'undirected',
											 diag = FALSE, weighted = TRUE)

	cbind(data.table(
		degree = degree(g),
		strength = strength(g),
		betweenness = betweenness(g, directed = FALSE),
		ID = names(degree(g))
	), life[n])
}

out <- rbindlist(mets)
setnames(out, 'ID', idCol)


### Output ----
out
