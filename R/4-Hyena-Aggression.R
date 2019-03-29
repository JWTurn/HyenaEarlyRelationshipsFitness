### Hyena Aggression Networks ====
# Alec Robitaille
# Started: March 01 2019

### Packages ----
libs <- c('data.table', 'spatsoc', 'asnipe', 'igraph', 'foreach')
lapply(libs, require, character.only = TRUE)

### Import data ----
derived <- dir('data/derived-data', full.names = TRUE)

# Aggression
aggr <- readRDS(derived[grepl('prep-aggr', derived)])

# Life stages
life <- readRDS(derived[grepl('ego-life', derived)])

# Association
asso <- readRDS(derived[grepl('prep-asso', derived)])

## Set column names
groupCol <- 'group'
idCol <- 'hyena'

### Make networks for each ego*life stage ----
# Set up parallel with doParallel and foreach
doParallel::registerDoParallel()

# To avoid the merge dropping out sessiondate to sessiondate and sessiondate.i (matching period start and end), we'll add it as an extra column and disregard those later
aggr[, idate := sessiondate]

#  average of behavior1 during period
avgLs <- foreach(i = seq(1, nrow(life))) %dopar% {
	focal <- aggr[life[i],
								 on = .(sessiondate >= period_start,
								 			 sessiondate < period_end)]
	focal[, .(avgB1 = mean(behavior1)), by = .(aggressor, recip)]
}

# Generate a GBI for each ego's life stage
gbiLs <- foreach(i = seq(1, nrow(life))) %dopar% {
	sub <- asso[life[i],
							on = .(sessiondate >= period_start,
										 sessiondate < period_end)]

	# Filter out < 10
	get_gbi(sub[get(idCol) %chin% sub[, .N, idCol][N > 10, get(idCol)]],
					groupCol, idCol)
}

# Calculate TWI
source('R/twi.R')
twiLs <- foreach(g = gbiLs) %dopar% {
	twi(g)
}

# Create edge list
edgeLs <- foreach(i = seq(1, nrow(life))) %dopar% {
	twi <- data.table(melt(twiLs[[i]]), stringsAsFactors = FALSE)
	twi[, c('Var1', 'Var2') := lapply(.SD, as.character), .SDcols = c(1, 2)]
	merge(avgLs[[i]], twi,
				by.x = c('aggressor', 'recip'),
				by.y = c('Var1', 'Var2'), all.x = TRUE)
}

# Generate graph and calculate network metrics
mets <- foreach(i = seq_along(edgeLs)) %dopar% {
	sub <- edgeLs[[i]][value != 0 & (value / avgB1) != 0]
	g <- graph_from_data_frame(sub[, .(aggressor, recip)],
														 directed = TRUE)

	# average of behavior1 during period/AI during period
	w <- sub[, avgB1 / value]
	E(g)$weight <- w

	return(
		cbind(
		data.table(
			aggr_degree = degree(g, mode = 'total'),
			aggr_outdegree = degree(g, mode = 'out'),
			aggr_indegree = degree(g, mode = 'in'),
			aggr_strength = strength(g, mode = 'total', weights = (w)),
			aggr_outstrength = strength(g, mode = 'out', weights = (w)),
			aggr_instrength = strength(g, mode = 'in', weights = (w)),
			aggr_betweenness = betweenness(g, directed = TRUE,
																		 weights = (1/w)),
			ID = names(degree(g))
		),
		life[i]
	))
}

out <- rbindlist(mets)

### Output ----
setnames(out, 'ID', idCol)

egos <- out[hyena == ego]

allegos <- merge(life, egos,
								 by = colnames(life), all.x = TRUE)

saveRDS(allegos, 'data/derived-data/aggression-metrics.Rds')


###
message('=== AGGRESSION NETWORK COMPLETE ===')
