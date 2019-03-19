### Hyena/Spatsoc - Aggression ====
# Alec Robitaille
# March 01 2019

### Notes ----
# directed, average of behavior1 during period/AI during period

### Packages ----
libs <- c('data.table', 'spatsoc', 'asnipe', 'igraph', 'foreach')
lapply(libs, require, character.only = TRUE)

### Import data ----
raw <- dir('data/raw-data', full.names = TRUE)
derived <- dir('data/derived-data', full.names = TRUE)

# Aggression
aggr <- fread(raw[grepl('data_aggr', raw)], drop = 'V1')

# Life stages
life <- readRDS(derived[grepl('ego', derived)])

# Association
asso <- fread(raw[grepl('asso', raw)], drop = 'V1')

### Prep ----
aggr[, sessiondate := as.IDate(sessiondate)]
aggr[, aggressiontime := as.ITime(aggressiontime)]

aggr[, behavior1 := as.numeric(behavior1)]

asso[, sessiondate := as.IDate(sessiondate)]
asso[, yr := year(sessiondate)]

asso[, group := .GRP, session]

# Keep only relevant columns
life <- life[, .(ego, period, period_start, period_end)]

### Make networks for each ego*life stage ----
# Set up parallel with doParallel and foreach
doParallel::registerDoParallel()

life <- life[1:5]

groupCol <- 'group'
idCol <- 'hyena'

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
	g <- graph_from_data_frame(edgeLs[[i]][, .(aggressor, recip)],
														 directed = TRUE)

	# average of behavior1 during period/AI during period
	E(g)$weight <- edgeLs[[i]][, .(w  = avgB1 / value)]

	return(cbind(
		data.table(
			outdegree = degree(g, mode = 'out'),
			indegree = degree(g, mode = 'in'),
			# outstrength = strength(g, mode = 'out'),
			# instrength = strength(g, mode = 'in'),
			# TODO: do we need the edge weighting formula again?
			# betweenness = betweenness(g, directed = TRUE),
			ID = names(degree(g))
		),
		life[i]
	))
}

out <- rbindlist(mets)
setnames(out, 'ID', idCol)

out <- out[hyena == ego]

### Output ----
out %>% knitr::kable()


saveRDS(out, 'data/derived-data/aggression-metrics.Rds')
