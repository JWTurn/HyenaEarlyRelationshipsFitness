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
	focal[, .(avgB1 = mean(behavior1), avgB1Len = mean(behavior1) / period_length,
						period_length = period_length[[1]]), by = .(aggressor, recip)]
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

# Calculate SRI
sriLs <- foreach(g = gbiLs) %dopar% {
	get_network(g, 'GBI', 'SRI')
}

# Create edge list
edgeLs <- foreach(i = seq(1, nrow(life))) %dopar% {
	sri <- melt(as.data.table(sriLs[[i]], keep.rownames = 'id1'), id.vars = 'id1')

	sri[, c('id1', 'variable') := lapply(.SD, as.character), .SDcols = c(1, 2)]
	setnames(sri, 'value', 'sri')
	merge(avgLs[[i]], sri,
				by.x = c('aggressor', 'recip'),
				by.y = c('id1', 'variable'), all.x = TRUE)
}

# Set na action to exlude to ensure NAs in res are padded
options(na.action = "na.exclude")

range01 <- function(x) {
	(x - min(x)) / (max(x) - min(x))
}

sumfunc <- function(x) {
	c(summary(x),
		sd = sd(x),
		se = sd(x) / sqrt(length(x)))
}

# Generate graph and calculate network metrics
mets <- foreach(i = seq_along(edgeLs)) %dopar% {
	# Aggression counts and SRI in edgeLs
	sub <- edgeLs[[i]][sri != 0 & (sri / avgB1Len) != 0]

	# Calculate residuals from affiliation rate ~ SRI
	sub[, res := residuals(glm(avgB1Len ~ sri, family = 'binomial'),
												 type = 'deviance')]

	sub[, res01 := range01(res)]

	# Summary stats for res and res01
	sumres <- sumfunc(sub$res)
	sumres01 <- sumfunc(sub$res01)

	# Generate graph
	g <- graph_from_data_frame(sub[, .(aggressor, recip)],
														 directed = TRUE)

	# average of behavior1 during period/AI during period
	w <- sub$res01
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
