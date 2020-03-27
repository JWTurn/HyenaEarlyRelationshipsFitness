### Hyena Affiliation Networks ====
# Alec Robitaille
# Started: March 01 2019

### Packages ----
libs <- c('data.table', 'spatsoc', 'asnipe', 'igraph', 'foreach', 'doParallel')
lapply(libs, require, character.only = TRUE)

### Import data ----
derived <- dir('data/derived-data', full.names = TRUE)

# Affiliation
affil <- readRDS(derived[grepl('prep-affil', derived)])

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
affil[, idate := sessiondate]

# Count number of (directed) affiliations between individuals
countLs <- foreach(i = seq(1, nrow(life))) %dopar% {
	focal <- affil[life[i],
								 on = .(sessiondate >= period_start,
								 			 sessiondate < period_end)]

	focal[, N := .N, by = .(ll_receiver, ll_solicitor)]
	focal[, .(ll_receiver, ll_solicitor, period_length,
						N, affilRate = N / period_length)]
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
	merge(countLs[[i]], sri, by.x = c('ll_receiver', 'll_solicitor'),
				by.y = c('id1', 'variable'), all.x = TRUE)
}

# Set na action to exlude to ensure NAs in res are padded
options(na.action = "na.exclude")

range01 <- function(x) {
	(x - min(x)) / (max(x) - min(x))
}

# Generate graph and calculate network metrics
mets <- foreach(i = seq_along(edgeLs)) %dopar% {
	# Affiliation counts and SRI in edgeLs
	sub <- edgeLs[[i]][value != 0]

	# Calculate residuals from affiliation rate ~ SRI
	sub[, res := residuals(glm(affilRate ~ sri, family = 'binomial'),
												 type = 'deviance')]

	sub[, res01 := range01(res)]

	# Generate the graph
	g <- graph_from_data_frame(sub[, .(ll_solicitor, ll_receiver)],
														 directed = TRUE)

	# Set edge weight to residuals
	w <- sub$res01
	E(g)$weight <- w

	return(cbind(
		data.table(
			affil_degree = degree(g, mode = 'total'),
			affil_outdegree = degree(g, mode = 'out'),
			affil_indegree = degree(g, mode = 'in'),
			affil_strength = strength(g, mode = 'total', weights = (w)),
			affil_outstrength = strength(g, mode = 'out', weights = (w)),
			affil_instrength = strength(g, mode = 'in', weights = (w)),
			affil_betweenness = betweenness(g, directed = TRUE, weights = (1/w)),
			ID = names(degree(g))
		),
		life[i]
	))
}

out <- rbindlist(mets, idcol = TRUE)
setnames(out, 'ID', idCol)

egos <- out[hyena == ego]

### Output ----
allegos <- merge(life, egos,
								 by = colnames(life), all.x = TRUE)

saveRDS(allegos, 'data/derived-data/affiliation-metrics.Rds')

###
message('=== AFFILIATION NETWORK COMPLETE ===')
