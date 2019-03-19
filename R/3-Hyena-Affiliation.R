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

# Association
asso <- fread(raw[grepl('asso', raw)], drop = 'V1')

### Prep ----
# Date columns
affil[, sessiondate := as.IDate(sessiondate)]
affil[, yr := year(sessiondate)]

periods <- c('period_start', 'period_end')
life[, (periods) := lapply(.SD, as.IDate), .SDcols = (periods)]

# Cast session to an integer group column
# affil[, group := .GRP, session]

asso[, sessiondate := as.IDate(sessiondate)]
asso[, yr := year(sessiondate)]

asso[, group := .GRP, session]


# Keep only relevant columns
life <- life[, .(ego, period, period_start, period_end)]

groupCol <- 'group'
idCol <- 'hyena'

setnames(affil, 'll_reciever', 'll_receiver')

### Make networks for each ego*life stage ----
# Set up parallel with doParallel and foreach
doParallel::registerDoParallel()

life <- life[1:5]

# To avoid the merge dropping out sessiondate to sessiondate and sessiondate.i (matching period start and end), we'll add it as an extra column and disregard those later
affil[, idate := sessiondate]


# Count number of (directed) affiliations between individuals
countLs <- foreach(i = seq(1, nrow(life))) %dopar% {
	focal <- affil[life[i],
								 on = .(sessiondate >= period_start,
								 			 sessiondate < period_end)]
	focal[, .N, .(ll_receiver, ll_solicitor)]
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
	merge(countLs[[i]], twi, by.x = c('ll_receiver', 'll_solicitor'),
	by.y = c('Var1', 'Var2'), all.x = TRUE)
}

# Generate graph and calculate network metrics
mets <- foreach(i = seq_along(edgeLs)) %dopar% {
	g <- graph_from_data_frame(edgeLs[[i]][, .(ll_solicitor, ll_receiver)],
														 directed = TRUE)
	# TODO: should the edge weight be inverted here too?
	w <- edgeLs[[i]][, N / value]
	E(g)$weight <- w

	return(cbind(
		data.table(
			affil_degree = degree(g, mode = 'total'),
			affil_outdegree = degree(g, mode = 'out'),
			affil_indegree = degree(g, mode = 'in'),
			affil_strength = strength(g, mode = 'total'),
			affil_outstrength = strength(g, mode = 'out'),
			affil_instrength = strength(g, mode = 'in'),
			affil_betweenness = betweenness(g, directed = TRUE,
																weights = (1/w)),
			ID = names(degree(g))
		),
		life[i]
	))
}

out <- rbindlist(mets)
setnames(out, 'ID', idCol)

out <- out[hyena == ego]

### Output ----
out <- merge(life, out,
			by = colnames(life), all.x = TRUE)

knitr::kable(out)

saveRDS(out, 'data/derived-data/affiliation-metrics.Rds')
