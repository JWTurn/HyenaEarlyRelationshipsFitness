### Hyena Network Randomizations ====
# Alec Robitaille
# Started: March 01 2019


### Packages ----
libs <- c('data.table', 'spatsoc', 'asnipe', 'igraph', 'foreach')
lapply(libs, require, character.only = TRUE)

### Import data ----
derived <- dir('data/derived-data', full.names = TRUE)

# Life stages
life <- readRDS(derived[grepl('ego-life', derived)])

# Association
asso <- readRDS(derived[grepl('prep-asso', derived)])

# Aggression
aggr <- readRDS(derived[grepl('prep-aggr', derived)])

# Affiliation
affil <- readRDS(derived[grepl('prep-affil', derived)])

## Set column names
groupCol <- 'group'
idCol <- 'randHyena'

## Iterations
iterations <- 3

### Merge network data together ----
# Count the number of affiliations (edges) in each session
affil[, countAffil := .N, session]

# Count the number of individuals associating in each session
asso[, countAsso := .N, session]

# Count the number of aggressions (edges) in each session
aggr[, countAggr := .N, session]

### Directed randomizations function ----
randomizations.directed <- function(DT, id, count, by, nms) {
	DT[, (nms) := {
		size <- get(count)[[1]]
		ids <- get(id)

		if (length(unique(ids)) > size) {
			l <- sample(ids, size = size)
			r <- sample(ids, size = size)

			while (any(l == r)) {
				l <- sample(ids, size = size)
				r <- sample(ids, size = size)
			}
			list(l, r)
		}
	}, by = by, .SDcols = c(id, count)]
}

### Randomize affiliation networks ----
source('R/twi.R')

# Set up parallel with doParallel and foreach
doParallel::registerDoParallel()

# Randomization --------------------------------------------------
randMets <- foreach(iter = seq(1, iterations)) %dopar% {
	# Randomize association IDs
	asso[, randHyena := sample(hyena)]

	## Affiliation -------------------------------------------------
	# Merge affiliation and association
	DT <- merge(
		affil, asso,
		by = 'session', allow.cartesian = TRUE
	)

	# Match the sessiondates/yrs to association data
	DT[, c('sessiondate', 'yr') := .(sessiondate.y, yr.y)]

	# Set output names of left and right randomized IDs
	nms <- c('ll_receiver', 'll_solicitor')

	# Randomize affiliation data
	randAffil <- randomizations.directed(
		DT, id = 'randHyena', count = 'countAffil',
		by = 'session', nms = nms
	)

	# Count matching edges
	countLs <- foreach(i = seq(1, nrow(life))) %do% {
		focal <- randAffil[life[i],
									 on = .(sessiondate >= period_start,
									 			 sessiondate < period_end)]
		focal[, .N, .(ll_receiver, ll_solicitor)]
	}

	## Aggression -------------------------------------------------
	# Merge aggression and association
	DT <- merge(
		aggr, asso,
		by = 'session', allow.cartesian = TRUE
	)

	# Match the sessiondates/yrs to association data
	DT[, sessiondate := sessiondate.y]

	# Set output names of left and right randomized IDs
	nms <- c('aggressor', 'recip')

	# Randomize aggression data
	randAggr <- randomizations.directed(
		DT, id = 'randHyena', count = 'countAggr',
		by = 'session', nms = nms
	)

	#  average of behavior1 during period
	avgLs <- foreach(i = seq(1, nrow(life))) %do% {
		focal <- randAggr[life[i],
									on = .(sessiondate >= period_start,
												 sessiondate < period_end)]
		focal[, .(avgB1 = mean(behavior1)), by = .(aggressor, recip)]
	}

	## Association -------------------------------------------------
	# Generate a GBI for each ego's life stage
	gbiLs <- foreach(i = seq(1, nrow(life))) %do% {
		sub <- asso[life[i],
								on = .(sessiondate >= period_start,
											 sessiondate < period_end)]

		# Filter out < 10
		get_gbi(sub[get(idCol) %chin% sub[, .N, idCol][N > 10, get(idCol)]],
						groupCol, idCol)
	}

	# Calculate TWI
	twiLs <- foreach(g = gbiLs) %do% {
		twi(g)
	}

	## Combine edges -----------------------------------------------
	edgeLs <- foreach(i = seq(1, nrow(life))) %do% {
		twi <- data.table(melt(twiLs[[i]]), stringsAsFactors = FALSE)
		twi[, c('Var1', 'Var2') := lapply(.SD, as.character), .SDcols = c(1, 2)]
		# TODO: add aggression
		merge(countLs[[i]], twi, by.x = c('ll_receiver', 'll_solicitor'),
					by.y = c('Var1', 'Var2'), all.x = TRUE)
	}

	# Generate graph and calculate network metrics
	mets <- foreach(i = seq_along(edgeLs)) %do% {
		sub <- edgeLs[[i]][value != 0]
		g <- graph_from_data_frame(sub[, .(ll_solicitor, ll_receiver)],
															 directed = TRUE)
		w <- sub[, N / value]
		E(g)$weight <- w

		# TODO: add aggression
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
			life[i],
			iteration = iter
		)[ID == ego])
	}
}

out <- rbindlist(do.call(c, randMets))
out
