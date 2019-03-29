### Hyena Network Randomizations ====
# Alec Robitaille
# Started: March 01 2019


### Packages ----
libs <- c('data.table', 'spatsoc', 'asnipe', 'igraph', 'foreach')
lapply(libs, require, character.only = TRUE)

### Import data ----
derived <- 'data/derived-data/'
der <- dir(derived, full.names = TRUE)

# Life stages
life <- readRDS(der[grepl('ego-life', der)])

# Association
asso <- readRDS(der[grepl('prep-asso', der)])

# Aggression
aggr <- readRDS(der[grepl('prep-aggr', der)])

# Affiliation
affil <- readRDS(der[grepl('prep-affil', der)])

## Set column names
groupCol <- 'group'
idCol <- 'ID'

## Iterations
iterations <- 2


### Count edges ----
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

life <- life[sample(.N, 30)]

life[, ID := ego]

# Randomization --------------------------------------------------
randMets <- foreach(iter = seq(0, iterations)) %dopar% {

	if (iter == 0) {
		asso[, ID := hyena]
	} else {
		# Randomize association IDs
		asso[, ID := sample(hyena)]
	}

	## Count sessions ----------------------------------------------
	nseshLs <- rbindlist(foreach(i = seq(1, nrow(life))) %do% {
		sub <- asso[life[i],
								on = .(sessiondate >= period_start,
											 sessiondate < period_end)]
		ego <- sub$ego[[i]]
		period <- sub$period[[i]]

		uasso <- unique(sub[, .(ID, session)])
		uasso[, nSession := .N, session]
		nsesh <- uasso[, .(nSession = .N, nAlone = sum(nSession == 1), period),
									 by = ID]
		return(nsesh[ID == ego])
	})


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
		DT, id = 'ID', count = 'countAffil',
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
		DT, id = 'ID', count = 'countAggr',
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

	## Combine edges, make graphs  -------------------------------------
	# Associations
	assoGraphs <- foreach(i = seq(1, nrow(life))) %dopar% {
		graph.adjacency(twiLs[[i]], 'undirected',
										diag = FALSE, weighted = TRUE)
	}

	# Affiliations
	affilGraphs <- foreach(i = seq(1, nrow(life))) %do% {
		twiDT <- data.table(melt(twiLs[[i]]), stringsAsFactors = FALSE)
		twiDT[, c('Var1', 'Var2') := lapply(.SD, as.character), .SDcols = c(1, 2)]
		sub <-
			merge(countLs[[i]], twiDT, by.x = c('ll_receiver', 'll_solicitor'),
						by.y = c('Var1', 'Var2'), all.x = TRUE)[value != 0]

		g <- graph_from_data_frame(sub[, .(ll_solicitor, ll_receiver)],
															 directed = TRUE)
		w <- sub[, N / value]
		E(g)$weight <- w
		return(g)
	}

	# Aggressions
	aggrGraphs <- foreach(i = seq(1, nrow(life))) %do% {
		twiDT <- data.table(melt(twiLs[[i]]), stringsAsFactors = FALSE)
		twiDT[, c('Var1', 'Var2') := lapply(.SD, as.character), .SDcols = c(1, 2)]
		sub <- merge(avgLs[[i]], twiDT, by.x = c('aggressor', 'recip'),
								 by.y = c('Var1', 'Var2'), all.x = TRUE)[
								 	value != 0 & (value / avgB1) != 0
								 ]

		g <- graph_from_data_frame(sub[, .(aggressor, recip)],
															 directed = TRUE)

		# average of behavior1 during period/AI during period
		w <- sub[, avgB1 / value]
		E(g)$weight <- w

		return(g)
	}

	## Return network metrics ---------------------------------
	mets <- foreach(i = seq(1, nrow(life))) %do% {
		affilG <- affilGraphs[[i]]
		aggrG <- aggrGraphs[[i]]
		assoG <- assoGraphs[[i]]

		ego <- life$ego[[i]]

		w <- E(assoG)$weight
		assoMets <- data.table(
			twi_degree = degree(assoG),
			twi_strength = strength(assoG),
			twi_betweenness = betweenness(assoG, directed = FALSE, weights = 1/w),
			ID = names(degree(assoG))
		)[ID == ego]

		w <- E(affilG)$weight
		affilMets <- data.table(
			affil_degree = degree(affilG, mode = 'total'),
			affil_outdegree = degree(affilG, mode = 'out'),
			affil_indegree = degree(affilG, mode = 'in'),
			affil_strength = strength(affilG, mode = 'total' ,weights = w),
			affil_outstrength = strength(affilG, mode = 'out', weights = w),
			affil_instrength = strength(affilG, mode = 'in', weights = w),
			affil_betweenness = betweenness(affilG, directed = TRUE, weights = 1/w),
			ID = names(degree(affilG))
		)[ID == ego]

		w <- E(aggrG)$weight
		aggrMets <- data.table(
			aggr_degree = degree(aggrG, mode = 'total'),
			aggr_outdegree = degree(aggrG, mode = 'out'),
			aggr_indegree = degree(aggrG, mode = 'in'),
			aggr_strength = strength(aggrG, mode = 'total', weights = w),
			aggr_outstrength = strength(aggrG, mode = 'out', weights = w),
			aggr_instrength = strength(aggrG, mode = 'in', weights = w),
			aggr_betweenness = betweenness(aggrG, directed = TRUE, weights = 1/w),
			ID = names(degree(aggrG))
		)[ID == ego]


		# If no rows, fill columns with NA
		if (nrow(assoMets) == 0) {
			assoMets <- assoMets[NA]
		}

		if (nrow(affilMets) == 0) {
			affilMets <- affilMets[NA]
		}

		if (nrow(aggrMets) == 0) {
			aggrMets <- aggrMets[NA]
		}

		cbind(life[i], assoMets, affilMets, aggrMets)
	}
	rbindlist(mets)[nseshLs, on = c('ID', 'period'), all = TRUE][, iteration := iter]
}

out <- rbindlist(randMets)

out[iteration == 0, observed := TRUE]
out[iteration != 0, observed := FALSE]

### Output ----
saveRDS(out, paste0(derived, 'observed-random-metrics.Rds'))
