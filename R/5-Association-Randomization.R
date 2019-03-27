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
idCol <- 'hyena'

## Iterations
iterations <- 3

### Merge network data together ----
# Count the number of affiliations (edges) in each session
affil[, countAffil := .N, session]

# Count the number of individuals associating in each session
asso[, countAsso := .N, session]


# Merge all individuals observed in each session
#  from associations, onto affiliations
DT <- merge(
	affil, asso,
	by = 'session', allow.cartesian = TRUE
)
DT[, c('sessiondate', 'yr') := .(sessiondate.x, yr.x)]
DT <- DT[, .SD, .SDcols = c(colnames(asso), 'grtTime', 'll_receiver', 'll_solicitor', 'countAffil')]

### Directed randomizations function ----
randomizations.directed <- function(DT, id, count, by, cols) {
	DT[, {
		size <- .SD[[2]][[1]]
		if (length(unique(.SD[[1]])) > size) {
			l <- sample(.SD[[1]], size = size)
			r <- sample(.SD[[1]], size = size)

			while (any(l == r)) {
				l <- sample(.SD[[1]], size = size)
				r <- sample(.SD[[1]], size = size)
			}
			list(l, r, .SD[[3]])
		}
	}, by = by, .SDcols = c(id, count, cols)]
}


### Randomize affiliation networks ----
source('R/twi.R')

# Set up parallel with doParallel and foreach
doParallel::registerDoParallel()

# To avoid the merge dropping out sessiondate to sessiondate and sessiondate.i (matching period start and end), we'll add it as an extra column and disregard those later
DT[, idate := sessiondate]

# Count number of (directed) affiliations between individuals
randMets <- foreach(iter = seq(1, iterations)) %dopar% {
	asso[, randHyena := sample(hyena)]
	DT <- merge(
		affil, asso,
		by = 'session', allow.cartesian = TRUE
	)
	DT[, c('sessiondate', 'yr') := .(sessiondate.x, yr.x)]
	DT <- DT[, .SD, .SDcols = c(colnames(asso), 'grtTime', 'll_receiver', 'll_solicitor', 'countAffil')]


	rand <- randomizations.directed(DT,
																	id = 'randHyena',
																	count = 'countAffil',
																	by = 'session',
																	cols = 'sessiondate')
	setnames(rand, c('session', 'll_solicitor', 'll_receiver', 'sessiondate'))
	countLs <- foreach(i = seq(1, nrow(life))) %do% {
		focal <- rand[life[i],
									 on = .(sessiondate >= period_start,
									 			 sessiondate < period_end)]
		focal[, .N, .(ll_receiver, ll_solicitor)]
	}

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


	# Create edge list
	edgeLs <- foreach(i = seq(1, nrow(life))) %do% {
		twi <- data.table(melt(twiLs[[i]]), stringsAsFactors = FALSE)
		twi[, c('Var1', 'Var2') := lapply(.SD, as.character), .SDcols = c(1, 2)]
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
		))
		#TODO: add iteration
	}
}

out <- rbindlist(do.call(c, randMets))
