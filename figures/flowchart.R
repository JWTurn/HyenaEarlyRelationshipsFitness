### Flowchart figure
# Alec Robitaille

### Packages ----
pkgs <- c(
	'data.table',
	'ggplot2',
	'patchwork',
	'ggthemes',
	'spatsoc',
	'igraph',
	'ggnetwork',
	'gridExtra',
	'foreach'
)
p <- lapply(pkgs, library, character.only = TRUE)

### Import data ----
derived <- dir('data/derived-data', full.names = TRUE)

# Associations
asso <- readRDS(derived[grepl('prep-asso', derived)])

# Life stages
life <- readRDS(derived[grepl('ego-life', derived)])

# Aggression
aggr <- readRDS(derived[grepl('prep-aggr', derived)])

# Affiliation
affil <- readRDS(derived[grepl('prep-affil', derived)])


# Set column names
groupCol <- 'group'
idCol <- 'hyena'

# Set focal individual
selfocal <- 'mono'

### Set theme ----
pal <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442")

theme_set(theme_classic())
theme_update(axis.text = element_blank(),
						 axis.title = element_blank(),
						 axis.ticks = element_blank(),
						 aspect.ratio = 1)

fontSize <- 24
gridTheme <- gridExtra::ttheme_default(
	base_size = fontSize
)

focal <- life[ego == selfocal]


### Association networks ----
## Make networks for each life stage
# To avoid the merge dropping out sessiondate to sessiondate and sessiondate.i (matching period start and end), we'll add it as an extra column and disregard those later
asso[, idate := sessiondate]

# Generate a GBI for each life stage
gbiLs <- foreach(i = seq(1, nrow(focal))) %do% {
	sub <- asso[focal[i],
							on = .(sessiondate >= period_start,
										 sessiondate < period_end)]

	# Filter out < 10
	get_gbi(sub[hyena %chin% sub[, .N, idCol][N > 10, get(idCol)]],
					groupCol, idCol)
}

# Calculate TWI
source('R/twi.R')
twiLs <- foreach(g = gbiLs) %do% {
	twi(g)
}

# Generate graph and calculate network metrics
assonets <- foreach(n = seq_along(twiLs)) %do% {
	g <- graph.adjacency(twiLs[[n]], 'undirected',
											 diag = FALSE, weighted = TRUE)

	w <- E(g)$weight
	g
}
names(assonets) <- paste0('association-', focal$period)


### Aggression networks ----
## Make networks for each ego*life stage
# To avoid the merge dropping out sessiondate to sessiondate and sessiondate.i (matching period start and end), we'll add it as an extra column and disregard those later
aggr[, idate := sessiondate]

#  average of behavior1 during period
avgLs <- foreach(i = seq(1, nrow(focal))) %do% {
	f <- aggr[focal[i],
								on = .(sessiondate >= period_start,
											 sessiondate < period_end)]
	f[, .(avgB1 = mean(behavior1)), by = .(aggressor, recip)]
}

# Create edge list
edgeLs <- foreach(i = seq(1, nrow(focal))) %do% {
	twi <- data.table(melt(twiLs[[i]]), stringsAsFactors = FALSE)
	twi[, c('Var1', 'Var2') := lapply(.SD, as.character), .SDcols = c(1, 2)]
	merge(avgLs[[i]], twi,
				by.x = c('aggressor', 'recip'),
				by.y = c('Var1', 'Var2'), all.x = TRUE)
}

# Generate graph and calculate network metrics
aggrnets <- foreach(i = seq_along(edgeLs)) %do% {
	sub <- edgeLs[[i]][value != 0 & (value / avgB1) != 0]
	graph_from_data_frame(sub[, .(aggressor, recip)], directed = TRUE)
}
names(aggrnets) <- paste0('aggr-', focal$period)


### Affiliation networks ----
# To avoid the merge dropping out sessiondate to sessiondate and sessiondate.i (matching period start and end), we'll add it as an extra column and disregard those later
affil[, idate := sessiondate]

# Count number of (directed) affiliations between individuals
countLs <- foreach(i = seq(1, nrow(focal))) %do% {
	f <- affil[focal[i],
								 on = .(sessiondate >= period_start,
								 			 sessiondate < period_end)]
	f[, .N, .(ll_receiver, ll_solicitor)]
}

# Create edge list
edgeLs <- foreach(i = seq(1, nrow(focal))) %do% {
	twi <- data.table(melt(twiLs[[i]]), stringsAsFactors = FALSE)
	twi[, c('Var1', 'Var2') := lapply(.SD, as.character), .SDcols = c(1, 2)]
	merge(countLs[[i]], twi, by.x = c('ll_receiver', 'll_solicitor'),
				by.y = c('Var1', 'Var2'), all.x = TRUE)
}

# Generate graph and calculate network metrics
affilnets <- foreach(i = seq_along(edgeLs)) %do% {
	sub <- edgeLs[[i]][value != 0]
	g <- graph_from_data_frame(sub[, .(ll_solicitor, ll_receiver)],
														 directed = TRUE)
	w <- sub[, N / value]
	E(g)$weight <- w

	g
}
names(affilnets) <- paste0('affil-', focal$period)


### Plot nets
nets <- rbindlist(lapply(c(assonets, aggrnets, affilnets),
													 ggnetwork),
										idcol = 'nm',
										fill = TRUE)
nets[, label := ifelse(vertex.names == selfocal, selfocal, ' ')]

nets[, c('type', 'period') := tstrsplit(nm, '-')]

nets[period == 'cd', period := 'CD']
nets[period == 'postgrad', period := 'DI']
nets[period == 'adult', period := 'Adult']


# t! And we can do the same for the aggression and affiliation networks
#too for the 3x3 panel we talked about?
#reorder so it's CD, DI, adult,

(gnets <- ggplot(nets,
								 aes(
								 	x = x,
								 	y = y,
								 	xend = xend,
								 	yend = yend
								 )) +
		geom_edges(color = 'grey', alpha = 0.1) +
		geom_nodes() +
		geom_nodes(
			color = 'red',
			shape = 19,
			size = 8,
			fill = 'red',
			data = dtnets[vertex.names == selfocal]
		) +
		# geom_nodetext(aes(label = label)) +
		theme_blank() +
		facet_wrap( ~ period))



### Patchwork ----
(fig <- gnets +
 	ggtitle('Mono') +
	theme(
		plot.title = element_text(hjust = 0.5),
		plot.tag = element_text(size = 14, face = 2),
		legend.position = c(.9, .75),
		legend.text = element_text(size = 16, face = 1),
		legend.title = element_text(size = 16, face = 1)
	))




### Output ---
ggsave(
	filename = 'figures/Figure5.pdf',
	plot = fig5,
	width = 115,
	height = 145,
	units = 'mm'
)



