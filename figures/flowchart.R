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
avgLs <- foreach(i = seq(1, nrow(life))) %dopar% {
	focal <- aggr[life[i],
								on = .(sessiondate >= period_start,
											 sessiondate < period_end)]
	focal[, .(avgB1 = mean(behavior1)), by = .(aggressor, recip)]
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
aggrnets <- foreach(i = seq_along(edgeLs)) %dopar% {
	sub <- edgeLs[[i]][value != 0 & (value / avgB1) != 0]
	graph_from_data_frame(sub[, .(aggressor, recip)], directed = TRUE)
}

out <- rbindlist(mets)


### Plot nets
dtnets <- rbindlist(lapply(nets, ggnetwork), idcol = 'period')
dtnets[, label := ifelse(vertex.names == selfocal, selfocal, ' ')]
dtnets[, period := tools::toTitleCase(period)]

(gnets <- ggplot(dtnets,
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



