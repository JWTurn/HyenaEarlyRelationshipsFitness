### Flowchart figure


### Packages ----
pkgs <- c(
	'data.table',
	'ggplot2',
	'patchwork',
	'ggthemes',
	'spatsoc',
	'igraph',
	'ggnetwork',
	'gridExtra'
)
p <- lapply(pkgs, library, character.only = TRUE)

### Import data ----
derived <- dir('data/derived-data', full.names = TRUE)

# Associations
asso <- readRDS(derived[grepl('prep-asso', derived)])

# Life stages
life <- readRDS(derived[grepl('ego-life', derived)])


## Set column names
groupCol <- 'group'
idCol <- 'hyena'

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

### Association network ----

nsesh <- rbindlist(foreach(i = seq(1, nrow(life))) %dopar% {
	sub <- asso[life[i],
							on = .(sessiondate >= period_start,
										 sessiondate < period_end)]
	ego <- sub$ego[[i]]
	period <- sub$period[[i]]

	uasso <- unique(sub[, .(hyena, session)])
	uasso[, nSession := .N, session]
	nsesh <- uasso[, .(nSession = .N, nAlone = sum(nSession == 1)), hyena]
	return(cbind(nsesh[hyena == ego], period))
})

### Make networks for each ego*life stage ----
#life <- life[1:5]

# To avoid the merge dropping out sessiondate to sessiondate and sessiondate.i (matching period start and end), we'll add it as an extra column and disregard those later
asso[, idate := sessiondate]

# Generate a GBI for each ego's life stage
gbiLs <- foreach(i = seq(1, nrow(life))) %dopar% {
	sub <- asso[life[i],
							on = .(sessiondate >= period_start,
										 sessiondate < period_end)]

	# Filter out < 10
	get_gbi(sub[hyena %chin% sub[, .N, idCol][N > 10, get(idCol)]],
					groupCol, idCol)
}

# Calculate TWI
source('R/twi.R')
twiLs <- foreach(g = gbiLs) %dopar% {
	twi(g)
}

# Generate graph and calculate network metrics
mets <- foreach(n = seq_along(twiLs)) %dopar% {
	g <- graph.adjacency(twiLs[[n]], 'undirected',
											 diag = FALSE, weighted = TRUE)

	w <- E(g)$weight
	cbind(data.table(
		twi_degree = degree(g),
		twi_strength = strength(g, weights = w),
		twi_betweenness = betweenness(g, directed = FALSE, weights = 1/w),
		ID = names(degree(g))
	), life[n])
}
###



gdist <- ggplot(m, aes(
	x = X,
	y = Y,
	xend = Xend,
	yend = Yend
)) +
	geom_edges() +
	geom_nodes(aes(color = ID1), size = 7) +
	geom_nodetext(aes(label = ID1)) +
	scale_color_manual(breaks = DT$ID1, values = pal) +
	guides(color = FALSE) +
	theme(axis.line = element_blank(),
				panel.border = element_rect(fill = NA))


### Edge nn ----
m <- melt(distm)
setDT(m)
m <-
	m[value != 0][, .SD[value == min(value)], Var1][order(value), .(Var1, Var2)]
setnames(m, c('ID', 'NN'))
setnames(DT, 'ID1', 'ID')
m[DT, c('X', 'Y') := .(X, Y), on = 'ID']
m <- merge(m,
					 DT[, .(ID, X, Y)],
					 by.x = 'NN',
					 by.y = 'ID',
					 suffixes = c('', 'end'))


edgenn <-
	ggplot() +
	annotation_custom(tableGrob(m[order(ID), .(ID, NN)], rows = NULL))

gnn <- ggplot(m, aes(
	x = X,
	y = Y,
	xend = Xend,
	yend = Yend
)) +
	geom_edges() +
	geom_nodes(aes(color = ID), size = 7) +
	geom_nodetext(aes(label = ID)) +
	scale_color_manual(breaks = DT$ID, values = pal) +
	guides(color = FALSE) +
	theme(axis.line = element_blank(),
				panel.border = element_rect(fill = NA))

### Output ----
fig5 <- gdist + edgedist + gnn + edgenn +
	plot_layout(ncol = 2, widths = c(3.4, 1)) +
	plot_annotation(tag_levels = 'A') &
	theme(
		plot.tag = element_text(size = 14, face = 2),
		legend.position = c(.9, .75),
		legend.text = element_text(size = 16, face = 1),
		legend.title = element_text(size = 16, face = 1)
	)

ggsave(
	filename = 'figures/Figure5.pdf',
	plot = fig5,
	width = 115,
	height = 145,
	units = 'mm'
)



