### Hyena Association Networks ====
# Julie Turner and Alec Robitaille
# Revisions: April 02 2020

### Packages ----
libs <- c('data.table', 'spatsoc', 'asnipe', 'igraph', 'foreach', 'doParallel', 'ggplot2', 'patchwork')
lapply(libs, require, character.only = TRUE)


### Import data ----
derived <- dir('data/derived-data', full.names = TRUE)

# Associations
asso <- readRDS(derived[grepl('prep-asso', derived)])

# Life stages
life <- readRDS(derived[grepl('ego-life', derived)])

set.seed(37)
## Set column names
groupCol <- 'group'
idCol <- 'hyena'

# Set up parallel with doParallel and foreach
doParallel::registerDoParallel()

### Count number of sessions and alone session ----
nsesh <- rbindlist(foreach(i = seq(1, nrow(life))) %do% {
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
life.n <- merge(life, nsesh, by.x = c('ego', 'period'), by.y = c('hyena', 'period'))
life.n <- life.n[(nSession - nAlone)>=50]

# To avoid the merge dropping out sessiondate to sessiondate and sessiondate.i (matching period start and end), we'll add it as an extra column and disregard those later
asso[, idate := sessiondate]
asso[,nhyenas := uniqueN(hyena), session]

### Generate networks for each n observations ----


#randSesh <- asso[, sample(unique(timegroup), size = maxn), (hyena)]
# Generate base 50 random sessions for each ego's life stage
randSesh <- foreach(i = seq(1, nrow(life.n))) %do% {
	sub <- asso[life.n[i],
							on = .(sessiondate >= period_start,
										 sessiondate < period_end)]
	sub[hyena == life.n[i]$ego, .(session = sample(unique(session), 50))]
# 	sesh.50 <- sub[, .(session = sample(unique(session), 50))]
#   sub[session %in% sesh.50$session]
}

maxn <- 50 #DT[, uniqueN(timegroup)]
nstep <- 5


nets <- lapply(seq(5, maxn, by = nstep), function(nobs) {
	# Select first n random timegroups,
	#  adding new observations to the tail with each iteration

	randsub <- foreach(i = seq(1, nrow(life.n))) %do% {
		asso[session %in% randSesh[[i]][, .SD[1:nobs]]$session]
	}

# Generate a GBI for each ego's life stage
gbiLs <- foreach(i = seq(1, nrow(life.n))) %do% {
	sub <- randsub[[i]][life.n[i],
							on = .(sessiondate >= period_start,
										 sessiondate < period_end)]

	get_gbi(sub[hyena %chin% sub[, .N, idCol][, get(idCol)]],
					groupCol, idCol)
}

# Calculate SRI
sriLs <- foreach(g = gbiLs) %do% {
	get_network(g, 'GBI', 'SRI')
}

# Generate graph and calculate network metrics
mets <- foreach(n = seq_along(sriLs)) %do% {
	g <- graph.adjacency(sriLs[[n]], 'undirected',
											 diag = FALSE, weighted = TRUE)

	w <- E(g)$weight
	cbind(data.table(
		sri_degree = degree(g),
		sri_strength = strength(g, weights = w),
		sri_Wbetweenness = betweenness(g, directed = FALSE, weights = 1/w),
		sri_betweenness = betweenness(g, directed = FALSE),
		ID = names(degree(g))
	), life[n])
}

out <- rbindlist(mets)

setnames(out, 'ID', idCol)

out <- out[hyena == ego]
out <- merge(out, nsesh, by = c('hyena', 'period'))
out[, nobs := nobs]

})

######

out <- rbindlist(nets)

#saveRDS(out, 'data/derived-data/sensitivity-mets.Rds')


mets.a <-readRDS('data/derived-data/sensitivity-metsWalone.Rds')
metscor <- dcast(mets.a, ego +period ~ nobs, value.var=c('sri_degree', 'sri_betweenness', 'sri_Wbetweenness', 'sri_strength'))

metscor[,cor.test(sri_degree_5, sri_degree_10)]
metscor[,cor.test(sri_degree_5, sri_degree_15)]
metscor[,cor.test(sri_degree_5, sri_degree_30)]
metscor[,cor.test(sri_degree_5, sri_degree_50)]

metscor[,cor.test(sri_degree_10, sri_degree_15)]
metscor[,cor.test(sri_degree_10, sri_degree_30)]
metscor[,cor.test(sri_degree_10, sri_degree_25)]
metscor[,cor.test(sri_degree_10, sri_degree_50)]


metscor[,cor.test(sri_betweenness_5, sri_betweenness_10)]
metscor[,cor.test(sri_betweenness_5, sri_betweenness_15)]
metscor[,cor.test(sri_betweenness_5, sri_betweenness_30)]
metscor[,cor.test(sri_betweenness_5, sri_betweenness_50)]

metscor[,cor.test(sri_betweenness_10, sri_betweenness_15)]
metscor[,cor.test(sri_betweenness_10, sri_betweenness_30)]
metscor[,cor.test(sri_betweenness_10, sri_betweenness_50)]


metscor[,cor.test(sri_strength_5, sri_strength_10)]
metscor[,cor.test(sri_strength_5, sri_strength_15)]
metscor[,cor.test(sri_strength_5, sri_strength_30)]
metscor[,cor.test(sri_strength_5, sri_strength_50)]

metscor[,cor.test(sri_strength_10, sri_strength_15)]
metscor[,cor.test(sri_strength_10, sri_strength_30)]
metscor[,cor.test(sri_strength_10, sri_strength_50)]

###

comps.a <- mets.a[hyena %in% mets.a[nobs==5]$hyena]


comps.a.cor <- dcast(comps.a, ego +period ~ nobs, value.var=c('sri_degree', 'sri_betweenness', 'sri_Wbetweenness', 'sri_strength'))
comps.a.cor <- na.omit(comps.a.cor)

comps.a <- merge(comps.a.cor[,.(ego, period)], mets.a, by=c('ego','period'))

pdeg <- ggplot(comps.a, aes(nobs, sri_degree))+
	geom_point()+
	ylim(0,65)+
	theme_bw()  + theme(
		panel.border = element_blank(),
		panel.grid.major = element_blank(),
		panel.grid.minor = element_blank(),
		axis.line = element_line(colour = "black", size = .7)) +
	theme(plot.title=element_text(size=12,hjust = 0.05),axis.text.x = element_text(size=12), axis.title = element_text(size=15),axis.text.y = element_text(size=12)) +
	theme(axis.text.x = element_text(margin=margin(10,10,10,10,"pt")),
				axis.text.y = element_text(margin=margin(10,10,10,10,"pt")))+ theme(axis.ticks.length = unit(-0.25, "cm")) +
	ylab("degree") + xlab("# of observations") +
	ggtitle("A)")


pstr <- ggplot(comps.a, aes(nobs, sri_strength))+
	geom_point()+
	theme_bw()  + theme(
		panel.border = element_blank(),
		panel.grid.major = element_blank(),
		panel.grid.minor = element_blank(),
		axis.line = element_line(colour = "black", size = .7)) +
	theme(plot.title=element_text(size=12,hjust = 0.05),axis.text.x = element_text(size=12), axis.title = element_text(size=15),axis.text.y = element_text(size=12)) +
	theme(axis.text.x = element_text(margin=margin(10,10,10,10,"pt")),
				axis.text.y = element_text(margin=margin(10,10,10,10,"pt")))+ theme(axis.ticks.length = unit(-0.25, "cm")) +
	ylab("strength") + xlab("# of observations") +
	ggtitle("B)")


pbet <- ggplot(comps.a, aes(nobs, sri_Wbetweenness))+
	geom_point()+
	ylim(0,125)+
	theme_bw()  + theme(
		panel.border = element_blank(),
		panel.grid.major = element_blank(),
		panel.grid.minor = element_blank(),
		axis.line = element_line(colour = "black", size = .7)) +
	theme(plot.title=element_text(size=12,hjust = 0.05),axis.text.x = element_text(size=12), axis.title = element_text(size=15),axis.text.y = element_text(size=12)) +
	theme(axis.text.x = element_text(margin=margin(10,10,10,10,"pt")),
				axis.text.y = element_text(margin=margin(10,10,10,10,"pt")))+ theme(axis.ticks.length = unit(-0.25, "cm")) +
	ylab("betweenness") + xlab("# of observations") +
	ggtitle("C)")

pdeg|pstr|pbet




