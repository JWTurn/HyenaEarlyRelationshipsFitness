#### Graphs to publish ####
# Julie Turner
# 13 Aug. 2019

### Packages ----
libs <- c('data.table', 'ggplot2', 'patchwork')
lapply(libs, require, character.only = TRUE)


### Input data ----
raw <- 'data/raw-data/'
derived <- 'data/derived-data/'

DT <- readRDS(paste0(derived, 'SNfitnessData_2020-04-05.Rds'))

# just the observed data
DT.obs <- DT[iteration == 0]

ll.cd.indeg <- ggplot(DT.obs[period=='cd' & !is.na(longevity_years)], aes(affil_indegree, log(longevity_years))) +
	geom_point(colour = 'gray33') +
	geom_smooth(method = 'lm', colour = 'black', se =F) +
	xlim(0, 15)+
	xlab("Affiliation in-degree") + ylab("log(Longevity in years)") +
	theme_bw()  + theme(
		panel.border = element_blank(),
		panel.grid.major = element_blank(),
		panel.grid.minor = element_blank(),
		axis.line = element_line(colour = "black", size = .7)) +
	theme(plot.title=element_text(size=12,hjust = 0.05),axis.text.x = element_text(size=12), axis.title = element_text(size=15),axis.text.y = element_text(size=12)) +
	theme(axis.text.x = element_text(margin=margin(10,10,10,10,"pt")),
				axis.text.y = element_text(margin=margin(10,10,10,10,"pt")))+ theme(axis.ticks.length = unit(-0.25, "cm"))


ai.di.bet <- ggplot(DT.obs[period=='di' & !is.na(longevity_years)], aes(sri_betweenness, log(longevity_years))) +
	geom_point(colour = 'gray33') +
	geom_smooth(method = 'lm', colour = 'black') +
	xlab("Association betweenness") + ylab("log(Longevity in years)") +
	ggtitle("A)") +
	xlim(0, 130)+
	theme_bw()  + theme(
		panel.border = element_blank(),
		panel.grid.major = element_blank(),
		panel.grid.minor = element_blank(),
		axis.line = element_line(colour = "black", size = .7)) +
	theme(plot.title=element_text(size=12,hjust = 0.05),axis.text.x = element_text(size=12), axis.title = element_text(size=15),axis.text.y = element_text(size=12)) +
	theme(axis.text.x = element_text(margin=margin(10,10,10,10,"pt")),
				axis.text.y = element_text(margin=margin(10,10,10,10,"pt")))+ theme(axis.ticks.length = unit(-0.25, "cm"))

ll.di.bet <- ggplot(DT.obs[period=='di' & !is.na(longevity_years)], aes(affil_betweenness,log (longevity_years))) +
	geom_point(colour = 'gray33') +
	geom_smooth(method = 'lm', colour = 'black') +
	xlab("Affiliation betweenness") + ylab("log(Longevity in years)") +
	ggtitle("B)") +
	xlim(0, 350)+
	theme_bw()  + theme(
		panel.border = element_blank(),
		panel.grid.major = element_blank(),
		panel.grid.minor = element_blank(),
		axis.line = element_line(colour = "black", size = .7)) +
	theme(plot.title=element_text(size=12,hjust = 0.05),axis.text.x = element_text(size=12), axis.title = element_text(size=15),axis.text.y = element_text(size=12)) +
	theme(axis.text.x = element_text(margin=margin(10,10,10,10,"pt")),
				axis.text.y = element_text(margin=margin(10,10,10,10,"pt")))+ theme(axis.ticks.length = unit(-0.25, "cm"))


gsup <- ai.di.bet | ll.di.bet

g <- ll.cd.indeg

ggsave('graphics/figure-2.pdf', g)
