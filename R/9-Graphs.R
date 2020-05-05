#### Graphs to publish ####
# Julie Turner
# 13 Aug. 2019

### Packages ----
libs <- c('data.table', 'ggplot2', 'patchwork')
lapply(libs, require, character.only = TRUE)


### Input data ----
raw <- 'data/raw-data/'
derived <- 'data/derived-data/'

DT <- readRDS(paste0(derived, 'DT_obs_rands.Rds'))

# just the observed data
DT.obs <- DT[iteration == 0]

data.f.long.cd <- DT.obs[period=='cd' & !is.na(longevity_years)]
data.f.long.cd[,'log.clan_size'] <- log(data.f.long.cd$clan_size)
data.f.long.cd[,'log.nSession'] <- log(data.f.long.cd$nSession)

l.cd.7 <- glmer(log(longevity_years) ~  scale(sri_degree)
								+ scale(aggr_outdegree) + scale(aggr_indegree)
								+ scale(affil_outdegree) + scale(affil_indegree)
								+ ego_period_rank + offset(log.clan_size) + offset(log.nSession) + (1|mom),
								data=data.f.long.cd, family = 'gaussian')
p.lcd.7 <- ggpredict(l.cd.7, ~affil_indegree)

ll.cd.indeg <- ggplot(DT.obs[period=='cd' & !is.na(longevity_years)], aes(affil_indegree, log(longevity_years))) +
	geom_point(colour = 'gray33') +
	geom_smooth(method = 'lm', colour = 'black') +
	xlim(0, 15)+
	xlab("Affiliation in-degree") + ylab("log(Longevity in years)") +
	# ggtitle("a) 60 days") +
	theme_bw()  + theme(
		#panel.background =element_rect(colour = "black", fill=NA, size=1),
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
		#panel.background =element_rect(colour = "black", fill=NA, size=1),
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
		#panel.background =element_rect(colour = "black", fill=NA, size=1),
		panel.border = element_blank(),
		panel.grid.major = element_blank(),
		panel.grid.minor = element_blank(),
		axis.line = element_line(colour = "black", size = .7)) +
	theme(plot.title=element_text(size=12,hjust = 0.05),axis.text.x = element_text(size=12), axis.title = element_text(size=15),axis.text.y = element_text(size=12)) +
	theme(axis.text.x = element_text(margin=margin(10,10,10,10,"pt")),
				axis.text.y = element_text(margin=margin(10,10,10,10,"pt")))+ theme(axis.ticks.length = unit(-0.25, "cm"))

ai.di.bet|ll.di.bet




setEPS()
postscript("Fig1.eps")
ll.cd.indeg
dev.off()



######
ggplot(DT.obs[period=='di' & !is.na(longevity_years)], aes(sri_degree, longevity_years)) +
	geom_point(colour = 'gray33') +
	geom_smooth(method = 'lm', colour = 'black') +
	xlab("Association degree") + ylab("log(Longevity in years)") +
	# ggtitle("a) 60 days") +
	theme_bw()  + theme(
		#panel.background =element_rect(colour = "black", fill=NA, size=1),
		panel.border = element_blank(),
		panel.grid.major = element_blank(),
		panel.grid.minor = element_blank(),
		axis.line = element_line(colour = "black", size = .7)) +
	theme(plot.title=element_text(size=12,hjust = 0.05),axis.text.x = element_text(size=12), axis.title = element_text(size=15),axis.text.y = element_text(size=12)) +
	theme(axis.text.x = element_text(margin=margin(10,10,10,10,"pt")),
				axis.text.y = element_text(margin=margin(10,10,10,10,"pt")))+ theme(axis.ticks.length = unit(-0.25, "cm"))


ggplot(DT.obs[period=='adult' & !is.na(annual_rs)], aes(aggr_outdegree, annual_rs)) +
	geom_point(colour = 'gray33') +
	geom_smooth(method = 'lm', colour = 'black') +
	ylim(0, 2.5)+
	xlim(0, 50)+
	xlab("Aggression out-degree") + ylab("Average annual reproductive success") +
	# ggtitle("a) 60 days") +
	theme_bw()  + theme(
		#panel.background =element_rect(colour = "black", fill=NA, size=1),
		panel.border = element_blank(),
		panel.grid.major = element_blank(),
		panel.grid.minor = element_blank(),
		axis.line = element_line(colour = "black", size = .7)) +
	theme(plot.title=element_text(size=12,hjust = 0.05),axis.text.x = element_text(size=12), axis.title = element_text(size=15),axis.text.y = element_text(size=12)) +
	theme(axis.text.x = element_text(margin=margin(10,10,10,10,"pt")),
				axis.text.y = element_text(margin=margin(10,10,10,10,"pt")))+ theme(axis.ticks.length = unit(-0.25, "cm"))

ggplot(p.lcd.7, aes(x, predicted)) +
	geom_line() +
	geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)



