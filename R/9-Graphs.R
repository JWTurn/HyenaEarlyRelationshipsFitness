#### Graphs to publish ####
# Julie Turner
# 13 Aug. 2019

### Packages ----
# remotes::install_github('bbolker/broom.mixed')
libs <- c('data.table', 'ggplot2')
lapply(libs, require, character.only = TRUE)


### Input data ----
raw <- 'data/raw-data/'
derived <- 'data/derived-data/'

DT <- readRDS(paste0(derived, 'DT_obs_rands.Rds'))

# just the observed data
DT.obs <- DT[iteration == 0]

data.f.long.cd <- DT.obs[period=='cd' & !is.na(longevity_years)]

l.cd.7 <- glmer(log(longevity_years) ~  scale(sri_degree)
								+ scale(aggr_outdegree) + scale(aggr_indegree)
								+ scale(affil_outdegree) + scale(affil_indegree)
								+ ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								data=data.f.long.cd, family = 'gaussian')
p.lcd.7 <- ggpredict(l.cd.7)

ggplot(DT.obs[period=='cd' & !is.na(longevity_years)], aes(affil_indegree, longevity_years)) +
	geom_point(colour = 'gray33') +
	geom_smooth(method = 'lm', colour = 'black') +
	xlim(0, 15)+
	xlab("Affiliation in-degree") + ylab("Longevity (years)") +
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



setEPS()
postscript("Fig1.eps")
plot(log(longevity_years)~affil_indegree, data.f.long.cd.noindegol, ylab = 'Log (Longevity in years)', xlab = 'Affiliation in-degree', bty= 'l')
lines(xllindeg.cd.nol, yllindeg.cd.nol[,1], lwd = 4)
# lines(xllindeg.cd.nol, yllindeg.cd.nol[,2], lty = 2)
# lines(xllindeg.cd, yllindeg.cd[,3], lty = 2)
dev.off()




