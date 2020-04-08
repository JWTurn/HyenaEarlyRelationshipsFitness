### Model comparisons  ====
# Julie Turner and Alec Robitaille
# Revisions started: March 31 2020


### Packages ----
# remotes::install_github('bbolker/broom.mixed')
libs <- c('lme4', 'data.table', 'broom.mixed', 'ggplot2', 'bbmle', 'car', 'performance')
lapply(libs, require, character.only = TRUE)

### Function ----
se <- function(x){
	sd(x, na.rm = T)/ sqrt(length(na.omit(x)))
}

### Input data ----
raw <- 'data/raw-data/'
derived <- 'data/derived-data/'

# mets <- readRDS(paste0(derived, 'observed-random-metrics.Rds'))
#
# egos <- fread(paste0(raw, 'egos_filtered.csv'))
#
#
# ### Merge ----
# # Fix inconsistent periods
# egos[, period := tolower(period)]
# egos <- egos[,.(ego, period, ego_period_rank, mom, clan_size, annual_rs, longevity_years)]
# mets[period == 'postgrad', period := 'di']
#
# # Merge
# cols <- c('ego', 'period')
# setkeyv(egos, cols)
# setkeyv(mets, cols)
#
# DT <- merge(mets, egos, all.x = TRUE)
# DT[, 7:23][is.na(DT[, 7:23])] <- 0
# DT[,'alone'] <- DT$nAlone/DT$nSession
# DT <- DT[order(iteration, ego),]

#saveRDS(DT, paste0(derived, 'DT_obs_rands.Rds'))

DT <- readRDS(paste0(derived, 'DT_obs_rands.Rds'))


# just the observed data
DT.obs <- DT[iteration == 0]
#DT<-DT[nSession>=10]


# specific data for model comparisons
data.f.long.cd <- subset(DT.obs, period == 'cd')
data.f.long.di <- subset(DT.obs, period == 'di')
data.f.long.ad <- subset(DT.obs, period == 'adult')
data.f.ars.cd <- subset(DT.obs, period == 'cd')
data.f.ars.di <- subset(DT.obs, period == 'di')
data.f.ars.ad <- subset(DT.obs, period == 'adult')

### Model comparisons ----

#### LONGEVITY ####
#### CD ###
l.cd.full <- glmer(log(longevity_years) ~  scale(alone) + scale(sri_degree) + scale(sri_strength) + scale(sri_betweenness)
									 + scale(aggr_outdegree) + scale(aggr_indegree) + scale(aggr_outstrength) + scale(aggr_instrength) + scale(aggr_betweenness)
									 + scale(affil_outdegree) + scale(affil_indegree) + scale(affil_outstrength) + scale(affil_instrength) + scale(affil_betweenness)
									 + ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
									 data=data.f.long.cd, family = 'gaussian')


l.cd.1 <- glmer(log(longevity_years) ~  scale(alone) + scale(sri_degree) + scale(sri_strength)
								+ scale(aggr_outdegree) + scale(aggr_indegree) + scale(aggr_outstrength) + scale(aggr_instrength)
								+ scale(affil_outdegree) + scale(affil_indegree) + scale(affil_outstrength) + scale(affil_instrength)
								+ ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								data=data.f.long.cd, family = 'gaussian')


l.cd.2 <- glmer(log(longevity_years) ~  scale(alone) + scale(sri_degree)
								+ scale(aggr_outdegree) + scale(aggr_indegree)
								+ scale(affil_outdegree) + scale(affil_indegree)
								+ ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								data=data.f.long.cd, family = 'gaussian')


l.cd.3 <- glmer(log(longevity_years) ~  scale(alone) + scale(sri_degree) + scale(sri_betweenness)
								+ scale(aggr_outdegree) + scale(aggr_indegree) + scale(aggr_betweenness)
								+ scale(affil_outdegree) + scale(affil_indegree) + scale(affil_betweenness)
								+ ego_period_rank + offset(log(clan_size))  + offset(log(nSession)) + (1|mom),
								data=data.f.long.cd, family = 'gaussian')


l.cd.4 <- glmer(log(longevity_years) ~  scale(alone) + scale(sri_strength) + scale(sri_betweenness)
								+ scale(aggr_outstrength) + scale(aggr_instrength) + scale(aggr_betweenness)
								+ scale(affil_outstrength) + scale(affil_instrength) + scale(affil_betweenness)
								+ ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								data=data.f.long.cd, family = 'gaussian')


l.cd.5 <- glmer(log(longevity_years) ~  scale(sri_degree) + scale(sri_strength) + scale(sri_betweenness)
								+ scale(aggr_outdegree) + scale(aggr_indegree) + scale(aggr_outstrength) + scale(aggr_instrength) + scale(aggr_betweenness)
								+ scale(affil_outdegree) + scale(affil_indegree) + scale(affil_outstrength) + scale(affil_instrength) + scale(affil_betweenness)
								+ ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								data=data.f.long.cd, family = 'gaussian')


l.cd.6 <- glmer(log(longevity_years) ~ scale(sri_degree) + scale(sri_strength)
								+ scale(aggr_outdegree) + scale(aggr_indegree) + scale(aggr_outstrength) + scale(aggr_instrength)
								+ scale(affil_outdegree) + scale(affil_indegree) + scale(affil_outstrength) + scale(affil_instrength)
								+ ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								data=data.f.long.cd, family = 'gaussian')


l.cd.7 <- glmer(log(longevity_years) ~  scale(sri_degree)
								+ scale(aggr_outdegree) + scale(aggr_indegree)
								+ scale(affil_outdegree) + scale(affil_indegree)
								+ ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								data=data.f.long.cd, family = 'gaussian')


l.cd.8 <- glmer(log(longevity_years) ~ scale(sri_degree) + scale(sri_betweenness)
								+ scale(aggr_outdegree) + scale(aggr_indegree) + scale(aggr_betweenness)
								+ scale(affil_outdegree) + scale(affil_indegree) + scale(affil_betweenness)
								+ ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								data=data.f.long.cd, family = 'gaussian')


l.cd.9 <- glmer(log(longevity_years) ~  scale(sri_strength) + scale(sri_betweenness)
								+ scale(aggr_outstrength) + scale(aggr_instrength) + scale(aggr_betweenness)
								+ scale(affil_outstrength) + scale(affil_instrength) + scale(affil_betweenness)
								+ ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								data=data.f.long.cd, family = 'gaussian')


l.cd.10 <- glmer(log(longevity_years) ~ scale(alone) + scale(sri_betweenness)
								 + scale(aggr_outstrength) + scale(aggr_instrength)
								 + scale(affil_outstrength) + scale(affil_instrength)
								 + ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								 data=data.f.long.cd, family = 'gaussian')


l.cd.11 <- glmer(log(longevity_years) ~ scale(sri_strength)
								 + scale(aggr_outstrength) + scale(aggr_instrength)
								 + scale(affil_outstrength) + scale(affil_instrength)
								 + ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								 data=data.f.long.cd, family = 'gaussian')


l.cd.12 <- glmer(log(longevity_years) ~ scale(alone) + scale(sri_betweenness)
								 + scale(aggr_betweenness)
								 + scale(affil_betweenness)
								 + ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								 data=data.f.long.cd, family = 'gaussian')


l.cd.13 <- glmer(log(longevity_years) ~ scale(sri_betweenness)
								 + scale(aggr_betweenness)
								 + scale(affil_betweenness)
								 + ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								 data=data.f.long.cd, family = 'gaussian')



#### DI ###
l.di.full <- glmer(log(longevity_years) ~  scale(alone) + scale(sri_degree) + scale(sri_strength) + scale(sri_betweenness)
									 + scale(aggr_outdegree) + scale(aggr_indegree) + scale(aggr_outstrength) + scale(aggr_instrength) + scale(aggr_betweenness)
									 + scale(affil_outdegree) + scale(affil_indegree) + scale(affil_outstrength) + scale(affil_instrength) + scale(affil_betweenness)
									 + ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
									 data=data.f.long.di, family = 'gaussian')


l.di.1 <- glmer(log(longevity_years) ~  scale(alone) + scale(sri_degree) + scale(sri_strength)
								+ scale(aggr_outdegree) + scale(aggr_indegree) + scale(aggr_outstrength) + scale(aggr_instrength)
								+ scale(affil_outdegree) + scale(affil_indegree) + scale(affil_outstrength) + scale(affil_instrength)
								+ ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								data=data.f.long.di, family = 'gaussian')


l.di.2 <- glmer(log(longevity_years) ~  scale(alone) + scale(sri_degree)
								+ scale(aggr_outdegree) + scale(aggr_indegree)
								+ scale(affil_outdegree) + scale(affil_indegree)
								+ ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								data=data.f.long.di, family = 'gaussian')


l.di.3 <- glmer(log(longevity_years) ~  scale(alone) + scale(sri_degree) + scale(sri_betweenness)
								+ scale(aggr_outdegree) + scale(aggr_indegree) + scale(aggr_betweenness)
								+ scale(affil_outdegree) + scale(affil_indegree) + scale(affil_betweenness)
								+ ego_period_rank + offset(log(clan_size))  + offset(log(nSession)) + (1|mom),
								data=data.f.long.di, family = 'gaussian')


l.di.4 <- glmer(log(longevity_years) ~  scale(alone) + scale(sri_strength) + scale(sri_betweenness)
								+ scale(aggr_outstrength) + scale(aggr_instrength) + scale(aggr_betweenness)
								+ scale(affil_outstrength) + scale(affil_instrength) + scale(affil_betweenness)
								+ ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								data=data.f.long.di, family = 'gaussian')


l.di.5 <- glmer(log(longevity_years) ~  scale(sri_degree) + scale(sri_strength) + scale(sri_betweenness)
								+ scale(aggr_outdegree) + scale(aggr_indegree) + scale(aggr_outstrength) + scale(aggr_instrength) + scale(aggr_betweenness)
								+ scale(affil_outdegree) + scale(affil_indegree) + scale(affil_outstrength) + scale(affil_instrength) + scale(affil_betweenness)
								+ ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								data=data.f.long.di, family = 'gaussian')


l.di.6 <- glmer(log(longevity_years) ~ scale(sri_degree) + scale(sri_strength)
								+ scale(aggr_outdegree) + scale(aggr_indegree) + scale(aggr_outstrength) + scale(aggr_instrength)
								+ scale(affil_outdegree) + scale(affil_indegree) + scale(affil_outstrength) + scale(affil_instrength)
								+ ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								data=data.f.long.di, family = 'gaussian')


l.di.7 <- glmer(log(longevity_years) ~  scale(sri_degree)
								+ scale(aggr_outdegree) + scale(aggr_indegree)
								+ scale(affil_outdegree) + scale(affil_indegree)
								+ ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								data=data.f.long.di, family = 'gaussian')


l.di.8 <- glmer(log(longevity_years) ~ scale(sri_degree) + scale(sri_betweenness)
								+ scale(aggr_outdegree) + scale(aggr_indegree) + scale(aggr_betweenness)
								+ scale(affil_outdegree) + scale(affil_indegree) + scale(affil_betweenness)
								+ ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								data=data.f.long.di, family = 'gaussian')


l.di.9 <- glmer(log(longevity_years) ~  scale(sri_strength) + scale(sri_betweenness)
								+ scale(aggr_outstrength) + scale(aggr_instrength) + scale(aggr_betweenness)
								+ scale(affil_outstrength) + scale(affil_instrength) + scale(affil_betweenness)
								+ ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								data=data.f.long.di, family = 'gaussian')


l.di.10 <- glmer(log(longevity_years) ~ scale(alone) + scale(sri_betweenness)
								 + scale(aggr_outstrength) + scale(aggr_instrength)
								 + scale(affil_outstrength) + scale(affil_instrength)
								 + ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								 data=data.f.long.di, family = 'gaussian')


l.di.11 <- glmer(log(longevity_years) ~ scale(sri_strength)
								 + scale(aggr_outstrength) + scale(aggr_instrength)
								 + scale(affil_outstrength) + scale(affil_instrength)
								 + ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								 data=data.f.long.di, family = 'gaussian')


l.di.12 <- glmer(log(longevity_years) ~ scale(alone) + scale(sri_betweenness)
								 + scale(aggr_betweenness)
								 + scale(affil_betweenness)
								 + ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								 data=data.f.long.di, family = 'gaussian')


l.di.13 <- glmer(log(longevity_years) ~ scale(sri_betweenness)
								 + scale(aggr_betweenness)
								 + scale(affil_betweenness)
								 + ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								 data=data.f.long.di, family = 'gaussian')



#### Adult ###
l.ad.full <- glmer(log(longevity_years) ~  scale(alone) + scale(sri_degree) + scale(sri_strength) + scale(sri_betweenness)
									 + scale(aggr_outdegree) + scale(aggr_indegree) + scale(aggr_outstrength) + scale(aggr_instrength) + scale(aggr_betweenness)
									 + scale(affil_outdegree) + scale(affil_indegree) + scale(affil_outstrength) + scale(affil_instrength) + scale(affil_betweenness)
									 + ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
									 data=data.f.long.ad, family = 'gaussian')


l.ad.1 <- glmer(log(longevity_years) ~  scale(alone) + scale(sri_degree) + scale(sri_strength)
								+ scale(aggr_outdegree) + scale(aggr_indegree) + scale(aggr_outstrength) + scale(aggr_instrength)
								+ scale(affil_outdegree) + scale(affil_indegree) + scale(affil_outstrength) + scale(affil_instrength)
								+ ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								data=data.f.long.ad, family = 'gaussian')


l.ad.2 <- glmer(log(longevity_years) ~  scale(alone) + scale(sri_degree)
								+ scale(aggr_outdegree) + scale(aggr_indegree)
								+ scale(affil_outdegree) + scale(affil_indegree)
								+ ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								data=data.f.long.ad, family = 'gaussian')


l.ad.3 <- glmer(log(longevity_years) ~  scale(alone) + scale(sri_degree) + scale(sri_betweenness)
								+ scale(aggr_outdegree) + scale(aggr_indegree) + scale(aggr_betweenness)
								+ scale(affil_outdegree) + scale(affil_indegree) + scale(affil_betweenness)
								+ ego_period_rank + offset(log(clan_size))  + offset(log(nSession)) + (1|mom),
								data=data.f.long.ad, family = 'gaussian')


l.ad.4 <- glmer(log(longevity_years) ~  scale(alone) + scale(sri_strength) + scale(sri_betweenness)
								+ scale(aggr_outstrength) + scale(aggr_instrength) + scale(aggr_betweenness)
								+ scale(affil_outstrength) + scale(affil_instrength) + scale(affil_betweenness)
								+ ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								data=data.f.long.ad, family = 'gaussian')


l.ad.5 <- glmer(log(longevity_years) ~  scale(sri_degree) + scale(sri_strength) + scale(sri_betweenness)
								+ scale(aggr_outdegree) + scale(aggr_indegree) + scale(aggr_outstrength) + scale(aggr_instrength) + scale(aggr_betweenness)
								+ scale(affil_outdegree) + scale(affil_indegree) + scale(affil_outstrength) + scale(affil_instrength) + scale(affil_betweenness)
								+ ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								data=data.f.long.ad, family = 'gaussian')


l.ad.6 <- glmer(log(longevity_years) ~ scale(sri_degree) + scale(sri_strength)
								+ scale(aggr_outdegree) + scale(aggr_indegree) + scale(aggr_outstrength) + scale(aggr_instrength)
								+ scale(affil_outdegree) + scale(affil_indegree) + scale(affil_outstrength) + scale(affil_instrength)
								+ ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								data=data.f.long.ad, family = 'gaussian')


l.ad.7 <- glmer(log(longevity_years) ~  scale(sri_degree)
								+ scale(aggr_outdegree) + scale(aggr_indegree)
								+ scale(affil_outdegree) + scale(affil_indegree)
								+ ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								data=data.f.long.ad, family = 'gaussian')


l.ad.8 <- glmer(log(longevity_years) ~ scale(sri_degree) + scale(sri_betweenness)
								+ scale(aggr_outdegree) + scale(aggr_indegree) + scale(aggr_betweenness)
								+ scale(affil_outdegree) + scale(affil_indegree) + scale(affil_betweenness)
								+ ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								data=data.f.long.ad, family = 'gaussian')


l.ad.9 <- glmer(log(longevity_years) ~  scale(sri_strength) + scale(sri_betweenness)
								+ scale(aggr_outstrength) + scale(aggr_instrength) + scale(aggr_betweenness)
								+ scale(affil_outstrength) + scale(affil_instrength) + scale(affil_betweenness)
								+ ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								data=data.f.long.ad, family = 'gaussian')


l.ad.10 <- glmer(log(longevity_years) ~ scale(alone) + scale(sri_betweenness)
								 + scale(aggr_outstrength) + scale(aggr_instrength)
								 + scale(affil_outstrength) + scale(affil_instrength)
								 + ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								 data=data.f.long.ad, family = 'gaussian')


l.ad.11 <- glmer(log(longevity_years) ~ scale(sri_strength)
								 + scale(aggr_outstrength) + scale(aggr_instrength)
								 + scale(affil_outstrength) + scale(affil_instrength)
								 + ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								 data=data.f.long.ad, family = 'gaussian')


l.ad.12 <- glmer(log(longevity_years) ~ scale(alone) + scale(sri_betweenness)
								 + scale(aggr_betweenness)
								 + scale(affil_betweenness)
								 + ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								 data=data.f.long.ad, family = 'gaussian')


l.ad.13 <- glmer(log(longevity_years) ~ scale(sri_betweenness)
								 + scale(aggr_betweenness)
								 + scale(affil_betweenness)
								 + ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								 data=data.f.long.ad, family = 'gaussian')



# comparing models

AIC.l.cd <- as.data.frame(AICctab(l.cd.full, l.cd.1,l.cd.2, l.cd.3, l.cd.4, l.cd.5, l.cd.6, l.cd.7, l.cd.8, l.cd.9, l.cd.10, l.cd.11, l.cd.12, l.cd.13))

AIC.l.di <- as.data.frame(AICctab(l.di.full, l.di.1,l.di.2, l.di.3, l.di.4, l.di.5, l.di.6, l.di.7, l.di.8, l.di.9, l.di.10, l.di.11, l.di.12, l.di.13))

AIC.l.ad <- as.data.frame(AICctab(l.ad.full, l.ad.1,l.ad.2, l.ad.3, l.ad.4, l.ad.5, l.ad.6, l.ad.7, l.ad.8, l.ad.9, l.ad.10, l.ad.11, l.ad.12, l.ad.13))

AIC.long <- as.data.frame(AICctab(l.cd.7, l.cd.11, l.di.13, l.di.7, l.ad.7, l.ad.13))


check_model(l.cd.7)


#### ARS ####
### CD ###
a.cd.full <- glmer(log(annual_rs) ~  scale(alone) + scale(sri_degree) + scale(sri_strength) + scale(sri_betweenness)
									 + scale(aggr_outdegree) + scale(aggr_indegree) + scale(aggr_outstrength) + scale(aggr_instrength) + scale(aggr_betweenness)
									 + scale(affil_outdegree) + scale(affil_indegree) + scale(affil_outstrength) + scale(affil_instrength) + scale(affil_betweenness)
									 + ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
									 data=data.f.ars.cd, family = 'gaussian')


a.cd.1 <- glmer(log(annual_rs) ~  scale(alone) + scale(sri_degree) + scale(sri_strength)
								+ scale(aggr_outdegree) + scale(aggr_indegree) + scale(aggr_outstrength) + scale(aggr_instrength)
								+ scale(affil_outdegree) + scale(affil_indegree) + scale(affil_outstrength) + scale(affil_instrength)
								+ ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								data=data.f.ars.cd, family = 'gaussian')


a.cd.2 <- glmer(log(annual_rs) ~  scale(alone) + scale(sri_degree)
								+ scale(aggr_outdegree) + scale(aggr_indegree)
								+ scale(affil_outdegree) + scale(affil_indegree)
								+ ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								data=data.f.ars.cd, family = 'gaussian')


a.cd.3 <- glmer(log(annual_rs) ~  scale(alone) + scale(sri_degree) + scale(sri_betweenness)
								+ scale(aggr_outdegree) + scale(aggr_indegree) + scale(aggr_betweenness)
								+ scale(affil_outdegree) + scale(affil_indegree) + scale(affil_betweenness)
								+ ego_period_rank + offset(log(clan_size))  + offset(log(nSession)) + (1|mom),
								data=data.f.ars.cd, family = 'gaussian')


a.cd.4 <- glmer(log(annual_rs) ~  scale(alone) + scale(sri_strength) + scale(sri_betweenness)
								+ scale(aggr_outstrength) + scale(aggr_instrength) + scale(aggr_betweenness)
								+ scale(affil_outstrength) + scale(affil_instrength) + scale(affil_betweenness)
								+ ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								data=data.f.ars.cd, family = 'gaussian')


a.cd.5 <- glmer(log(annual_rs) ~  scale(sri_degree) + scale(sri_strength) + scale(sri_betweenness)
								+ scale(aggr_outdegree) + scale(aggr_indegree) + scale(aggr_outstrength) + scale(aggr_instrength) + scale(aggr_betweenness)
								+ scale(affil_outdegree) + scale(affil_indegree) + scale(affil_outstrength) + scale(affil_instrength) + scale(affil_betweenness)
								+ ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								data=data.f.ars.cd, family = 'gaussian')


a.cd.6 <- glmer(log(annual_rs) ~ scale(sri_degree) + scale(sri_strength)
								+ scale(aggr_outdegree) + scale(aggr_indegree) + scale(aggr_outstrength) + scale(aggr_instrength)
								+ scale(affil_outdegree) + scale(affil_indegree) + scale(affil_outstrength) + scale(affil_instrength)
								+ ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								data=data.f.ars.cd, family = 'gaussian')


a.cd.7 <- glmer(log(annual_rs) ~  scale(sri_degree)
								+ scale(aggr_outdegree) + scale(aggr_indegree)
								+ scale(affil_outdegree) + scale(affil_indegree)
								+ ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								data=data.f.ars.cd, family = 'gaussian')


a.cd.8 <- glmer(log(annual_rs) ~ scale(sri_degree) + scale(sri_betweenness)
								+ scale(aggr_outdegree) + scale(aggr_indegree) + scale(aggr_betweenness)
								+ scale(affil_outdegree) + scale(affil_indegree) + scale(affil_betweenness)
								+ ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								data=data.f.ars.cd, family = 'gaussian')


a.cd.9 <- glmer(log(annual_rs) ~  scale(sri_strength) + scale(sri_betweenness)
								+ scale(aggr_outstrength) + scale(aggr_instrength) + scale(aggr_betweenness)
								+ scale(affil_outstrength) + scale(affil_instrength) + scale(affil_betweenness)
								+ ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								data=data.f.ars.cd, family = 'gaussian')


a.cd.10 <- glmer(log(annual_rs) ~ scale(alone) + scale(sri_betweenness)
								 + scale(aggr_outstrength) + scale(aggr_instrength)
								 + scale(affil_outstrength) + scale(affil_instrength)
								 + ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								 data=data.f.ars.cd, family = 'gaussian')


a.cd.11 <- glmer(log(annual_rs) ~ scale(sri_strength)
								 + scale(aggr_outstrength) + scale(aggr_instrength)
								 + scale(affil_outstrength) + scale(affil_instrength)
								 + ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								 data=data.f.ars.cd, family = 'gaussian')


a.cd.12 <- glmer(log(annual_rs) ~ scale(alone) + scale(sri_betweenness)
								 + scale(aggr_betweenness)
								 + scale(affil_betweenness)
								 + ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								 data=data.f.ars.cd, family = 'gaussian')


a.cd.13 <- glmer(log(annual_rs) ~ scale(sri_betweenness)
								 + scale(aggr_betweenness)
								 + scale(affil_betweenness)
								 + ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								 data=data.f.ars.cd, family = 'gaussian')



#### DI ###
a.di.full <- glmer(log(annual_rs) ~  scale(alone) + scale(sri_degree) + scale(sri_strength) + scale(sri_betweenness)
									 + scale(aggr_outdegree) + scale(aggr_indegree) + scale(aggr_outstrength) + scale(aggr_instrength) + scale(aggr_betweenness)
									 + scale(affil_outdegree) + scale(affil_indegree) + scale(affil_outstrength) + scale(affil_instrength) + scale(affil_betweenness)
									 + ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
									 data=data.f.ars.di, family = 'gaussian')


a.di.1 <- glmer(log(annual_rs) ~  scale(alone) + scale(sri_degree) + scale(sri_strength)
								+ scale(aggr_outdegree) + scale(aggr_indegree) + scale(aggr_outstrength) + scale(aggr_instrength)
								+ scale(affil_outdegree) + scale(affil_indegree) + scale(affil_outstrength) + scale(affil_instrength)
								+ ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								data=data.f.ars.di, family = 'gaussian')


a.di.2 <- glmer(log(annual_rs) ~  scale(alone) + scale(sri_degree)
								+ scale(aggr_outdegree) + scale(aggr_indegree)
								+ scale(affil_outdegree) + scale(affil_indegree)
								+ ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								data=data.f.ars.di, family = 'gaussian')


a.di.3 <- glmer(log(annual_rs) ~  scale(alone) + scale(sri_degree) + scale(sri_betweenness)
								+ scale(aggr_outdegree) + scale(aggr_indegree) + scale(aggr_betweenness)
								+ scale(affil_outdegree) + scale(affil_indegree) + scale(affil_betweenness)
								+ ego_period_rank + offset(log(clan_size))  + offset(log(nSession)) + (1|mom),
								data=data.f.ars.di, family = 'gaussian')


a.di.4 <- glmer(log(annual_rs) ~  scale(alone) + scale(sri_strength) + scale(sri_betweenness)
								+ scale(aggr_outstrength) + scale(aggr_instrength) + scale(aggr_betweenness)
								+ scale(affil_outstrength) + scale(affil_instrength) + scale(affil_betweenness)
								+ ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								data=data.f.ars.di, family = 'gaussian')


a.di.5 <- glmer(log(annual_rs) ~  scale(sri_degree) + scale(sri_strength) + scale(sri_betweenness)
								+ scale(aggr_outdegree) + scale(aggr_indegree) + scale(aggr_outstrength) + scale(aggr_instrength) + scale(aggr_betweenness)
								+ scale(affil_outdegree) + scale(affil_indegree) + scale(affil_outstrength) + scale(affil_instrength) + scale(affil_betweenness)
								+ ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								data=data.f.ars.di, family = 'gaussian')


a.di.6 <- glmer(log(annual_rs) ~ scale(sri_degree) + scale(sri_strength)
								+ scale(aggr_outdegree) + scale(aggr_indegree) + scale(aggr_outstrength) + scale(aggr_instrength)
								+ scale(affil_outdegree) + scale(affil_indegree) + scale(affil_outstrength) + scale(affil_instrength)
								+ ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								data=data.f.ars.di, family = 'gaussian')


a.di.7 <- glmer(log(annual_rs) ~  scale(sri_degree)
								+ scale(aggr_outdegree) + scale(aggr_indegree)
								+ scale(affil_outdegree) + scale(affil_indegree)
								+ ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								data=data.f.ars.di, family = 'gaussian')


a.di.8 <- glmer(log(annual_rs) ~ scale(sri_degree) + scale(sri_betweenness)
								+ scale(aggr_outdegree) + scale(aggr_indegree) + scale(aggr_betweenness)
								+ scale(affil_outdegree) + scale(affil_indegree) + scale(affil_betweenness)
								+ ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								data=data.f.ars.di, family = 'gaussian')


a.di.9 <- glmer(log(annual_rs) ~  scale(sri_strength) + scale(sri_betweenness)
								+ scale(aggr_outstrength) + scale(aggr_instrength) + scale(aggr_betweenness)
								+ scale(affil_outstrength) + scale(affil_instrength) + scale(affil_betweenness)
								+ ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								data=data.f.ars.di, family = 'gaussian')


a.di.10 <- glmer(log(annual_rs) ~ scale(alone) + scale(sri_betweenness)
								 + scale(aggr_outstrength) + scale(aggr_instrength)
								 + scale(affil_outstrength) + scale(affil_instrength)
								 + ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								 data=data.f.ars.di, family = 'gaussian')


a.di.11 <- glmer(log(annual_rs) ~ scale(sri_strength)
								 + scale(aggr_outstrength) + scale(aggr_instrength)
								 + scale(affil_outstrength) + scale(affil_instrength)
								 + ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								 data=data.f.ars.di, family = 'gaussian')


a.di.12 <- glmer(log(annual_rs) ~ scale(alone) + scale(sri_betweenness)
								 + scale(aggr_betweenness)
								 + scale(affil_betweenness)
								 + ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								 data=data.f.ars.di, family = 'gaussian')


a.di.13 <- glmer(log(annual_rs) ~ scale(sri_betweenness)
								 + scale(aggr_betweenness)
								 + scale(affil_betweenness)
								 + ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								 data=data.f.ars.di, family = 'gaussian')



#### Adult ###
a.ad.full <- glmer(log(annual_rs) ~  scale(alone) + scale(sri_degree) + scale(sri_strength) + scale(sri_betweenness)
									 + scale(aggr_outdegree) + scale(aggr_indegree) + scale(aggr_outstrength) + scale(aggr_instrength) + scale(aggr_betweenness)
									 + scale(affil_outdegree) + scale(affil_indegree) + scale(affil_outstrength) + scale(affil_instrength) + scale(affil_betweenness)
									 + ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
									 data=data.f.ars.ad, family = 'gaussian')


a.ad.1 <- glmer(log(annual_rs) ~  scale(alone) + scale(sri_degree) + scale(sri_strength)
								+ scale(aggr_outdegree) + scale(aggr_indegree) + scale(aggr_outstrength) + scale(aggr_instrength)
								+ scale(affil_outdegree) + scale(affil_indegree) + scale(affil_outstrength) + scale(affil_instrength)
								+ ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								data=data.f.ars.ad, family = 'gaussian')


a.ad.2 <- glmer(log(annual_rs) ~  scale(alone) + scale(sri_degree)
								+ scale(aggr_outdegree) + scale(aggr_indegree)
								+ scale(affil_outdegree) + scale(affil_indegree)
								+ ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								data=data.f.ars.ad, family = 'gaussian')


a.ad.3 <- glmer(log(annual_rs) ~  scale(alone) + scale(sri_degree) + scale(sri_betweenness)
								+ scale(aggr_outdegree) + scale(aggr_indegree) + scale(aggr_betweenness)
								+ scale(affil_outdegree) + scale(affil_indegree) + scale(affil_betweenness)
								+ ego_period_rank + offset(log(clan_size))  + offset(log(nSession)) + (1|mom),
								data=data.f.ars.ad, family = 'gaussian')


a.ad.4 <- glmer(log(annual_rs) ~  scale(alone) + scale(sri_strength) + scale(sri_betweenness)
								+ scale(aggr_outstrength) + scale(aggr_instrength) + scale(aggr_betweenness)
								+ scale(affil_outstrength) + scale(affil_instrength) + scale(affil_betweenness)
								+ ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								data=data.f.ars.ad, family = 'gaussian')


a.ad.5 <- glmer(log(annual_rs) ~  scale(sri_degree) + scale(sri_strength) + scale(sri_betweenness)
								+ scale(aggr_outdegree) + scale(aggr_indegree) + scale(aggr_outstrength) + scale(aggr_instrength) + scale(aggr_betweenness)
								+ scale(affil_outdegree) + scale(affil_indegree) + scale(affil_outstrength) + scale(affil_instrength) + scale(affil_betweenness)
								+ ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								data=data.f.ars.ad, family = 'gaussian')


a.ad.6 <- glmer(log(annual_rs) ~ scale(sri_degree) + scale(sri_strength)
								+ scale(aggr_outdegree) + scale(aggr_indegree) + scale(aggr_outstrength) + scale(aggr_instrength)
								+ scale(affil_outdegree) + scale(affil_indegree) + scale(affil_outstrength) + scale(affil_instrength)
								+ ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								data=data.f.ars.ad, family = 'gaussian')


a.ad.7 <- glmer(log(annual_rs) ~  scale(sri_degree)
								+ scale(aggr_outdegree) + scale(aggr_indegree)
								+ scale(affil_outdegree) + scale(affil_indegree)
								+ ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								data=data.f.ars.ad, family = 'gaussian')


a.ad.8 <- glmer(log(annual_rs) ~ scale(sri_degree) + scale(sri_betweenness)
								+ scale(aggr_outdegree) + scale(aggr_indegree) + scale(aggr_betweenness)
								+ scale(affil_outdegree) + scale(affil_indegree) + scale(affil_betweenness)
								+ ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								data=data.f.ars.ad, family = 'gaussian')


a.ad.9 <- glmer(log(annual_rs) ~  scale(sri_strength) + scale(sri_betweenness)
								+ scale(aggr_outstrength) + scale(aggr_instrength) + scale(aggr_betweenness)
								+ scale(affil_outstrength) + scale(affil_instrength) + scale(affil_betweenness)
								+ ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								data=data.f.ars.ad, family = 'gaussian')


a.ad.10 <- glmer(log(annual_rs) ~ scale(alone) + scale(sri_betweenness)
								 + scale(aggr_outstrength) + scale(aggr_instrength)
								 + scale(affil_outstrength) + scale(affil_instrength)
								 + ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								 data=data.f.ars.ad, family = 'gaussian')


a.ad.11 <- glmer(log(annual_rs) ~ scale(sri_strength)
								 + scale(aggr_outstrength) + scale(aggr_instrength)
								 + scale(affil_outstrength) + scale(affil_instrength)
								 + ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								 data=data.f.ars.ad, family = 'gaussian')


a.ad.12 <- glmer(log(annual_rs) ~ scale(alone) + scale(sri_betweenness)
								 + scale(aggr_betweenness)
								 + scale(affil_betweenness)
								 + ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								 data=data.f.ars.ad, family = 'gaussian')


a.ad.13 <- glmer(log(annual_rs) ~ scale(sri_betweenness)
								 + scale(aggr_betweenness)
								 + scale(affil_betweenness)
								 + ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								 data=data.f.ars.ad, family = 'gaussian')



AIC.a.cd <- as.data.frame(AICctab(a.cd.full, a.cd.1,a.cd.2, a.cd.3, a.cd.4, a.cd.5, a.cd.6, a.cd.7, a.cd.8, a.cd.9, a.cd.10, a.cd.11, a.cd.12, a.cd.13))

AIC.a.di <- as.data.frame(AICctab(a.di.full, a.di.1,a.di.2, a.di.3, a.di.4, a.di.5, a.di.6, a.di.7, a.di.8, a.di.9, a.di.10, a.di.11, a.di.12, a.di.13))

AIC.a.ad <- as.data.frame(AICctab(a.ad.full, a.ad.1, a.ad.2, a.ad.3, a.ad.4, a.ad.5, a.ad.6, a.ad.7, a.ad.8, a.ad.9, a.ad.10, a.ad.11, a.ad.12, a.ad.13))

AIC.ars <- as.data.frame(AICctab(a.cd.7, a.di.7, a.ad.7))

### Best models and randomizations ----
mod.7.terms <- c("ego_period_rank", "scale(sri_degree)", "scale(aggr_outdegree)", "scale(aggr_indegree)",
								 "scale(affil_outdegree)", "scale(affil_indegree)", "sd__(Intercept)")

mod.11.terms <- c("ego_period_rank", "scale(sri_strength)", "scale(aggr_outstrength)", "scale(aggr_instrength)",
								 "scale(affil_outstrength)", "scale(affil_instrength)", "sd__(Intercept)")

mod.13.terms <- c("ego_period_rank", "scale(sri_betweenness)", "scale(aggr_betweenness)", "scale(affil_betweenness)", "sd__(Intercept)")

