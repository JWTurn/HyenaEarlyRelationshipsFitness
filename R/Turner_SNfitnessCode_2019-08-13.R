### Models ====
# SN fitness
# Julie Turner, Alec Robitaille, Patrick Bills, Kay Holekamp



### Packages ----
# remotes::install_github('bbolker/broom.mixed')
libs <- c('lme4', 'data.table', 'broom.mixed', 'car', 'bbmle')
lapply(libs, require, character.only = TRUE)


### Data ----

### Data labels
# ego = focal female for analyses
# period = cd (communal den), di (den independent), or adult (early adulthood)
# ego_period_rank = ego's rank during that period
# mom = ego's mother
# clan_size = number of individuals in the clan during stated period for that ego
# nSession = number of sessions the ego was observed in during that period
# alone = alone rate
# twi_degree = degree centrality of the ego during that period based on the association network (twice weight index = twi)
# twi_strength = stength of the ego during that period based on the association network (twice weight index = twi)
# twi_betweenness = betweenness of the ego during that period based on the association network (twice weight index = twi)
# aggr_outdegree = out-degree centrality of the ego during that period based on the aggression network
# aggr_indegree = in-degree centrality of the ego during that period based on the aggression network
# aggr_outstrength = out-stength of the ego during that period based on the aggression network
# aggr_instrength = in-stength of the ego during that period based on the aggression network
# aggr_betweenness = betweenness of the ego during that period based on the aggression network
# affil_outdegree = out-degree centrality of the ego during that period based on the affiliation network (ll = lift leg, how a greeting is initiated)
# affil_indegree = in-degree centrality of the ego during that period based on the affiliation network (ll = lift leg)
# affil_outstrength = out-stength of the ego during that period based on the affiliation network (ll = lift leg)
# affil_instrength = in-stength of the ego during that period based on the affiliation network (ll = lift leg)
# affil_betweenness = betweenness of the ego during that period based on the affiliation network (ll = lift leg)
# longevity_years = age in years when the ego died
# annual_rs = annual reproductive success (ARS) for the ego
# iteration = iteration of the randomizations, 0 being the observed data
# observed = TRUE - the data comes from observations; FALSE - the data comes from randomizations


# all data including randomizations
DT <- fread("SNfitnessData_2019-04-29.csv", header = T, sep = ',')
DT<-DT[nSession>=10]

# just the observed data
DT.obs <- DT[iteration == 0]

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
l.cd.full <- glmer(log(longevity_years) ~  scale(alone) + scale(twi_degree) + scale(twi_strength) + scale(twi_betweenness)
										 + scale(aggr_outdegree) + scale(aggr_indegree) + scale(aggr_outstrength) + scale(aggr_instrength) + scale(aggr_betweenness)
										 + scale(affil_outdegree) + scale(affil_indegree) + scale(affil_outstrength) + scale(affil_instrength) + scale(affil_betweenness)
										 + ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
										 data=data.f.long.cd, family = 'gaussian')


l.cd.1 <- glmer(log(longevity_years) ~  scale(alone) + scale(twi_degree) + scale(twi_strength)
									+ scale(aggr_outdegree) + scale(aggr_indegree) + scale(aggr_outstrength) + scale(aggr_instrength)
									+ scale(affil_outdegree) + scale(affil_indegree) + scale(affil_outstrength) + scale(affil_instrength)
									+ ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
									data=data.f.long.cd, family = 'gaussian')


l.cd.2 <- glmer(log(longevity_years) ~  scale(alone) + scale(twi_degree)
									+ scale(aggr_outdegree) + scale(aggr_indegree)
									+ scale(affil_outdegree) + scale(affil_indegree)
									+ ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
									data=data.f.long.cd, family = 'gaussian')


l.cd.3 <- glmer(log(longevity_years) ~  scale(alone) + scale(twi_degree) + scale(twi_betweenness)
									+ scale(aggr_outdegree) + scale(aggr_indegree) + scale(aggr_betweenness)
									+ scale(affil_outdegree) + scale(affil_indegree) + scale(affil_betweenness)
									+ ego_period_rank + offset(log(clan_size))  + offset(log(nSession)) + (1|mom),
									data=data.f.long.cd, family = 'gaussian')


l.cd.4 <- glmer(log(longevity_years) ~  scale(alone) + scale(twi_strength) + scale(twi_betweenness)
									+ scale(aggr_outstrength) + scale(aggr_instrength) + scale(aggr_betweenness)
									+ scale(affil_outstrength) + scale(affil_instrength) + scale(affil_betweenness)
									+ ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
									data=data.f.long.cd, family = 'gaussian')


l.cd.5 <- glmer(log(longevity_years) ~  scale(twi_degree) + scale(twi_strength) + scale(twi_betweenness)
									+ scale(aggr_outdegree) + scale(aggr_indegree) + scale(aggr_outstrength) + scale(aggr_instrength) + scale(aggr_betweenness)
									+ scale(affil_outdegree) + scale(affil_indegree) + scale(affil_outstrength) + scale(affil_instrength) + scale(affil_betweenness)
									+ ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
									data=data.f.long.cd, family = 'gaussian')


l.cd.6 <- glmer(log(longevity_years) ~ scale(twi_degree) + scale(twi_strength)
									+ scale(aggr_outdegree) + scale(aggr_indegree) + scale(aggr_outstrength) + scale(aggr_instrength)
									+ scale(affil_outdegree) + scale(affil_indegree) + scale(affil_outstrength) + scale(affil_instrength)
									+ ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
									data=data.f.long.cd, family = 'gaussian')


l.cd.7 <- glmer(log(longevity_years) ~  scale(twi_degree)
									+ scale(aggr_outdegree) + scale(aggr_indegree)
									+ scale(affil_outdegree) + scale(affil_indegree)
									+ ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
									data=data.f.long.cd, family = 'gaussian')


l.cd.8 <- glmer(log(longevity_years) ~ scale(twi_degree) + scale(twi_betweenness)
									+ scale(aggr_outdegree) + scale(aggr_indegree) + scale(aggr_betweenness)
									+ scale(affil_outdegree) + scale(affil_indegree) + scale(affil_betweenness)
									+ ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
									data=data.f.long.cd, family = 'gaussian')


l.cd.9 <- glmer(log(longevity_years) ~  scale(twi_strength) + scale(twi_betweenness)
									+ scale(aggr_outstrength) + scale(aggr_instrength) + scale(aggr_betweenness)
									+ scale(affil_outstrength) + scale(affil_instrength) + scale(affil_betweenness)
									+ ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
									data=data.f.long.cd, family = 'gaussian')


l.cd.10 <- glmer(log(longevity_years) ~ scale(alone) + scale(twi_betweenness)
									 + scale(aggr_outstrength) + scale(aggr_instrength)
									 + scale(affil_outstrength) + scale(affil_instrength)
									 + ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
									 data=data.f.long.cd, family = 'gaussian')


l.cd.11 <- glmer(log(longevity_years) ~ scale(twi_strength)
									 + scale(aggr_outstrength) + scale(aggr_instrength)
									 + scale(affil_outstrength) + scale(affil_instrength)
									 + ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
									 data=data.f.long.cd, family = 'gaussian')


l.cd.12 <- glmer(log(longevity_years) ~ scale(alone) + scale(twi_betweenness)
								 + scale(aggr_betweenness)
								 + scale(affil_betweenness)
								 + ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								 data=data.f.long.cd, family = 'gaussian')


l.cd.13 <- glmer(log(longevity_years) ~ scale(twi_betweenness)
								 + scale(aggr_betweenness)
								 + scale(affil_betweenness)
								 + ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								 data=data.f.long.cd, family = 'gaussian')



#### DI ###
l.di.full <- glmer(log(longevity_years) ~  scale(alone) + scale(twi_degree) + scale(twi_strength) + scale(twi_betweenness)
									 + scale(aggr_outdegree) + scale(aggr_indegree) + scale(aggr_outstrength) + scale(aggr_instrength) + scale(aggr_betweenness)
									 + scale(affil_outdegree) + scale(affil_indegree) + scale(affil_outstrength) + scale(affil_instrength) + scale(affil_betweenness)
									 + ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
									 data=data.f.long.di, family = 'gaussian')


l.di.1 <- glmer(log(longevity_years) ~  scale(alone) + scale(twi_degree) + scale(twi_strength)
								+ scale(aggr_outdegree) + scale(aggr_indegree) + scale(aggr_outstrength) + scale(aggr_instrength)
								+ scale(affil_outdegree) + scale(affil_indegree) + scale(affil_outstrength) + scale(affil_instrength)
								+ ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								data=data.f.long.di, family = 'gaussian')


l.di.2 <- glmer(log(longevity_years) ~  scale(alone) + scale(twi_degree)
								+ scale(aggr_outdegree) + scale(aggr_indegree)
								+ scale(affil_outdegree) + scale(affil_indegree)
								+ ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								data=data.f.long.di, family = 'gaussian')


l.di.3 <- glmer(log(longevity_years) ~  scale(alone) + scale(twi_degree) + scale(twi_betweenness)
								+ scale(aggr_outdegree) + scale(aggr_indegree) + scale(aggr_betweenness)
								+ scale(affil_outdegree) + scale(affil_indegree) + scale(affil_betweenness)
								+ ego_period_rank + offset(log(clan_size))  + offset(log(nSession)) + (1|mom),
								data=data.f.long.di, family = 'gaussian')


l.di.4 <- glmer(log(longevity_years) ~  scale(alone) + scale(twi_strength) + scale(twi_betweenness)
								+ scale(aggr_outstrength) + scale(aggr_instrength) + scale(aggr_betweenness)
								+ scale(affil_outstrength) + scale(affil_instrength) + scale(affil_betweenness)
								+ ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								data=data.f.long.di, family = 'gaussian')


l.di.5 <- glmer(log(longevity_years) ~  scale(twi_degree) + scale(twi_strength) + scale(twi_betweenness)
								+ scale(aggr_outdegree) + scale(aggr_indegree) + scale(aggr_outstrength) + scale(aggr_instrength) + scale(aggr_betweenness)
								+ scale(affil_outdegree) + scale(affil_indegree) + scale(affil_outstrength) + scale(affil_instrength) + scale(affil_betweenness)
								+ ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								data=data.f.long.di, family = 'gaussian')


l.di.6 <- glmer(log(longevity_years) ~ scale(twi_degree) + scale(twi_strength)
								+ scale(aggr_outdegree) + scale(aggr_indegree) + scale(aggr_outstrength) + scale(aggr_instrength)
								+ scale(affil_outdegree) + scale(affil_indegree) + scale(affil_outstrength) + scale(affil_instrength)
								+ ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								data=data.f.long.di, family = 'gaussian')


l.di.7 <- glmer(log(longevity_years) ~  scale(twi_degree)
								+ scale(aggr_outdegree) + scale(aggr_indegree)
								+ scale(affil_outdegree) + scale(affil_indegree)
								+ ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								data=data.f.long.di, family = 'gaussian')


l.di.8 <- glmer(log(longevity_years) ~ scale(twi_degree) + scale(twi_betweenness)
								+ scale(aggr_outdegree) + scale(aggr_indegree) + scale(aggr_betweenness)
								+ scale(affil_outdegree) + scale(affil_indegree) + scale(affil_betweenness)
								+ ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								data=data.f.long.di, family = 'gaussian')


l.di.9 <- glmer(log(longevity_years) ~  scale(twi_strength) + scale(twi_betweenness)
								+ scale(aggr_outstrength) + scale(aggr_instrength) + scale(aggr_betweenness)
								+ scale(affil_outstrength) + scale(affil_instrength) + scale(affil_betweenness)
								+ ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								data=data.f.long.di, family = 'gaussian')


l.di.10 <- glmer(log(longevity_years) ~ scale(alone) + scale(twi_betweenness)
								 + scale(aggr_outstrength) + scale(aggr_instrength)
								 + scale(affil_outstrength) + scale(affil_instrength)
								 + ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								 data=data.f.long.di, family = 'gaussian')


l.di.11 <- glmer(log(longevity_years) ~ scale(twi_strength)
								 + scale(aggr_outstrength) + scale(aggr_instrength)
								 + scale(affil_outstrength) + scale(affil_instrength)
								 + ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								 data=data.f.long.di, family = 'gaussian')


l.di.12 <- glmer(log(longevity_years) ~ scale(alone) + scale(twi_betweenness)
								 + scale(aggr_betweenness)
								 + scale(affil_betweenness)
								 + ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								 data=data.f.long.di, family = 'gaussian')


l.di.13 <- glmer(log(longevity_years) ~ scale(twi_betweenness)
								 + scale(aggr_betweenness)
								 + scale(affil_betweenness)
								 + ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								 data=data.f.long.di, family = 'gaussian')



#### Adult ###
l.ad.full <- glmer(log(longevity_years) ~  scale(alone) + scale(twi_degree) + scale(twi_strength) + scale(twi_betweenness)
									 + scale(aggr_outdegree) + scale(aggr_indegree) + scale(aggr_outstrength) + scale(aggr_instrength) + scale(aggr_betweenness)
									 + scale(affil_outdegree) + scale(affil_indegree) + scale(affil_outstrength) + scale(affil_instrength) + scale(affil_betweenness)
									 + ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
									 data=data.f.long.ad, family = 'gaussian')


l.ad.1 <- glmer(log(longevity_years) ~  scale(alone) + scale(twi_degree) + scale(twi_strength)
								+ scale(aggr_outdegree) + scale(aggr_indegree) + scale(aggr_outstrength) + scale(aggr_instrength)
								+ scale(affil_outdegree) + scale(affil_indegree) + scale(affil_outstrength) + scale(affil_instrength)
								+ ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								data=data.f.long.ad, family = 'gaussian')


l.ad.2 <- glmer(log(longevity_years) ~  scale(alone) + scale(twi_degree)
								+ scale(aggr_outdegree) + scale(aggr_indegree)
								+ scale(affil_outdegree) + scale(affil_indegree)
								+ ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								data=data.f.long.ad, family = 'gaussian')


l.ad.3 <- glmer(log(longevity_years) ~  scale(alone) + scale(twi_degree) + scale(twi_betweenness)
								+ scale(aggr_outdegree) + scale(aggr_indegree) + scale(aggr_betweenness)
								+ scale(affil_outdegree) + scale(affil_indegree) + scale(affil_betweenness)
								+ ego_period_rank + offset(log(clan_size))  + offset(log(nSession)) + (1|mom),
								data=data.f.long.ad, family = 'gaussian')


l.ad.4 <- glmer(log(longevity_years) ~  scale(alone) + scale(twi_strength) + scale(twi_betweenness)
								+ scale(aggr_outstrength) + scale(aggr_instrength) + scale(aggr_betweenness)
								+ scale(affil_outstrength) + scale(affil_instrength) + scale(affil_betweenness)
								+ ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								data=data.f.long.ad, family = 'gaussian')


l.ad.5 <- glmer(log(longevity_years) ~  scale(twi_degree) + scale(twi_strength) + scale(twi_betweenness)
								+ scale(aggr_outdegree) + scale(aggr_indegree) + scale(aggr_outstrength) + scale(aggr_instrength) + scale(aggr_betweenness)
								+ scale(affil_outdegree) + scale(affil_indegree) + scale(affil_outstrength) + scale(affil_instrength) + scale(affil_betweenness)
								+ ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								data=data.f.long.ad, family = 'gaussian')


l.ad.6 <- glmer(log(longevity_years) ~ scale(twi_degree) + scale(twi_strength)
								+ scale(aggr_outdegree) + scale(aggr_indegree) + scale(aggr_outstrength) + scale(aggr_instrength)
								+ scale(affil_outdegree) + scale(affil_indegree) + scale(affil_outstrength) + scale(affil_instrength)
								+ ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								data=data.f.long.ad, family = 'gaussian')


l.ad.7 <- glmer(log(longevity_years) ~  scale(twi_degree)
								+ scale(aggr_outdegree) + scale(aggr_indegree)
								+ scale(affil_outdegree) + scale(affil_indegree)
								+ ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								data=data.f.long.ad, family = 'gaussian')


l.ad.8 <- glmer(log(longevity_years) ~ scale(twi_degree) + scale(twi_betweenness)
								+ scale(aggr_outdegree) + scale(aggr_indegree) + scale(aggr_betweenness)
								+ scale(affil_outdegree) + scale(affil_indegree) + scale(affil_betweenness)
								+ ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								data=data.f.long.ad, family = 'gaussian')


l.ad.9 <- glmer(log(longevity_years) ~  scale(twi_strength) + scale(twi_betweenness)
								+ scale(aggr_outstrength) + scale(aggr_instrength) + scale(aggr_betweenness)
								+ scale(affil_outstrength) + scale(affil_instrength) + scale(affil_betweenness)
								+ ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								data=data.f.long.ad, family = 'gaussian')


l.ad.10 <- glmer(log(longevity_years) ~ scale(alone) + scale(twi_betweenness)
								 + scale(aggr_outstrength) + scale(aggr_instrength)
								 + scale(affil_outstrength) + scale(affil_instrength)
								 + ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								 data=data.f.long.ad, family = 'gaussian')


l.ad.11 <- glmer(log(longevity_years) ~ scale(twi_strength)
								 + scale(aggr_outstrength) + scale(aggr_instrength)
								 + scale(affil_outstrength) + scale(affil_instrength)
								 + ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								 data=data.f.long.ad, family = 'gaussian')


l.ad.12 <- glmer(log(longevity_years) ~ scale(alone) + scale(twi_betweenness)
								 + scale(aggr_betweenness)
								 + scale(affil_betweenness)
								 + ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								 data=data.f.long.ad, family = 'gaussian')


l.ad.13 <- glmer(log(longevity_years) ~ scale(twi_betweenness)
								 + scale(aggr_betweenness)
								 + scale(affil_betweenness)
								 + ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								 data=data.f.long.ad, family = 'gaussian')



# comparing models

AICtab(l.cd.full, l.cd.1,l.cd.2, l.cd.3, l.cd.4, l.cd.5, l.cd.6, l.cd.7, l.cd.8, l.cd.9, l.cd.10, l.cd.11, l.cd.12, l.cd.13)

AICtab(l.di.full, l.di.1,l.di.2, l.di.3, l.di.4, l.di.5, l.di.6, l.di.7, l.di.8, l.di.9, l.di.10, l.di.11, l.di.12, l.di.13)

AICtab(l.ad.full, l.ad.1,l.ad.2, l.ad.3, l.ad.4, l.ad.5, l.ad.6, l.ad.7, l.ad.8, l.ad.9, l.ad.10, l.ad.11, l.ad.12, l.ad.13)




#### ARS ####
### CD ###
a.cd.full <- glmer(log(annual_rs) ~  scale(alone) + scale(twi_degree) + scale(twi_strength) + scale(twi_betweenness)
									 + scale(aggr_outdegree) + scale(aggr_indegree) + scale(aggr_outstrength) + scale(aggr_instrength) + scale(aggr_betweenness)
									 + scale(affil_outdegree) + scale(affil_indegree) + scale(affil_outstrength) + scale(affil_instrength) + scale(affil_betweenness)
									 + ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
									 data=data.f.ars.cd, family = 'gaussian')


a.cd.1 <- glmer(log(annual_rs) ~  scale(alone) + scale(twi_degree) + scale(twi_strength)
								+ scale(aggr_outdegree) + scale(aggr_indegree) + scale(aggr_outstrength) + scale(aggr_instrength)
								+ scale(affil_outdegree) + scale(affil_indegree) + scale(affil_outstrength) + scale(affil_instrength)
								+ ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								data=data.f.ars.cd, family = 'gaussian')


a.cd.2 <- glmer(log(annual_rs) ~  scale(alone) + scale(twi_degree)
								+ scale(aggr_outdegree) + scale(aggr_indegree)
								+ scale(affil_outdegree) + scale(affil_indegree)
								+ ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								data=data.f.ars.cd, family = 'gaussian')


a.cd.3 <- glmer(log(annual_rs) ~  scale(alone) + scale(twi_degree) + scale(twi_betweenness)
								+ scale(aggr_outdegree) + scale(aggr_indegree) + scale(aggr_betweenness)
								+ scale(affil_outdegree) + scale(affil_indegree) + scale(affil_betweenness)
								+ ego_period_rank + offset(log(clan_size))  + offset(log(nSession)) + (1|mom),
								data=data.f.ars.cd, family = 'gaussian')


a.cd.4 <- glmer(log(annual_rs) ~  scale(alone) + scale(twi_strength) + scale(twi_betweenness)
								+ scale(aggr_outstrength) + scale(aggr_instrength) + scale(aggr_betweenness)
								+ scale(affil_outstrength) + scale(affil_instrength) + scale(affil_betweenness)
								+ ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								data=data.f.ars.cd, family = 'gaussian')


a.cd.5 <- glmer(log(annual_rs) ~  scale(twi_degree) + scale(twi_strength) + scale(twi_betweenness)
								+ scale(aggr_outdegree) + scale(aggr_indegree) + scale(aggr_outstrength) + scale(aggr_instrength) + scale(aggr_betweenness)
								+ scale(affil_outdegree) + scale(affil_indegree) + scale(affil_outstrength) + scale(affil_instrength) + scale(affil_betweenness)
								+ ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								data=data.f.ars.cd, family = 'gaussian')


a.cd.6 <- glmer(log(annual_rs) ~ scale(twi_degree) + scale(twi_strength)
								+ scale(aggr_outdegree) + scale(aggr_indegree) + scale(aggr_outstrength) + scale(aggr_instrength)
								+ scale(affil_outdegree) + scale(affil_indegree) + scale(affil_outstrength) + scale(affil_instrength)
								+ ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								data=data.f.ars.cd, family = 'gaussian')


a.cd.7 <- glmer(log(annual_rs) ~  scale(twi_degree)
								+ scale(aggr_outdegree) + scale(aggr_indegree)
								+ scale(affil_outdegree) + scale(affil_indegree)
								+ ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								data=data.f.ars.cd, family = 'gaussian')


a.cd.8 <- glmer(log(annual_rs) ~ scale(twi_degree) + scale(twi_betweenness)
								+ scale(aggr_outdegree) + scale(aggr_indegree) + scale(aggr_betweenness)
								+ scale(affil_outdegree) + scale(affil_indegree) + scale(affil_betweenness)
								+ ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								data=data.f.ars.cd, family = 'gaussian')


a.cd.9 <- glmer(log(annual_rs) ~  scale(twi_strength) + scale(twi_betweenness)
								+ scale(aggr_outstrength) + scale(aggr_instrength) + scale(aggr_betweenness)
								+ scale(affil_outstrength) + scale(affil_instrength) + scale(affil_betweenness)
								+ ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								data=data.f.ars.cd, family = 'gaussian')


a.cd.10 <- glmer(log(annual_rs) ~ scale(alone) + scale(twi_betweenness)
								 + scale(aggr_outstrength) + scale(aggr_instrength)
								 + scale(affil_outstrength) + scale(affil_instrength)
								 + ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								 data=data.f.ars.cd, family = 'gaussian')


a.cd.11 <- glmer(log(annual_rs) ~ scale(twi_strength)
								 + scale(aggr_outstrength) + scale(aggr_instrength)
								 + scale(affil_outstrength) + scale(affil_instrength)
								 + ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								 data=data.f.ars.cd, family = 'gaussian')


a.cd.12 <- glmer(log(annual_rs) ~ scale(alone) + scale(twi_betweenness)
								 + scale(aggr_betweenness)
								 + scale(affil_betweenness)
								 + ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								 data=data.f.ars.cd, family = 'gaussian')


a.cd.13 <- glmer(log(annual_rs) ~ scale(twi_betweenness)
								 + scale(aggr_betweenness)
								 + scale(affil_betweenness)
								 + ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								 data=data.f.ars.cd, family = 'gaussian')



#### DI ###
a.di.full <- glmer(log(annual_rs) ~  scale(alone) + scale(twi_degree) + scale(twi_strength) + scale(twi_betweenness)
									 + scale(aggr_outdegree) + scale(aggr_indegree) + scale(aggr_outstrength) + scale(aggr_instrength) + scale(aggr_betweenness)
									 + scale(affil_outdegree) + scale(affil_indegree) + scale(affil_outstrength) + scale(affil_instrength) + scale(affil_betweenness)
									 + ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
									 data=data.f.ars.di, family = 'gaussian')


a.di.1 <- glmer(log(annual_rs) ~  scale(alone) + scale(twi_degree) + scale(twi_strength)
								+ scale(aggr_outdegree) + scale(aggr_indegree) + scale(aggr_outstrength) + scale(aggr_instrength)
								+ scale(affil_outdegree) + scale(affil_indegree) + scale(affil_outstrength) + scale(affil_instrength)
								+ ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								data=data.f.ars.di, family = 'gaussian')


a.di.2 <- glmer(log(annual_rs) ~  scale(alone) + scale(twi_degree)
								+ scale(aggr_outdegree) + scale(aggr_indegree)
								+ scale(affil_outdegree) + scale(affil_indegree)
								+ ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								data=data.f.ars.di, family = 'gaussian')


a.di.3 <- glmer(log(annual_rs) ~  scale(alone) + scale(twi_degree) + scale(twi_betweenness)
								+ scale(aggr_outdegree) + scale(aggr_indegree) + scale(aggr_betweenness)
								+ scale(affil_outdegree) + scale(affil_indegree) + scale(affil_betweenness)
								+ ego_period_rank + offset(log(clan_size))  + offset(log(nSession)) + (1|mom),
								data=data.f.ars.di, family = 'gaussian')


a.di.4 <- glmer(log(annual_rs) ~  scale(alone) + scale(twi_strength) + scale(twi_betweenness)
								+ scale(aggr_outstrength) + scale(aggr_instrength) + scale(aggr_betweenness)
								+ scale(affil_outstrength) + scale(affil_instrength) + scale(affil_betweenness)
								+ ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								data=data.f.ars.di, family = 'gaussian')


a.di.5 <- glmer(log(annual_rs) ~  scale(twi_degree) + scale(twi_strength) + scale(twi_betweenness)
								+ scale(aggr_outdegree) + scale(aggr_indegree) + scale(aggr_outstrength) + scale(aggr_instrength) + scale(aggr_betweenness)
								+ scale(affil_outdegree) + scale(affil_indegree) + scale(affil_outstrength) + scale(affil_instrength) + scale(affil_betweenness)
								+ ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								data=data.f.ars.di, family = 'gaussian')


a.di.6 <- glmer(log(annual_rs) ~ scale(twi_degree) + scale(twi_strength)
								+ scale(aggr_outdegree) + scale(aggr_indegree) + scale(aggr_outstrength) + scale(aggr_instrength)
								+ scale(affil_outdegree) + scale(affil_indegree) + scale(affil_outstrength) + scale(affil_instrength)
								+ ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								data=data.f.ars.di, family = 'gaussian')


a.di.7 <- glmer(log(annual_rs) ~  scale(twi_degree)
								+ scale(aggr_outdegree) + scale(aggr_indegree)
								+ scale(affil_outdegree) + scale(affil_indegree)
								+ ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								data=data.f.ars.di, family = 'gaussian')


a.di.8 <- glmer(log(annual_rs) ~ scale(twi_degree) + scale(twi_betweenness)
								+ scale(aggr_outdegree) + scale(aggr_indegree) + scale(aggr_betweenness)
								+ scale(affil_outdegree) + scale(affil_indegree) + scale(affil_betweenness)
								+ ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								data=data.f.ars.di, family = 'gaussian')


a.di.9 <- glmer(log(annual_rs) ~  scale(twi_strength) + scale(twi_betweenness)
								+ scale(aggr_outstrength) + scale(aggr_instrength) + scale(aggr_betweenness)
								+ scale(affil_outstrength) + scale(affil_instrength) + scale(affil_betweenness)
								+ ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								data=data.f.ars.di, family = 'gaussian')


a.di.10 <- glmer(log(annual_rs) ~ scale(alone) + scale(twi_betweenness)
								 + scale(aggr_outstrength) + scale(aggr_instrength)
								 + scale(affil_outstrength) + scale(affil_instrength)
								 + ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								 data=data.f.ars.di, family = 'gaussian')


a.di.11 <- glmer(log(annual_rs) ~ scale(twi_strength)
								 + scale(aggr_outstrength) + scale(aggr_instrength)
								 + scale(affil_outstrength) + scale(affil_instrength)
								 + ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								 data=data.f.ars.di, family = 'gaussian')


a.di.12 <- glmer(log(annual_rs) ~ scale(alone) + scale(twi_betweenness)
								 + scale(aggr_betweenness)
								 + scale(affil_betweenness)
								 + ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								 data=data.f.ars.di, family = 'gaussian')


a.di.13 <- glmer(log(annual_rs) ~ scale(twi_betweenness)
								 + scale(aggr_betweenness)
								 + scale(affil_betweenness)
								 + ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								 data=data.f.ars.di, family = 'gaussian')



#### Adult ###
a.ad.full <- glmer(log(annual_rs) ~  scale(alone) + scale(twi_degree) + scale(twi_strength) + scale(twi_betweenness)
									 + scale(aggr_outdegree) + scale(aggr_indegree) + scale(aggr_outstrength) + scale(aggr_instrength) + scale(aggr_betweenness)
									 + scale(affil_outdegree) + scale(affil_indegree) + scale(affil_outstrength) + scale(affil_instrength) + scale(affil_betweenness)
									 + ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
									 data=data.f.ars.ad, family = 'gaussian')


a.ad.1 <- glmer(log(annual_rs) ~  scale(alone) + scale(twi_degree) + scale(twi_strength)
								+ scale(aggr_outdegree) + scale(aggr_indegree) + scale(aggr_outstrength) + scale(aggr_instrength)
								+ scale(affil_outdegree) + scale(affil_indegree) + scale(affil_outstrength) + scale(affil_instrength)
								+ ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								data=data.f.ars.ad, family = 'gaussian')


a.ad.2 <- glmer(log(annual_rs) ~  scale(alone) + scale(twi_degree)
								+ scale(aggr_outdegree) + scale(aggr_indegree)
								+ scale(affil_outdegree) + scale(affil_indegree)
								+ ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								data=data.f.ars.ad, family = 'gaussian')


a.ad.3 <- glmer(log(annual_rs) ~  scale(alone) + scale(twi_degree) + scale(twi_betweenness)
								+ scale(aggr_outdegree) + scale(aggr_indegree) + scale(aggr_betweenness)
								+ scale(affil_outdegree) + scale(affil_indegree) + scale(affil_betweenness)
								+ ego_period_rank + offset(log(clan_size))  + offset(log(nSession)) + (1|mom),
								data=data.f.ars.ad, family = 'gaussian')


a.ad.4 <- glmer(log(annual_rs) ~  scale(alone) + scale(twi_strength) + scale(twi_betweenness)
								+ scale(aggr_outstrength) + scale(aggr_instrength) + scale(aggr_betweenness)
								+ scale(affil_outstrength) + scale(affil_instrength) + scale(affil_betweenness)
								+ ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								data=data.f.ars.ad, family = 'gaussian')


a.ad.5 <- glmer(log(annual_rs) ~  scale(twi_degree) + scale(twi_strength) + scale(twi_betweenness)
								+ scale(aggr_outdegree) + scale(aggr_indegree) + scale(aggr_outstrength) + scale(aggr_instrength) + scale(aggr_betweenness)
								+ scale(affil_outdegree) + scale(affil_indegree) + scale(affil_outstrength) + scale(affil_instrength) + scale(affil_betweenness)
								+ ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								data=data.f.ars.ad, family = 'gaussian')


a.ad.6 <- glmer(log(annual_rs) ~ scale(twi_degree) + scale(twi_strength)
								+ scale(aggr_outdegree) + scale(aggr_indegree) + scale(aggr_outstrength) + scale(aggr_instrength)
								+ scale(affil_outdegree) + scale(affil_indegree) + scale(affil_outstrength) + scale(affil_instrength)
								+ ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								data=data.f.ars.ad, family = 'gaussian')


a.ad.7 <- glmer(log(annual_rs) ~  scale(twi_degree)
								+ scale(aggr_outdegree) + scale(aggr_indegree)
								+ scale(affil_outdegree) + scale(affil_indegree)
								+ ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								data=data.f.ars.ad, family = 'gaussian')


a.ad.8 <- glmer(log(annual_rs) ~ scale(twi_degree) + scale(twi_betweenness)
								+ scale(aggr_outdegree) + scale(aggr_indegree) + scale(aggr_betweenness)
								+ scale(affil_outdegree) + scale(affil_indegree) + scale(affil_betweenness)
								+ ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								data=data.f.ars.ad, family = 'gaussian')


a.ad.9 <- glmer(log(annual_rs) ~  scale(twi_strength) + scale(twi_betweenness)
								+ scale(aggr_outstrength) + scale(aggr_instrength) + scale(aggr_betweenness)
								+ scale(affil_outstrength) + scale(affil_instrength) + scale(affil_betweenness)
								+ ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								data=data.f.ars.ad, family = 'gaussian')


a.ad.10 <- glmer(log(annual_rs) ~ scale(alone) + scale(twi_betweenness)
								 + scale(aggr_outstrength) + scale(aggr_instrength)
								 + scale(affil_outstrength) + scale(affil_instrength)
								 + ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								 data=data.f.ars.ad, family = 'gaussian')


a.ad.11 <- glmer(log(annual_rs) ~ scale(twi_strength)
								 + scale(aggr_outstrength) + scale(aggr_instrength)
								 + scale(affil_outstrength) + scale(affil_instrength)
								 + ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								 data=data.f.ars.ad, family = 'gaussian')


a.ad.12 <- glmer(log(annual_rs) ~ scale(alone) + scale(twi_betweenness)
								 + scale(aggr_betweenness)
								 + scale(affil_betweenness)
								 + ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								 data=data.f.ars.ad, family = 'gaussian')


a.ad.13 <- glmer(log(annual_rs) ~ scale(twi_betweenness)
								 + scale(aggr_betweenness)
								 + scale(affil_betweenness)
								 + ego_period_rank + offset(log(clan_size)) + offset(log(nSession)) + (1|mom),
								 data=data.f.ars.ad, family = 'gaussian')



AICtab(a.cd.full, a.cd.1,a.cd.2, a.cd.3, a.cd.4, a.cd.5, a.cd.6, a.cd.7, a.cd.8, a.cd.9, a.cd.10, a.cd.11, a.cd.12, a.cd.13)

AICtab(a.di.full, a.di.1,a.di.2, a.di.3, a.di.4, a.di.5, a.di.6, a.di.7, a.di.8, a.di.9, a.di.10, a.di.11, a.di.12, a.di.13)

AICtab(a.ad.full, a.ad.1, a.ad.2, a.ad.3, a.ad.4, a.ad.5, a.ad.6, a.ad.7, a.ad.8, a.ad.9, a.ad.10, a.ad.11, a.ad.12, a.ad.13)



### Best models and randomizations ----
#### best models for longevity CD ####
# check the VIF of the model
vif(l.cd.7)

# run model for each iteration
DT.lcd.7 <- DT[period == 'cd', {
	m <- glmer(
		log(longevity_years) ~
			ego_period_rank +
			scale(twi_degree) +
			scale(aggr_outdegree) + scale(aggr_indegree) +
			scale(affil_outdegree) + scale(affil_indegree) +
			offset(log(clan_size)) +
			offset(log(nSession)) +
			(1 | mom),
		data = .SD,
		family = 'gaussian'
	)
	tidy(m)
}, by = iteration]

# set up model outputs so can compare random model output to observed model output
DT.lcd.7.p <-
	merge(
		DT.lcd.7[iteration != 0],
		DT.lcd.7[iteration == 0, .(effect, group, term, estimate)],
		by = c('effect', 'group', 'term'),
		suffixes = c('', '.obs')
	)

# calculate two-tailed p-values and ranges of random model parameter estimates
# less is relative to the lowest values of the random ranges, more is relative to the highest values of the random ranges
# sum is total value, and p is proportions
DT.lcd.7.pboth <- DT.lcd.7.p[, .(estimate = unique(estimate.obs), min = min(estimate), max = max(estimate),
																			 sumless = sum(estimate < estimate.obs), summore = sum(estimate > estimate.obs),
																			 pless = sum(estimate < estimate.obs)/1000, pmore = sum(estimate > estimate.obs)/1000),
																	 term]




vif(l.cd.2)
DT.lcd.2 <- DT[period == 'cd', {
	m <- glmer(
		log(longevity_years) ~
			ego_period_rank +
			scale(alone) +
			scale(twi_degree) +
			scale(aggr_outdegree) + scale(aggr_indegree) +
			scale(affil_outdegree) + scale(affil_indegree) +
			offset(log(clan_size)) +
			offset(log(nSession)) +
			(1 | mom),
		data = .SD,
		family = 'gaussian'
	)
	tidy(m)
}, by = iteration]


DT.lcd.2.p <-
	merge(
		DT.lcd.2[iteration != 0],
		DT.lcd.2[iteration == 0, .(effect, group, term, estimate)],
		by = c('effect', 'group', 'term'),
		suffixes = c('', '.obs')
	)


DT.lcd.2.pboth <- DT.lcd.2.p[, .(estimate = unique(estimate.obs), min = min(estimate), max = max(estimate),
																			 sumless = sum(estimate < estimate.obs), summore = sum(estimate > estimate.obs),
																			 pless = sum(estimate < estimate.obs)/1000, pmore = sum(estimate > estimate.obs)/1000),
																	 term]




#### best model for longevity DI ####
vif(l.di.7)
DT.ldi.7 <- DT[period == 'di', {
	m <- glmer(
		log(longevity_years) ~
			ego_period_rank +
			scale(twi_degree) +
			scale(aggr_outdegree) + scale(aggr_indegree) +
			scale(affil_outdegree) + scale(affil_indegree) +
			offset(log(clan_size)) +
			offset(log(nSession)) +
			(1 | mom),
		data = .SD,
		family = 'gaussian'
	)
	tidy(m)
}, by = iteration]


DT.ldi.7.p <-
	merge(
		DT.ldi.7[iteration != 0],
		DT.ldi.7[iteration == 0, .(effect, group, term, estimate)],
		by = c('effect', 'group', 'term'),
		suffixes = c('', '.obs')
	)

DT.ldi.7.pboth <- DT.ldi.7.p[, .(estimate = unique(estimate.obs), min = min(estimate), max = max(estimate),
																			 sumless = sum(estimate < estimate.obs), summore = sum(estimate > estimate.obs),
																			 pless = sum(estimate < estimate.obs)/1000, pmore = sum(estimate > estimate.obs)/1000),
																	 term]





#### best model for longevity Adult ####
vif(l.ad.7)
DT.lad.7 <- DT[period == 'adult', {
	m <- glmer(
		log(longevity_years) ~
			ego_period_rank +
			scale(twi_degree) +
			scale(aggr_outdegree) + scale(aggr_indegree) +
			scale(affil_outdegree) + scale(affil_indegree) +
			offset(log(clan_size)) +
			offset(log(nSession)) +
			(1 | mom),
		data = .SD,
		family = 'gaussian'
	)
	tidy(m)
}, by = iteration]


DT.lad.7.p <-
	merge(
		DT.lad.7[iteration != 0],
		DT.lad.7[iteration == 0, .(effect, group, term, estimate)],
		by = c('effect', 'group', 'term'),
		suffixes = c('', '.obs')
	)


DT.lad.7.pboth <- DT.lad.7.p[, .(estimate = unique(estimate.obs), min = min(estimate), max = max(estimate),
																			 sumless = sum(estimate < estimate.obs), summore = sum(estimate > estimate.obs),
																			 pless = sum(estimate < estimate.obs)/1000, pmore = sum(estimate > estimate.obs)/1000),
																	 term]




#### best models for ARS CD ####
vif(a.cd.7)
DT.acd.7 <- DT[period == 'cd', {
	m <- glmer(
		log(annual_rs) ~
			ego_period_rank +
			scale(twi_degree) +
			scale(aggr_outdegree) + scale(aggr_indegree) +
			scale(affil_outdegree) + scale(affil_indegree) +
			offset(log(clan_size)) +
			offset(log(nSession)) +
			(1 | mom),
		data = .SD,
		family = 'gaussian'
	)
	tidy(m)
}, by = iteration]


DT.acd.7.p <-
	merge(
		DT.acd.7[iteration != 0],
		DT.acd.7[iteration == 0, .(effect, group, term, estimate)],
		by = c('effect', 'group', 'term'),
		suffixes = c('', '.obs')
	)


DT.acd.7.pboth <- DT.acd.7.p[, .(estimate = unique(estimate.obs), min = min(estimate), max = max(estimate),
																			 sumless = sum(estimate < estimate.obs), summore = sum(estimate > estimate.obs),
																			 pless = sum(estimate < estimate.obs)/1000, pmore = sum(estimate > estimate.obs)/1000),
																	 term]




vif(a.cd.2)
DT.acd.2 <- DT[period == 'cd', {
	m <- glmer(
		log(annual_rs) ~
			ego_period_rank +
			scale(alone) +
			scale(twi_degree) +
			scale(aggr_outdegree) + scale(aggr_indegree) +
			scale(affil_outdegree) + scale(affil_indegree) +
			offset(log(clan_size)) +
			offset(log(nSession)) +
			(1 | mom),
		data = .SD,
		family = 'gaussian'
	)
	tidy(m)
}, by = iteration]


DT.acd.2.p <-
	merge(
		DT.acd.2[iteration != 0],
		DT.acd.2[iteration == 0, .(effect, group, term, estimate)],
		by = c('effect', 'group', 'term'),
		suffixes = c('', '.obs')
	)


DT.acd.2.pboth <- DT.acd.2.p[, .(estimate = unique(estimate.obs), min = min(estimate), max = max(estimate),
																			 sumless = sum(estimate < estimate.obs), summore = sum(estimate > estimate.obs),
																			 pless = sum(estimate < estimate.obs)/1000, pmore = sum(estimate > estimate.obs)/1000),
																	 term]







#### best models for ARS DI ####
vif(a.di.11)
DT.adi.11 <- DT[period == 'di', {
	m <- glmer(
		log(annual_rs) ~
			ego_period_rank +
			scale(twi_strength) +
			scale(aggr_outstrength) +
			scale(aggr_instrength) +
			scale(affil_outstrength) +
			scale(affil_instrength) +
			offset(log(clan_size)) +
			offset(log(nSession)) +
			(1 | mom),
		data = .SD,
		family = 'gaussian'
	)
	tidy(m)
}, by = iteration]



DT.adi.11.p <-
	merge(
		DT.adi.11[iteration != 0],
		DT.adi.11[iteration == 0, .(effect, group, term, estimate)],
		by = c('effect', 'group', 'term'),
		suffixes = c('', '.obs')
	)


DT.adi.11.pboth <- DT.adi.11.p[, .(estimate = unique(estimate.obs), min = min(estimate), max = max(estimate),
																						 sumless = sum(estimate < estimate.obs), summore = sum(estimate > estimate.obs),
																						 pless = sum(estimate < estimate.obs)/1000, pmore = sum(estimate > estimate.obs)/1000),
																				 term]




vif(a.di.7)
DT.adi.7 <- DT[period == 'di', {
	m <- glmer(
		log(annual_rs) ~
			ego_period_rank +
			scale(twi_degree) +
			scale(aggr_outdegree) + scale(aggr_indegree) +
			scale(affil_outdegree) + scale(affil_indegree) +
			offset(log(clan_size)) +
			offset(log(nSession)) +
			(1 | mom),
		data = .SD,
		family = 'gaussian'
	)
	tidy(m)
}, by = iteration]


DT.adi.7.p <-
	merge(
		DT.adi.7[iteration != 0],
		DT.adi.7[iteration == 0, .(effect, group, term, estimate)],
		by = c('effect', 'group', 'term'),
		suffixes = c('', '.obs')
	)


DT.adi.7.pboth <- DT.adi.7.p[, .(estimate = unique(estimate.obs), min = min(estimate), max = max(estimate),
																			 sumless = sum(estimate < estimate.obs), summore = sum(estimate > estimate.obs),
																			 pless = sum(estimate < estimate.obs)/1000, pmore = sum(estimate > estimate.obs)/1000),
																	 term]




#### best models for ARS Adult ####
vif(a.ad.7)
DT.aad.7 <- DT[period == 'adult', {
	m <- glmer(
		log(annual_rs) ~
			ego_period_rank +
			scale(twi_degree) +
			scale(aggr_outdegree) + scale(aggr_indegree) +
			scale(affil_outdegree) + scale(affil_indegree) +
			offset(log(clan_size)) +
			offset(log(nSession)) +
			(1 | mom),
		data = .SD,
		family = 'gaussian'
	)
	tidy(m)
}, by = iteration]


DT.aad.7.p <-
	merge(
		DT.aad.7[iteration != 0],
		DT.aad.7[iteration == 0, .(effect, group, term, estimate)],
		by = c('effect', 'group', 'term'),
		suffixes = c('', '.obs')
	)


DT.aad.7.pboth <- DT.aad.7.p[, .(estimate = unique(estimate.obs), min = min(estimate), max = max(estimate),
																			 sumless = sum(estimate < estimate.obs), summore = sum(estimate > estimate.obs),
																			 pless = sum(estimate < estimate.obs)/1000, pmore = sum(estimate > estimate.obs)/1000),
																	 term]


