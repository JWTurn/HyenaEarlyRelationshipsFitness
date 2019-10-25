### Models ====
# Alec Robitaille
# Started: March 28 2019


### Packages ----
# remotes::install_github('bbolker/broom.mixed')
libs <- c('lme4', 'data.table', 'broom.mixed', 'ggplot2')
lapply(libs, require, character.only = TRUE)


### Input data ----
raw <- 'data/raw-data/'
derived <- 'data/derived-data/'

egos <- fread(paste0(raw, 'egos_filtered.csv'))

# mets_a <- readRDS(paste0(derived, 'observed-random-metrics.Rds'))
# mets_b <- readRDS(paste0(derived, 'observed-random-metrics_b.Rds'))
# mets_c <- readRDS(paste0(derived, 'observed-random-metrics_c.Rds'))
# mets_d <- readRDS(paste0(derived, 'observed-random-metrics_d.Rds'))
# mets_e <- readRDS(paste0(derived, 'observed-random-metrics_e.Rds'))
# mets_e <- mets_e[iteration < 31]
#
# mets_b$iteration <- mets_b$iteration + 250
# mets_c$iteration <- mets_c$iteration + 500
# mets_d$iteration <- mets_d$iteration + 750
# mets_e$iteration <- mets_e$iteration + 1150
#
# mets <- rbind(mets_a, mets_b, mets_c, mets_d, mets_e)

#mets[, uniqueN(iteration)]

#saveRDS(mets, paste0(derived, 'observed-random-metrics_all.Rds'))

mets <- readRDS(paste0(derived, 'observed-random-metrics_all.Rds'))

assocols <- colnames(mets)[grepl('twi', colnames(mets))]
affilcols <- colnames(mets)[grepl('affil', colnames(mets))]
aggrcols <- colnames(mets)[grepl('aggr', colnames(mets))]

### Merge ----
# Fix inconsistent periods
egos[, period := tolower(period)]
egos[period == 'di', period := 'postgrad']

# Merge
cols <- c('ego', 'period')
setkeyv(egos, cols)
setkeyv(mets, cols)

DT <- merge(mets, egos, all.x = TRUE)
DT[, 6:24][is.na(DT[, 6:24])] <- 0
DT[,'alone'] <- DT$nAlone/DT$nSession

#saveRDS(DT, paste0(derived, 'DT_obs_rands.Rds'))

#### data for publishing ####
# DT.pub <- DT[,.(ego, period, ego_period_rank, clan_size, mom, nSession,
# 								alone, twi_degree, twi_strength, twi_betweenness,
# 								aggr_outdegree, aggr_indegree, aggr_outstrength, aggr_instrength, aggr_betweenness,
# 								affil_outdegree, affil_indegree, affil_outstrength, affil_instrength, affil_betweenness,
# 								annual_rs, longevity_years, iteration, observed)]
# DT.pub[period == 'postgrad', period := 'di']
#
# write.csv(DT.pub, file = paste0(derived, 'SNfitnessData_2019-04-29.csv'))

DT.obs <- DT[iteration == 0]

### correlations ####

DT.cd <- DT.obs[period=='cd', .(ego_period_rank,
								alone, twi_degree, twi_strength, twi_betweenness,
								aggr_outdegree, aggr_indegree, aggr_outstrength, aggr_instrength, aggr_betweenness,
								affil_outdegree, affil_indegree, affil_outstrength, affil_instrength, affil_betweenness)]
View(cor(DT.cd))

DT.di <- DT.obs[period=='di', .(ego_period_rank,
																alone, twi_degree, twi_strength, twi_betweenness,
																aggr_outdegree, aggr_indegree, aggr_outstrength, aggr_instrength, aggr_betweenness,
																affil_outdegree, affil_indegree, affil_outstrength, affil_instrength, affil_betweenness)]
View(cor(DT.di))

DT.ad <- DT.obs[period=='adult', .(ego_period_rank,
																alone, twi_degree, twi_strength, twi_betweenness,
																aggr_outdegree, aggr_indegree, aggr_outstrength, aggr_instrength, aggr_betweenness,
																affil_outdegree, affil_indegree, affil_outstrength, affil_instrength, affil_betweenness)]
View(cor(DT.ad))


### Models ----
#### best models for longevity CD ####
# all and directed only
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


DT.lcd.7.p <-
	merge(
		DT.lcd.7[iteration != 0],
		DT.lcd.7[iteration == 0, .(effect, group, term, estimate)],
		by = c('effect', 'group', 'term'),
		suffixes = c('', '.obs')
	)

# DT.lcd.7.pless <- DT.lcd.7.p[, sum(estimate < estimate.obs), term]
# DT.lcd.7.pless[,'pless'] <- DT.lcd.7.pless$V1/1000
# DT.lcd.7.pmore <- DT.lcd.7.p[, sum(estimate > estimate.obs), term]
# DT.lcd.7.pmore[,'pmore'] <- DT.lcd.7.pmore$V1/1000


DT.lcd.7.pboth <- DT.lcd.7.p[, .(estimate = unique(estimate.obs), min = min(estimate), max = max(estimate),
																	 sumless = sum(estimate < estimate.obs), summore = sum(estimate > estimate.obs),
																	 pless = sum(estimate < estimate.obs)/1000, pmore = sum(estimate > estimate.obs)/1000),
															 term]



DT.lcd.7.obs <- glmer(log(longevity_years) ~
												ego_period_rank +
												scale(twi_degree) +
												scale(aggr_outdegree) + scale(aggr_indegree) +
												scale(affil_outdegree) + scale(affil_indegree) +
												offset(log(clan_size)) +
												offset(log(nSession)) +
												(1 | mom),
											data = subset(DT, period == 'cd'),
											family = 'gaussian')
broom.mixed::tidy(DT.lcd.7.obs, effects = 'ran_vals')
DT.lcd.7.ci <- confint(DT.lcd.7.obs, method = 'profile')
DT.lcd.7.ci.tab<-as.data.frame(DT.lcd.7.ci)




DT.lcd.7.us <- DT[period == 'cd', {
	m <- glmer(
		log(longevity_years) ~
			ego_period_rank +
			twi_degree +
			aggr_outdegree + aggr_indegree +
			affil_outdegree + affil_indegree +
			offset(log(clan_size)) +
			offset(log(nSession)) +
			(1 | mom),
		data = .SD,
		family = 'gaussian'
	)
	tidy(m)
}, by = iteration]


DT.lcd.7.us.p <-
	merge(
		DT.lcd.7.us[iteration != 0],
		DT.lcd.7.us[iteration == 0, .(effect, group, term, estimate)],
		by = c('effect', 'group', 'term'),
		suffixes = c('', '.obs')
	)

# DT.lcd.7.pless <- DT.lcd.7.p[, sum(estimate < estimate.obs), term]
# DT.lcd.7.pless[,'pless'] <- DT.lcd.7.pless$V1/1000
# DT.lcd.7.pmore <- DT.lcd.7.p[, sum(estimate > estimate.obs), term]
# DT.lcd.7.pmore[,'pmore'] <- DT.lcd.7.pmore$V1/1000


DT.lcd.7.us.pboth <- DT.lcd.7.us.p[, .(estimate = unique(estimate.obs), min = min(estimate), max = max(estimate),
																 sumless = sum(estimate < estimate.obs), summore = sum(estimate > estimate.obs),
																 pless = sum(estimate < estimate.obs)/1000, pmore = sum(estimate > estimate.obs)/1000),
														 term]





# all
DT.lcd.19 <- DT[period == 'cd', {
	m <- glmer(
		log(longevity_years) ~
			ego_period_rank +
			scale(twi_strength) +
			scale(aggr_strength) +
			scale(affil_strength) +
			offset(log(clan_size)) +
			offset(log(nSession)) +
			(1 | mom),
		data = .SD,
		family = 'gaussian'
	)
	tidy(m)
}, by = iteration]


DT.lcd.19.p <-
	merge(
		DT.lcd.19[iteration != 0],
		DT.lcd.19[iteration == 0, .(effect, group, term, estimate)],
		by = c('effect', 'group', 'term'),
		suffixes = c('', '.obs')
	)

# DT.lcd.19.pless <- DT.lcd.19.p[, sum(estimate < estimate.obs), term]
# DT.lcd.19.pless[,'pless'] <- DT.lcd.19.pless$V1/1000
# DT.lcd.19.pmore <- DT.lcd.19.p[, sum(estimate > estimate.obs), term]
# DT.lcd.19.pmore[,'pmore'] <- DT.lcd.19.pmore$V1/1000

DT.lcd.19.pboth <- DT.lcd.19.p[, .(estimate = unique(estimate.obs), min = min(estimate), max = max(estimate),
																	 sumless = sum(estimate < estimate.obs), summore = sum(estimate > estimate.obs),
																	 pless = sum(estimate < estimate.obs)/1000, pmore = sum(estimate > estimate.obs)/1000),
															 term]


DT.lcd.19.obs <- glmer(log(longevity_years) ~
											 	ego_period_rank +
											 	scale(twi_strength) +
											 	scale(aggr_strength) +
											 	scale(affil_strength) +
											 	offset(log(clan_size)) +
											 	offset(log(nSession)) +
											 	(1 | mom),
											data = subset(DT, period == 'cd'),
											family = 'gaussian')
DT.lcd.19.ci <- confint(DT.lcd.19.obs, method = 'profile')
DT.lcd.19.ci.tab <- as.data.frame(DT.lcd.19.ci)



# all and directed only

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

# DT.lcd.2.pless <- DT.lcd.2.p[, sum(estimate < estimate.obs), term]
# DT.lcd.2.pless[,'pless'] <- DT.lcd.2.pless$V1/1000
# DT.lcd.2.pmore <- DT.lcd.2.p[, sum(estimate > estimate.obs), term]
# DT.lcd.2.pmore[,'pmore'] <- DT.lcd.2.pmore$V1/1000


DT.lcd.2.pboth <- DT.lcd.2.p[, .(estimate = unique(estimate.obs), min = min(estimate), max = max(estimate),
																	 sumless = sum(estimate < estimate.obs), summore = sum(estimate > estimate.obs),
																	 pless = sum(estimate < estimate.obs)/1000, pmore = sum(estimate > estimate.obs)/1000),
															 term]


DT.lcd.2.obs <- glmer(log(longevity_years) ~
												ego_period_rank +
												scale(alone) +
												scale(twi_degree) +
												scale(aggr_outdegree) + scale(aggr_indegree) +
												scale(affil_outdegree) + scale(affil_indegree) +
												offset(log(clan_size)) +
												offset(log(nSession)) +
												(1 | mom),
											data = subset(DT, period == 'cd'),
											family = 'gaussian')
DT.lcd.2.ci <- confint(DT.lcd.2.obs, method = 'profile')
DT.lcd.2.ci.tab<- as.data.frame(DT.lcd.2.ci)


DT.lcd.2.us <- DT[period == 'cd', {
	m <- glmer(
		log(longevity_years) ~
			ego_period_rank +
			alone +
			twi_degree +
			aggr_outdegree + aggr_indegree +
			affil_outdegree + affil_indegree +
			offset(log(clan_size)) +
			offset(log(nSession)) +
			(1 | mom),
		data = .SD,
		family = 'gaussian'
	)
	tidy(m)
}, by = iteration]


DT.lcd.2.us.p <-
	merge(
		DT.lcd.2.us[iteration != 0],
		DT.lcd.2.us[iteration == 0, .(effect, group, term, estimate)],
		by = c('effect', 'group', 'term'),
		suffixes = c('', '.obs')
	)


DT.lcd.2.us.pboth <- DT.lcd.2.us.p[, .(estimate = unique(estimate.obs), min = min(estimate), max = max(estimate),
																 sumless = sum(estimate < estimate.obs), summore = sum(estimate > estimate.obs),
																 pless = sum(estimate < estimate.obs)/1000, pmore = sum(estimate > estimate.obs)/1000),
														 term]





#### best model for longevity DI ####
# all
DT.lpg.18 <- DT[period == 'postgrad', {
	m <- glmer(
		log(longevity_years) ~
			ego_period_rank +
			scale(twi_degree) +
			scale(aggr_degree) +
			scale(affil_degree) +
			offset(log(clan_size)) +
			offset(log(nSession)) +
			(1 | mom),
		data = .SD,
		family = 'gaussian'
	)
	tidy(m)
}, by = iteration]




DT.lpg.18.p <-
	merge(
		DT.lpg.18[iteration != 0],
		DT.lpg.18[iteration == 0, .(effect, group, term, estimate)],
		by = c('effect', 'group', 'term'),
		suffixes = c('', '.obs')
	)

# DT.lpg.18.pless <- DT.lpg.18.p[, sum(estimate < estimate.obs), term]
# DT.lpg.18.pless[,'pless'] <- DT.lpg.18.pless$V1/1000
# DT.lpg.18.pmore <- DT.lpg.18.p[, sum(estimate > estimate.obs), term]
# DT.lpg.18.pmore[,'pmore'] <- DT.lpg.18.pmore$V1/1000


DT.lpg.18.pboth <- DT.lpg.18.p[, .(estimate = unique(estimate.obs), min = min(estimate), max = max(estimate),
																	 sumless = sum(estimate < estimate.obs), summore = sum(estimate > estimate.obs),
																	 pless = sum(estimate < estimate.obs)/1000, pmore = sum(estimate > estimate.obs)/1000),
															 term]


DT.lpg.18.obs <- glmer(log(longevity_years) ~
										 	ego_period_rank +
											 	scale(twi_degree) +
											 	scale(aggr_degree) +
											 	scale(affil_degree) +
											 	offset(log(clan_size)) +
											 	offset(log(nSession)) +
										 	(1 | mom),
										 data = subset(DT, period == 'postgrad'),
										 family = 'gaussian')
DT.lpg.18.ci <- confint(DT.lpg.18.obs, method = 'profile')
#DT.lpg.18.boot <- confint(DT.lpg.18.obs, method = 'boot')

DT.lpg.18.ci.tab <- as.data.frame(DT.lpg.18.ci)


# all and directed only
DT.lpg.7 <- DT[period == 'postgrad', {
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


DT.lpg.7.p <-
	merge(
		DT.lpg.7[iteration != 0],
		DT.lpg.7[iteration == 0, .(effect, group, term, estimate)],
		by = c('effect', 'group', 'term'),
		suffixes = c('', '.obs')
	)

# DT.lpg.7.pless <- DT.lpg.7.p[, sum(estimate < estimate.obs), term]
# DT.lpg.7.pless[,'pless'] <- DT.lpg.7.pless$V1/1000
# DT.lpg.7.pmore <- DT.lpg.7.p[, sum(estimate > estimate.obs), term]
# DT.lpg.7.pmore[,'pmore'] <- DT.lpg.7.pmore$V1/1000


DT.lpg.7.pboth <- DT.lpg.7.p[, .(estimate = unique(estimate.obs), min = min(estimate), max = max(estimate),
																 sumless = sum(estimate < estimate.obs), summore = sum(estimate > estimate.obs),
																 pless = sum(estimate < estimate.obs)/1000, pmore = sum(estimate > estimate.obs)/1000),
														 term]


DT.lpg.7.obs <- glmer(log(longevity_years) ~
												ego_period_rank +
												scale(twi_degree) +
												scale(aggr_outdegree) + scale(aggr_indegree) +
												scale(affil_outdegree) + scale(affil_indegree) +
												offset(log(clan_size)) +
												offset(log(nSession)) +
												(1 | mom),
											data = subset(DT, period == 'postgrad'),
											family = 'gaussian')
DT.lpg.7.ci <- confint(DT.lpg.7.obs, method = 'profile')
DT.lpg.7.ci.tab<-as.data.frame(DT.lpg.7.ci)




#### best model for longevity Adult ####
# all
DT.lad.18 <- DT[period == 'adult', {
	m <- glmer(
		log(longevity_years) ~
			ego_period_rank +
			scale(twi_degree) +
			scale(aggr_degree) +
			scale(affil_degree) +
			offset(log(clan_size)) +
			offset(log(nSession)) +
			(1 | mom),
		data = .SD,
		family = 'gaussian'
	)
	tidy(m)
}, by = iteration]


DT.lad.18.p <-
	merge(
		DT.lad.18[iteration != 0],
		DT.lad.18[iteration == 0, .(effect, group, term, estimate)],
		by = c('effect', 'group', 'term'),
		suffixes = c('', '.obs')
	)

# DT.lad.18.pless <- DT.lad.18.p[, sum(estimate < estimate.obs), term]
# DT.lad.18.pless[,'pless'] <- DT.lad.18.pless$V1/1000
# DT.lad.18.pmore <- DT.lad.18.p[, sum(estimate > estimate.obs), term]
# DT.lad.18.pmore[,'pmore'] <- DT.lad.18.pmore$V1/1000


DT.lad.18.pboth <- DT.lad.18.p[, .(estimate = unique(estimate.obs), min = min(estimate), max = max(estimate),
																	 sumless = sum(estimate < estimate.obs), summore = sum(estimate > estimate.obs),
																	 pless = sum(estimate < estimate.obs)/1000, pmore = sum(estimate > estimate.obs)/1000),
															 term]


DT.lad.18.obs <- glmer(log(longevity_years) ~
											 	ego_period_rank +
											 	scale(twi_degree) +
											 	scale(aggr_degree) +
											 	scale(affil_degree) +
											 	offset(log(clan_size)) +
											 	offset(log(nSession)) +
											 	(1 | mom),
											 data = subset(DT, period == 'adult'),
											 family = 'gaussian')
DT.lad.18.ci <- confint(DT.lad.18.obs, method = 'profile')
#DT.lad.18.boot <- confint(DT.lad.18.obs, method = 'boot')

DT.lad.18.ci.tab <- as.data.frame(DT.lad.18.ci)


# all and directed only
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

# DT.lad.7.pless <- DT.lad.7.p[, sum(estimate < estimate.obs), term]
# DT.lad.7.pless[,'pless'] <- DT.lad.7.pless$V1/1000
# DT.lad.7.pmore <- DT.lad.7.p[, sum(estimate > estimate.obs), term]
# DT.lad.7.pmore[,'pmore'] <- DT.lad.7.pmore$V1/1000


DT.lad.7.pboth <- DT.lad.7.p[, .(estimate = unique(estimate.obs), min = min(estimate), max = max(estimate),
																 sumless = sum(estimate < estimate.obs), summore = sum(estimate > estimate.obs),
																 pless = sum(estimate < estimate.obs)/1000, pmore = sum(estimate > estimate.obs)/1000),
														 term]


DT.lad.7.obs <- glmer(log(longevity_years) ~
												ego_period_rank +
												scale(twi_degree) +
												scale(aggr_outdegree) + scale(aggr_indegree) +
												scale(affil_outdegree) + scale(affil_indegree) +
												offset(log(clan_size)) +
												offset(log(nSession)) +
												(1 | mom),
											data = subset(DT, period == 'adult'),
											family = 'gaussian')
DT.lad.7.ci <- confint(DT.lad.7.obs, method = 'profile')
DT.lad.7.ci.tab<-as.data.frame(DT.lad.7.ci)


#### best models for ARS CD ####
# all
DT.acd.18 <- DT[period == 'cd', {
	m <- glmer(
		log(annual_rs) ~
			ego_period_rank +
			scale(twi_degree) +
			scale(aggr_degree) +
			scale(affil_degree) +
			offset(log(clan_size)) +
			offset(log(nSession)) +
			(1 | mom),
		data =  na.omit(.SD, cols = 'annual_rs'),
		family = 'gaussian'
	)
	tidy(m)
}, by = iteration]



DT.acd.18.p <-
	merge(
		DT.acd.18[iteration != 0],
		DT.acd.18[iteration == 0, .(effect, group, term, estimate)],
		by = c('effect', 'group', 'term'),
		suffixes = c('', '.obs')
	)

# DT.acd.18.pless <- DT.acd.18.p[, sum(estimate < estimate.obs), term]
# DT.acd.18.pless[,'pless'] <- DT.acd.18.pless$V1/1000
# DT.acd.18.pmore <- DT.acd.18.p[, sum(estimate > estimate.obs), term]
# DT.acd.18.pmore[,'pmore'] <- DT.acd.18.pmore$V1/1000


DT.acd.18.pboth <- DT.acd.18.p[, .(estimate = unique(estimate.obs), min = min(estimate), max = max(estimate),
																	 sumless = sum(estimate < estimate.obs), summore = sum(estimate > estimate.obs),
																	 pless = sum(estimate < estimate.obs)/1000, pmore = sum(estimate > estimate.obs)/1000),
															 term]


DT.acd.18.obs <- glmer(log(annual_rs) ~
											 	ego_period_rank +
											 	scale(twi_degree) +
											 	scale(aggr_degree) +
											 	scale(affil_degree) +
											 	offset(log(clan_size)) +
											 	offset(log(nSession)) +
											 	(1 | mom),
											 data = na.omit(DT.obs[period == 'cd'], cols = 'annual_rs'),
											 family = 'gaussian')
DT.acd.18.ci <- confint(DT.acd.18.obs, method = 'profile')
#DT.acd.18.boot <- confint(DT.acd.18.obs, method = 'boot')

DT.acd.18.ci.tab <- as.data.frame(DT.acd.18.ci)


# all
# dAIC = 0.5
DT.acd.11 <- DT[period == 'cd', {
	m <- glmer(
		log(annual_rs) ~
			ego_period_rank +
			scale(alone) +
			scale(twi_degree) +
			scale(aggr_degree) +
			scale(affil_degree) +
			offset(log(clan_size)) +
			offset(log(nSession)) +
			(1 | mom),
		data = .SD,
		family = 'gaussian'
	)
	tidy(m)
}, by = iteration]


DT.acd.11.p <-
	merge(
		DT.acd.11[iteration != 0],
		DT.acd.11[iteration == 0, .(effect, group, term, estimate)],
		by = c('effect', 'group', 'term'),
		suffixes = c('', '.obs')
	)

# DT.acd.11.pless <- DT.acd.11.p[, sum(estimate < estimate.obs), term]
# DT.acd.11.pless[,'pless'] <- DT.acd.11.pless$V1/1000
# DT.acd.11.pmore <- DT.acd.11.p[, sum(estimate > estimate.obs), term]
# DT.acd.11.pmore[,'pmore'] <- DT.acd.11.pmore$V1/1000


DT.acd.11.pboth <- DT.acd.11.p[, .(estimate = unique(estimate.obs), min = min(estimate), max = max(estimate),
																	 sumless = sum(estimate < estimate.obs), summore = sum(estimate > estimate.obs),
																	 pless = sum(estimate < estimate.obs)/1000, pmore = sum(estimate > estimate.obs)/1000),
															 term]


DT.acd.11.obs <- glmer(log(annual_rs) ~
											 	ego_period_rank +
											 	scale(alone) +
											 	scale(twi_degree) +
											 	scale(aggr_degree) +
											 	scale(affil_degree) +
											 	offset(log(clan_size)) +
											 	offset(log(nSession)) +
											 	(1 | mom),
											 data = subset(DT, period == 'cd'),
											 family = 'gaussian')
DT.acd.11.ci <- confint(DT.acd.11.obs, method = 'profile')

DT.acd.11.ci.tab <- as.data.frame(DT.acd.11.ci)



# directed only
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

# DT.acd.7.pless <- DT.acd.7.p[, sum(estimate < estimate.obs), term]
# DT.acd.7.pless[,'pless'] <- DT.acd.7.pless$V1/1000
# DT.acd.7.pmore <- DT.acd.7.p[, sum(estimate > estimate.obs), term]
# DT.acd.7.pmore[,'pmore'] <- DT.acd.7.pmore$V1/1000


DT.acd.7.pboth <- DT.acd.7.p[, .(estimate = unique(estimate.obs), min = min(estimate), max = max(estimate),
																 sumless = sum(estimate < estimate.obs), summore = sum(estimate > estimate.obs),
																 pless = sum(estimate < estimate.obs)/1000, pmore = sum(estimate > estimate.obs)/1000),
														 term]


DT.acd.7.obs <- glmer(log(annual_rs) ~
												ego_period_rank +
												scale(twi_degree) +
												scale(aggr_outdegree) + scale(aggr_indegree) +
												scale(affil_outdegree) + scale(affil_indegree) +
												offset(log(clan_size)) +
												offset(log(nSession)) +
												(1 | mom),
											data = subset(DT, period == 'cd'),
											family = 'gaussian')
DT.acd.7.ci <- confint(DT.acd.7.obs, method = 'profile')
DT.acd.7.ci.tab<-as.data.frame(DT.acd.7.ci)


# directed only
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

# DT.acd.2.pless <- DT.acd.2.p[, sum(estimate < estimate.obs), term]
# DT.acd.2.pless[,'pless'] <- DT.acd.2.pless$V1/1000
# DT.acd.2.pmore <- DT.acd.2.p[, sum(estimate > estimate.obs), term]
# DT.acd.2.pmore[,'pmore'] <- DT.acd.2.pmore$V1/1000


DT.acd.2.pboth <- DT.acd.2.p[, .(estimate = unique(estimate.obs), min = min(estimate), max = max(estimate),
																 sumless = sum(estimate < estimate.obs), summore = sum(estimate > estimate.obs),
																 pless = sum(estimate < estimate.obs)/1000, pmore = sum(estimate > estimate.obs)/1000),
														 term]


DT.acd.2.obs <- glmer(log(annual_rs) ~
												ego_period_rank +
												scale(alone) +
												scale(twi_degree) +
												scale(aggr_outdegree) + scale(aggr_indegree) +
												scale(affil_outdegree) + scale(affil_indegree) +
												offset(log(clan_size)) +
												offset(log(nSession)) +
												(1 | mom),
											data = subset(DT, period == 'cd'),
											family = 'gaussian')
DT.acd.2.ci <- confint(DT.acd.2.obs, method = 'profile')
DT.acd.2.ci.tab<-as.data.frame(DT.acd.2.ci)





#### best models for ARS DI ####
#all

DT.apg.18 <- DT[period == 'postgrad', {
	m <- glmer(
		log(annual_rs) ~
			ego_period_rank +
			scale(twi_degree) +
			scale(aggr_degree) +
			scale(affil_degree) +
			offset(log(clan_size)) +
			offset(log(nSession)) +
			(1 | mom),
		data = .SD,
		family = 'gaussian'
	)
	tidy(m)
}, by = iteration]



DT.apg.18.p <-
	merge(
		DT.apg.18[iteration != 0],
		DT.apg.18[iteration == 0, .(effect, group, term, estimate)],
		by = c('effect', 'group', 'term'),
		suffixes = c('', '.obs')
	)

# DT.apg.18.pless <- DT.apg.18.p[, sum(estimate < estimate.obs), term]
# DT.apg.18.pless[,'pless'] <- DT.apg.18.pless$V1/1000
# DT.apg.18.pmore <- DT.apg.18.p[, sum(estimate > estimate.obs), term]
# DT.apg.18.pmore[,'pmore'] <- DT.apg.18.pmore$V1/1000


DT.apg.18.pboth <- DT.apg.18.p[, .(estimate = unique(estimate.obs), min = min(estimate), max = max(estimate),
																	 sumless = sum(estimate < estimate.obs), summore = sum(estimate > estimate.obs),
																	 pless = sum(estimate < estimate.obs)/1000, pmore = sum(estimate > estimate.obs)/1000),
															 term]


DT.apg.18.obs <- glmer(log(annual_rs) ~
											 	ego_period_rank +
											 	scale(twi_degree) +
											 	scale(aggr_degree) +
											 	scale(affil_degree) +
											 	offset(log(clan_size)) +
											 	offset(log(nSession)) +
											 	(1 | mom),
											 data = na.omit(DT.obs[period == 'cd'], cols = 'annual_rs'),
											 family = 'gaussian')
DT.apg.18.ci <- confint(DT.apg.18.obs, method = 'profile')

DT.apg.18.ci.tab <- as.data.frame(DT.apg.18.ci)


#all

DT.apg.19 <- DT[period == 'postgrad', {
	m <- glmer(
		log(annual_rs) ~
			ego_period_rank +
			scale(twi_strength) +
			scale(aggr_strength) +
			scale(affil_strength) +
			offset(log(clan_size)) +
			offset(log(nSession)) +
			(1 | mom),
		data = .SD,
		family = 'gaussian'
	)
	tidy(m)
}, by = iteration]



DT.apg.19.p <-
	merge(
		DT.apg.19[iteration != 0],
		DT.apg.19[iteration == 0, .(effect, group, term, estimate)],
		by = c('effect', 'group', 'term'),
		suffixes = c('', '.obs')
	)

# DT.apg.19.pless <- DT.apg.19.p[, sum(estimate < estimate.obs), term]
# DT.apg.19.pless[,'pless'] <- DT.apg.19.pless$V1/1000
# DT.apg.19.pmore <- DT.apg.19.p[, sum(estimate > estimate.obs), term]
# DT.apg.19.pmore[,'pmore'] <- DT.apg.19.pmore$V1/1000

DT.apg.19.pboth <- DT.apg.19.p[, .(estimate = unique(estimate.obs), min = min(estimate), max = max(estimate),
																	 sumless = sum(estimate < estimate.obs), summore = sum(estimate > estimate.obs),
																	 pless = sum(estimate < estimate.obs)/1000, pmore = sum(estimate > estimate.obs)/1000),
															 term]



DT.apg.19.obs <- glmer(log(annual_rs) ~
											 	ego_period_rank +
											 	scale(twi_strength) +
											 	scale(aggr_strength) +
											 	scale(affil_strength) +
											 	offset(log(clan_size)) +
											 	offset(log(nSession)) +
											 	(1 | mom),
											 data = na.omit(DT.obs[period == 'cd'], cols = 'annual_rs'),
											 family = 'gaussian')
DT.apg.19.ci <- confint(DT.apg.19.obs, method = 'profile')

DT.apg.19.ci.tab <- as.data.frame(DT.apg.19.ci)



# directed only

DT.apg.19.a <- DT[period == 'postgrad', {
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



DT.apg.19.a.p <-
	merge(
		DT.apg.19.a[iteration != 0],
		DT.apg.19.a[iteration == 0, .(effect, group, term, estimate)],
		by = c('effect', 'group', 'term'),
		suffixes = c('', '.obs')
	)

# DT.apg.19.a.pless <- DT.apg.19.a.p[, sum(estimate < estimate.obs), term]
# DT.apg.19.a.pless[,'pless'] <- DT.apg.19.a.pless$V1/1000
# DT.apg.19.a.pmore <- DT.apg.19.a.p[, sum(estimate > estimate.obs), term]
# DT.apg.19.a.pmore[,'pmore'] <- DT.apg.19.a.pmore$V1/1000

DT.apg.19.a.pboth <- DT.apg.19.a.p[, .(estimate = unique(estimate.obs), min = min(estimate), max = max(estimate),
																	 sumless = sum(estimate < estimate.obs), summore = sum(estimate > estimate.obs),
																	 pless = sum(estimate < estimate.obs)/1000, pmore = sum(estimate > estimate.obs)/1000),
															 term]



DT.apg.19.a.obs <- glmer(log(annual_rs) ~
												 	ego_period_rank +
												 	scale(twi_strength) +
												 	scale(aggr_outstrength) +
												 	scale(aggr_instrength) +
												 	scale(affil_outstrength) +
												 	scale(affil_instrength) +
												 	offset(log(clan_size)) +
												 	offset(log(nSession)) +
											 	(1 | mom),
											 data = na.omit(DT.obs[period == 'cd'], cols = 'annual_rs'),
											 family = 'gaussian')
DT.apg.19.a.ci <- confint(DT.apg.19.a.obs, method = 'profile')

DT.apg.19.a.ci.tab <- as.data.frame(DT.apg.19.a.ci)




# directed only
DT.apg.7 <- DT[period == 'postgrad', {
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


DT.apg.7.p <-
	merge(
		DT.apg.7[iteration != 0],
		DT.apg.7[iteration == 0, .(effect, group, term, estimate)],
		by = c('effect', 'group', 'term'),
		suffixes = c('', '.obs')
	)

# DT.apg.7.pless <- DT.apg.7.p[, sum(estimate < estimate.obs), term]
# DT.apg.7.pless[,'pless'] <- DT.apg.7.pless$V1/1000
# DT.apg.7.pmore <- DT.apg.7.p[, sum(estimate > estimate.obs), term]
# DT.apg.7.pmore[,'pmore'] <- DT.apg.7.pmore$V1/1000


DT.apg.7.pboth <- DT.apg.7.p[, .(estimate = unique(estimate.obs), min = min(estimate), max = max(estimate),
																 sumless = sum(estimate < estimate.obs), summore = sum(estimate > estimate.obs),
																 pless = sum(estimate < estimate.obs)/1000, pmore = sum(estimate > estimate.obs)/1000),
														 term]


DT.apg.7.obs <- glmer(log(annual_rs) ~
												ego_period_rank +
												scale(twi_degree) +
												scale(aggr_outdegree) + scale(aggr_indegree) +
												scale(affil_outdegree) + scale(affil_indegree) +
												offset(log(clan_size)) +
												offset(log(nSession)) +
												(1 | mom),
											data = subset(DT, period == 'postgrad'),
											family = 'gaussian')
DT.apg.7.ci <- confint(DT.apg.7.obs, method = 'profile')
DT.apg.7.ci.tab<-as.data.frame(DT.apg.7.ci)





#### best models for ARS Adult ####
DT.aad.18 <- DT[period == 'adult', {
	m <- glmer(
		log(annual_rs) ~
			ego_period_rank +
			scale(twi_degree) +
			scale(aggr_degree) +
			scale(affil_degree) +
			offset(log(clan_size)) +
			offset(log(nSession)) +
			(1 | mom),
		data = .SD,
		family = 'gaussian'
	)
	tidy(m)
}, by = iteration]



DT.aad.18.p <-
	merge(
		DT.aad.18[iteration != 0],
		DT.aad.18[iteration == 0, .(effect, group, term, estimate)],
		by = c('effect', 'group', 'term'),
		suffixes = c('', '.obs')
	)

# DT.aad.18.pless <- DT.aad.18.p[, .(sum(estimate > estimate.obs), min = min(estimate), max = max(estimate)), term]
# DT.aad.18.pless[,'pless'] <- DT.aad.18.pless$V1/1000
# DT.aad.18.pmore <- DT.aad.18.p[, .(sum(estimate < estimate.obs), min = min(estimate), max = max(estimate)), term]
# DT.aad.18.pmore[,'pmore'] <- DT.aad.18.pmore$V1/1000

DT.aad.18.pboth <- DT.aad.18.p[, .(estimate = unique(estimate.obs), min = min(estimate), max = max(estimate),
																	 sumless = sum(estimate < estimate.obs), summore = sum(estimate > estimate.obs),
																	 pless = sum(estimate < estimate.obs)/1000, pmore = sum(estimate > estimate.obs)/1000),
															 term]


DT.aad.18.obs <- glmer(log(annual_rs) ~
											 	ego_period_rank +
											 	scale(twi_degree) +
											 	scale(aggr_degree) +
											 	scale(affil_degree) +
											 	offset(log(clan_size)) +
											 	offset(log(nSession)) +
											 	(1 | mom),
											 data = na.omit(DT.obs[period == 'cd'], cols = 'annual_rs'),
											 family = 'gaussian')
DT.aad.18.ci <- confint(DT.aad.18.obs, method = 'profile')

DT.aad.18.ci.tab <- as.data.frame(DT.aad.18.ci)



# directed only
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

# DT.aad.7.pless <- DT.aad.7.p[, sum(estimate < estimate.obs), term]
# DT.aad.7.pless[,'pless'] <- DT.aad.7.pless$V1/1000
# DT.aad.7.pmore <- DT.aad.7.p[, sum(estimate > estimate.obs), term]
# DT.aad.7.pmore[,'pmore'] <- DT.aad.7.pmore$V1/1000


DT.aad.7.pboth <- DT.aad.7.p[, .(estimate = unique(estimate.obs), min = min(estimate), max = max(estimate),
																 sumless = sum(estimate < estimate.obs), summore = sum(estimate > estimate.obs),
																 pless = sum(estimate < estimate.obs)/1000, pmore = sum(estimate > estimate.obs)/1000),
														 term]


DT.aad.7.obs <- glmer(log(annual_rs) ~
												ego_period_rank +
												scale(twi_degree) +
												scale(aggr_outdegree) + scale(aggr_indegree) +
												scale(affil_outdegree) + scale(affil_indegree) +
												offset(log(clan_size)) +
												offset(log(nSession)) +
												(1 | mom),
											data = subset(DT, period == 'adult'),
											family = 'gaussian')

DT.aad.7.obs <- glmer(log(annual_rs) ~
												ego_period_rank +
												scale(twi_degree) +
												scale(aggr_outdegree) + scale(aggr_indegree) +
												scale(affil_outdegree) + scale(affil_indegree) +
												offset(log(clan_size)) +
												offset(log(nSession)) +
												(1 | mom),
											data = subset(DT, period == 'adult'),
											family = 'gaussian')
DT.aad.7.ci <- confint(DT.aad.7.obs, method = 'profile')
DT.aad.7.ci.tab<-as.data.frame(DT.aad.7.ci)



#### PLOTS ####
DT.lcd.7.g <- DT.lcd.7.p[(term != '(Intercept)' & term !=  'sd__Observation' & term !=  'sd__(Intercept)')]
DT.lcd.7.g.obs <- DT.lcd.7.g[,.(term = unique(term), estimate.obs = unique(estimate.obs))]
boxplot(estimate~term, data = DT.lcd.7.g)
points(1:length(DT.lcd.7.g.obs$term), DT.lcd.7.g.obs$estimate.obs, pch = 18, col = "blue", cex = 3)

DT.lcd.2.g <- DT.lcd.2.p[(term != '(Intercept)' & term !=  'sd__Observation' & term !=  'sd__(Intercept)')]
DT.lcd.2.g.obs <- DT.lcd.2.g[,.(term = unique(term), estimate.obs = unique(estimate.obs))]
boxplot(estimate~term, data = DT.lcd.2.g)
points(1:length(DT.lcd.2.g.obs$term), DT.lcd.2.g.obs$estimate.obs, pch = 18, col = "blue", cex = 3)


DT.lpg.7.g <- DT.lpg.7.p[(term != '(Intercept)' & term !=  'sd__Observation' & term !=  'sd__(Intercept)')]
DT.lpg.7.g.obs <- DT.lpg.7.g[,.(term = unique(term), estimate.obs = unique(estimate.obs))]
boxplot(estimate~term, data = DT.lpg.7.g)
points(1:length(DT.lpg.7.g.obs$term), DT.lpg.7.g.obs$estimate.obs, pch = 18, col = "blue", cex = 3)


DT.lad.7.g <- DT.lad.7.p[(term != '(Intercept)' & term !=  'sd__Observation' & term !=  'sd__(Intercept)')]
DT.lad.7.g.obs <- DT.lad.7.g[,.(term = unique(term), estimate.obs = unique(estimate.obs))]
boxplot(estimate~term, data = DT.lad.7.g)
points(1:length(DT.lad.7.g.obs$term), DT.lad.7.g.obs$estimate.obs, pch = 18, col = "blue", cex = 3)




DT.acd.7.g <- DT.acd.7.p[(term != '(Intercept)' & term !=  'sd__Observation' & term !=  'sd__(Intercept)')]
DT.acd.7.g.obs <- DT.acd.7.g[,.(term = unique(term), estimate.obs = unique(estimate.obs))]
boxplot(estimate~term, data = DT.acd.7.g)
points(1:length(DT.acd.7.g.obs$term), DT.acd.7.g.obs$estimate.obs, pch = 18, col = "blue", cex = 3)

DT.acd.2.g <- DT.acd.2.p[(term != '(Intercept)' & term !=  'sd__Observation' & term !=  'sd__(Intercept)')]
DT.acd.2.g.obs <- DT.acd.2.g[,.(term = unique(term), estimate.obs = unique(estimate.obs))]
boxplot(estimate~term, data = DT.acd.2.g)
points(1:length(DT.acd.2.g.obs$term), DT.acd.2.g.obs$estimate.obs, pch = 18, col = "blue", cex = 3)

DT.apg.19.a.g <- DT.apg.19.a.p[(term != '(Intercept)' & term !=  'sd__Observation' & term !=  'sd__(Intercept)')]
DT.apg.19.a.g.obs <- DT.apg.19.a.g[,.(term = unique(term), estimate.obs = unique(estimate.obs))]
boxplot(estimate~term, data = DT.apg.19.a.g)
points(1:length(DT.apg.19.a.g.obs$term), DT.apg.19.a.g.obs$estimate.obs, pch = 18, col = "blue", cex = 3)

DT.apg.7.g <- DT.apg.7.p[(term != '(Intercept)' & term !=  'sd__Observation' & term !=  'sd__(Intercept)')]
DT.apg.7.g.obs <- DT.apg.7.g[,.(term = unique(term), estimate.obs = unique(estimate.obs))]
boxplot(estimate~term, data = DT.apg.7.g)
points(1:length(DT.apg.7.g.obs$term), DT.apg.7.g.obs$estimate.obs, pch = 18, col = "blue", cex = 3)

DT.aad.7.g <- DT.aad.7.p[(term != '(Intercept)' & term !=  'sd__Observation' & term !=  'sd__(Intercept)')]
DT.aad.7.g.obs <- DT.aad.7.g[,.(term = unique(term), estimate.obs = unique(estimate.obs))]
boxplot(estimate~term, data = DT.aad.7.g)
points(1:length(DT.aad.7.g.obs$term), DT.aad.7.g.obs$estimate.obs, pch = 18, col = "blue", cex = 3)





DT.acd.18.g <- DT.acd.18.p[(term != '(Intercept)' & term !=  'sd__Observation' & term !=  'sd__(Intercept)')]
DT.acd.18.g.obs <- DT.acd.18.g[,.(term = unique(term), estimate.obs = unique(estimate.obs))]
boxplot(estimate~term, data = DT.acd.18.g)
points(1:length(DT.acd.18.g.obs$term), DT.acd.18.g.obs$estimate.obs, pch = 18, col = "blue", cex = 3)


DT.acd.11.g <- DT.acd.11.p[(term != '(Intercept)' & term !=  'sd__Observation' & term !=  'sd__(Intercept)')]
DT.acd.11.g.obs <- DT.acd.11.g[,.(term = unique(term), estimate.obs = unique(estimate.obs))]
boxplot(estimate~term, data = DT.acd.11.g)
points(1:length(DT.acd.11.g.obs$term), DT.acd.11.g.obs$estimate.obs, pch = 18, col = "blue", cex = 3)


DT.apg.18.g <- DT.apg.18.p[(term != '(Intercept)' & term !=  'sd__Observation' & term !=  'sd__(Intercept)')]
DT.apg.18.g.obs <- DT.apg.18.g[,.(term = unique(term), estimate.obs = unique(estimate.obs))]
boxplot(estimate~term, data = DT.apg.18.g)
points(1:length(DT.apg.18.g.obs$term), DT.apg.18.g.obs$estimate.obs, pch = 18, col = "blue", cex = 3)



DT.obs.pg.ars<-DT.obs[period=='postgrad' & annual_rs<2.5]
plot(DT.obs.pg.ars$twi_strength, DT.obs.pg.ars$annual_rs)
abline(lm(annual_rs~twi_strength, DT.obs.pg.ars))

plot(DT.obs.pg.ars$aggr_outdegree, DT.obs.pg.ars$annual_rs)
abline(lm(annual_rs~aggr_outdegree, DT.obs.pg.ars))

DT.aad.18.g <- DT.aad.18.p[(term != '(Intercept)' & term !=  'sd__Observation' & term !=  'sd__(Intercept)')]
DT.aad.18.g.obs <- DT.aad.18.g[,.(term = unique(term), estimate.obs = unique(estimate.obs))]
boxplot(estimate~term, data = DT.aad.18.g)
points(1:length(DT.aad.18.g.obs$term), DT.aad.18.g.obs$estimate.obs, pch = 18, col = "blue", cex = 3)





DT.obs.cd.long<-DT.obs[period=='cd']
plot(DT.obs.cd.long$affil_outdegree, DT.obs.cd.long$longevity_years)
abline(lm(longevity_years~affil_outdegree, DT.obs.cd.long))
