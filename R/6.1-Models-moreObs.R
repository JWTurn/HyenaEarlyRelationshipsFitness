### Models 2 (more obs) ====
# Alec Robitaille and Julie Turner
# Started: April 22 2019


### Packages ----
# remotes::install_github('bbolker/broom.mixed')
libs <- c('lme4', 'data.table', 'broom.mixed', 'ggplot2')
lapply(libs, require, character.only = TRUE)


### Input data ----
raw <- 'data/raw-data/'
derived <- 'data/derived-data/'

egos <- fread(paste0(raw, 'egos_filtered.csv'))



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

DT.25 <- merge(mets, egos, all.x = TRUE)
DT.25[, 6:24][is.na(DT.25[, 6:24])] <- 0
DT.25[,'alone'] <- DT.25$nAlone/DT.25$nSession

#saveRDS(DT.25, paste0(derived, 'DT.25_obs_rands.Rds'))

DT.obs <- DT.25[iteration == 0]
DT.obs.ses <- DT.obs[,.(ego, period, nSession.obs = nSession)]
DT.25.obs <- DT.25[iteration == 0 & nSession >= 25]
# names<-colnames(DT.25)
# names_nSes<- names[names != 'nSession']
DT.25 <-
	merge(DT.25, DT.obs.ses, by = c('ego', 'period'))
DT.25 <- DT.25[nSession.obs>=25]


### Models ----
#### best models for longevity CD ####
# all and directed only
DT.25.lcd.7 <- DT.25[period == 'cd', {
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


DT.25.lcd.7.p <-
	merge(
		DT.25.lcd.7[iteration != 0],
		DT.25.lcd.7[iteration == 0, .(effect, group, term, estimate)],
		by = c('effect', 'group', 'term'),
		suffixes = c('', '.obs')
	)

# DT.25.lcd.7.pless <- DT.25.lcd.7.p[, sum(estimate < estimate.obs), term]
# DT.25.lcd.7.pless[,'pless'] <- DT.25.lcd.7.pless$V1/1000
# DT.25.lcd.7.pmore <- DT.25.lcd.7.p[, sum(estimate > estimate.obs), term]
# DT.25.lcd.7.pmore[,'pmore'] <- DT.25.lcd.7.pmore$V1/1000


DT.25.lcd.7.pboth <- DT.25.lcd.7.p[, .(estimate = unique(estimate.obs), min = min(estimate), max = max(estimate),
																 sumless = sum(estimate < estimate.obs), summore = sum(estimate > estimate.obs),
																 pless = sum(estimate < estimate.obs)/1000, pmore = sum(estimate > estimate.obs)/1000),
														 term]





# all and directed only

DT.25.lcd.2 <- DT.25[period == 'cd', {
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


DT.25.lcd.2.p <-
	merge(
		DT.25.lcd.2[iteration != 0],
		DT.25.lcd.2[iteration == 0, .(effect, group, term, estimate)],
		by = c('effect', 'group', 'term'),
		suffixes = c('', '.obs')
	)

# DT.25.lcd.2.pless <- DT.25.lcd.2.p[, sum(estimate < estimate.obs), term]
# DT.25.lcd.2.pless[,'pless'] <- DT.25.lcd.2.pless$V1/1000
# DT.25.lcd.2.pmore <- DT.25.lcd.2.p[, sum(estimate > estimate.obs), term]
# DT.25.lcd.2.pmore[,'pmore'] <- DT.25.lcd.2.pmore$V1/1000


DT.25.lcd.2.pboth <- DT.25.lcd.2.p[, .(estimate = unique(estimate.obs), min = min(estimate), max = max(estimate),
																 sumless = sum(estimate < estimate.obs), summore = sum(estimate > estimate.obs),
																 pless = sum(estimate < estimate.obs)/1000, pmore = sum(estimate > estimate.obs)/1000),
														 term]




#### best model for longevity DI ####
# all and directed only
DT.25.lpg.7 <- DT.25[period == 'postgrad', {
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


DT.25.lpg.7.p <-
	merge(
		DT.25.lpg.7[iteration != 0],
		DT.25.lpg.7[iteration == 0, .(effect, group, term, estimate)],
		by = c('effect', 'group', 'term'),
		suffixes = c('', '.obs')
	)

# DT.25.lpg.7.pless <- DT.25.lpg.7.p[, sum(estimate < estimate.obs), term]
# DT.25.lpg.7.pless[,'pless'] <- DT.25.lpg.7.pless$V1/1000
# DT.25.lpg.7.pmore <- DT.25.lpg.7.p[, sum(estimate > estimate.obs), term]
# DT.25.lpg.7.pmore[,'pmore'] <- DT.25.lpg.7.pmore$V1/1000


DT.25.lpg.7.pboth <- DT.25.lpg.7.p[, .(estimate = unique(estimate.obs), min = min(estimate), max = max(estimate),
																 sumless = sum(estimate < estimate.obs), summore = sum(estimate > estimate.obs),
																 pless = sum(estimate < estimate.obs)/1000, pmore = sum(estimate > estimate.obs)/1000),
														 term]





#### best model for longevity Adult ####
# all and directed only
DT.25.lad.7 <- DT.25[period == 'adult', {
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


DT.25.lad.7.p <-
	merge(
		DT.25.lad.7[iteration != 0],
		DT.25.lad.7[iteration == 0, .(effect, group, term, estimate)],
		by = c('effect', 'group', 'term'),
		suffixes = c('', '.obs')
	)

# DT.25.lad.7.pless <- DT.25.lad.7.p[, sum(estimate < estimate.obs), term]
# DT.25.lad.7.pless[,'pless'] <- DT.25.lad.7.pless$V1/1000
# DT.25.lad.7.pmore <- DT.25.lad.7.p[, sum(estimate > estimate.obs), term]
# DT.25.lad.7.pmore[,'pmore'] <- DT.25.lad.7.pmore$V1/1000


DT.25.lad.7.pboth <- DT.25.lad.7.p[, .(estimate = unique(estimate.obs), min = min(estimate), max = max(estimate),
																 sumless = sum(estimate < estimate.obs), summore = sum(estimate > estimate.obs),
																 pless = sum(estimate < estimate.obs)/1000, pmore = sum(estimate > estimate.obs)/1000),
														 term]




#### best models for ARS CD ####
# directed only
DT.25.acd.7 <- DT.25[period == 'cd', {
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


DT.25.acd.7.p <-
	merge(
		DT.25.acd.7[iteration != 0],
		DT.25.acd.7[iteration == 0, .(effect, group, term, estimate)],
		by = c('effect', 'group', 'term'),
		suffixes = c('', '.obs')
	)

# DT.25.acd.7.pless <- DT.25.acd.7.p[, sum(estimate < estimate.obs), term]
# DT.25.acd.7.pless[,'pless'] <- DT.25.acd.7.pless$V1/1000
# DT.25.acd.7.pmore <- DT.25.acd.7.p[, sum(estimate > estimate.obs), term]
# DT.25.acd.7.pmore[,'pmore'] <- DT.25.acd.7.pmore$V1/1000


DT.25.acd.7.pboth <- DT.25.acd.7.p[, .(estimate = unique(estimate.obs), min = min(estimate), max = max(estimate),
																 sumless = sum(estimate < estimate.obs), summore = sum(estimate > estimate.obs),
																 pless = sum(estimate < estimate.obs)/1000, pmore = sum(estimate > estimate.obs)/1000),
														 term]




# directed only
DT.25.acd.2 <- DT.25[period == 'cd', {
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


DT.25.acd.2.p <-
	merge(
		DT.25.acd.2[iteration != 0],
		DT.25.acd.2[iteration == 0, .(effect, group, term, estimate)],
		by = c('effect', 'group', 'term'),
		suffixes = c('', '.obs')
	)

# DT.25.acd.2.pless <- DT.25.acd.2.p[, sum(estimate < estimate.obs), term]
# DT.25.acd.2.pless[,'pless'] <- DT.25.acd.2.pless$V1/1000
# DT.25.acd.2.pmore <- DT.25.acd.2.p[, sum(estimate > estimate.obs), term]
# DT.25.acd.2.pmore[,'pmore'] <- DT.25.acd.2.pmore$V1/1000


DT.25.acd.2.pboth <- DT.25.acd.2.p[, .(estimate = unique(estimate.obs), min = min(estimate), max = max(estimate),
																 sumless = sum(estimate < estimate.obs), summore = sum(estimate > estimate.obs),
																 pless = sum(estimate < estimate.obs)/1000, pmore = sum(estimate > estimate.obs)/1000),
														 term]







#### best models for ARS DI ####
# directed only

DT.25.apg.19.a <- DT.25[period == 'postgrad', {
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



DT.25.apg.19.a.p <-
	merge(
		DT.25.apg.19.a[iteration != 0],
		DT.25.apg.19.a[iteration == 0, .(effect, group, term, estimate)],
		by = c('effect', 'group', 'term'),
		suffixes = c('', '.obs')
	)

# DT.25.apg.19.a.pless <- DT.25.apg.19.a.p[, sum(estimate < estimate.obs), term]
# DT.25.apg.19.a.pless[,'pless'] <- DT.25.apg.19.a.pless$V1/1000
# DT.25.apg.19.a.pmore <- DT.25.apg.19.a.p[, sum(estimate > estimate.obs), term]
# DT.25.apg.19.a.pmore[,'pmore'] <- DT.25.apg.19.a.pmore$V1/1000

DT.25.apg.19.a.pboth <- DT.25.apg.19.a.p[, .(estimate = unique(estimate.obs), min = min(estimate), max = max(estimate),
																			 sumless = sum(estimate < estimate.obs), summore = sum(estimate > estimate.obs),
																			 pless = sum(estimate < estimate.obs)/1000, pmore = sum(estimate > estimate.obs)/1000),
																	 term]




# directed only
DT.25.apg.7 <- DT.25[period == 'postgrad', {
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


DT.25.apg.7.p <-
	merge(
		DT.25.apg.7[iteration != 0],
		DT.25.apg.7[iteration == 0, .(effect, group, term, estimate)],
		by = c('effect', 'group', 'term'),
		suffixes = c('', '.obs')
	)

# DT.25.apg.7.pless <- DT.25.apg.7.p[, sum(estimate < estimate.obs), term]
# DT.25.apg.7.pless[,'pless'] <- DT.25.apg.7.pless$V1/1000
# DT.25.apg.7.pmore <- DT.25.apg.7.p[, sum(estimate > estimate.obs), term]
# DT.25.apg.7.pmore[,'pmore'] <- DT.25.apg.7.pmore$V1/1000


DT.25.apg.7.pboth <- DT.25.apg.7.p[, .(estimate = unique(estimate.obs), min = min(estimate), max = max(estimate),
																 sumless = sum(estimate < estimate.obs), summore = sum(estimate > estimate.obs),
																 pless = sum(estimate < estimate.obs)/1000, pmore = sum(estimate > estimate.obs)/1000),
														 term]




#### best models for ARS Adult ####
# directed only
DT.25.aad.7 <- DT.25[period == 'adult', {
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


DT.25.aad.7.p <-
	merge(
		DT.25.aad.7[iteration != 0],
		DT.25.aad.7[iteration == 0, .(effect, group, term, estimate)],
		by = c('effect', 'group', 'term'),
		suffixes = c('', '.obs')
	)

# DT.25.aad.7.pless <- DT.25.aad.7.p[, sum(estimate < estimate.obs), term]
# DT.25.aad.7.pless[,'pless'] <- DT.25.aad.7.pless$V1/1000
# DT.25.aad.7.pmore <- DT.25.aad.7.p[, sum(estimate > estimate.obs), term]
# DT.25.aad.7.pmore[,'pmore'] <- DT.25.aad.7.pmore$V1/1000


DT.25.aad.7.pboth <- DT.25.aad.7.p[, .(estimate = unique(estimate.obs), min = min(estimate), max = max(estimate),
																 sumless = sum(estimate < estimate.obs), summore = sum(estimate > estimate.obs),
																 pless = sum(estimate < estimate.obs)/1000, pmore = sum(estimate > estimate.obs)/1000),
														 term]



