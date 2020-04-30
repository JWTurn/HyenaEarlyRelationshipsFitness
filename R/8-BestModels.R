### Models  ====
# Julie Turner and Alec Robitaille
# Revisions started: March 31 2020


### Packages ----
# remotes::install_github('bbolker/broom.mixed')
libs <- c('lme4', 'data.table', 'broom.mixed', 'ggplot2', 'bbmle', 'car')
lapply(libs, require, character.only = TRUE)

### Function ----
se <- function(x){
	sd(x, na.rm = T)/ sqrt(length(na.omit(x)))
}

### Input data ----
raw <- 'data/raw-data/'
derived <- 'data/derived-data/'

DT <- readRDS(paste0(derived, 'DT_obs_rands.Rds'))

# just the observed data
DT.obs <- DT[iteration == 0]

DT.obs.ses <- DT.obs[,.(ego, period, nSession.obs = nSession)]
DT <-
	merge(DT, DT.obs.ses, by = c('ego', 'period'))

#### descriptive ####
DT.obs[,.( mean =mean(affil_betweenness), se=se(affil_betweenness)), by = .(period)]

### correlations ####

DT.cd <- DT.obs[period=='cd', .(nSession, ego_period_rank,
																alone, sri_degree, sri_strength, sri_betweenness,
																aggr_outdegree, aggr_indegree, aggr_outstrength, aggr_instrength, aggr_betweenness,
																affil_outdegree, affil_indegree, affil_outstrength, affil_instrength, affil_betweenness)]
View(cor(DT.cd))

DT.di <- DT.obs[period=='postgrad', .(nSession, ego_period_rank,
																			alone, sri_degree, sri_strength, sri_betweenness,
																			aggr_outdegree, aggr_indegree, aggr_outstrength, aggr_instrength, aggr_betweenness,
																			affil_outdegree, affil_indegree, affil_outstrength, affil_instrength, affil_betweenness)]
View(cor(DT.di))

DT.ad <- DT.obs[period=='adult', .(nSession, ego_period_rank,
																	 alone, sri_degree, sri_strength, sri_betweenness,
																	 aggr_outdegree, aggr_indegree, aggr_outstrength, aggr_instrength, aggr_betweenness,
																	 affil_outdegree, affil_indegree, affil_outstrength, affil_instrength, affil_betweenness)]
View(cor(DT.ad))


### Best models and randomizations ----
mod.7.terms <- c("ego_period_rank", "scale(sri_degree)", "scale(aggr_outdegree)", "scale(aggr_indegree)",
								 "scale(affil_outdegree)", "scale(affil_indegree)", "sd__(Intercept)")

mod.11.terms <- c("ego_period_rank", "scale(sri_strength)", "scale(aggr_outstrength)", "scale(aggr_instrength)",
									"scale(affil_outstrength)", "scale(affil_instrength)", "sd__(Intercept)")

mod.13.terms <- c("ego_period_rank", "scale(sri_betweenness)", "scale(aggr_betweenness)", "scale(affil_betweenness)", "sd__(Intercept)")


#### best models for longevity CD ####
# check the VIF of the model
l.cd.7 <- DT[period == 'cd' & iteration ==0,
	glmer(
		log(longevity_years) ~
			ego_period_rank +
			scale(sri_degree) +
			scale(aggr_outdegree) + scale(aggr_indegree) +
			scale(affil_outdegree) + scale(affil_indegree) +
			offset(log(clan_size)) +
			offset(log(nSession)) +
			(1 | mom),
		data = .SD,
		family = 'gaussian'
	)
]

range(vif(l.cd.7))
tidy(l.cd.7)

# run model for each iteration
DT.lcd.7 <- DT[period == 'cd', {
	m <- glmer(
		log(longevity_years) ~
			ego_period_rank +
			scale(sri_degree) +
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
																 median = median(estimate), '5%'=quantile(estimate, probs = c(0.05), na.rm = T),'95%'=quantile(estimate, probs = c(0.95), na.rm = T),
																 sumless = sum(estimate < estimate.obs), summore = sum(estimate > estimate.obs),
																 pless = sum(estimate < estimate.obs)/1000, pmore = sum(estimate > estimate.obs)/1000),
														 term]

DT.lcd.7.out <- DT.lcd.7.pboth[,.(term, estimate, corEffect = estimate - median, median, randRange= paste(round(`5%`, 3), round(`95%`, 3), sep = " to "), pmore, pless)]
DT.lcd.7.out[,'Prand'] <- ifelse(DT.lcd.7.out$estimate >= DT.lcd.7.out$median, DT.lcd.7.out$pmore, DT.lcd.7.out$pless)
DT.lcd.7.tab <- DT.lcd.7.out[term %in% mod.7.terms,.(term, estimate = round(estimate, 3), corEffect = round(corEffect, 3), randRange, Prand)]
DT.lcd.7.tab[,'network type'] <- DT.lcd.7.tab[,ifelse(term %like% 'sri', 'association', ifelse(term %like% 'aggr', 'aggression',
																																															 ifelse(term %like% 'affil', 'affiliation', '')))]
DT.lcd.7.tab$`network type` <- factor(DT.lcd.7.tab$`network type`, levels = c('association', 'aggression', 'affiliation'), labels = c('association', 'aggression', 'affiliation'))
DT.lcd.7.tab$term <- factor(DT.lcd.7.tab$term, levels = mod.7.terms, labels = c('dominance rank', 'degree',
																																								'out-degree', 'in-degree',
																																								'out-degree', 'in-degree', 'mother (random effect sd)'))

DT.lcd.7.tab <- DT.lcd.7.tab[,.(`network type`, term, estimate, corEffect, randRange, Prand)]





# range(vif(l.cd.11))
# tidy(l.cd.11)

DT.lcd.11 <- DT[period == 'cd', {
	m <- glmer(
		log(longevity_years) ~
			ego_period_rank +
			scale(sri_strength) +
			scale(aggr_outstrength) + scale(aggr_instrength) +
			scale(affil_outstrength) + scale(affil_instrength) +
			offset(log(clan_size)) +
			offset(log(nSession)) +
			(1 | mom),
		data = .SD,
		family = 'gaussian'
	)
	tidy(m)
}, by = iteration]


DT.lcd.11.p <-
	merge(
		DT.lcd.11[iteration != 0],
		DT.lcd.11[iteration == 0, .(effect, group, term, estimate)],
		by = c('effect', 'group', 'term'),
		suffixes = c('', '.obs')
	)


DT.lcd.11.pboth <- DT.lcd.11.p[, .(estimate = unique(estimate.obs), min = min(estimate), max = max(estimate),
																 median = median(estimate), '5%'=quantile(estimate, probs = c(0.05), na.rm = T),'95%'=quantile(estimate, probs = c(0.95), na.rm = T),
																 sumless = sum(estimate < estimate.obs), summore = sum(estimate > estimate.obs),
																 pless = sum(estimate < estimate.obs)/1000, pmore = sum(estimate > estimate.obs)/1000),
														 term]

DT.lcd.11.out <- DT.lcd.11.pboth[,.(term, estimate, corEffect = estimate - median, median, randRange= paste(round(`5%`, 3), round(`95%`, 3), sep = " to "), pmore, pless)]
DT.lcd.11.out[,'Prand'] <- ifelse(DT.lcd.11.out$estimate >= DT.lcd.11.out$median, DT.lcd.11.out$pmore, DT.lcd.11.out$pless)
DT.lcd.11.tab <- DT.lcd.11.out[term %in% mod.11.terms,.(term, estimate = round(estimate, 3), corEffect = round(corEffect, 3), randRange, Prand)]
DT.lcd.11.tab[,'network type'] <- DT.lcd.11.tab[,ifelse(term %like% 'sri', 'association', ifelse(term %like% 'aggr', 'aggression',
																																															 ifelse(term %like% 'affil', 'affiliation', '')))]
DT.lcd.11.tab$`network type` <- factor(DT.lcd.11.tab$`network type`, levels = c('association', 'aggression', 'affiliation'), labels = c('association', 'aggression', 'affiliation'))
DT.lcd.11.tab$term <- factor(DT.lcd.11.tab$term, levels = mod.11.terms, labels = c('dominance rank', 'strength',
																																								'out-strength', 'in-strength',
																																								'out-strength', 'in-strength', 'mother (random effect sd)'))

DT.lcd.11.tab <- DT.lcd.11.tab[,.(`network type`, term, estimate, corEffect, randRange, Prand)]




#### best model for longevity DI ####
# check_model(l.di.13)
# range(vif(l.di.13))
# tidy(l.di.13)

DT.ldi.13 <- DT[period == 'di', {
	m <- glmer(
		log(longevity_years) ~
			ego_period_rank +
			scale(sri_betweenness) +
			scale(aggr_betweenness) +
			scale(affil_betweenness) +
			offset(log(clan_size)) +
			offset(log(nSession)) +
			(1 | mom),
		data = .SD,
		family = 'gaussian'
	)
	tidy(m)
}, by = iteration]


DT.ldi.13.p <-
	merge(
		DT.ldi.13[iteration != 0],
		DT.ldi.13[iteration == 0, .(effect, group, term, estimate)],
		by = c('effect', 'group', 'term'),
		suffixes = c('', '.obs')
	)

DT.ldi.13.pboth <- DT.ldi.13.p[, .(estimate = unique(estimate.obs), min = min(estimate), max = max(estimate),
																 median = median(estimate), '5%'=quantile(estimate, probs = c(0.05), na.rm = T),'95%'=quantile(estimate, probs = c(0.95), na.rm = T),
																 sumless = sum(estimate < estimate.obs), summore = sum(estimate > estimate.obs),
																 pless = sum(estimate < estimate.obs)/1000, pmore = sum(estimate > estimate.obs)/1000),
														 term]

DT.ldi.13.out <- DT.ldi.13.pboth[,.(term, estimate, corEffect = estimate - median, median, randRange= paste(round(`5%`, 3), round(`95%`, 3), sep = " to "), pmore, pless)]
DT.ldi.13.out[,'Prand'] <- ifelse(DT.ldi.13.out$estimate >= DT.ldi.13.out$median, DT.ldi.13.out$pmore, DT.ldi.13.out$pless)
DT.ldi.13.tab <- DT.ldi.13.out[term %in% mod.13.terms,.(term, estimate = round(estimate, 3), corEffect = round(corEffect, 3), randRange, Prand)]
DT.ldi.13.tab[,'network type'] <- DT.ldi.13.tab[,ifelse(term %like% 'sri', 'association', ifelse(term %like% 'aggr', 'aggression',
																																															 ifelse(term %like% 'affil', 'affiliation', '')))]
DT.ldi.13.tab$`network type` <- factor(DT.ldi.13.tab$`network type`, levels = c('association', 'aggression', 'affiliation'), labels = c('association', 'aggression', 'affiliation'))
DT.ldi.13.tab$term <- factor(DT.ldi.13.tab$term, levels = mod.13.terms, labels = c('dominance rank', 'betweenness',
																																								'betweenness', 'betweenness',
																																								 'mother (random effect sd)'))

DT.ldi.13.tab <- DT.ldi.13.tab[,.(`network type`, term, estimate, corEffect, randRange, Prand)]



# check_model(l.di.7)
# range(vif(l.di.7))
# tidy(l.di.7)

DT.ldi.7 <- DT[period == 'di', {
	m <- glmer(
		log(longevity_years) ~
			ego_period_rank +
			scale(sri_degree) +
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
																 median = median(estimate), '5%'=quantile(estimate, probs = c(0.05), na.rm = T),'95%'=quantile(estimate, probs = c(0.95), na.rm = T),
																 sumless = sum(estimate < estimate.obs), summore = sum(estimate > estimate.obs),
																 pless = sum(estimate < estimate.obs)/1000, pmore = sum(estimate > estimate.obs)/1000),
														 term]

DT.ldi.7.out <- DT.ldi.7.pboth[,.(term, estimate, corEffect = estimate - median, median, randRange= paste(round(`5%`, 3), round(`95%`, 3), sep = " to "), pmore, pless)]
DT.ldi.7.out[,'Prand'] <- ifelse(DT.ldi.7.out$estimate >= DT.ldi.7.out$median, DT.ldi.7.out$pmore, DT.ldi.7.out$pless)
DT.ldi.7.tab <- DT.ldi.7.out[term %in% mod.7.terms,.(term, estimate = round(estimate, 3), corEffect = round(corEffect, 3), randRange, Prand)]
DT.ldi.7.tab[,'network type'] <- DT.ldi.7.tab[,ifelse(term %like% 'sri', 'association', ifelse(term %like% 'aggr', 'aggression',
																																															 ifelse(term %like% 'affil', 'affiliation', '')))]
DT.ldi.7.tab$`network type` <- factor(DT.ldi.7.tab$`network type`, levels = c('association', 'aggression', 'affiliation'), labels = c('association', 'aggression', 'affiliation'))
DT.ldi.7.tab$term <- factor(DT.ldi.7.tab$term, levels = mod.7.terms, labels = c('dominance rank', 'degree',
																																								'out-degree', 'in-degree',
																																								'out-degree', 'in-degree', 'mother (random effect sd)'))

DT.ldi.7.tab <- DT.ldi.7.tab[,.(`network type`, term, estimate, corEffect, randRange, Prand)]




#### best model for longevity Adult ####
# check_model(l.ad.7)
# range(vif(l.ad.7))
# tidy(l.ad.7)

DT.lad.7 <- DT[period == 'adult', {
	m <- glmer(
		log(longevity_years) ~
			ego_period_rank +
			scale(sri_degree) +
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
																 median = median(estimate), '5%'=quantile(estimate, probs = c(0.05), na.rm = T),'95%'=quantile(estimate, probs = c(0.95), na.rm = T),
																 sumless = sum(estimate < estimate.obs), summore = sum(estimate > estimate.obs),
																 pless = sum(estimate < estimate.obs)/1000, pmore = sum(estimate > estimate.obs)/1000),
														 term]

DT.lad.7.out <- DT.lad.7.pboth[,.(term, estimate, corEffect = estimate - median, median, randRange= paste(round(`5%`, 3), round(`95%`, 3), sep = " to "), pmore, pless)]
DT.lad.7.out[,'Prand'] <- ifelse(DT.lad.7.out$estimate >= DT.lad.7.out$median, DT.lad.7.out$pmore, DT.lad.7.out$pless)
DT.lad.7.tab <- DT.lad.7.out[term %in% mod.7.terms,.(term, estimate = round(estimate, 3), corEffect = round(corEffect, 3), randRange, Prand)]
DT.lad.7.tab[,'network type'] <- DT.lad.7.tab[,ifelse(term %like% 'sri', 'association', ifelse(term %like% 'aggr', 'aggression',
																																															 ifelse(term %like% 'affil', 'affiliation', '')))]
DT.lad.7.tab$`network type` <- factor(DT.lad.7.tab$`network type`, levels = c('association', 'aggression', 'affiliation'), labels = c('association', 'aggression', 'affiliation'))
DT.lad.7.tab$term <- factor(DT.lad.7.tab$term, levels = mod.7.terms, labels = c('dominance rank', 'degree',
																																								'out-degree', 'in-degree',
																																								'out-degree', 'in-degree', 'mother (random effect sd)'))

DT.lad.7.tab <- DT.lad.7.tab[,.(`network type`, term, estimate, corEffect, randRange, Prand)]



# check_model(l.ad.13)
# range(vif(l.ad.13))
# tidy(l.ad.13)

DT.lad.13 <- DT[period == 'adult', {
	m <- glmer(
		log(longevity_years) ~
			ego_period_rank +
			scale(sri_betweenness) +
			scale(aggr_betweenness) +
			scale(affil_betweenness) +
			offset(log(clan_size)) +
			offset(log(nSession)) +
			(1 | mom),
		data = .SD,
		family = 'gaussian'
	)
	tidy(m)
}, by = iteration]


DT.lad.13.p <-
	merge(
		DT.lad.13[iteration != 0],
		DT.lad.13[iteration == 0, .(effect, group, term, estimate)],
		by = c('effect', 'group', 'term'),
		suffixes = c('', '.obs')
	)

DT.lad.13.pboth <- DT.lad.13.p[, .(estimate = unique(estimate.obs), min = min(estimate), max = max(estimate),
																	 median = median(estimate), '5%'=quantile(estimate, probs = c(0.05), na.rm = T),'95%'=quantile(estimate, probs = c(0.95), na.rm = T),
																	 sumless = sum(estimate < estimate.obs), summore = sum(estimate > estimate.obs),
																	 pless = sum(estimate < estimate.obs)/1000, pmore = sum(estimate > estimate.obs)/1000),
															 term]

DT.lad.13.out <- DT.lad.13.pboth[,.(term, estimate, corEffect = estimate - median, median, randRange= paste(round(`5%`, 3), round(`95%`, 3), sep = " to "), pmore, pless)]
DT.lad.13.out[,'Prand'] <- ifelse(DT.lad.13.out$estimate >= DT.lad.13.out$median, DT.lad.13.out$pmore, DT.lad.13.out$pless)
DT.lad.13.tab <- DT.lad.13.out[term %in% mod.13.terms,.(term, estimate = round(estimate, 3), corEffect = round(corEffect, 3), randRange, Prand)]
DT.lad.13.tab[,'network type'] <- DT.lad.13.tab[,ifelse(term %like% 'sri', 'association', ifelse(term %like% 'aggr', 'aggression',
																																																 ifelse(term %like% 'affil', 'affiliation', '')))]
DT.lad.13.tab$`network type` <- factor(DT.lad.13.tab$`network type`, levels = c('association', 'aggression', 'affiliation'), labels = c('association', 'aggression', 'affiliation'))
DT.lad.13.tab$term <- factor(DT.lad.13.tab$term, levels = mod.13.terms, labels = c('dominance rank', 'betweenness',
																																									 'betweenness', 'betweenness',
																																									 'mother (random effect sd)'))

DT.lad.13.tab <- DT.lad.13.tab[,.(`network type`, term, estimate, corEffect, randRange, Prand)]




#### best models for ARS CD ####
# check_model(a.cd.7)
# range(vif(a.cd.7))
# tidy(a.cd.7)

DT.acd.7 <- DT[period == 'cd', {
	m <- glmer(
		log(annual_rs) ~
			ego_period_rank +
			scale(sri_degree) +
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
																 median = median(estimate), '5%'=quantile(estimate, probs = c(0.05), na.rm = T),'95%'=quantile(estimate, probs = c(0.95), na.rm = T),
																 sumless = sum(estimate < estimate.obs), summore = sum(estimate > estimate.obs),
																 pless = sum(estimate < estimate.obs)/1000, pmore = sum(estimate > estimate.obs)/1000),
														 term]

DT.acd.7.out <- DT.acd.7.pboth[,.(term, estimate, corEffect = estimate - median, median, randRange= paste(round(`5%`, 3), round(`95%`, 3), sep = " to "), pmore, pless)]
DT.acd.7.out[,'Prand'] <- ifelse(DT.acd.7.out$estimate >= DT.acd.7.out$median, DT.acd.7.out$pmore, DT.acd.7.out$pless)
DT.acd.7.tab <- DT.acd.7.out[term %in% mod.7.terms,.(term, estimate = round(estimate, 3), corEffect = round(corEffect, 3), randRange, Prand)]
DT.acd.7.tab[,'network type'] <- DT.acd.7.tab[,ifelse(term %like% 'sri', 'association', ifelse(term %like% 'aggr', 'aggression',
																																															 ifelse(term %like% 'affil', 'affiliation', '')))]
DT.acd.7.tab$`network type` <- factor(DT.acd.7.tab$`network type`, levels = c('association', 'aggression', 'affiliation'), labels = c('association', 'aggression', 'affiliation'))
DT.acd.7.tab$term <- factor(DT.acd.7.tab$term, levels = mod.7.terms, labels = c('dominance rank', 'degree',
																																								'out-degree', 'in-degree',
																																								'out-degree', 'in-degree', 'mother (random effect sd)'))

DT.acd.7.tab <- DT.acd.7.tab[,.(`network type`, term, estimate, corEffect, randRange, Prand)]

#### best models for ARS DI ####
# check_model(a.di.7)
# range(vif(a.di.7))
# tidy(a.di.7)

DT.adi.7 <- DT[period == 'di', {
	m <- glmer(
		log(annual_rs) ~
			ego_period_rank +
			scale(sri_degree) +
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
																 median = median(estimate), '5%'=quantile(estimate, probs = c(0.05), na.rm = T),'95%'=quantile(estimate, probs = c(0.95), na.rm = T),
																 sumless = sum(estimate < estimate.obs), summore = sum(estimate > estimate.obs),
																 pless = sum(estimate < estimate.obs)/1000, pmore = sum(estimate > estimate.obs)/1000),
														 term]

DT.adi.7.out <- DT.adi.7.pboth[,.(term, estimate, corEffect = estimate - median, median, randRange= paste(round(`5%`, 3), round(`95%`, 3), sep = " to "), pmore, pless)]
DT.adi.7.out[,'Prand'] <- ifelse(DT.adi.7.out$estimate >= DT.adi.7.out$median, DT.adi.7.out$pmore, DT.adi.7.out$pless)
DT.adi.7.tab <- DT.adi.7.out[term %in% mod.7.terms,.(term, estimate = round(estimate, 3), corEffect = round(corEffect, 3), randRange, Prand)]
DT.adi.7.tab[,'network type'] <- DT.adi.7.tab[,ifelse(term %like% 'sri', 'association', ifelse(term %like% 'aggr', 'aggression',
																																															 ifelse(term %like% 'affil', 'affiliation', '')))]
DT.adi.7.tab$`network type` <- factor(DT.adi.7.tab$`network type`, levels = c('association', 'aggression', 'affiliation'), labels = c('association', 'aggression', 'affiliation'))
DT.adi.7.tab$term <- factor(DT.adi.7.tab$term, levels = mod.7.terms, labels = c('dominance rank', 'degree',
																																								'out-degree', 'in-degree',
																																								'out-degree', 'in-degree', 'mother (random effect sd)'))

DT.adi.7.tab <- DT.adi.7.tab[,.(`network type`, term, estimate, corEffect, randRange, Prand)]


#### best models for ARS Adult ####
# check_model(a.ad.7)
# range(vif(a.ad.7))
# tidy(a.ad.7)

DT.aad.7 <- DT[period == 'adult', {
	m <- glmer(
		log(annual_rs) ~
			ego_period_rank +
			scale(sri_degree) +
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
																 median = median(estimate), '5%'=quantile(estimate, probs = c(0.05), na.rm = T),'95%'=quantile(estimate, probs = c(0.95), na.rm = T),
																 sumless = sum(estimate < estimate.obs), summore = sum(estimate > estimate.obs),
																 pless = sum(estimate < estimate.obs)/1000, pmore = sum(estimate > estimate.obs)/1000),
														 term]

DT.aad.7.out <- DT.aad.7.pboth[,.(term, estimate, corEffect = estimate - median, median, randRange= paste(round(`5%`, 3), round(`95%`, 3), sep = " to "), pmore, pless)]
DT.aad.7.out[,'Prand'] <- ifelse(DT.aad.7.out$estimate >= DT.aad.7.out$median, DT.aad.7.out$pmore, DT.aad.7.out$pless)
DT.aad.7.tab <- DT.aad.7.out[term %in% mod.7.terms,.(term, estimate = round(estimate, 3), corEffect = round(corEffect, 3), randRange, Prand)]
DT.aad.7.tab[,'network type'] <- DT.aad.7.tab[,ifelse(term %like% 'sri', 'association', ifelse(term %like% 'aggr', 'aggression',
																																															 ifelse(term %like% 'affil', 'affiliation', '')))]
DT.aad.7.tab$`network type` <- factor(DT.aad.7.tab$`network type`, levels = c('association', 'aggression', 'affiliation'), labels = c('association', 'aggression', 'affiliation'))
DT.aad.7.tab$term <- factor(DT.aad.7.tab$term, levels = mod.7.terms, labels = c('dominance rank', 'degree',
																																								'out-degree', 'in-degree',
																																								'out-degree', 'in-degree', 'mother (random effect sd)'))

DT.aad.7.tab <- DT.aad.7.tab[,.(`network type`, term, estimate, corEffect, randRange, Prand)]




#### 25 observations ####
#### best models for longevity CD ####
# run model for each iteration
DT.25.lcd.7 <- DT[period == 'cd' & nSession.obs >= 25, {
	m <- glmer(
		log(longevity_years) ~
			ego_period_rank +
			scale(sri_degree) +
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
DT.25.lcd.7.p <-
	merge(
		DT.25.lcd.7[iteration != 0],
		DT.25.lcd.7[iteration == 0, .(effect, group, term, estimate)],
		by = c('effect', 'group', 'term'),
		suffixes = c('', '.obs')
	)

# calculate two-tailed p-values and ranges of random model parameter estimates
# less is relative to the lowest values of the random ranges, more is relative to the highest values of the random ranges
# sum is total value, and p is proportions
DT.25.lcd.7.pboth <- DT.25.lcd.7.p[, .(estimate = unique(estimate.obs), min = min(estimate), max = max(estimate),
																 median = median(estimate), '5%'=quantile(estimate, probs = c(0.05), na.rm = T),'95%'=quantile(estimate, probs = c(0.95), na.rm = T),
																 sumless = sum(estimate < estimate.obs), summore = sum(estimate > estimate.obs),
																 pless = sum(estimate < estimate.obs)/1000, pmore = sum(estimate > estimate.obs)/1000),
														 term]

DT.25.lcd.7.out <- DT.25.lcd.7.pboth[,.(term, estimate, corEffect = estimate - median, median, randRange= paste(round(`5%`, 3), round(`95%`, 3), sep = " to "), pmore, pless)]
DT.25.lcd.7.out[,'Prand'] <- ifelse(DT.25.lcd.7.out$estimate >= DT.25.lcd.7.out$median, DT.25.lcd.7.out$pmore, DT.25.lcd.7.out$pless)
DT.25.lcd.7.tab <- DT.25.lcd.7.out[term %in% mod.7.terms,.(term, estimate = round(estimate, 3), corEffect = round(corEffect, 3), randRange, Prand)]
DT.25.lcd.7.tab[,'network type'] <- DT.25.lcd.7.tab[,ifelse(term %like% 'sri', 'association', ifelse(term %like% 'aggr', 'aggression',
																																															 ifelse(term %like% 'affil', 'affiliation', '')))]
DT.25.lcd.7.tab$`network type` <- factor(DT.25.lcd.7.tab$`network type`, levels = c('association', 'aggression', 'affiliation'), labels = c('association', 'aggression', 'affiliation'))
DT.25.lcd.7.tab$term <- factor(DT.25.lcd.7.tab$term, levels = mod.7.terms, labels = c('dominance rank', 'degree',
																																								'out-degree', 'in-degree',
																																								'out-degree', 'in-degree', 'mother (random effect sd)'))

DT.25.lcd.7.tab <- DT.25.lcd.7.tab[,.(`network type`, term, estimate, corEffect, randRange, Prand)]





DT.25.lcd.11 <- DT[period == 'cd' & nSession.obs >= 25, {
	m <- glmer(
		log(longevity_years) ~
			ego_period_rank +
			scale(sri_strength) +
			scale(aggr_outstrength) + scale(aggr_instrength) +
			scale(affil_outstrength) + scale(affil_instrength) +
			offset(log(clan_size)) +
			offset(log(nSession)) +
			(1 | mom),
		data = .SD,
		family = 'gaussian'
	)
	tidy(m)
}, by = iteration]


DT.25.lcd.11.p <-
	merge(
		DT.25.lcd.11[iteration != 0],
		DT.25.lcd.11[iteration == 0, .(effect, group, term, estimate)],
		by = c('effect', 'group', 'term'),
		suffixes = c('', '.obs')
	)


DT.25.lcd.11.pboth <- DT.25.lcd.11.p[, .(estimate = unique(estimate.obs), min = min(estimate), max = max(estimate),
																	 median = median(estimate), '5%'=quantile(estimate, probs = c(0.05), na.rm = T),'95%'=quantile(estimate, probs = c(0.95), na.rm = T),
																	 sumless = sum(estimate < estimate.obs), summore = sum(estimate > estimate.obs),
																	 pless = sum(estimate < estimate.obs)/1000, pmore = sum(estimate > estimate.obs)/1000),
															 term]

DT.25.lcd.11.out <- DT.25.lcd.11.pboth[,.(term, estimate, corEffect = estimate - median, median, randRange= paste(round(`5%`, 3), round(`95%`, 3), sep = " to "), pmore, pless)]
DT.25.lcd.11.out[,'Prand'] <- ifelse(DT.25.lcd.11.out$estimate >= DT.25.lcd.11.out$median, DT.25.lcd.11.out$pmore, DT.25.lcd.11.out$pless)
DT.25.lcd.11.tab <- DT.25.lcd.11.out[term %in% mod.11.terms,.(term, estimate = round(estimate, 3), corEffect = round(corEffect, 3), randRange, Prand)]
DT.25.lcd.11.tab[,'network type'] <- DT.25.lcd.11.tab[,ifelse(term %like% 'sri', 'association', ifelse(term %like% 'aggr', 'aggression',
																																																 ifelse(term %like% 'affil', 'affiliation', '')))]
DT.25.lcd.11.tab$`network type` <- factor(DT.25.lcd.11.tab$`network type`, levels = c('association', 'aggression', 'affiliation'), labels = c('association', 'aggression', 'affiliation'))
DT.25.lcd.11.tab$term <- factor(DT.25.lcd.11.tab$term, levels = mod.11.terms, labels = c('dominance rank', 'strength',
																																									 'out-strength', 'in-strength',
																																									 'out-strength', 'in-strength', 'mother (random effect sd)'))

DT.25.lcd.11.tab <- DT.25.lcd.11.tab[,.(`network type`, term, estimate, corEffect, randRange, Prand)]




#### best model for longevity DI ####

DT.25.ldi.13 <- DT[period == 'di' & nSession.obs >= 25, {
	m <- glmer(
		log(longevity_years) ~
			ego_period_rank +
			scale(sri_betweenness) +
			scale(aggr_betweenness) +
			scale(affil_betweenness) +
			offset(log(clan_size)) +
			offset(log(nSession)) +
			(1 | mom),
		data = .SD,
		family = 'gaussian'
	)
	tidy(m)
}, by = iteration]


DT.25.ldi.13.p <-
	merge(
		DT.25.ldi.13[iteration != 0],
		DT.25.ldi.13[iteration == 0, .(effect, group, term, estimate)],
		by = c('effect', 'group', 'term'),
		suffixes = c('', '.obs')
	)

DT.25.ldi.13.pboth <- DT.25.ldi.13.p[, .(estimate = unique(estimate.obs), min = min(estimate), max = max(estimate),
																	 median = median(estimate), '5%'=quantile(estimate, probs = c(0.05), na.rm = T),'95%'=quantile(estimate, probs = c(0.95), na.rm = T),
																	 sumless = sum(estimate < estimate.obs), summore = sum(estimate > estimate.obs),
																	 pless = sum(estimate < estimate.obs)/1000, pmore = sum(estimate > estimate.obs)/1000),
															 term]

DT.25.ldi.13.out <- DT.25.ldi.13.pboth[,.(term, estimate, corEffect = estimate - median, median, randRange= paste(round(`5%`, 3), round(`95%`, 3), sep = " to "), pmore, pless)]
DT.25.ldi.13.out[,'Prand'] <- ifelse(DT.25.ldi.13.out$estimate >= DT.25.ldi.13.out$median, DT.25.ldi.13.out$pmore, DT.25.ldi.13.out$pless)
DT.25.ldi.13.tab <- DT.25.ldi.13.out[term %in% mod.13.terms,.(term, estimate = round(estimate, 3), corEffect = round(corEffect, 3), randRange, Prand)]
DT.25.ldi.13.tab[,'network type'] <- DT.25.ldi.13.tab[,ifelse(term %like% 'sri', 'association', ifelse(term %like% 'aggr', 'aggression',
																																																 ifelse(term %like% 'affil', 'affiliation', '')))]
DT.25.ldi.13.tab$`network type` <- factor(DT.25.ldi.13.tab$`network type`, levels = c('association', 'aggression', 'affiliation'), labels = c('association', 'aggression', 'affiliation'))
DT.25.ldi.13.tab$term <- factor(DT.25.ldi.13.tab$term, levels = mod.13.terms, labels = c('dominance rank', 'betweenness',
																																									 'betweenness', 'betweenness',
																																									 'mother (random effect sd)'))

DT.25.ldi.13.tab <- DT.25.ldi.13.tab[,.(`network type`, term, estimate, corEffect, randRange, Prand)]





DT.25.ldi.7 <- DT[period == 'di' & nSession.obs >= 25, {
	m <- glmer(
		log(longevity_years) ~
			ego_period_rank +
			scale(sri_degree) +
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


DT.25.ldi.7.p <-
	merge(
		DT.25.ldi.7[iteration != 0],
		DT.25.ldi.7[iteration == 0, .(effect, group, term, estimate)],
		by = c('effect', 'group', 'term'),
		suffixes = c('', '.obs')
	)

DT.25.ldi.7.pboth <- DT.25.ldi.7.p[, .(estimate = unique(estimate.obs), min = min(estimate), max = max(estimate),
																 median = median(estimate), '5%'=quantile(estimate, probs = c(0.05), na.rm = T),'95%'=quantile(estimate, probs = c(0.95), na.rm = T),
																 sumless = sum(estimate < estimate.obs), summore = sum(estimate > estimate.obs),
																 pless = sum(estimate < estimate.obs)/1000, pmore = sum(estimate > estimate.obs)/1000),
														 term]

DT.25.ldi.7.out <- DT.25.ldi.7.pboth[,.(term, estimate, corEffect = estimate - median, median, randRange= paste(round(`5%`, 3), round(`95%`, 3), sep = " to "), pmore, pless)]
DT.25.ldi.7.out[,'Prand'] <- ifelse(DT.25.ldi.7.out$estimate >= DT.25.ldi.7.out$median, DT.25.ldi.7.out$pmore, DT.25.ldi.7.out$pless)
DT.25.ldi.7.tab <- DT.25.ldi.7.out[term %in% mod.7.terms,.(term, estimate = round(estimate, 3), corEffect = round(corEffect, 3), randRange, Prand)]
DT.25.ldi.7.tab[,'network type'] <- DT.25.ldi.7.tab[,ifelse(term %like% 'sri', 'association', ifelse(term %like% 'aggr', 'aggression',
																																															 ifelse(term %like% 'affil', 'affiliation', '')))]
DT.25.ldi.7.tab$`network type` <- factor(DT.25.ldi.7.tab$`network type`, levels = c('association', 'aggression', 'affiliation'), labels = c('association', 'aggression', 'affiliation'))
DT.25.ldi.7.tab$term <- factor(DT.25.ldi.7.tab$term, levels = mod.7.terms, labels = c('dominance rank', 'degree',
																																								'out-degree', 'in-degree',
																																								'out-degree', 'in-degree', 'mother (random effect sd)'))

DT.25.ldi.7.tab <- DT.25.ldi.7.tab[,.(`network type`, term, estimate, corEffect, randRange, Prand)]




#### best model for longevity Adult ####

DT.25.lad.7 <- DT[period == 'adult' & nSession.obs >= 25, {
	m <- glmer(
		log(longevity_years) ~
			ego_period_rank +
			scale(sri_degree) +
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


DT.25.lad.7.pboth <- DT.25.lad.7.p[, .(estimate = unique(estimate.obs), min = min(estimate), max = max(estimate),
																 median = median(estimate), '5%'=quantile(estimate, probs = c(0.05), na.rm = T),'95%'=quantile(estimate, probs = c(0.95), na.rm = T),
																 sumless = sum(estimate < estimate.obs), summore = sum(estimate > estimate.obs),
																 pless = sum(estimate < estimate.obs)/1000, pmore = sum(estimate > estimate.obs)/1000),
														 term]

DT.25.lad.7.out <- DT.25.lad.7.pboth[,.(term, estimate, corEffect = estimate - median, median, randRange= paste(round(`5%`, 3), round(`95%`, 3), sep = " to "), pmore, pless)]
DT.25.lad.7.out[,'Prand'] <- ifelse(DT.25.lad.7.out$estimate >= DT.25.lad.7.out$median, DT.25.lad.7.out$pmore, DT.25.lad.7.out$pless)
DT.25.lad.7.tab <- DT.25.lad.7.out[term %in% mod.7.terms,.(term, estimate = round(estimate, 3), corEffect = round(corEffect, 3), randRange, Prand)]
DT.25.lad.7.tab[,'network type'] <- DT.25.lad.7.tab[,ifelse(term %like% 'sri', 'association', ifelse(term %like% 'aggr', 'aggression',
																																															 ifelse(term %like% 'affil', 'affiliation', '')))]
DT.25.lad.7.tab$`network type` <- factor(DT.25.lad.7.tab$`network type`, levels = c('association', 'aggression', 'affiliation'), labels = c('association', 'aggression', 'affiliation'))
DT.25.lad.7.tab$term <- factor(DT.25.lad.7.tab$term, levels = mod.7.terms, labels = c('dominance rank', 'degree',
																																								'out-degree', 'in-degree',
																																								'out-degree', 'in-degree', 'mother (random effect sd)'))

DT.25.lad.7.tab <- DT.25.lad.7.tab[,.(`network type`, term, estimate, corEffect, randRange, Prand)]





DT.25.lad.13 <- DT[period == 'adult' & nSession.obs >= 25, {
	m <- glmer(
		log(longevity_years) ~
			ego_period_rank +
			scale(sri_betweenness) +
			scale(aggr_betweenness) +
			scale(affil_betweenness) +
			offset(log(clan_size)) +
			offset(log(nSession)) +
			(1 | mom),
		data = .SD,
		family = 'gaussian'
	)
	tidy(m)
}, by = iteration]


DT.25.lad.13.p <-
	merge(
		DT.25.lad.13[iteration != 0],
		DT.25.lad.13[iteration == 0, .(effect, group, term, estimate)],
		by = c('effect', 'group', 'term'),
		suffixes = c('', '.obs')
	)

DT.25.lad.13.pboth <- DT.25.lad.13.p[, .(estimate = unique(estimate.obs), min = min(estimate), max = max(estimate),
																	 median = median(estimate), '5%'=quantile(estimate, probs = c(0.05), na.rm = T),'95%'=quantile(estimate, probs = c(0.95), na.rm = T),
																	 sumless = sum(estimate < estimate.obs), summore = sum(estimate > estimate.obs),
																	 pless = sum(estimate < estimate.obs)/1000, pmore = sum(estimate > estimate.obs)/1000),
															 term]

DT.25.lad.13.out <- DT.25.lad.13.pboth[,.(term, estimate, corEffect = estimate - median, median, randRange= paste(round(`5%`, 3), round(`95%`, 3), sep = " to "), pmore, pless)]
DT.25.lad.13.out[,'Prand'] <- ifelse(DT.25.lad.13.out$estimate >= DT.25.lad.13.out$median, DT.25.lad.13.out$pmore, DT.25.lad.13.out$pless)
DT.25.lad.13.tab <- DT.25.lad.13.out[term %in% mod.13.terms,.(term, estimate = round(estimate, 3), corEffect = round(corEffect, 3), randRange, Prand)]
DT.25.lad.13.tab[,'network type'] <- DT.25.lad.13.tab[,ifelse(term %like% 'sri', 'association', ifelse(term %like% 'aggr', 'aggression',
																																																 ifelse(term %like% 'affil', 'affiliation', '')))]
DT.25.lad.13.tab$`network type` <- factor(DT.25.lad.13.tab$`network type`, levels = c('association', 'aggression', 'affiliation'), labels = c('association', 'aggression', 'affiliation'))
DT.25.lad.13.tab$term <- factor(DT.25.lad.13.tab$term, levels = mod.13.terms, labels = c('dominance rank', 'betweenness',
																																									 'betweenness', 'betweenness',
																																									 'mother (random effect sd)'))

DT.25.lad.13.tab <- DT.25.lad.13.tab[,.(`network type`, term, estimate, corEffect, randRange, Prand)]




#### best models for ARS CD ####

DT.25.acd.7 <- DT[period == 'cd' & nSession.obs >= 25, {
	m <- glmer(
		log(annual_rs) ~
			ego_period_rank +
			scale(sri_degree) +
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


DT.25.acd.7.pboth <- DT.25.acd.7.p[, .(estimate = unique(estimate.obs), min = min(estimate), max = max(estimate),
																 median = median(estimate), '5%'=quantile(estimate, probs = c(0.05), na.rm = T),'95%'=quantile(estimate, probs = c(0.95), na.rm = T),
																 sumless = sum(estimate < estimate.obs), summore = sum(estimate > estimate.obs),
																 pless = sum(estimate < estimate.obs)/1000, pmore = sum(estimate > estimate.obs)/1000),
														 term]

DT.25.acd.7.out <- DT.25.acd.7.pboth[,.(term, estimate, corEffect = estimate - median, median, randRange= paste(round(`5%`, 3), round(`95%`, 3), sep = " to "), pmore, pless)]
DT.25.acd.7.out[,'Prand'] <- ifelse(DT.25.acd.7.out$estimate >= DT.25.acd.7.out$median, DT.25.acd.7.out$pmore, DT.25.acd.7.out$pless)
DT.25.acd.7.tab <- DT.25.acd.7.out[term %in% mod.7.terms,.(term, estimate = round(estimate, 3), corEffect = round(corEffect, 3), randRange, Prand)]
DT.25.acd.7.tab[,'network type'] <- DT.25.acd.7.tab[,ifelse(term %like% 'sri', 'association', ifelse(term %like% 'aggr', 'aggression',
																																															 ifelse(term %like% 'affil', 'affiliation', '')))]
DT.25.acd.7.tab$`network type` <- factor(DT.25.acd.7.tab$`network type`, levels = c('association', 'aggression', 'affiliation'), labels = c('association', 'aggression', 'affiliation'))
DT.25.acd.7.tab$term <- factor(DT.25.acd.7.tab$term, levels = mod.7.terms, labels = c('dominance rank', 'degree',
																																								'out-degree', 'in-degree',
																																								'out-degree', 'in-degree', 'mother (random effect sd)'))

DT.25.acd.7.tab <- DT.25.acd.7.tab[,.(`network type`, term, estimate, corEffect, randRange, Prand)]

#### best models for ARS DI ####

DT.25.adi.7 <- DT[period == 'di' & nSession.obs >= 25, {
	m <- glmer(
		log(annual_rs) ~
			ego_period_rank +
			scale(sri_degree) +
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


DT.25.adi.7.p <-
	merge(
		DT.25.adi.7[iteration != 0],
		DT.25.adi.7[iteration == 0, .(effect, group, term, estimate)],
		by = c('effect', 'group', 'term'),
		suffixes = c('', '.obs')
	)


DT.25.adi.7.pboth <- DT.25.adi.7.p[, .(estimate = unique(estimate.obs), min = min(estimate), max = max(estimate),
																 median = median(estimate), '5%'=quantile(estimate, probs = c(0.05), na.rm = T),'95%'=quantile(estimate, probs = c(0.95), na.rm = T),
																 sumless = sum(estimate < estimate.obs), summore = sum(estimate > estimate.obs),
																 pless = sum(estimate < estimate.obs)/1000, pmore = sum(estimate > estimate.obs)/1000),
														 term]

DT.25.adi.7.out <- DT.25.adi.7.pboth[,.(term, estimate, corEffect = estimate - median, median, randRange= paste(round(`5%`, 3), round(`95%`, 3), sep = " to "), pmore, pless)]
DT.25.adi.7.out[,'Prand'] <- ifelse(DT.25.adi.7.out$estimate >= DT.25.adi.7.out$median, DT.25.adi.7.out$pmore, DT.25.adi.7.out$pless)
DT.25.adi.7.tab <- DT.25.adi.7.out[term %in% mod.7.terms,.(term, estimate = round(estimate, 3), corEffect = round(corEffect, 3), randRange, Prand)]
DT.25.adi.7.tab[,'network type'] <- DT.25.adi.7.tab[,ifelse(term %like% 'sri', 'association', ifelse(term %like% 'aggr', 'aggression',
																																															 ifelse(term %like% 'affil', 'affiliation', '')))]
DT.25.adi.7.tab$`network type` <- factor(DT.25.adi.7.tab$`network type`, levels = c('association', 'aggression', 'affiliation'), labels = c('association', 'aggression', 'affiliation'))
DT.25.adi.7.tab$term <- factor(DT.25.adi.7.tab$term, levels = mod.7.terms, labels = c('dominance rank', 'degree',
																																								'out-degree', 'in-degree',
																																								'out-degree', 'in-degree', 'mother (random effect sd)'))

DT.25.adi.7.tab <- DT.25.adi.7.tab[,.(`network type`, term, estimate, corEffect, randRange, Prand)]


#### best models for ARS Adult ####

DT.25.aad.7 <- DT[period == 'adult' & nSession.obs >= 25, {
	m <- glmer(
		log(annual_rs) ~
			ego_period_rank +
			scale(sri_degree) +
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

DT.25.aad.7.pboth <- DT.25.aad.7.p[, .(estimate = unique(estimate.obs), min = min(estimate), max = max(estimate),
																 median = median(estimate), '5%'=quantile(estimate, probs = c(0.05), na.rm = T),'95%'=quantile(estimate, probs = c(0.95), na.rm = T),
																 sumless = sum(estimate < estimate.obs), summore = sum(estimate > estimate.obs),
																 pless = sum(estimate < estimate.obs)/1000, pmore = sum(estimate > estimate.obs)/1000),
														 term]

DT.25.aad.7.out <- DT.25.aad.7.pboth[,.(term, estimate, corEffect = estimate - median, median, randRange= paste(round(`5%`, 3), round(`95%`, 3), sep = " to "), pmore, pless)]
DT.25.aad.7.out[,'Prand'] <- ifelse(DT.25.aad.7.out$estimate >= DT.25.aad.7.out$median, DT.25.aad.7.out$pmore, DT.25.aad.7.out$pless)
DT.25.aad.7.tab <- DT.25.aad.7.out[term %in% mod.7.terms,.(term, estimate = round(estimate, 3), corEffect = round(corEffect, 3), randRange, Prand)]
DT.25.aad.7.tab[,'network type'] <- DT.25.aad.7.tab[,ifelse(term %like% 'sri', 'association', ifelse(term %like% 'aggr', 'aggression',
																																															 ifelse(term %like% 'affil', 'affiliation', '')))]
DT.25.aad.7.tab$`network type` <- factor(DT.25.aad.7.tab$`network type`, levels = c('association', 'aggression', 'affiliation'), labels = c('association', 'aggression', 'affiliation'))
DT.25.aad.7.tab$term <- factor(DT.25.aad.7.tab$term, levels = mod.7.terms, labels = c('dominance rank', 'degree',
																																								'out-degree', 'in-degree',
																																								'out-degree', 'in-degree', 'mother (random effect sd)'))

DT.25.aad.7.tab <- DT.25.aad.7.tab[,.(`network type`, term, estimate, corEffect, randRange, Prand)]



#### 50 observations ####
#### best models for longevity CD ####
# run model for each iteration
DT.50.lcd.7 <- DT[period == 'cd' & nSession.obs >= 50, {
	m <- glmer(
		log(longevity_years) ~
			ego_period_rank +
			scale(sri_degree) +
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
DT.50.lcd.7.p <-
	merge(
		DT.50.lcd.7[iteration != 0],
		DT.50.lcd.7[iteration == 0, .(effect, group, term, estimate)],
		by = c('effect', 'group', 'term'),
		suffixes = c('', '.obs')
	)

# calculate two-tailed p-values and ranges of random model parameter estimates
# less is relative to the lowest values of the random ranges, more is relative to the highest values of the random ranges
# sum is total value, and p is proportions
DT.50.lcd.7.pboth <- DT.50.lcd.7.p[, .(estimate = unique(estimate.obs), min = min(estimate), max = max(estimate),
																			 median = median(estimate), '5%'=quantile(estimate, probs = c(0.05), na.rm = T),'95%'=quantile(estimate, probs = c(0.95), na.rm = T),
																			 sumless = sum(estimate < estimate.obs), summore = sum(estimate > estimate.obs),
																			 pless = sum(estimate < estimate.obs)/1000, pmore = sum(estimate > estimate.obs)/1000),
																	 term]

DT.50.lcd.7.out <- DT.50.lcd.7.pboth[,.(term, estimate, corEffect = estimate - median, median, randRange= paste(round(`5%`, 3), round(`95%`, 3), sep = " to "), pmore, pless)]
DT.50.lcd.7.out[,'Prand'] <- ifelse(DT.50.lcd.7.out$estimate >= DT.50.lcd.7.out$median, DT.50.lcd.7.out$pmore, DT.50.lcd.7.out$pless)
DT.50.lcd.7.tab <- DT.50.lcd.7.out[term %in% mod.7.terms,.(term, estimate = round(estimate, 3), corEffect = round(corEffect, 3), randRange, Prand)]
DT.50.lcd.7.tab[,'network type'] <- DT.50.lcd.7.tab[,ifelse(term %like% 'sri', 'association', ifelse(term %like% 'aggr', 'aggression',
																																																		 ifelse(term %like% 'affil', 'affiliation', '')))]
DT.50.lcd.7.tab$`network type` <- factor(DT.50.lcd.7.tab$`network type`, levels = c('association', 'aggression', 'affiliation'), labels = c('association', 'aggression', 'affiliation'))
DT.50.lcd.7.tab$term <- factor(DT.50.lcd.7.tab$term, levels = mod.7.terms, labels = c('dominance rank', 'degree',
																																											'out-degree', 'in-degree',
																																											'out-degree', 'in-degree', 'mother (random effect sd)'))

DT.50.lcd.7.tab <- DT.50.lcd.7.tab[,.(`network type`, term, estimate, corEffect, randRange, Prand)]





DT.50.lcd.11 <- DT[period == 'cd' & nSession.obs >= 50, {
	m <- glmer(
		log(longevity_years) ~
			ego_period_rank +
			scale(sri_strength) +
			scale(aggr_outstrength) + scale(aggr_instrength) +
			scale(affil_outstrength) + scale(affil_instrength) +
			offset(log(clan_size)) +
			offset(log(nSession)) +
			(1 | mom),
		data = .SD,
		family = 'gaussian'
	)
	tidy(m)
}, by = iteration]


DT.50.lcd.11.p <-
	merge(
		DT.50.lcd.11[iteration != 0],
		DT.50.lcd.11[iteration == 0, .(effect, group, term, estimate)],
		by = c('effect', 'group', 'term'),
		suffixes = c('', '.obs')
	)


DT.50.lcd.11.pboth <- DT.50.lcd.11.p[, .(estimate = unique(estimate.obs), min = min(estimate), max = max(estimate),
																				 median = median(estimate), '5%'=quantile(estimate, probs = c(0.05), na.rm = T),'95%'=quantile(estimate, probs = c(0.95), na.rm = T),
																				 sumless = sum(estimate < estimate.obs), summore = sum(estimate > estimate.obs),
																				 pless = sum(estimate < estimate.obs)/1000, pmore = sum(estimate > estimate.obs)/1000),
																		 term]

DT.50.lcd.11.out <- DT.50.lcd.11.pboth[,.(term, estimate, corEffect = estimate - median, median, randRange= paste(round(`5%`, 3), round(`95%`, 3), sep = " to "), pmore, pless)]
DT.50.lcd.11.out[,'Prand'] <- ifelse(DT.50.lcd.11.out$estimate >= DT.50.lcd.11.out$median, DT.50.lcd.11.out$pmore, DT.50.lcd.11.out$pless)
DT.50.lcd.11.tab <- DT.50.lcd.11.out[term %in% mod.11.terms,.(term, estimate = round(estimate, 3), corEffect = round(corEffect, 3), randRange, Prand)]
DT.50.lcd.11.tab[,'network type'] <- DT.50.lcd.11.tab[,ifelse(term %like% 'sri', 'association', ifelse(term %like% 'aggr', 'aggression',
																																																			 ifelse(term %like% 'affil', 'affiliation', '')))]
DT.50.lcd.11.tab$`network type` <- factor(DT.50.lcd.11.tab$`network type`, levels = c('association', 'aggression', 'affiliation'), labels = c('association', 'aggression', 'affiliation'))
DT.50.lcd.11.tab$term <- factor(DT.50.lcd.11.tab$term, levels = mod.11.terms, labels = c('dominance rank', 'strength',
																																												 'out-strength', 'in-strength',
																																												 'out-strength', 'in-strength', 'mother (random effect sd)'))

DT.50.lcd.11.tab <- DT.50.lcd.11.tab[,.(`network type`, term, estimate, corEffect, randRange, Prand)]




#### best model for longevity DI ####

DT.50.ldi.13 <- DT[period == 'di' & nSession.obs >= 50, {
	m <- glmer(
		log(longevity_years) ~
			ego_period_rank +
			scale(sri_betweenness) +
			scale(aggr_betweenness) +
			scale(affil_betweenness) +
			offset(log(clan_size)) +
			offset(log(nSession)) +
			(1 | mom),
		data = .SD,
		family = 'gaussian'
	)
	tidy(m)
}, by = iteration]


DT.50.ldi.13.p <-
	merge(
		DT.50.ldi.13[iteration != 0],
		DT.50.ldi.13[iteration == 0, .(effect, group, term, estimate)],
		by = c('effect', 'group', 'term'),
		suffixes = c('', '.obs')
	)

DT.50.ldi.13.pboth <- DT.50.ldi.13.p[, .(estimate = unique(estimate.obs), min = min(estimate), max = max(estimate),
																				 median = median(estimate), '5%'=quantile(estimate, probs = c(0.05), na.rm = T),'95%'=quantile(estimate, probs = c(0.95), na.rm = T),
																				 sumless = sum(estimate < estimate.obs), summore = sum(estimate > estimate.obs),
																				 pless = sum(estimate < estimate.obs)/1000, pmore = sum(estimate > estimate.obs)/1000),
																		 term]

DT.50.ldi.13.out <- DT.50.ldi.13.pboth[,.(term, estimate, corEffect = estimate - median, median, randRange= paste(round(`5%`, 3), round(`95%`, 3), sep = " to "), pmore, pless)]
DT.50.ldi.13.out[,'Prand'] <- ifelse(DT.50.ldi.13.out$estimate >= DT.50.ldi.13.out$median, DT.50.ldi.13.out$pmore, DT.50.ldi.13.out$pless)
DT.50.ldi.13.tab <- DT.50.ldi.13.out[term %in% mod.13.terms,.(term, estimate = round(estimate, 3), corEffect = round(corEffect, 3), randRange, Prand)]
DT.50.ldi.13.tab[,'network type'] <- DT.50.ldi.13.tab[,ifelse(term %like% 'sri', 'association', ifelse(term %like% 'aggr', 'aggression',
																																																			 ifelse(term %like% 'affil', 'affiliation', '')))]
DT.50.ldi.13.tab$`network type` <- factor(DT.50.ldi.13.tab$`network type`, levels = c('association', 'aggression', 'affiliation'), labels = c('association', 'aggression', 'affiliation'))
DT.50.ldi.13.tab$term <- factor(DT.50.ldi.13.tab$term, levels = mod.13.terms, labels = c('dominance rank', 'betweenness',
																																												 'betweenness', 'betweenness',
																																												 'mother (random effect sd)'))

DT.50.ldi.13.tab <- DT.50.ldi.13.tab[,.(`network type`, term, estimate, corEffect, randRange, Prand)]





DT.50.ldi.7 <- DT[period == 'di' & nSession.obs >= 50, {
	m <- glmer(
		log(longevity_years) ~
			ego_period_rank +
			scale(sri_degree) +
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


DT.50.ldi.7.p <-
	merge(
		DT.50.ldi.7[iteration != 0],
		DT.50.ldi.7[iteration == 0, .(effect, group, term, estimate)],
		by = c('effect', 'group', 'term'),
		suffixes = c('', '.obs')
	)

DT.50.ldi.7.pboth <- DT.50.ldi.7.p[, .(estimate = unique(estimate.obs), min = min(estimate), max = max(estimate),
																			 median = median(estimate), '5%'=quantile(estimate, probs = c(0.05), na.rm = T),'95%'=quantile(estimate, probs = c(0.95), na.rm = T),
																			 sumless = sum(estimate < estimate.obs), summore = sum(estimate > estimate.obs),
																			 pless = sum(estimate < estimate.obs)/1000, pmore = sum(estimate > estimate.obs)/1000),
																	 term]

DT.50.ldi.7.out <- DT.50.ldi.7.pboth[,.(term, estimate, corEffect = estimate - median, median, randRange= paste(round(`5%`, 3), round(`95%`, 3), sep = " to "), pmore, pless)]
DT.50.ldi.7.out[,'Prand'] <- ifelse(DT.50.ldi.7.out$estimate >= DT.50.ldi.7.out$median, DT.50.ldi.7.out$pmore, DT.50.ldi.7.out$pless)
DT.50.ldi.7.tab <- DT.50.ldi.7.out[term %in% mod.7.terms,.(term, estimate = round(estimate, 3), corEffect = round(corEffect, 3), randRange, Prand)]
DT.50.ldi.7.tab[,'network type'] <- DT.50.ldi.7.tab[,ifelse(term %like% 'sri', 'association', ifelse(term %like% 'aggr', 'aggression',
																																																		 ifelse(term %like% 'affil', 'affiliation', '')))]
DT.50.ldi.7.tab$`network type` <- factor(DT.50.ldi.7.tab$`network type`, levels = c('association', 'aggression', 'affiliation'), labels = c('association', 'aggression', 'affiliation'))
DT.50.ldi.7.tab$term <- factor(DT.50.ldi.7.tab$term, levels = mod.7.terms, labels = c('dominance rank', 'degree',
																																											'out-degree', 'in-degree',
																																											'out-degree', 'in-degree', 'mother (random effect sd)'))

DT.50.ldi.7.tab <- DT.50.ldi.7.tab[,.(`network type`, term, estimate, corEffect, randRange, Prand)]




#### best model for longevity Adult ####

DT.50.lad.7 <- DT[period == 'adult' & nSession.obs >= 50, {
	m <- glmer(
		log(longevity_years) ~
			ego_period_rank +
			scale(sri_degree) +
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


DT.50.lad.7.p <-
	merge(
		DT.50.lad.7[iteration != 0],
		DT.50.lad.7[iteration == 0, .(effect, group, term, estimate)],
		by = c('effect', 'group', 'term'),
		suffixes = c('', '.obs')
	)


DT.50.lad.7.pboth <- DT.50.lad.7.p[, .(estimate = unique(estimate.obs), min = min(estimate), max = max(estimate),
																			 median = median(estimate), '5%'=quantile(estimate, probs = c(0.05), na.rm = T),'95%'=quantile(estimate, probs = c(0.95), na.rm = T),
																			 sumless = sum(estimate < estimate.obs), summore = sum(estimate > estimate.obs),
																			 pless = sum(estimate < estimate.obs)/1000, pmore = sum(estimate > estimate.obs)/1000),
																	 term]

DT.50.lad.7.out <- DT.50.lad.7.pboth[,.(term, estimate, corEffect = estimate - median, median, randRange= paste(round(`5%`, 3), round(`95%`, 3), sep = " to "), pmore, pless)]
DT.50.lad.7.out[,'Prand'] <- ifelse(DT.50.lad.7.out$estimate >= DT.50.lad.7.out$median, DT.50.lad.7.out$pmore, DT.50.lad.7.out$pless)
DT.50.lad.7.tab <- DT.50.lad.7.out[term %in% mod.7.terms,.(term, estimate = round(estimate, 3), corEffect = round(corEffect, 3), randRange, Prand)]
DT.50.lad.7.tab[,'network type'] <- DT.50.lad.7.tab[,ifelse(term %like% 'sri', 'association', ifelse(term %like% 'aggr', 'aggression',
																																																		 ifelse(term %like% 'affil', 'affiliation', '')))]
DT.50.lad.7.tab$`network type` <- factor(DT.50.lad.7.tab$`network type`, levels = c('association', 'aggression', 'affiliation'), labels = c('association', 'aggression', 'affiliation'))
DT.50.lad.7.tab$term <- factor(DT.50.lad.7.tab$term, levels = mod.7.terms, labels = c('dominance rank', 'degree',
																																											'out-degree', 'in-degree',
																																											'out-degree', 'in-degree', 'mother (random effect sd)'))

DT.50.lad.7.tab <- DT.50.lad.7.tab[,.(`network type`, term, estimate, corEffect, randRange, Prand)]





DT.50.lad.13 <- DT[period == 'adult' & nSession.obs >= 50, {
	m <- glmer(
		log(longevity_years) ~
			ego_period_rank +
			scale(sri_betweenness) +
			scale(aggr_betweenness) +
			scale(affil_betweenness) +
			offset(log(clan_size)) +
			offset(log(nSession)) +
			(1 | mom),
		data = .SD,
		family = 'gaussian'
	)
	tidy(m)
}, by = iteration]


DT.50.lad.13.p <-
	merge(
		DT.50.lad.13[iteration != 0],
		DT.50.lad.13[iteration == 0, .(effect, group, term, estimate)],
		by = c('effect', 'group', 'term'),
		suffixes = c('', '.obs')
	)

DT.50.lad.13.pboth <- DT.50.lad.13.p[, .(estimate = unique(estimate.obs), min = min(estimate), max = max(estimate),
																				 median = median(estimate), '5%'=quantile(estimate, probs = c(0.05), na.rm = T),'95%'=quantile(estimate, probs = c(0.95), na.rm = T),
																				 sumless = sum(estimate < estimate.obs), summore = sum(estimate > estimate.obs),
																				 pless = sum(estimate < estimate.obs)/1000, pmore = sum(estimate > estimate.obs)/1000),
																		 term]

DT.50.lad.13.out <- DT.50.lad.13.pboth[,.(term, estimate, corEffect = estimate - median, median, randRange= paste(round(`5%`, 3), round(`95%`, 3), sep = " to "), pmore, pless)]
DT.50.lad.13.out[,'Prand'] <- ifelse(DT.50.lad.13.out$estimate >= DT.50.lad.13.out$median, DT.50.lad.13.out$pmore, DT.50.lad.13.out$pless)
DT.50.lad.13.tab <- DT.50.lad.13.out[term %in% mod.13.terms,.(term, estimate = round(estimate, 3), corEffect = round(corEffect, 3), randRange, Prand)]
DT.50.lad.13.tab[,'network type'] <- DT.50.lad.13.tab[,ifelse(term %like% 'sri', 'association', ifelse(term %like% 'aggr', 'aggression',
																																																			 ifelse(term %like% 'affil', 'affiliation', '')))]
DT.50.lad.13.tab$`network type` <- factor(DT.50.lad.13.tab$`network type`, levels = c('association', 'aggression', 'affiliation'), labels = c('association', 'aggression', 'affiliation'))
DT.50.lad.13.tab$term <- factor(DT.50.lad.13.tab$term, levels = mod.13.terms, labels = c('dominance rank', 'betweenness',
																																												 'betweenness', 'betweenness',
																																												 'mother (random effect sd)'))

DT.50.lad.13.tab <- DT.50.lad.13.tab[,.(`network type`, term, estimate, corEffect, randRange, Prand)]


#### best models for ARS CD ####

DT.50.acd.7 <- DT[period == 'cd' & nSession.obs >= 50, {
	m <- glmer(
		log(annual_rs) ~
			ego_period_rank +
			scale(sri_degree) +
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


DT.50.acd.7.p <-
	merge(
		DT.50.acd.7[iteration != 0],
		DT.50.acd.7[iteration == 0, .(effect, group, term, estimate)],
		by = c('effect', 'group', 'term'),
		suffixes = c('', '.obs')
	)


DT.50.acd.7.pboth <- DT.50.acd.7.p[, .(estimate = unique(estimate.obs), min = min(estimate), max = max(estimate),
																			 median = median(estimate), '5%'=quantile(estimate, probs = c(0.05), na.rm = T),'95%'=quantile(estimate, probs = c(0.95), na.rm = T),
																			 sumless = sum(estimate < estimate.obs), summore = sum(estimate > estimate.obs),
																			 pless = sum(estimate < estimate.obs)/1000, pmore = sum(estimate > estimate.obs)/1000),
																	 term]

DT.50.acd.7.out <- DT.50.acd.7.pboth[,.(term, estimate, corEffect = estimate - median, median, randRange= paste(round(`5%`, 3), round(`95%`, 3), sep = " to "), pmore, pless)]
DT.50.acd.7.out[,'Prand'] <- ifelse(DT.50.acd.7.out$estimate >= DT.50.acd.7.out$median, DT.50.acd.7.out$pmore, DT.50.acd.7.out$pless)
DT.50.acd.7.tab <- DT.50.acd.7.out[term %in% mod.7.terms,.(term, estimate = round(estimate, 3), corEffect = round(corEffect, 3), randRange, Prand)]
DT.50.acd.7.tab[,'network type'] <- DT.50.acd.7.tab[,ifelse(term %like% 'sri', 'association', ifelse(term %like% 'aggr', 'aggression',
																																																		 ifelse(term %like% 'affil', 'affiliation', '')))]
DT.50.acd.7.tab$`network type` <- factor(DT.50.acd.7.tab$`network type`, levels = c('association', 'aggression', 'affiliation'), labels = c('association', 'aggression', 'affiliation'))
DT.50.acd.7.tab$term <- factor(DT.50.acd.7.tab$term, levels = mod.7.terms, labels = c('dominance rank', 'degree',
																																											'out-degree', 'in-degree',
																																											'out-degree', 'in-degree', 'mother (random effect sd)'))

DT.50.acd.7.tab <- DT.50.acd.7.tab[,.(`network type`, term, estimate, corEffect, randRange, Prand)]

#### best models for ARS DI ####

DT.50.adi.7 <- DT[period == 'di' & nSession.obs >= 50, {
	m <- glmer(
		log(annual_rs) ~
			ego_period_rank +
			scale(sri_degree) +
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


DT.50.adi.7.p <-
	merge(
		DT.50.adi.7[iteration != 0],
		DT.50.adi.7[iteration == 0, .(effect, group, term, estimate)],
		by = c('effect', 'group', 'term'),
		suffixes = c('', '.obs')
	)


DT.50.adi.7.pboth <- DT.50.adi.7.p[, .(estimate = unique(estimate.obs), min = min(estimate), max = max(estimate),
																			 median = median(estimate), '5%'=quantile(estimate, probs = c(0.05), na.rm = T),'95%'=quantile(estimate, probs = c(0.95), na.rm = T),
																			 sumless = sum(estimate < estimate.obs), summore = sum(estimate > estimate.obs),
																			 pless = sum(estimate < estimate.obs)/1000, pmore = sum(estimate > estimate.obs)/1000),
																	 term]

DT.50.adi.7.out <- DT.50.adi.7.pboth[,.(term, estimate, corEffect = estimate - median, median, randRange= paste(round(`5%`, 3), round(`95%`, 3), sep = " to "), pmore, pless)]
DT.50.adi.7.out[,'Prand'] <- ifelse(DT.50.adi.7.out$estimate >= DT.50.adi.7.out$median, DT.50.adi.7.out$pmore, DT.50.adi.7.out$pless)
DT.50.adi.7.tab <- DT.50.adi.7.out[term %in% mod.7.terms,.(term, estimate = round(estimate, 3), corEffect = round(corEffect, 3), randRange, Prand)]
DT.50.adi.7.tab[,'network type'] <- DT.50.adi.7.tab[,ifelse(term %like% 'sri', 'association', ifelse(term %like% 'aggr', 'aggression',
																																																		 ifelse(term %like% 'affil', 'affiliation', '')))]
DT.50.adi.7.tab$`network type` <- factor(DT.50.adi.7.tab$`network type`, levels = c('association', 'aggression', 'affiliation'), labels = c('association', 'aggression', 'affiliation'))
DT.50.adi.7.tab$term <- factor(DT.50.adi.7.tab$term, levels = mod.7.terms, labels = c('dominance rank', 'degree',
																																											'out-degree', 'in-degree',
																																											'out-degree', 'in-degree', 'mother (random effect sd)'))

DT.50.adi.7.tab <- DT.50.adi.7.tab[,.(`network type`, term, estimate, corEffect, randRange, Prand)]


#### best models for ARS Adult ####

DT.50.aad.7 <- DT[period == 'adult' & nSession.obs >= 50, {
	m <- glmer(
		log(annual_rs) ~
			ego_period_rank +
			scale(sri_degree) +
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


DT.50.aad.7.p <-
	merge(
		DT.50.aad.7[iteration != 0],
		DT.50.aad.7[iteration == 0, .(effect, group, term, estimate)],
		by = c('effect', 'group', 'term'),
		suffixes = c('', '.obs')
	)

DT.50.aad.7.pboth <- DT.50.aad.7.p[, .(estimate = unique(estimate.obs), min = min(estimate), max = max(estimate),
																			 median = median(estimate), '5%'=quantile(estimate, probs = c(0.05), na.rm = T),'95%'=quantile(estimate, probs = c(0.95), na.rm = T),
																			 sumless = sum(estimate < estimate.obs), summore = sum(estimate > estimate.obs),
																			 pless = sum(estimate < estimate.obs)/1000, pmore = sum(estimate > estimate.obs)/1000),
																	 term]

DT.50.aad.7.out <- DT.50.aad.7.pboth[,.(term, estimate, corEffect = estimate - median, median, randRange= paste(round(`5%`, 3), round(`95%`, 3), sep = " to "), pmore, pless)]
DT.50.aad.7.out[,'Prand'] <- ifelse(DT.50.aad.7.out$estimate >= DT.50.aad.7.out$median, DT.50.aad.7.out$pmore, DT.50.aad.7.out$pless)
DT.50.aad.7.tab <- DT.50.aad.7.out[term %in% mod.7.terms,.(term, estimate = round(estimate, 3), corEffect = round(corEffect, 3), randRange, Prand)]
DT.50.aad.7.tab[,'network type'] <- DT.50.aad.7.tab[,ifelse(term %like% 'sri', 'association', ifelse(term %like% 'aggr', 'aggression',
																																																		 ifelse(term %like% 'affil', 'affiliation', '')))]
DT.50.aad.7.tab$`network type` <- factor(DT.50.aad.7.tab$`network type`, levels = c('association', 'aggression', 'affiliation'), labels = c('association', 'aggression', 'affiliation'))
DT.50.aad.7.tab$term <- factor(DT.50.aad.7.tab$term, levels = mod.7.terms, labels = c('dominance rank', 'degree',
																																											'out-degree', 'in-degree',
																																											'out-degree', 'in-degree', 'mother (random effect sd)'))

DT.50.aad.7.tab <- DT.50.aad.7.tab[,.(`network type`, term, estimate, corEffect, randRange, Prand)]


