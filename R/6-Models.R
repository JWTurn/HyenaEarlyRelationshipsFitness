### Models ====
# Alec Robitaille
# Started: March 28 2019


### Packages ----
libs <- c('lme4', 'data.table', 'ggplot2')
lapply(libs, require, character.only = TRUE)


### Input data ----
derived <- 'data/derived-data/'
DT <- readRDS(paste0(derived, 'observed-random-metrics.Rds'))

assocols <- colnames(DT)[grepl('twi', colnames(DT))]
affilcols <- colnames(DT)[grepl('affil', colnames(DT))]
aggrcols <- colnames(DT)[grepl('aggr', colnames(DT))]


### Histograms ----
# pdf('graphics/association-histograms.pdf')
lapply(assocols, function(col) {
	ggplot(DT) +
		geom_histogram(aes_string(col), binwidth = 1)
})
# dev.off()


### Models ----
## best models for longevity CD
l.cd.19 <- glmer(log(longevity_years) ~ ais_str_scl
								 + agg_str_scl
								 + ll_str_scl
								 + ego_period_rank + offset(log(clan_size)) + offset(log(sessions_count)) + (1|mom),
								 data=data.f.long.cd, family = 'gaussian')

# dAIC 0.6
l.cd.7 <- glmer(log(longevity_years) ~  ais_deg_scl
								+ agg_outdeg_scl + agg_indeg_scl
								+ ll_outdeg_scl + ll_indeg_scl
								+ ego_period_rank + offset(log(clan_size)) + offset(log(sessions_count)) + (1|mom),
								data=data.f.long.cd, family = 'gaussian')



## best model for longevity DI
l.pg.18 <- glmer(log(longevity_years) ~ ais_deg_scl
								 + agg_deg_scl
								 + ll_deg_scl
								 + ego_period_rank + offset(log(clan_size)) + offset(log(sessions_count)) + (1|mom),
								 data=data.f.long.pg, family = 'gaussian')



## best model for longevity Adult
l.ad.18 <- glmer(log(longevity_years) ~ ais_deg_scl
								 + agg_deg_scl
								 + ll_deg_scl
								 + ego_period_rank + offset(log(clan_size)) + offset(log(sessions_count)) + (1|mom),
								 data=data.f.long.ad, family = 'gaussian')




## best models for ARS CD
a.cd.18 <- glmer(log(annual_rs) ~ ais_deg_scl
								 + agg_deg_scl
								 + ll_deg_scl
								 + ego_period_rank + offset(log(clan_size)) + offset(log(sessions_count)) + (1|mom),
								 data=data.f.ars.cd, family = 'gaussian')

# dAIC = 0.5
a.cd.11 <- glmer(log(annual_rs) ~ alone_scl + ais_deg_scl
								 + agg_deg_scl
								 + ll_deg_scl
								 + ego_period_rank + offset(log(clan_size)) + offset(log(sessions_count)) + (1|mom),
								 data=data.f.ars.cd, family = 'gaussian')



## best models for ARS DI
a.pg.19 <- glmer(log(annual_rs) ~ ais_str_scl
								 + agg_str_scl
								 + ll_str_scl
								 + ego_period_rank + offset(log(clan_size)) + offset(log(sessions_count)) + (1|mom),
								 data=data.f.ars.pg, family = 'gaussian')

# dAIC = 1.1
a.pg.18 <- glmer(log(annual_rs) ~ ais_deg_scl
								 + agg_deg_scl
								 + ll_deg_scl
								 + ego_period_rank + offset(log(clan_size)) + offset(log(sessions_count)) + (1|mom),
								 data=data.f.ars.pg, family = 'gaussian')



## best models for ARS Adult
a.ad.19 <- glmer(log(annual_rs) ~ ais_str_scl
								 + agg_str_scl
								 + ll_str_scl
								 + ego_period_rank + offset(log(clan_size)) + offset(log(sessions_count)) + (1|mom),
								 data=data.f.ars.ad, family = 'gaussian')

# dAIC = 1.2
a.ad.18 <- glmer(log(annual_rs) ~ ais_deg_scl
								 + agg_deg_scl
								 + ll_deg_scl
								 + ego_period_rank + offset(log(clan_size)) + offset(log(sessions_count)) + (1|mom),
								 data=data.f.ars.ad, family = 'gaussian')
