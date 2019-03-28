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

mets <- readRDS(paste0(derived, 'observed-random-metrics.Rds'))

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

### Models ----
## best models for longevity CD
DT[, {
	m <- glmer(
		log(longevity_years) ~
			scale(twi_strength) +
			scale(aggr_strength) +
			scale(affil_strength) +
			ego_period_rank +
			offset(log(clan_size)) +
			offset(log(nSession)) +
			(1 | mom),
		data = .SD,
		family = 'gaussian'
	)
	tidy(m)
}, by = iteration]


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
