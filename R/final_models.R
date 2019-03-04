#### final models ####

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
