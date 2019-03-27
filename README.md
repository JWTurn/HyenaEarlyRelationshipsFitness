hyena-spatsoc
================
Alec Robitaille

## Project Structure

    .
    ├── data
    │   ├── derived-data
    │   │   └── mean-mets-association.Rds
    │   └── raw-data
    │       ├── data_affiliations.csv
    │       ├── data_aggressions.csv
    │       ├── data_associations.csv
    │       ├── data_egos.csv
    │       ├── egos.csv
    │       ├── lifeperiod.csv
    │       ├── lifeperiods_aggressions_el.csv
    │       ├── lifeperiods_ais_el.csv
    │       ├── lifeperiods_clanlists.csv
    │       ├── lifeperiods.csv
    │       ├── lifeperiods_ll_el.csv
    │       └── SNfitnessData_2019-03-04.csv
    ├── lit
    │   ├── 1-s2.0-S0003347219300119-main.pdf
    │   └── BU-902-M.pdf
    ├── R
    │   ├── 1-Hyena-Association.R
    │   ├── 2-Hyena-Affiliation.R
    │   ├── 3-Hyena-Aggression.R
    │   ├── egonet_rand.R
    │   └── final_models.R
    ├── hyena-spatsoc.Rproj
    ├── README.md
    └── README.Rmd

## TODOs

``` bash
grep -rni 'R/' -e 'TODO';
grep -rni 'R/' -e 'NOTE';
#> R/5-Association-Randomization.R:101: # TODO: add aggression
#> R/5-Association-Randomization.R:124:     # TODO: add aggression
#> R/5-Association-Randomization.R:137:     # TODO: add aggression
#> R/egonet_rand.R:228:  # note this is inefficient and doubles memory requirement, but only ~ 100,000 datapoints right now
#> R/egonet_rand.R:244:  # note the merge command takes just two dfs as a time
```
