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
#> R/.fyinterest.R:26:# TODO: why nulls in sessiondate?
#> R/.fyinterest.R:38:  #TODO: what association index?
#> R/.fyinterest.R:48:  #TODO: which network metrics?
#> R/5-Association-Randomization.R:91:# TODO: use all AND unique to gather all individuals in each session, from association, affiliation and aggression
#> R/5-Association-Randomization.R:92:# TODO: fill with NAs wherever repeated (careful with direction and repeated affil/aggressions)
#> R/5-Association-Randomization.R:93:# TODO: randomize both columns of the directed networks
#> R/egonet_rand.R:228:  # note this is inefficient and doubles memory requirement, but only ~ 100,000 datapoints right now
#> R/egonet_rand.R:244:  # note the merge command takes just two dfs as a time
#> R/5-Association-Randomization.R:77:# NOTE: there are 75 sessions with mismatching sessiondates, and some mismatching years
```
