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
#> R/.fyinterest.R:26:# TODO: why nulls in sessiondate?
#> R/.fyinterest.R:38:  #TODO: what association index?
#> R/.fyinterest.R:48:  #TODO: which network metrics?
#> R/5-Association-Randomization.R:61:#TODO: what kind of association index?
#> R/5-Association-Randomization.R:70:  # TODO: Are these the network metrics you want? Add them here...
```
