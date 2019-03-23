### Makefile
# Alec Robitaille

der = data/derived-data
raw = data/raw-data

# 4 - Aggression
aggression-metrics.Rds: R/2-Hyena-Aggression.R $(der)/prep-aggression.Rds
	Rscript "R/2-Hyena-Aggression.R"

# 3 - Affiliation
affiliation-metrics.Rds: R/2-Hyena-Affiliation.R $(der)/prep-affiliation.Rds
	Rscript "R/2-Hyena-Affiliation.R"

# 2 - Association
association-metrics.Rds: R/2-Hyena-Association.R $(der)/prep-association.Rds
	Rscript "R/2-Hyena-Association.R"

# 1 - Prep
T1 := $(der)/prep-aggression.Rds $(der)/prep-affiliation.Rds $(der)/prep-association.Rds $(der)/filtered-ego-lifestages.Rds
$(T1): R/1-PrepData.R
	Rscript "R/1-PrepData.R"
