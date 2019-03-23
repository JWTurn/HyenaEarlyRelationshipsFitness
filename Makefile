### Makefile
# Alec Robitaille

der = data/derived-data
raw = data/raw-data

all: aggression-metrics.Rds affiliation-metrics.Rds association-metrics.Rds
	echo "complete"


# 4 - Aggression
aggression-metrics.Rds: R/4-Hyena-Aggression.R $(der)/prep-aggression.Rds
	Rscript "R/4-Hyena-Aggression.R"

# 3 - Affiliation
affiliation-metrics.Rds: R/3-Hyena-Affiliation.R $(der)/prep-affiliation.Rds
	Rscript "R/3-Hyena-Affiliation.R"

# 2 - Association
association-metrics.Rds: R/2-Hyena-Association.R $(der)/prep-association.Rds
	Rscript "R/2-Hyena-Association.R"

# 1 - Prep
T1 := $(der)/prep-aggression.Rds $(der)/prep-affiliation.Rds $(der)/prep-association.Rds $(der)/filtered-ego-lifestages.Rds
$(T1): R/1-PrepData.R
	Rscript "R/1-PrepData.R"
