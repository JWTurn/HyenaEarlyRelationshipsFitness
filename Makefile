### Makefile
# Alec Robitaille

#  (all is a build target that is not a filename)
.PHONY: all

# Assign variables
der = data/derived-data
raw = data/raw-data

# Fake
all: $(der)/aggression-metrics.Rds $(der)/affiliation-metrics.Rds $(der)/association-metrics.Rds
	echo "complete"

# 4 - Aggression
$(der)/aggression-metrics.Rds: R/4-Hyena-Aggression.R $(der)/prep-aggression.Rds
	Rscript "R/4-Hyena-Aggression.R"

# 3 - Affiliation
$(der)/affiliation-metrics.Rds: R/3-Hyena-Affiliation.R $(der)/prep-affiliation.Rds
	Rscript "R/3-Hyena-Affiliation.R"

# 2 - Association
$(der)/association-metrics.Rds: R/2-Hyena-Association.R $(der)/prep-association.Rds
	Rscript "R/2-Hyena-Association.R"

# 1 - Prep
T1 := $(der)/prep-aggression.Rds $(der)/prep-affiliation.Rds $(der)/prep-association.Rds $(der)/filtered-ego-lifestages.Rds
$(T1): R/1-PrepData.R
	Rscript "R/1-PrepData.R"
