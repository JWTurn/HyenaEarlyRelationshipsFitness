#### Graphs to publish ####
# Julie Turner
# 13 Aug. 2019

### Packages ----
# remotes::install_github('bbolker/broom.mixed')
libs <- c('data.table', 'ggplot2')
lapply(libs, require, character.only = TRUE)


### Input data ----
raw <- 'data/raw-data/'
derived <- 'data/derived-data/'

egos <- fread(paste0(raw, 'egos_filtered.csv'))

mets <- readRDS(paste0(derived, 'observed-random-metrics_all.Rds'))

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
DT[, 6:24][is.na(DT[, 6:24])] <- 0
DT[,'alone'] <- DT$nAlone/DT$nSession

DT.obs <- DT[iteration == 0]


data.f.long.cd <- DT.obs[period=='cd' & !is.na(longevity_years)]

par(mfrow=c(1,1))
ll.indeg.cd<-lm(log(longevity_years)~affil_indegree, data.f.long.cd)
range(data.f.long.cd$affil_indegree)
xllindeg.cd <- seq(0, 30, 0.02)
yllindeg.cd <- predict(ll.indeg.cd, list(affil_indegree = xllindeg.cd), type = 'response', interval = 'confidence', level = 0.95)
plot(log(longevity_years)~affil_indegree, data.f.long.cd)
lines(xllindeg.cd, yllindeg.cd[,1], lwd = 4)
lines(xllindeg.cd, yllindeg.cd[,2], lty = 2)
lines(xllindeg.cd, yllindeg.cd[,3], lty = 2)

data.f.long.cd.noindegol<- subset(data.f.long.cd, affil_indegree<25)
ll.indeg.cd.nol<-lm(log(longevity_years)~affil_indegree, data.f.long.cd.noindegol)
range(data.f.long.cd.noindegol$affil_indegree)
xllindeg.cd.nol <- seq(0, 14, 0.02)
yllindeg.cd.nol <- predict(ll.indeg.cd.nol, list(affil_indegree = xllindeg.cd.nol), type = 'response', interval = 'confidence', level = 0.95)

setEPS()
postscript("Fig1.eps")
plot(log(longevity_years)~affil_indegree, data.f.long.cd.noindegol, ylab = 'Log (Longevity in years)', xlab = 'Affiliation in-degree', bty= 'l')
lines(xllindeg.cd.nol, yllindeg.cd.nol[,1], lwd = 4)
# lines(xllindeg.cd.nol, yllindeg.cd.nol[,2], lty = 2)
# lines(xllindeg.cd, yllindeg.cd[,3], lty = 2)
dev.off()




