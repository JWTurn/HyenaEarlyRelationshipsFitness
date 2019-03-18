# X: number of times a and b were observed together
# Ya: number of times individual a was observed and individual b was not observed
# Yb: swap a for b above
# Yab: number of times both individuals were observed but in different groups

library(hwig)
library(spatsoc)
library(data.table)
data(DT)

edges <- readRDS('data/derived-data/single-focal-life-stage.Rds')
edges


get_gbi_t <- function(DT = NULL, group = "group", id = NULL, timegroup = NULL){
	uDT <- stats::na.omit(unique(DT[, .SD, .SDcols = c(group,
																										 id)]), cols = group)
	cDT <-
		data.table::dcast(
			uDT,
			formula = stats::reformulate(id, c(group, timegroup)),
			fun.aggregate = length,
			value.var = group
		)
	ids <- colnames(cDT)[!grepl(group, colnames(cDT))]
	m <- as.matrix(cDT[, .SD, .SDcols = ids])
	rownames(m) <- cDT[[group]]
	return(m)
}

gbi <- get_gbi(edges, 'group', 'hyena')
gbit <- get_gbi_t(edges, 'group', 'hyena', 'session')

gbi <- gbi[1:5,]


# spread a 1 in a timegroup, to all rows of that timegroup

hwi <- function(gbi, times) {
	if (missing(times)) {
		do.call(rbind,
						lapply(seq_len(ncol(gbi)), function(i) {
							m <- matrix(rep(gbi[, i], ncol(gbi)), ncol = ncol(gbi))
							x <- apply(m + gbi, 2, function(x)
								sum(x == 2))
							ya <- apply(m - gbi, 2, function(x)
								sum(x == 1))
							yb <- apply(m - gbi, 2, function(x)
								sum(x == -1))
							yab <- 0

							x / ((0.5 * (ya + yb)) + yab + x)
						}))
	}
}
