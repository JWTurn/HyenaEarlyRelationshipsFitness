# X: number of times a and b were observed together
# Ya: number of times individual a was observed and individual b was not observed
# Yb: swap a for b above
# Yab: number of times both individuals were observed but in different groups

library(hwig)
library(spatsoc)
library(data.table)
data(DT)
gbi <- get_gbi(DT[id <= 5], 'group', 'id')

## If we assume that all groups are temporally distinct
# 	then yab is 0
gbi <- gbi[1:5,]
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
						yab <- ya + yb

						# x/(x + 0.5 * yab)
						x / ((0.5 * (ya + yb)) + yab + x)
					}))
}




# hwi <- function(gbi, time)
do.call(rbind,
lapply(seq_len(ncol(gbi)), function(i) {
	m <- matrix(rep(gbi[, i], ncol(gbi)), ncol = ncol(gbi))
	x <- apply(m + gbi, 2, function(x) sum(x == 2))
	ya <- apply(m - gbi, 2, function(x) sum(x == 1))
	yb <- apply(m - gbi, 2, function(x) sum(x == -1))
	yab <- ya + yb

	# x/(x + 0.5 * yab)
	x / ((0.5 * (ya + yb)) + yab + x)
})
)



##

asso[, t := .GRP, .(sessiondate, session)]
asso[, group := .GRP, session]
DT <- asso[year(sessiondate) == 1988][hyena %chin% DT[, unique(hyena)[1:5]]]
DT
gbi <- get_gbi(DT, 'group', 'hyena')
asnipe::get_network(gbi, 'GBI', 'HWI', times = seq(1:69))
