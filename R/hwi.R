gbi <- get_gbi(DT[id <= 5], 'group', 'id')

lapply(seq_len(ncol(gbi)), function(i) {
	m <- matrix(rep(gbi[, i], ncol(gbi)), ncol = ncol(gbi))
	x <- apply(m + gbi, 2, function(x) sum(x == 2))
	yb <- apply(m - gbi, 2, function(x) sum(x == -1))
	ya <- apply(m - gbi, 2, function(x) sum(x == 1))
	yab <- ya + yb

	# x/(x + 0.5 * yab)
	x/((0.5*(ya+yb)) + yab + x)
})
