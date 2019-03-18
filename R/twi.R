twi <- function(gbi, times) {
	## If we assume that all sessions are temporally distinct
	# 	then yab is 0
	#   ya is all times where a is observed and not b
	#   yb is all times where b is observed and not a
	#   x is all times where a and b are observed associating
	if (missing(times)) {
		yab <- 0
		out <- do.call(rbind,
									 lapply(seq_len(ncol(gbi)), function(i) {
									 	m <- matrix(rep(gbi[, i], ncol(gbi)), ncol = ncol(gbi))
									 	x <- apply(m + gbi, 2, function(x)
									 		sum(x == 2))
									 	ya <- apply(m - gbi, 2, function(x)
									 		sum(x == 1))
									 	yb <- apply(m - gbi, 2, function(x)
									 		sum(x == -1))
									 	x / (x + (2 * yab) + ya + yb)
									 }))
		dimnames(out) <- list(colnames(gbi), colnames(gbi))
		out
	}
}
