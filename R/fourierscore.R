fourierscore <-
  function(eset, T, times=seq_len(ncol(eset)))
{
    et <- t(exprs(eset))
    p <- 2 * pi / T * times
    sqrt(colSums(cos(p) * et) ^ 2 + colSums(sin(p) * et) ^ 2)
}
