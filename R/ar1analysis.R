ar1analysis <-
    function(eset)
{
    r <- apply(exprs(eset), 1, ar, order=1, aic=FALSE)
    list(alpha=sapply(r, "[[", 2), sigma2=sapply(r, "[[", 3))
}



