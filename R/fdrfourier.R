fdrfourier <-
    function(eset, T, times, background.model="rr", N=100,
             progress=FALSE)
{
    eset <- standardise(eset)

    if (progress)
        message("Calculation of Fourier scores")
    F <- fourierscore(eset, T=T, times=times)

    if (progress)
        message("Generating background distribution")
    f <- function(eset, T, times) {
        eset.b <- backgroundData(eset, model=background.model)
        fourierscore(standardise(eset.b), T=T, times=times)
    }
    F.b <- replicate(N, f(eset, T, times))

    if (progress)
        message("Calculation of significance")
    scale <- length(F)/length(F.b)
    fdr <- sapply(F, function(F.i) {
        sum(F.b >= F.i) / sum(F >= F.i)
    }) * scale

    names(fdr)  <- names(F) <- featureNames(eset)
    list(fdr=fdr, F = F, F.b = F.b)
}

