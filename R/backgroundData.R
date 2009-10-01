backgroundData <-
    function(eset,model=c("rr", "gauss", "ar1"))
{
    model <- match.arg(model)
    esetB <- eset
    data <- exprs(eset)
    nrow <- nrow(data)
    ncol <- ncol(data)

    dataB <- matrix(NA, ncol=ncol, nrow=nrow)

    if (model=="rr")
        ## RANDOMISATION OF ROWS
        dataB <- t(apply(data, 1, sample))

    if (model=="gauss"){
        ## GAUSSIAN PROCESS AR(0)
        means <- rowMeans(data, na.rm=TRUE)
        sds <- apply(data, 1, sd, na.rm=TRUE)
        dataB <- matrix(rnorm(length(data), means, sds),
                        nrow=nrow)
    }

    if (model=="ar1"){
        ## AR(1) BACKGROUND
        a <- t(apply(data, 1, function(x, ...) {
            unlist(ar(x, ...)[1:4])
        }, order=1, aic=FALSE))
        sd <- sqrt(a[,3])
        r <- a[,4] + matrix(rnorm(nrow * (ncol - 1), 0, sd),
                            nrow=nrow)
        dataB[,1] <- data[,1]
        for (j in 2:ncol){
            dataB[,j] <- r[,j-1] + a[,2] * (dataB[,j-1] - a[,4])
          }
      }
    exprs(esetB) <-  dataB
    esetB
}
