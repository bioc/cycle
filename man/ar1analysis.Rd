\name{ar1analysis}
\alias{ar1analysis}
\title{Performs AR1 fitting}
\description{Calculation of  the autocorrelation coefficients 
genes  and
 variance of corresponding random variables  
to fit gene expression time series by  AR1 processes }

  \usage{ar1analysis(eset)}
  \arguments{\item{eset}{object of the class \dQuote{ExpressionSet}}
    }
\value{List of fitted autocorrelation coefficients (\code{alpha})
for ExpressionSet features  and  variance (\code{sigma2})
 of corresponding random variables obtained using the \code{\link[stats]{ar}} function
of the \emph{stats} package.}

\note{Note that this function evaluates soley the \code{exprs} matrix and 
no information is used from the \code{phenoData}. In particular, 
the ordering of samples (arrays) is the same as the ordering 
of the columns in the \code{exprs} matrix. Also, replicated arrays in the 
\code{exprs} matrix are treated as independent 
i.e. they should be averagered prior to analysis or placed into different
distinct \dQuote{ExpressionSet} objects.}

\seealso{ \code{\link[stats]{ar}}}

\examples{
data(yeast) # loading the reduced CDC28 yeast set (from the Mfuzz package)

# Data preprocessing 
if (interactive()){
data(yeast) 

yeast <- filter.NA(yeast) 
# filters genes with more than 25% of the expression values missing 

yeast  <- fill.NA(yeast) 
# for illustration only; rather use knn method for replacing missing values

tmp <- ar1analysis(yeast) 
# fits AR1 process autocorrelation coefficients  

plot(density(tmp$alpha),main="Autocorrelation")

}
}
\keyword{ts}
