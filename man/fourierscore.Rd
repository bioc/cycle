\name{fourierscore}
\alias{fourierscore}
\title{Calculation of the Fourier  score}
\description{The function calculates fourier score for a chosen periodicity.}
\usage{fourierscore(eset,T,times=seq_len(ncol(eset)))}
\arguments{\item{eset}{object of the class \dQuote{ExpressionSet}}
  \item{T}{length of chosen period}
  \item{times}{measurement times (with the index of the column as default)}
 }

   
\details{The Fourier score can be used to detect periodic signals. 
The closer a gene's expression follows a (possibly shifted) cosine curve of period T, the larger is the Fourier score. Mathematical details can be found in the given reference. The function \code{fourierscore} calculates the Fourier scores for all features of an ExpressionSet object. }

\value{Fourier scores for all features (genes) of \code{eset}}

\note{Note that this function evaluates soley the \code{exprs} matrix and 
no information is used from the \code{phenoData}. In particular, 
the ordering of samples (arrays) is the same as the ordering 
of the columns in the \code{exprs} matrix. Also, replicated arrays in the 
\code{exprs} matrix are treated as independent 
i.e. they should be averagered prior to analysis or placed into different
distinct \dQuote{ExpressionSet} objects.}

\author{Matthias E. Futschik (\url{http://www.cbme.ualg.pt/mfutschik_cbme.html})}

\references{Matthias E. Futschik and Hanspeter Herzel (2008) Are we overestimating the number of cell-cycling genes? The impact of background models on time series analysis, \emph{Bioinformatics}, 24(8):1063-1069
}
\examples{
# Data preprocessing 
if (interactive()){
set.seed(1)
data(yeast)  # loading the reduced CDC28 yeast set (from the Mfuzz package)
yeast <- yeast[1:600,]  # for illustration 
yeast <- filter.NA(yeast) # filters genes with more than 25% of the expression values missing 
yeast  <- fill.NA(yeast) # for illustration only; rather use knn method for 
yeast <- standardise(yeast)

T.yeast <- 85   # cell cycle period (t=85min)
times.yeast <-  pData(yeast)$time  # time of measurements

F <- fourierscore(yeast,T = T.yeast, times= times.yeast) 
# calculates the Fourier scores for yeast genes 

# Plot highest scoring gene
plot(times.yeast,exprs(yeast)[order(-F)[1],],type="o",
     xlab="Time",ylab="Expression",
     main=featureNames(yeast)[order(-F)[1]])

# Plot lowest scoring gene
plot(times.yeast,exprs(yeast)[order(F)[1],],type="o",
     xlab="Time",ylab="Expression",
     main=featureNames(yeast)[order(F)[1]])
}
}


\keyword{ts}
