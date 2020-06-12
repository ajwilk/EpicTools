\name{mergeCM}
\alias{mergeCM}
\title{
Fast and iterative merging of sparse matrices
}
\description{
mergeCM accepts a list of up to 48 count matrices and uses EpicMerge to merge them into one final count matrix.
}
\usage{
mergeCM(count_matrix.list, type = c("emat", "nmat"))
}
\arguments{
  \item{x}{
A list of dgCMatrix objects
}
  \item{type}{
  Character indicating which type of count matrices to merge.
  }
}
\details{
This function will keep all colnames and rownames present in the input matrices and fill blank measurements with zeros.
}
\value{
Returns a merged dgCMatrix.
}
\author{
Aaron J. Wilk
}
\examples{
mergeCM(count_matrix.list, type = c("emat", "nmat"))
}

\keyword{ ~merge }% use one of  RShowDoc("KEYWORDS")
