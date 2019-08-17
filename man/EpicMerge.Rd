\name{EpicMerge}
\alias{EpicMerge}
\title{
Fast and iterative merging of sparse matrices
}
\description{
Using the Matrix.utils package, EpicMerge iteratively merges up to 12 dgCMatrix objects in a single command.
}
\usage{
EpicMerge(x, ...)
}
\arguments{
  \item{x}{
A matrix of matrix-like object
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
EpicMerge(emat1, emat2, emat3)
}

\keyword{ ~merge }% use one of  RShowDoc("KEYWORDS")
