\name{EpicPre}
\alias{EpicPre}
\title{
Pre-processing of droplet-based scRNA-seq data
}
\description{
EpicPre performs pre-processing of count matrices built by dropEst.

EpicPreMN contains defaults for Macaca nemestrina-derived data. Due to the poor annotation of this genome, the threshold for mitochondrial read percentages must be much lower than for other species, and cells cannot be filtered based on the percentage of rRNA reads.

EpicPreHS contains defaults for Homo sapiens-derived data.
}
\usage{
EpicPreMN(cm_name, min.counts = 1e3, max.counts = 15e3, max.complexity = 0.75, max.percent.mito = 0.001)

EpicPreHS <- function(cm_name, min.counts = 1e3, max.counts = 15e3, max.complexity = 0.75, max.percent.mito = 0.2, max.percent.rRNA = 0.2)
}

\arguments{
  \item{cm_name}{
The RDS output of dropEst. NOTE: this file name must in with ".cm" to ensure proper transferring of cell names. Additionally, this code parses and processes dropEst objects that must contain exonic, intronic, and spanning count matrices, created using the -V flag.
}
  \item{min.counts}{
  The minimum number of UMIs per cell. Defaults to 1e3.
}
  \item{max.counts}{
  The maximum number of UMIs per cell. Defaults to 15e3.
}
  \item{max.complexity}{
  This estimates complexity by dividing the number of genes per cell by the number of UMIs in that cell (in order to filter out doublets). Defaults to 0.75.
}
  \item{max.percent.mito}{
  The maximum percentage of mitochondrial reads per cell. Defaults to 0.2 for human and 0.001 for M. nemestrina.
}
  \item{max.percent.rRNA}{
  The maximum percentage of rRNA reads per cell, based on RNA18S5 and RNA28S5. Defaults to 0.2 for human only.
}
}

\value{
Returns a list of 3 containing processed, emat, nmat, and smat sparse matrices. Also prints histograms showing distributions of complexity, mitochondrial reads, and rRNA reads (if applicable) to the console.
}

\author{
Aaron J. Wilk
}

\examples{
EpicPreMN(late_w00.cm, min.counts = 1e3, max.counts = 15e3, max.complexity = 0.75, max.percent.mito = 0.001)
EpicPreHS(NK_only.cm, min.counts = 1e3, max.counts = 15e3, max.complexity = 0.75, max.percent.mito = 0.2, max.percent.rRNA = 0.2)

\keyword{ ~pre-processing }% use one of  RShowDoc("KEYWORDS")
