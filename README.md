# EpicTools
Tools for pre-processing of count matrices for droplet-based scRNA-seq analysis

This package contains two functions that process scRNA-seq count matrices before initialization of a Seurat object for post-processing. 
1. EpicMerge rapidly and iteratively merges up to 12 sparse matrices in a single command. 
2. EpicPre filters and parses layered exonic, intronic, and spanning count matrices. 

# Installation
```
devtools::install_github('ajwilk/EpicTools')
```

# Usage
The ```EpicMerge()``` function takes up to 12 dgCMatrix or other matrix-like objects as arguments
```
EpicMerge(x, y, z)
```

```EpicPre()``` filters count matrices built by dropEst by the number of UMIs per cell, complexity per cell (the number of genes divided by the number of UMIs), and the percentage of mitochondrial and rRNA (if applicable) reads. There are two ```EpicPre()``` functions that come with different defaults for filtering metrics. ```EpicPreHS()``` is designed for processing human scRNA-seq data, and ```EpicPreMN()``` for M. nemestrina data: 
```
EpicPreHS <- function(cm_name, min.counts = 1e3, max.counts = 15e3, max.complexity = 0.75, max.percent.mito = 0.2, max.percent.rRNA = 0.2)
EpicPreMN(cm_name, min.counts = 1e3, max.counts = 15e3, max.complexity = 0.75, max.percent.mito = 0.001)
```
Note that the input to ```EpicPre()``` must be named to end with ".cm" to ensure proper transferring of cell names. Additionally, this code parses and processes dropEst objects that must contain exonic, intronic, and spanning count matrices, created using the -V flag.
