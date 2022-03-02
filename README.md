# EpicTools

NOTE: this repository is no longer actively maintained

Tools for pre-processing of count matrices for droplet-based scRNA-seq analysis

This package contains several functions that process scRNA-seq count matrices built by dropEst before initialization of a Seurat object for post-processing. 
1. EpicPre filters and parses layered exonic, intronic, and spanning count matrices. 
2. mergeCM rapidly and iteratively merges up to 48 sparse matrices in a single command. 


# Installation
```
devtools::install_github('ajwilk/EpicTools')
```

# Usage
The output of dropEst -V (https://github.com/hms-dbmi/dropEst) is an .rds file that contains 3 dgCMatrix objects (exon/intron/spanning) for a given scRNA-seq sample. 

First, read in the count matrix files you wish to pre-process: 
```
path = "path_to_dropEst_output/"
cm.list = paste0(path, list.files(pattern = "*.matrices.rds", path = path))
cm.files <- lapply(cm.list, readRDS)
names(cm.files) <- sub("\\_cell.counts.matrices.rds", "", list.files(pattern = "*.matrices.rds", path = path))
```
The count matrices are now stored as a list that can be processed by ```EpicPre()```, which filters count matrices built by dropEst by the number of UMIs per cell, complexity per cell (the number of genes divided by the number of UMIs), and the percentage of mitochondrial and rRNA (if applicable) reads. There are two ```EpicPre()``` functions that come with different defaults for filtering metrics. ```EpicPreHS()``` is designed for processing human scRNA-seq data, and ```EpicPreMN()``` for M. nemestrina data. 

```EpicPre()``` will also print several plots to help you QC each sample and ensure that the filtering thresholds are set appropriately. Defaults are shown below: 

```
EpicPreHS(cm_name, min.counts = 1e3, max.counts = 15e3, max.complexity = 0.75, max.percent.mito = 0.2, max.percent.rRNA = 0.2)
EpicPreMN(cm_name, min.counts = 1e3, max.counts = 15e3, max.complexity = 0.75, max.percent.mito = 0.001)
```
This function parses and processes dropEst objects that must contain exonic, intronic, and spanning count matrices, created using the -V flag.

```
cm.pp <- mapply(EpicPreHS, cm.files, orig.ident = names(cm.files), SIMPLIFY = F)
```

Next, the filtered count matrices can be merged together using ```mergeCM```. This function will accept up to 48 count matrices for merging. 

```
combined.emat <- mergeCM(cm.pp, type = "emat")
combined.nmat <- mergeCM(cm.pp, type = "nmat")
```

```combined.emat``` is now ready to be used as an input count matrix for Seurat. Cell names contain  the name of each ```cm_file``` followed by "." followed by a number ```1:ncol(cm_file)``` followed by "_" and the cell barcode. 
