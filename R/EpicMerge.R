EpicMerge <- function(x,...){
  args = list(x,...)
  narg = length(args)
  if ("Matrix.utils" %in% installed.packages()==FALSE) {
    stop("You need to install Matrix.utils")
  }
  if (narg<1) {
    stop("You must merge at least two matrices")
  }
  if (narg==2) {
    output.name <- join.Matrix(args[[1]], args[[2]], by.x = rownames(args[[1]]), by.y = rownames(args[[2]]), all.x = TRUE, all.y = TRUE)
    output.name <- output.name[!rownames(output.name)=='fill.x',]
    return(output.name)
  }
  if (narg==3) {
    output.name1 <- join.Matrix(args[[1]], args[[2]], by.x = rownames(args[[1]]), by.y = rownames(args[[2]]), all.x = TRUE, all.y = TRUE)
    output.name <- join.Matrix(output.name1, args[[3]], by.x = rownames(output.name1), by.y = rownames(args[[3]]), all.x = TRUE, all.y = TRUE)
    output.name <- output.name[!rownames(output.name)=='fill.x',]
    return(output.name)
  }
  if (narg==4) {
    output.name1 <- join.Matrix(args[[1]], args[[2]], by.x = rownames(args[[1]]), by.y = rownames(args[[2]]), all.x = TRUE, all.y = TRUE)
    output.name2 <- join.Matrix(output.name1, args[[3]], by.x = rownames(output.name1), by.y = rownames(args[[3]]), all.x = TRUE, all.y = TRUE)
    output.name <- join.Matrix(output.name2, args[[4]], by.x = rownames(output.name2), by.y = rownames(args[[4]]), all.x = TRUE, all.y = TRUE)
    output.name <- output.name[!rownames(output.name)=='fill.x',]
    return(output.name)
  }
  if (narg==5) {
    output.name1 <- join.Matrix(args[[1]], args[[2]], by.x = rownames(args[[1]]), by.y = rownames(args[[2]]), all.x = TRUE, all.y = TRUE)
    output.name2 <- join.Matrix(output.name1, args[[3]], by.x = rownames(output.name1), by.y = rownames(args[[3]]), all.x = TRUE, all.y = TRUE)
    output.name3 <- join.Matrix(output.name2, args[[4]], by.x = rownames(output.name2), by.y = rownames(args[[4]]), all.x = TRUE, all.y = TRUE)
    output.name <- join.Matrix(output.name3, args[[5]], by.x = rownames(output.name3), by.y = rownames(args[[5]]), all.x = TRUE, all.y = TRUE)
    output.name <- output.name[!rownames(output.name)=='fill.x',]
    return(output.name)
  }
  if (narg==6) {
    output.name1 <- join.Matrix(args[[1]], args[[2]], by.x = rownames(args[[1]]), by.y = rownames(args[[2]]), all.x = TRUE, all.y = TRUE)
    output.name2 <- join.Matrix(output.name1, args[[3]], by.x = rownames(output.name1), by.y = rownames(args[[3]]), all.x = TRUE, all.y = TRUE)
    output.name3 <- join.Matrix(output.name2, args[[4]], by.x = rownames(output.name2), by.y = rownames(args[[4]]), all.x = TRUE, all.y = TRUE)
    output.name4 <- join.Matrix(output.name3, args[[5]], by.x = rownames(output.name3), by.y = rownames(args[[5]]), all.x = TRUE, all.y = TRUE)
    output.name <- join.Matrix(output.name4, args[[6]], by.x = rownames(output.name4), by.y = rownames(args[[6]]), all.x = TRUE, all.y = TRUE)
    output.name <- output.name[!rownames(output.name)=='fill.x',]
    return(output.name)
  }
  if (narg==7) {
    output.name1 <- join.Matrix(args[[1]], args[[2]], by.x = rownames(args[[1]]), by.y = rownames(args[[2]]), all.x = TRUE, all.y = TRUE)
    output.name2 <- join.Matrix(output.name1, args[[3]], by.x = rownames(output.name1), by.y = rownames(args[[3]]), all.x = TRUE, all.y = TRUE)
    output.name3 <- join.Matrix(output.name2, args[[4]], by.x = rownames(output.name2), by.y = rownames(args[[4]]), all.x = TRUE, all.y = TRUE)
    output.name4 <- join.Matrix(output.name3, args[[5]], by.x = rownames(output.name3), by.y = rownames(args[[5]]), all.x = TRUE, all.y = TRUE)
    output.name5 <- join.Matrix(output.name4, args[[6]], by.x = rownames(output.name4), by.y = rownames(args[[6]]), all.x = TRUE, all.y = TRUE)
    output.name <- join.Matrix(output.name5, args[[7]], by.x = rownames(output.name5), by.y = rownames(args[[7]]), all.x = TRUE, all.y = TRUE)
    output.name <- output.name[!rownames(output.name)=='fill.x',]
    return(output.name)
  }
  if (narg==8) {
    output.name1 <- join.Matrix(args[[1]], args[[2]], by.x = rownames(args[[1]]), by.y = rownames(args[[2]]), all.x = TRUE, all.y = TRUE)
    output.name2 <- join.Matrix(output.name1, args[[3]], by.x = rownames(output.name1), by.y = rownames(args[[3]]), all.x = TRUE, all.y = TRUE)
    output.name3 <- join.Matrix(output.name2, args[[4]], by.x = rownames(output.name2), by.y = rownames(args[[4]]), all.x = TRUE, all.y = TRUE)
    output.name4 <- join.Matrix(output.name3, args[[5]], by.x = rownames(output.name3), by.y = rownames(args[[5]]), all.x = TRUE, all.y = TRUE)
    output.name5 <- join.Matrix(output.name4, args[[6]], by.x = rownames(output.name4), by.y = rownames(args[[6]]), all.x = TRUE, all.y = TRUE)
    output.name6 <- join.Matrix(output.name5, args[[7]], by.x = rownames(output.name5), by.y = rownames(args[[7]]), all.x = TRUE, all.y = TRUE)
    output.name <- join.Matrix(output.name6, args[[8]], by.x = rownames(output.name6), by.y = rownames(args[[8]]), all.x = TRUE, all.y = TRUE)
    output.name <- output.name[!rownames(output.name)=='fill.x',]
    return(output.name)
  }
  if (narg==9) {
    output.name1 <- join.Matrix(args[[1]], args[[2]], by.x = rownames(args[[1]]), by.y = rownames(args[[2]]), all.x = TRUE, all.y = TRUE)
    output.name2 <- join.Matrix(output.name1, args[[3]], by.x = rownames(output.name1), by.y = rownames(args[[3]]), all.x = TRUE, all.y = TRUE)
    output.name3 <- join.Matrix(output.name2, args[[4]], by.x = rownames(output.name2), by.y = rownames(args[[4]]), all.x = TRUE, all.y = TRUE)
    output.name4 <- join.Matrix(output.name3, args[[5]], by.x = rownames(output.name3), by.y = rownames(args[[5]]), all.x = TRUE, all.y = TRUE)
    output.name5 <- join.Matrix(output.name4, args[[6]], by.x = rownames(output.name4), by.y = rownames(args[[6]]), all.x = TRUE, all.y = TRUE)
    output.name6 <- join.Matrix(output.name5, args[[7]], by.x = rownames(output.name5), by.y = rownames(args[[7]]), all.x = TRUE, all.y = TRUE)
    output.name7 <- join.Matrix(output.name6, args[[8]], by.x = rownames(output.name6), by.y = rownames(args[[8]]), all.x = TRUE, all.y = TRUE)
    output.name <- join.Matrix(output.name7, args[[9]], by.x = rownames(output.name7), by.y = rownames(args[[9]]), all.x = TRUE, all.y = TRUE)
    output.name <- output.name[!rownames(output.name)=='fill.x',]
    return(output.name)
  }
  if (narg==10) {
    output.name1 <- join.Matrix(args[[1]], args[[2]], by.x = rownames(args[[1]]), by.y = rownames(args[[2]]), all.x = TRUE, all.y = TRUE)
    output.name2 <- join.Matrix(output.name1, args[[3]], by.x = rownames(output.name1), by.y = rownames(args[[3]]), all.x = TRUE, all.y = TRUE)
    output.name3 <- join.Matrix(output.name2, args[[4]], by.x = rownames(output.name2), by.y = rownames(args[[4]]), all.x = TRUE, all.y = TRUE)
    output.name4 <- join.Matrix(output.name3, args[[5]], by.x = rownames(output.name3), by.y = rownames(args[[5]]), all.x = TRUE, all.y = TRUE)
    output.name5 <- join.Matrix(output.name4, args[[6]], by.x = rownames(output.name4), by.y = rownames(args[[6]]), all.x = TRUE, all.y = TRUE)
    output.name6 <- join.Matrix(output.name5, args[[7]], by.x = rownames(output.name5), by.y = rownames(args[[7]]), all.x = TRUE, all.y = TRUE)
    output.name7 <- join.Matrix(output.name6, args[[8]], by.x = rownames(output.name6), by.y = rownames(args[[8]]), all.x = TRUE, all.y = TRUE)
    output.name8 <- join.Matrix(output.name7, args[[9]], by.x = rownames(output.name7), by.y = rownames(args[[9]]), all.x = TRUE, all.y = TRUE)
    output.name <- join.Matrix(output.name8, args[[10]], by.x = rownames(output.name8), by.y = rownames(args[[10]]), all.x = TRUE, all.y = TRUE)
    output.name <- output.name[!rownames(output.name)=='fill.x',]
    return(output.name)
  }
  if (narg==11) {
    output.name1 <- join.Matrix(args[[1]], args[[2]], by.x = rownames(args[[1]]), by.y = rownames(args[[2]]), all.x = TRUE, all.y = TRUE)
    output.name2 <- join.Matrix(output.name1, args[[3]], by.x = rownames(output.name1), by.y = rownames(args[[3]]), all.x = TRUE, all.y = TRUE)
    output.name3 <- join.Matrix(output.name2, args[[4]], by.x = rownames(output.name2), by.y = rownames(args[[4]]), all.x = TRUE, all.y = TRUE)
    output.name4 <- join.Matrix(output.name3, args[[5]], by.x = rownames(output.name3), by.y = rownames(args[[5]]), all.x = TRUE, all.y = TRUE)
    output.name5 <- join.Matrix(output.name4, args[[6]], by.x = rownames(output.name4), by.y = rownames(args[[6]]), all.x = TRUE, all.y = TRUE)
    output.name6 <- join.Matrix(output.name5, args[[7]], by.x = rownames(output.name5), by.y = rownames(args[[7]]), all.x = TRUE, all.y = TRUE)
    output.name7 <- join.Matrix(output.name6, args[[8]], by.x = rownames(output.name6), by.y = rownames(args[[8]]), all.x = TRUE, all.y = TRUE)
    output.name8 <- join.Matrix(output.name7, args[[9]], by.x = rownames(output.name7), by.y = rownames(args[[9]]), all.x = TRUE, all.y = TRUE)
    output.name9 <- join.Matrix(output.name8, args[[10]], by.x = rownames(output.name8), by.y = rownames(args[[10]]), all.x = TRUE, all.y = TRUE)
    output.name <- join.Matrix(output.name9, args[[11]], by.x = rownames(output.name9), by.y = rownames(args[[11]]), all.x = TRUE, all.y = TRUE)
    output.name <- output.name[!rownames(output.name)=='fill.x',]
    return(output.name)
  }
  if (narg==12) {
    output.name1 <- join.Matrix(args[[1]], args[[2]], by.x = rownames(args[[1]]), by.y = rownames(args[[2]]), all.x = TRUE, all.y = TRUE)
    output.name2 <- join.Matrix(output.name1, args[[3]], by.x = rownames(output.name1), by.y = rownames(args[[3]]), all.x = TRUE, all.y = TRUE)
    output.name3 <- join.Matrix(output.name2, args[[4]], by.x = rownames(output.name2), by.y = rownames(args[[4]]), all.x = TRUE, all.y = TRUE)
    output.name4 <- join.Matrix(output.name3, args[[5]], by.x = rownames(output.name3), by.y = rownames(args[[5]]), all.x = TRUE, all.y = TRUE)
    output.name5 <- join.Matrix(output.name4, args[[6]], by.x = rownames(output.name4), by.y = rownames(args[[6]]), all.x = TRUE, all.y = TRUE)
    output.name6 <- join.Matrix(output.name5, args[[7]], by.x = rownames(output.name5), by.y = rownames(args[[7]]), all.x = TRUE, all.y = TRUE)
    output.name7 <- join.Matrix(output.name6, args[[8]], by.x = rownames(output.name6), by.y = rownames(args[[8]]), all.x = TRUE, all.y = TRUE)
    output.name8 <- join.Matrix(output.name7, args[[9]], by.x = rownames(output.name7), by.y = rownames(args[[9]]), all.x = TRUE, all.y = TRUE)
    output.name9 <- join.Matrix(output.name8, args[[10]], by.x = rownames(output.name8), by.y = rownames(args[[10]]), all.x = TRUE, all.y = TRUE)
    output.name10 <- join.Matrix(output.name9, args[[11]], by.x = rownames(output.name9), by.y = rownames(args[[11]]), all.x = TRUE, all.y = TRUE)
    output.name <- join.Matrix(output.name10, args[[12]], by.x = rownames(output.name10), by.y = rownames(args[[12]]), all.x = TRUE, all.y = TRUE)
    output.name <- output.name[!rownames(output.name)=='fill.x',]
    return(output.name)
  }
  if (narg>12) {
    stop("This code only supports merging a maximum of 12 matrices at one time. However, you can still use this function iteratively, calling it ~(n/12)+1 times to merge n matrices.")
  }
}

EpicPreMN <- function(cm_name, min.counts = 1e3, max.counts = 15e3, max.complexity = 0.75, max.percent.mito = 0.001){
  if (any(grep(".cm$", deparse(substitute(cm_name))))==FALSE){
    warning("The resulting cell names will be strange if the input object name does not end with '.cm'")
  }
  cm_name.emat <- cm_name$exon
  cm_name.emat <- cm_name.emat[,Matrix::colSums(cm_name.emat)>=min.counts]
  cm_name.emat <- cm_name.emat[,Matrix::colSums(cm_name.emat)<=max.counts]

  cm_name.doublet <- (Matrix::colSums(cm_name.emat != 0)/Matrix::colSums(cm_name.emat))
  hist(cm_name.doublet, xlim = c(0, 1), breaks = 20)
  cm_name.emat <- cm_name.emat[,cm_name.doublet<max.complexity]

  cm_name.mito.genes = c(grep("^MT-", rownames(cm_name.emat), value = T),grep("^MTR", rownames(cm_name.emat), value = T))
  cm_name.percent.mito = Matrix::colSums(cm_name.emat[cm_name.mito.genes,])/Matrix::colSums(cm_name.emat)
  hist(cm_name.percent.mito, xlim = c(0, 0.006), breaks = 20)
  cm_name.emat <- cm_name.emat[,cm_name.percent.mito<max.percent.mito]

  cm_name.nmat <- cm_name$intron
  cm_name.smat <- cm_name$spanning
  bcs.cm_name.emat <- colnames(cm_name.emat)
  cm_name.nmat <- cm_name.nmat[,which(colnames(cm_name.nmat) %in% bcs.cm_name.emat), drop = FALSE]
  cm_name.smat <- cm_name.smat[,which(colnames(cm_name.smat) %in% bcs.cm_name.emat), drop = FALSE]
  names.bcs.cm_name = as.data.frame(cbind(bcs.cm_name.emat, paste(rep(substr(deparse(substitute(cm_name)), 1, nchar(deparse(substitute(cm_name)))-3),ncol(cm_name.emat)), c(1:ncol(cm_name.emat)), sep=".")))
  colnames(names.bcs.cm_name) <- c("bcs", "cm_name")
  cm_name.nmat.bcs <- as.data.frame(colnames(cm_name.nmat))
  colnames(cm_name.nmat.bcs) <- "bcs"
  cm_name.bcs.nmat <- join(names.bcs.cm_name, cm_name.nmat.bcs, type = "right", by = "bcs")
  colnames(cm_name.emat) <- names.bcs.cm_name$cm_name
  colnames(cm_name.nmat) <- cm_name.bcs.nmat$cm_name
  cm_name.smat.bcs <- as.data.frame(colnames(cm_name.smat))
  colnames(cm_name.smat.bcs) <- "bcs"
  cm_name.bcs.smat <- join(names.bcs.cm_name, cm_name.smat.bcs, type = "right", by = "bcs")
  colnames(cm_name.smat) <- cm_name.bcs.smat$cm_name
  cm_name = list(emat = cm_name.emat, nmat = cm_name.nmat, smat = cm_name.smat)
  return(cm_name)
}

EpicPreHS <- function(cm_name, min.counts = 1e3, max.counts = 15e3, max.complexity = 0.75, max.percent.mito = 0.2, max.percent.rRNA = 0.2){
  if (any(grep(".cm$", deparse(substitute(cm_name))))==FALSE){
    warning("The resulting cell names will be strange if the input object name does not end with '.cm'")
  }
  cm_name.emat <- cm_name$exon
  cm_name.emat <- cm_name.emat[,Matrix::colSums(cm_name.emat)>=min.counts]
  cm_name.emat <- cm_name.emat[,Matrix::colSums(cm_name.emat)<=max.counts]

  cm_name.doublet <- (Matrix::colSums(cm_name.emat != 0)/Matrix::colSums(cm_name.emat))
  hist(cm_name.doublet, xlim = c(0, 1), breaks = 20)
  cm_name.emat <- cm_name.emat[,cm_name.doublet<max.complexity]

  cm_name.mito.genes = c(grep("^MT-", rownames(cm_name.emat), value = T),grep("^MTR", rownames(cm_name.emat), value = T))
  cm_name.percent.mito = Matrix::colSums(cm_name.emat[cm_name.mito.genes,])/Matrix::colSums(cm_name.emat)
  hist(cm_name.percent.mito, xlim = c(0, 0.006), breaks = 20)
  cm_name.emat <- cm_name.emat[,cm_name.percent.mito<max.percent.mito]

  cm_name.rRNA.genes = c(grep("^RNA18S5", rownames(cm_name.emat), value = T),grep("^RNA28S5", rownames(cm_name.emat), value = T))
  cm_name.percent.rRNA = Matrix::colSums(cm_name.emat[cm_name.rRNA.genes,])/Matrix::colSums(cm_name.emat)
  hist(cm_name.percent.rRNA)
  cm_name.emat <- cm_name.emat[,cm_name.percent.rRNA<max.percent.rRNA]

  cm_name.nmat <- cm_name$intron
  cm_name.smat <- cm_name$spanning
  bcs.cm_name.emat <- colnames(cm_name.emat)
  cm_name.nmat <- cm_name.nmat[,which(colnames(cm_name.nmat) %in% bcs.cm_name.emat), drop = FALSE]
  cm_name.smat <- cm_name.smat[,which(colnames(cm_name.smat) %in% bcs.cm_name.emat), drop = FALSE]
  names.bcs.cm_name = as.data.frame(cbind(bcs.cm_name.emat, paste(rep(substr(deparse(substitute(cm_name)), 1, nchar(deparse(substitute(cm_name)))-3),ncol(cm_name.emat)), c(1:ncol(cm_name.emat)), sep=".")))
  colnames(names.bcs.cm_name) <- c("bcs", "cm_name")
  cm_name.nmat.bcs <- as.data.frame(colnames(cm_name.nmat))
  colnames(cm_name.nmat.bcs) <- "bcs"
  cm_name.bcs.nmat <- join(names.bcs.cm_name, cm_name.nmat.bcs, type = "right", by = "bcs")
  colnames(cm_name.emat) <- names.bcs.cm_name$cm_name
  colnames(cm_name.nmat) <- cm_name.bcs.nmat$cm_name
  cm_name.smat.bcs <- as.data.frame(colnames(cm_name.smat))
  colnames(cm_name.smat.bcs) <- "bcs"
  cm_name.bcs.smat <- join(names.bcs.cm_name, cm_name.smat.bcs, type = "right", by = "bcs")
  colnames(cm_name.smat) <- cm_name.bcs.smat$cm_name
  cm_name = list(emat = cm_name.emat, nmat = cm_name.nmat, smat = cm_name.smat)
  return(cm_name)
}
