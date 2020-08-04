EpicJoin <- function (x, y, by.x, by.y, all.x = TRUE, all.y = TRUE)
{
  requireNamespace("grr")
  indices <- grr::matches(by.x, by.y, all.x, all.y, nomatch = NA)
  x.match <- as(grr::extract(x, indices$x[complete.cases(indices)]), class(x))
  y.match <- as(grr::extract(y, indices$y[complete.cases(indices)]), class(y))
  xy.complete <- cbind2(x.match,y.match)
  ynotinx <- as(grr::extract(y, indices$y[is.na(indices$x)]), class(y))
  ynotinx <- cbind(as(matrix(0, nrow = length(indices$y[is.na(indices$x)]), ncol = ncol(x)), class(x)), ynotinx)
  result <- rbind(xy.complete, ynotinx)
  xnotiny <- as(grr::extract(x, indices$x[is.na(indices$y)]), class(x))
  xnotiny <- cbind(xnotiny, as(matrix(0, nrow = length(indices$x[is.na(indices$y)]), ncol = ncol(y)), class(y)))
  result <- rbind(result, xnotiny)
  return(result)
}

EpicMerge <- function(x,...){
  args = list(x,...)
  narg = length(args)
  if (narg<1) {
    stop("You must merge at least two matrices")
  }
  if (narg==2) {
    output.name <- EpicJoin(args[[1]], args[[2]], by.x = rownames(args[[1]]), by.y = rownames(args[[2]]), all.x = TRUE, all.y = TRUE)

    return(output.name)
  }
  if (narg==3) {
    output.name1 <- EpicJoin(args[[1]], args[[2]], by.x = rownames(args[[1]]), by.y = rownames(args[[2]]), all.x = TRUE, all.y = TRUE)
    output.name <- EpicJoin(output.name1, args[[3]], by.x = rownames(output.name1), by.y = rownames(args[[3]]), all.x = TRUE, all.y = TRUE)

    return(output.name)
  }
  if (narg==4) {
    output.name1 <- EpicJoin(args[[1]], args[[2]], by.x = rownames(args[[1]]), by.y = rownames(args[[2]]), all.x = TRUE, all.y = TRUE)
    output.name2 <- EpicJoin(output.name1, args[[3]], by.x = rownames(output.name1), by.y = rownames(args[[3]]), all.x = TRUE, all.y = TRUE)
    output.name <- EpicJoin(output.name2, args[[4]], by.x = rownames(output.name2), by.y = rownames(args[[4]]), all.x = TRUE, all.y = TRUE)

    return(output.name)
  }
  if (narg==5) {
    output.name1 <- EpicJoin(args[[1]], args[[2]], by.x = rownames(args[[1]]), by.y = rownames(args[[2]]), all.x = TRUE, all.y = TRUE)
    output.name2 <- EpicJoin(output.name1, args[[3]], by.x = rownames(output.name1), by.y = rownames(args[[3]]), all.x = TRUE, all.y = TRUE)
    output.name3 <- EpicJoin(output.name2, args[[4]], by.x = rownames(output.name2), by.y = rownames(args[[4]]), all.x = TRUE, all.y = TRUE)
    output.name <- EpicJoin(output.name3, args[[5]], by.x = rownames(output.name3), by.y = rownames(args[[5]]), all.x = TRUE, all.y = TRUE)

    return(output.name)
  }
  if (narg==6) {
    output.name1 <- EpicJoin(args[[1]], args[[2]], by.x = rownames(args[[1]]), by.y = rownames(args[[2]]), all.x = TRUE, all.y = TRUE)
    output.name2 <- EpicJoin(output.name1, args[[3]], by.x = rownames(output.name1), by.y = rownames(args[[3]]), all.x = TRUE, all.y = TRUE)
    output.name3 <- EpicJoin(output.name2, args[[4]], by.x = rownames(output.name2), by.y = rownames(args[[4]]), all.x = TRUE, all.y = TRUE)
    output.name4 <- EpicJoin(output.name3, args[[5]], by.x = rownames(output.name3), by.y = rownames(args[[5]]), all.x = TRUE, all.y = TRUE)
    output.name <- EpicJoin(output.name4, args[[6]], by.x = rownames(output.name4), by.y = rownames(args[[6]]), all.x = TRUE, all.y = TRUE)

    return(output.name)
  }
  if (narg==7) {
    output.name1 <- EpicJoin(args[[1]], args[[2]], by.x = rownames(args[[1]]), by.y = rownames(args[[2]]), all.x = TRUE, all.y = TRUE)
    output.name2 <- EpicJoin(output.name1, args[[3]], by.x = rownames(output.name1), by.y = rownames(args[[3]]), all.x = TRUE, all.y = TRUE)
    output.name3 <- EpicJoin(output.name2, args[[4]], by.x = rownames(output.name2), by.y = rownames(args[[4]]), all.x = TRUE, all.y = TRUE)
    output.name4 <- EpicJoin(output.name3, args[[5]], by.x = rownames(output.name3), by.y = rownames(args[[5]]), all.x = TRUE, all.y = TRUE)
    output.name5 <- EpicJoin(output.name4, args[[6]], by.x = rownames(output.name4), by.y = rownames(args[[6]]), all.x = TRUE, all.y = TRUE)
    output.name <- EpicJoin(output.name5, args[[7]], by.x = rownames(output.name5), by.y = rownames(args[[7]]), all.x = TRUE, all.y = TRUE)

    return(output.name)
  }
  if (narg==8) {
    output.name1 <- EpicJoin(args[[1]], args[[2]], by.x = rownames(args[[1]]), by.y = rownames(args[[2]]), all.x = TRUE, all.y = TRUE)
    output.name2 <- EpicJoin(output.name1, args[[3]], by.x = rownames(output.name1), by.y = rownames(args[[3]]), all.x = TRUE, all.y = TRUE)
    output.name3 <- EpicJoin(output.name2, args[[4]], by.x = rownames(output.name2), by.y = rownames(args[[4]]), all.x = TRUE, all.y = TRUE)
    output.name4 <- EpicJoin(output.name3, args[[5]], by.x = rownames(output.name3), by.y = rownames(args[[5]]), all.x = TRUE, all.y = TRUE)
    output.name5 <- EpicJoin(output.name4, args[[6]], by.x = rownames(output.name4), by.y = rownames(args[[6]]), all.x = TRUE, all.y = TRUE)
    output.name6 <- EpicJoin(output.name5, args[[7]], by.x = rownames(output.name5), by.y = rownames(args[[7]]), all.x = TRUE, all.y = TRUE)
    output.name <- EpicJoin(output.name6, args[[8]], by.x = rownames(output.name6), by.y = rownames(args[[8]]), all.x = TRUE, all.y = TRUE)

    return(output.name)
  }
  if (narg==9) {
    output.name1 <- EpicJoin(args[[1]], args[[2]], by.x = rownames(args[[1]]), by.y = rownames(args[[2]]), all.x = TRUE, all.y = TRUE)
    output.name2 <- EpicJoin(output.name1, args[[3]], by.x = rownames(output.name1), by.y = rownames(args[[3]]), all.x = TRUE, all.y = TRUE)
    output.name3 <- EpicJoin(output.name2, args[[4]], by.x = rownames(output.name2), by.y = rownames(args[[4]]), all.x = TRUE, all.y = TRUE)
    output.name4 <- EpicJoin(output.name3, args[[5]], by.x = rownames(output.name3), by.y = rownames(args[[5]]), all.x = TRUE, all.y = TRUE)
    output.name5 <- EpicJoin(output.name4, args[[6]], by.x = rownames(output.name4), by.y = rownames(args[[6]]), all.x = TRUE, all.y = TRUE)
    output.name6 <- EpicJoin(output.name5, args[[7]], by.x = rownames(output.name5), by.y = rownames(args[[7]]), all.x = TRUE, all.y = TRUE)
    output.name7 <- EpicJoin(output.name6, args[[8]], by.x = rownames(output.name6), by.y = rownames(args[[8]]), all.x = TRUE, all.y = TRUE)
    output.name <- EpicJoin(output.name7, args[[9]], by.x = rownames(output.name7), by.y = rownames(args[[9]]), all.x = TRUE, all.y = TRUE)

    return(output.name)
  }
  if (narg==10) {
    output.name1 <- EpicJoin(args[[1]], args[[2]], by.x = rownames(args[[1]]), by.y = rownames(args[[2]]), all.x = TRUE, all.y = TRUE)
    output.name2 <- EpicJoin(output.name1, args[[3]], by.x = rownames(output.name1), by.y = rownames(args[[3]]), all.x = TRUE, all.y = TRUE)
    output.name3 <- EpicJoin(output.name2, args[[4]], by.x = rownames(output.name2), by.y = rownames(args[[4]]), all.x = TRUE, all.y = TRUE)
    output.name4 <- EpicJoin(output.name3, args[[5]], by.x = rownames(output.name3), by.y = rownames(args[[5]]), all.x = TRUE, all.y = TRUE)
    output.name5 <- EpicJoin(output.name4, args[[6]], by.x = rownames(output.name4), by.y = rownames(args[[6]]), all.x = TRUE, all.y = TRUE)
    output.name6 <- EpicJoin(output.name5, args[[7]], by.x = rownames(output.name5), by.y = rownames(args[[7]]), all.x = TRUE, all.y = TRUE)
    output.name7 <- EpicJoin(output.name6, args[[8]], by.x = rownames(output.name6), by.y = rownames(args[[8]]), all.x = TRUE, all.y = TRUE)
    output.name8 <- EpicJoin(output.name7, args[[9]], by.x = rownames(output.name7), by.y = rownames(args[[9]]), all.x = TRUE, all.y = TRUE)
    output.name <- EpicJoin(output.name8, args[[10]], by.x = rownames(output.name8), by.y = rownames(args[[10]]), all.x = TRUE, all.y = TRUE)

    return(output.name)
  }
  if (narg==11) {
    output.name1 <- EpicJoin(args[[1]], args[[2]], by.x = rownames(args[[1]]), by.y = rownames(args[[2]]), all.x = TRUE, all.y = TRUE)
    output.name2 <- EpicJoin(output.name1, args[[3]], by.x = rownames(output.name1), by.y = rownames(args[[3]]), all.x = TRUE, all.y = TRUE)
    output.name3 <- EpicJoin(output.name2, args[[4]], by.x = rownames(output.name2), by.y = rownames(args[[4]]), all.x = TRUE, all.y = TRUE)
    output.name4 <- EpicJoin(output.name3, args[[5]], by.x = rownames(output.name3), by.y = rownames(args[[5]]), all.x = TRUE, all.y = TRUE)
    output.name5 <- EpicJoin(output.name4, args[[6]], by.x = rownames(output.name4), by.y = rownames(args[[6]]), all.x = TRUE, all.y = TRUE)
    output.name6 <- EpicJoin(output.name5, args[[7]], by.x = rownames(output.name5), by.y = rownames(args[[7]]), all.x = TRUE, all.y = TRUE)
    output.name7 <- EpicJoin(output.name6, args[[8]], by.x = rownames(output.name6), by.y = rownames(args[[8]]), all.x = TRUE, all.y = TRUE)
    output.name8 <- EpicJoin(output.name7, args[[9]], by.x = rownames(output.name7), by.y = rownames(args[[9]]), all.x = TRUE, all.y = TRUE)
    output.name9 <- EpicJoin(output.name8, args[[10]], by.x = rownames(output.name8), by.y = rownames(args[[10]]), all.x = TRUE, all.y = TRUE)
    output.name <- EpicJoin(output.name9, args[[11]], by.x = rownames(output.name9), by.y = rownames(args[[11]]), all.x = TRUE, all.y = TRUE)

    return(output.name)
  }
  if (narg==12) {
    output.name1 <- EpicJoin(args[[1]], args[[2]], by.x = rownames(args[[1]]), by.y = rownames(args[[2]]), all.x = TRUE, all.y = TRUE)
    output.name2 <- EpicJoin(output.name1, args[[3]], by.x = rownames(output.name1), by.y = rownames(args[[3]]), all.x = TRUE, all.y = TRUE)
    output.name3 <- EpicJoin(output.name2, args[[4]], by.x = rownames(output.name2), by.y = rownames(args[[4]]), all.x = TRUE, all.y = TRUE)
    output.name4 <- EpicJoin(output.name3, args[[5]], by.x = rownames(output.name3), by.y = rownames(args[[5]]), all.x = TRUE, all.y = TRUE)
    output.name5 <- EpicJoin(output.name4, args[[6]], by.x = rownames(output.name4), by.y = rownames(args[[6]]), all.x = TRUE, all.y = TRUE)
    output.name6 <- EpicJoin(output.name5, args[[7]], by.x = rownames(output.name5), by.y = rownames(args[[7]]), all.x = TRUE, all.y = TRUE)
    output.name7 <- EpicJoin(output.name6, args[[8]], by.x = rownames(output.name6), by.y = rownames(args[[8]]), all.x = TRUE, all.y = TRUE)
    output.name8 <- EpicJoin(output.name7, args[[9]], by.x = rownames(output.name7), by.y = rownames(args[[9]]), all.x = TRUE, all.y = TRUE)
    output.name9 <- EpicJoin(output.name8, args[[10]], by.x = rownames(output.name8), by.y = rownames(args[[10]]), all.x = TRUE, all.y = TRUE)
    output.name10 <- EpicJoin(output.name9, args[[11]], by.x = rownames(output.name9), by.y = rownames(args[[11]]), all.x = TRUE, all.y = TRUE)
    output.name <- EpicJoin(output.name10, args[[12]], by.x = rownames(output.name10), by.y = rownames(args[[12]]), all.x = TRUE, all.y = TRUE)

    return(output.name)
  }
  if (narg>12) {
    stop("This code only supports merging a maximum of 12 matrices at one time. However, you can still use this function iteratively, calling it ~(n/12)+1 times to merge n matrices.")
  }
}


mergeCM <- function(cm_list = NULL, type = "emat") {
  unlisted <- unlist(cm_list)
  unlisted.sort <- unlisted[grep(type, names(unlisted))]
  if (length(unlisted.sort)<=12){
    names(unlisted.sort) <- NULL
    final <- do.call(myMerge, args = unlisted.sort)
  }
  if (between(length(unlisted.sort), 13, 24)){
    names(unlisted.sort) <- NULL
    final.1 <- do.call(myMerge, args = unlisted.sort[1:12])
    final.2 <- do.call(myMerge, args = unlisted.sort[13:length(unlisted.sort)])
    final <- do.call(myMerge, args = list(final.1, final.2))
  }
  if (between(length(unlisted.sort), 25, 36)){
    names(unlisted.sort) <- NULL
    final.1 <- do.call(myMerge, args = unlisted.sort[1:12])
    final.2 <- do.call(myMerge, args = unlisted.sort[13:24])
    final.3 <- do.call(myMerge, args = unlisted.sort[25:length(unlisted.sort)])
    final <- do.call(myMerge, args = list(final.1, final.2, final.3))
  }
  if (between(length(unlisted.sort), 37, 48)){
    names(unlisted.sort) <- NULL
    final.1 <- do.call(myMerge, args = unlisted.sort[1:12])
    final.2 <- do.call(myMerge, args = unlisted.sort[13:24])
    final.3 <- do.call(myMerge, args = unlisted.sort[25:36])
    final.4 <- do.call(myMerge, args = unlisted.sort[37:length(unlisted.sort)])
    final <- do.call(myMerge, args = list(final.1, final.2, final.3, final.4))
  }
  if (between(length(unlisted.sort), 49, 60)){
    names(unlisted.sort) <- NULL
    final.1 <- do.call(myMerge, args = unlisted.sort[1:12])
    final.2 <- do.call(myMerge, args = unlisted.sort[13:24])
    final.3 <- do.call(myMerge, args = unlisted.sort[25:36])
    final.4 <- do.call(myMerge, args = unlisted.sort[37:48])
    final.5 <- do.call(myMerge, args = unlisted.sort[49:length(unlisted.sort)])
    final <- do.call(myMerge, args = list(final.1, final.2, final.3, final.4, final.5))
  }
  if (between(length(unlisted.sort), 61, 72)){
    names(unlisted.sort) <- NULL
    final.1 <- do.call(myMerge, args = unlisted.sort[1:12])
    final.2 <- do.call(myMerge, args = unlisted.sort[13:24])
    final.3 <- do.call(myMerge, args = unlisted.sort[25:36])
    final.4 <- do.call(myMerge, args = unlisted.sort[37:48])
    final.5 <- do.call(myMerge, args = unlisted.sort[49:60])
    final.6 <- do.call(myMerge, args = unlisted.sort[61:length(unlisted.sort)])
    final <- do.call(myMerge, args = list(final.1, final.2, final.3, final.4, final.5, final.6))
  }
  return(final)
}


EpicPreHS <- function(cm_name, min.counts = 1e3, max.counts = 15e3, max.doublet.ratio = 0.75, max.percent.mito = 0.2, max.percent.rRNA = 0.2, orig.ident = NULL){
  cm_name.emat <- cm_name$exon
  cm_name.emat <- cm_name.emat[,Matrix::colSums(cm_name.emat)>=min.counts]
  cm_name.emat <- cm_name.emat[,Matrix::colSums(cm_name.emat)<=max.counts]

  cm_name.doublet <- (Matrix::colSums(cm_name.emat != 0)/Matrix::colSums(cm_name.emat))
  hist(cm_name.doublet, xlim = c(0, 1), breaks = 50, main = paste0(orig.ident, " Complexity"))
  cm_name.emat <- cm_name.emat[,cm_name.doublet<max.doublet.ratio]

  cm_name.mito.genes = c(grep("^MT-", rownames(cm_name.emat), value = T), grep("^MTR", rownames(cm_name.emat), value = T))
  cm_name.percent.mito = Matrix::colSums(cm_name.emat[cm_name.mito.genes,])/Matrix::colSums(cm_name.emat)
  hist(cm_name.percent.mito, breaks = 50, main = paste0(orig.ident, " % mitochondrial"))
  cm_name.emat <- cm_name.emat[,cm_name.percent.mito<max.percent.mito]

  cm_name.rRNA.genes = c(grep("^RPL", rownames(cm_name.emat), value = T),grep("^RPS", rownames(cm_name.emat), value = T))
  cm_name.percent.rRNA = Matrix::colSums(cm_name.emat[cm_name.rRNA.genes,])/Matrix::colSums(cm_name.emat)
  hist(cm_name.percent.rRNA, xlim = c(0, 1), breaks = 50, main = paste0(orig.ident, " % ribosomal"))
  cm_name.emat <- cm_name.emat[,cm_name.percent.rRNA<max.percent.rRNA]

  cm_name.nmat <- cm_name$intron
  cm_name.smat <- cm_name$spanning
  bcs.cm_name.emat <- colnames(cm_name.emat)
  cm_name.nmat <- cm_name.nmat[,which(colnames(cm_name.nmat) %in% bcs.cm_name.emat), drop = FALSE]
  cm_name.smat <- cm_name.smat[,which(colnames(cm_name.smat) %in% bcs.cm_name.emat), drop = FALSE]
  if (is.null(orig.ident)==TRUE){
    names.bcs.cm_name = as.data.frame(cbind(bcs.cm_name.emat, paste(rep(substr(deparse(substitute(cm_name)), 1, nchar(deparse(substitute(cm_name)))-3),ncol(cm_name.emat)), c(1:ncol(cm_name.emat)), sep=".")))
  }
  else {
    names.bcs.cm_name = as.data.frame(cbind(bcs.cm_name.emat, paste(rep(orig.ident,ncol(cm_name.emat)), c(1:ncol(cm_name.emat)), sep=".")))
  }
  names.bcs.cm_name$V2 <- paste(names.bcs.cm_name$V2,names.bcs.cm_name$bcs.cm_name.emat,sep = "_")
  colnames(names.bcs.cm_name) <- c("bcs", "cm_name")
  cm_name.nmat.bcs <- as.data.frame(colnames(cm_name.nmat))
  colnames(cm_name.nmat.bcs) <- "bcs"
  cm_name.bcs.nmat <- plyr::join(names.bcs.cm_name, cm_name.nmat.bcs, type = "right", by = "bcs")
  colnames(cm_name.emat) <- names.bcs.cm_name$cm_name
  colnames(cm_name.nmat) <- cm_name.bcs.nmat$cm_name
  cm_name.smat.bcs <- as.data.frame(colnames(cm_name.smat))
  colnames(cm_name.smat.bcs) <- "bcs"
  cm_name.bcs.smat <- plyr::join(names.bcs.cm_name, cm_name.smat.bcs, type = "right", by = "bcs")
  colnames(cm_name.smat) <- cm_name.bcs.smat$cm_name
  cm_name = list(emat = cm_name.emat, nmat = cm_name.nmat, smat = cm_name.smat)
  return(cm_name)
}


EpicPreMM <- function(cm_name, min.counts = 1e3, max.counts = 15e3, max.doublet.ratio = 0.45, max.percent.mito = 0.01, max.percent.rRNA = 0.4, orig.ident = NULL){
  if (any(grep(".cm$", deparse(substitute(cm_name))))==FALSE){
    warning("The resulting cell names will be strange if the input object name does not end with '.cm'")
  }
  cm_name.emat <- cm_name$exon
  cm_name.emat <- cm_name.emat[,Matrix::colSums(cm_name.emat)>=min.counts]
  cm_name.emat <- cm_name.emat[,Matrix::colSums(cm_name.emat)<=max.counts]

  cm_name.doublet <- (Matrix::colSums(cm_name.emat != 0)/Matrix::colSums(cm_name.emat))
  hist(cm_name.doublet, xlim = c(0, 1), breaks = 50)
  cm_name.emat <- cm_name.emat[,cm_name.doublet<max.doublet.ratio]

  cm_name.mito.genes = grep("^mt-", rownames(cm_name.emat), value = T)
  cm_name.percent.mito = Matrix::colSums(cm_name.emat[cm_name.mito.genes,])/Matrix::colSums(cm_name.emat)
  hist(cm_name.percent.mito, breaks = 50)
  cm_name.emat <- cm_name.emat[,cm_name.percent.mito<max.percent.mito]

  cm_name.rRNA.genes = c(grep("^Rpl", rownames(cm_name.emat), value = T),grep("^Rps", rownames(cm_name.emat), value = T))
  cm_name.percent.rRNA = Matrix::colSums(cm_name.emat[cm_name.rRNA.genes,])/Matrix::colSums(cm_name.emat)
  hist(cm_name.percent.rRNA, xlim = c(0, 1), breaks = 50)
  cm_name.emat <- cm_name.emat[,cm_name.percent.rRNA<max.percent.rRNA]

  cm_name.nmat <- cm_name$intron
  cm_name.smat <- cm_name$spanning
  bcs.cm_name.emat <- colnames(cm_name.emat)
  cm_name.nmat <- cm_name.nmat[,which(colnames(cm_name.nmat) %in% bcs.cm_name.emat), drop = FALSE]
  cm_name.smat <- cm_name.smat[,which(colnames(cm_name.smat) %in% bcs.cm_name.emat), drop = FALSE]
  if (is.null(orig.ident)==TRUE){
    names.bcs.cm_name = as.data.frame(cbind(bcs.cm_name.emat, paste(rep(substr(deparse(substitute(cm_name)), 1, nchar(deparse(substitute(cm_name)))-3),ncol(cm_name.emat)), c(1:ncol(cm_name.emat)), sep=".")))
  }
  else {
    names.bcs.cm_name = as.data.frame(cbind(bcs.cm_name.emat, paste(rep(orig.ident,ncol(cm_name.emat)), c(1:ncol(cm_name.emat)), sep=".")))
  }
  names.bcs.cm_name$V2 <- paste(names.bcs.cm_name$V2,names.bcs.cm_name$bcs.cm_name.emat,sep = "_")
  colnames(names.bcs.cm_name) <- c("bcs", "cm_name")
  cm_name.nmat.bcs <- as.data.frame(colnames(cm_name.nmat))
  colnames(cm_name.nmat.bcs) <- "bcs"
  cm_name.bcs.nmat <- plyr::join(names.bcs.cm_name, cm_name.nmat.bcs, type = "right", by = "bcs")
  colnames(cm_name.emat) <- names.bcs.cm_name$cm_name
  colnames(cm_name.nmat) <- cm_name.bcs.nmat$cm_name
  cm_name.smat.bcs <- as.data.frame(colnames(cm_name.smat))
  colnames(cm_name.smat.bcs) <- "bcs"
  cm_name.bcs.smat <- plyr::join(names.bcs.cm_name, cm_name.smat.bcs, type = "right", by = "bcs")
  colnames(cm_name.smat) <- cm_name.bcs.smat$cm_name
  cm_name = list(emat = cm_name.emat, nmat = cm_name.nmat, smat = cm_name.smat)
  return(cm_name)
}

EpicPreMN <- function(cm_name, min.counts = 1e3, max.counts = 15e3, max.doublet.ratio = 0.75, max.percent.mito = 0.001, max.percent.rRNA = 0.2, orig.ident = NULL){
  if (any(grep(".cm$", deparse(substitute(cm_name))))==FALSE){
    warning("The resulting cell names will be strange if the input object name does not end with '.cm'")
  }
  cm_name.emat <- cm_name$exon
  cm_name.emat <- cm_name.emat[,Matrix::colSums(cm_name.emat)>=min.counts]
  cm_name.emat <- cm_name.emat[,Matrix::colSums(cm_name.emat)<=max.counts]

  cm_name.doublet <- (Matrix::colSums(cm_name.emat != 0)/Matrix::colSums(cm_name.emat))
  hist(cm_name.doublet, xlim = c(0, 1), breaks = 50)
  cm_name.emat <- cm_name.emat[,cm_name.doublet<max.doublet.ratio]

  cm_name.mito.genes = c(grep("^MT-", rownames(cm_name.emat), value = T), grep("^MTR", rownames(cm_name.emat), value = T))
  cm_name.percent.mito = Matrix::colSums(cm_name.emat[cm_name.mito.genes,])/Matrix::colSums(cm_name.emat)
  hist(cm_name.percent.mito, breaks = 50)
  cm_name.emat <- cm_name.emat[,cm_name.percent.mito<max.percent.mito]

  cm_name.rRNA.genes = c(grep("^RPL", rownames(cm_name.emat), value = T),grep("^RPS", rownames(cm_name.emat), value = T))
  cm_name.percent.rRNA = Matrix::colSums(cm_name.emat[cm_name.rRNA.genes,])/Matrix::colSums(cm_name.emat)
  hist(cm_name.percent.rRNA, xlim = c(0, 1), breaks = 50)
  cm_name.emat <- cm_name.emat[,cm_name.percent.rRNA<max.percent.rRNA]

  cm_name.nmat <- cm_name$intron
  cm_name.smat <- cm_name$spanning
  bcs.cm_name.emat <- colnames(cm_name.emat)
  cm_name.nmat <- cm_name.nmat[,which(colnames(cm_name.nmat) %in% bcs.cm_name.emat), drop = FALSE]
  cm_name.smat <- cm_name.smat[,which(colnames(cm_name.smat) %in% bcs.cm_name.emat), drop = FALSE]
  if (is.null(orig.ident)==TRUE){
    names.bcs.cm_name = as.data.frame(cbind(bcs.cm_name.emat, paste(rep(substr(deparse(substitute(cm_name)), 1, nchar(deparse(substitute(cm_name)))-3),ncol(cm_name.emat)), c(1:ncol(cm_name.emat)), sep=".")))
  }
  else {
    names.bcs.cm_name = as.data.frame(cbind(bcs.cm_name.emat, paste(rep(orig.ident,ncol(cm_name.emat)), c(1:ncol(cm_name.emat)), sep=".")))
  }
  names.bcs.cm_name$V2 <- paste(names.bcs.cm_name$V2,names.bcs.cm_name$bcs.cm_name.emat,sep = "_")
  colnames(names.bcs.cm_name) <- c("bcs", "cm_name")
  cm_name.nmat.bcs <- as.data.frame(colnames(cm_name.nmat))
  colnames(cm_name.nmat.bcs) <- "bcs"
  cm_name.bcs.nmat <- plyr::join(names.bcs.cm_name, cm_name.nmat.bcs, type = "right", by = "bcs")
  colnames(cm_name.emat) <- names.bcs.cm_name$cm_name
  colnames(cm_name.nmat) <- cm_name.bcs.nmat$cm_name
  cm_name.smat.bcs <- as.data.frame(colnames(cm_name.smat))
  colnames(cm_name.smat.bcs) <- "bcs"
  cm_name.bcs.smat <- plyr::join(names.bcs.cm_name, cm_name.smat.bcs, type = "right", by = "bcs")
  colnames(cm_name.smat) <- cm_name.bcs.smat$cm_name
  cm_name = list(emat = cm_name.emat, nmat = cm_name.nmat, smat = cm_name.smat)
  return(cm_name)
}
