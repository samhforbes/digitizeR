#' modified version of the kabsch algorithm
#'
#' adapted to allow application of the matrix to a third dataset. Adapted from the kabsch function used in this package, original site listed there.
#'
#' @param pm a matrix
#' @param qm a matrix
#' @param om a third matrix
#'
#' @return om transposed.
#' @export

modified_kabsch <- function(pm, qm, om) {
  pm_dims <- dim(pm)
  if (!all(dim(qm) == pm_dims)) {
    stop(call. = TRUE, "Point sets must have the same dimensions")
  }
  # The rotation matrix will have (ncol - 1) leading ones in the diagonal
  diag_ones <- rep(1, pm_dims[2] - 1)

  # center the points
  pm <- scale(pm, center = TRUE, scale = FALSE)
  qm <- scale(qm, center = TRUE, scale = FALSE)

  am <- crossprod(pm, qm)

  svd_res <- svd(am)
  # use the sign of the determinant to ensure a right-hand coordinate system
  d <- determinant(tcrossprod(svd_res$v, svd_res$u))$sign
  dm <- diag(c(diag_ones, d))

  # rotation matrix
  um <- svd_res$v %*% tcrossprod(dm, svd_res$u)

  #om needs to be a matrix
  om <- as.matrix(om)
  # Rotate and then translate to the original centroid location of pm
  sweep(t(tcrossprod(um, om)), 2, -attr(pm, "scaled:center"))
}

#' Align a dataset to the template for each cap size
#'
#' This is done only for a full set of alignments, in other occasions you can use align_to_template2
#'
#' @param template The template dataset
#' @param data the data output of select_caps_by_npoints
#' @param num_aligned the number of points to use in the alignment
#'
#' @return the listed dataset aligned to the template in each cap size
#' @export

align_to_template <- function(template, data, num_aligned){
  if(ncol(data[[1]][[2]]) > 3){
  normalised <- lapply(data, function(x)
    lapply(x, function(y) y[,2:4]))
  } else{normalised <- data}

  for(i in 1:length(normalised)){
    if(length(normalised[[i]]) == 0) next
    for(j in 1:length(normalised[[i]])){
      if(any(is.na(normalised[[i]][[j]][1:num_aligned,])) == TRUE){
        normalised[[i]][[j]][1:num_aligned,] <- template[[i]][1:num_aligned,]
      }
        normalised[[i]][[j]] <- data.frame(modified_kabsch(template[[i]][1:num_aligned,], normalised[[i]][[j]][1:num_aligned,], normalised[[i]][[j]]))

    }
  }

  n <- c('x', 'y', 'z')

  normalised <- lapply(normalised, function(x)
    lapply(x, setNames, n))
}

#' a second modified version of the kabsch algorithm to work on a subset of a matrix.
#'
#' @param pm a matrix
#' @param qm a matrix
#' @param om a third matrix
#'
#' @return a transposed om
#' @export

modified_kabsch2 <- function(pm, qm, om) {
  pm_dims <- dim(pm)
  if (!all(dim(qm) == pm_dims)) {
    stop(call. = TRUE, "Point sets must have the same dimensions")
  }
  # The rotation matrix will have (ncol - 1) leading ones in the diagonal
  diag_ones <- rep(1, pm_dims[2] - 1)

  pm_blank <- which(is.na(pm[1]))
  qm_blank <- which(is.na(qm[1]))

  nam <- names(pm)[1]
  nam2 <- names(qm)[1]

  if(length(qm_blank) >= 1){
    pm2 <- pm[-qm_blank,]
  }else{
    pm2 <- pm
  }
  pm2 <- pm2[!is.na(pm2[[nam]]),]

  if(length(pm_blank) >= 1){
    qm2 <- qm[-pm_blank,]
  }else{
    qm2 <- qm
  }
  qm2 <- qm2[!is.na(qm2[[nam2]]),]

  # center the points
  pm2 <- scale(pm2, center = TRUE, scale = FALSE)
  qm2 <- scale(qm2, center = TRUE, scale = FALSE)

  am <- crossprod(pm2, qm2)

  svd_res <- svd(am)
  # use the sign of the determinant to ensure a right-hand coordinate system
  d <- determinant(tcrossprod(svd_res$v, svd_res$u))$sign
  dm <- diag(c(diag_ones, d))

  # rotation matrix
  um <- svd_res$v %*% tcrossprod(dm, svd_res$u)

  #add the new part where we only work on a subset of om
  om_blank <- which(is.na(om[1]))
  om$Index <- 1:nrow(om)

  if(length(om_blank) >= 1){
    om2 <- om[-om_blank,]
  }else{
    om2 <- om
  }

  inni <- om2$Index
  inni <- data.frame(inni)

  om2$Index <- NULL
  #om needs to be a matrix
  om2 <- scale(om2, center = TRUE, scale = FALSE)
  # Rotate and then translate to the original centroid location of pm
  out <- sweep(t(tcrossprod(um, om2)), 2, -attr(pm2, "scaled:center"))

  out2 <- data.frame(out)
  out2 <- cbind(out2, inni)
  out3 <- rename(out2, Index = inni)

  full_index <- om$Index
  full_index2 <- data.frame(full_index)
  names(full_index2)[1] <- 'Index'
  out_data <- left_join(full_index2, out3, by = 'Index')

  out_data$Index <- NULL

  return(out_data)
}

#' Align a partial dataset to the template for each cap size
#'
#' This is used for a partial set of alignments if needed.
#'
#' @param template The template dataset
#' @param data the data output of select_caps_by_npoints
#' @param num_aligned the number of points to use in the alignment
#'
#' @return the listed dataset aligned to the template in each cap size
#' @export

align_to_template2 <- function(template, data, num_aligned){
  if(ncol(data[[1]][[2]]) > 3){
    normalised <- lapply(data, function(x)
      lapply(x, function(y) y[,2:4]))
  } else{normalised <- data}

  for(i in 1:length(normalised)){
    if(length(normalised[[i]]) == 0) next
    for(j in 1:length(normalised[[i]])){
     # if(any(is.na(normalised[[i]][[j]][1:num_aligned,])) == TRUE){
      #  normalised[[i]][[j]][1:num_aligned,] <- template[[i]][1:num_aligned,]
      #}
      blanks <- which(is.na(normalised[[i]][[j]][1]))
      num_aligned2 <- num_aligned - length(blanks)
      normalised[[i]][[j]] <- modified_kabsch2(template[[i]][1:num_aligned,], normalised[[i]][[j]][1:num_aligned,], normalised[[i]][[j]])

    }
  }

  n <- c('x', 'y', 'z')

  normalised <- lapply(normalised, function(x)
    lapply(x, setNames, n))
}
