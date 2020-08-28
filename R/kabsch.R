#' Kabsch Algorithm
#' 
#' Aligns two sets of points via rotations and translations.
#' 
#' Given two sets of points, with one specified as the reference set,
#' the other set will be rotated so that the RMSD between the two is minimized.
#' The format of the matrix is that there should be one row for each of
#' n observations, and the number of columns, d, specifies the dimensionality
#' of the points. The point sets must be of equal size and with the same
#' ordering, i.e. point one of the second matrix is mapped to point one of
#' the reference matrix, point two of the second matrix is mapped to point two 
#' of the reference matrix, and so on.
#'   
#' @param pm n x d matrix of reference points.
#' @param qm n x d matrix of points to align to to \code{pm}
#' @return Matrix \code{qm} rotated and translated so that the ith point 
#'  is aligned to the ith point of \code{pm} in the least-squares sense.
#' @references
#' \url{https://en.wikipedia.org/wiki/Kabsch_algorithm}
kabsch <- function(pm, qm) {
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
  
  # Rotate and then translate to the original centroid location of pm
  sweep(t(tcrossprod(um, qm)), 2, -attr(pm, "scaled:center"))
}