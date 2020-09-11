#' calculate 3d distance
#'
#' A helper function for 3d distance
#' @param point0 A point x y z
#' @param point1 A point x y z
#'
#' @return The 3d distance from 0 to 1
#' @export

calc_3d_dist <- function(point0, point1){
  dist = c()
  for(i in 1:nrow(point1)){
    x0 <- point0[i,1]
    y0 <- point0[i,2]
    z0 <- point0[i,3]

    x1 <- point1[i,1]
    y1 <- point1[i,2]
    z1 <- point1[i,3]

    dist[i] <- sqrt(((x1-x0)^2) + ((y1 - y0)^2) + ((z1-z0)^2))
  }
  return(dist)
}

calc_dist_and_replace <- function(template_index, data, max){
  #calculate distances from a template index
  template <- list()
  for(i in 1:length(template_index)){
    ind <- as.numeric(template_index[[i]])

    template[[i]] <- data[[i]][[ind]]
  }

  fulldist <- data
  for(i in 1:length(fulldist)){
    for(j in 1:length(fulldist[[i]])){
      fulldist[[i]][[j]] <- calc_3d_dist(template[[i]], fulldist[[i]][[j]])
    }
  }

  # and remove bad points
  corrected <- data
  for(i in 1:length(fulldist)){
    for(j in 1:length(fulldist[[i]])){
      for(k in 1:length(fulldist[[i]][[j]]))
        if(fulldist[[i]][[j]][k] > max){
          corrected[[i]][[j]][k,] <- template[[i]][k,]
        }
    }
  }
  return(corrected)
}

calc_dist_and_replace_template <- function(template, data, max){
  #calculate distances from a template prespecified
  #template <- list()
  #for(i in 1:length(template_index)){
  #  ind <- as.numeric(template_index[[i]])
  #
  #  template[[i]] <- data[[i]][[ind]]
  #}

  fulldist <- data
  for(i in 1:length(fulldist)){
    if(length(fulldist[[i]]) == 0) next
    for(j in 1:length(fulldist[[i]])){
      fulldist[[i]][[j]] <- calc_3d_dist(template[[i]], fulldist[[i]][[j]])
    }
  }

  # and remove bad points
  a <- 0
  corrected <- data
  for(i in 1:length(fulldist)){
    if(length(fulldist[[i]]) == 0) next
    for(j in 1:length(fulldist[[i]])){
      for(k in 1:length(fulldist[[i]][[j]])){
        if(fulldist[[i]][[j]][k] > max | is.na(fulldist[[i]][[j]][k])){
          a <- a +1
          corrected[[i]][[j]][k,] <- template[[i]][k,]
        }
      }
    }
  }
  cat(a)
  return(corrected)
}

#' calculate the 3d distance and replace with NAs when outside the specified distance
#'
#' @param template the templates to align to
#' @param aligned_data the data post-alignment
#' @param original_data the data before alignment
#' @param max the maximum distance
#'
#' @return Aligned data with NAs replacing the points that are out of bounds
#' @export
calc_dist_and_replace_na <- function(template, aligned_data, original_data, max){
  #calculate distances from a template prespecified
  #template <- list()
  #for(i in 1:length(template_index)){
  #  ind <- as.numeric(template_index[[i]])
  #
  #  template[[i]] <- data[[i]][[ind]]
  #}

  fulldist <- aligned_data
  for(i in 1:length(fulldist)){
    if(length(fulldist[[i]]) == 0) next
    for(j in 1:length(fulldist[[i]])){
      fulldist[[i]][[j]] <- calc_3d_dist(template[[i]], fulldist[[i]][[j]])
    }
  }

  # and remove bad points
  corrected <- original_data
  a <- 0
  for(i in 1:length(fulldist)){
    if(length(fulldist[[i]]) == 0) next
    for(j in 1:length(fulldist[[i]])){
      for(k in 1:length(fulldist[[i]][[j]])){
        if(fulldist[[i]][[j]][k] > max | is.na(fulldist[[i]][[j]][k])){
          corrected[[i]][[j]][k,] <- NA
          a <- a + 1
        }
      }
    }
  }
  cat(a)
  return(corrected)
}

replace_nas_with_template <- function(template, data){


  corrected <- data
  for(i in 1:length(data)){
    if(length(data[[i]]) == 0) next
    for(j in 1:length(data[[i]])){
      for(k in 1:nrow(data[[i]][[j]]))
        if(is.na(data[[i]][[j]][k, 1])){
          corrected[[i]][[j]][k,1:3] <- template[[i]][k,1:3]
        }
    }
  }
  return(corrected)
}

calc_participant_numbers <- function(data){
  a <- 0
  for(i in 1:length(data)){
      a <- a + length(data[[i]])
  }
  return(a)
}

rezero_template_ind <- function(template){
  data <- template
  if(length(data) == 0) next
  for(i in 1:length(data)){

    #inbuilt sanity check
    for(j in 1:nrow(data[[i]])){
      if(data[[i]]$y[j] > (data[[i]]$y[2] + 1.5)){
        data[[i]]$y[j] <- (data[[i]]$y[2] + 1.5)
      }
      if(data[[i]]$y[j] < (data[[i]]$y[3] - 1.5)){
        data[[i]]$y[j] <- (data[[i]]$y[3] - 1.5)
      }
      if(data[[i]]$x[j] < (data[[i]]$x[5] - 1)){
        data[[i]]$x[j] <- (data[[i]]$x[5] - 1)
      }
      if(data[[i]]$z[j] < (data[[i]]$z[4] + 0.5)){
        data[[i]]$z[j] <- (data[[i]]$z[4] + 0.5)
      }
    }
  }
  return(data)
}

#iterative replacement
iterative_replacement <- function(template, aligned_data, original_data, npoints_in_cap, max_threshold){
  n <- calc_participant_numbers(aligned_data)
  ntotal <- n * npoints_in_cap
  fulldist <- aligned_data
  dist <- fulldist
  corrected <- original_data
  int <- c(20, 19, 18, 17, 16, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4.5, 4, 3.5, 3, 2.5, 2, 1.5, 1, .5)
  a <- 0
    for(max in int){
      if(a/ntotal >= max_threshold) next
      cat('it\'s at this level:', max, '\n')
      a <- 0
      #calc dist and replace NA
      for(i in 1:length(fulldist)){
        if(length(fulldist[[i]]) == 0) next
        for(j in 1:length(fulldist[[i]])){
          dist[[i]][[j]] <- calc_3d_dist(template[[i]], fulldist[[i]][[j]])
          # and remove bad points
          for(k in 1:length(dist[[i]][[j]])){

            if(dist[[i]][[j]][k] > max | is.na(dist[[i]][[j]][k])){
              corrected[[i]][[j]][k,] <- NA
              a <- a + 1
            }
          }

        }
      }
      fulldist <- align_to_template2(templates3, corrected, npoints_in_cap)
    }
#max loop
  for(i in 1:length(fulldist)){
    if(length(fulldist[[i]]) == 0) next
    for(j in 1:length(fulldist[[i]])){
      for(k in 1:nrow(fulldist[[i]][[j]])){
        if(is.na(fulldist[[i]][[j]][k, 1])){
          fulldist[[i]][[j]][k,] <- template[[i]][k,]
        }
      }
    }
  }
  cat(a, 'points replaced \n')
  return(fulldist)
}

#like the pointwise replacement in calc_dist_replace_template, but for overall caps
headwise_replacement <- function(template, data, max){
  fulldist <- data
  for(i in 1:length(fulldist)){
    if(length(fulldist[[i]]) == 0) next
    for(j in 1:length(fulldist[[i]])){
      fulldist[[i]][[j]] <- calc_3d_dist(template[[i]], fulldist[[i]][[j]])
      fulldist[[i]][[j]] <- mean(fulldist[[i]][[j]])
    }
  }
  a <- 0
  corrected <- data
  for(i in 1:length(fulldist)){
    if(length(fulldist[[i]]) == 0) next
    for(j in 1:length(fulldist[[i]])){
        if(fulldist[[i]][[j]] > max | is.na(fulldist[[i]][[j]])){
          a <- a +1
          corrected[[i]][[j]] <- template[[i]]
      }
    }
  }
  cat(a)
  return(corrected)
}
