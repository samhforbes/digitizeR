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

#' Calculate the 3d distance and replace points outside with the template
#'
#' @param template the template data to replace with
#' @param data the data to replace
#' @param max the maximum distance
#'
#' @return data where bad points are replaced with the template
#' @export

calc_dist_and_replace_template <- function(template, data, max){

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
  cat(a, '\n')
  class(corrected) <- class(data)
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
  cat(a, '\n')
  class(corrected) <- class(aligned_data)
  return(corrected)
}

#' replace NAs with template
#'
#' A simple replacement function for NAs
#' @param template template data
#' @param data the cap dataset
#'
#' @return the data with NAs replaced with values from the template
#' @export

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
  class(corrected) <- class(data)
  return(corrected)
}

#' Calculate participant numbers
#'
#' @param data the data set
#' @return the number of participants in the dataset
#' @export

calc_participant_numbers <- function(data){
  a <- 0
  for(i in 1:length(data)){
      a <- a + length(data[[i]])
  }
  return(a)
}

#' iterative replacement
#'
#' Iteratively replaces with nas until the end when replaced by template.
#' Reduced by integer until below 5, when it is .5
#' This function is experimental.
#' @param template the template dataset
#' @param aligned_data the data after alignment
#' @param original_data the original dataset
#' @param npoints_n_cap the number of points in the cap.
#' @param max_threshold the max proportion of points you are willing to replace
#'
#' @return Data aligned to the template in an iterative fashion
#' @export
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
      cat('it\'s at this level:', max,': ',a/ntotal, ' <=', max_threshold, '\n')
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
      fulldist <- align_to_template2(template, corrected, npoints_in_cap)
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
  class(fulldist) <- class(aligned_data)
  attr(fulldist, 'digitization') <- attr(aligned_data, 'digitization')
  return(fulldist)
}

#' Headwise replacement
#'
#' replaces by a whole cap rather than individual points.
#' When the average distance for the cap goes beyond the max value, the whole cap is replaced.
#' This function is experimental.
#'
#' @param template the template dataset
#' @param data the dataset to replace
#' @param max the maximum distance to accept
#'
#' @return The dataset with caps replaced with template caps when poor
#' @export
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
  class(corrected) <- class(data)
  attr(corrected, 'digitization') <- attr(data, 'digitization')
  return(corrected)
}

#' three step replacement
#'
#' This function is Experimental. Replaces points greater than the acceptable distance 3 times and replaces with NA to realign and try again.
#' At the end replaces with template data where outside of the required distance.
#' @param template The template dataset to align to
#' @param aligned_data the data in a preliminary aligned form
#' @param original_data the data after selecting by npoints
#' @param npoints The number of points in the cap (to align by)
#' @param dist1 Numeric. The first distance to align at
#' @param dist2 Numeric. The second distance to align at
#' @param dist3 Numeric. The third distance to align at
#' @return a dataset that has been realigned three times at different distances to replace hopefully no more than the needed number of points only.
#' @export

threestep_alignment <- function(template, aligned_data, original_data, npoints, dist1, dist2, dist3){
  message('Removing at ', dist1, '\n')
  clean_data <- calc_dist_and_replace_na(template, aligned_data, original_data, dist1)
  #re-align with NAs in

  aligned_data_2 <- align_to_template2(template, clean_data, npoints)
  #aligned_data_3 <- replace_nas_with_template(templates3, aligned_data_2)
  #try again
  message('Removing at ', dist2, '\n')
  clean_data_2 <- calc_dist_and_replace_na(template, aligned_data_2, original_data, dist2)

  aligned_data_3 <- align_to_template2(template, clean_data_2, npoints)
  #aligned_data_5 <- replace_nas_with_template(templates3, aligned_data_4)
  #and final replacement
  message('Removing at ', dist3, '\n')
  clean_data_3 <- calc_dist_and_replace_template(template, aligned_data_3, dist3)

  class(clean_data_3) <- class(aligned_data)
  attr(clean_data_3, 'digitization') <- attr(aligned_data, 'digitization')
  return(clean_data_3)
}
