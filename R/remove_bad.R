#' Find the best cap to align to
#'
#' This is a helpful step to make the templates which allows visualisation of whether a sensible choice has been made by the algorithm.
#'
#' @param aligned_caps the nested list from count_nested_aligned_caps
#' @return A list of best candidates in each cap size
#' @export

find_greatest_alignment <- function(aligned_caps){
  dd <- aligned_caps
  dd2 <- lapply(dd, function(x) which.max(lengths(x)))
  return(dd2)
}

#' See the number of caps that align with the best candidate
#'
#' This function gets used to check that the best candidate is aligning to enough other caps to be an acceptable choice. If it is too low, the distance should be increased above.
#'
#' @param aligned_caps the nested list from count_nested_aligned_caps
#' @return The number of caps the best candidate aligns with in each cap size
#' @export

greatest_alignment <- function(aligned_caps){
  dd <- aligned_caps
  dd2 <- lapply(dd, function(x) max(lengths(x)))
  return(dd2)
}

#' Adjust the points so they have the right number
#'
#' This is just necessary to take bad templates further along the pipeline.
#' @param data the dataset to adjust
#' @param npoints The correct number of points it should have, but currently doesn't
#' @return A set of caps with the correct number of points (although some points will be bad)
#' @export

adjust_points <- function(data, npoints){
  correct <- data
  for(i in 1:length(correct)){
    if(length(correct[[i]]) == 0) next
    for(j in 1:length(correct[[i]])){
      if(nrow(correct[[i]][[j]]) > npoints){
        correct[[i]][[j]] <- correct[[i]][[j]][1:npoints,]
      }

      if(nrow(correct[[i]][[j]]) < npoints){
        p = nrow(correct[[i]][[j]])
        q = npoints - p
        for(t in 1:q){
          s = p + t
          correct[[i]][[j]][s,] <- correct[[i]][[j]][p,]
        }
      }
    }
  }
  return(correct)
}


remove_bad <- function(original, cleaned){
  for(i in 1:length(cleaned)){
    removed <- which(is.na(match(names(original[[i]]), names(cleaned[[i]]))))
    foo <- original[[i]]
    for(j in 1:length(removed)){
      b <- removed[j]
      original[[i]][[b]] <- NA
    }
    original[[i]] <- original[[i]][!is.na(original[[i]])]
  }
  return(original)
}

keep_bad <- function(original, cleaned){
  for(i in 1:length(cleaned)){
    removed <- which(!is.na(match(names(original[[i]]), names(cleaned[[i]]))))
    foo <- original[[i]]
    for(j in 1:length(removed)){
      b <- removed[j]
      original[[i]][[b]] <- NA
    }
    original[[i]] <- original[[i]][!is.na(original[[i]])]
  }
  return(original)
}

remove_outside_distance <- function(data, templates, max){
  corrected <- norm
  for(i in 1:length(fulldist)){
    for(j in 1:length(fulldist[[i]])){
      for(k in 1:length(fulldist[[i]][[j]]))
        if(fulldist[[i]][[j]][k] > max){
          corrected[[i]][[j]][k,] <- templates[[i]][k,]
        }
    }
  }
}

count_aligned_caps <- function(data, max){
  distances <- data

  for(i in 1:length(distances)){
    for(j in 1:length(distances[[i]])){
      sublist <- list()
      for(k in 1:length(distances[[i]])){

        sublist[[k]] <- calc_3d_dist(data[[i]][[j]], data[[i]][[k]])

      }
      distances[[i]][[j]] <- sublist
    }
  }
  fulldist <- distances
  for(i in 1:length(fulldist)){
    for(j in 1:length(fulldist[[i]])){
      for(k in 1:length(fulldist[[i]][[j]])){

          if(any(fulldist[[i]][[j]][[k]] > max)){
            fulldist[[i]][[j]][[k]] <- NA
          }
      }
    }
  }

  new_distance <- fulldist
  for(i in 1:length(new_distance)){
    for(j in 1:length(new_distance[[i]])){
      new_distance[[i]][[j]] <- length(which(!is.na(new_distance[[i]][[j]])))
    }
  }
  return(new_distance)
}

select_nested_caps_by_npoints <- function(data, npoints){
  correct <- data
  for(i in 1:length(correct)){
    for(j in 1:length(correct[[i]])){
      for(k in 1:length(correct[[i]][[j]])){
      if(nrow(correct[[i]][[j]][[k]]) != npoints){
        correct[[i]][[j]][[k]] <- NA
        }
      }
      correct[[i]][[j]] <- correct[[i]][[j]][!is.na(correct[[i]][[j]])]
    }
  }
  return(correct)
}

adjust_points2 <- function(data, npoints){
  correct <- data
  for(i in 1:length(correct)){
    if(length(correct[[i]]) == 0) next
    for(j in 1:length(correct[[i]])){
      if(nrow(correct[[i]][[j]]) > npoints){
        correct[[i]][[j]] <- correct[[i]][[j]][1:npoints,]
      }

      if(nrow(correct[[i]][[j]]) < npoints){
        p = nrow(correct[[i]][[j]])
        q = npoints - p
        for(t in 1:q){
          s = p + t
          correct[[i]][[j]][s,] <- NA
        }
      }
    }
  }
  return(correct)
}
