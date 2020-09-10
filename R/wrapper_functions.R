#' select caps to use by a certain number of points
#'
#' @details take any number of nested caps and select only the ones with the right number of points.
#' @param data the data in list format from read_in_caps()
#' @param npoints the correct number of points without which the digi is invalid.
#' @return the data list containing only the valid digis
#' @export

select_caps_by_npoints <- function(data, npoints){
  correct <- data
  for(i in 1:length(correct)){
    for(j in 1:length(correct[[i]])){
      if(nrow(correct[[i]][[j]]) != npoints){
        correct[[i]][[j]] <- NA
      }
    }
    correct[[i]] <- correct[[i]][!is.na(correct[[i]])]
  }
  return(correct)
}

#' select only the caps without a number of points
#'
#' this is useful on the occasions when you want to detail with caps that are obviously wrong because they miss a number of points.
#' @param data the overall dataset to pull from
#' @param npoints the number of points you want the data to NOT have
#' @return the subset of data that misses points
#' @export

select_caps_without_npoints <- function(data, npoints){
  correct <- data
  for(i in 1:length(correct)){
    for(j in 1:length(correct[[i]])){
      if(nrow(correct[[i]][[j]]) == npoints){
        correct[[i]][[j]] <- NA
      }
    }
    correct[[i]] <- correct[[i]][!is.na(correct[[i]])]
  }
  return(correct)
}

#' Align all nested caps to all other caps in a cap size
#'
#' @details use the modified kabsch to align every cap to every other cap in a certain size.
#' Fairly intensive for large data sets so use wisely.
#'
#' @param data the digitizeR nested data list
#' @param num_aligned the number of points you want to use to do the alignment
#' @return a list of lists which contains every participants rotation to every other participant within each cap size
#' @export


align_all_caps_nested <- function(data, num_aligned){
  cut_list <- lapply(data, function(x)
    lapply(x, function(y) select(y, one_of(c('V2', 'V3', 'V4')))))

  #align it
  land_norm <- cut_list
  land_norm2 <- land_norm
  for(i in 1:length(land_norm)){
    for(j in 1:length(land_norm[[i]])){
      sub_list <- list()
      land_norm2[[i]][[j]] <- land_norm[[i]]
      land_norm2[[i]][[j]][[j]] <- land_norm[[i]][[j]]
      for(k in 1:length(land_norm2[[i]][[j]])){

        land_norm2[[i]][[j]][[k]] <- data.frame(modified_kabsch(cut_list[[i]][[j]][1:num_aligned, 1:3], land_norm2[[i]][[j]][[k]][1:num_aligned, 1:3], land_norm2[[i]][[j]][[k]][,1:3]))

      }
    }
  }
  return(land_norm2)
}

#' count all caps that align with each participant in each cap size
#'
#' @details This works on the basis of a maximum acceptable distance given by max.
#'
#' @param nested_data the dataset which results from align_all_caps_nested
#' @param original_data The listed caps after using select_caps_by_npoints
#' @param max The maximum acceptable distance in cm
#' @param points The number of points that were used in alignment previously
#'
#' @return a list of participants who align with any other participant
#' @export

count_nested_aligned_caps <- function(nested_data, original_data, max, points){
  distances <- nested_data

  cut_data <- lapply(original_data, function(x)
    lapply(x, function(y) select(y, one_of(c('V2', 'V3', 'V4')))))

  cut_data <- select_caps_by_npoints(original_data, points)

  for(i in 1:length(cut_data)){
    for(j in 1:length(cut_data[[i]])){
      sublist <- list(list())
      for(l in 1:length(distances[[i]][[j]])){

        sublist[[l]] <- calc_3d_dist(distances[[i]][[j]][[j]], distances[[i]][[j]][[l]])

      }
      distances[[i]][[j]] <- sublist
      names(distances[[i]][[j]]) <- names(distances[[i]])
    }
  }

  fulldist <- distances
  for(i in 1:length(fulldist)){
    for(j in 1:length(fulldist[[i]])){
      for(k in 1:length(fulldist[[i]][[j]])){
        # for(l in 1:length(fulldist[[i]][[j]][[k]])){

        if(any(fulldist[[i]][[j]][[k]] > max)){
          fulldist[[i]][[j]][[k]] <- NA
          #   }
        }
      }
    }
  }

  new_distance <- fulldist
  for(i in 1:length(new_distance)){
    for(j in 1:length(new_distance[[i]])){
      # for(k in 1:length(new_distance[[i]][[j]])){
      new_distance[[i]][[j]] <- names(which(!is.na(new_distance[[i]][[j]])))
      #  }
    }
  }
  return(new_distance)
}
