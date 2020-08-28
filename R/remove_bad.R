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

find_greatest_alignment <- function(aligned_caps){
  dd <- aligned_caps
  dd2 <- lapply(dd, function(x) which.max(lengths(x)))
  return(dd2)
}

greatest_alignment <- function(aligned_caps){
  dd <- aligned_caps
  dd2 <- lapply(dd, function(x) max(lengths(x)))
  return(dd2)
}

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
