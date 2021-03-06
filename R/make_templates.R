#' create partial preliminary templates from data
#'
#' This is used as a first step before creating the final template.
#' The idea is just to check we are on the right track.
#'
#' @param template_index output of find_greatest_alignment
#' @param data raw data output of select_caps_by_npoints
#' @return one sample cap for each cap size
#' @export

make_partial_templates <- function(template_index, data){
  #used to be make_templates
  new_templates <- list()
  for(i in 1:length(template_index)){
    ind <- as.numeric(template_index[[i]])

    new_templates[[i]] <- data[[i]][[ind]]
  }

  new_templates <- lapply(new_templates, function(x) x[,2:4])

  return(new_templates)
}

#' Make final or full templates after the partial templates
#'
#' The expectation is that this is used after making partial templates and aligning to them. This then takes the output of those functions
#'
#' @param data the dataset. In normal circumstances should be the output of align_to_template to the partial template.
#' @param template_index Output of find_greatest_alignment
#' @param distances Output of count_nested_aligned_caps
#' @param length The number of points that exist in the cap
#' @return a template in each cap size
#' @export

make_full_templates <- function(data, template_index, distances, length){

#  data <- lapply(data, function(x)
#    lapply(x, function(y) y[,2:4]))

  data_list <- list()
  for(i in 1:length(template_index)){
    nam <- names(template_index[[i]])
    caps <- list()
    caps <- distances[[i]][[nam]]

    mylist <- list()
    for(j in 1:length(caps)){
      k <- caps[[j]]
      mylist[[j]] <- data[[i]][[k]]
    }
    data_list[[i]] <- mylist
  }

  #then make the means
  data_list2 <- data_list
  for(i in 1:length(data_list2)){
    v2 <- as.character(c(paste('v',seq(1,length), sep = '')))
       x <- lapply(data_list[[i]], function(y) cbind(y,v2))

       a <- do.call(rbind.data.frame, x)


         template <- a %>% group_by(v2) %>%
           #summarise_each(funs(mean(., na.rm = T)), x, y, z)
           summarise(x = mean(x, na.rm = T),
                     y = mean(y, na.rm = T),
                     z = mean(z, na.rm = T))

         las <- data.frame(v2)
         las$v2 <- as.character(las$v2)
         template$v2 <- as.character(template$v2)
         template <- merge(las, template, by = 'v2', sort = F)
         template <- dplyr::select(template, x, y, z)

         data_list2[[i]] <- template
  }
  return(data_list2)
}

#' Rezero template
#'
#' This is a helper function to allow bringing the z value of the cz point back to zero so you are largely in the same space. It also sanity checks for outlier points.
#'
#' @param template The template dataset (usually the output of make_full_templates)
#' @return Slightly cleaned up version of templates
#' @export

rezero_template <- function(template){
  data <- template
  if(length(data) == 0) next
  for(i in 1:length(data)){
    data[[i]]$z <- data[[i]]$z - data[[i]]$z[4]

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
    }
  }
  return(data)
}

#' rezero and sanity check templates
#'
#' Contains extra sanity checks that are more conservative than rezero_template
#'
#' @param template the template dataset
#'
#' @return templates such that the CZ is at 0, and wide points are moved in
#' @export
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
      if(data[[i]]$z[j] < (data[[i]]$z[4] - 1)){
        data[[i]]$z[j] <- (data[[i]]$z[4] - 1)
      }
    }
  }
  class(data) <- class(template)
  return(data)
}
