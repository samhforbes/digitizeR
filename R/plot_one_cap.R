#' Plot the cap of a single participant
#'
#' @details This is done by numbers, so you choose to plot eg fro the second cap size the fourth participant.
#'
#' @param data the dataset from which to plot
#' @param index an integer indicating the ordinal capsize list you wish to choose from
#' @param part an integer indicating the ordinal participant number you wish to choose in that cap size
#' @param flipped logical indicating whether the cap should be flipped on the z axis for visualisation
#' @return a 3d plot of the points of one cap
#' @export

plot_one_participant <- function(data, index, part, flipped = T){
  #index refers to the index of that cap in the list
  a <- data[[index]][[part]]

  if(ncol(a) >3 ){
    names(a)[c(2,3,4)] <- c('x', 'y', 'z')
  }

  if(flipped == T){
    a <- a %>%
      mutate(z = -z)
  }

  a$Points <- ifelse(as.numeric(row.names(a))<6, 'Landmark', 'Cap')

  plot <- plot_ly(a, x = ~x, y = ~y, z = ~z, color = ~Points, colors = 'Set1') %>%
    add_markers()

  plot
}

#' Plot to see the alignment of landmarks for each cap size
#'
#' This is a way to check that all capsize lists are roughly in the same space
#'
#' @param template_index the results of find_greatest_alignment
#' @param nested_data the resutls of align_all_caps_nested
#' @param flipped logical, if true will diplay cap right way up
#' @return a plot of the landmarks for each cap size
#' @export

plot_visual_alignment <- function(template_index, nested_data, flipped = T){
  #used to be plot_template_by_index
  new_data <- list()
  for(i in 1:length(template_index)){
    a <-as.numeric(template_index[[i]])
    new_data[[i]] <- data[[i]][[a]][[a]]
  }

  plotting_data <- new_data
  #n <- names(new_data)
  #for(i in 1:length(plotting_data)){
  #  plotting_data[[i]]$cap <- n[i]
  #}
  n <- c('x', 'y', 'z')

  plotting_data <- lapply(plotting_data, function(x) x[1:5,])
  v <- c('nz', 'ar', 'al', 'cz', 'iz')
  plotting_data <- lapply(plotting_data, function(x) cbind(x,v))

  true_data <- do.call(rbind, plotting_data)
  names(true_data)[c(1:3)] <- n

  if(flipped == T){
    a <- a %>%
      mutate(z = -z)
  }

  plot <- plot_ly(data = true_data, x = ~x, y = ~y, z= ~z, color = ~v) %>%
    add_markers()
  plot
}

#' Plot a template from the template dataset to see how they look
#'
#' This is a way to plot all the points in a template to see the spread of digipoints.
#'
#' @param template the tamplate dataset
#' @param index the numerical index of which cap size to choose
#' @param flipped logical, if true will diplay cap right way up
#' @return a full plot
#' @export

plot_template <- function(template, index, flipped = T){
  a <- template[[index]]

  a$Points <- ifelse(as.numeric(row.names(a))<6, 'Landmark', 'Cap')

  if(flipped == T){
    a <- a %>%
      mutate(z = -z)
  }

  plot <- plot_ly(a, x = ~x, y = ~y, z = ~z, color = ~Points, colors = 'Set1') %>%
    add_markers()

  plot
}

# plot_one <- function(data, index){
#   #index refers to the index of that cap in the list
#   a <- data[[index]]
#
#   plot <- plot_ly(a, x = ~x, y = ~y, z = ~z) %>%
#     add_markers()
#
#   plot
# }


