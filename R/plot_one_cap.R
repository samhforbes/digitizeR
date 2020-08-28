plot_one_cap <- function(data, index){
  #index refers to the index of that cap in the list
  a <- data[[index]]
  
  a <- lapply(a, function(x) x[1:5,])
  n <- c('x', 'y', 'z')
  a <- lapply(a, setNames, n)
  
  v <- c('nz', 'ar', 'al', 'cz', 'iz')
  a <- lapply(a, function(x) cbind(x,v))
  
  a <- unname(a)
  a <- lapply(a, function(x)
    lapply(x, function(y) unname(y)))
  #last one fails
 # a[length(a)] <- NULL
  
  
  
  a <- do.call(rbind.data.frame, a)
  a$v <- as.character(a$v)
  
  plot <- plot_ly(a, x = ~x, y = ~y, z = ~z, color = ~v) %>%
    add_markers()
  
  plot
}

plot_template <- function(data, index){
  a <- data[[index]]
  
  a$Points <- ifelse(as.numeric(row.names(a))<6, 'Landmark', 'Cap')
  plot <- plot_ly(a, x = ~x, y = ~y, z = ~z, color = ~Points, colors = 'Set1') %>%
    add_markers()
  
  plot
}

plot_one_participant <- function(data, index, part){
  #index refers to the index of that cap in the list
  a <- data[[index]][[part]]
  
  if(ncol(a) >3 ){
  names(a)[c(2,3,4)] <- c('x', 'y', 'z')
  }
  
  a$Points <- ifelse(as.numeric(row.names(a))<6, 'Landmark', 'Cap')
  
  plot <- plot_ly(a, x = ~x, y = ~y, z = ~z, color = ~Points, colors = 'Set1') %>%
    add_markers()
  
  plot
}

plot_one <- function(data, index){
  #index refers to the index of that cap in the list
  a <- data[[index]]
  
  plot <- plot_ly(a, x = ~x, y = ~y, z = ~z) %>%
    add_markers()
  
  plot
}

plot_templates_by_index <- function(template_index, data){
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
  
  plot <- plot_ly(data = true_data, x = ~x, y = ~y, z= ~z, color = ~v) %>%
    add_markers()
  plot
}
