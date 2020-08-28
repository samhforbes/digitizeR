# make_templates <- function(x){
#   a <- unname(x)
#   
#   a <- lapply(a, function(y)
#     lapply(y, function(z) unname(z)))
#   #lapply misses the last element for now
#   a[length(a)] <- NULL
#   
#   a <- do.call(rbind.data.frame, a)
#   
#   template <- a %>% group_by(v) %>%
#     summarise_each(funs(mean(., na.rm = T)), x, y, z)
#   v <- as.character(c('nz', 'ar', 'al', 'cz', 'iz'))
#   las <- data.frame(v)
#   las$v <- as.character(las$v)
#   template$v <- as.character(template$v)
#   template <- merge(las, template, by = 'v', sort = F)
#   template <- dplyr::select(template, x, y, z, v)
#   
#   return(template)
# }
# 
# make_full_templates <- function(data, length){
#   v2 <- as.character(c(paste('v',seq(1,length), sep = '')))
# 
#   x <- lapply(data, function(y) cbind(y,v2))
#   a <- unname(x)
# 
#   a <- lapply(a, function(y)
#     lapply(y, function(z) unname(z)))
#   #lapply misses the last element for now
#   a[length(a)] <- NULL
# 
#   a <- do.call(rbind.data.frame, a)
# 
# 
#   template <- a %>% group_by(v2) %>%
#     summarise_each(funs(mean(., na.rm = T)), x, y, z)
# 
#   las <- data.frame(v2)
#   las$v2 <- as.character(las$v2)
#   template$v2 <- as.character(template$v2)
#   template <- merge(las, template, by = 'v2', sort = F)
#   template <- dplyr::select(template, x, y, z, v2)
# 
#   return(template)
#}
# 
# make_gates_templates <- function(x){
#   v2 <- as.character(c(paste('v',seq(1,37), sep = '')))
#   
#   x <- lapply(x, function(y) cbind(y,v2))
#   a <- unname(x)
#   
#   a <- lapply(a, function(y)
#     lapply(y, function(z) unname(z)))
#   #lapply misses the last element for now
#   a[length(a)] <- NULL
#   
#   a <- do.call(rbind.data.frame, a)
#   
#   
#   template <- a %>% group_by(v2) %>%
#     summarise_each(funs(mean(., na.rm = T)), x, y, z)
#   
#   las <- data.frame(v2)
#   las$v2 <- as.character(las$v2)
#   template$v2 <- as.character(template$v2)
#   template <- merge(las, template, by = 'v2', sort = F)
#   template <- dplyr::select(template, x, y, z, v2)
#   
#   return(template)
# }

make_templates <- function(template_index, data){
  
  new_templates <- list()
  for(i in 1:length(template_index)){
    ind <- as.numeric(template_index[[i]])
    
    new_templates[[i]] <- data[[i]][[ind]]
  }
  
  new_templates <- lapply(new_templates, function(x) x[,2:4])
  
  return(new_templates)
}

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
