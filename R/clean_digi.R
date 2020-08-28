get_cap_means <- function(x){
  a <- unname(x)
  
  a <- lapply(a, function(y)
    lapply(y, function(z) unname(z)))
  #lapply misses the last element for now
  #a[length(a)] <- NULL
  
  a <- do.call(rbind.data.frame, a)
  
  # min <- a %>% group_by(v) %>%
  #   summarise_each(funs(mean(., na.rm = T) - 3), x, y, z)
  # v <- as.character(c('nz', 'ar', 'al', 'cz', 'iz'))
  # las <- data.frame(v)
  # las$v <- as.character(las$v)
  # min$v <- as.character(min$v)
  # min2 <- merge(las, min, by = 'v', sort = F)
  # min2 <- dplyr::select(min2, x, y, z, v)
  # 
  # max <- a %>% group_by(v) %>%
  #   summarise_each(funs(mean(., na.rm = T) + 3), x, y, z) 
  # v <- as.character(c('nz', 'ar', 'al', 'cz', 'iz'))
  # las <- data.frame(v)
  # las$v <- as.character(las$v)
  # max$v <- as.character(max$v)
  # max2 <- merge(las, max, by = 'v', sort = F)
  # max2 <- dplyr::select(max2, x, y, z, v)
  
  template <- a %>% group_by(v) %>%
    summarise_each(funs(mean(., na.rm = T)), x, y, z)
  v <- as.character(c('nz', 'ar', 'al', 'cz', 'iz'))
  las <- data.frame(v)
  las$v <- as.character(las$v)
  template$v <- as.character(template$v)
  template <- merge(las, template, by = 'v', sort = F)
  template <- dplyr::select(template, x, y, z, v)
  
  return(template)
  # #now compare
  # below_min <- lapply(x, function(y) y < min2)
  # below_min2 <- lapply(below_min, function(y) 
  #   apply(y, 2, sum))
  # below_min3 <- lapply(below_min2, function(y) sum(y, na.rm = T))
  # 
  # for(i in 1:length(below_min3)){
  #   if(below_min3[[i]] > 0){
  #     #cat(below_min3[[i]])
  #     x[[i]] <- NULL
  #   }
  # }
  # 
  # above_max <- lapply(x, function(y) y > max2)
  # above_max2 <- lapply(above_max, function(y) 
  #   apply(y, 2, sum))
  # above_max3 <- lapply(above_max2, function(y) sum(y, na.rm = T))
  # 
  # for(i in 1:length(above_max3)){
  #   if(above_max3[[i]] > 0){
  #     #cat(above_max3[[i]])
  #     x[[i]] <- NULL
  #   }
  # }
  # return(x)
}
