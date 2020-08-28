make_landmarks <- function(original, num_landmarks){
  cut_list <- lapply(original, function(x) 
    lapply(x, function(y) select(y, one_of(c('V2', 'V3', 'V4')))))
  
  landmarks <- lapply(cut_list, function(x) 
    lapply(x, function(y) y[1:num_landmarks,]))
  
  land_norm <- landmarks
  
  land_norm <- lapply(land_norm, function(x)
    lapply(x, function(y)
      data.frame(apply(y, 2, function(z) as.numeric(z)))))
  
  return(land_norm)
}

align_landmarks <- function(land_norm, num_landmarks, num_aligned){
  for(i in 1:length(land_norm)){
    
    for(j in 1:length(land_norm[[i]])){
      
      land_norm[[i]][[j]][1:num_landmarks,1:3] <- modified_kabsch(land_norm[[i]][[j]][1:num_aligned,1:3], land_norm[[i]][[j]][1:num_aligned,1:3], land_norm[[i]][[j]][1:num_landmarks,1:3])
      
    }
  }
  n <- c('x', 'y', 'z')
  
  land_norm_new <- lapply(land_norm, function(x)
    lapply(x, setNames, n))
  
  
  v <- as.character(c('nz', 'ar', 'al', 'cz', 'iz'))
  
  land_plot <- lapply(land_norm_new, function(x)
    lapply(x, function(y) cbind(y,v)))
  
  return(land_plot)
}