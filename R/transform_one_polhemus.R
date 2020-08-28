transform_one_polhemus <- function(filename, num.sources = 49){
  
  library(dplyr)
  
  data <- read.table(filename, header = F)
  if (length(data$V1) == num.sources){
    data_short <- select(data, c(2, 3, 4))
    #rescale to mm
    data_short <- data_short*10
    
    #add empty rows
    empty <- data.frame(0,0,0)
    names(empty)[c(1:3)] <- c('V2', 'V3', 'V4')
    
    data_short2 <- rbind(data_short[1:23,],empty,
          data_short[24:30,], empty,
          data_short[31:37,], empty,
          data_short[38:44,], empty,
          data_short[45:49,])
    
    #add marker names
    a<- c('nz:', 'ar:', 'al:', 'cz:', 'iz:', paste('s', 1:16, ':', sep =''), 
          paste('d', 1:32, ':', sep = ''))
    
    a2 <- as.character(a)
    
    new_data <- cbind(a2,data_short2)
    
    write.table(new_data, file = 'digpts.txt', row.names = F, col.names = F, quote = F)
    print('File successfully transformed and saved in working directory')
  }
  else{
    print('Dataframe does not contain correct number of sources')
  }
}
