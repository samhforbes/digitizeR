save_templates_by_index <- function(data, template_index){
  
  #get the templates
  new_data <- list()
  for(i in 1:length(template_index)){
    a <- as.numeric(template_index[[i]])
    new_data[[i]] <- data[[i]][[a]]
    
    if(ncol(new_data[[i]]) >3 ){
      #names(new_data[[i]])[c(2,3,4)] <- c('x', 'y', 'z')
      new_data[[i]] <- new_data[[i]][,2:4]
    }
  }
  
  cfiles <- new_data
  #multiply by 10
  cfiles2 <- lapply(cfiles, function(x)
      data.frame(apply(x, 2, function(z) z*10)))
  
  n <- c('x', 'y', 'z')
  cfiles2 <- lapply(cfiles2, setNames, n)
  
  empty <- data.frame(0,0,0)
  names(empty)[c(1:3)] <- c('x', 'y', 'z')
  
  cfiles3 <- lapply(cfiles2, function(x) rbind(x[1:23,],empty,
                                x[24:30,], empty,
                                x[31:37,], empty,
                                x[38:44,], empty,
                                x[45:49,]))
  
  a <- as.character(c('nz:', 'ar:', 'al:', 'cz:', 'iz:', paste('s', 1:16, ':', sep =''), 
                      paste('d', 1:32, ':', sep = '')))
  
  cfiles4 <- lapply(cfiles3, function(x) cbind(a, x))
  
  names(cfiles4) <- names(data)
  
  for(i in names(cfiles4)){
      dir.create(paste('../../Documents/digi_special/',i, sep = ''), recursive = T)
      write.table(cfiles4[[i]], 
                  file = paste('../../Documents/digi_special/' ,i, '/digpts.txt', sep = ''), 
                  row.names = F, col.names = F, quote = F)
  }
}

save_caps <- function(data, path, type = c('NIHVWM', 'Gates', 'HWB')){
  if(type == 'NIHVWM'){
  cfiles <- lapply(data, function(x) 
    lapply(x, function(y) y <- y[,1:3]))
  
  #multiply by 10
  cfiles2 <- lapply(cfiles, function(x)
    lapply(x, function(y) 
      data.frame(apply(y, 2, function(z) z*10))))
  
  empty <- data.frame(0,0,0)
  names(empty)[c(1:3)] <- c('x', 'y', 'z')
  
  cfiles3 <- lapply(cfiles2, function(y) 
    lapply(y, function(x) rbind(x[1:23,],empty,
                                x[24:30,], empty,
                                x[31:37,], empty,
                                x[38:44,], empty,
                                x[45:49,])))
  
  a <- as.character(c('nz:', 'ar:', 'al:', 'cz:', 'iz:', paste('s', 1:16, ':', sep =''), 
                      paste('d', 1:32, ':', sep = '')))
  
  cfiles4 <- lapply(cfiles3, function(x)
    lapply(x, function(y) cbind(a, y)))
  }
  if(type == 'Gates'){
      cfiles <- lapply(data, function(x) 
        lapply(x, function(y) y <- y[,1:3]))
      
      #multiply by 10
      cfiles2 <- lapply(cfiles, function(x)
        lapply(x, function(y) 
          data.frame(apply(y, 2, function(z) z*10))))
      
      
      cfiles3 <- cfiles2
      
      a <- as.character(c('nz:', 'ar:', 'al:', 'cz:', 'iz:', paste('s', 1:12, ':', sep =''), 
                          paste('d', 1:20, ':', sep = '')))
      
      cfiles4 <- lapply(cfiles3, function(x)
        lapply(x, function(y) cbind(a, y)))
  }
  if(type == 'HWB'){
    cfiles <- lapply(data, function(x) 
      lapply(x, function(y) y <- y[,1:3]))
    
    #multiply by 10
    cfiles2 <- lapply(cfiles, function(x)
      lapply(x, function(y) 
        data.frame(apply(y, 2, function(z) z*10))))
    
    
    cfiles3 <- cfiles2
    
    a <- as.character(c('nz:', 'ar:', 'al:', 'cz:', 'iz:', paste('s', 1:8, ':', sep =''), 
                        paste('d', 1:16, ':', sep = '')))
    
    cfiles4 <- lapply(cfiles3, function(x)
      lapply(x, function(y) cbind(a, y)))
  }
  for(i in names(cfiles4)){
    for(j in names(cfiles4[[i]])){
      k = ifelse(substr(j, nchar(j) -1, nchar(j)) == 'Y2', paste(substr(j, 1, nchar(j)-6), 'Y2', sep = ''), substr(j, 1, nchar(j) -4))
      dir.create(paste(path, k, sep = ''), recursive = T)
      write.table(cfiles4[[i]][[j]], 
                  file = paste(path ,k, '/digpts.txt', sep = ''), 
                  row.names = F, col.names = F, quote = F)
    }
  }
}

save_templates <- function(template, data, path, type = c('NIHVWM', 'Gates', 'HWB')){
  
  #get the templates
  if(type == 'NIHVWM'){
  
  cfiles <- template
  #multiply by 10
  cfiles2 <- lapply(cfiles, function(x)
    data.frame(apply(x, 2, function(z) z*10)))
  
  n <- c('x', 'y', 'z')
  cfiles2 <- lapply(cfiles2, setNames, n)
  
  empty <- data.frame(0,0,0)
  names(empty)[c(1:3)] <- c('x', 'y', 'z')
  
  cfiles3 <- lapply(cfiles2, function(x) rbind(x[1:23,],empty,
                                               x[24:30,], empty,
                                               x[31:37,], empty,
                                               x[38:44,], empty,
                                               x[45:49,]))
  
  a <- as.character(c('nz:', 'ar:', 'al:', 'cz:', 'iz:', paste('s', 1:16, ':', sep =''), 
                      paste('d', 1:32, ':', sep = '')))
  
  cfiles4 <- lapply(cfiles3, function(x) cbind(a, x))
  
  names(cfiles4) <- names(data)
  
  for(i in names(cfiles4)){
    dir.create(paste(path, i, sep = ''), recursive = T)
    write.table(cfiles4[[i]], 
                file = paste(path ,i, '/digpts.txt', sep = ''), 
                row.names = F, col.names = F, quote = F)
    }
  }
  if(type == 'Gates'){
    
    cfiles <- template
    #multiply by 10
    cfiles2 <- lapply(cfiles, function(x)
      data.frame(apply(x, 2, function(z) z*10)))
    
    n <- c('x', 'y', 'z')
    cfiles2 <- lapply(cfiles2, setNames, n)
    
    #names(empty)[c(1:3)] <- c('x', 'y', 'z')
    
    cfiles3 <- cfiles2
    
    a <- as.character(c('nz:', 'ar:', 'al:', 'cz:', 'iz:', paste('s', 1:12, ':', sep =''), 
                        paste('d', 1:20, ':', sep = '')))
    
    cfiles4 <- lapply(cfiles3, function(x) cbind(a, x))
    
    names(cfiles4) <- names(data)
    
    for(i in names(cfiles4)){
      dir.create(paste(path, i, sep = ''), recursive = T)
      write.table(cfiles4[[i]], 
                  file = paste(path ,i, '/digpts.txt', sep = ''), 
                  row.names = F, col.names = F, quote = F)
    }
  }
  if(type == 'HWB'){
    
    cfiles <- template
    #multiply by 10
    cfiles2 <- lapply(cfiles, function(x)
      data.frame(apply(x, 2, function(z) z*10)))
    
    n <- c('x', 'y', 'z')
    cfiles2 <- lapply(cfiles2, setNames, n)
    
    #names(empty)[c(1:3)] <- c('x', 'y', 'z')
    
    cfiles3 <- cfiles2
    
    a <- as.character(c('nz:', 'ar:', 'al:', 'cz:', 'iz:', paste('s', 1:8, ':', sep =''), 
                        paste('d', 1:16, ':', sep = '')))
    
    cfiles4 <- lapply(cfiles3, function(x) cbind(a, x))
    
    names(cfiles4) <- names(data)
    
    for(i in names(cfiles4)){
      dir.create(paste(path, i, sep = ''), recursive = T)
      write.table(cfiles4[[i]], 
                  file = paste(path ,i, '/digpts.txt', sep = ''), 
                  row.names = F, col.names = F, quote = F)
    }
  }  
}
