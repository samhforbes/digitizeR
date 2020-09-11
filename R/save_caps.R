#' Save caps to disk as digpts.txt
#'
#' This is useful if you want to use individual caps for each participant. Experimental - the type argument would need adjustment for a specific cap geometry.
#'
#' @param data data to save
#' @param path path to save the data
#' @param type to save the data for a specific cap geometry.
#' @return Saved digpts.txt to the path in a folder for each participant, ready to use in AtlasViewerGUI
#' @export

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

#' Save template output
#'
#' This function is experimental and will write the caps as digpts.txt to disk.
#' Note that the type argument would need adjustment for your cap geometry, the saving function also multipleis by 10 to go to mm for AtlasViewerGUI
#'
#' @param template list of template caps
#' @param data the dataset aligned to the templates - for naming purposes only
#' @param path the path to save the digpts.txt files
#' @param type will need adjustment for a specific geometry.
#'
#' @return a directory of folders named with the cap size, each of which contains digpts.txt
#' @export
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
