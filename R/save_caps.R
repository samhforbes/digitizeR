#' Save caps to disk as digpts.txt
#'
#' This is useful if you want to use individual caps for each participant. Experimental - the type argument would need adjustment for a specific cap geometry.
#'
#' @param data data to save
#' @param path path to save the data
#' @return Saved digpts.txt to the path in a folder for each participant, ready to use in AtlasViewerGUI
#' @export

save_caps <- function(data, path){

  cap_data <- attr(data, 'digitization')
  landmarks = cap_data$Landmark
  num_source = cap_data$Sources
  num_detector = cap_data$Detector
  short = cap_data$Short

  cfiles <- lapply(data, function(x)
    lapply(x, function(y) y <- y[,1:3]))

  #multiply by 10
  cfiles2 <- lapply(cfiles, function(x)
    lapply(x, function(y)
      data.frame(apply(y, 2, function(z) z*10))))

  empty <- data.frame(0,0,0)
  names(empty)[c(1:3)] <- c('x', 'y', 'z')

#assume four short sources
  #key number is short source + num source + 5 (-1, so 23 if first is 3)
  if(!is.null(short)){
    message('We are assuming 4 short sources. If you have more or less this may not work!')
    d <- num_source + 5
    cfiles3 <- lapply(cfiles2, function(y)
      lapply(y, function(x) rbind(x[1:(short[1] + d - 1),],empty,
                                  x[(short[1] + d):(short[2] + d - 2),], empty,
                                  x[(short[2] + d-1):(short[3] + d -3),], empty,
                                  x[(short[3] + d-2):(short[4] + d -4),], empty,
                                  x[(short[4] + d-3):nrow(x),])))
  }else{
    cfiles3 <- cfiles2
  }

  a <- as.character(c(paste(landmarks, ':', sep = ''), paste('s', 1:num_source, ':', sep =''),
                      paste('d', 1:num_detector, ':', sep = '')))

  cfiles4 <- lapply(cfiles3, function(x)
    lapply(x, function(y) cbind(a, y)))


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
#'
#' @return a directory of folders named with the cap size, each of which contains digpts.txt
#' @export
save_templates <- function(template, data, path){

  if('digi_template' %in% class(template) == F){
    stop('This template is not of class digi_template. Make sure to re-run it')
  }

  cap_data <- attr(template, 'digi_template')
  landmarks = cap_data$Landmark
  num_source = cap_data$Sources
  num_detector = cap_data$Detector
  short = cap_data$Short

  #get the templates

  cfiles <- template
  #multiply by 10
  cfiles2 <- lapply(cfiles, function(x)
    data.frame(apply(x, 2, function(z) z*10)))

  n <- c('x', 'y', 'z')
  cfiles2 <- lapply(cfiles2, setNames, n)

  empty <- data.frame(0,0,0)
  names(empty)[c(1:3)] <- c('x', 'y', 'z')

  if(!is.null(short)){
    message('We are assuming 4 short sources. If you have more or less this may not work!')
    d <- num_source + 5
    cfiles3 <- lapply(cfiles2,  function(x) rbind(x[1:(short[1] + d - 1),],empty,
                                                  x[(short[1] + d):(short[2] + d - 2),], empty,
                                                  x[(short[2] + d-1):(short[3] + d -3),], empty,
                                                  x[(short[3] + d-2):(short[4] + d -4),], empty,
                                                  x[(short[4] + d-3):nrow(x),]))
  }else{
    cfiles3 <- cfiles2
  }

  a <- as.character(c(paste(landmarks, ':', sep = ''), paste('s', 1:num_source, ':', sep =''),
                      paste('d', 1:num_detector, ':', sep = '')))

  cfiles4 <- lapply(cfiles3, function(x) cbind(a, x))

  names(cfiles4) <- names(data)

  for(i in names(cfiles4)){
    dir.create(paste(path, i, sep = ''), recursive = T)
    write.table(cfiles4[[i]],
                file = paste(path ,i, '/digpts.txt', sep = ''),
                row.names = F, col.names = F, quote = F)
    }

}
