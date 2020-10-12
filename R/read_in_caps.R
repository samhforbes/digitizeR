#' read in caps
#'
#' @param digipath path to digitizations as a string
#' @param pattern a pattern in files passed to list.files as a string
#' @param capsize the path to the cap size document as a string
#' @param landmarks the refpoints to be adjusted for cap geometry as a vector
#' @param num_source numeric value of the number of sources
#' @param num_detector numeric value of the number of detectors
#' @param short a vector indicating the numeric value of short sources, if any
#' @param subchar a vector indicating the characters from the ID to use, passed to substr
#' @return a list of digitizations in dataframe form
#' @export
#'
#' @details currently in beta form for generalisation to other projects. For argument "type", running this depends on locations of short sources and layout of sources vs detectors.

read_in_caps <- function(digipath, pattern, capsize, landmarks = c('nz', 'ar', 'al', 'cz', 'iz'), num_source = 16, num_detector = 32, short = c(3, 11, 19, 27), subchar = c(1, 12)){

  capsize <- read.csv(capsize, stringsAsFactors = F)

  names(capsize)[c(1,2,3)] <- c('ID', 'cap', 'head')
  capsize$ID <- as.character(capsize$ID)
  capsize <- capsize[!is.na(capsize$cap),]
  # if(type == 'NIHVWM'){
  # capsize$age <- ifelse(grepl('30NIHVWM', capsize$ID), '30', '06')
  # }
  # if(type == 'HWB'){
  #   capsize$age <- ifelse(grepl('32HWB', capsize$ID), '32', '54')
  # }

  file.list <- list.files(path = digipath, pattern = pattern, recursive = TRUE)
  #make into dataframe for merging
  dataframe <- data.frame(file.list)
  # if(type =='NIHVWM'){
  #   dataframe$ID <- substr(dataframe$file.list, 1, 12)
  # }
  # if(type =='Gates'){
  #   dataframe$ID <- substr(dataframe$file.list, 1, 9)
  # }
  # if(type =='HWB'){
  #   dataframe$ID <- substr(dataframe$file.list, 1, 11)
  # }
  if(!is.null(subchar)){
    dataframe$ID <- substr(dataframe$file.list, subchar[1], subchar[2])
  }else{
    dataframe$ID <- dataframe$file.list
  }

  fulldata <- dplyr::full_join(dataframe, capsize, by = 'ID')

  no_nirs <- subset(fulldata, is.na(fulldata$file.list))
  no_caps <- subset(fulldata, is.na(fulldata$cap))

  test_data <- fulldata %>%
    filter(!is.na(file.list)) %>%
    filter(!is.na(cap))
  #file.list <- subset(file.list.30, !grepl('anatomical', file.list.30))

  test_data$file.list <- as.character(test_data$file.list)

  full_list <- list()
  for (j in 1:length(unique(test_data$cap))){
    # read in function
    datalist <- subset(test_data, test_data$cap == unique(test_data$cap)[j])

    sub_list <- list()
    for (i in 1:nrow(datalist)){
      sub_list[[i]] <- read.table(paste(digipath, '/', datalist$file.list[i], sep = ''))
      names(sub_list)[[i]] <- as.character(datalist$file.list[i])
    }
    full_list[[j]] <- sub_list
    names(full_list)[[j]] <- paste('capsize', unique(test_data$cap)[j], sep = '')
  }
  class(full_list) <- c('digitization', class(full_list))
  attr(full_list, 'digitization') <- list(Landmark = landmarks,
                                          Sources = num_source,
                                          Detectors = num_detector,
                                          Short = short)

  return(full_list)
}

