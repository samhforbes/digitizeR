#' read in caps
#'
#' @param digipath path to digitizations
#' @param pattern a pattern in files passed to list.files
#' @param capsize the path to the cap size document
#' @param type to be adjusted for cap geometry
#' @return a list of digitizations in dataframe form
#' @export
#'
#' @details currently in beta form for generalisation to other projects. For argument "type", running this depends on locations of short sources and layout of sources vs detectors.

read_in_caps <- function(digipath, pattern, capsize, type = c('NIHVWM', 'Gates', 'HWB')){
  capsize <- read.csv(capsize, stringsAsFactors = F)
  names(capsize)[c(1,2,3)] <- c('ID', 'cap', 'head')
  capsize$ID <- as.character(capsize$ID)
  capsize <- capsize[!is.na(capsize$cap),]
  if(type == 'NIHVWM'){
  capsize$age <- ifelse(grepl('30NIHVWM', capsize$ID), '30', '06')
  }
  if(type == 'HWB'){
    capsize$age <- ifelse(grepl('32HWB', capsize$ID), '32', '54')
  }

  file.list <- list.files(path = digipath, pattern = pattern, recursive = TRUE)
  #make into dataframe for merging
  dataframe <- data.frame(file.list)
  if(type =='NIHVWM'){
    dataframe$ID <- substr(dataframe$file.list, 1, 12)
  }
  if(type =='Gates'){
    dataframe$ID <- substr(dataframe$file.list, 1, 9)
  }
  if(type =='HWB'){
    dataframe$ID <- substr(dataframe$file.list, 1, 11)
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
  return(full_list)
}

#' read in templates
#'
#' @param digipath path to digitizations
#' @param path the path to the templates
#' @return a list of digitizations in dataframe form
#' @export
#'
#' @details If templates have been previously made, they can be loaded in with this function.

read_in_templates <- function(path){
  file.list <- list.files(path = path, pattern = "digpts.txt", recursive = TRUE)
  namess = substr(file.list, 1, 9)

  V1 <- c("nz:", "ar:","al:","cz:","iz:",
          "s1:","s4:", "s6:", "s7:", "s13:", "s9:", "s15:", "s16:",
          "d7:", "d2:", "d5:",  "d6:",  "d12:", "d10:", "d13:", "d16:" ,
          "d23:", "d18:", "d21:", "d22:", "d28:", "d26:", "d29:", "d32:")
  df <- data.frame(V1)

  df$V1 <- as.character(df$V1)

  mylist <- list()
  mylist2 <- list()
  for(i in 1:length(file.list)){
    mylist[[i]] <- read.table(paste(path, file.list[i], sep = ''))
    names(mylist)[[i]] <- namess[i]

    dd <- data.frame(mylist[[i]])
    dd <- dd %>%
       mutate(V2 = V2/10,
             V3 = V3/10,
             V4 = V4/10,
             V1 = as.character(V1))

    a <- merge(df, dd, by = "V1", all.x = T, all.y = F, sort = F)



    mylist2[[i]] <- a
    names(mylist2)[[i]] <- namess[i]
  }
  return(mylist2)
}
