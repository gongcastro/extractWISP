# import.wisp: Data extraction from postprocessed WISP files
# Gonzalo García-Castro
# Speech Acquisition and Perception research group
# Center for Brain and Cognition, University Pompeu Fabra
# Barcelona (Spain)

import.wisp <-
  function(){
    
    library(magrittr)
    
    # locate files containing data
    files <- list.files(pattern = "txt")
    if (length(files) == 0) stop("No files were found. Please check that the data files are in .txt format and that the right working directory has been set.")
    
    participants <- sub("\\.txt","", files) # name of each dataset
    n <- participants %>% length # sample size
    
    # column names of the WISP file
    col.names <- c("trial", "phase", "item", "location",
                   "block", "time", "looksaway", "prelook",
                   "postlook", "protocol", "id", "tester",
                   "gender", "age", "comments", "NULL", "NULL", "NULL")
    col.classes <- c("character", "character", "character", "character",
                     "character", "numeric", "numeric", "numeric",
                     "numeric", "character", "character", "character",
                     "character", "character", "NULL", "NULL", "NULL", "NULL")
    
    # store each participant's dataset into a list slot
    raw <- lapply(files,
                  read.delim,
                  header = FALSE,
                  colClasses = col.classes,
                  col.names = col.names,
                  row.names = NULL,
                  blank.lines.skip = FALSE,
                  na.strings = c(" ", "NA", "\t", "  ", "-", ""),
                  skip = 1,
                  stringsAsFactors = TRUE)
    for (i in 1:n) raw[[i]] %<>% dplyr::filter(phase == 3)
    "raw" %>% assign(., raw, envir = .GlobalEnv) # save the resulting list as "raw"
  }
