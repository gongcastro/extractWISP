# import.wisp: Data extraction from postprocessed WISP files
# Gonzalo García-Castro
# Speech Acquisition and Perception research group
# Center for Brain and Cognition, University Pompeu Fabra
# Barcelona (Spain)

import.wisp <-
  function(format = "txt", # format of input files
           sep = "\t"){    # how should values be separated in the output file?
    
    library(magrittr)
    
    filenames <- format %>% grep(., list.files(), value = TRUE)
    participants <- paste(".", format, sep = "") %>% gsub(., "", filenames) # name of each dataset
    n <- participants %>% length # sample size
    
    # column names of the WISP file
    variables <- c("trial", "phase", "item", "location",
                   "block", "time", "looksaway", "prelook",
                   "postlook", "protocol", "id", "tester",
                   "gender", "age", "comments", "familiarization", "blank", "blank")
    classes <- c("integer", "character", "character", "character",
                 "character", "numeric", "numeric", "numeric",
                 "numeric", "character", "character", "character",
                 "character", "numeric", "character", "NULL", "NULL", "NULL")
    
    # store each participant's dataset into a list slot
    raw <- list() # preallocation
    for (i in 1:n){
      raw[i] <-
        read.table(filenames[i],
                   sep = sep,
                   col.names = variables,
                   colClasses = classes,
                   row.names = NULL,
                   blank.lines.skip = FALSE,
                   na.strings = c(" ", "NA", "\t", "  ", "-", ""),
                   skip = 1,
                   stringsAsFactors = TRUE) %>% list
      raw[[i]] %<>% dplyr::filter(phase == 3)
    }
    "raw" %>% assign(., raw, envir = .GlobalEnv) # save the resulting list as "raw"
  }
