# extractWISP: Data extraction from postprocessed WISP files
# Gonzalo García-Castro
# Speech Acquisition and Perception research group
# Center for Brain and Cognition, University Pompeu Fabra
# Barcelona (Spain)


extractWISP <-
  
  function(format = "txt", #format of input files
           print = TRUE, #should the output file be saved in the working directory?
           sep = "\t", #how should values be separated in the output file?
           filename = "data"){ #name of the output file
  
  library(tidyverse) # data manipulation
  library(magrittr) # data manipulation
  
  filenames <- grep(format, list.files(), value = TRUE)
  participants <- gsub(paste(".", format, sep = ""), "", filenames) #name of each dataset
  n <- length(participants) #sample size
  #column names of the WISP file
  columns <- c("trial", "phase",	"item",	"location",
                 "block", "lookingtime",	"looksaway", "prelook",
                 "postlook", "group", "condition", "language",
                 "id", "tester", "gender","age",
                 "comments", "familiarization", "dominance")
  #column class of the WISP file
  classColumns <- c("numeric", "character", "character",	"character",
                    "character", "integer",	"numeric", "numeric",
                    "numeric", "character", "character", "character",
                    "character", "character", "character", "numeric",
                    "character", "character", "numeric")
  #variables names of the desired resulting dataset 
  variables <- c("group", "condition", "language", "gender",
                    "age", "id", "dominance", "lookingtime",
                    "lookingtime1", "lookingtime2", "lookingtime3",
                    "lookingtimeFamiliar", "lookingtimeUnfamiliar")
  
  segRaw <- list() #preallocation
  
  #store each participant's dataset into a list slot
  for (i in 1:n){
    segRaw[i] <-
      list(read.delim(filenames[i],
                      sep = ",",
                      col.names = columns,
                      row.names = NULL,
                      dec = ".",
                      blank.lines.skip = FALSE,
                      fill = TRUE,
                      quote = "",
                      skip = 1,
                      colClasses = classColumns,
                      stringsAsFactors = TRUE))
    assign("raw", segRaw, envir = .GlobalEnv) #save the resulting list as "raw"
  }
  
  segPost <- data.frame(matrix(0, n, length(variables))) #preallocation
  colnames(segPost) <- variables 
  rownames(segPost) <- participants
  
  segPost %$% group %>% factor(., levels = c("1", "2"), labels = c("monolingual", "bilingual"))
  segPost %$% condition %>% factor(., levels = c("1", "2"), labels = c("gonmus", "forpul"))
  segPost %$% language %>% factor(., levels = c("1", "2"), labels = c("catalan", "spanish"))
  segPost %$% gender %>% factor(., levels = c("1", "2"), labels = c("male", "female"))
  
  #create a dataset summarizing each participants data
  for (i in 1:n){
    
    segPost$group[i] <- raw[[i]]$group[1] #group
    segPost$condition[i] <- raw[[i]]$condition[1] #condition
    segPost$language[i] <- raw[[i]]$language[1] #language
    segPost$gender[i] <- raw[[i]]$gender[1] #gender
    segPost$age[i] <- raw[[i]]$age[1] #age in days
    segPost$dominance[i] <- raw[[i]]$dominance[1] #dominance
    segPost$id[i] <- raw[[i]]$id[1] #id
    segPost$lookingtime[i] <- #total looking time
      mean(na.exclude(raw[[i]]$lookingtime[raw[[i]]$phase == 3]))
    segPost$lookingtime1[i] <- #looking time in block 1
      mean(na.exclude(raw[[i]]$lookingtime[raw[[i]]$phase == 3
                                           & raw[[i]]$block == 1]))
    segPost$lookingtime2[i] <- #looking time in block 2
      mean(na.exclude(raw[[i]]$lookingtime[raw[[i]]$phase == 3
                                           & raw[[i]]$block == 2]))
    segPost$lookingtime3[i] <- #looking time in block 3
      mean(na.exclude(raw[[i]]$lookingtime[raw[[i]]$phase == 3
                                           & raw[[i]]$block == 3]))
    
    if(raw[[i]]$condition[10] == "gonmus"){
      segPost$lookingtimeFamiliar[i] <- #looking time to familiar items for condition 1
        mean(raw[[i]]$lookingtime[raw[[i]]$phase == 3
                                  & raw[[i]]$item == 1
                                  | raw[[i]]$item == 2],
             na.rm = TRUE)

      segPost$lookingtimeUnfamiliar[i] <- #looking time to unfamiliar items for condition 1
        mean(raw[[i]]$lookingtime[raw[[i]]$phase == 3
                                  & raw[[i]]$item == 3
                                  | raw[[i]]$item == 4],
             na.rm = TRUE)
    }else{
      segPost$lookingtimeFamiliar[i] <- #looking time to familiar items for condition 2
        mean(raw[[i]]$lookingtime[raw[[i]]$phase == 3
                                  & raw[[i]]$item == 3
                                  | raw[[i]]$item == 4],
             na.rm = TRUE)
      segPost$lookingtimeUnfamiliar[i] <- #looking time to unfamiliar items for condition 2
        mean(raw[[i]]$lookingtime[raw[[i]]$phase == 3
                                  & raw[[i]]$item == 1
                                  | raw[[i]]$item == 2],
             na.rm = TRUE)
    }
    
    if (raw[[i]]$language[10] == "spanish"){ #exposure to Spanish (%)
      segPost$spanish[i] <- raw[[i]]$dominance[10]
    }else{
      segPost$spanish[i] <- 100 - raw[[i]]$dominance[10]
    }
  }
  segPost$difference <- #calculate looking time difference to familiar - unfamiliar items
    segPost$lookingtimeFamiliar - segPost$lookingtimeUnfamiliar
  segPost$loglookingtime <- log(segPost$lookingtime)
  
  
  assign("post", segPost, envir = .GlobalEnv) #store resulting dataframe as nre object in workspace
  
  if (print){ #save table as .txt file in working directory
    write.table(post, filename, sep = sep)
  }
}
  

  
  
  
