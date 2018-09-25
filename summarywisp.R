# summarywisp: build a summary dataset with demographic variables and processed looking time measures
# Gonzalo García-Castro
# Speech Acquisition and Perception research group
# Center for Brain and Cognition, University Pompeu Fabra
# Barcelona (Spain)

summarywisp <-
  function(stim1,                 # items corresponding to first category of stimuli as a character vector (e.g., c(1, 2))
           stim2,                 # items corresponding to first category of stimuli as a character vector (e.g., c(3, 4))
           print = TRUE,          # should the output be save in directory as a file?
           filename = "wispdata", # how should the output fule be named?
           format = "csv"){       # what format should the output file be in?
    
    library(magrittr) # data manipulation
    
    n <- length(raw.wisp) # sample size
    
    wisp <- matrix(0, n, 29) %>% data.frame
    colnames(wisp) <- c("id", "gender", "age", "tester", "condition", "time",
                        "block1item1", "block1item2", "block1item3", "block1item4",
                        "block2item1", "block2item2", "block2item3", "block2item4",
                        "block3item1", "block3item2", "block3item3", "block3item4",
                        "block1stim1", "block1stim2",
                        "block2stim1", "block2stim2",
                        "block3stim1", "block3stim2",
                        "block1", "block2", "block3",
                        "stim1", "stim2")
    
    for (i in 1:n){
      wisp$id[i] <- raw.wisp[[i]]$id[8]
      wisp$gender[i] <- raw.wisp[[i]]$gender[8]
      wisp$age[i] <- raw.wisp[[i]]$age[8]
      wisp$tester[i] <- raw.wisp[[i]]$tester[8]
      wisp$condition[i] <- raw.wisp[[i]]$protocol[8]
      
      wisp$time[i] <- times.replaced.wisp[[i]] %$% time %>% mean
      
      wisp$block1item1[i] <- times.replaced.wisp[[i]] %>% dplyr::filter(block == 1, item == 1) %$% time %>% mean
      wisp$block1item2[i] <- times.replaced.wisp[[i]] %>% dplyr::filter(block == 1, item == 2) %$% time %>% mean
      wisp$block1item3[i] <- times.replaced.wisp[[i]] %>% dplyr::filter(block == 1, item == 3) %$% time %>% mean
      wisp$block1item4[i] <- times.replaced.wisp[[i]] %>% dplyr::filter(block == 1, item == 4) %$% time %>% mean
      
      wisp$block2item1[i] <- times.replaced.wisp[[i]] %>% dplyr::filter(block == 2, item == 1) %$% time %>% mean
      wisp$block2item2[i] <- times.replaced.wisp[[i]] %>% dplyr::filter(block == 2, item == 2) %$% time %>% mean
      wisp$block2item3[i] <- times.replaced.wisp[[i]] %>% dplyr::filter(block == 2, item == 3) %$% time %>% mean
      wisp$block2item4[i] <- times.replaced.wisp[[i]] %>% dplyr::filter(block == 2, item == 4) %$% time %>% mean
      
      wisp$block3item1[i] <- times.replaced.wisp[[i]] %>% dplyr::filter(block == 3, item == 1) %$% time %>% mean
      wisp$block3item2[i] <- times.replaced.wisp[[i]] %>% dplyr::filter(block == 3, item == 2) %$% time %>% mean
      wisp$block3item3[i] <- times.replaced.wisp[[i]] %>% dplyr::filter(block == 3, item == 3) %$% time %>% mean
      wisp$block3item4[i] <- times.replaced.wisp[[i]] %>% dplyr::filter(block == 3, item == 4) %$% time %>% mean
      
      wisp$block1stim1[i] <- times.replaced.wisp[[i]] %>% dplyr::filter(block == 1, is.element(item, stim1)) %$% time %>% mean
      wisp$block1stim2[i] <- times.replaced.wisp[[i]] %>% dplyr::filter(block == 1, is.element(item, stim2)) %$% time %>% mean
      wisp$block2stim1[i] <- times.replaced.wisp[[i]] %>% dplyr::filter(block == 2, is.element(item, stim1)) %$% time %>% mean
      wisp$block2stim2[i] <- times.replaced.wisp[[i]] %>% dplyr::filter(block == 2, is.element(item, stim2)) %$% time %>% mean
      wisp$block3stim1[i] <- times.replaced.wisp[[i]] %>% dplyr::filter(block == 3, is.element(item, stim1)) %$% time %>% mean
      wisp$block3stim2[i] <- times.replaced.wisp[[i]] %>% dplyr::filter(block == 3, is.element(item, stim2)) %$% time %>% mean
      
      wisp$block1[i] <- times.replaced.wisp[[i]] %>% dplyr::filter(block == 1) %$% time %>% mean
      wisp$block2[i] <- times.replaced.wisp[[i]] %>% dplyr::filter(block == 2) %$% time %>% mean
      wisp$block3[i] <- times.replaced.wisp[[i]] %>% dplyr::filter(block == 3) %$% time %>% mean
      
      wisp$stim1[i] <- times.replaced.wisp[[i]] %>% dplyr::filter(is.element(item, stim1)) %$% time %>% mean
      wisp$stim2[i] <- times.replaced.wisp[[i]] %>% dplyr::filter(is.element(item, stim2)) %$% time %>% mean
      
    }
    assign("wisp", wisp, envir = .GlobalEnv)
    
    if (print == TRUE){
      if(format == "csv"){sep <- ","}else if(format == "txt"){sep <- "\t"}
      write.table(wisp, paste(filename, ".", format, sep = ""), sep = sep)
    }
    
  }
