# summarise.wisp: build a summary dataset with demographic variables and processed looking time measures
# Gonzalo García-Castro
# Speech Acquisition and Perception research group
# Center for Brain and Cognition, University Pompeu Fabra
# Barcelona (Spain)

summarise.wisp <-
  function(stim1,                 # items corresponding to first category of stimuli as a character vector (e.g., c(1, 2))
           stim2,                 # items corresponding to first category of stimuli as a character vector (e.g., c(3, 4))
           print = TRUE,          # should the output be save in directory as a file?
           filename = "data",     # how should the output fule be named?
           format = "csv"){       # what format should the output file be in?
    
    library(magrittr) # data manipulation
    
    n <- raw %>% length # sample size
    valid <- unlist(lapply(times.replaced, is.data.frame))
    n.valid <- valid %>% sum
    
    times.replaced.valid <- times.replaced[valid]
    
    data <- matrix(0, n.valid, 29) %>% data.frame
    colnames(data) <- c("id", "gender", "age", "tester", "condition", "time",
                        "block1item1", "block1item2", "block1item3", "block1item4",
                        "block2item1", "block2item2", "block2item3", "block2item4",
                        "block3item1", "block3item2", "block3item3", "block3item4",
                        "block1stim1", "block1stim2",
                        "block2stim1", "block2stim2",
                        "block3stim1", "block3stim2",
                        "block1", "block2", "block3",
                        "stim1", "stim2")
    
    for (i in 1:n.valid){
      data$id[i] <- raw[[i]]$id[2]
      data$gender[i] <- raw[[i]]$gender[2]
      data$age[i] <- raw[[i]]$age[2]
      data$tester[i] <- raw[[i]]$tester[2]
      data$condition[i] <- raw[[i]]$protocol[2]
      
      data$time[i] <- times.replaced.valid[[i]] %$% time %>% mean
      
      data$block1item1[i] <- times.replaced.valid[[i]] %>% dplyr::filter(block == 1, item == 1) %$% time %>% mean
      data$block1item2[i] <- times.replaced.valid[[i]] %>% dplyr::filter(block == 1, item == 2) %$% time %>% mean
      data$block1item3[i] <- times.replaced.valid[[i]] %>% dplyr::filter(block == 1, item == 3) %$% time %>% mean
      data$block1item4[i] <- times.replaced.valid[[i]] %>% dplyr::filter(block == 1, item == 4) %$% time %>% mean
      
      data$block2item1[i] <- times.replaced.valid[[i]] %>% dplyr::filter(block == 2, item == 1) %$% time %>% mean
      data$block2item2[i] <- times.replaced.valid[[i]] %>% dplyr::filter(block == 2, item == 2) %$% time %>% mean
      data$block2item3[i] <- times.replaced.valid[[i]] %>% dplyr::filter(block == 2, item == 3) %$% time %>% mean
      data$block2item4[i] <- times.replaced.valid[[i]] %>% dplyr::filter(block == 2, item == 4) %$% time %>% mean
      
      data$block3item1[i] <- times.replaced.valid[[i]] %>% dplyr::filter(block == 3, item == 1) %$% time %>% mean
      data$block3item2[i] <- times.replaced.valid[[i]] %>% dplyr::filter(block == 3, item == 2) %$% time %>% mean
      data$block3item3[i] <- times.replaced.valid[[i]] %>% dplyr::filter(block == 3, item == 3) %$% time %>% mean
      data$block3item4[i] <- times.replaced.valid[[i]] %>% dplyr::filter(block == 3, item == 4) %$% time %>% mean
      
      data$block1stim1[i] <- times.replaced.valid[[i]] %>% dplyr::filter(block == 1, is.element(item, stim1)) %$% time %>% mean
      data$block1stim2[i] <- times.replaced.valid[[i]] %>% dplyr::filter(block == 1, is.element(item, stim2)) %$% time %>% mean
      data$block2stim1[i] <- times.replaced.valid[[i]] %>% dplyr::filter(block == 2, is.element(item, stim1)) %$% time %>% mean
      data$block2stim2[i] <- times.replaced.valid[[i]] %>% dplyr::filter(block == 2, is.element(item, stim2)) %$% time %>% mean
      data$block3stim1[i] <- times.replaced.valid[[i]] %>% dplyr::filter(block == 3, is.element(item, stim1)) %$% time %>% mean
      data$block3stim2[i] <- times.replaced.valid[[i]] %>% dplyr::filter(block == 3, is.element(item, stim2)) %$% time %>% mean
      
      data$block1[i] <- times.replaced.valid[[i]] %>% dplyr::filter(block == 1) %$% time %>% mean
      data$block2[i] <- times.replaced.valid[[i]] %>% dplyr::filter(block == 2) %$% time %>% mean
      data$block3[i] <- times.replaced.valid[[i]] %>% dplyr::filter(block == 3) %$% time %>% mean
      
      data$stim1[i] <- times.replaced.valid[[i]] %>% dplyr::filter(is.element(item, stim1)) %$% time %>% mean
      data$stim2[i] <- times.replaced.valid[[i]] %>% dplyr::filter(is.element(item, stim2)) %$% time %>% mean
      
    }
    "data" %>% assign(., data, envir = .GlobalEnv)
    
    if (print == TRUE){
      if (format == "csv") {sep <- ","} else if (format == "txt") {sep <- "\t"}
      write.table(data, paste(filename, ".", format, sep = ""), sep = sep)
    }
    
  }
