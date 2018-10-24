# summarise.wisp2: build a summary dataset with demographic variables and processed looking time measures
# Gonzalo García-Castro (gonzalo.garciadecastro@upf.edu)
# Speech Acquisition and Perception research group
# Center for Brain and Cognition, University Pompeu Fabra
# Barcelona (Spain)

summarise.wisp2 <-
  function(stim1 = NULL,          # items corresponding to first category of stimuli as a character vector (e.g., c(1, 2))
           stim2 = NULL,          # items corresponding to first category of stimuli as a character vector (e.g., c(3, 4))
           blocks = 3,            # number of blocks
           print = TRUE,          # should the summarised results be printed in the console?
           save = TRUE,           # should the output be saved in directory as a file?
           filename = "data",     # how should the output file be named?
           format = "csv"){       # what format should the output file be in?
    
    library(magrittr) # data manipulation
    
    if(is.null(stim1)| is.null(stim2)) stop("Please specify which items correspond to each condition, stim1 and stim2")
    if(!is.character(filename)| !is.character(format)) stop("filename and format arguments must be specified as in character class")
    
    n <- raw %>% length # total sample size
    valid <- lapply(times.replaced, is.data.frame) %>% unlist() # which subjects are valid?
    n.valid <- valid %>% sum() # how many subjects are valid?
    
    times.replaced.valid <- times.replaced[valid] # filter valid subjects
    assign("times.replaced.valid", times.replaced.valid, envir = .GlobalEnv)
    
    variables3 <- c("id", "gender", "age", "tester", "condition", "time",
                    "block1item1", "block1item2", "block1item3", "block1item4",
                    "block2item1", "block2item2", "block2item3", "block2item4",
                    "block3item1", "block3item2", "block3item3", "block3item4")
    variables2 <- c("id", "gender", "age", "tester", "condition", "time",
                    "block1item1", "block1item2", "block1item3", "block1item4",
                    "block2item1", "block2item2", "block2item3", "block2item4")
    variables1 <- c("id", "gender", "age", "tester", "condition", "time",
                    "block1item1", "block1item2", "block1item3", "block1item4")
    
    stim <- c(stim1, stim2)
    itemsblocks <- paste("data$block", rep(1:blocks, each = length(stim)), "item", stim, sep = "")

    if (blocks == 3){ # for 3 blocks
      
      data <- matrix(0, n.valid, 18) %>% data.frame # pre-allocation
      colnames(data) <- variables3
      
      for (i in 1:n.valid){ # for each pariticipant
        data$id[i] <- raw[[i]]$id[2]
        data$gender[i] <- raw[[i]]$gender[2]
        data$age[i] <- raw[[i]]$age[2]
        data$tester[i] <- raw[[i]]$tester[2]
        data$condition[i] <- raw[[i]]$protocol[2]
        
        data$time[i] <- times.replaced.valid[[i]] %$% time %>% mean()
        
        data$block1item1[i] <- times.replaced.valid[[i]] %>% dplyr::filter(block == 1, item == 1) %$% time %>% mean()
        data$block1item2[i] <- times.replaced.valid[[i]] %>% dplyr::filter(block == 1, item == 2) %$% time %>% mean()
        data$block1item3[i] <- times.replaced.valid[[i]] %>% dplyr::filter(block == 1, item == 3) %$% time %>% mean()
        data$block1item4[i] <- times.replaced.valid[[i]] %>% dplyr::filter(block == 1, item == 4) %$% time %>% mean()
        
        data$block2item1[i] <- times.replaced.valid[[i]] %>% dplyr::filter(block == 2, item == 1) %$% time %>% mean()
        data$block2item2[i] <- times.replaced.valid[[i]] %>% dplyr::filter(block == 2, item == 2) %$% time %>% mean()
        data$block2item3[i] <- times.replaced.valid[[i]] %>% dplyr::filter(block == 2, item == 3) %$% time %>% mean()
        data$block2item4[i] <- times.replaced.valid[[i]] %>% dplyr::filter(block == 2, item == 4) %$% time %>% mean()
        
        data$block3item1[i] <- times.replaced.valid[[i]] %>% dplyr::filter(block == 3, item == 1) %$% time %>% mean()
        data$block3item2[i] <- times.replaced.valid[[i]] %>% dplyr::filter(block == 3, item == 2) %$% time %>% mean()
        data$block3item3[i] <- times.replaced.valid[[i]] %>% dplyr::filter(block == 3, item == 3) %$% time %>% mean()
        data$block3item4[i] <- times.replaced.valid[[i]] %>% dplyr::filter(block == 3, item == 4) %$% time %>% mean()
      }
      data %<>% dplyr::mutate(block1stim1 = cbind(data[6+stim1[1]], data[6+stim1[2]]) %>% rowMeans(na.rm = TRUE),
                              block2stim1 = cbind(data[10+stim1[1]], data[10+stim1[2]]) %>% rowMeans(na.rm = TRUE),
                              block3stim1 = cbind(data[14+stim1[1]], data[14+stim1[2]]) %>% rowMeans(na.rm = TRUE),
                              block1stim2 = cbind(data[6+stim2[1]], data[6+stim2[2]]) %>% rowMeans(na.rm = TRUE),
                              block2stim2 = cbind(data[10+stim2[1]], data[10+stim2[2]]) %>% rowMeans(na.rm = TRUE),
                              block3stim2 = cbind(data[14+stim2[1]], data[14+stim2[2]]) %>% rowMeans(na.rm = TRUE),
                              block1 = cbind(block1stim1, block1stim1) %>% rowMeans(na.rm = TRUE),
                              block2 = cbind(block2stim1, block2stim2) %>% rowMeans(na.rm = TRUE),
                              block3 = cbind(block3stim1, block3item2) %>% rowMeans(na.rm = TRUE),
                              stim1 = cbind(block1stim1, block2stim1, block3stim1) %>% rowMeans(na.rm = TRUE),
                              stim2 = cbind(block1stim2, block2stim2, block3stim2) %>% rowMeans(na.rm = TRUE))

    }
    
    else if (blocks == 2){
      data <- matrix(0, n.valid, 14) %>% data.frame
      colnames(data) <- variables2
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
      }
      
      data %<>% dplyr::mutate(block1stim1 = cbind(block1item1, block1item2) %>% rowMeans(na.rm = TRUE),
                             block2stim1 = cbind(block2item1, block2item2) %>% rowMeans(na.rm = TRUE),
                             block1stim2 = cbind(block1item3, block1item4) %>% rowMeans(na.rm = TRUE),
                             block2stim2 = cbind(block2item3, block2item4) %>% rowMeans(na.rm = TRUE),
                             block1 = cbind(block1stim1, block1stim1) %>% rowMeans(na.rm = TRUE),
                             block2 = cbind(block2stim1, block2stim2) %>% rowMeans(na.rm = TRUE),
                             stim1 = cbind(block1stim1, block2stim1, block3stim1) %>% rowMeans(na.rm = TRUE),
                             stim2 = cbind(block1stim2, block2stim2, block3stim2) %>% rowMeans(na.rm = TRUE))
    }
    
    else if (blocks == 1){
      data <- matrix(0, n.valid, 10) %>% data.frame
      colnames(data) <- variables1
      for (i in 1:n.valid){
        data$id[i] <- raw[[i]]$id[2]
        data$gender[i] <- raw[[i]]$gender[2]
        data$age[i] <- raw[[i]]$age[2]
        data$tester[i] <- raw[[i]]$tester[2]
        data$condition[i] <- raw[[i]]$protocol[2]
        
        data$time[i] <- times.replaced.valid[[i]] %$% time %>% mean()
        
        data$block1item1[i] <- times.replaced.valid[[i]] %>% dplyr::filter(block == 1, item == 1) %$% time %>% mean
        data$block1item2[i] <- times.replaced.valid[[i]] %>% dplyr::filter(block == 1, item == 2) %$% time %>% mean
        data$block1item3[i] <- times.replaced.valid[[i]] %>% dplyr::filter(block == 1, item == 3) %$% time %>% mean
        data$block1item4[i] <- times.replaced.valid[[i]] %>% dplyr::filter(block == 1, item == 4) %$% time %>% mean
      }
      data %<>% dplyr::mutate(stim1 = cbind(block1item1, block1item2) %>% rowMeans(na.rm = TRUE),
                              stim2 = cbind(block1item3, block1item4) %>% rowMeans(na.rm = TRUE))
    }
    
    "data" %>% assign(., data, envir = .GlobalEnv)
    
    if (save == TRUE){
      if (format == "csv") {sep <- ","} else if (format == "txt") {sep <- "\t"}
      write.table(data, paste(filename, ".", format, sep = ""), sep = sep, row.names = FALSE)
      print(paste("A dataset was saved in directory as", paste(filename, format, sep = "."), sep = " "))
    }
    if (print) print(data)
  }
