# replace.wisp: replace invalid trials with extra trials in their correspondent blocks
# Gonzalo García-Castro
# Speech Acquisition and Perception research group
# Center for Brain and Cognition, University Pompeu Fabra
# Barcelona (Spain)

replace.wisp <-
  function(min.trials = 8,   # minimun number of successful test trials
           min.time = 2000,  # minimun looking time for successful trial
           blocks = 3){      # number of blocks
    
    library(magrittr) # data manipulation
    '%!in%' <- function(x,y)!('%in%'(x,y)) # new operator "is not in"
    
    n <- length(raw) # sample size
    n.blocks <- 1:blocks
    
    times.filter <- list()
    times.extra <- list()
    times.delete <- list()
    for (i in 1:n){
      times.filter[[i]] <- times[[i]] %>% dplyr::filter(time >= min.time, block %in% n.blocks)
      times.extra[[i]] <- times[[i]] %>% dplyr::filter(block %!in% n.blocks)
      times.delete[[i]] <- times[[i]] %>% dplyr::filter(time < min.time, block %in% n.blocks)
      }
    
    "times.filter" %>% assign(., times.filter, envir = .GlobalEnv)
    "times.extra" %>% assign(., times.extra, envir = .GlobalEnv)
    "times.delete" %>% assign(., times.delete, envir = .GlobalEnv)
    
    k <- list()
    m <- list()
    times.replaced <- list()
    to.replace <- list()
    
    for (i in 1:n){
      k[[i]] <- times.delete[[i]] %>% nrow                                                                      # number of unsuccessful trials that must be replaced
      m[[i]] <- times.extra[[i]] %>% dplyr::filter(time >= min.time) %>% nrow                                   # number of extra trials available to replace unsuccessful trials
      to.replace[[i]] <- which(times.extra[[i]]$time >= min.time)                                               # extra trial numbers (index) available to replace unsuccessful trials
      if (k[[i]] > 0 & m[[i]] > 0){                                                                             # if a subject made unsuccessful trials
        for (j in 1:length(to.replace[[i]])){                                                                   # 1) identify the each trial to be replaced,
          times.extra[[i]]$block[to.replace[[i]][[j]]] <- times.delete[[i]] %$% block[to.replace[[i]][[j]]]     # 2) change the block of each extra trial for the block of the trial it will replace
          times.replaced[[i]] <- rbind(times.filter[[i]], times.extra[[i]] %>% dplyr::filter(time >= min.time)) # 3) replace each unsuccessful trial by its correspondent extra trial
        }
      } else times.replaced[[i]] <- times.filter[[i]]                                                           # if no unsuccessful trials are present, keep data as it is
      if (nrow(times.replaced[[i]]) <= min.trials){
        times.replaced[[i]] <- paste("Completed less than", min.trials, " trials with looking time >", min.time, "ms", sep = " ")
      }
    }
    "times.replaced" %>% assign(., times.replaced, envir = .GlobalEnv)
  }
