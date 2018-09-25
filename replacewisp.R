# replacewisp: replace invalid trials with extra trials in their correspondent blocks
# Gonzalo García-Castro
# Speech Acquisition and Perception research group
# Center for Brain and Cognition, University Pompeu Fabra
# Barcelona (Spain)

replacewisp <-
  function(min.trials = 8,   # minimun number of successful test trials
           min.time = 2000){ # minimun looking time for successful trial
    library(magrittr) # data manipulation
    
    n <- length(raw.wisp) # sample size
    
    times.filter <- list()
    times.extra <- list()
    times.delete <- list()
    for (i in 1:n){
      times.filter[[i]] <- times.wisp[[i]] %>% dplyr::filter(time >= min.time, block == 1 | block == 2 | block == 3)
      times.extra[[i]] <- times.wisp[[i]] %>% dplyr::filter(block == 4 | block == 5)
      times.delete[[i]] <- times.wisp[[i]] %>% dplyr::filter(time < min.time, block == 1 | block == 2 | block == 3)
      }
    
    assign("times.filter.wisp", times.filter, envir = .GlobalEnv)
    assign("times.extra.wisp", times.extra, envir = .GlobalEnv)
    assign("times.delete.wisp", times.delete, envir = .GlobalEnv)
    
    k <- list()
    m <- list()
    times.replaced <- list()
    to.replace <- list()
    for (i in 1:n){
      k[[i]] <- times.delete[[i]] %>% nrow
      m[[i]] <- times.extra[[i]] %>% dplyr::filter(time >= min.time) %>% nrow
      to.replace[[i]] <- which(times.extra[[i]]$time >= min.time)
      if (k[[i]] > 0 & m[[i]] > 0){
        for (j in 1:length(to.replace[[i]])){
          times.extra[[i]]$block[to.replace[[i]][[j]]] <- times.delete[[i]]$block[to.replace[[i]][[j]]]
          times.replaced[[i]] <- rbind(times.filter[[i]], times.extra[[i]] %>% dplyr::filter(time >= min.time))
        }
      } else {times.replaced[[i]] <- times.filter[[i]]
      }
    }
    assign("times.replaced.wisp", times.replaced, envir = .GlobalEnv)
  }
