# times.wisp: create a within-subject dataset with looking times across trials
# Gonzalo García-Castro
# Speech Acquisition and Perception research group
# Center for Brain and Cognition, University Pompeu Fabra
# Barcelona (Spain)

times.wisp <-
  function(){

    library(magrittr) # data manipulation
    
    n <- raw %>% length # sample size

    times <- list()
    for (i in 1:n){
      times[[i]] <- data.frame(trial = raw[[i]] %$% trial,
                               block = raw[[i]] %$% block,
                               location = raw[[i]] %$% location,
                               item = raw[[i]] %$% item,
                               time = raw[[i]] %$% time)
    }
    "times" %>% assign(., times, envir = .GlobalEnv)
  }

    
    