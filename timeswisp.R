# timeswisp: create a within-subject dataset with looking times across trials
# Gonzalo García-Castro
# Speech Acquisition and Perception research group
# Center for Brain and Cognition, University Pompeu Fabra
# Barcelona (Spain)

timeswisp <-
  function(){  # number of the items from category 2 as a vector (e.g., c(3, 4))

    library(magrittr) # data manipulation
    
    n <- length(raw.wisp) # sample size

    times.wisp <- list()
    for (i in 1:n){
      times.wisp[[i]] <- data.frame(trial = raw.wisp[[i]] %$% trial,
                                    block = raw.wisp[[i]] %$% block,
                                    location = raw.wisp[[i]] %$% location,
                                    item = raw.wisp[[i]] %$% item,
                                    time = raw.wisp[[i]] %$% time)
    }
    "times.wisp" %>% assign(., times.wisp, envir = .GlobalEnv)
  }

    
    