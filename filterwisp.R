# filterwisp: delete
# Gonzalo García-Castro
# Speech Acquisition and Perception research group
# Center for Brain and Cognition, University Pompeu Fabra
# Barcelona (Spain)

filterwisp <-
  function(min.time = 2000){ # minimun looking time by trial in milisenconds)
    
    library(magrittr) # data manipulation
    
    n <- length(raw.wisp) # sample size
    

    "times.filter.wisp" %>% assign(., times.filter, envir = .GlobalEnv)
    "times.extra.wisp" %>% assign(., times.extra, envir = .GlobalEnv)
    
  }
