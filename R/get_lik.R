#' Get likelihood estimates for all scenarios from fitted bn model, simple or complex
#'
#' @param cdat conditional data used to create bn model
#' @param cdat_mod bn model
#' @param bysal should project effects by evaluated by salinity levels
#'
get_lik <- function(cdat, cdat_mod, bysal = T){

  if(bysal){
    
    # get all relevant combos to query in model from cdat, separate for each salnity level
    out <- cdat %>%
      select_if(is.factor) %>%
      na.omit %>%
      data.frame %>%
      dplyr::select(-matches('^nilev$')) %>%
      mutate_if(is.factor, as.character) %>%
      gather('project', 'event', -chlev, -salev) %>%
      unique %>%
      mutate(est = NA)
    
    # iterate through scenarios to get likelihoods
    for(i in 1:nrow(out)){
      
      toest <- out[i, ]
      est <- paste0('cpquery(cdat_mod, event = (chlev == "', toest$chlev, '"), evidence = (salev == "', toest$salev, '" & ', toest$project, ' == "', toest$event, '"))')
      est <- eval(parse(text = est))
      out[i, 'est'] <- est
      
    }
    
  } else {
    
    # get all relevant combos to query in model from cdat, separate for each salnity level
    out <- cdat %>%
      select_if(is.factor) %>%
      na.omit %>%
      data.frame %>%
      dplyr::select(-matches('^salev$|^nilev$')) %>%
      mutate_if(is.factor, as.character) %>%
      gather('project', 'event', -chlev) %>%
      unique %>%
      mutate(est = NA)
    
    # iterate through scenarios to get likelihoods
    for(i in 1:nrow(out)){
      
      toest <- out[i, ]
      est <- paste0('cpquery(cdat_mod, event = (chlev == "', toest$chlev, '"), evidence = (', toest$project, ' == "', toest$event, '"))')
      est <- eval(parse(text = est))
      out[i, 'est'] <- est
      
    }
    
  }

  return(out)

}