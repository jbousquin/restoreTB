#' Get likelihood estimates for all scenarios from fitted bn model, simple or complex
#'
#' @param cdat conditional data used to create bn model
#' @param cdat_mod bn model
#'
get_lik <- function(cdat, cdat_mod){

  # get all relevant combos to query in model from cdat
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

  return(out)

}