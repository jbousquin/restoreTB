#' Get final conditional probability for last child node
#'
#' @param chlchg 
#' @param salbrk 
#' @param salchg
#' @param lbs
#' @param ... additional arguments as chr strings for grouping variables
#'
#' @return
#' @export
#'
#' @examples
get_fin <- function(chlchg, salbrk, salchg, lbs = c('lo', 'md', 'hi'), ...){
  
  # merge with salinity, bet salinity levels
  salbrk <- salbrk %>% 
    group_by_(...) %>% 
    nest(.key = 'levs')
  allchg <- full_join(chlchg, salchg, by = c('hab', 'wtr', 'stat')) %>% 
    rename(
      salev = cval.y, 
      cval = cval.x
    ) %>% 
    group_by_(...) %>% 
    nest %>% 
    left_join(salbrk, by = c('hab', 'wtr')) %>% 
    mutate(
      sallev = pmap(list(data, levs), function(data, levs){
        
        out <- data %>% 
          mutate(
            saval = salev,
            salev = cut(salev, breaks = c(-Inf, levs$qts, Inf), labels = lbs),
            salev = as.character(salev)
          )
        
        return(out)
        
      })
    ) %>% 
    dplyr::select(-data, -levs) %>% 
    unnest
  
  return(allchg)
  
}