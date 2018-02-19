#' Get final conditional probability for last child node
#'
#' @param chlchg 
#' @param salbrk 
#' @param salchg
#' @param lbs
#' @qts 
#'
#' @return
#' @export
#'
#' @examples
get_fin <- function(chlchg, salbrk, salchg, lbs = c('lo', 'md', 'hi'), qts = c(0.33, 0.66)){

  # grouping variables
  grps <- names(salbrk)[-grep('^qts$|^brk$', names(salbrk))]
  
  # merge with salinity, bet salinity levels
  salbrk <- salbrk %>% 
    group_by_(.dots = grps) %>% 
    nest(.key = 'levs')
  allchg <- full_join(chlchg, salchg, by = c(grps, 'stat')) %>% 
    rename(
      salev = cval.y, 
      cval = cval.x
    ) %>% 
    group_by_(.dots = grps) %>% 
    nest %>% 
    left_join(salbrk, by = grps) %>% 
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
  
  # numeric values
  emp <- allchg

  # get conditional probs only where estimable
  cdt <- allchg %>% 
    filter(!is.na(salev)) %>%
    get_cdt %>% 
    mutate(
      chk = map(crv, ~ .x %>% is.na %>% any),
      chk = unlist(chk)
    ) %>% 
    filter(!chk) %>% 
    dplyr::select(-chk)
  brk <- get_brk(cdt, qts = qts) %>% 
    group_by_if(is.character) %>% 
    nest(.key = 'levs')

  # get final cond probs
  dsc <- allchg %>% 
    group_by_if(is.character) %>% 
    nest %>% 
    inner_join(brk) %>% 
    mutate(
      lev = pmap(list(data, levs), function(data, levs){
    
        out <- data %>% 
          mutate(
            lev = cut(cval, breaks = c(-Inf, levs$qts, Inf), labels = lbs),
            lev = as.character(lev)
          )
        
        return(out)
        
      })
    ) %>% 
    dplyr::select(-data, -levs) %>% 
    unnest %>% 
    rename(
      chlev = lev, 
      chval = cval
    ) 
  
  # output list
  out <- list(
    emp = emp, 
    dsc = dsc
  )
  
  return(out)
  
}