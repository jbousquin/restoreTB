#' Get CDF breaks for restoration type, treatment combos
#'
#' @param wqcdt
#' @param qts quantile levels to split variable
#'
get_brk <- function(wqcdt, qts = c(0.33, 0.66)){

  # empirical value of wq data given quantiles
  vals <- wqcdt %>% 
    select(data) %>% 
    unnest %>% 
    .$cval %>% 
    quantile(., qts, na.rm = T)
  
  # get quantile levels, interpolate to cdf values, relabel
  brk <- wqcdt %>% 
    dplyr::select(-data, -crv) %>% 
    unnest %>% 
    group_by_if(is.character) %>% 
    nest %>% 
    mutate(
      brk = map(data, function(data){

        est <- approx(x = data$cval, y = data$cumest, xout = vals)
        out <- data.frame(qts = vals, brk = est$y)
        return(out)
        
      })
    ) %>% 
    dplyr::select(-data) %>% 
    unnest
  
  return(brk)
  
  }
