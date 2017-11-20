#' Get combined conditional probabilities
#'
#' Get combined conditional probabilities
#' 
#' @param restdat \code{data.frame} of restoration data
#' @param reststat \code{data.frame} of restoration data locations 
#' @param wqdat \code{data.frame} of water quality data
#' @param wqstat \code{data.frame} of water quality data locations 
#' @param mtch numeric value of number of restoration sites to match to each water quality site
#' @param yrdf numeric value of year window for evaluation water quality differences before/after each restoration project
#' @param resgrp chr string for grouping variable of restoration project types
#' @param qts numeric vector of quantile values for conditional probabilities
#' @param lbs labels for defining quantile values in \code{qts}
#'
#' @return
#' @export
#'
#' @examples
get_all <- function(restdat, reststat, wqdat, wqstat, mtch = 10, yrdf = 5, resgrp = 'top', qts = c(0.33, 0.66), 
                    lbs = c('lo', 'md', 'hi')){
 
  ## Distance to restoration sites
  wqmtch <- get_clo(restdat, reststat, wqstat, resgrp = resgrp, mtch = mtch)
  
  ## Summarizing effects of restoration projects on salinity
  salchg <- get_chg(wqdat, wqmtch, statdat, restdat, wqvar = 'sal', yrdf = yrdf)
  
  ## Summarizing effects of restoration projects on chlorophyll
  chlchg <- get_chg(wqdat, wqmtch, statdat, restdat, wqvar = 'chla', yrdf = yrdf)

  # Get conditional probability distributions for the restoration type on salinity 
  wqcdt <- get_cdt(salchg)
  
  # Discretization of salinity conditional probability distributions: 
  salbrk <- get_brk(wqcdt, qts = qts)
  
  # get final conditional probabiliyt for last child node:
  allchg <- get_fin(chlchg, salbrk, salchg, lbs = lbs)
  
  return(allchg)
   
}