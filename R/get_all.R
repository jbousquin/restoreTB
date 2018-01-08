#' Get combined conditional probabilities
#'
#' Get combined conditional probabilities
#' 
#' @param restdat \code{data.frame} of restoration data
#' @param reststat \code{data.frame} of restoration data locations 
#' @param wqdat \code{data.frame} of water quality data
#' @param wqstat \code{data.frame} of water quality data locations
#' @param wqvar1 chr string of first conditional water quality variable
#' @param wavar2 chr string of second conditional water quality variable
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
get_all <- function(restdat, reststat, wqdat, wqstat, wqvar1 = 'sal', wqvar2 = 'chla', mtch = 10, yrdf = 5, resgrp = 'top', 
                    qts = c(0.33, 0.66), lbs = c('lo', 'md', 'hi')){
 
  ## Distance to restoration sites
  wqmtch <- get_clo(restdat, reststat, wqstat, resgrp = resgrp, mtch = mtch)
  
  ## Summarizing effects of restoration projects on salinity
  wq1chg <- get_chg(wqdat, wqmtch, statdat, restdat, wqvar = wqvar1, yrdf = yrdf)
  
  ## Summarizing effects of restoration projects on chlorophyll
  wq2chg <- get_chg(wqdat, wqmtch, statdat, restdat, wqvar = wqvar2, yrdf = yrdf)

  # Get conditional probability distributions for the restoration type on salinity 
  wqcdt <- get_cdt(wq1chg)
  
  # Discretization of salinity conditional probability distributions: 
  salbrk <- get_brk(wqcdt, qts = qts)

  # get final conditional probabiliyt for last child node:
  allchg <- get_fin(wq2chg, salbrk, wq1chg, lbs = lbs, qts = qts)
  
  return(allchg)
   
}