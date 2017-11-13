#' Randomize restoration data
#'
#' @param restdat 
#' @param reststat 
#' @param tbpoly 
#' @param loc 
#' @param dts 
#' @param prj 
#'
#' @return
#' @export
#'
#' @examples
rnd_dat <- function(restdat, reststat, tbpoly, loc = TRUE, dts = TRUE, prj = TRUE){
  
  # total obs to create
  tot <- nrow(reststat)

  # id values, date ranges
  id <- stri_rand_strings(tot, 5)
  
  # random locations
  if(loc){
    
    lon <- runif(10 * tot, min(reststat$lon), max(reststat$lon))
    lat <- runif(10 * tot, min(reststat$lat), max(reststat$lat))
    reststat <- SpatialPoints(cbind(lon, lat), proj4string = crs(tbpoly)) %>% 
      .[tbpoly, ] %>% 
      .[sample(1:nrow(.@coords), tot, replace = F), ]
    
  } 
  
  # random dates
  if(dts){
    
    dts <- range(restdat$date)  
    restdat$date <- sample(seq(dts[1], dts[2]), tot, replace = T)
    
  }
  
  # random projects
  if(prj){
    
    restdat$top <- sample(c('hab', 'wtr'), tot, replace = T) 
    
  }
  
  # combine as new
  restdat_rnd <- tibble(id) %>% 
    mutate(
      date = restdat$date,
      top = restdat$top   
    )
  
  reststat_rnd <- tibble(id) %>% 
    mutate(
      lat = reststat$lat, 
      lon = reststat$lon
    )
  
  # return
  out <- list(
    restdat_rnd = restdat_rnd, 
    reststat_rnd = reststat_rnd
  )
  
  return(out)

}