#' Read Points out of a KML file.
#' 
#' @param x A KML file exported from Google Maps.
#' @param layer The name of the layer to extract from: defaults to \code{"d1"}.
#' @param verbose Whether to report invalid coordinates and/or altitudes below
#' sea level; defaults to \code{TRUE}. See \link{kml_coordinate}.
#' @return A \link[tibble:tibble]{tibble} containing the \code{name}, 
#' \code{description}, \code{styleUrl} and Point coordinates (\code{longitude}, 
#' \code{latitude} and \code{altitude}) of each Placemark element contained in 
#' the KML file. Placemark elements with no Point coordinates, such as Polygon
#' elements, will be discarded.
#' @seealso \url{https://developers.google.com/kml/documentation/kmlreference}
kml_points <- function(x, layer = "d1", verbose = TRUE) {
  
  require(dplyr)
  require(stringr)
  require(xml2)
  
  #' Extract Placemark fields.
  #' 
  #' @param x A nodeset of Placemarks.
  #' @param field The name of the field to extract, e.g. \code{"name"}.
  #' @param layer The name of the layer to extract from; defaults to \code{"d1"}.
  #' @return A character vector. Missing values, i.e. empty fields, will be
  #' returned as \code{NA} values.
  get_field <- function(x, field, layer = "d1") {
    
    # vectorization required to get missing values when field is xml_missing
    lapply(x, xml_find_first, str_c(layer, ":", field)) %>%
      sapply(xml_text)
    
  }
  
  x <- read_xml(x) %>%
    xml_find_all(str_c("//", layer, ":Point/.."))
  
  x <- data_frame(
    name = get_field(x, "name", layer),
    description = get_field(x, "description", layer),
    styleUrl = get_field(x, "styleUrl", layer),
    coordinates = get_field(x, str_c("Point/", layer, ":coordinates"), layer)
  )
  
  x$longitude <- kml_coordinate(x$coordinates, 1, verbose)
  x$latitude  <- kml_coordinate(x$coordinates, 2, verbose)
  x$altitude  <- kml_coordinate(x$coordinates, 3, verbose)
  
  return(select(x, -coordinates))
  
}

#' Read Polygons out of a KML file.
#' 
#' @param x A KML file exported from Google Maps.
#' @param layer The name of the layer to extract from: defaults to \code{"d1"}.
#' @param verbose Whether to report invalid coordinates and/or altitudes below
#' sea level; defaults to \code{TRUE}. See \link{kml_coordinate}.
#' @return A \link[tibble:tibble]{tibble} containing the \code{name}, 
#' \code{description}, \code{styleUrl} and Point coordinates (\code{longitude}, 
#' \code{latitude} and \code{altitude}) of each Placemark element contained in 
#' the KML file. Placemark elements with no Point coordinates, such as Polygon
#' elements, will be discarded.
#' @seealso \url{https://developers.google.com/kml/documentation/kmlreference}
kml_polygons <- function(x, layer = "d1", verbose = TRUE) {
  
  require(dplyr)
  require(stringr)
  require(xml2)
  
  #' Extract Placemark fields.
  #' 
  #' @param x A nodeset of Placemarks.
  #' @param field The name of the field to extract, e.g. \code{"name"}.
  #' @param layer The name of the layer to extract from; defaults to \code{"d1"}.
  #' @return A character vector. Missing values, i.e. empty fields, will be
  #' returned as \code{NA} values.
  get_field <- function(x, field, layer = "d1") {
    
    xml_find_first(x, str_c(layer, ":", field)) %>%
      xml_text
    
  }
  
  x <- read_xml(x) %>%
    xml_find_all(str_c("//", layer, ":Polygon/.."))
  
  x <- lapply(x, function(x) {
    data_frame(
      name = get_field(x, "name"),
      description = get_field(x, "description"),
      styleUrl = get_field(x, "styleUrl"),
      coordinates = get_field(x, str_c("Polygon//", layer, ":coordinates")) %>%
        str_split(" ") %>%
        unlist
    )
  }) %>%
    bind_rows
  
  x$longitude <- kml_coordinate(x$coordinates, 1)
  x$latitude  <- kml_coordinate(x$coordinates, 2)
  x$altitude  <- kml_coordinate(x$coordinates, 3)
  
  return(select(x, -coordinates))
  
}

#' Extract KML coordinates
#' 
#' @param x A character vector of KML coordinates, of the form
#' \code{"longitude,latitude,altitude"}.
#' @param coord Which coordinate to extract: either \code{1} (longitude), 
#' \code{2} (latitude) or \code{3} (altitude).
#' @param verbose Whether to report invalid coordinates and/or altitudes below
#' sea level; defaults to \code{TRUE}.
#' @return A numeric vector.
#' @seealso \url{https://developers.google.com/kml/documentation/kmlreference}
kml_coordinate <- function(x, coord, verbose = TRUE) {
  
  require(stringr) # includes `%>%`
  
  x <- str_replace(x, "(.*),(.*),(.*)", str_c("\\", coord)) %>%
    as.numeric
  
  if (verbose && coord == 1 && any(abs(x) > 180))
    message("Some longitudes are not contained within [-180, 180].")
  
  if (verbose && coord == 2 && any(abs(x) > 90))
    message("Some latitudes are not contained within [-90, 90].")
  
  if (verbose && coord == 3 && any(x < 0))
    message("Some altitudes are below sea level.")
  
  return(x)
  
}