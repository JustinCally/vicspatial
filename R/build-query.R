# Modifications Copyright 2020 Justin Cally
# Copyright 2019 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# https://www.apache.org/licenses/LICENSE-2.0.txt
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.
# 
# Modifications/State changes made to original work: 
# + base url is now the Victorian WFS server (vs Province of British Columbia WFS server)
# + vicmap_query() is derived from bcdc_query_geodata, modifications made are: 
#   - Does not use S3 methods
#   - Specifications of url query are different 
#   - Depending on WFS version adds either maxFeatures or count to the query
#   - Returns object of 'vicmap_promise' (class similar to 'bcdc_promise')
# + Adds a show_query.vicmap_promise() function
# + For collect() No trycatch is used 
# + The method of collection of results has been changed from using crul::Paginator to using a for 
#   loop and the 'startIndex' parameter within the wfs query. Modified collect then reads the object in from the url 
#   using sf::read_sf() while bcdata calls another internal function to read it in as an sf object
# + head.vicmap_promise() has been rewritten from head.bcdc_promise()
# + print.vicmap_promise() has laregly been rewritten but produces a similar desired output as formatting (using cli) 
#   is retained and only six records are printed to the screen, which was the case in bcdata. print.bcdc_promise() uses
#   several utility function (like bcdc_tidy_resources) not developed in this package.

base_wfs_url <- "https://opendata.maps.vic.gov.au/geoserver/ows"
base_chunk_lim <- 5000L

#' Establish Vicmap Query
#'
#' @description Begin a Vicmap WFS query by selecting a WFS layer. The record must be available as a 
#' Web Feature Service (WFS) layer (listed in `listLayers()`)  
#'
#' @param layer vicmap layer to query. Options are listed in `listLayers()`
#' @param CRS Coordinate Reference System (default is 4283)
#' @param wfs_version The current version of WFS is 2.0.0. 
#' GeoServer supports versions 2.0.0, 1.1.0, and 1.0.0. 
#' However in order for filtering to be correctly applied wfs_version must be 2.0.0 (default is 2.0.0)
#'
#' @details The returned `vicmap_promise` object is not data, rather it is a 'promise' of the data that can 
#' be returned if `collect()` is used; which returns an `sf` object. 
#' @return object of class `vicmap_promise`, which is a 'promise' of the data that can  be returned if `collect()` is used
#' @export
#'
#' @examples
#' \donttest{
#' try(
#' vicmap_query(layer = "open-data-platform:hy_watercourse")
#' )
#' }
vicmap_query <- function(layer, CRS = 4283, wfs_version = "2.0.0") {
  
  # Check if query exceeds vicmap limit 
  check_chunk_limit()
  
  url <- httr::parse_url(getOption("vicmap.base_url", default = base_wfs_url))
  url$query <- list(service = "wfs",
                    version = wfs_version,
                    request = "GetFeature",
                    typeNames = layer,
                    outputFormat = "application/json",
                    count = getOption("vicmap.chunk_limit", default = 5000L),
                    maxFeatures = getOption("vicmap.chunk_limit", default = 5000L),
                    srsName = paste0("EPSG:", CRS))
  
  #maxFeatures or count depends on version
  if(url$query$version == "2.0.0") {
    url$query$maxFeatures <- NULL 
  } else {
    url$query$count <- NULL 
  }
  
  url$query <- purrr::discard(url$query, is.null)
  
  url <- convert_layer_name(url)
  
  as.vicmap_promise(url)
  
}

#' Show The Query
#' 
#' @description `show_query()` summarises the constructed query that has been passed to it by printing details 
#' about the query in a human readable format.
#' 
#' @details The printed information consists of three sections: 
#' \itemize{
#'  \item{\strong{base url}}{ The base url of the query, this can be changed with options(vicmap.base_url = another_url)}
#'  \item{\strong{body}}{ Lists the parameters of the WFS query, these can be modified through various functions such as `vicmap_query()`, `filter()`, `select()` and `head()`}
#'  \item{\strong{full query url}}{ The constructed url of the final query to be collected}
#' }
#'
#' @param x Object of class `vicmap_promise` (likely passed from [vicmap_query()])
#' @param ... Other parameters possibly used by generic
#'
#' @describeIn show_query show_query.vicmap_promise
#' @return object of class `vicmap_promise` (invisible: query printed to console), which is a 'promise' of the data that can  be returned if `collect()` is used
#' @export
#'
#' @examples
#' \donttest{
#' try(
#' vicmap_query(layer = "open-data-platform:hy_watercourse") %>%
#' head(50) %>%
#' show_query()
#' )
#' }

show_query.vicmap_promise <- function(x, ...) {
  
  x$query$CQL_FILTER <- finalize_cql(x$query$CQL_FILTER)
  #request <- httr::build_url(x)
  
  cli::cat_line("<base url>")
  cli::cat_line(glue::glue("{x[['scheme']]}://{x[['hostname']]}/{x[['path']]}"))
  cli::cat_line()
  cli::cat_line("<body>")
  cli::cat_line(glue::glue("{names(x$query)}: {x$query} \n"))
  cli::cat_line()
  cli::cat_line("<full query url>")
  cli::cat_line(httr::build_url(x))
  invisible(x)
  
}

#' Return Data
#' 
#' @description `collect()` will force the execution of the `vicmap_promise` query. 
#' In doing so it will return an `sf` object into memory.  
#' 
#' @details Collecting certain datasets without filters will likely result in a large object being returned. Given 
#' that their is a limit on the number of rows that can be returned from the Vicmap geoserver (70,000) data will be 
#' paginated; which essentially means that multiple queries will be sent with the data bound together at the end. This 
#' process may take a while to run, thus it is recommended to filter large datasets before collection.
#' 
#' @describeIn collect collect.vicmap_promise
#'
#' @param x object of class `vicmap_promise` (likely passed from [vicmap_query()])
#' @param quiet logical; whether to suppress the printing of messages and progress
#' @param paginate logical; whether to allow pagination of results to extract all records (default is TRUE, 
#' meaning all data will be returned but it will take more time)
#' @param ... additional arguments passed to \link[sf]{st_read}
#'
#' @return sf/tbl_df/tbl/data.frame matching the query parameters
#' @export
#'
#' @examples
#' \donttest{
#' try(
#' vicmap_query(layer = "open-data-platform:hy_watercourse") %>%
#' head(5) %>%
#' collect()
#' )
#' }
collect.vicmap_promise <- function(x, quiet = FALSE, paginate = TRUE, ...) {
  
  # Exit out if null
  if(is.null(x)){
    return(NULL)
  }
  
  # Exit out if problem with connection
  if(!check_geoserver(timeout = 10, quiet = TRUE)) {
    return(NULL)
  }
  
  x$query$CQL_FILTER <- finalize_cql(x$query$CQL_FILTER)
  
  # check number of records
  number_of_records <- feature_hits(x)
  
  #get queried count
  if(x$query$version == "2.0.0") {
    the_count <- x$query$count
  } else {
    the_count <- x$query$maxFeatures 
  }
  
  # For when head is used
  if(the_count > getOption("vicmap.chunk_limit", default = 5000L)) {
    number_of_records <- the_count
  }
  
  #paginate?
  if(number_of_records > getOption("vicmap.chunk_limit", default = 5000L) & paginate == TRUE & the_count >= getOption("vicmap.chunk_limit", default = 5000L)) {
    # number of times to loop
    loop_times <- ceiling(number_of_records/getOption("vicmap.chunk_limit", default = 5000L))
    # inform user of delay
    if(!quiet) {
    message(paste0("There are ", number_of_records, " rows to be retrieved. This is more than the Vicmap chunk limit (", getOption("vicmap.chunk_limit", default = 5000L),"). The collection of data will be paginated and might take some time."))
    }
    # pick something to sort by
    cols <- feature_cols(x)
    sort_col <- ifelse("OBJECTID" %in% cols, "OBJECTID", cols[1])
    
    #set up list
    returned_sf <- list()
    
    #progress bar
    if(!quiet) {
    pb <- utils::txtProgressBar(min = 0, max = loop_times, initial = 0, width = 50, style = 3) 
    }
    
    for(i in 1:loop_times) {
      cl <- getOption("vicmap.chunk_limit", default = 5000L)
      x$query$startIndex <- (i-1)*cl
      if(getOption("vicmap.backend", default = "AWS") != "AWS") {
      x$query$sortBy <- sort_col 
      }
      if(x$query$version == "2.0.0") {
        x$query$count <- min(c(number_of_records-((i-1)*cl), cl))
      } else {
        x$query$maxFeatures <- min(c(number_of_records-((i-1)*cl)))
      }
      request <- httr::build_url(x)
      returned_sf[[i]] <- sf::read_sf(request, ...)
      
      # Update progress bar
      if(!quiet) {
        utils::setTxtProgressBar(pb,i)
      }
      
    }
    return(do.call("rbind", returned_sf))
     
  } else {
    # if less than only loop once
    request <- httr::build_url(x)
    return(sf::read_sf(request, ...))
  }
  
  
  
}

#' Return the first n rows of the data
#'
#' @param x object of class `vicmap_promise` (likely passed from [vicmap_query()])
#' @param n integer; number of rows to return
#' @param ... Other parameters possibly used by generic
#'
#' @describeIn head head.vicmap_promise
#' @return Object of class `vicmap_promise`, which is a 'promise' of the data that can  be returned if `collect()` is used
#' @export
#'
#' @examples
#' \donttest{
#' try(
#' vicmap_query(layer = "open-data-platform:hy_watercourse") %>%
#' head(50)
#' )
#' }

head.vicmap_promise <- function(x, n = 5, ...) {
  
  # Different names for versions
  
  if(x$query$version == "2.0.0") {
  x$query$count <- n 
  } else {
    x$query$maxFeatures <- n 
  }
  
  return(x)
  
}


#' Print a Snapshot of the Data
#' 
#' @description  `print()` displays a cut of the data (no more than  six rows) 
#' alongside the number of rows and columns that would be returned.
#'
#' @param x object of class `vicmap_promise` (likely passed from [vicmap_query()])
#' @param ... arguments to be passed to \link[base]{print}
#'
#' @return vicmap_promise (invisible), promise sample printed to console
#' @export
#'
#' @examples
#' \donttest{
#' try(
#' query <- vicmap_query(layer = "open-data-platform:hy_watercourse")
#' )
#' try(
#' print(query)
#' )
#' }
print.vicmap_promise <- function(x, ...) {
  
  # Exit out if null
  if(is.null(x)){
    return(NULL)
  }
  
  # Exit out if problem with connection
  if(!check_geoserver(timeout = 10, quiet = TRUE)) {
    return(NULL)
  }
  
  x$query$CQL_FILTER <- finalize_cql(x$query$CQL_FILTER)
  
  number_of_records <- feature_hits(x)
  
  if(is.null(number_of_records) || is.na(number_of_records) || number_of_records == 0) {
    stop("No data available to query. Check your layer and query parameters")
  }
  
  if(number_of_records > 6) {
    if(x$query$version == "2.0.0") {
      x$query$count <- 6 
    } else {
      x$query$maxFeatures <- 6 
    }
  }
  
  request <- httr::build_url(x)  
  
  sample_data <- sf::read_sf(request)
  
  fields <- length(sample_data)
  
  cli::cat_bullet(strwrap(glue::glue("Using {cli::col_blue('collect()')} on this object will return {cli::col_green(number_of_records)} features ",
                                "and {cli::col_green(fields)} fields")))
  cli::cat_bullet(strwrap("At most six rows of the record are printed here"))
  cli::cat_rule()
  print(sample_data, ...)
  invisible(x)
  
}
