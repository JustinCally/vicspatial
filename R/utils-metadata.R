# Modifications Copyright 2020 Justin Cally
# Copyright 2018 Province of British Columbia
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
# + retained the specify_geom_name() and geom_col_name() but rewrote geom_col_name() to just look for 'gml:' string 
# + feature_hits() does a similar job to bcdc_number_wfs_records() but has been rewritten to work with the Vicmap geoserver
# + get_col_df() added and uses the DescribeFeatureType service to 


#' The Number of Rows of the Promised Data
#' 
#' @description `feature_hits()` returns an integer of the number of rows that match the passed query/promise. 
#' This is similar to how `nrow()` works for a data.frame, however it will evaluate the number of rows to be returned
#' without having to download the data. 
#'
#' @param x object of class `vicmap_promise`
#'
#' @return integer
#' @export
#'
#' @examples
#' \donttest{
#' vicmap_query(layer = "open-data-platform:hy_watercourse") %>%
#'  feature_hits()
#'  }
feature_hits <- function(x) {
  
  if(!check_geoserver()) {
    return(0)
  }

  x$query$resultType <- "hits"
  x$query$outputFormat <- "text/xml"
  x$query$version <- "2.0.0"
  
  if("CQL_FILTER" %in% names(x$query)) {
    x$query$CQL_FILTER <- finalize_cql(x$query$CQL_FILTER)
  }
  
  request <- httr::build_url(x)
  response <- httr::GET(request)
  
  # stop if broken
  httr::stop_for_status(response)
  
  parsed <- httr::content(response, encoding = "UTF-8")
  
  n_hits <- as.numeric(xml2::xml_attrs(parsed)["numberMatched"])
  return(n_hits)
}

#' Get Column Information
#' @description `geom_col_name` returns a single value for the name of the geometry column for the 
#' WFS layer selected in the `vicmap_promise` object (e.g. `SHAPE`). This column will become the `geometry` column 
#' when using `collect()`. `feature_cols()` provides a vector of all column names for the WFS layer selected in the 
#' `vicmap_promise` object and  `get_col_df()` returns a data.frame with the column names and their XML schema string 
#' datatypes.
#'
#' @param x object of class `vicmap_promise`
#'
#' @return character/data.frame
#' @export
#'
#' @examples
#' \donttest{
#' # Return the name of the geometry column
#' vicmap_query(layer = "open-data-platform:hy_watercourse") %>% 
#'   geom_col_name()
#'  }
geom_col_name <- function(x) {
  
  if(!check_geoserver(timeout = 10, quiet = TRUE)) {
    return(NULL)
  }
  
  geom_col <- get_col_df(x) %>% 
    dplyr::filter(grepl(x = type, pattern = "gml:")) %>%
    dplyr::pull(name)
  
  return(geom_col)
  
}

#' feature column names
#' @rdname geom_col_name
#' @export
#' @examples
#' \donttest{
#' # Return the column names as a character vector
#' vicmap_query(layer = "open-data-platform:hy_watercourse") %>% 
#'   feature_cols()
#' }   
feature_cols <- function(x) {
  
  return(get_col_df(x)$name)
  
}

#' apply cql to geom
#'
#' @param x object of class `vicmap_promise` 
#' @param CQL_statement CQL filter statement
#' @noRd
specify_geom_name <- function(x, CQL_statement){
  # Find the geometry field and get the name of the field
  geom_col <- geom_col_name(x)
  
  # substitute the geometry column name into the CQL statement and add sql class
  dbplyr::sql(glue::glue(CQL_statement, geom_name = geom_col))
}

#' @rdname geom_col_name  
#' @export
#' @examples
#' \donttest{
#' # Return a data.frame of the columns and their XML schema string datatypes
#' try(
#' vicmap_query(layer = "open-data-platform:hy_watercourse") %>% 
#'   get_col_df()
#'   )
#'  }
get_col_df <- function(x) {
  
  if(!check_geoserver(timeout = 10, quiet = TRUE)) {
    return(NULL)
  }
  
  layer <- x$query$version
  if(getOption("vicmap.backend", default = "AWS") == "AWS") {
    base_url_n_wfs <- getOption("vicmap.base_url", default = base_wfs_url)
    r <- httr::GET(paste0(base_url_n_wfs, "?service=wfs&version=", x$query$version, "&request=DescribeFeatureType&typeNames=", x$query$typeNames))
  } else {
  base_url_n_wfs <- substr(getOption("vicmap.base_url", default = base_wfs_url), start = 0, stop = nchar(getOption("vicmap.base_url", default = base_wfs_url)) - 3)
  r <- httr::GET(paste0(base_url_n_wfs, x$query$typeNames, "/wfs?service=wfs&version=", x$query$version, "&request=DescribeFeatureType"))
  }
  
  # stop if broken
  httr::stop_for_status(r)
  
  c <- httr::content(r, encoding = "UTF-8", type="text/xml") 
  
  list <- xml2::xml_child(xml2::xml_child(xml2::xml_child(xml2::xml_child(c, "xsd:complexType"), 
                                      "xsd:complexContent"), 
                            "xsd:extension"), 
                  "xsd:sequence") %>% 
    xml2::as_list()
  
  data <- data.frame(name = sapply(list, function(x) attr(x, "name")),
                     type = sapply(list, function(x) attr(x, "type")), stringsAsFactors = F)

  return(data)
}


