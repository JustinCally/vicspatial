# Copyright 2019 Justin Cally
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

#' List Available WFS Layers
#' @description Lists layers available from the WFS geoserver. This is similar to sending the 
#' WFS request of `getFeatureTypes`. `listLayers()` returns a data.frame with the 'Name' and title of the
#' layers available. The 'Name' is what is used within `vicmap_query()` while the title provides somewhat of a 
#' description/clarification about the layer.
#'
#' @param ... Additional arguments passed to \link[base]{grep}. The `pattern` argument can be used to search for specific layers with matching names or titles.
#' @param abstract Whether to return a column of abstract (and metadata ID), the default is true. Switching to FALSE will provide a data.frame with only 2 columns and may be slightly faster. 
#'
#' @return data.frame of 2 (abstract = FALSE) or 4 (abstract = TRUE) columns 
#' @export
#'
#' @examples
#' \donttest{
#' try(
#' listLayers(pattern = "trees", ignore.case = TRUE)
#' )
#' }

listLayers <- function(..., abstract = TRUE) {
  
  if(!check_geoserver()) {
    return(NULL)
  }
  
  url <- httr::parse_url(getOption("vicmap.base_url", default = base_wfs_url))
  url$query <- list(service = "wfs",
                    version = "2.0.0",
                    request = "GetCapabilities")
  
  request <- httr::build_url(url)
  response <- httr::GET(request)
  
  # stop if broken
  httr::stop_for_status(response)
  
  parsed <- httr::content(response, encoding = "UTF-8") %>% xml2::xml_child(4)
  attr_list <- xml2::as_list(parsed)
  
  if(!abstract) {
  
  df <- lapply(attr_list, function(x) {
    data.frame(x[["Name"]], x[["Title"]], stringsAsFactors = F) %>% `colnames<-`(c("Name", "Title"))
  }) %>% dplyr::bind_rows()
  
  } else {
    
    df <- lapply(attr_list, function(x) {
      # get metadataID
      mdid <- stringr::str_sub(stringr::str_subset(unlist(x), "MetadataID="), 12)
      data.frame(x[["Name"]], 
                 x[["Title"]], 
                 if(purrr::is_empty(mdid)) NA_character_ else mdid, 
                 stringsAsFactors = F) %>% `colnames<-`(c("Name", "Title", "metadataID"))
    }) %>% 
      dplyr::bind_rows() %>%
      dplyr::left_join(get_abstract_df(), by = "metadataID") %>%
      dplyr::select(Name, Title, Abstract, metadataID) %>%
      dplyr::distinct()
    
  }
  
  if(methods::hasArg('pattern')){
    df <- dplyr::filter_all(df, dplyr::any_vars(grepl(x = ., ...)))
  }
  return(df)
}  

#' get abstracts from Elasticsearch API (GeoNetwork 4.x)
#' @param base_url base URL of the geonetwork instance
#' @return data.frame with Abstract and metadataID column
#' @noRd
get_abstract_df <- function(base_url = "https://metashare.maps.vic.gov.au/geonetwork") {
  
  search_url <- paste0(base_url, "/srv/api/search/records/_search")
  
  # Get total count first
  count_res <- httr::POST(
    search_url,
    httr::add_headers(`Content-Type` = "application/json", `Accept` = "application/json"),
    body = '{"from":0,"size":0,"query":{"match_all":{}}}',
    encode = "raw"
  )
  httr::stop_for_status(count_res)
  total <- jsonlite::fromJSON(
    httr::content(count_res, as = "text", encoding = "UTF-8"),
    simplifyVector = FALSE
  )$hits$total$value
  
  if (is.null(total) || total == 0) {
    return(tibble::tibble(metadataID = character(), Abstract = character()))
  }
  
  # Paginate in blocks of 100
  from_seq <- seq(from = 0, to = total - 1, by = 100)
  
  abstract_data <- lapply(from_seq, function(from_i) {
    body <- sprintf(
      '{"from":%d,"size":100,"_source":["uuid","resourceAbstractObject"],"query":{"match_all":{}}}',
      from_i
    )
    res <- httr::POST(
      search_url,
      httr::add_headers(`Content-Type` = "application/json", `Accept` = "application/json"),
      body = body,
      encode = "raw"
    )
    httr::stop_for_status(res)
    hits <- jsonlite::fromJSON(
      httr::content(res, as = "text", encoding = "UTF-8"),
      simplifyVector = FALSE
    )$hits$hits
    
    purrr::map_dfr(hits, function(h) {
      src <- h[["_source"]]
      tibble::tibble(
        metadataID = as.character(src[["uuid"]] %||% NA_character_),
        Abstract   = as.character(src[["resourceAbstractObject"]][["default"]] %||% NA_character_)
      )
    })
  })
  
  dplyr::bind_rows(abstract_data)
}
