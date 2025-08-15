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

#' get abstracts from API
#' @param url url of geonetwork api
#' @return data.frame with Abstract and metadataID column
#' @noRd

get_abstract_df <- function(url = "https://metashare.maps.vic.gov.au/geonetwork/srv/eng/q?") {
  
  # parse the api url
  api_url <- httr::parse_url(url)
  
  # set the base query for the api
  base_query <- list(`_content_type`= "json", 
                     fast= "index",
                     mdClassification="unclassified", 
                     spatialRepresentationType="vector")
  
  # set the api url with the base query
  api_base_query_url <- api_url 
  api_base_query_url$query <- base_query
  
  # get the number of hits from the api
  hits <- httr::GET(api_base_query_url, 
                    query = list(resultType="hits", summaryOnly="1"))
  
  # stop if there is an error
  httr::stop_for_status(hits)
  
  # get the number of hits
  hit_cont <- httr::content(hits)[[1]][["@count"]]
  
  # set the start and end points for the api query
  breaks_start <- seq(from = 1, to = hit_cont, by = 100)
  breaks_end <- c(seq(from = 100, to = hit_cont, by = 100), hit_cont)
  
  abstract_data <- list()
  for(i in 1:length(breaks_start)) {
    res <- httr::GET(api_base_query_url, 
                     query = list(from = breaks_start[i], 
                                  to = breaks_end[i]))
    httr::stop_for_status(res)
    contents <- httr::content(res)
    
    abstract_data[[i]] <- extract_abstract(contents)
  }
  
  return_data <- dplyr::bind_rows(abstract_data)
  
  return(return_data)
}

#' get abstract df from content of API response
#' @noRd
#' @param c content from metashare api

# robust replacement for vicspatial:::extract_abstract
extract_abstract <- function(c) {
  md_list <- c[["metadata"]]
  
  # empty-safe return
  if (is.null(md_list) || length(md_list) == 0) {
    return(tibble::tibble(metadataID = character(), Abstract = character()))
  }
  
  # small helper to dig safely through nested lists
  get_in <- function(x, path, default = NA_character_) {
    for (nm in path) {
      if (is.null(x) || is.null(x[[nm]])) return(default)
      x <- x[[nm]]
    }
    x
  }
  
  # normalise possible list-y abstracts into a single character string
  normalise_abstract <- function(ab) {
    if (is.null(ab) || length(ab) == 0) return(NA_character_)
    if (is.list(ab)) {
      # some APIs return text under "$" or "#text"; otherwise collapse
      ab <- ab[["$"]] %||% ab[["#text"]] %||% paste0(unlist(ab), collapse = " ")
    }
    as.character(ab)
  }
  
  # define %||% locally to avoid importing rlang
  `%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
  
  purrr::map_dfr(md_list, function(x) {
    tibble::tibble(
      metadataID = as.character(get_in(x, c("geonet:info", "uuid"))),
      Abstract   = normalise_abstract(x[["abstract"]])
    )
  })
}
