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


#' Layer Metadata
#' @description formatted metadata attributes of a given vicmap layer (`vicmap_query(layer)`). 
#' Metadata is retrieved from the Vicmap catalogue. `data_citation()` prints a BibTex style citation for a given record; 
#' similar to `base::citation()`. `data_dictionary()` returns a table with names, types and descriptions of the data within the
#' selected layer (see details). `get_metdata()` returns a list with three elements, containing metadata, the data dictionary and the url of the 
#' metadata for the record.   
#'
#' @param x Object of class `vicmap_promise` (likely passed from [vicmap_query()])
#' @param metadataID character: ID of data (useful if data is not available through WFS)
#'
#' @return citation, data.frame or list
#' @export
#'
#' @examples
#' \donttest{
#' try(
#' data_citation(vicmap_query(layer = "datavic:VMHYDRO_WATERCOURSE_DRAIN"))
#' )
#' }

data_citation <- function(x = NULL, metadataID = NULL) {
  
  md <- get_metadata(x, metadataID)
  nl <- as.character(md[[1]][[2]])
  names(nl) <- as.character(md[[1]][[1]])
  
  cat("  @ELECTRONIC{", nl["Resource Name"], ",", sep = "")
  cat("\n")
  cat("        author = {", nl["Custodian"], "},", sep = "")
  cat("\n")
  cat("        title = {", nl["Title"], "},", sep = "")
  cat("\n")
  cat("        year = {", lubridate::year(as.POSIXct(nl["Metadata Date"])), "},", sep = "")
  cat("\n")
  cat("        url = {", md[[3]] , "},", sep = "")
  cat("\n")
  cat("        owner = {", nl["Owner"], "},", sep = "")
  cat("\n")
  cat("        timestamp = {", format(Sys.Date(), "%Y.%m.%d"), "},", sep = "")
  cat("\n")
  cat("}")
}

#' @rdname data_citation
#' @export
#' @examples
#' \donttest{
#' try(
#' data_dictionary(vicmap_query(layer = "datavic:VMHYDRO_WATERCOURSE_DRAIN"))
#' )
#' }
data_dictionary <- function(x = NULL, metadataID = NULL) {
  
  get_metadata(x, metadataID)[[2]]
}

get_metadataID <- function(x) {
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
  
  feat_names <- unlist(lapply(attr_list, function(x) x[["Name"]]))
  
  feat <- which(x[["query"]][["typeNames"]] == feat_names)
  
  keywords <- unlist(attr_list[[feat]][["Keywords"]]) %>% 
    unique()
  
  key_lookup <- grep(pattern = "^MetadataID", x = keywords, value = TRUE)
  key_lookup_sub <- sub(pattern = "MetadataID=", replacement = "", x = key_lookup)
  return(key_lookup_sub)
}

#' @rdname data_citation
#' @export
#' @examples
#' \donttest{
#' try(
#' get_metadata(vicmap_query(layer = "datavic:VMHYDRO_WATERCOURSE_DRAIN"))
#' )
#' }
get_metadata <- function(x = NULL, metadataID = NULL) {
  
  if (is.null(x) && is.null(metadataID)) stop("x or metadataID must be provided")
  
  key_lookup <- if (is.null(metadataID)) get_metadataID(x) else metadataID
  
  base_url <- "https://metashare.maps.vic.gov.au/geonetwork"
  search_url <- paste0(base_url, "/srv/api/search/records/_search")
  key_url <- paste0(base_url, "/srv/eng/catalog.search#/metadata/", key_lookup)
  formatter_url <- paste0(base_url, "/srv/api/records/", key_lookup,
                          "/formatters/sdm-html?root=html&output=html")
  
  # Fetch flattened record from ES index
  body <- sprintf('{"query":{"term":{"uuid":"%s"}}}', key_lookup)
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
  
  if (length(hits) == 0) stop("No metadata record found for ID: ", key_lookup)
  
  md <- hits[[1]]$`_source`
  
  # Helper: extract org for a given contact role (gracefully returns NA if absent)
  org_for_role <- function(role) {
    contacts <- md[["contact"]]
    if (is.null(contacts) || length(contacts) == 0) return(NA_character_)
    matches <- purrr::keep(contacts, ~ isTRUE(.x[["role"]] == role))
    if (length(matches) == 0) return(NA_character_)
    as.character(matches[[1]][["organisation"]] %||% NA_character_)
  }
  
  fields <- list(
    "Resource Name" = md[["resourceTitleObject"]][["default"]],
    "Title"         = md[["resourceTitleObject"]][["default"]],
    "Abstract"      = md[["resourceAbstractObject"]][["default"]],
    "Custodian"     = org_for_role("custodian"),
    "Owner"         = org_for_role("owner"),
    "Metadata Date" = md[["dateStamp"]],
    "Resource Type" = md[["resourceType"]][[1]]
  )
  
  meta_df <- tibble::tibble(
    `Metadata Name` = names(fields),
    Descriptions    = as.character(lapply(fields, function(x) {
      if (is.null(x)) NA_character_ else x
    }))
  )
  
  # Data dictionary via sdm-html formatter
  dd_df <- tryCatch({
    doc  <- rvest::read_html(formatter_url)
    tabs <- rvest::html_elements(doc, "table") %>% rvest::html_table(na.strings = "")
    tabs[[length(tabs)]]
  }, error = function(e) {
    tibble::tibble(Name = character(), Type = character(), Description = character())
  })
  
  list(meta_df, dd_df, key_url)
}

