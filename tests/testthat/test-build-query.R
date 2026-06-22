# Copyright 2020 Justin Cally
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

# Treat the geoserver as "down" if it can't actually serve a filtered query.
# The base URL can be reachable while filtered WFS requests are rejected (e.g.
# CI / datacentre IPs receive HTTP 400), which would otherwise fail these tests
# instead of skipping them.
geoserver_down <- tryCatch(
  {
    feature_hits(filter(vicmap_query("open-data-platform:hy_watercourse"), hierarchy == "L"))
    FALSE
  },
  error = function(e) TRUE
)

test_that("vicmap_query works", {
  skip_if_offline()
  skip_if(geoserver_down, message = "vicspatial geoserver not currently available")
  
  q <- vicmap_query(layer = "open-data-platform:hy_watercourse", CRS = 3111, wfs_version = "1.0.0")
  q2 <- vicmap_query(layer = "open-data-platform:hy_watercourse", wfs_version = "2.0.0")
  expect_error(vicmap_query(), regexp = 'argument "layer" is missing, with no default')
  expect_is(q, c("vicmap_promise", "url"))
  expect_setequal(names(q$query), c("service", 
                                    "version", 
                                    "request",
                                    "typeNames",
                                    "outputFormat", 
                                    "maxFeatures",
                                    "srsName"))
  expect_setequal(names(q2$query), c("service", 
                                    "version", 
                                    "request",
                                    "typeNames",
                                    "outputFormat", 
                                    "count",
                                    "srsName"))
  expect_equal(q$query$srsName, "EPSG:3111")
  expect_equal(q$query$version, "1.0.0")
})

test_that("print.vicmap_promise works", {
  skip_if_offline()
  skip_if(geoserver_down, message = "vicspatial geoserver not currently available")
  
  expect_error(print(vicmap_query('not a layer')), regexp = "Bad Request [(]HTTP 400[)].")
  expect_output(print(vicmap_query(layer = "open-data-platform:hy_watercourse", wfs_version = "2.0.0")), regexp = NULL)
})

test_that("head.vicmap_promise works", {
  skip_if_offline()
  skip_if(geoserver_down, message = "vicspatial geoserver not currently available")
  
  r <- vicmap_query(layer = "open-data-platform:hy_watercourse") %>%
    head(10) %>%
    collect() %>% 
    nrow()

  expect_equal(r, 10)
})

test_that("collect.vicmap_promise works", {
  skip_if_offline()
  skip_if(geoserver_down, message = "vicspatial geoserver not currently available")
  
  d <- vicmap_query(layer = "open-data-platform:hy_watercourse") %>%
    head(10) %>% 
    collect()
  
  expect_is(d, c("data.frame", "sf", "tbl_df", "tbl"))
  
  options(vicmap.chunk_limit = 100)
  expect_message(object = {d2 <- vicmap_query(layer = "open-data-platform:hy_watercourse") %>%
    head(101) %>% select(id) %>%
    collect()}, regexp = NULL)
  
  expect_equal(nrow(d2), 101)
  
  options(vicmap.chunk_limit = 5000L)
})

test_that("show_query.vicmap_promise works", {
  skip_if_offline()
  skip_if(geoserver_down, message = "vicspatial geoserver not currently available")
  
  expect_output({vicmap_query(layer = "open-data-platform:hy_watercourse") %>% 
                  show_query()}, regexp = NULL)
})
