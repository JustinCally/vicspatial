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

test_that("get_metadata works", {
  skip_if_offline()
  skip_if(geoserver_down, message = "vicspatial geoserver not currently available")
  
  r <- get_metadata(vicmap_query(layer = "open-data-platform:hy_watercourse"))
  
  expect_equal(class(r), "list")
  expect_equal(length(r), 3)
  expect_true(is.data.frame(r[[1]]))
  expect_true(is.data.frame(r[[2]]))
  expect_true(is.character(r[[3]]))
  

  r2 <- get_metadata(metadataID = "cc373943-7848-5c21-9be4-7a92632e624c")
  
  expect_identical(r, r2)
})

test_that("data_dictionary works", {
  skip_if_offline()
  skip_if(geoserver_down, message = "vicspatial geoserver not currently available")
  
  r <- data_dictionary(vicmap_query(layer = "open-data-platform:apiary"))
  
  expect_true(is.data.frame(r))
  
})

test_that("data_citation works", {
  skip_if_offline()
  skip_if(geoserver_down, message = "vicspatial geoserver not currently available")
  
  data_citation(vicmap_query(layer = "open-data-platform:apiary"))
  
  expect_output({data_citation(vicmap_query(layer = "open-data-platform:apiary"))})
  
})
