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

test_that("listLayers works", {
  skip_if_offline()
  skip_if(geoserver_down, message = "vicspatial geoserver not currently available")
  
  full_ll <- listLayers(pattern = "trees", ignore.case = TRUE)
  sub_ll <- listLayers(abstract = FALSE)
  
  expect_equal(class(sub_ll), "data.frame")
  expect_lte(nrow(full_ll), 5)
  expect_equal(ncol(full_ll), 4)
  expect_equal(ncol(sub_ll), 2)
})
