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

test_that("filter works", {
  skip_if_offline()
  skip_if(geoserver_down, message = "vicspatial geoserver not currently available")
  
  r <- vicmap_query("open-data-platform:hy_watercourse", wfs_version = "2.0.0") %>% 
    filter(hierarchy == "L", pfi == 8553127)
  
  expect_equal(as.character(r[["query"]][["CQL_FILTER"]]), "((\"hierarchy\" = 'L') AND (\"pfi\" = 8553127.0))")
  })

test_that("passing an non-existent object to a geom predicate", {
  skip_if_offline()
  skip_if(geoserver_down, message = "vicspatial geoserver not currently available")
  
  expect_error(vicmap_query("open-data-platform:hy_watercourse") %>%
                 filter(INTERSECTS(districts)))
})


test_that("geometric filter works", {
  skip_if_offline()
  skip_if(geoserver_down, message = "vicspatial geoserver not currently available")
  skip_if(!(sf::sf_extSoftVersion()[["GDAL"]] > 3))
  
  polygon <- sf::st_read(system.file("shapes/melbourne.geojson", package="vicspatial"), quiet = TRUE)
  
  dataobject <- vicmap_query("open-data-platform:hy_watercourse") %>%
                 filter(INTERSECTS(polygon)) %>%
    feature_hits()
  
  expect_gt(dataobject, expected = 0)
  
  polygon_3111 <- sf::st_transform(polygon, 3111)
  
  dataobject <- vicmap_query("open-data-platform:hy_watercourse") %>%
    filter(INTERSECTS(polygon_3111)) %>%
    feature_hits()
  
  expect_gt(dataobject, expected = 0)
  
})
