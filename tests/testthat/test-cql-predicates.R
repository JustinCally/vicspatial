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


# Tests here have been implemented from bcdata: https://github.com/bcgov/bcdata/blob/master/tests/testthat/test-cql-string.R

suppressPackageStartupMessages(library(sf, quietly = TRUE))

the_geom <- st_sf(st_sfc(st_point(c(1,1)))) %>% `st_crs<-`(4283)

geoserver_down <- !(check_geoserver(timeout = 5, quiet = TRUE))

test_that("vicmap_cql_string fails when an invalid arguments are given",{
  expect_error(vicspatial:::vicmap_cql_string(the_geom, "FOO"))
  expect_error(vicspatial:::vicmap_cql_string(quakes, "DWITHIN"))
})

test_that("vicmap_cql_string fails when used on an uncollected (promise) object", {
  expect_error(vicspatial:::vicmap_cql_string(structure(list, class = "vicmap_promise")))
})

test_that("CQL function works", {
  expect_is(CQL("SELECT * FROM foo;"), c("CQL", "SQL"))
})

test_that("All cql geom predicate functions work", {
  single_arg_functions <- c("EQUALS","DISJOINT","INTERSECTS",
                            "TOUCHES", "CROSSES", "WITHIN",
                            "CONTAINS", "OVERLAPS")
  for (f in single_arg_functions) {
    expect_equal(
      do.call(f, list(the_geom)),
      CQL(paste0(f, "({geom_name}, POINT (1 1))"))
    )
  }
  expect_equal(
    DWITHIN(the_geom, 1), #default units meters
    CQL("DWITHIN({geom_name}, POINT (1 1), 1, meters)")
  )
  expect_equal(
    DWITHIN(the_geom, 1, "meters"),
    CQL("DWITHIN({geom_name}, POINT (1 1), 1, meters)")
  )
  expect_equal(
    BEYOND(the_geom, 1, "feet"),
    CQL("BEYOND({geom_name}, POINT (1 1), 1, feet)")
  )
  expect_equal(
    RELATE(the_geom, "*********"),
    CQL("RELATE({geom_name}, POINT (1 1), *********)")
  )
  expect_equal(
    BBOX(c(1,2,1,2)),
    CQL("BBOX({geom_name}, 1, 2, 1, 2)")
  )
  expect_equal(
    BBOX(c(1,2,1,2), crs = 'EPSG:4326'),
    CQL("BBOX({geom_name}, 1, 2, 1, 2, 'EPSG:4326')")
  )
  expect_equal(
    BBOX(c(1,2,1,2), crs = 4326),
    CQL("BBOX({geom_name}, 1, 2, 1, 2, 'EPSG:4326')")
  )
})

test_that("CQL functions fail correctly", {
  expect_error(EQUALS(quakes), "x is not a valid sf object")
  expect_error(BEYOND(the_geom, "five"), "'distance' must be numeric")
  expect_error(DWITHIN(the_geom, 5, "fathoms"), "'arg' should be one of")
  expect_error(DWITHIN(the_geom, "10", "meters"), "must be numeric")
  expect_error(RELATE(the_geom, "********"), "pattern") # 8 characters
  expect_error(RELATE(the_geom, "********5"), "pattern") # invalid character
  expect_error(RELATE(the_geom, rep("TTTTTTTTT", 2)), "pattern") # > length 1
  expect_error(BBOX(c(1,2,3)), "numeric vector")
  expect_error(BBOX(c("1","2","3", "4")), "numeric vector")
  expect_error(BBOX(c(1,2,3,4), crs = c("EPSG:4326", "EPSG:3005")),
               "must be a character string")
})

test_that("CQL translate works", {
  skip_if_offline()
  skip_if(geoserver_down, message = "vicspatial geoserver not currently available")
  
  expect_is(vicspatial:::cql_translate(CQL(INTERSECTS(the_geom))), c("sql", "character"))
  expect_is(vicspatial:::cql_translate(CQL(BBOX(!!st_bbox(the_geom)))), c("sql", "character"))
  
  # Test not equal predicate
  vmne <- vicmap_query("open-data-platform:vmlite_victoria_polygon_su5") %>%
    filter(state != "VIC")
  
  expect_equal(as.character(vmne$query$CQL_FILTER), "(NOT \"state\" = 'VIC')")
  
})

