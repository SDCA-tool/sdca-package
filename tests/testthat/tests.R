# # tests 
# args = readLines("portishead_example.geojson")
# args = paste(args, collapse = "")
# test_that("Simple test of parsing", {
#   res <- process_results(args)
#   expect_is(res, "json")
# })
# 
# args = readLines("bristol_cycling_example.json")
# args = paste(args, collapse = "")
# test_that("Bristol cycling example", {
#   res <- process_results(args)
#   expect_is(res, "json")
# })


# Test via API
test_that("Cycle across the sea", {
  dat <- geojson_api(path = "water_and_demand_test.geojson",
                    path_dem = "dem.tif",
                    path_landcover = "landcover.tif",
                    path_bedrock = "bedrock.tif",
                    path_superficial = "superficial.tif")
  res <- process_results(dat, local = TRUE)
  expect_is(res, "json")
})


test_that("HSR in portishead", {
  dat <- geojson_api(path = "portishead_example.geojson",
                     path_dem = "dem.tif",
                     path_landcover = "landcover.tif",
                     path_bedrock = "bedrock.tif",
                     path_superficial = "superficial.tif")
  res <- process_results(dat, local = TRUE)
  expect_is(res, "json")
})

test_that("Bristol cycling example", {
  dat <- geojson_api(path = "simple_cycle.geojson",
                     path_dem = "dem.tif",
                     path_landcover = "landcover.tif",
                     path_bedrock = "bedrock.tif",
                     path_superficial = "superficial.tif")
  res <- process_results(dat, local = TRUE)
  expect_is(res, "json")
})


# timing out
# test_that("London CS3", {
#   dat <- geojson_api(path = "london_cs3.geojson",
#                      path_dem = "dem.tif",
#                      path_landcover = "landcover.tif",
#                      path_bedrock = "bedrock.tif",
#                      path_superficial = "superficial.tif")
#   res <- process_results(dat, local = TRUE)
#   expect_is(res, "json")
# })
