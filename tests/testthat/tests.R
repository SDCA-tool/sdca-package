# tests 
args = readLines("portishead_example_r_input.geojson")
args = paste(args, collapse = "")
test_that("Simple test of parsing", {
  res <- process_results(args)
  expect_is(res, "json")
})

args = readLines("bristol_cycling_example.json")
args = paste(args, collapse = "")
test_that("Bristol cycling example", {
  res <- process_results(args)
  expect_is(res, "json")
})


# Test via API
test_that("Cycle across the sea", {
  dat <- geojson_api(path = "water_and_demand_test.geojson")
  dat$path_dem <- "dem.tif"
  dat$path_landcover <- "landcover.tif"
  dat$path_bedrock <- "bedrock.tif"
  dat$path_superficial <- "superficial.tif"
  res <- process_results(dat, local = TRUE)
  expect_is(res, "json")
})


