# tests 
args = readLines("portishead_example_r_input.geojson")
args = paste(args, collapse = "")


test_that("Simple test of parsing", {
  res <- process_results(args)
  expect_is(res, "json")
})


