# tests 
args = readLines("portishead_example_r_input.geojson")
args = paste(args, collapse = "")

context("Simple test of parsing")

test_that("test otp_list_clean", {
  res <- process_results(args)
  expect_is(res, "json")
})