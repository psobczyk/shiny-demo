source("../../helpers.R")

test_that("capitalize works", {
  expect_equal(capitalize("tEkst"), "Tekst")
})
