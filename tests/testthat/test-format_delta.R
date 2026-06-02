test_that("format_delta - cas standards", {

  nbsp <- "\u00a0"

  expect_equal(
    format_delta(365484),
    paste0("+365", nbsp, "500")
  )

  expect_equal(
    format_delta(365484, 0),
    paste0("365", nbsp, "500")
  )

  expect_equal(
    format_delta(-365484),
    paste0("-365", nbsp, "500")
  )

  expect_equal(
    format_delta(-365484, 0, -1),
    paste0("365", nbsp, "480")
  )

})
