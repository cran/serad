test_that("format_niv - arrondi negatif", {

  nbsp <- "\u00a0"

  expect_equal(
    format_niv(365484, detail = -2),
    paste0("365", nbsp, "500")
  )

})
