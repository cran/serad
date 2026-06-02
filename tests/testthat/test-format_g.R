test_that("format_g", {

  nbsp <- "\u00a0"

  expect_equal(
    format_g(5.3654, signe = 0),
    paste0("5,4", nbsp, "%")
  )

  expect_equal(
    format_g(5.3654, 1),
    paste0("+5,4", nbsp, "%")
  )

  expect_equal(
    format_g(-5.3654, 0),
    paste0("5,4", nbsp, "%")
  )

  expect_equal(
    format_g(-5.3654, 1),
    paste0("-5,4", nbsp, "%")
  )

  expect_equal(
    format_g(-5.3654),
    paste0("-5,4", nbsp, "%")
  )

  expect_equal(
    format_g(-5.3654, detail = 2),
    paste0("-5,37", nbsp, "%")
  )

  expect_equal(
    format_g(0.35),
    paste0("+0,4", nbsp, "%")
  )

})
