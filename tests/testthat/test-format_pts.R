test_that("format_pts", {

  nbsp <- "\u00a0"

  expect_equal(
    format_pts(5.3654, signe = 0),
    paste0("5,4", nbsp, "points")
  )

  expect_equal(
    format_pts(1.3654, signe = 0),
    paste0("1,4", nbsp, "point")
  )

  expect_equal(
    format_pts(5.3654, abrev = 1),
    paste0("+5,4", nbsp, "pts")
  )

  expect_equal(
    format_pts(5.3654, 1),
    paste0("+5,4", nbsp, "points")
  )

  expect_equal(
    format_pts(-5.3654, 0),
    paste0("5,4", nbsp, "points")
  )

  expect_equal(
    format_pts(-5.3654, 1),
    paste0("-5,4", nbsp, "points")
  )

  expect_equal(
    format_pts(-5.3654),
    paste0("-5,4", nbsp, "points")
  )

  expect_equal(
    format_pts(-5.3654, detail = 2),
    paste0("-5,37", nbsp, "points")
  )

  expect_equal(
    format_pts(0.35),
    paste0("+0,4", nbsp, "point")
  )

})
