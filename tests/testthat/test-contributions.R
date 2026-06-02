test_that("contributions - cas standards", {

  nbsp <- "\u00a0"

  col0 <- c("Y1T1", "Y1T2", "Y1trim3", "Y1T4", "Y2T1", "Y2-T2")
  col1 <- c(12, 6, 2, 86, 19, 10)
  col2 <- c(4, 8, 7, 34, 87, 14)
  col3 <- c(10, 20, 3, 66, 90, 54)
  col4 <- c(29, 12, 4, 16, 40, 94)
  col5 <- c(58, 76, 1, 3, 34, 19)

  df1 <- data.frame(col0, col1, col2, col3, col4, col5)

  expect_equal(
    contributions(df1),
    paste0(
      "col2 contribue pour +92,4", nbsp,
      "% à l'évolution entre Y2T1 et Y2-T2 ; ",
      "col3 contribue pour +45,6", nbsp,
      "% à l'évolution entre Y2T1 et Y2-T2 ; ",
      "col4 contribue pour -68,4", nbsp,
      "% à l'évolution entre Y2T1 et Y2-T2 ; "
    )
  )

  expect_equal(
    contributions(df1, Tglissement = 4),
    paste0(
      "col4 contribue pour +118,8", nbsp,
      "% à l'évolution entre Y1T2 et Y2-T2 ; ",
      "col3 contribue pour +49,3", nbsp,
      "% à l'évolution entre Y1T2 et Y2-T2 ; ",
      "col5 contribue pour -82,6", nbsp,
      "% à l'évolution entre Y1T2 et Y2-T2 ; "
    )
  )

  expect_equal(
    contributions(df1, Tglissement = 4, seuilpc = 50),
    paste0(
      "col4 contribue pour +118,8", nbsp,
      "% à l'évolution entre Y1T2 et Y2-T2 ; ",
      "col5 contribue pour -82,6", nbsp,
      "% à l'évolution entre Y1T2 et Y2-T2 ; "
    )
  )

})
