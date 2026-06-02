test_that("plushautniveau - détection des extrêmes", {

  col0 <- c("Y1T1", "Y1T2", "Y1trim3", "Y1T4", "Y2T1", "Y2-T2")
  col1 <- c(12, 11, 7, 6, 9, 10)
  col2 <- c(12, 11, 7, 6, 9, 14)
  col3 <- c(12, 11, 3, 6, 9, 4)
  col4 <- c(12, 11, 7, 6, 9, 4)
  col5 <- c(12, 11, 7, 3, 9, 4)

  df1 <- data.frame(col0, col1, col2, col3, col4, col5)

  # ---- Plus haut standard ----
  expect_equal(
    plushautniveau(df1),
    "C'est le plus haut niveau depuis Y1T2."
  )

  # ---- Condition nbperiode ----
  expect_equal(
    plushautniveau(df1, nbperiode = 5),
    ""
  )

  # ---- Plus haut depuis début ----
  expect_equal(
    plushautniveau(df1, vary = "col2"),
    "C'est le plus haut niveau depuis le début de la série."
  )

  # ---- Plus bas standard ----
  expect_equal(
    plushautniveau(df1, vary = "col3"),
    "C'est le plus bas niveau depuis Y1trim3."
  )

  # ---- Plus bas depuis début ----
  expect_equal(
    plushautniveau(df1, vary = "col4"),
    "C'est le plus bas niveau depuis le début de la série."
  )

})
