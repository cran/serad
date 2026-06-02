test_that("g - calcul variation relative", {

  # ---- Cas standard ----
  expect_equal(g(2, 1), 100)

  # ---- Division par zéro ----
  expect_warning(
    g(2, 0),
    "division par 0 dans serad::g()"
  )

})
