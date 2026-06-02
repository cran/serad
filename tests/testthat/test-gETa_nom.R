test_that("gETa_nom - hiérarchie des cas", {

  # ---- Stabilité ----
  expect_equal(
    gETa_nom(1.00049, 1, 0.9996),
    "une stabilité"
  )

  expect_equal(
    gETa_nom(1.00049, 1, 0.981),
    "une stabilisation"
  )


  # ---- Accélération ----
  expect_equal(
    gETa_nom(1.1, 1, 0.99),
    "une accélération"
  )

  expect_equal(
    gETa_nom(1.1, 1, 0.99, TRUE),
    "Accélération"
  )

  # ---- Hausse simple ----
  expect_equal(
    gETa_nom(1.01, 1, 0.99),
    "une poursuite de la hausse"
  )


  # ---- Ralentissement ----
  expect_equal(
    gETa_nom(1.003, 1, 0.99, 0),
    "un ralentissement"
  )


  # ---- Rebond ----
  expect_equal(
    gETa_nom(1.001, 1, 1.01),
    "un rebond"
  )


  # ---- Baisse ----
  expect_equal(
    gETa_nom(0.999, 1, 1.01),
    "une poursuite de la baisse"
  )

  expect_equal(
    gETa_nom(0.96, 1, 1.01),
    "un nouveau recul"
  )

  expect_equal(
    gETa_nom(0.96, 1, 0.99),
    "un repli"
  )


  # ---- Forte baisse ----
  expect_equal(
    gETa_nom(0.8, 1, 0.99),
    "un fort repli"
  )

  expect_equal(
    gETa_nom(0.8, 1, 1),
    "une forte baisse"
  )
})

test_that("gETa_nom - aléatoire", {

  # ---- Accélération ----
  res <- gETa_nom(1.1, 1, 0.99, alea = 0.5)
  expect_true(res %in% c("une accélération", "un regain de dynamisme"))

  # ---- Stabilisation ----
  res <- gETa_nom(1.00049, 1, 0.98, alea = 0.5)
  expect_true(res %in% c("une stabilisation", "un essoufflement"))

  # ---- Hausse ----
  expect_warning(gETa_nom(1.01, 1, 1, alea = 0.5),
                 regexp = "division par 0 dans serad::g()")
  res <- suppressWarnings(gETa_nom(1.01, 1, 1, alea = 0.5))
  expect_true(res %in% c("une hausse", "une progression"))

  # ---- Rebond ----
  res <- gETa_nom(1.001, 1, 1.01, alea = 0.5)
  expect_true(res %in% c("un rebond", "un redressement"))

  # ---- Baisse ----
  res <- gETa_nom(0.99, 1, 1.01, alea = 0.5)
  expect_true(res %in% c("une poursuite de la baisse", "un repli"))

  # ---- Chute ----
  res <- gETa_nom(0.79, 1, 1.01, alea = 0.5)
  expect_true(res %in% c("une chute", "un effondrement"))

})
