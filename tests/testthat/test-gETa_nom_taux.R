test_that("gETa_nom_taux - hiérarchie des cas", {

  # ---- Stabilité ----
  expect_equal(gETa_nom_taux(0.049, 0.049),
               "une stabilité")

  expect_equal(gETa_nom_taux(0.049, 2),
               "une stabilisation")


  # ---- Accélération ----
  expect_equal(gETa_nom_taux(10, 1),
               "une accélération")

  expect_equal(gETa_nom_taux(4, 1, TRUE),
               "Accélération")


  # ---- Hausse ----
  expect_equal(gETa_nom_taux(1, 1),
               "une poursuite de la hausse")


  # ---- Ralentissement ----
  expect_equal(gETa_nom_taux(0.3, 1),
               "un ralentissement")


  # ---- Rebond ----
  expect_equal(gETa_nom_taux(0.1, -1),
               "un rebond")


  # ---- Baisse ----
  expect_equal(gETa_nom_taux(-0.1, -1),
               "une poursuite de la baisse")

  expect_equal(gETa_nom_taux(-4, -1, TRUE),
               "Nouveau recul")

  expect_equal(gETa_nom_taux(-21, 1),
               "une chute")

})

test_that("gETa_nom_taux - aléatoire", {

  # ---- Accélération ----
  res <- gETa_nom_taux(10, 1, alea = 0.5)
  expect_true(res %in% c("une accélération", "un regain de dynamisme"))

  # ---- Stabilisation ----
  res <- gETa_nom_taux(0.049, 2, alea = 0.5)
  expect_true(res %in% c("une stabilisation", "un essoufflement"))

  # ---- Hausse ----
  res <- gETa_nom_taux(1, 1, alea = 0.5)
  expect_true(res %in% c("une poursuite de la hausse", "le prolongement de la hausse"))

  # ---- Rebond ----
  res <- gETa_nom_taux(0.1, -1, alea = 0.5)
  expect_true(res %in% c("un rebond", "un redressement"))

  # ---- Baisse ----
  res <- gETa_nom_taux(-0.1, -1, alea = 0.5)
  expect_true(res %in% c("une poursuite de la baisse", "un repli"))

  # ---- Chute ----
  res <- gETa_nom_taux(-21, 1, alea = 0.5)
  expect_true(res %in% c("une chute", "un effondrement"))

})
