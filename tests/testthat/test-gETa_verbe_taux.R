test_that("gETa_verbe_taux - partition principale", {

  # ---- Stabilité ----
  expect_equal(gETa_verbe_taux(0.049, 0.049),
               "reste stable")

  expect_equal(gETa_verbe_taux(0.049, 2),
               "se stabilise")

  # ---- Accélération ----
  expect_equal(gETa_verbe_taux(10, 1),
               "accélère")

  expect_equal(gETa_verbe_taux(4, 1, 0),
               "accélèrent")

  # ---- Progression simple ----
  expect_equal(gETa_verbe_taux(1, 1),
               "poursuit sa hausse")

  # ---- Forte baisse ----
  expect_equal(gETa_verbe_taux(-20, 1),
               "se replie fortement")

  expect_equal(gETa_verbe_taux(-20, 0),
               "baisse fortement")

  expect_equal(gETa_verbe_taux(-21, 1),
               "chute")
})

test_that("gETa_verbe_taux - aléatoire", {

  # ---- Accélération ----
  res <- gETa_verbe_taux(10, 1, alea = 0.5)
  expect_true(res %in% c("accélère", "augmente plus vite"))

  # ---- Stabilisation ----
  res <- gETa_verbe_taux(0.049, 2, alea = 0.5)
  expect_true(res %in% c("se stabilise", "se fige"))

  # ---- Hausse ----
  res <- gETa_verbe_taux(1, 1, alea = 0.5)
  expect_true(res %in% c("poursuit sa hausse", "continue d'augmenter"))

  # ---- Rebond ----
  res <- gETa_verbe_taux(0.1, -1, alea = 0.5)
  expect_true(res %in% c("repart à la hausse", "se redresse"))

  # ---- Baisse ----
  res <- gETa_verbe_taux(-0.1, -1, alea = 0.5)
  expect_true(res %in% c("poursuit sa baisse", "se replie"))

  # ---- Chute ----
  res <- gETa_verbe_taux(-21, 1, alea = 0.5)
  expect_true(res %in% c("chute", "s'effondre"))

})
