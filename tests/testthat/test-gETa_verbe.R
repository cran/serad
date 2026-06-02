test_that("gETa_verbe - pipeline complet", {

  set.seed(123)

  # ---- Stabilité ----
  expect_equal(
    gETa_verbe(1.00049, 1, 0.9996),
    "reste stable"
  )

  expect_equal(
    gETa_verbe(1.00049, 1, 0.981),
    "se stabilise"
  )

  # ---- Accélération ----
  expect_equal(
    gETa_verbe(1.1, 1, 0.99),
    "accélère"
  )

  expect_equal(
    gETa_verbe(1.1, 1, 0.99, 0),
    "accélèrent"
  )

  expect_equal(
    gETa_verbe(1.04, 1, 0.99),
    "accélère"
  )

  # ---- Progression simple ----
  expect_equal(
    gETa_verbe(1.01, 1, 0.99),
    "poursuit sa hausse"
  )

  # ---- Forte baisse ----
  expect_equal(
    gETa_verbe(0.8, 1, 0.99),
    "se replie fortement"
  )

  expect_equal(
    gETa_verbe(0.8, 1, 1),
    "baisse fortement"
  )

  expect_equal(
    gETa_verbe(0.79, 1, 0.99),
    "chute"
  )

  # ---- Verbe aléatoire ----
  expect_equal(
    gETa_verbe(1.003,1,0.99,0),
    "ralentissent"
  )

  expect_equal(
    gETa_verbe(1.001,1,1.01),
    "repart à la hausse"
  )

  expect_equal(
    gETa_verbe(0.999,1,1.01),
    "poursuit sa baisse"
  )

  expect_equal(
    gETa_verbe(0.96,1,1.01),
    "recule de nouveau"
  )

  expect_equal(
    gETa_verbe(0.96,1,0.99),
    "se replie"
  )
})

test_that("gETa_verbe - aléatoire", {

  set.seed(123)

  # ---- Accélération ----
  res <- gETa_verbe(1.1, 1, 0.99, alea = 0.5)
  expect_true(res %in% c("accélère", "augmente plus vite"))

  # ---- Stabilisation ----
  res <- gETa_verbe(1.00049, 1, 0.98, alea = 0.5)
  expect_true(res %in% c("se stabilise", "se fige"))

  # ---- Hausse ----
  expect_warning(gETa_verbe(1.01, 1, 1, alea = 0.5),
                 regexp = "division par 0 dans serad::g()")
  res <- suppressWarnings(gETa_verbe(1.01, 1, 1, alea = 0.5))
  expect_true(res %in% c("augmente", "progresse"))

  # ---- Rebond ----
  res <- gETa_verbe(1.001, 1, 1.01, alea = 0.5)
  expect_true(res %in% c("repart à la hausse", "se redresse"))

  # ---- Baisse ----
  res <- gETa_verbe(0.99, 1, 1.01, alea = 0.5)
  expect_true(res %in% c("poursuit sa baisse", "se replie"))

  # ---- Chute ----
  res <- gETa_verbe(0.79, 1, 1.01, alea = 0.5)
  expect_true(res %in% c("chute", "s'effondre"))

})
