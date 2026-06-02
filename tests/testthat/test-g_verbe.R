test_that("g_verbe - classification complète", {

  # ---- Hausse ----
  expect_equal(g_verbe(1.1, 1),
               "bondit de 10,0\ua0%")

  expect_equal(g_verbe(1.04, 1),
               "s'accro\u00EEt de 4,0\ua0%")

  expect_equal(g_verbe(1.01, 1, sing = FALSE),
               "sont en hausse de 1,0\ua0%")

  expect_equal(g_verbe(1.003, 1),
               "augmente de 0,3\ua0%")

  expect_equal(g_verbe(1.001, 1),
               "s'accro\u00EEt très légèrement de 0,1\ua0%")

  # ---- Stabilité ----
  expect_equal(g_verbe(0.999, 1, stable_sans_valeur = FALSE),
               "est stable à -0,1\ua0%")

  expect_equal(g_verbe(0.999, 1, stable_sans_valeur = TRUE),
               "est stable")

  expect_equal(g_verbe(0.999, 1),
               "est stable")

  # ---- Baisse ----
  expect_equal(g_verbe(0.997, 1),
               "diminue légèrement de 0,3\ua0%")

  expect_equal(g_verbe(0.99, 1),
               "recule légèrement de 1,0\ua0%")

  expect_equal(g_verbe(0.96, 1),
               "baisse de 4,0\ua0%")

  expect_equal(g_verbe(0.8, 1),
               "recule de 20,0\ua0%")

  expect_equal(g_verbe(0.79, 1),
               "chute de 21,0\ua0%")

})
