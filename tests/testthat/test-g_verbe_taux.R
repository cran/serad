test_that("g_verbe_taux - classification complète", {

  # ---- Hausse ----
  expect_equal(g_verbe_taux(10, stable_sans_valeur = FALSE),
               "bondit de 10,0\ua0%")

  expect_equal(g_verbe_taux(4),
               "s'accro\u00EEt de 4,0\ua0%")

  expect_equal(g_verbe_taux(1, sing = FALSE),
               "sont en hausse de 1,0\ua0%")

  expect_equal(g_verbe_taux(0.3),
               "augmente de 0,3\ua0%")

  expect_equal(g_verbe_taux(0.1),
               "s'accro\u00EEt très légèrement de 0,1\ua0%")

  # ---- Stabilité ----
  expect_equal(g_verbe_taux(-0.1, stable_sans_valeur = FALSE),
               "est stable à -0,1\ua0%")

  expect_equal(g_verbe_taux(-0.1, stable_sans_valeur = TRUE),
               "est stable")

  expect_equal(g_verbe_taux(-0.1),
               "est stable")

  # ---- Baisse ----
  expect_equal(g_verbe_taux(-0.3),
               "diminue légèrement de 0,3\ua0%")

  expect_equal(g_verbe_taux(-1),
               "recule légèrement de 1,0\ua0%")

  expect_equal(g_verbe_taux(-4),
               "baisse de 4,0\ua0%")

  expect_equal(g_verbe_taux(-20),
               "recule de 20,0\ua0%")

  expect_equal(g_verbe_taux(-21),
               "chute de 21,0\ua0%")

})
