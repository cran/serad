test_that("g_verbe_taux - classification complète", {

  # ---- Hausse ----
  expect_equal(g_verbe_taux(10, stable_sans_valeur = FALSE),
               "augmente fortement de 10,0\ua0%")

  expect_equal(g_verbe_taux(4),
               "augmente de 4,0\ua0%")

  expect_equal(g_verbe_taux(1, sing = FALSE),
               "augmentent de 1,0\ua0%")

  expect_equal(g_verbe_taux(0.3),
               "augmente légèrement de 0,3\ua0%")

  # ---- Stabilité ----
  expect_equal(g_verbe_taux(-0.1, stable_sans_valeur = FALSE),
               "est stable à -0,1\ua0%")

  expect_equal(g_verbe_taux(-0.1, stable_sans_valeur = TRUE),
               "est stable")

  # ---- Baisse ----
  expect_equal(g_verbe_taux(-0.3),
               "baisse légèrement de 0,3\ua0%")

  expect_equal(g_verbe_taux(-4),
               "baisse de 4,0\ua0%")

  expect_equal(g_verbe_taux(-20),
               "baisse fortement de 20,0\ua0%")

})
