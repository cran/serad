test_that("g_nom_taux - classification des niveaux", {

  expect_equal(g_nom_taux(8),   "une forte hausse")
  expect_equal(g_nom_taux(4),   "une hausse")
  expect_equal(g_nom_taux(0.4, titre = TRUE), "Légère hausse")
  expect_equal(g_nom_taux(0.1), "une stabilité")

  expect_equal(g_nom_taux(-0.3), "une légère baisse")
  expect_equal(g_nom_taux(-1),   "une baisse")
  expect_equal(g_nom_taux(-4, titre = TRUE), "Baisse")
  expect_equal(g_nom_taux(-7),   "une forte baisse")

})
