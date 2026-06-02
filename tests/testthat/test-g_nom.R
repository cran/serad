test_that("g_nom - classification des niveaux", {

  expect_equal(g_nom(1.04, 1),  "une forte hausse")
  expect_equal(g_nom(1.01, 1),  "une hausse")
  expect_equal(g_nom(1.004, 1, titre = TRUE), "Hausse modérée")
  expect_equal(g_nom(1.001, 1), "une légère hausse")
  expect_equal(g_nom(1, 1),     "une stabilité")

  expect_equal(g_nom(0.997, 1), "une légère baisse")
  expect_equal(g_nom(0.99, 1),  "une baisse modérée")
  expect_equal(g_nom(0.96, 1, titre = TRUE),  "Baisse")
  expect_equal(g_nom(0.95, 1),  "une forte baisse")

})
