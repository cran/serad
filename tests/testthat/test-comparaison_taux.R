test_that("comparaison_taux - cas hausse", {
  expect_equal(
    comparaison_taux(5, "augmente", "reste stable", "diminue"),
    "augmente"
  )
})

test_that("comparaison_taux - cas egalite", {
  expect_equal(
    comparaison_taux(0.05, "augmente", "reste stable", "diminue"),
    "reste stable"
  )
})

test_that("comparaison_taux - seuil nul", {
  expect_equal(
    comparaison_taux(0.05, "augmente", "reste stable", "diminue", seuil = 0),
    "augmente"
  )

  expect_equal(
    comparaison_taux(0, "augmente", "reste stable", "diminue", seuil = 0),
    "augmente"
  )
})

test_that("comparaison_taux - formes alternatives", {
  expect_equal(
    comparaison_taux(
      0,
      "as", "bs", "cs",
      seuil = 0,
      alt = 1,
      hausse_alt = "a",
      egalite_alt = "b",
      baisse_alt = "c"
    ),
    "a"
  )

  expect_equal(
    comparaison_taux(
      0,
      "as", "bs", "cs",
      seuil = 0,
      alt = 0,
      hausse_alt = "a",
      egalite_alt = "b",
      baisse_alt = "c"
    ),
    "as"
  )

  expect_equal(
    comparaison_taux(
      0,
      "as", "bs", "cs",
      seuil = 0,
      hausse_alt = "a",
      egalite_alt = "b",
      baisse_alt = "c"
    ),
    "as"
  )
})
