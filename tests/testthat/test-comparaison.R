# ===============================
# comparaison
# ===============================

test_that("comparaison - cas hausse", {
  expect_equal(
    comparaison(1.04, 1, "augmente", "reste stable", "diminue"),
    "augmente"
  )
})

test_that("comparaison - cas egalite", {
  expect_equal(
    comparaison(0.9991, 1, "augmente", "reste stable", "diminue"),
    "reste stable"
  )
})

test_that("comparaison - cas baisse", {
  expect_equal(
    comparaison(0.999, 1, "augmente", "reste stable", "diminue"),
    "diminue"
  )
})

test_that("comparaison - seuil nul", {
  expect_equal(
    comparaison(0.9991, 1, "augmente", "reste stable", "diminue", seuil = 0),
    "diminue"
  )

  expect_equal(
    comparaison(1, 1, "augmente", "reste stable", "diminue", seuil = 0),
    "augmente"
  )
})


# ===============================
# audessus
# ===============================

test_that("audessus", {
  expect_equal(audessus(1.04, 1), "au-dessus")
  expect_equal(audessus(1.04, 1.04), "au-dessus")
  expect_equal(audessus(0.96, 1), "en dessous")
})


# ===============================
# alahausse
# ===============================

test_that("alahausse", {
  expect_equal(alahausse(1.004, 1), "à la hausse")
  expect_equal(alahausse(0.996, 1), "à la baisse")
  expect_equal(alahausse(1, 1.0004), "inchangé")
})


# ===============================
# davantage
# ===============================

test_that("davantage", {
  expect_equal(davantage(1.04, 1), "davantage")
  expect_equal(davantage(1.04, 1.04), "davantage")
  expect_equal(davantage(0.96, 1), "moins")
})


# ===============================
# depasse
# ===============================

test_that("depasse", {
  expect_equal(depasse(1.04, 1), "excèdent")
  expect_equal(depasse(1.04, 1, sing = 1), "excède")
  expect_equal(depasse(0.96, 1), "sont en dessous de")
  expect_equal(depasse(0.9991, 1), "sont au niveau de")
})


# ===============================
# g_nom_simple
# ===============================

test_that("g_nom_simple", {
  expect_equal(g_nom_simple(3, 1), "en hausse de 200,0\u00a0%")
  expect_equal(g_nom_simple(3, 5), "en baisse de 40,0\u00a0%")
})
