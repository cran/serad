test_that("fonctions dates - trimestres et mois", {

  # =========================
  # Trimestres
  # =========================

  expect_equal(
    quelTrim(3, 2023),
    "troisième trimestre 2023"
  )

  expect_equal(
    quelTrim(3, 2023, majuscule = 1),
    "Troisième trimestre 2023"
  )

  expect_equal(
    quelTrim(3, 2023, type = "chiffres"),
    "3^e^ trimestre 2023"
  )

  expect_equal(
    quelTrim(1, 2023, type = "chiffres"),
    "1^er^ trimestre 2023"
  )

  expect_equal(
    prevTrim(1, 2023),
    "quatrième trimestre 2022"
  )

  expect_equal(
    nextTrim(3, 2023),
    "quatrième trimestre 2023"
  )

  expect_equal(
    nextTrim(4, 2023),
    "premier trimestre 2024"
  )

  # =========================
  # Mois
  # =========================

  expect_equal(
    quelMois(3, 2023),
    "mars 2023"
  )

  expect_equal(
    quelMois(3, 2023, majuscule = 1),
    "Mars 2023"
  )

  expect_equal(
    quelMois(3, 2023, type = "autres"),
    "mars"
  )

  expect_equal(
    prevMois(1, 2023),
    "décembre 2022"
  )

  expect_equal(
    nextMois(3, 2023),
    "avril 2023"
  )

  expect_equal(
    nextMois(12, 2023),
    "janvier 2024"
  )

  # =========================
  # Parsing texte
  # =========================

  expect_equal(
    whichMois("En Juil 98"),
    7
  )

})
