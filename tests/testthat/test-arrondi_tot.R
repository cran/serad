test_that("arrondi_tot - arrondi a l'unite", {
  expect_equal(
    arrondi_tot(1877.85, digit = 0),
    1878
  )
})

test_that("arrondi_tot - arrondi a 1 decimale", {
  expect_equal(
    arrondi_tot(1877.85, digit = 1),
    1877.9
  )
})

test_that("arrondi_tot - arrondi a 2 decimales", {
  expect_equal(
    arrondi_tot(1877.85, digit = 2),
    1877.85
  )
})

test_that("arrondi_tot - arrondi a la dizaine", {
  expect_equal(
    arrondi_tot(1877.85, digit = -1),
    1880
  )
})

test_that("arrondi_tot - arrondi a la centaine", {
  expect_equal(
    arrondi_tot(1877.85, digit = -2),
    1900
  )
})
