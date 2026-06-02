test_that("a - cas sans acceleration", {
  expect_equal(
    a(4, 2, 1),
    0
  )
})

test_that("a - cas acceleration positive", {
  expect_equal(
    a(6, 2, 1),
    100
  )
})

test_that("a - division par zero", {
  # depend de la valeur par defaut de serad0$eps = 1e-8
  expect_warning(
    a(2, 1, 1),
    "division par 0 dans serad::g()"
  )
})
