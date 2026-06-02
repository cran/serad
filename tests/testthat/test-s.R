test_that("s - gestion singulier/pluriel", {

  # ---- Cas simples ----
  expect_equal(s(-7.5), "s")
  expect_equal(s(-2),   "s")
  expect_equal(s(1.97), "")

  # ---- Formes personnalisées ----
  expect_equal(
    s(1.4, "chat parle", "chats parlent"),
    "chat parle"
  )

  expect_equal(
    s(-2, "chat parle", "chats parlent"),
    "chats parlent"
  )

  # ---- Interaction avec arrondi_tot ----
  expect_equal(
    s(arrondi_tot(1.97)),
    "s"
  )

  # ---- Seuil personnalisé ----
  expect_equal(
    s(1.97, seuil = 1.95),
    "s"
  )

})
