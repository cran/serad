#' @rdname init_serad
#' @export
init_serad_en <- function() {

  # ###                          Parametres globaux                          -----

  serad0 <- list()

  # arrondis utilisés dans :
  # - format_niv()
  # - format_delta()
  serad0$arrondi_niv <- -2

  # arrondis utilisés dans :
  # - format_g()
  # - format_pts()
  serad0$arrondi_pourcent <- 1

  # symbole du signe négatif utilisé dans :
  # - format_g()
  # - format_pts()
  # - format_delta()
  serad0$moins <- "-"

  # ###                           Evolution simple                           -----

  # Table principale utilisée par :
  # - g_verbe_taux()
  # - g_nom_taux()
  # - g_verbe()
  # - g_nom()
  # Permet d’associer une variation (g) à :
  # - un verbe (singulier / pluriel)
  # - une formulation nominale
  evo_simple <- tibble::tribble(
    ~seuil, ~verbe_sing, ~verbe_plur, ~nom,

    9.95,   "surged by",              "surged by",              "a sharp increase",
    3.95,   "rose sharply by",        "rose sharply by",        "a strong increase",
    0.95,   "rose by",                "rose by",                "an increase",
    0.25,   "increased by",           "increased by",           "a moderate increase",
    0.05,   "edged up by",            "edged up by",            "a slight increase",
    -0.15,  "was stable",             "were stable",            "stability",
    -0.35,  "decreased slightly by",  "decreased slightly by",  "a slight decline",
    -1.05,  "fell slightly by",       "fell slightly by",       "a moderate decline",
    -4.05,  "decreased by",           "decreased by",           "a decline",
    -20.05, "fell by",                "fell by",                "a sharp decline",
    -Inf,   "plunged by",             "plunged by",             "a collapse"
  )

  serad0$evo_simple <- evo_simple

  # ###                      Evolution avec accélération                     -----

  # Seuils utilisés dans :
  # - gETa_verbe_taux()
  # - gETa_nom_taux()
  # Définissent la logique d'accélération / ralentissement
  serad0$seuil <- list(
    stable = 0.05,
    g2_bas = -0.5,
    g2_haut = 0.95,
    g1_bas = -10,
    g1_tres_bas = -20,
    accel_hausse = 30,
    accel_baisse = -30,
    accel_recul = 30
  )

  # Table principale utilisée par :
  # - gETa_verbe_taux()
  # - gETa_nom_taux()
  # Chaque ligne définit un cas logique basé sur :
  # - g1 (évolution récente)
  # - g2 (évolution passée)
  # - a (accélération)
  evo_accel <- tibble::tribble(
    ~cond_g1, ~cond_g2, ~cond_a, ~verbe_sing, ~verbe_plur, ~nom,

    # Strong increase
    "g1 >= seuil_stable", "g2 >= seuil_stable", "a > seuil_accel_hausse",
    "accelerated", "accelerated", "an acceleration",

    "g1 >= seuil_stable", "g2 >= seuil_stable", "a < seuil_accel_baisse",
    "slowed", "slowed", "a slowdown",

    "g1 >= seuil_stable", "g2 >= seuil_stable", "a >= seuil_accel_baisse & a <= seuil_accel_hausse",
    "continued to rise", "continued to rise", "continued growth",

    "g1 >= seuil_stable", "g2 >= seuil_g2_bas & g2 < seuil_stable", "TRUE",
    "increased", "increased", "an increase",

    "g1 >= seuil_stable", "g2 < seuil_g2_bas", "TRUE",
    "rebounded", "rebounded", "a rebound",

    # Stability
    "abs(g1) < seuil_stable", "abs(g2) >= seuil_stable", "TRUE",
    "stabilised", "stabilised", "a stabilisation",

    "abs(g1) < seuil_stable", "abs(g2) < seuil_stable", "TRUE",
    "remained stable", "remained stable", "stability",

    # Decline
    "g1 >= seuil_g1_bas & g1 < -seuil_stable", "g2 > seuil_g2_haut", "TRUE",
    "fell back", "fell back", "a fallback",

    "g1 >= seuil_g1_bas & g1 < -seuil_stable", "g2 >= -seuil_stable & g2 <= seuil_g2_haut", "TRUE",
    "decreased", "decreased", "a decrease",

    "g1 >= seuil_g1_bas & g1 < -seuil_stable", "g2 < -seuil_stable", "a > seuil_accel_recul",
    "declined again", "declined again", "a renewed decline",

    "g1 >= seuil_g1_bas & g1 < -seuil_stable", "g2 < -seuil_stable", "a <= seuil_accel_recul",
    "continued to fall", "continued to fall", "continued decline",

    # Sharp decline
    "g1 >= seuil_g1_tres_bas & g1 < seuil_g1_bas", "g2 > seuil_g2_haut", "TRUE",
    "fell back sharply", "fell back sharply", "a sharp fallback",

    "g1 >= seuil_g1_tres_bas & g1 < seuil_g1_bas", "g2 <= seuil_g2_haut", "TRUE",
    "fell sharply", "fell sharply", "a sharp decline",

    "g1 < seuil_g1_tres_bas", "TRUE", "TRUE",
    "collapsed", "collapsed", "a collapse"
  )

  # Table alternative utilisée par :
  # - gETa_verbe_taux()
  # - gETa_nom_taux()
  # Permet d’introduire de la variabilité (alea)
  evo_accel_alt <- tibble::tribble(
    ~verbe_sing_alt, ~verbe_plur_alt, ~nom_alt,

    "grew faster",            "grew faster",            "renewed momentum",
    "moderated",              "moderated",              "a slowdown",
    "continued to increase",  "continued to increase",  "continued growth",
    "advanced",               "advanced",               "an increase",
    "recovered",              "recovered",              "a recovery",
    "levelled off",           "levelled off",           "a stabilisation",
    "remained stable",        "remained stable",        "stability",
    "pulled back",            "pulled back",            "a pullback",
    "declined",               "declined",               "a decrease",
    "fell again",             "fell again",             "a renewed decline",
    "continued to fall",      "continued to fall",      "continued decline",
    "pulled back sharply",    "pulled back sharply",    "a sharp pullback",
    "fell sharply",           "fell sharply",           "a sharp decline",
    "collapsed",              "collapsed",              "a collapse"
  )

  serad0$evo_accel <- evo_accel
  serad0$evo_accel_alt <- evo_accel_alt

  # ###                      Enregistrement des options                      -----

  options(serad = serad0)
  invisible(serad0)
}
