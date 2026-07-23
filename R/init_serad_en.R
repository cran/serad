#' @rdname init_serad
#' @export
init_serad_en <- function() {

  # ###                         Parametres globaux                         -----

  serad0 <- list()

  # arrondis utilisés dans :
  # - format_niv()
  # - format_delta()
  serad0$arrondi_niv <- -2 # arrondi a la centaine

  # arrondis utilisés dans :
  # - format_g()
  # - format_pts()
  serad0$arrondi_pourcent <- 1 # arrondi a un chiffre apres la virgule

  # symbole du signe négatif utilisé dans :
  # - format_g()
  # - format_pts()
  # - format_delta()
  serad0$moins <- "-"

  # ###                          Evolution simple                          -----

  # Table principale utilisée par :
  # - g_verbe_taux()
  # - g_nom_taux()
  # - g_verbe()
  # - g_nom()
  #
  # Permet d’associer une variation (g) à :
  # - un verbe (singulier / pluriel)
  # - une formulation nominale
  evo_simple <- tibble::tribble(
    ~condition,                         ~verbe_sing,                 ~verbe_plur,                 ~nom,

    "g > 6.95",                         "increased sharply by",       "increased sharply by",       "a sharp increase",
    "g > 0.95 & g <= 6.95",             "increased by",               "increased by",               "an increase",
    "g > 0.05 & g <= 0.95",             "increased slightly by",      "increased slightly by",      "a slight increase",
    "g >= -0.05 & g <= 0.05",           "was stable at",              "were stable at",                "stability",
    "g >= -0.95 & g < -0.05",           "decreased slightly by",      "decreased slightly by",      "a slight decrease",
    "g >= -6.95 & g < -0.95",           "decreased by",               "decreased by",               "a decrease",
    "g < -6.95",                        "decreased sharply by",       "decreased sharply by",       "a sharp decrease"
  )

  serad0$evo_simple <- evo_simple

  # ###                     Evolution avec accélération                    -----

  # Seuils utilisés dans :
  # - gETa_verbe_taux()
  # - gETa_nom_taux()
  # - gETa_verbe()
  # - gETa_nom()
  #
  # Définissent la logique d'accélération / ralentissement
  serad0$seuil <- list(
    seuil_stable = 0.05,

    seuil_g1_tres_haut = 20,
    seuil_g1_haut = 10,
    seuil_g1_bas = -10,
    seuil_g1_tres_bas = -20,

    seuil_g2_bas = -0.95,
    seuil_g2_haut = 0.95,

    seuil_accel_pos = 30,
    seuil_accel_neg = -30
  )

  # Table principale utilisée par :
  # - gETa_verbe_taux()
  # - gETa_nom_taux()
  # - gETa_verbe()
  # - gETa_nom()
  #
  # Chaque ligne définit un cas logique basé sur :
  # - g1 (évolution récente)
  # - g2 (évolution passée)
  # - a (accélération)
  evo_accel <- tibble::tribble(
    ~cond_g1, ~cond_g2, ~cond_a, ~verbe_sing, ~verbe_plur, ~nom,

    # Strong increase
    "g1 > seuil_g1_tres_haut", "TRUE", "TRUE",
    "soared", "soared", "a surge",

    "g1 > seuil_g1_haut & g1 <= seuil_g1_tres_haut", "g2 >= seuil_g2_bas", "TRUE",
    "increased sharply", "increased sharply", "a sharp increase",

    "g1 > seuil_g1_haut & g1 <= seuil_g1_tres_haut", "g2 < seuil_g2_bas", "TRUE",
    "rebounded sharply", "rebounded sharply", "a sharp rebound",

    # Strong increase
    "g1 > seuil_stable & g1 <= seuil_g1_haut", "g2 >= seuil_stable", "a > seuil_accel_hausse",
    "accelerated", "accelerated", "an acceleration",

    "g1 > seuil_stable & g1 <= seuil_g1_haut", "g2 >= seuil_stable", "a >= seuil_accel_baisse & a <= seuil_accel_hausse",
    "increased again", "increased again", "a renewed increase",

    "g1 > seuil_stable & g1 <= seuil_g1_haut", "g2 >= seuil_stable", "a < seuil_accel_baisse",
    "slowed", "slowed", "a slowdown",

    "g1 > seuil_stable & g1 <= seuil_g1_haut", "g2 >= seuil_g2_bas & g2 < seuil_stable", "TRUE",
    "increased", "increased", "an increase",

    "g1 > seuil_stable & g1 <= seuil_g1_haut", "g2 < seuil_g2_bas", "TRUE",
    "rebounded", "rebounded", "a rebound",

    # Stability
    "abs(g1) <= seuil_stable", "abs(g2) >= seuil_stable", "TRUE",
    "stabilised", "stabilised", "a stabilisation",

    "abs(g1) <= seuil_stable", "abs(g2) < seuil_stable", "TRUE",
    "remained stable", "remained stable", "stability",

    # Decline
    "g1 >= seuil_g1_bas & g1 < -seuil_stable", "g2 > seuil_g2_haut", "TRUE",
    "fell back", "fell back", "a fallback",

    "g1 >= seuil_g1_bas & g1 < -seuil_stable", "g2 >= -seuil_stable & g2 <= seuil_g2_haut", "TRUE",
    "decreased", "decreased", "a decrease",

    "g1 >= seuil_g1_bas & g1 < -seuil_stable", "g2 < -seuil_stable", "a > seuil_accel_pos",
    "declined more sharply than in the previous month", "declined more sharply than in the previous month", "a sharper decline",

    "g1 >= seuil_g1_bas & g1 < -seuil_stable", "g2 < -seuil_stable", "a >= seuil_accel_neg & a <= seuil_accel_pos",
    "decreased again", "decreased again", "a renewed decrease",

    "g1 >= seuil_g1_bas & g1 < -seuil_stable", "g2 < -seuil_stable", "a < seuil_accel_neg",
    "declined less sharply than in the previous month", "declined less sharply than in the previous month", "a slowdown in the decline",

    # Sharp decline
    "g1 >= seuil_g1_tres_bas & g1 < seuil_g1_bas", "g2 > seuil_g2_haut", "TRUE",
    "fell back sharply", "fell back sharply", "a sharp fallback",

    "g1 >= seuil_g1_tres_bas & g1 < seuil_g1_bas", "g2 <= seuil_g2_haut", "TRUE",
    "fell sharply", "fell sharply", "a sharp decline",

    "g1 < seuil_g1_tres_bas", "TRUE", "TRUE",
    "plunged", "plunged", "a plunge"
  )

  # Table alternative utilisee par :
  # - gETa_verbe_taux()
  # - gETa_nom_taux()
  # - gETa_verbe()
  # - gETa_nom()
  #
  # Permet d'introduire de la variabilite via le parametre alea
  evo_accel_alt <- tibble::tribble(
    ~verbe_sing_alt, ~verbe_plur_alt, ~nom_alt,

    "rose very sharply",      "rose very sharply",      "a very sharp increase",
    "rose sharply",           "rose sharply",           "a sharp increase",
    "picked up sharply",      "picked up sharply",      "a sharp pickup",
    "grew faster",            "grew faster",            "renewed momentum",
    "continued to increase",  "continued to increase",  "continued growth",
    "moderated",              "moderated",              "a slowdown",
    "advanced",               "advanced",               "an increase",
    "recovered",              "recovered",              "a recovery",
    "levelled off",           "levelled off",           "a stabilisation",
    "remained stable",        "remained stable",        "stability",
    "pulled back",            "pulled back",            "a pullback",
    "declined",               "declined",               "a decrease",
    "fell again",             "fell again",             "a renewed decline",
    "continued to fall",      "continued to fall",      "continued decline",
    "decreased less sharply", "decreased less sharply", "a less sharp decrease",
    "pulled back sharply",    "pulled back sharply",    "a sharp pullback",
    "fell sharply",           "fell sharply",           "a sharp decline",
    "collapsed",              "collapsed",              "a collapse"
  )

  serad0$evo_accel <- evo_accel
  serad0$evo_accel_alt <- evo_accel_alt

  # ###                    Enregistrement des options                      -----

  options(serad = serad0)
  invisible(serad0)
}
