#' @rdname init_serad
#' @export
init_serad_fr <- function() {

  # ###                          Parametres globaux                          -----

  serad0 <- list()

  # arrondis utilises dans :
  # - format_niv()
  # - format_delta()
  serad0$arrondi_niv <- -2 # arrondi a la centaine

  # arrondis utilises dans :
  # - format_g()
  # - format_pts()
  serad0$arrondi_pourcent <- 1 # arrondi a un chiffre apres la virgule

  # symbole du signe negatif utilise dans :
  # - format_g()
  # - format_pts()
  # - format_delta()
  serad0$moins <- "-"

  # ###                           Evolution simple                           -----

  # Table principale utilisee par :
  # - g_verbe_taux()
  # - g_nom_taux()
  # - g_verbe()
  # - g_nom()
  #
  # Associe une variation (g) a :
  # - un verbe (singulier / pluriel)
  # - une formulation nominale
  evo_simple <- tibble::tribble(
    ~seuil, ~verbe_sing, ~verbe_plur, ~nom,

    9.95,   "bondit de",                                        "bondissent de",                                   "une forte hausse",
    3.95,   "s'accro\u00EEt de",                                "s'accroissent de",                                "une forte hausse",
    0.95,   "est en hausse de",                                 "sont en hausse de",                               "une hausse",
    0.25,   "augmente de",                                      "augmentent de",                                   "une hausse mod\u00E9r\u00E9e",
    0.05,   "s'accro\u00EEt tr\u00E8s l\u00E9g\u00E8rement de", "s'accroissent tr\u00E8s l\u00E9g\u00E8rement de", "une l\u00E9g\u00E8re hausse",
    -0.15,  "est stable",                                       "sont stables",                                    "une stabilit\u00E9",
    -0.35,  "diminue l\u00E9g\u00E8rement de",                  "diminuent l\u00E9g\u00E8rement de",               "une l\u00E9g\u00E8re baisse",
    -1.05,  "recule l\u00E9g\u00E8rement de",                   "reculent l\u00E9g\u00E8rement de",                "une baisse mod\u00E9r\u00E9e",
    -4.05,  "baisse de",                                        "baissent de",                                     "une baisse",
    -20.05, "recule de",                                        "reculent de",                                     "une forte baisse",
    -Inf,   "chute de",                                         "chutent de",                                      "une forte baisse"
  )

  serad0$evo_simple <- evo_simple

  # ###                      Evolution avec acceleration                     -----

  # Seuils utilises dans :
  # - gETa_verbe_taux()
  # - gETa_nom_taux()
  # - gETa_verbe()
  # - gETa_nom()
  #
  # Definissent la logique d'acceleration / ralentissement
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

  # Table principale utilisee par :
  # - gETa_verbe_taux()
  # - gETa_nom_taux()
  # - gETa_verbe()
  # - gETa_nom()
  #
  # Chaque ligne correspond a un cas logique base sur :
  # - g1 (evolution recente)
  # - g2 (evolution passee)
  # - a  (acceleration)
  evo_accel <- tibble::tribble(
    ~cond_g1, ~cond_g2, ~cond_a, ~verbe_sing, ~verbe_plur, ~nom,

    # Hausse forte
    "g1 >= seuil_stable", "g2 >= seuil_stable", "a > seuil_accel_hausse",
    "acc\u00E9l\u00E8re", "acc\u00E9l\u00E8rent", "une acc\u00E9l\u00E9ration",

    "g1 >= seuil_stable", "g2 >= seuil_stable", "a < seuil_accel_baisse",
    "ralentit", "ralentissent", "un ralentissement",

    "g1 >= seuil_stable", "g2 >= seuil_stable", "a >= seuil_accel_baisse & a <= seuil_accel_hausse",
    "poursuit sa hausse", "poursuivent leur hausse", "une poursuite de la hausse",

    "g1 >= seuil_stable", "g2 >= seuil_g2_bas & g2 < seuil_stable", "TRUE",
    "augmente", "augmentent", "une hausse",

    "g1 >= seuil_stable", "g2 < seuil_g2_bas", "TRUE",
    "repart \u00E0 la hausse", "repartent \u00E0 la hausse", "un rebond",

    # Stabilite
    "abs(g1) < seuil_stable", "abs(g2) >= seuil_stable", "TRUE",
    "se stabilise", "se stabilisent", "une stabilisation",

    "abs(g1) < seuil_stable", "abs(g2) < seuil_stable", "TRUE",
    "reste stable", "restent stables", "une stabilit\u00E9",

    # Baisse
    "g1 >= seuil_g1_bas & g1 < -seuil_stable", "g2 > seuil_g2_haut", "TRUE",
    "se replie", "se replient", "un repli",

    "g1 >= seuil_g1_bas & g1 < -seuil_stable", "g2 >= -seuil_stable & g2 <= seuil_g2_haut", "TRUE",
    "baisse", "baissent", "une baisse",

    "g1 >= seuil_g1_bas & g1 < -seuil_stable", "g2 < -seuil_stable", "a > seuil_accel_recul",
    "recule de nouveau", "reculent de nouveau", "un nouveau recul",

    "g1 >= seuil_g1_bas & g1 < -seuil_stable", "g2 < -seuil_stable", "a <= seuil_accel_recul",
    "poursuit sa baisse", "poursuivent leur baisse", "une poursuite de la baisse",

    # Baisse forte
    "g1 >= seuil_g1_tres_bas & g1 < seuil_g1_bas", "g2 > seuil_g2_haut", "TRUE",
    "se replie fortement", "se replie fortement", "un fort repli",

    "g1 >= seuil_g1_tres_bas & g1 < seuil_g1_bas", "g2 <= seuil_g2_haut", "TRUE",
    "baisse fortement", "baissent fortement", "une forte baisse",

    "g1 < seuil_g1_tres_bas", "TRUE", "TRUE",
    "chute", "chutent", "une chute"
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

    "augmente plus vite",      "augmentent plus vite",       "un regain de dynamisme",
    "se mod\u00E8re",          "se mod\u00E8rent",           "un essoufflement",
    "continue d'augmenter",    "continuent d'augmenter",     "le prolongement de la hausse",
    "progresse",               "progressent",                "une progression",
    "se redresse",             "se redressent",              "un redressement",
    "se fige",                 "se figent",                  "une stabilisation",
    "demeure stable",          "demeurent stables",          "une stabilit\u00E9",
    "recul",                   "reculent",                   "un recul",
    "diminue",                 "diminuent",                  "une diminution",
    "repart \u00E0 la baisse", "repartent \u00E0 la baisse", "un nouveau recul",
    "poursuit sa baisse",      "poursuivent leur baisse",    "une poursuite de la baisse",
    "recul fortement",         "recul fortement",            "un fort recul",
    "diminue fortement",       "diminuent fortement",        "une forte diminution",
    "s'effondre",              "s'effondrent",               "un effondrement"
  )

  serad0$evo_accel <- evo_accel
  serad0$evo_accel_alt <- evo_accel_alt

  # ###                      Enregistrement des options                      -----

  options(serad = serad0)
  invisible(serad0)
}
