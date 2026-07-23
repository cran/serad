#' @rdname init_serad
#' @export
init_serad_fr <- function() {

  # ###                         Parametres globaux                         -----

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

  # ###                          Evolution simple                          -----

  # Table principale utilisee par :
  # - g_verbe_taux()
  # - g_nom_taux()
  # - g_verbe()
  # - g_nom()
  #
  # Permet d’associer une variation (g) à :
  # - un verbe (singulier / pluriel)
  # - une formulation nominale
  evo_simple <- tibble::tribble(
    ~condition,                         ~verbe_sing,                                  ~verbe_plur,                                    ~nom,

    "g > 6.95",                         "augmente fortement de",                       "augmentent fortement de",                       "une forte hausse",
    "g > 0.95 & g <= 6.95",             "augmente de",                                 "augmentent de",                                 "une hausse",
    "g > 0.10 & g <= 0.95",             "augmente l\u00E9g\u00E8rement de",             "augmentent l\u00E9g\u00E8rement de",             "une l\u00E9g\u00E8re hausse",
    "g >= -0.10 & g <= 0.10",           "est stable \u00E0",                            "sont stables \u00E0",                            "une stabilit\u00E9",
    "g >= -0.95 & g < -0.10",           "baisse l\u00E9g\u00E8rement de",                "baissent l\u00E9g\u00E8rement de",                "une l\u00E9g\u00E8re baisse",
    "g >= -6.95 & g < -0.95",           "baisse de",                                   "baissent de",                                   "une baisse",
    "g < -6.95",                        "baisse fortement de",                         "baissent fortement de",                         "une forte baisse"
  )

  serad0$evo_simple <- evo_simple

  # ###                     Evolution avec acceleration                    -----

  # Seuils utilises dans :
  # - gETa_verbe_taux()
  # - gETa_nom_taux()
  # - gETa_verbe()
  # - gETa_nom()
  #
  # Definissent la logique d'acceleration / ralentissement
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

    # Forte hausse
    "g1 > seuil_g1_tres_haut", "TRUE", "TRUE",
    "s'envole", "s'envolent", "une envol\u00e9e",

    "g1 > seuil_g1_haut & g1 <= seuil_g1_tres_haut", "g2 >= seuil_g2_bas", "TRUE",
    "augmente fortement", "augmentent fortement", "une forte hausse",

    "g1 > seuil_g1_haut & g1 <= seuil_g1_tres_haut", "g2 < seuil_g2_bas", "TRUE",
    "repart fortement \u00E0 la hausse", "repartent fortement \u00E0 la hausse", "un fort rebond",

    # Hausse
    "g1 > seuil_stable & g1 <= seuil_g1_haut", "g2 > seuil_stable", "a > seuil_accel_pos",
    "acc\u00E9l\u00E8re", "acc\u00E9l\u00E8rent", "une acc\u00E9l\u00E9ration",

    "g1 > seuil_stable & g1 <= seuil_g1_haut", "g2 > seuil_stable", "a >= seuil_accel_neg & a <= seuil_accel_pos",
    "augmente de nouveau", "augmente de nouveau", "une nouvelle hausse",

    "g1 > seuil_stable & g1 <= seuil_g1_haut", "g2 > seuil_stable", "a < seuil_accel_neg",
    "ralentit", "ralentissent", "un ralentissement",

    "g1 > seuil_stable & g1 <= seuil_g1_haut", "g2 >= seuil_g2_bas & g2 <= seuil_stable", "TRUE",
    "augmente", "augmentent", "une hausse",

    "g1 > seuil_stable & g1 <= seuil_g1_haut", "g2 < seuil_g2_bas", "TRUE",
    "repart \u00E0 la hausse", "repartent \u00E0 la hausse", "un rebond",

    # Stabilite
    "abs(g1) <= seuil_stable", "abs(g2) >= seuil_stable", "TRUE",
    "se stabilise", "se stabilisent", "une stabilisation",

    "abs(g1) <= seuil_stable", "abs(g2) < seuil_stable", "TRUE",
    "reste stable", "restent stables", "une stabilit\u00E9",

    # Baisse
    "g1 >= seuil_g1_bas & g1 < -seuil_stable", "g2 > seuil_g2_haut", "TRUE",
    "se replie", "se replient", "un repli",

    "g1 >= seuil_g1_bas & g1 < -seuil_stable", "g2 >= -seuil_stable & g2 <= seuil_g2_haut", "TRUE",
    "baisse", "baissent", "une baisse",

    "g1 >= seuil_g1_bas & g1 < -seuil_stable", "g2 < -seuil_stable", "a > seuil_accel_pos",
    "baisse plus fortement qu'au mois p\u00E9c\u00E9dent", "baissent plus fortement qu'au mois p\u00E9c\u00E9dent", "un nouveau recul",

    "g1 >= seuil_g1_bas & g1 < -seuil_stable", "g2 < -seuil_stable", "a >= seuil_accel_neg & a <= seuil_accel_pos",
    "baisse de nouveau", "baissent de nouveau", "une nouvelle baisse",

    "g1 >= seuil_g1_bas & g1 < -seuil_stable", "g2 < -seuil_stable", "a < seuil_accel_neg",
    "baisse moins fortement qu'au mois p\u00E9c\u00E9dent", "baissent moins fortement qu'au mois p\u00E9c\u00E9dent", "un ralentissement de la baisse",

    # Baisse forte
    "g1 >= seuil_g1_tres_bas & g1 < seuil_g1_bas", "g2 > seuil_g2_haut", "TRUE",
    "se replie fortement", "se replient fortement", "un fort repli",

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
    "augmente tr\u00e8s fortement", "augmentent tr\u00e8s fortement", "une tr\u00e8s forte hausse",
    "cro\u00eet fortement",         "croissent fortement",            "une forte croissance",
    "se redresse fortement",        "se redressent fortement",        "un fort redressement",
    "augmente plus vite",           "augmentent plus vite",           "un regain de dynamisme",
    "continue d'augmenter",         "continuent d'augmenter",         "le prolongement de la hausse",
    "se mod\u00E8re",               "se mod\u00E8rent",               "un essoufflement",
    "progresse",                    "progressent",                    "une progression",
    "se redresse",                  "se redressent",                  "un redressement",
    "se fige",                      "se figent",                      "une stabilisation",
    "demeure stable",               "demeurent stables",              "une stabilit\u00E9",
    "recul",                        "reculent",                       "un recul",
    "diminue",                      "diminuent",                      "une diminution",
    "repart \u00E0 la baisse",      "repartent \u00E0 la baisse",     "un nouveau recul",
    "poursuit sa baisse",           "poursuivent leur baisse",        "une poursuite de la baisse",
    "baisse moins fortement",       "baissent moins fortement",       "une baisse moins forte",
    "recul fortement",              "recul fortement",                "un fort recul",
    "diminue fortement",            "diminuent fortement",            "une forte diminution",
    "s'effondre",                   "s'effondrent",                   "un effondrement"
  )

  serad0$evo_accel <- evo_accel
  serad0$evo_accel_alt <- evo_accel_alt

  # ###                     Enregistrement des options                     -----

  options(serad = serad0)
  invisible(serad0)
}
