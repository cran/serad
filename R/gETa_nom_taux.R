#' Évolution nominale tenant compte de l'accélération
#'
#' @description
#' Décrit l'évolution sous forme nominale en tenant compte
#' de l'accélération entre deux variations successives.
#'
#' @param g1 Dernière évolution, exprimée en pourcentage.
#' @param g2 Évolution précédente, exprimée en pourcentage.
#' @param titre Indicateur logique : TRUE pour supprimer l'article
#'   initial et mettre une majuscule, notamment en début de titre.
#' @param alea Paramètre numérique compris entre 0 et 1 contrôlant
#'   l'utilisation de formulations alternatives. Si `alea = 0`,
#'   la formulation est déterministe. Si `alea = 1`, la formulation
#'   alternative est toujours utilisée. Des valeurs intermédiaires
#'   permettent un tirage aléatoire.
#' @param lang Langue de sortie : "fr" ou "en".
#'
#' @return
#' Une chaîne de caractères correspondant à la formulation
#' nominale retenue (par exemple : "une accélération",
#' "une stabilisation").
#'
#' @details
#' La fonction calcule d'abord l'accélération entre `g1`
#' et `g2` à l'aide de \code{\link{a}}.
#'
#' Elle parcourt ensuite la table \code{getOption("serad")$evo_accel}
#' ligne par ligne. Chaque ligne contient un ensemble de conditions
#' sur `g1`, `g2` et l'accélération. La première ligne dont
#' toutes les conditions sont vérifiées détermine la formulation
#' retenue.
#'
#' Une formulation alternative peut être utilisée via la table
#' \code{getOption("serad")$evo_accel_alt}, qui contient une variante
#' pour chaque ligne de `evo_accel`. Le choix entre la formulation
#' principale et la variante dépend du paramètre `alea`.
#'
#' Si `titre = TRUE`, l'article initial est supprimé et la
#' première lettre restante est mise en majuscule.
#'
#' @section Personnalisation:
#' Les formulations utilisées par cette fonction proviennent des tables
#' \code{getOption("serad")$evo_accel} et
#' \code{getOption("serad")$evo_accel_alt}.
#'
#' Pour modifier les seuils, les conditions ou les libellés, voir
#' \code{\link{init_serad}}.
#'
#' @examples
#' gETa_nom_taux(0.049, 0.049)
#' gETa_nom_taux(10, 1)
#' gETa_nom_taux(-4, 1, titre = TRUE)
#' gETa_nom_taux(-21, 1)
#' gETa_nom_taux(10, 1, alea = 0.5)
#'
#' @seealso
#' \code{\link{a}},
#' \code{\link{gETa_verbe_taux}},
#' \code{\link{init_serad}}
#'
#' @importFrom stats runif
#' @export
gETa_nom_taux <- function(g1, g2,
                          titre = FALSE,
                          alea = 0,
                          lang = get_serad_language()) {

  serad <- getOption("serad")
  tab <- serad$evo_accel
  tab_alt <- serad$evo_accel_alt
  seuil <- serad$seuil

  a <- serad::g(g1, g2)

  pick <- function(base, alt, alea) {
    if (alea == 0 || is.na(alt) || alt == "") return(base)
    if (runif(1) < alea) alt else base
  }

  env <- list2env(c(
    list(g1 = g1, g2 = g2, a = a),
    list(
      seuil_stable       = seuil$stable,
      seuil_g2_bas       = seuil$g2_bas,
      seuil_g2_haut      = seuil$g2_haut,
      seuil_g1_bas       = seuil$g1_bas,
      seuil_g1_tres_bas  = seuil$g1_tres_bas,
      seuil_accel_hausse = seuil$accel_hausse,
      seuil_accel_baisse = seuil$accel_baisse,
      seuil_accel_recul  = seuil$accel_recul
    )
  ))

  for (i in seq_len(nrow(tab))) {

    cond <- c(tab$cond_g1[i], tab$cond_g2[i], tab$cond_a[i])
    cond <- cond[cond != "TRUE"]

    results <- sapply(cond, function(x) eval(parse(text = x), envir = env))

    if (length(results) == 0 || all(results)) {

      res <- pick(
        base = as.character(tab$nom[i]),
        alt  = as.character(tab_alt$nom_alt[i]),
        alea = alea
      )

      if (titre) {

        if (lang == "en") {
          res <- sub(
            "^(a|an|the)\\s+",
            "",
            res,
            ignore.case = TRUE
          )
        } else {
          res <- sub(
            "^(une|un|des|la|le|les|du|de la|de l'|d'|l')\\s*",
            "",
            res,
            ignore.case = TRUE
          )
        }

        res <- paste0(
          toupper(substr(res, 1, 1)),
          substr(res, 2, nchar(res))
        )
      }

      return(res)
    }
  }

  NA_character_
}
