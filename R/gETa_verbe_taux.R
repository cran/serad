#' Évolution verbale tenant compte de l'accélération
#'
#' @description
#' Décrit l'évolution sous forme verbale en tenant compte
#' de l'accélération entre deux variations successives.
#'
#' @param g1 Dernière évolution, exprimée en pourcentage.
#' @param g2 Évolution précédente, exprimée en pourcentage.
#' @param sing Indicateur logique : TRUE si le sujet est singulier
#'   (par défaut), FALSE sinon.
#' @param alea Nombre réel compris entre 0 et 1 contrôlant
#'   l'utilisation de formulations alternatives. Si `alea = 0`,
#'   la formulation est déterministe. Si `alea = 1`, la formulation
#'   alternative est toujours utilisée. Des valeurs intermédiaires
#'   permettent un tirage aléatoire.
#' @param lang Langue de sortie : "fr" ou "en".
#'
#' @return
#' Une chaîne de caractères correspondant à la formulation
#' verbale retenue (par exemple : "accélère", "se stabilise").
#'
#' @details
#' La fonction calcule d'abord l'accélération entre `g1`
#' et `g2` à l'aide de \code{\link{a}}.
#'
#' Elle parcourt ensuite la table \code{getOption("serad")$evo_accel}
#' ligne par ligne. Chaque ligne contient un ensemble de conditions
#' sur `g1`, `g2` et l'accélération. La première ligne pour laquelle
#' toutes les conditions sont vérifiées détermine la formulation
#' retenue.
#'
#' Une formulation alternative peut être utilisée via la table
#' \code{getOption("serad")$evo_accel_alt}. Le choix entre la
#' formulation principale et la variante dépend du paramètre `alea`.
#'
#' Si `sing = TRUE`, la fonction renvoie la colonne `verbe_sing`.
#' Sinon, elle renvoie `verbe_plur`.
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
#' gETa_verbe_taux(0.049, 0.049)
#' gETa_verbe_taux(10, 1)
#' gETa_verbe_taux(4, 1, FALSE)
#' gETa_verbe_taux(-4, 1)
#' gETa_verbe_taux(-21, 1)
#' gETa_verbe_taux(10, 1, alea = 0.5)
#'
#' @seealso
#' \code{\link{a}},
#' \code{\link{gETa_nom_taux}},
#' \code{\link{init_serad}}
#'
#' @importFrom stats runif
#' @export
gETa_verbe_taux <- function(g1, g2,
                            sing = TRUE,
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

  if (nrow(tab) != nrow(tab_alt)) {
    stop("evo_accel et evo_accel_alt doivent avoir le meme nombre de lignes")
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

      if (sing) {
        return(pick(
          as.character(tab$verbe_sing[i]),
          as.character(tab_alt$verbe_sing_alt[i]),
          alea
        ))
      } else {
        return(pick(
          as.character(tab$verbe_plur[i]),
          as.character(tab_alt$verbe_plur_alt[i]),
          alea
        ))
      }

    }
  }

  NA_character_
}
