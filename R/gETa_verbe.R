#' Évolution verbale tenant compte de l'accélération
#'
#' @description
#' Décrit l'évolution sous forme verbale à partir de trois niveaux,
#' en tenant compte de l'accélération entre deux variations successives.
#'
#' @param x1 Niveau le plus récent.
#' @param x2 Niveau précédent.
#' @param x3 Niveau le plus ancien.
#' @param sing Indicateur logique : TRUE si le sujet est singulier
#'   (par défaut), FALSE sinon.
#' @param alea Paramètre numérique compris entre 0 et 1 contrôlant
#'   l'utilisation de formulations alternatives. Si `alea = 0`,
#'   la formulation est déterministe. Si `alea = 1`,
#'   la formulation alternative est toujours utilisée. Des valeurs
#'   intermédiaires permettent un tirage aléatoire.
#' @param lang Langue de sortie : "fr" ou "en".
#'
#' @return
#' Une chaîne de caractères correspondant à la formulation verbale retenue.
#'
#' @details
#' La fonction calcule d'abord deux évolutions successives :
#' `g1 <- serad::g(x1, x2)` et `g2 <- serad::g(x2, x3)`.
#'
#' Ces deux variations sont ensuite transmises à
#' \code{\link{gETa_verbe_taux}}, qui calcule leur accélération à l'aide
#' de \code{\link{g}} et détermine la formulation verbale appropriée
#' à partir de la table `getOption("serad")$evo_accel`.
#'
#' Une formulation alternative peut être utilisée via la table
#' `getOption("serad")$evo_accel_alt`. Le choix entre la formulation
#' principale et la variante dépend du paramètre `alea`.
#'
#' @section Personnalisation :
#' Les formulations utilisées par cette fonction proviennent des tables
#' `getOption("serad")$evo_accel` et
#' `getOption("serad")$evo_accel_alt`.
#'
#' Pour modifier les seuils, les conditions ou les libellés, voir
#' \code{\link{init_serad}}.
#'
#' @seealso
#' \code{\link{gETa_verbe_taux}},
#' \code{\link{g}},
#' \code{\link{init_serad}}
#'
#' @examples
#' gETa_verbe(1.00049, 1, 0.9996)
#' gETa_verbe(1.1, 1, 0.99)
#' gETa_verbe(1.003, 1, 0.99, sing = FALSE)
#' gETa_verbe(0.96, 1, 1.01)
#' gETa_verbe(1.1, 1, 0.99, alea = 0.5)
#'
#' @export
gETa_verbe <- function(x1, x2, x3,
                       sing = TRUE,
                       alea = 0,
                       lang = get_serad_language()) {

  g1 <- serad::g(x1, x2)
  g2 <- serad::g(x2, x3)

  gETa_verbe_taux(
    g1,
    g2,
    sing = sing,
    alea = alea,
    lang = lang
  )
}
