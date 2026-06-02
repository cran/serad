#' Évolution nominale tenant compte de l'accélération
#'
#' @description
#' Décrit l'évolution sous forme nominale à partir de trois niveaux,
#' en tenant compte de l'accélération entre deux variations successives.
#'
#' @param x1 Niveau le plus récent.
#' @param x2 Niveau précédent.
#' @param x3 Niveau le plus ancien.
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
#' Une chaîne de caractères correspondant à la formulation nominale
#' retenue.
#'
#' @details
#' La fonction calcule d'abord deux évolutions successives :
#' `g1 <- serad::g(x1, x2)` et `g2 <- serad::g(x2, x3)`.
#'
#' Ces deux variations sont ensuite transmises à
#' \code{\link{gETa_nom_taux}}, qui calcule leur accélération à l'aide
#' de \code{\link{a}} et détermine la formulation nominale appropriée
#' à partir de la table `getOption("serad")$evo_accel`.
#'
#' Une formulation alternative peut être utilisée via la table
#' `getOption("serad")$evo_accel_alt`. Le choix entre la
#' formulation principale et la variante dépend du paramètre `alea`.
#'
#' @section Personnalisation:
#' Les formulations utilisées par cette fonction proviennent des tables
#' `getOption("serad")$evo_accel` et
#' `getOption("serad")$evo_accel_alt`.
#'
#' Pour modifier les seuils, les conditions ou les libellés, voir
#' \code{\link{init_serad}}.
#'
#' @seealso
#' \code{\link{gETa_nom_taux}},
#' \code{\link{g}},
#' \code{\link{a}},
#' \code{\link{init_serad}}
#'
#' @examples
#' gETa_nom(1.00049, 1, 0.9996)
#' gETa_nom(1.1, 1, 0.99)
#' gETa_nom(1.003, 1, 0.99)
#' gETa_nom(0.96, 1, 1.01)
#' gETa_nom(0.8, 1, 0.99)
#' gETa_nom(1.1, 1, 0.99, alea = 0.5)
#'
#' @export
gETa_nom <- function(x1, x2, x3,
                     titre = FALSE,
                     alea = 0,
                     lang = get_serad_language()) {

  g1 <- serad::g(x1, x2)
  g2 <- serad::g(x2, x3)

  gETa_nom_taux(
    g1,
    g2,
    titre = titre,
    alea = alea,
    lang = lang
  )
}
