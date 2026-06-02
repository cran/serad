#' Évolution verbale ne tenant pas compte de l'accélération et suivie de la valeur
#'
#' @description
#' Décrit une évolution sous forme verbale à partir de deux niveaux,
#' sans tenir compte d'une éventuelle accélération, et en ajoutant
#' la valeur de variation.
#'
#' @param x1 Le niveau le plus récent.
#' @param x2 Le niveau le plus ancien.
#' @param sing Indicateur logique : TRUE si le sujet est singulier
#'   (par défaut), FALSE sinon.
#' @param evolution Type d'évolution :
#'   `"pourcents"` (variation relative, par défaut) ou `"points"`.
#' @param stable_sans_valeur Indicateur logique.
#'   TRUE (par défaut) : n'affiche pas la valeur en cas de stabilité.
#'   FALSE : affiche la valeur après la formulation de stabilité.
#' @param lang Langue de sortie : "fr" ou "en".
#'
#' @return
#' Une chaîne de caractères correspondant à la formulation verbale
#' retenue, par exemple : "bondit de 10,0 %".
#'
#' @details
#' La fonction calcule d'abord une évolution à partir de `x1`
#' et `x2` :
#' \itemize{
#'   \item si `evolution = "pourcents"`, elle utilise
#'   \code{\link{g}} ;
#'   \item si `evolution = "points"`, elle calcule `x1 - x2`.
#' }
#'
#' La valeur obtenue est ensuite transmise à \code{\link{g_verbe_taux}},
#' qui détermine la formulation à partir de la table
#' `getOption("serad")$evo_simple`.
#'
#' @section Personnalisation:
#' Les formulations utilisées par cette fonction proviennent de la table
#' `getOption("serad")$evo_simple`.
#'
#' Pour modifier les seuils ou les libellés, voir
#' \code{\link{init_serad}}.
#'
#' @seealso
#' \code{\link{g_verbe_taux}},
#' \code{\link{g}},
#' \code{\link{init_serad}}
#'
#' @examples
#' g_verbe(1.1, 1)
#' g_verbe(1.04, 1)
#' g_verbe(1.01, 1, sing = FALSE)
#' g_verbe(1.003, 1)
#' g_verbe(0.999, 1)
#' g_verbe(0.999, 1, stable_sans_valeur = FALSE)
#' g_verbe(0.96, 1)
#' g_verbe(0.79, 1)
#'
#' @export
g_verbe <- function(x1, x2,
                    sing = TRUE,
                    evolution = c("pourcents", "points"),
                    stable_sans_valeur = TRUE,
                    lang = get_serad_language()) {

  evolution <- match.arg(evolution)

  valeur <- if (evolution == "pourcents") {
    serad::g(x1, x2)
  } else {
    x1 - x2
  }

  g_verbe_taux(
    valeur,
    sing = sing,
    evolution = evolution,
    stable_sans_valeur = stable_sans_valeur,
    lang = lang
  )
}
