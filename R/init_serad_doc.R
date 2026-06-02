#' Initialisation des règles de rédaction de \{serad\}
#'
#' @description
#' Les fonctions `init_serad_fr()` et `init_serad_en()` définissent
#' l'ensemble des règles utilisées par \{serad\} pour produire des textes
#' de conjoncture.
#'
#' Elles initialisent notamment :
#' \itemize{
#'   \item les règles d'arrondi ;
#'   \item le symbole de signe négatif ;
#'   \item les seuils d'évolution ;
#'   \item les tables de correspondance entre évolutions et formulations ;
#'   \item les règles de gestion de l'accélération.
#' }
#'
#' @details
#' Les principales structures utilisées sont :
#'
#' \itemize{
#'   \item `evo_simple` : utilisée pour les évolutions simples
#'   (\code{\link{g_nom_taux}}, \code{\link{g_verbe_taux}}) ;
#'
#'   \item `evo_accel` : utilisée pour les évolutions tenant compte
#'   de l'accélération
#'   (\code{\link{gETa_nom_taux}}, \code{\link{gETa_verbe_taux}}) ;
#'
#'   \item `evo_accel_alt` : variantes utilisées lorsque l'argument
#'   \code{alea} est activé.
#' }
#'
#' @section Personnalisation :
#'
#' La méthode recommandée pour personnaliser les règles de rédaction
#' consiste à copier le contenu de `init_serad_fr()` ou
#' `init_serad_en()` dans un script utilisateur, puis à modifier
#' directement les tables et les seuils.
#'
#' Exemple :
#'
#' \preformatted{
#' # Copier le contenu de init_serad_fr()
#'
#' serad0 <- list()
#'
#' serad0$evo_simple <- tibble::tribble(
#'   ~seuil, ~verbe_sing, ~verbe_plur, ~nom,
#'   1, "augmente", "augmentent", "une hausse",
#'   0, "est stable", "sont stables", "une stabilité",
#'   -Inf, "diminue", "diminuent", "une baisse"
#' )
#'
#' options(serad = serad0)
#' }
#'
#' Cette approche permet :
#' \itemize{
#'   \item une personnalisation complète des formulations ;
#'   \item une adaptation aux conventions métier ;
#'   \item une reproductibilité via un script versionné.
#' }
#'
#' @return
#' Pas de valeur de retour, appelée pour ses effets de bord.
#'
#' @seealso
#' \code{\link{g_nom_taux}},
#' \code{\link{g_verbe_taux}},
#' \code{\link{gETa_nom_taux}},
#' \code{\link{gETa_verbe_taux}}
#'
#' @name init_serad
#' @aliases init_serad_fr init_serad_en
NULL
