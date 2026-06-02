#' Indique si le niveau est le plus haut ou le plus bas de la série
#'
#' Détermine si la dernière valeur d'une série correspond
#' au plus haut ou au plus bas niveau observé depuis
#' un certain nombre de périodes.
#'
#' @param df Data frame en entrée.
#' @param temps Ordre temporel : "croissant" (par défaut)
#'   si les valeurs vont du passé vers le présent,
#'   "décroissant" sinon.
#' @param voc_haut Formulation utilisée pour un plus haut niveau.
#'   Si omis, dépend de la langue.
#' @param voc_bas Formulation utilisée pour un plus bas niveau.
#'   Si omis, dépend de la langue.
#' @param vart Variable de temps du data frame.
#'   Par défaut : première colonne.
#' @param vary Variable numérique du data frame.
#'   Par défaut : seconde colonne.
#' @param nbperiode Nombre minimal de périodes depuis
#'   lesquelles le niveau doit être un extremum pour
#'   retourner une phrase (par défaut : 3).
#' @param lang Langue de sortie : "fr" ou "en".
#'
#' @return
#' Une chaîne de caractères indiquant le plus haut
#' ou le plus bas niveau depuis une date donnée.
#' Retourne une chaîne vide si rien de notable.
#'
#' @examples
#' col0 <- c("Y1T1", "Y1T2", "Y1trim3", "Y1T4", "Y2T1", "Y2-T2")
#' col1 <- c(12, 11, 7, 6, 9, 10)
#' col2 <- c(12, 11, 7, 6, 9, 14)
#' col3 <- c(12, 11, 3, 6, 9, 4)
#' col4 <- c(12, 11, 7, 6, 9, 4)
#' col5 <- c(12, 11, 7, 3, 9, 4)
#' df1 <- data.frame(col0, col1, col2, col3, col4, col5)
#'
#' plushautniveau(df1)
#' plushautniveau(df1, nbperiode = 5)
#' plushautniveau(df1, vary = "col2")
#' plushautniveau(df1, vary = "col3")
#' plushautniveau(df1, vary = "col4")
#'
#' @export
plushautniveau <- function(df,
                           temps = "croissant",
                           voc_haut,
                           voc_bas,
                           vart,
                           vary,
                           nbperiode = 3,
                           lang = get_serad_language()) {

  # ---- Vocabulaires par defaut ----
  if (missing(voc_haut)) {
    voc_haut <- if (lang == "en") {
      "This is the highest level since"
    } else {
      "C'est le plus haut niveau depuis"
    }
  }

  if (missing(voc_bas)) {
    voc_bas <- if (lang == "en") {
      "This is the lowest level since"
    } else {
      "C'est le plus bas niveau depuis"
    }
  }

  debut_serie <- if (lang == "en") {
    "the start of the series"
  } else {
    "le d\u00E9but de la s\u00E9rie"
  }

  # ---- Verifications ----
  if (!is.data.frame(df) || ncol(df) < 2)
    stop("df doit \u00EAtre un dataframe avec au moins 2 colonnes")

  if (!temps %in% c("croissant", "decroissant"))
    stop("temps doit \u00EAtre 'croissant' ou 'decroissant'")

  if (!is.numeric(nbperiode) || length(nbperiode) != 1 ||
      nbperiode < 2 ||
      abs(nbperiode - round(nbperiode)) > .Machine$double.eps) {
    stop("nbperiode doit etre un entier >= 2")
  }

  if (!missing(vart) && !(vart %in% names(df)))
    stop("vart doit \u00EAtre un nom de variable du dataframe")

  if (!missing(vary) && !(vary %in% names(df)))
    stop("vary doit \u00EAtre un nom de variable du dataframe")

  # ---- Extraction des variables ----
  xt <- if (missing(vart)) df[[1]] else df[[vart]]
  xy <- if (missing(vary)) df[[2]] else df[[vary]]

  if (!is.numeric(xy))
    stop("La s\u00E9rie d'int\u00E9r\u00EAt doit \u00EAtre num\u00E9rique")

  if (temps == "decroissant") {
    xt <- rev(xt)
    xy <- rev(xy)
  }

  if (length(xy) < 2)
    return("")

  last_val  <- utils::tail(xy, 1)
  prev_vals <- utils::head(xy, -1)

  if (all(is.na(prev_vals)))
    return("")

  # =========================
  # CAS PLUS HAUT
  # =========================
  if (all(last_val >= utils::tail(prev_vals, nbperiode - 1))) {

    idx_candidates <- which(prev_vals > last_val)

    if (length(idx_candidates) == 0) {
      idx <- 0
    } else {
      idx <- max(idx_candidates)
    }

    nb <- length(prev_vals) - idx

    if (nb < nbperiode - 1)
      return("")

    if (idx == 0) {
      return(paste0(voc_haut, " ", debut_serie, "."))
    } else {
      return(paste0(voc_haut, " ", xt[idx], "."))
    }
  }

  # =========================
  # CAS PLUS BAS
  # =========================
  if (all(last_val <= utils::tail(prev_vals, nbperiode - 1))) {

    idx_candidates <- which(prev_vals < last_val)

    if (length(idx_candidates) == 0) {
      idx <- 0
    } else {
      idx <- max(idx_candidates)
    }

    nb <- length(prev_vals) - idx

    if (nb < nbperiode - 1)
      return("")

    if (idx == 0) {
      return(paste0(voc_bas, " ", debut_serie, "."))
    } else {
      return(paste0(voc_bas, " ", xt[idx], "."))
    }
  }

  return("")
}
