#' Fournit dynamiquement le trimestre en toutes lettres ou en chiffres
#'
#' Retourne une formulation littéraire du trimestre
#' pour une année donnée.
#'
#' @param trim Trimestre sous forme numérique : 1, 2, 3 ou 4.
#' @param annee Année sur 4 chiffres.
#' @param type "lettres" (par défaut) ou "chiffres".
#' @param majuscule Indicateur logique : TRUE pour une majuscule initiale
#'   (ex. "Premier"), FALSE sinon.
#' @param exposant Indicateur logique : TRUE pour afficher
#'   les exposants Markdown (ex. "1^er^"),
#'   FALSE pour afficher "1er".
#' @param mois Si renseigné (entier entre 1 et 12),
#'   remplace `trim` en déduisant le trimestre correspondant.
#' @param lang Langue de sortie : "fr" ou "en".
#'
#' @return
#' Une chaîne de caractères du type
#' "troisième trimestre 2023"
#' ou "3^e^ trimestre 2023".
#'
#' @examples
#' quelTrim(3, 2023)
#' quelTrim(3, 2023, majuscule = TRUE)
#' quelTrim(3, 2023, type = "chiffres")
#' quelTrim(1, 2023, type = "chiffres")
#' quelTrim(999, 2023, type = "chiffres", mois = 8)
#' quelTrim(3, 2023, type = "chiffres", exposant = FALSE)
#'
#' @seealso \code{\link{nextTrim}}, \code{\link{prevTrim}}
#'
#' @export
quelTrim <- function(trim,
                     annee,
                     type = c("lettres", "chiffres"),
                     majuscule = FALSE,
                     exposant = TRUE,
                     mois = NULL,
                     lang = get_serad_language()) {

  type <- match.arg(type)

  annee <- as.numeric(annee)
  if (is.na(annee)) stop("annee doit \u00EAtre num\u00E9rique")

  if (!is.null(mois)) {
    mois <- as.numeric(mois)
    if (is.na(mois) || !(mois %in% 1:12))
      stop("mois doit \u00EAtre entre 1 et 12")
    trim <- ceiling(mois / 3)
  } else {
    trim <- as.numeric(trim)
    if (is.na(trim) || !(trim %in% 1:4))
      stop("trim doit \u00EAtre entre 1 et 4")
  }

  if (type == "lettres") {

    if (lang == "en") {
      noms <- c("first", "second", "third", "fourth")
      unite <- "quarter"
    } else {
      noms <- c("premier", "deuxi\u00E8me", "troisi\u00E8me", "quatri\u00E8me")
      unite <- "trimestre"
    }

    b <- noms[trim]

    if (majuscule)
      b <- paste0(toupper(substr(b, 1, 1)), substr(b, 2, nchar(b)))

  } else {

    if (lang == "en") {
      if (exposant) {
        b <- switch(as.character(trim),
                    "1" = "1^st^",
                    "2" = "2^nd^",
                    "3" = "3^rd^",
                    "4" = "4^th^")
      } else {
        b <- switch(as.character(trim),
                    "1" = "1st",
                    "2" = "2nd",
                    "3" = "3rd",
                    "4" = "4th")
      }
      unite <- "quarter"
    } else {
      if (exposant) {
        b <- if (trim == 1) "1^er^" else paste0(trim, "^e^")
      } else {
        b <- if (trim == 1) "1er" else paste0(trim, "e")
      }
      unite <- "trimestre"
    }
  }

  paste(b, unite, annee)
}


#' Fournit dynamiquement le trimestre suivant
#'
#' Retourne la formulation du trimestre situé à `k` périodes
#' après le trimestre indiqué.
#'
#' @param trim Trimestre sous forme numérique : 1, 2, 3 ou 4.
#' @param annee Année sur 4 chiffres.
#' @param type "lettres" (par défaut) ou "chiffres".
#' @param majuscule Indicateur logique : TRUE pour une majuscule initiale,
#'   FALSE sinon.
#' @param exposant Indicateur logique : TRUE pour afficher
#'   les exposants Markdown (ex. "1^er^"),
#'   FALSE pour afficher "1er".
#' @param k Décalage en nombre de trimestres.
#'   Par défaut : 1 (trimestre suivant).
#' @param lang Langue de sortie : "fr" ou "en".
#'
#' @return
#' Une chaîne de caractères correspondant au trimestre suivant.
#'
#' @examples
#' nextTrim(3, 2023)
#' nextTrim(4, 2023)
#'
#' @seealso \code{\link{quelTrim}}, \code{\link{prevTrim}}
#'
#' @export
nextTrim <- function(trim,
                     annee,
                     type = c("lettres", "chiffres"),
                     majuscule = FALSE,
                     exposant = TRUE,
                     k = 1,
                     lang = get_serad_language()) {

  type <- match.arg(type)

  trim  <- as.numeric(trim)
  annee <- as.numeric(annee)

  if (is.na(trim) || !(trim %in% 1:4))
    stop("trim doit \u00EAtre entre 1 et 4")

  if (is.na(annee))
    stop("annee doit \u00EAtre num\u00E9rique")

  if (!is.numeric(k))
    stop("k doit \u00EAtre num\u00E9rique")

  total <- trim - 1 + k

  new_trim  <- (total %% 4) + 1
  new_annee <- annee + (total %/% 4)

  quelTrim(
    new_trim,
    new_annee,
    type = type,
    majuscule = majuscule,
    exposant = exposant,
    lang = lang
  )
}


#' Fournit dynamiquement le trimestre précédent
#'
#' Retourne la formulation du trimestre situé à `k` périodes
#' avant le trimestre indiqué.
#'
#' @param trim Trimestre sous forme numérique : 1, 2, 3 ou 4.
#' @param annee Année sur 4 chiffres.
#' @param type "lettres" (par défaut) ou "chiffres".
#' @param majuscule Indicateur logique : TRUE pour une majuscule initiale,
#'   FALSE sinon.
#' @param exposant Indicateur logique : TRUE pour afficher
#'   les exposants Markdown (ex. "1^er^"),
#'   FALSE pour afficher "1er".
#' @param k Décalage en nombre de trimestres.
#'   Par défaut : 1 (trimestre précédent).
#' @param lang Langue de sortie : "fr" ou "en".
#'
#' @return
#' Une chaîne de caractères correspondant au trimestre précédent.
#'
#' @examples
#' prevTrim(1, 2023)
#' prevTrim(1, 2023, type = "chiffres", exposant = FALSE)
#'
#' @seealso \code{\link{nextTrim}}, \code{\link{quelTrim}}
#'
#' @export
prevTrim <- function(trim,
                     annee,
                     type = c("lettres", "chiffres"),
                     majuscule = FALSE,
                     exposant = TRUE,
                     k = 1,
                     lang = get_serad_language()) {

  type <- match.arg(type)

  nextTrim(
    trim,
    annee,
    type = type,
    majuscule = majuscule,
    exposant = exposant,
    k = -k,
    lang = lang
  )
}


#' Fournit dynamiquement le mois en toutes lettres
#'
#' Retourne une formulation littéraire du mois,
#' avec ou sans année.
#'
#' @param mois Mois sous forme numérique : 1 à 12.
#' @param annee Année sur 4 chiffres.
#' @param type "Annee" (par défaut, ex. "janvier 2023")
#'   ou "autres" (ex. "janvier").
#' @param majuscule Indicateur logique : TRUE pour une majuscule
#'   initiale (ex. "Janvier"), FALSE sinon.
#' @param lang Langue de sortie : "fr" ou "en".
#'
#' @return
#' Une chaîne de caractères correspondant au mois,
#' avec ou sans année.
#'
#' @examples
#' quelMois(3, 2023)
#' quelMois(3, 2023, majuscule = TRUE)
#' quelMois(3, 2023, type = "autres")
#'
#' @seealso \code{\link{quelTrim}}, \code{\link{nextMois}}, \code{\link{prevMois}}
#'
#' @export
quelMois <- function(mois,
                     annee,
                     type = c("Annee", "autres"),
                     majuscule = FALSE,
                     lang = get_serad_language()) {

  type <- match.arg(type)

  mois  <- as.numeric(mois)
  annee <- as.numeric(annee)

  if (is.na(mois) || !(mois %in% 1:12))
    stop("mois doit \u00EAtre entre 1 et 12")

  if (lang == "en") {
    noms <- c(
      "january", "february", "march", "april", "may", "june",
      "july", "august", "september", "october", "november", "december"
    )
  } else {
    noms <- c(
      "janvier", "f\u00E9vrier", "mars", "avril", "mai", "juin",
      "juillet", "ao\u00FBt", "septembre", "octobre", "novembre", "d\u00E9cembre"
    )
  }

  b <- noms[mois]

  if (majuscule)
    b <- paste0(toupper(substr(b, 1, 1)), substr(b, 2, nchar(b)))

  if (type == "Annee") {
    if (is.na(annee))
      stop("annee doit \u00EAtre num\u00E9rique")
    b <- paste(b, annee)
  }

  b
}


#' Fournit dynamiquement le mois suivant
#'
#' Retourne la formulation du mois situé à `k` périodes
#' après le mois indiqué.
#'
#' @param mois Mois sous forme numérique : 1 à 12.
#' @param annee Année sur 4 chiffres.
#' @param type "Annee" (par défaut) ou "autres".
#' @param majuscule Indicateur logique : TRUE pour une majuscule
#'   initiale, FALSE sinon.
#' @param k Décalage en nombre de mois.
#'   Par défaut : 1 (mois suivant).
#' @param lang Langue de sortie : "fr" ou "en".
#'
#' @return
#' Une chaîne de caractères correspondant au mois suivant.
#'
#' @examples
#' nextMois(3, 2023)
#' nextMois(12, 2023)
#' nextMois(12, 2023, k = 2)
#'
#' @seealso \code{\link{quelMois}}, \code{\link{prevMois}}, \code{\link{nextTrim}}
#'
#' @export
nextMois <- function(mois,
                     annee,
                     type = c("Annee", "autres"),
                     majuscule = FALSE,
                     k = 1,
                     lang = get_serad_language()) {

  type <- match.arg(type)

  mois  <- as.numeric(mois)
  annee <- as.numeric(annee)

  if (is.na(mois) || !(mois %in% 1:12))
    stop("mois doit \u00EAtre entre 1 et 12")

  if (is.na(annee))
    stop("annee doit \u00EAtre num\u00E9rique")

  total <- mois - 1 + k

  new_mois  <- (total %% 12) + 1
  new_annee <- annee + (total %/% 12)

  quelMois(
    new_mois,
    new_annee,
    type = type,
    majuscule = majuscule,
    lang = lang
  )
}


#' Fournit dynamiquement le mois précédent
#'
#' Retourne la formulation du mois situé à `k` périodes
#' avant le mois indiqué.
#'
#' @param mois Mois sous forme numérique : 1 à 12.
#' @param annee Année sur 4 chiffres.
#' @param type "Annee" (par défaut) ou "autres".
#' @param majuscule Indicateur logique : TRUE pour une majuscule
#'   initiale, FALSE sinon.
#' @param k Décalage en nombre de mois.
#'   Par défaut : 1 (mois précédent).
#' @param lang Langue de sortie : "fr" ou "en".
#'
#' @return
#' Une chaîne de caractères correspondant au mois précédent.
#'
#' @examples
#' prevMois(1, 2023)
#' prevMois(12, 2023, k = 2)
#'
#' @seealso \code{\link{prevTrim}}, \code{\link{nextMois}}, \code{\link{quelMois}}
#'
#' @export
prevMois <- function(mois,
                     annee,
                     type = c("Annee", "autres"),
                     majuscule = FALSE,
                     k = 1,
                     lang = get_serad_language()) {

  type <- match.arg(type)

  nextMois(
    mois,
    annee,
    type = type,
    majuscule = majuscule,
    k = -k,
    lang = lang
  )
}


#' Récupère le numéro du mois à partir d'un texte
#'
#' Extrait le numéro du mois (1 à 12) à partir
#' d'une chaîne de caractères contenant un mois
#' en format abrégé ou littéraire.
#'
#' @param mois Chaîne de caractères contenant un mois
#'   (ex. "En juil. 98").
#'
#' @return
#' Un entier entre 1 et 12 si un mois est détecté,
#' 0 sinon. NA si l'entrée est NA.
#'
#' @examples
#' whichMois("En juil. 98")
#'
#' @seealso \code{\link{quelMois}}
#'
#' @export
whichMois <- function(mois) {

  if (is.na(mois))
    return(NA_integer_)

  mois_clean <- tolower(mois)
  mois_clean <- iconv(mois_clean, from = "UTF-8", to = "ASCII//TRANSLIT")

  dict <- list(
    "1"  = c("jan", "january"),
    "2"  = c("fev", "feb", "fevrier", "february"),
    "3"  = c("mar", "march"),
    "4"  = c("avr", "apr", "avril", "april"),
    "5"  = c("mai", "may"),
    "6"  = c("juin", "jun", "june"),
    "7"  = c("juil", "jul", "july"),
    "8"  = c("aou", "aug", "aout", "august"),
    "9"  = c("sep", "sept", "september"),
    "10" = c("oct", "october"),
    "11" = c("nov", "november"),
    "12" = c("dec", "decembre", "december")
  )

  for (i in seq_along(dict)) {
    if (any(sapply(dict[[i]], grepl, mois_clean, fixed = TRUE)))
      return(as.numeric(i))
  }

  return(0L)
}
