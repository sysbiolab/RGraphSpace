
#-------------------------------------------------------------------------------
.validate_gs_args <- function(check, name, para, notNA = TRUE) {
  if (check == "numeric_vec") {
    msg <- paste0("'", name, "' should be a numeric vector.")
    if (!is.vector(para) || !.all_numericValues(para)) rlang::abort(msg)
  } else if (check == "character_vec") {
    msg <- paste0("'", name, "' should be a character vector.")
    if (!is.vector(para) || !.all_characterValues(para)) 
      rlang::abort(msg)
  } else if (check == "integer_vec") {
    msg <- paste0("'", name, "' should be an integer vector.")
    if (!is.vector(para) || !.all_integerValues(para, notNA)) 
      rlang::abort(msg)
  } else if (check == "numeric_mtx") {
    msg <- paste0("'", name, "' should be a numeric matrix")
    if (!is.numeric(para) || !is.matrix(para)) rlang::abort(msg)
  } else if (check == "image_mtx") {
    msg1 <- paste0("Invalid '", name, "' input. Expected a raster object\n")
    msg2 <- c("or a numeric matrix with values in the range [0, 1]")
    if (!is.raster(para)){
      if (!is.matrix(para) || !is.numeric(para)) {
        rlang::abort(paste0(msg1, msg2))
      }
      rg <- range(para, na.rm = TRUE)
      if (rg[1] < 0 || rg[2] > 1) {
        rlang::abort(paste0(msg1, msg2))
      }
    }
  } else if (check == "allCharacter") {
    msg <- paste0("'", name, "' should be a vector of strings.")
    if (!.all_characterValues(para)) rlang::abort(msg)
  } else if (check == "allCharacterOrInteger") {
    msg <- paste0("'", name, " 'should be a vector of strings of integers.")
    if (! (.all_characterValues(para) | .all_integerValues(para) ) ) 
      rlang::abort(msg)
  } else if (check == "allCharacterOrNa") {
    msg <- paste0("'", name, "' should be a vector of strings.")
    if (!.all_characterValues(para, notNA=FALSE)) rlang::abort(msg)
  } else if (check == "allBinary") {
    msg <- paste0("'", name, "' should be a vector of binary values.")
    if (!.all_binaryValues(para)) rlang::abort(msg)
  } else if (check == "allInteger") {
    msg <- paste0("'", name, "' should be a vector of integer values.")
    if (!.all_integerValues(para)) rlang::abort(msg)
  } else if (check == "singleString") {
    msg <- paste0("'", name, "' should be a single string.")
    if (!.is_singleString(para)) rlang::abort(msg)
  } else if (check == "singleInteger") {
    msg <- paste0("'", name, "' should be a single integer value.")
    if (!.is_singleInteger(para)) rlang::abort(msg)
  } else if (check == "singleNumber") {
    msg <- paste0("'", name, "' should be a single numeric value.")
    if (!.is_singleNumber(para)) rlang::abort(msg)
  } else if (check == "singlePositiveNumber") {
    msg <- paste0("'", name, "' should be a single numeric value >=0.")
    if (!.is_singleNumber(para) || para<0) rlang::abort(msg)
  } else if (check == "function") {
    msg <- paste0("'", name, "' should be a function.")
    if (!is.function(para)) rlang::abort(msg)
  } else if (check == "singleLogical") {
    msg <- paste0("'", name, "' should be a single logical value.")
    if (!.is_singleLogical(para)) rlang::abort(msg)
  } else {
    rlang::abort(
      c(paste0("Unrecognised `check` value: '", check, "'."),
        "i" = "This is an internal check and shouldn't be reachable.",
        "i" = "If you see this, please report it as a bug."
      )
    )
  }
}

#-------------------------------------------------------------------------------
.validate_gs_colors <- function(check, name, para) {
  if (check == "singleColor") {
    if (!.is_singleColor(para)) {
      msg <- paste0("'", name, "' should be a single color.")
      rlang::abort(msg)
    }
  } else if (check == "allColors") {
    if (!.is_color(para)) {
      msg <- paste0("'", name, "' should be a vector with colors.")
      rlang::abort(msg)
    }
  } else {
    rlang::abort(
      c(paste0("Unrecognised `check` value: '", check, "'."),
        "i" = "This is an internal check and shouldn't be reachable.",
        "i" = "If you see this, please report it as a bug."
      )
    )
  }
}

#-------------------------------------------------------------------
.is_singleNumber <- function(para) {
  (is.integer(para) || is.numeric(para)) &&
    length(para) == 1L && !is.na(para)
}
.is_singleInteger <- function(para) {
  lg <- (is.integer(para) || is.numeric(para)) &&
    length(para) == 1L && !is.na(para)
  if (lg) {
    para <- abs(para)
    lg <- abs(para - round(para)) <= .Machine$double.eps
  }
  return(lg)
}
.is_singleString <- function(para) {
  is.character(para) && length(para) == 1L && !is.na(para)
}
.is_singleLogical <- function(para) {
  is.logical(para) && length(para) == 1L && !is.na(para)
}
.all_binaryValues <- function(para) {
  if (length(para) == 0L) return(FALSE)
  all(para %in% c(0, 1, NA))
}
.all_integerValues <- function(para, notNA = TRUE) {
  if (length(para) == 0L) return(FALSE)
  if (is.character(para) || is.list(para)) return(FALSE)
  lg <- is.integer(para) || is.numeric(para) || all(is.na(para))
  if (lg) {
    para <- abs(para)
    lg <- all( abs(para - round(para)) <= .Machine$double.eps, na.rm=TRUE)
  }
  if(lg && notNA) lg <- !any(is.na(para))
  return(lg)
}
.all_numericValues <- function(para, notNA = TRUE) {
  if (length(para) == 0L) return(FALSE)
  lg <- is.numeric(para) || all(is.na(para))
  if(lg && notNA) lg <- !any(is.na(para))
  return(lg)
}
.all_characterValues <- function(para, notNA = TRUE) {
  if (length(para) == 0L) return(FALSE)
  lg <- is.character(para) || all(is.na(para))
  if(lg && notNA) lg <- !any(is.na(para))
  return(lg)
}
.is_numericVector <- function(para){
  is.vector(para) && .all_numericValues(para)
}
.is_integerVector <- function(para){
  is.vector(para) && .all_integerValues(para)
}
.is_characterVector <- function(para){
  is.vector(para) && .all_characterValues(para)
}
.is_color <- function(x) {
  # if (anyNA(x)) return(FALSE)
  tryCatch({ col2rgb(x); TRUE }, error = function(e) FALSE)
}
.is_singleColor <- function(para) {
  .is_color(para) && length(para) == 1L && !is.na(para)
}
