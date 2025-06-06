
#-------------------------------------------------------------------------------
.validate.args <- function(check, name, para) {
    if (check == "numeric_vec") {
        msg <- paste0("'", name, "' should be a numeric vector.")
        if (!is.numeric(para) || !is.vector(para)) stop(msg, call. = FALSE)
    } else if (check == "integer_vec") {
        msg <- paste0("'", name, "' should be an integer vector.")
        if (!is.vector(para) || !.all_integerValues(para)) 
            stop(msg, call. = FALSE)
    } else if (check == "numeric_mtx") {
        msg <- paste0("'", name, "' should be a numeric matrix")
        if (!is.numeric(para) || !is.matrix(para)) stop(msg, call. = FALSE)
    } else if (check == "image_mtx") {
        msg <- paste0("'", name, "' should be a numeric matrix or array.")
        if (!is.raster(para) || !is.matrix(para) || !is.array(para))
            stop(msg, call. = FALSE)
    } else if (check == "allCharacter") {
        msg <- paste0("'", name, "' should be a vector with strings.")
        if (!.all_characterValues(para)) stop(msg, call. = FALSE)
    } else if (check == "allCharacterOrInteger") {
        msg <- paste("'", name, "'should be a vector of strings of integers.")
        if (! (.all_characterValues(para) | .all_integerValues(para) ) ) 
            stop(msg, call. = FALSE)
    } else if (check == "allCharacterOrNa") {
        msg <- paste0("'", name, "' should be a vector with strings.")
        if (!.all_characterValues(para, notNA=FALSE)) stop(msg, call. = FALSE)
    } else if (check == "allBinary") {
        msg <- paste0("'", name, "' should be a vector with binary values.")
        if (!.all_binaryValues(para)) stop(msg, call. = FALSE)
    } else if (check == "allInteger") {
        msg <- paste0("'", name, "' should be a vector with integer values.")
        if (!.all_integerValues(para)) stop(msg, call. = FALSE)
    } else if (check == "singleString") {
        msg <- paste0("'", name, "' should be a single string.")
        if (!.is_singleString(para)) stop(msg, call. = FALSE)
    } else if (check == "singleInteger") {
        msg <- paste0("'", name, "' should be a single integer value.")
        if (!.is_singleInteger(para)) stop(msg, call. = FALSE)
    } else if (check == "singleNumber") {
        msg <- paste0("'", name, "' should be a single numeric value.")
        if (!.is_singleNumber(para)) stop(msg, call. = FALSE)
    } else if (check == "function") {
        msg <- paste0("'", name, "' should be a function.")
        if (!is.function(para)) stop(msg, call. = FALSE)
    } else if (check == "singleLogical") {
        msg <- paste0("'", name, "' should be a single logical value.")
        if (!.is_singleLogical(para)) stop(msg, call. = FALSE)
    }
}

#-------------------------------------------------------------------------------
.validate.colors <- function(check, name, para) {
    if (name == "singleColor") {
        if (!.is_singleColor(para)) {
            msg <- paste0("'", name, "' should be a single color.")
            stop(msg, call. = FALSE)
        }
    } else if (name == "allColors") {
        if (!.is_color(para)) {
            msg <- paste0("'", name, "' should be a vector with colors.")
            stop(msg, call. = FALSE)
        }
    }
}

#-------------------------------------------------------------------------------
# Validate custom plot args
.validate.plot.args <- function(name, para) {
    if (name == "node.labels") {
        if (!is.null(para)) {
            if (!.all_characterValues(para)) {
                msg <- paste0("'", name, "' should be a vector with strings.")
                stop(msg, call. = FALSE)
            }
        }
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
        lg <- abs(para - round(para)) <= para
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
    all(para %in% c(0, 1, NA))
}
.all_integerValues <- function(para, notNA = TRUE) {
    lg <- is.integer(para) || is.numeric(para) || all(is.na(para))
    if (lg) {
        para <- abs(para)
        lg <- all(abs(para - round(para)) <= para, na.rm=TRUE)
    }
    if(lg && notNA) lg <- !any(is.na(para))
    return(lg)
}
.all_numericValues <- function(para, notNA = TRUE) {
    lg <- is.numeric(para) || all(is.na(para))
    if(lg && notNA) lg <- !any(is.na(para))
    return(lg)
}
.all_characterValues <- function(para, notNA = TRUE) {
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
    res <- try(col2rgb(x), silent = TRUE)
    return(!"try-error" %in% class(res))
}
.is_singleColor <- function(para) {
    .is_color(para) && length(para) == 1L && !is.na(para)
}
