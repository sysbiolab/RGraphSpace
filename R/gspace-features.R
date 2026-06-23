
#-------------------------------------------------------------------------------
#' Manipulate node features in a GraphSpace object
#'
#' Utilities for extracting and adding node-associated features stored in the
#' \code{fdata} container of a \code{GraphSpace} object.
#'
#' @param x A \code{GraphSpace} object.
#'
#' @param vars Character vector specifying feature names to extract.
#' If \code{NULL}, all features are returned.
#'
#' @param as_df Logical. If \code{TRUE}, returns a \code{data.frame}.
#' Otherwise returns the original backend representation.
#'
#' @param data A matrix-like or \code{data.frame} object containing node
#' features. Rows must correspond to node identifiers.
#'
#' @return
#' \itemize{
#'   \item \code{gs_fetch_features()} returns a matrix-like object or
#'   \code{data.frame} containing the selected node features.
#'
#'   \item \code{gs_add_features()} returns a modified
#'   \code{GraphSpace} object.
#' }
#'
#' @aliases gs_fetch_features
#' @aliases gs_add_features
#' @name gs_features-utils
NULL

#-------------------------------------------------------------------------------
#' @rdname gs_features-utils
#' @export
gs_fetch_features <- function(x, vars = NULL, as_df = FALSE) {
  
  if (!inherits(x, "GraphSpace")) {
    rlang::abort("'x' must be a GraphSpace object.")
  }
  
  .check_updated_gs(x)
  
  if (!is.null(vars)) {
    .validate_gs_args("allCharacter", "vars", vars)
  }
  .validate_gs_args("singleLogical", "as_df", as_df)
  
  fdata <- gs_fdata(x)
  
  if (!inherits(fdata, "Matrix")) {
    return(NULL)
  }
  
  if (!is.null(vars)) {
    vars <- intersect(vars, colnames(fdata))
    if (length(vars) == 0) {
      return(NULL)
    }
    fdata <- fdata[, vars, drop = FALSE]
  }
  
  if (isTRUE(as_df)) {
    fdata <- as.data.frame(fdata, drop = FALSE)
  }
  
  return(fdata)
}

#' @rdname gs_features-utils
#' @importFrom utils head
#' @export
gs_add_features <- function(x, data) {
  
  if (!inherits(x, "GraphSpace")) {
    rlang::abort("'x' must be a GraphSpace object.")
  }
  
  .check_updated_gs(x)
  
  if (length(dim(data)) != 2) {
    rlang::abort("'data' must be two-dimensional (e.g. matrix-like object).")
  }
  
  if (is.data.frame(data)) {
    data <- tryCatch(
      Matrix::Matrix(as.matrix(data)),
      error = function(e) {
        rlang::abort("'data' could not be coerced to a Matrix object.")
      }
    )
  } else if (!inherits(data, "Matrix")) {
    data <- tryCatch(
      Matrix::Matrix(data),
      error = function(e){
        rlang::abort("'data' could not be coerced to a Matrix object.")
      }
    )
  }
  
  if (is.null(rownames(data))) {
    rlang::abort("'data' must contain rownames matching node identifiers.")
  }
  
  if (is.null(colnames(data))) {
    rlang::abort("'data' must contain feature names as column names.")
  }
  
  node_ids <- gs_nodes(x)$name
  
  # auto-transpose if node IDs are more prevalent in columns than rows
  n_col_hits <- sum(colnames(data) %in% node_ids)
  n_row_hits <- sum(rownames(data) %in% node_ids)
  if (n_col_hits > n_row_hits) {
    rlang::inform(
      "Feature matrix transposed: more node IDs found in columns than rows."
      )
     data <- Matrix::t(data)
  }

  if (!any(rownames(data) %in% node_ids)) {
    rlang::abort("No GraphSpace node identifiers found in 'data'.")
  }

  if (anyDuplicated(rownames(data))) {
    rlang::abort("'data' contains duplicated identifiers.")
  }
  
  # subset and reorder to match node order
  matched_idx <- match(node_ids, rownames(data))
  missing_count <- sum(is.na(matched_idx))
  if (missing_count == 0) {
    
    # safe: matched_idx has no NAs here
    data <- data[matched_idx, , drop = FALSE]
    
  } else {
    
    rlang::warn(sprintf(
      "%d node(s) have no feature data and will be set to NA.", 
      missing_count))
    
    # 'result' is always dense (Matrix::Matrix(NA_real_, ...)); 'data' may
    # be sparse (e.g. dgCMatrix) at this point. Assigning a sparse-derived
    # row subset into a dense row-subset target, as below, has been
    # verified empirically: correct values (including stored structural
    # zeros, not coerced to NA) for both partial node coverage (this
    # branch) and shuffled/reordered row names matched via 'matched_idx'.
    # Not a risk needing further guarding -- Matrix's S4 dispatch handles
    # this correctly.
    result <- Matrix::Matrix(
      NA_real_,
      nrow = length(node_ids),
      ncol = ncol(data),
      dimnames = list(node_ids, colnames(data))
    )
    present <- !is.na(matched_idx)
    result[present, ] <- data[matched_idx[present], , drop = FALSE]
    data <- result
  }
  
  # Load fdata slot
  x@fdata <- data
  x@pars$signal.layer <- TRUE
  
  validObject(x)
  
  return(x)
  
}

