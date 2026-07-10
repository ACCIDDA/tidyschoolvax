#' Apply a table of manual data corrections ("patches")
#'
#' Applies a set of manual, per-record corrections to a data frame from a
#' separate \emph{patch table}, so that state-specific fixes live in data rather
#' than being hard-coded into cleaning scripts.  A patch table is long-form with
#' one correction per row: a key that identifies the record(s) to change, the
#' \code{field} (column) to change, and the new \code{value}.
#'
#' Patch tables typically contain identifiable, unpublished corrections and
#' should be treated like raw data: kept out of version control (see the package
#' \code{.gitignore}) and read from a private path at run time.  The bundled
#' \code{extdata/example_patches.csv} is synthetic and exists only to document
#' the format.
#'
#' @param data A data frame to correct.
#' @param patches A patch table (a data frame with columns \code{key},
#'   \code{field}, \code{value}) or a path to a \code{.csv} or \code{.rds} file
#'   holding one.  When a data frame is supplied, the key column may be named
#'   \code{key} or may share the name passed to \code{key}.
#' @param key Name of the column used to match patch rows against \code{data}.
#'   Prefer a stable identifier (for example a school id) over a mutable label
#'   such as \code{school_name_orig}: matching on a name is brittle and breaks
#'   silently when the upstream label changes.  Defaults to \code{"key"}.
#' @param strict If \code{TRUE} (the default), a patch whose key matches no row
#'   in \code{data} is an error, so a stale correction cannot silently do
#'   nothing.  If \code{FALSE}, unmatched patches emit a warning and are skipped.
#'
#' @return \code{data} with the patches applied.  Patched values are coerced to
#'   the class of the target column.
#'
#' @examples
#' df <- data.frame(
#'   school_id = c("A1", "A2", "A3"),
#'   addr_clean = c("1 main st", NA, "3 oak ave"),
#'   lat = c(35.1, NA, 35.3),
#'   stringsAsFactors = FALSE
#' )
#' patches <- data.frame(
#'   key = c("A2", "A2"),
#'   field = c("addr_clean", "lat"),
#'   value = c("2 elm st", "35.2"),
#'   stringsAsFactors = FALSE
#' )
#' apply_patches(df, patches, key = "school_id")
#'
#' @export
apply_patches <- function(data, patches, key = "key", strict = TRUE) {
  if (is.character(patches) && length(patches) == 1L) {
    patches <- read_patch_file(patches)
  }
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }
  if (!is.data.frame(patches)) {
    stop("`patches` must be a data frame or a path to a .csv/.rds file.",
         call. = FALSE)
  }

  # The patch table may carry its key column under the generic name "key" or
  # under the same name as the matching column in `data`.
  patch_key <- if (key %in% names(patches)) key else "key"
  needed <- c(patch_key, "field", "value")
  missing_cols <- setdiff(needed, names(patches))
  if (length(missing_cols)) {
    stop("patch table is missing column(s): ",
         paste(missing_cols, collapse = ", "),
         ". A patch table must be long-form with columns '", patch_key,
         "', 'field' and 'value'.", call. = FALSE)
  }
  if (!key %in% names(data)) {
    stop("match key '", key, "' is not a column in `data`.", call. = FALSE)
  }

  patches$field <- as.character(patches$field)
  unknown_fields <- setdiff(unique(patches$field), names(data))
  if (length(unknown_fields)) {
    stop("patch refers to field(s) not present in `data`: ",
         paste(unknown_fields, collapse = ", "), ".", call. = FALSE)
  }

  # Match in the key column's own type so that, e.g., a CSV-read character key
  # lines up with a numeric id column.
  keys <- coerce_like(patches[[patch_key]], data[[key]])
  unmatched <- character(0L)

  for (i in seq_len(nrow(patches))) {
    field <- patches$field[i]
    hit <- !is.na(data[[key]]) & data[[key]] == keys[i]
    if (!any(hit)) {
      unmatched <- c(unmatched, as.character(patches[[patch_key]][i]))
      next
    }
    data[[field]][hit] <- coerce_like(patches$value[i], data[[field]])
  }

  if (length(unmatched)) {
    msg <- paste0("patch key(s) matched no row in `data`: ",
                  paste(unique(unmatched), collapse = ", "),
                  " (stale patch?).")
    if (strict) stop(msg, call. = FALSE) else warning(msg, call. = FALSE)
  }

  data
}

#' Coerce a character patch value to the class of its target column
#'
#' @param value A length-1 or vector patch value (usually character).
#' @param target The destination column, whose type dictates the coercion.
#' @return \code{value} coerced to \code{target}'s type.
#' @noRd
coerce_like <- function(value, target) {
  value <- as.character(value)
  if (inherits(target, "Date")) return(as.Date(value))
  if (is.integer(target)) return(as.integer(value))
  if (is.numeric(target)) return(as.numeric(value))
  if (is.logical(target)) return(as.logical(value))
  value
}

#' Read a patch table from disk
#'
#' @param path Path to a \code{.csv} or \code{.rds} patch file.
#' @return A data frame patch table.
#' @noRd
read_patch_file <- function(path) {
  if (!file.exists(path)) {
    stop("patch file not found: ", path, call. = FALSE)
  }
  ext <- tolower(tools::file_ext(path))
  switch(
    ext,
    csv = utils::read.csv(path, stringsAsFactors = FALSE, colClasses = "character"),
    rds = as.data.frame(readRDS(path)),
    stop("unsupported patch file type '.", ext,
         "'; use .csv or .rds.", call. = FALSE)
  )
}
