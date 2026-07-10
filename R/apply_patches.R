#' Apply a table of manual data corrections ("patches")
#'
#' Applies a set of manual, per-record corrections to a data frame from a
#' separate \emph{patch table}, so that state-specific fixes live in data rather
#' than being hard-coded into cleaning scripts.  A patch table is long-form with
#' one correction per row: the key(s) that identify the record(s) to change, the
#' \code{field} (column) to change, and the new \code{value}.
#'
#' Patch tables typically contain identifiable, unpublished corrections and
#' should be treated like raw data: kept out of version control (see the package
#' \code{.gitignore}) and read from a private path at run time.  The bundled
#' \code{extdata/example_patches.csv} is synthetic and exists only to document
#' the format.
#'
#' @param data A data frame to correct.
#' @param patches A patch table (a data frame with the key column(s) plus
#'   \code{field} and \code{value}) or a path to a \code{.csv} or \code{.rds}
#'   file holding one.
#' @param key Name(s) of the column(s) used to match patch rows against
#'   \code{data}.  Pass a character vector for a \emph{composite} key (for
#'   example \code{c("county", "year", "school_name")}): a patch row then
#'   matches the \code{data} rows that are equal on \emph{all} of those columns.
#'   Prefer stable identifiers over a mutable label such as
#'   \code{school_name_orig}, which is brittle and breaks silently when the
#'   upstream label changes.  The patch table must contain each key column by
#'   name; a single-column key may instead be named \code{key}.  Defaults to
#'   \code{"key"}.
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
#' # A composite key (county + year + school) identifies the record:
#' enroll <- data.frame(
#'   county = c("wake", "wake"),
#'   year = c("2017-18", "2018-19"),
#'   school = c("underwood", "underwood"),
#'   total_enrollment = c(NA, 70L)
#' )
#' epatch <- data.frame(
#'   county = "wake", year = "2017-18", school = "underwood",
#'   field = "total_enrollment", value = "67", stringsAsFactors = FALSE
#' )
#' apply_patches(enroll, epatch, key = c("county", "year", "school"))
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

  # Resolve the patch table's key column(s). A single-column key may be carried
  # under the generic name "key" or under the same name as the match column; a
  # composite key must name every column explicitly.
  if (length(key) == 1L) {
    patch_keys <- if (key %in% names(patches)) key else "key"
  } else {
    patch_keys <- key
  }

  needed <- c(patch_keys, "field", "value")
  missing_cols <- setdiff(needed, names(patches))
  if (length(missing_cols)) {
    stop("patch table is missing column(s): ",
         paste(missing_cols, collapse = ", "),
         ". A patch table must be long-form with column(s) ",
         paste(sprintf("'%s'", patch_keys), collapse = ", "),
         " plus 'field' and 'value'.", call. = FALSE)
  }
  missing_key <- setdiff(key, names(data))
  if (length(missing_key)) {
    stop("match key column(s) not present in `data`: ",
         paste(missing_key, collapse = ", "), ".", call. = FALSE)
  }

  patches$field <- as.character(patches$field)
  unknown_fields <- setdiff(unique(patches$field), names(data))
  if (length(unknown_fields)) {
    stop("patch refers to field(s) not present in `data`: ",
         paste(unknown_fields, collapse = ", "), ".", call. = FALSE)
  }

  # Match each key column in the type of its `data` counterpart, so a
  # CSV-read character key lines up with, e.g., a numeric id column.
  keys <- Map(
    function(pcol, dcol) coerce_like(pcol, dcol),
    patches[patch_keys], data[key]
  )

  unmatched <- integer(0L)
  for (i in seq_len(nrow(patches))) {
    hit <- rep_len(TRUE, nrow(data))
    for (j in seq_along(key)) {
      col <- data[[key[j]]]
      hit <- hit & !is.na(col) & col == keys[[j]][i]
    }
    if (!any(hit)) {
      unmatched <- c(unmatched, i)
      next
    }
    field <- patches$field[i]
    data[[field]][hit] <- coerce_like(patches$value[i], data[[field]])
  }

  if (length(unmatched)) {
    labels <- do.call(paste, c(
      lapply(patch_keys, function(k) as.character(patches[[k]][unmatched])),
      sep = " | "
    ))
    msg <- paste0("patch key(s) matched no row in `data`: ",
                  paste(unique(labels), collapse = "; "),
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
