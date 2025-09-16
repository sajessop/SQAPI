#' Build Translate Object for SQUIDLE Export
#'
#' Constructs a `translate` list object to pass to the SQUIDLE API export function.
#'
#' @param target_label_scheme_id Integer. ID of the target label scheme (required).
#' @param vocab_registry_keys Character vector. Optional ordered keys of vocab registries for semantic translation.
#' @param mapping_override Named list of source label IDs -> target label IDs for overriding translations. Names must be source label IDs (as character or numeric) and values must be target label IDs (numeric).
#'
#' @return A list suitable for passing as the `translate` parameter in `export()`.
#' @examples
#' build_translate(
#'   target_label_scheme_id = 123,
#'   vocab_registry_keys = c("squidle_core", "user_labels"),
#'   mapping_override = list("45" = 100, "46" = 101)
#' )
#' @export
translate <- function(target_label_scheme_id,
                            vocab_registry_keys = NULL,
                            mapping_override = NULL) {
  if (missing(target_label_scheme_id) || !is.numeric(target_label_scheme_id)) {
    stop("`target_label_scheme_id` must be input as interger.")
  }

  translate <- list(target_label_scheme_id = as.numeric(target_label_scheme_id))

  if (!is.null(vocab_registry_keys)) {
    if (!is.character(vocab_registry_keys)) {
      stop("`vocab_registry_keys` must be a character vector.")
    }
    translate$vocab_registry_keys <- vocab_registry_keys
  }

  if (!is.null(mapping_override)) {
    if (!is.list(mapping_override)) {
      stop("`mapping_override` must be a named list.")
    }

    # Ensure names are present and values are numeric
    if (is.null(names(mapping_override)) || any(names(mapping_override) == "")) {
      stop("`mapping_override` must have names corresponding to source label IDs.")
    }
    if (!all(vapply(mapping_override, is.numeric, logical(1)))) {
      stop("All values in `mapping_override` must be numeric target label IDs.")
    }

    # Convert names to character to ensure JSON serializes correctly
    names(mapping_override) <- as.character(names(mapping_override))

    translate$mapping_override <- mapping_override
  }

  return(translate)
}
