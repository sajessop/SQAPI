#' Calculate percent cover from random-point annotations
#'
#' Calculates point-intercept percent cover per image, with the ability to
#' exclude certain label classes (e.g. \code{"Unscoreable"}, \code{"Shadow"},
#' \code{"Laser"}) and/or points carrying certain tags (e.g. a point
#' labelled \code{"Coral"} but tagged \code{"obscured"}) BEFORE the
#' denominator is calculated - so percent cover is of valid scoreable
#' points, not diluted by points that were never real substrate.
#'
#' @param annotations A data frame of Squidle+ annotations, one row per
#'   annotation point.
#' @param exclude_classes Character vector of label values to drop before
#'   the per-image total is calculated. Matching is a case-insensitive
#'   SEARCH (substring match) by default.
#' @param exclude_regex Logical. If TRUE, elements of \code{exclude_classes}
#'   are treated as regex patterns.
#' @param tag_exclude_relabel Character vector of replacement labels for
#'   excluded tags. Must be the same length and order as
#'   \code{exclude_tags}. For example:
#'   \code{exclude_tags = c("Unattached","Dead")}
#'   and
#'   \code{tag_exclude_relabel = c("Unattached","Dead")}.
#' @param tag_col Column holding each point's tags.
#' @param tag_delim Delimiter used if \code{tag_col} is a string column.
#' @param recode_map Optional named character vector to merge/relabel classes
#'   before calculating cover.
#' @param image_id_col Character. Column identifying the image each point
#'   belongs to.
#' @param label_col Character. Column holding the annotation label.
#' @param wide Logical. If TRUE, returns an image x label matrix of percent
#'   cover suitable for community analyses. Metadata are retained in long
#'   format only.
#'
#' @return A list with two elements:
#'   \code{cover} (percent cover per image grouped by
#'   \code{image_id_col} and \code{label_col}, with original image-level
#'   annotation metadata retained in long format) and
#'   \code{qc} (per-image point totals, exclusions, and percent excluded).
#'
#'#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @importFrom stringr str_detect
#'
#' @export
calc_percent_cover <- function(annotations,
                               exclude_classes = NULL,
                               exclude_regex   = FALSE,
                               exclude_tags    = NULL,
                               tag_col         = "tag_names",
                               tag_delim       = "|",
                               tag_exclude_relabel = NULL,
                               recode_map      = NULL,
                               image_id_col    = "point.media.key",
                               label_col       = "label.lineage_names",
                               wide            = FALSE) {

  # retain all original annotation attributes
  df <- annotations %>%
    dplyr::mutate(
      image_id = .data[[image_id_col]],
      label    = .data[[label_col]],
      tags     = if (!is.null(tag_col)) .data[[tag_col]] else NA_character_
    )

  if (!is.null(recode_map)) {
    df <- df %>%
      dplyr::mutate(label = dplyr::recode(label, !!!recode_map))
  }

  n_total_raw <- df %>%
    dplyr::count(image_id, name = "n_points_total")

  df <- df %>%
    dplyr::mutate(
      excluded_by_tag = .point_has_excluded_tag(
        tags,
        exclude_tags,
        tag_delim
      )
    )

  n_excluded_by_tag <- df %>%
    dplyr::filter(excluded_by_tag) %>%
    dplyr::count(
      image_id,
      name = "n_excluded_by_tag"
    )

  # Optional QC relabelling only - do not modify label used for cover calculation
  if (!is.null(tag_exclude_relabel)) {

    df <- df %>%
      dplyr::mutate(
        tag_exclusion_label = .get_tag_relabel(
          tags,
          exclude_tags,
          tag_exclude_relabel,
          tag_delim
        )
      )

  } else {

    df <- df %>%
      dplyr::mutate(
        tag_exclusion_label = NA_character_
      )

  }

  if (!is.null(exclude_classes)) {

    if (exclude_regex) {
      pattern <- paste(exclude_classes, collapse = "|")
    } else {
      pattern <- paste(
        stringr::str_escape(exclude_classes),
        collapse = "|"
      )
    }

    df <- df %>%
      dplyr::mutate(
        excluded_by_label = stringr::str_detect(
          label,
          stringr::regex(pattern, ignore_case = TRUE)
        )
      )

  } else {

    df <- df %>%
      dplyr::mutate(excluded_by_label = FALSE)

  }

  df <- df %>%
    dplyr::mutate(
      excluded = excluded_by_tag | excluded_by_label
    )


  df_valid <- df %>%
    dplyr::filter(
      !excluded,
      !is.na(label),
      label != ""
    )

  n_valid <- df_valid %>%
    dplyr::count(image_id, name = "n_points_valid")


  qc <- n_total_raw %>%
    dplyr::left_join(n_valid, by = "image_id") %>%
    dplyr::left_join(n_excluded_by_tag, by = "image_id") %>%
    dplyr::mutate(
      n_points_valid = tidyr::replace_na(
        n_points_valid,
        0
      ),
      n_excluded_by_tag = tidyr::replace_na(
        n_excluded_by_tag,
        0
      ),
      n_excluded_total = n_points_total - n_points_valid,
      pct_excluded = 100 * n_excluded_total / n_points_total
    )


  low_valid <- qc %>%
    dplyr::filter(
      n_points_valid < 0.5 * n_points_total
    )

  if (nrow(low_valid) > 0) {
    warning(sprintf(
      "%d image(s) lost >50%% of points to exclusion:\n%s",
      nrow(low_valid),
      paste(
        utils::capture.output(print(low_valid)),
        collapse = "\n"
      )
    ))
  }


  # calculate cover and restore image-level metadata
  # Create image-level metadata table
  image_metadata <- annotations %>%
    dplyr::group_by(.data[[image_id_col]]) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      -dplyr::all_of(label_col)
    ) %>%
    dplyr::rename(image_id = !!image_id_col)

  # Calculate cover
  cover_long <- df_valid %>%
    dplyr::filter(
      !is.na(label),
      label != ""
    ) %>%
    dplyr::count(
      image_id,
      label,
      name = "n_points"
    ) %>%
    dplyr::left_join(
      n_valid,
      by = "image_id"
    ) %>%
    dplyr::mutate(
      percent_cover = 100 * n_points / n_points_valid
    ) %>%
    dplyr::left_join(
      image_metadata,
      by = "image_id"
    )

  if (!wide) {

    cover_out <- cover_long %>%
      dplyr::rename(
        !!image_id_col := image_id,
        !!label_col := label
      )

    qc_out <- qc %>%
      dplyr::rename(
        !!image_id_col := image_id
      )

    return(
      list(
        cover = cover_out,
        qc = qc_out
      )
    )
  }


  # community matrix format (with metadata)
  cover_wide <- cover_long %>%
    dplyr::select(
      image_id,
      label,
      percent_cover
    ) %>%
    tidyr::pivot_wider(
      names_from = label,
      values_from = percent_cover,
      values_fill = 0
    ) %>%
    dplyr::left_join(
      image_metadata,
      by = "image_id"
    ) %>%
    dplyr::relocate(
      dplyr::all_of(names(image_metadata)),
      .before = 1
    ) %>%
    dplyr::rename(
      !!image_id_col := image_id
    )


  qc_out <- qc %>%
    dplyr::rename(
      !!image_id_col := image_id
    )

  list(
    cover = cover_wide,
    qc = qc_out
  )
}


#' Does a point's tag names contain any excluded tag?
#'
#' @keywords internal
#' @noRd
.point_has_excluded_tag <- function(tag_names, exclude_tags, delim) {

  if (is.null(exclude_tags)) {
    return(rep(FALSE, length(tag_names)))
  }

  exclude_tags_lc <- tolower(exclude_tags)

  if (is.list(tag_names)) {

    vapply(
      tag_names,
      function(t) {

        if (length(t) == 0 || all(is.na(t))) {
          return(FALSE)
        }

        any(
          tolower(trimws(t)) %in% exclude_tags_lc
        )
      },
      logical(1)
    )

  } else {

    vapply(
      tag_names,
      function(x) {

        if (is.na(x) || x == "") {
          return(FALSE)
        }

        tags_i <- trimws(
          strsplit(
            as.character(x),
            delim,
            fixed = TRUE
          )[[1]]
        )

        any(
          tolower(tags_i) %in% exclude_tags_lc
        )
      },
      logical(1)
    )
  }
}


#' Get relabel value for excluded tags
#'
#' @keywords internal
#' @noRd
.get_tag_relabel <- function(tag_names,
                             exclude_tags,
                             tag_exclude_relabel,
                             delim) {

  if (is.null(exclude_tags) ||
      is.null(tag_exclude_relabel)) {
    return(rep(NA_character_, length(tag_names)))
  }

  if (length(tag_exclude_relabel) != length(exclude_tags)) {
    stop(
      "`tag_exclude_relabel` must have the same length as `exclude_tags`."
    )
  }


  vapply(
    tag_names,
    function(x) {

      if (is.na(x) || x == "") {
        return(NA_character_)
      }

      tags <- trimws(
        strsplit(
          as.character(x),
          delim,
          fixed = TRUE
        )[[1]]
      )

      matched <- match(
        tolower(tags),
        tolower(exclude_tags)
      )

      if (all(is.na(matched))) {
        return(NA_character_)
      }

      tag_exclude_relabel[
        matched[!is.na(matched)][1]
      ]

    },
    character(1)
  )
}
