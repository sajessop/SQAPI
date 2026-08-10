#' Calculate organism density per image using a laser-derived scale factor
#'
#' Counts every non-laser annotation label per image and divides by the image
#' footprint area to calculate organism density. Selected metadata attributes
#' from the original annotations dataframe are retained.
#'
#' @param annotations A data frame of Squidle+ annotations, one row per
#' annotation point.
#' @param scale_df Output of \code{\link{calc_laser_scale}} with one row per
#' image and \code{point.media.key} and \code{image_area_m2}.
#' @param metadata_cols Character vector of columns from \code{annotations}
#' to retain in the output. Metadata are retained at the image x label level.
#' @param laser_label_pattern Character. Regex used to exclude laser
#' annotations.
#' @param label_col Character. Column containing annotation labels.
#'
#' @return A data frame with one row per image x annotation class containing
#' density metrics and selected metadata.
#'
#' @export
calc_density <- function(annotations,
                         scale_df,
                         metadata_cols = NULL,
                         laser_label_pattern = "laser",
                         label_col = "label.lineage_names") {

  # Remove laser points
  filtered_annotations <- annotations %>%
    dplyr::filter(
      !stringr::str_detect(
        .data[[label_col]],
        stringr::regex(laser_label_pattern, ignore_case = TRUE)
      )
    )

  # Calculate counts
  density <- filtered_annotations %>%
    dplyr::count(
      point.media.key,
      label = .data[[label_col]],
      name = "n_points"
    ) %>%
    dplyr::left_join(
      scale_df %>%
        dplyr::select(
          point.media.key,
          image_area_m2
        ),
      by = "point.media.key"
    ) %>%
    dplyr::mutate(
      density_per_m2 = n_points / image_area_m2
    )


  # Add selected metadata
  if (!is.null(metadata_cols)) {

    missing_cols <- setdiff(metadata_cols, names(annotations))

    if (length(missing_cols) > 0) {
      stop(
        "Metadata columns not found: ",
        paste(missing_cols, collapse = ", ")
      )
    }

    metadata <- filtered_annotations %>%
      dplyr::select(
        point.media.key,
        dplyr::all_of(label_col),
        dplyr::all_of(metadata_cols)
      ) %>%
      dplyr::group_by(
        point.media.key,
        .data[[label_col]]
      ) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup()

    density <- density %>%
      dplyr::left_join(
        metadata,
        by = c(
          "point.media.key",
          setNames(label_col, "label")
        )
      )
  }

  density
}
