#' Add pixel dimensions to image metadata
#'
#' Reads images from paths contained in a data frame and adds pixel width
#' and height. Useful for converting Squidle+ normalised point coordinates
#' to pixel coordinates when image dimensions are not present in exports.
#'
#' @param df Data frame containing image paths.
#'
#' @param media_col Character. Name of the column containing full image paths
#' or URLs.
#'
#' @param show_progress Logical. Display progress bar. Defaults to TRUE.
#'
#' @return The input data frame with \code{pixel_width} and
#' \code{pixel_height} columns added.
#'
#' @export
get_image_dimensions <- function(df,
                                 media_col = "point.media.path_best",
                                 show_progress = TRUE) {

  if (!requireNamespace("magick", quietly = TRUE)) {
    stop("Package 'magick' is required.")
  }

  if (!media_col %in% names(df)) {
    stop("media_col must match a column name in df.")
  }

  image_paths <- df[[media_col]]

  # Avoid repeatedly reading the same image
  unique_paths <- unique(image_paths)

  if (show_progress && requireNamespace("progress", quietly = TRUE)) {
    pb <- progress::progress_bar$new(
      format = "  Reading images [:bar] :percent (:current/:total)",
      total = length(unique_paths),
      clear = FALSE,
      width = 60
    )
  } else {
    pb <- NULL
  }

  dimensions <- lapply(unique_paths, function(path) {

    if (!is.null(pb)) pb$tick()

    tryCatch({

      img <- magick::image_read(path)
      info <- magick::image_info(img)

      data.frame(
        media_path = path,
        pixel_width = as.integer(info$width),
        pixel_height = as.integer(info$height)
      )

    }, error = function(e) {

      data.frame(
        media_path = path,
        pixel_width = NA_integer_,
        pixel_height = NA_integer_
      )

    })
  })

  dimensions <- do.call(rbind, dimensions)

  # Join dimensions back onto original data frame
  df <- merge(
    df,
    dimensions,
    by.x = media_col,
    by.y = "media_path",
    all.x = TRUE,
    sort = FALSE
  )

  df
}
