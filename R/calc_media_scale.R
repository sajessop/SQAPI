#' Calculate a pixel-to-real-world scale factor for images
#'
#' Derives a per-image scale factor (cm per pixel) and the resulting
#' real-world image footprint, from one of three sources:
#'
#' \describe{
#'   \item{\code{"laser"}}{Two laser-dot annotation points per image (a
#'     fixed, known distance apart on the camera rig). scale = known
#'     distance / pixel distance between the dots. Most accurate, since
#'     it's measured directly in each image — use this wherever laser
#'     points are available.}
#'   \item{\code{"manual"}}{A scale you already know per image (e.g. from a
#'     prior calibration, or a fixed rig geometry with no laser dots),
#'     supplied directly as a \code{cm_per_pixel} column.}
#'   \item{\code{"altitude"}}{Camera height above the seafloor
#'     (\code{pose.pose.alt}, assumed in metres) combined with camera
#'     intrinsics, via the pinhole camera model:
#'     \code{pixel_size_m = altitude_m * sensor_width_mm / (focal_length_mm * width_px)}
#'     (provide \code{sensor_width_mm} + \code{focal_length_mm}), or
#'     equivalently
#'     \code{pixel_size_m = 2 * altitude_m * tan(hfov/2) / width_px}
#'     (provide \code{hfov_deg}). Assumes near-nadir imagery over a locally
#'     flat seafloor — least accurate of the three, fall back to this only
#'     where laser points aren't visible and no manual value is known.}
#' }
#'
#' All three methods return the same column structure so they can be mixed
#' (e.g. laser scale where available, altitude scale filled in for the
#' rest of the survey) and used interchangeably downstream.
#'
#' @param data For \code{method = "laser"}: a data frame of Squidle+
#'   annotations, one row per annotation point, including the two
#'   laser-dot points per image. For \code{method = "manual"} or
#'   \code{"altitude"}: a data frame with one row per image (media item).
#' @param method One of \code{"laser"}, \code{"manual"}, \code{"altitude"}.
#' @param media_col Column identifying the image (default
#'   \code{"point.media.key"}, matching Squidle+ annotation exports).
#' @param width_col,height_col Columns holding image pixel width/height.
#' @param laser_label_pattern (laser) Regex matched against \code{label_col}
#'   (case-insensitive) to identify laser-dot annotations.
#' @param known_distance_cm (laser) The fixed real-world separation between
#'   the two laser dots on your rig, in centimetres.
#' @param x_col,y_col (laser) Columns holding point x/y coordinates.
#' @param label_col (laser) Column holding the annotation label used to
#'   identify laser points.
#' @param coords_normalised (laser) \code{TRUE} if x/y coordinates are
#'   normalised 0-1 coordinates and need converting to pixels.
#' @param scale_col (manual) Column already containing scale in cm per
#'   pixel.
#' @param altitude_col (altitude) Column with camera altitude above the
#'   seafloor, e.g. \code{"pose.pose.alt"}, in metres.
#' @param sensor_width_mm,focal_length_mm (altitude, pinhole model) Camera
#'   sensor width and focal length in mm. Provide these OR \code{hfov_deg}.
#' @param hfov_deg (altitude, FOV model) Horizontal field of view of the
#'   camera in degrees. Provide this OR \code{sensor_width_mm} +
#'   \code{focal_length_mm}.
#'
#' @return A data frame with one row per \code{point.media.key} (or
#'   whatever \code{media_col} is set to) and columns: \code{pixel_distance}
#'   (laser-measured pixel separation; \code{NA} for manual/altitude),
#'   \code{width_px}, \code{height_px}, \code{cm_per_pixel},
#'   \code{pixels_per_cm}, \code{image_width_cm}, \code{image_height_cm},
#'   \code{image_area_m2}, and \code{scale_method}.
#'
#' @export
calc_image_scale <- function(data,
                             method = c("laser", "manual", "altitude"),
                             media_col = "point.media.key",
                             width_col = "pixel_width",
                             height_col = "pixel_height",
                             # laser args
                             laser_label_pattern = "laser",
                             known_distance_cm = 10,
                             x_col = "point.x",
                             y_col = "point.y",
                             label_col = "label.name",
                             coords_normalised = TRUE,
                             # manual args
                             scale_col = "cm_per_pixel",
                             # altitude args
                             altitude_col = "pose.pose.alt",
                             sensor_width_mm = NULL,
                             focal_length_mm = NULL,
                             hfov_deg = NULL) {
  method <- match.arg(method)

  switch(
    method,
    laser = calc_laser_scale(
      annotations = data,
      laser_label_pattern = laser_label_pattern,
      known_distance_cm = known_distance_cm,
      x_col = x_col, y_col = y_col,
      width_col = width_col, height_col = height_col,
      label_col = label_col,
      coords_normalised = coords_normalised,
      media_col = media_col
    ),
    manual = calc_manual_scale(
      media = data,
      media_col = media_col,
      width_col = width_col, height_col = height_col,
      scale_col = scale_col
    ),
    altitude = calc_altitude_scale(
      media = data,
      media_col = media_col,
      width_col = width_col, height_col = height_col,
      altitude_col = altitude_col,
      sensor_width_mm = sensor_width_mm,
      focal_length_mm = focal_length_mm,
      hfov_deg = hfov_deg
    )
  )
}

#' Calculate a pixel-to-real-world scale factor from paired laser points
#'
#' Uses two laser-dot annotation points per image (a fixed, known distance
#' apart on the camera rig) to derive a scale factor for that image, and
#' from it the real-world footprint area of the image. Prefer
#' [calc_image_scale()] for a unified interface across laser/manual/
#' altitude methods — this is kept as a standalone entry point since it's
#' also called directly by [calc_image_scale(method = "laser")].
#'
#' @inheritParams calc_image_scale
#' @param annotations A data frame of Squidle+ annotations, one row per
#'   annotation point, including the two laser-dot points per image.
#'
#' @return See [calc_image_scale()]. \code{scale_method} is always
#'   \code{"laser"}.
#'
#' @export
calc_laser_scale <- function(annotations,
                             laser_label_pattern = "laser",
                             known_distance_cm = 10,
                             x_col = "point.x",
                             y_col = "point.y",
                             width_col = "pixel_width",
                             height_col = "pixel_height",
                             label_col = "label.name",
                             coords_normalised = TRUE,
                             media_col = "point.media.key") {
  required_cols <- c(media_col, x_col, y_col, width_col, height_col, label_col)
  missing_cols <- setdiff(required_cols, names(annotations))

  if (length(missing_cols) > 0) {
    stop("Missing required column(s): ", paste(missing_cols, collapse = ", "))
  }

  laser_pts <- annotations |>
    dplyr::filter(
      stringr::str_detect(
        .data[[label_col]],
        stringr::regex(laser_label_pattern, ignore_case = TRUE)
      )
    ) |>
    dplyr::transmute(
      point.media.key = .data[[media_col]],
      x = .data[[x_col]],
      y = .data[[y_col]],
      width_px = .data[[width_col]],
      height_px = .data[[height_col]]
    )

  if (nrow(laser_pts) == 0) {
    stop(
      "No annotations matched laser_label_pattern = '", laser_label_pattern,
      "'. Check label_col and laser_label_pattern."
    )
  }

  if (coords_normalised) {
    laser_pts <- laser_pts |>
      dplyr::mutate(x_px = x * width_px, y_px = y * height_px)
  } else {
    laser_pts <- laser_pts |>
      dplyr::mutate(x_px = x, y_px = y)
  }

  n_per_image <- laser_pts |>
    dplyr::count(point.media.key, name = "n_laser_pts")

  bad_images <- n_per_image |>
    dplyr::filter(n_laser_pts != 2)

  if (nrow(bad_images) > 0) {
    warning(sprintf(
      "%d image(s) have != 2 laser points and will be excluded.",
      nrow(bad_images)
    ))
  }

  laser_pts |>
    dplyr::semi_join(
      n_per_image |> dplyr::filter(n_laser_pts == 2),
      by = "point.media.key"
    ) |>
    dplyr::group_by(point.media.key) |>
    dplyr::summarise(
      pixel_distance = sqrt(diff(x_px)^2 + diff(y_px)^2),
      width_px = dplyr::first(width_px),
      height_px = dplyr::first(height_px),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      cm_per_pixel = known_distance_cm / pixel_distance,
      pixels_per_cm = pixel_distance / known_distance_cm,
      image_width_cm = width_px * cm_per_pixel,
      image_height_cm = height_px * cm_per_pixel,
      image_area_m2 = (image_width_cm / 100) * (image_height_cm / 100),
      scale_method = "laser"
    )
}

#' Calculate scale from a manually supplied cm-per-pixel value
#'
#' For images where scale is already known — e.g. a fixed-geometry rig
#' with a pre-measured footprint, or a value pulled from a prior
#' calibration — rather than measured from laser points or estimated from
#' altitude. Called by [calc_image_scale(method = "manual")].
#'
#' @inheritParams calc_image_scale
#' @param media A data frame with one row per image, containing
#'   `media_col`, `width_col`, `height_col`, and `scale_col`.
#'
#' @return See [calc_image_scale()]. \code{pixel_distance} is \code{NA} and
#'   \code{scale_method} is \code{"manual"}.
#'
#' @export
calc_manual_scale <- function(media,
                              media_col = "point.media.key",
                              width_col = "pixel_width",
                              height_col = "pixel_height",
                              scale_col = "cm_per_pixel") {
  required_cols <- c(media_col, width_col, height_col, scale_col)
  missing_cols <- setdiff(required_cols, names(media))

  if (length(missing_cols) > 0) {
    stop("Missing required column(s): ", paste(missing_cols, collapse = ", "))
  }

  media |>
    dplyr::transmute(
      point.media.key = .data[[media_col]],
      pixel_distance = NA_real_,
      width_px = .data[[width_col]],
      height_px = .data[[height_col]],
      cm_per_pixel = .data[[scale_col]],
      pixels_per_cm = 1 / cm_per_pixel,
      image_width_cm = width_px * cm_per_pixel,
      image_height_cm = height_px * cm_per_pixel,
      image_area_m2 = (image_width_cm / 100) * (image_height_cm / 100),
      scale_method = "manual"
    )
}

#' Calculate scale from camera altitude and intrinsics (pinhole model)
#'
#' Fallback for images without laser points and without a known manual
#' scale: derives cm-per-pixel from camera altitude above the seafloor
#' (`pose.pose.alt`) plus either sensor width + focal length, or horizontal
#' field of view. Assumes near-nadir imagery over a locally flat seafloor
#' — least accurate of the three methods, prefer laser or manual scale
#' where available. Called by [calc_image_scale(method = "altitude")].
#'
#' @inheritParams calc_image_scale
#' @param media A data frame with one row per image, containing
#'   `media_col`, `width_col`, `height_col`, and `altitude_col`.
#'
#' @return See [calc_image_scale()]. \code{pixel_distance} is \code{NA} and
#'   \code{scale_method} is \code{"altitude"}.
#'
#' @export
calc_altitude_scale <- function(media,
                                media_col = "point.media.key",
                                width_col = "pixel_width",
                                height_col = "pixel_height",
                                altitude_col = "pose.pose.alt",
                                sensor_width_mm = NULL,
                                focal_length_mm = NULL,
                                hfov_deg = NULL) {
  required_cols <- c(media_col, width_col, height_col, altitude_col)
  missing_cols <- setdiff(required_cols, names(media))

  if (length(missing_cols) > 0) {
    stop("Missing required column(s): ", paste(missing_cols, collapse = ", "))
  }

  has_hfov <- !is.null(hfov_deg)
  has_focal <- !is.null(sensor_width_mm) && !is.null(focal_length_mm)

  if (!has_hfov && !has_focal) {
    stop(
      "calc_altitude_scale: provide either hfov_deg, or both sensor_width_mm ",
      "and focal_length_mm."
    )
  }

  out <- media |>
    dplyr::transmute(
      point.media.key = .data[[media_col]],
      pixel_distance = NA_real_,
      width_px = .data[[width_col]],
      height_px = .data[[height_col]],
      altitude_m = .data[[altitude_col]]
    )

  pixel_size_m <- if (has_hfov) {
    2 * out$altitude_m * tan((hfov_deg * pi / 180) / 2) / out$width_px
  } else {
    out$altitude_m * sensor_width_mm / (focal_length_mm * out$width_px)
  }

  out |>
    dplyr::mutate(
      cm_per_pixel = pixel_size_m * 100,
      pixels_per_cm = 1 / cm_per_pixel,
      image_width_cm = width_px * cm_per_pixel,
      image_height_cm = height_px * cm_per_pixel,
      image_area_m2 = (image_width_cm / 100) * (image_height_cm / 100),
      scale_method = "altitude"
    ) |>
    dplyr::select(-altitude_m)
}

#' Convert calc_image_scale() output into the format expected by the
#' FRAGSTATS patch-metric functions
#'
#' [calc_patch_metrics()] / [class_metrics()] / [landscape_metrics()] /
#' [calc_fragstats()] expect a `scale` data frame with columns `media_id`
#' (matching whatever `media_col` was used in [build_patch_sf()]) and
#' `scale` in real-world units *per pixel* (e.g. m/px), whereas
#' [calc_image_scale()] reports `cm_per_pixel` keyed on `point.media.key`.
#' This bridges the two.
#'
#' @param image_scale Output of [calc_image_scale()] (or
#'   [calc_laser_scale()] / [calc_manual_scale()] / [calc_altitude_scale()]
#'   directly).
#' @param media_id_col Name to use for the id column in the output
#'   (default `"media_id"`, matching [build_patch_sf()]'s default
#'   `media_col = "point.media.id"` after renaming — adjust if your
#'   annotation export's media identifier differs from
#'   `point.media.key`).
#' @param unit Real-world unit to report scale in: `"m"` (default, m/px)
#'   or `"cm"` (cm/px, matching `cm_per_pixel` directly).
#' @return A data frame with columns `media_id` and `scale`, ready to pass
#'   as the `scale` argument to the FRAGSTATS metric functions.
#' @export
to_fragstats_scale <- function(image_scale, media_id_col = "media_id", unit = c("m", "cm")) {
  unit <- match.arg(unit)
  factor <- if (unit == "m") 0.01 else 1 # cm_per_pixel -> m_per_pixel = *0.01

  out <- data.frame(
    media_id = image_scale$point.media.key,
    scale = image_scale$cm_per_pixel * factor
  )
  names(out)[1] <- media_id_col
  out
}
