#' FRAGSTATS-style patch/class/landscape metrics from point.polygon annotations
#'
#' SQUIDLE+ polygon annotations store the polygon as `point.polygon`: a list
#' of vertices given as offsets *relative to* the annotation's anchor point
#' (`point.x`, `point.y`). Each media item (image) can carry many such
#' polygons, each with a class label — i.e. a small vector "landscape" per
#' image. These functions reconstruct real polygon geometry from that
#' relative representation and compute patch-, class-, and landscape-level
#' metrics analogous to FRAGSTATS, computed directly on vector geometry (via
#' sf) rather than on a rasterised grid.
#'
#' The full pipeline, in order:
#' \enumerate{
#'   \item [build_patch_sf()] reconstructs polygon geometry for every
#'     annotation across every image and class in one call, flags/removes
#'     geometrically invalid patches and patches that touch the image
#'     border (see below), and converts relative (0-1) coordinates to
#'     pixel coordinates via `media_width`/`media_height`.
#'   \item [calc_patch_metrics()] / [class_metrics()] / [landscape_metrics()]
#'     compute the actual metrics, optionally converting pixel area/
#'     perimeter to real-world units via a `scale` from [calc_image_scale()].
#'   \item [calc_fragstats()] wraps the whole pipeline in one call.
#' }
#'
#' \strong{Border/edge patches}: a polygon that touches the image frame is
#' truncated — it isn't the complete organism/substrate patch, just
#' whatever fraction fell inside the frame. FRAGSTATS treats this as a
#' known source of bias. `build_patch_sf()` counts how many vertices of
#' each polygon lie on the image border and, by default, flags (but does
#' not remove) any patch with 2 or more such vertices — a single border
#' vertex can happen by coincidence, but two or more strongly suggests the
#' patch boundary genuinely runs along the frame edge.
#'
#' \strong{Invalid geometry}: self-intersecting or otherwise degenerate
#' polygons (e.g. a hand-drawn "bowtie" shape) can still produce an area/
#' perimeter value that looks plausible but is wrong. `build_patch_sf()`
#' checks every patch with `sf::st_is_valid()` and flags failures by
#' default.
#'
#' Dependencies: sf, dplyr, jsonlite (only if point.polygon arrives as a
#' JSON string rather than a parsed list-column).

# ---------------------------------------------------------------------------
# 1. Geometry reconstruction
# ---------------------------------------------------------------------------

#' Parse a point.polygon annotation into absolute (unclosed) coordinates
#'
#' Internal helper shared by [polygon_to_sf()] and the border-vertex check
#' in [build_patch_sf()].
#'
#' @keywords internal
parse_polygon_coords <- function(point_x, point_y, polygon, media_width = 1, media_height = 1) {
  if (is.null(polygon)) return(NULL)
  if (is.character(polygon)) {
    if (!nzchar(polygon)) return(NULL)
    polygon <- jsonlite::fromJSON(polygon, simplifyDataFrame = TRUE)
  }
  if (length(polygon) == 0) return(NULL)

  verts <- if (is.data.frame(polygon) || is.matrix(polygon)) {
    as.data.frame(polygon)
  } else {
    do.call(rbind, lapply(polygon, function(v) data.frame(x = v$x, y = v$y)))
  }

  if (nrow(verts) < 3) return(NULL)

  abs_x <- (point_x + verts$x) * media_width
  abs_y <- (point_y + verts$y) * media_height
  cbind(abs_x, abs_y)
}

#' Count how many (unclosed) polygon vertices lie on the image border
#'
#' @keywords internal
count_border_vertices <- function(coords, media_width, media_height, edge_tolerance = 1e-6) {
  if (is.null(coords)) return(NA_integer_)
  x <- coords[, 1]
  y <- coords[, 2]
  on_border <- (x <= edge_tolerance) | (x >= media_width - edge_tolerance) |
    (y <= edge_tolerance) | (y >= media_height - edge_tolerance)
  sum(on_border)
}

#' Convert a single point.polygon annotation into an sf POLYGON
#'
#' @param point_x,point_y Anchor point coordinates for the annotation.
#' @param polygon The `point.polygon` value for this annotation: a list of
#'   `list(x=, y=)` vertices, a two-column data frame/matrix of vertex
#'   offsets, or a JSON string encoding either.
#' @param media_width,media_height Media dimensions to scale normalised
#'   (0-1) coordinates into pixel units. Leave at 1 if point.x/point.y and
#'   the polygon vertices are already in the units you want.
#' @return An sfg POLYGON, or NULL if the polygon is missing, empty, or
#'   degenerate (fewer than 3 distinct vertices).
#' @export
polygon_to_sf <- function(point_x, point_y, polygon, media_width = 1, media_height = 1) {
  coords <- parse_polygon_coords(point_x, point_y, polygon, media_width, media_height)
  if (is.null(coords)) return(NULL)

  if (!isTRUE(all.equal(coords[1, ], coords[nrow(coords), ]))) {
    coords <- rbind(coords, coords[1, ])
  }
  if (nrow(coords) < 4) return(NULL)

  tryCatch(sf::st_polygon(list(coords)), error = function(e) NULL)
}

#' Build an sf data frame of polygon patches from an annotation export
#'
#' Reconstructs geometry for every polygon annotation across all media
#' items and classes in one call. By default also flags (without removing)
#' patches with invalid geometry and patches whose polygon touches the
#' image border, since both are common sources of misleading patch metrics
#' — see Details.
#'
#' @param ann A data frame with an anchor point, polygon vertex list, class
#'   label, and media identifier column, spanning as many images/classes as
#'   you like — this is not scoped to one image or one class.
#' @param x_col,y_col Column names for the anchor point.
#' @param polygon_col Column name holding the vertex list/JSON.
#' @param label_col Column name holding the class label.
#' @param media_col Column name identifying the media item (image).
#' @param media_width,media_height Passed to [polygon_to_sf()]. Use the
#'   actual image width/height in pixels if you plan to apply a per-pixel
#'   `scale` downstream (see [calc_image_scale()]), and note these are also
#'   used as the image border for edge-touch detection.
#' @param flag_invalid If `TRUE` (default), add a `valid_geom` column
#'   (`sf::st_is_valid()` result) to the output.
#' @param remove_invalid If `TRUE`, drop patches with invalid geometry from
#'   the output entirely. Checked *after* `fix_invalid`, so only patches
#'   that remain invalid after an attempted repair are removed.
#' @param fix_invalid If `TRUE`, attempt to repair invalid geometries with
#'   `sf::st_make_valid()` before the validity check/removal above.
#' @param flag_edge If `TRUE` (default), add `n_border_vertices` and
#'   `touches_edge` columns marking patches whose polygon touches the image
#'   frame.
#' @param remove_edge If `TRUE`, drop patches flagged as touching the edge
#'   from the output entirely.
#' @param edge_vertex_threshold Minimum number of vertices lying on the
#'   image border for a patch to be flagged/removed as an edge patch
#'   (default `2`; a single border vertex can happen by coincidence, two or
#'   more strongly suggests the boundary genuinely runs along the frame).
#' @param edge_tolerance Numeric tolerance (same units as
#'   `media_width`/`media_height`) for considering a vertex "on" the
#'   border, to allow for floating-point imprecision.
#' @return An sf data frame with columns `media_id`, `class`, `patch_id`,
#'   `geometry`, and (unless disabled) `valid_geom`, `n_border_vertices`,
#'   `touches_edge`.
#' @export
build_patch_sf <- function(ann,
                           x_col = "point.x", y_col = "point.y",
                           polygon_col = "point.polygon",
                           label_col = "label.name",
                           media_col = "point.media.id",
                           media_width = 1, media_height = 1,
                           flag_invalid = TRUE, remove_invalid = FALSE, fix_invalid = FALSE,
                           flag_edge = TRUE, remove_edge = FALSE,
                           edge_vertex_threshold = 2, edge_tolerance = 1e-6) {
  required <- c(x_col, y_col, polygon_col, label_col, media_col)
  missing_cols <- setdiff(required, names(ann))
  if (length(missing_cols) > 0) {
    stop("build_patch_sf: missing column(s): ", paste(missing_cols, collapse = ", "),
         call. = FALSE)
  }

  n <- nrow(ann)
  polys <- vector("list", n)
  n_border <- rep(NA_integer_, n)

  for (i in seq_len(n)) {
    coords <- tryCatch(
      parse_polygon_coords(ann[[x_col]][i], ann[[y_col]][i], ann[[polygon_col]][[i]],
                           media_width, media_height),
      error = function(e) NULL
    )
    if (is.null(coords)) next

    n_border[i] <- count_border_vertices(coords, media_width, media_height, edge_tolerance)

    ring <- coords
    if (!isTRUE(all.equal(ring[1, ], ring[nrow(ring), ]))) ring <- rbind(ring, ring[1, ])
    if (nrow(ring) >= 4) {
      polys[[i]] <- tryCatch(sf::st_polygon(list(ring)), error = function(e) NULL)
    }
  }

  valid_geom_present <- !vapply(polys, is.null, logical(1))
  n_dropped <- sum(!valid_geom_present)
  if (n_dropped > 0) {
    message(sprintf(
      "build_patch_sf: dropped %d/%d annotation(s) with no usable polygon (point/line annotations, or malformed geometry).",
      n_dropped, n
    ))
  }

  out <- data.frame(
    media_id = ann[[media_col]][valid_geom_present],
    class = ann[[label_col]][valid_geom_present],
    stringsAsFactors = FALSE
  )
  out$patch_id <- seq_len(nrow(out))
  out$n_border_vertices <- n_border[valid_geom_present]
  out$touches_edge <- out$n_border_vertices >= edge_vertex_threshold

  patches <- sf::st_sf(out, geometry = sf::st_sfc(polys[valid_geom_present]))

  patches <- check_patch_validity(patches, fix = fix_invalid)

  n_invalid <- sum(!patches$valid_geom)
  n_edge <- sum(patches$touches_edge)
  if (n_invalid > 0) {
    message(sprintf("build_patch_sf: %d patch(es) flagged with invalid geometry.", n_invalid))
  }
  if (n_edge > 0) {
    message(sprintf(
      "build_patch_sf: %d patch(es) flagged as touching the image border (>= %d vertices).",
      n_edge, edge_vertex_threshold
    ))
  }

  if (isTRUE(remove_invalid)) patches <- patches[patches$valid_geom, ]
  if (isTRUE(remove_edge)) patches <- patches[!patches$touches_edge, ]

  if (!isTRUE(flag_invalid)) patches$valid_geom <- NULL
  if (!isTRUE(flag_edge)) {
    patches$n_border_vertices <- NULL
    patches$touches_edge <- NULL
  }

  patches
}

#' Check (and optionally repair) patch geometry validity
#'
#' Standalone version of the validity check performed inside
#' [build_patch_sf()] — useful if you already have an sf patch data frame
#' from elsewhere and just want to (re-)check it.
#'
#' @param patches An sf data frame of patches.
#' @param fix If `TRUE`, attempt to repair invalid geometries with
#'   `sf::st_make_valid()` before reporting validity.
#' @return `patches` with a `valid_geom` logical column added/updated.
#' @export
check_patch_validity <- function(patches, fix = FALSE) {
  if (isTRUE(fix)) {
    bad <- which(!sf::st_is_valid(patches))
    if (length(bad) > 0) {
      sf::st_geometry(patches)[bad] <- sf::st_make_valid(sf::st_geometry(patches)[bad])
      message(sprintf(
        "check_patch_validity: attempted to repair %d invalid geometr%s via st_make_valid().",
        length(bad), if (length(bad) == 1) "y" else "ies"
      ))
    }
  }
  patches$valid_geom <- sf::st_is_valid(patches)
  patches
}

#' Flag patches whose polygon touches the image border
#'
#' Standalone version of the edge-touch check performed inside
#' [build_patch_sf()] — useful if you already have an sf patch data frame
#' and just want to (re-)flag it against a given frame size, recomputing
#' border-touch counts directly from geometry rather than the original
#' relative vertex list.
#'
#' @param patches An sf data frame of patches, in the same coordinate units
#'   as `media_width`/`media_height`.
#' @param media_width,media_height Image dimensions defining the border.
#' @param edge_vertex_threshold Minimum number of border-touching vertices
#'   to flag a patch as an edge patch (default `2`).
#' @param edge_tolerance Numeric tolerance for considering a vertex "on"
#'   the border.
#' @return `patches` with `n_border_vertices` and `touches_edge` columns
#'   added/updated.
#' @export
flag_edge_patches <- function(patches, media_width, media_height,
                              edge_vertex_threshold = 2, edge_tolerance = 1e-6) {
  n_border <- vapply(sf::st_geometry(patches), function(g) {
    coords <- sf::st_coordinates(g)[, 1:2, drop = FALSE]
    coords <- coords[-nrow(coords), , drop = FALSE] # drop closing duplicate vertex
    count_border_vertices(coords, media_width, media_height, edge_tolerance)
  }, numeric(1))

  patches$n_border_vertices <- n_border
  patches$touches_edge <- n_border >= edge_vertex_threshold
  patches
}

# ---------------------------------------------------------------------------
# 2. Image scale (pixels -> real-world units)
# ---------------------------------------------------------------------------

#' Estimate per-image real-world scale (units per pixel)
#'
#' Polygon area/perimeter computed straight from pixel coordinates aren't
#' comparable across images unless every image was captured from the same
#' altitude with the same camera — rarely true for AUV/ROV/drop-camera
#' survey imagery. This derives a per-media-item scale factor (real-world
#' units per pixel, e.g. m/px) from one of three sources:
#'
#' \describe{
#'   \item{\code{"manual"}}{A scale value you already know (e.g. from a
#'     prior calibration or a fixed rig geometry) supplied directly in
#'     `media`.}
#'   \item{\code{"laser"}}{Two parallel laser points a known real-world
#'     distance apart, visible in the image. scale = known separation /
#'     pixel distance between the dots. Most accurate since it's measured
#'     directly in each image.}
#'   \item{\code{"altitude"}}{Camera height above the seafloor
#'     (`pose.pose.alt`) combined with camera intrinsics, via the pinhole
#'     camera model:
#'     \code{scale = altitude * sensor_width / (focal_length * image_width_px)}
#'     (provide `sensor_width_mm` + `focal_length_mm`), or equivalently
#'     \code{scale = 2 * altitude * tan(hfov/2) / image_width_px}
#'     (provide `hfov_deg`). Assumes near-nadir imagery over a locally flat
#'     seafloor — least accurate of the three, use laser points where
#'     available.}
#' }
#'
#' @param media A data frame with one row per media item, containing
#'   whichever columns the chosen `method` needs.
#' @param media_col Column identifying the media item.
#' @param method One of `"manual"`, `"laser"`, `"altitude"`.
#' @param scale_col (manual) Column already containing scale in real-world
#'   units per pixel.
#' @param laser_px_distance_col,laser_separation_col (laser) Columns giving
#'   the pixel distance between two laser points, and their known
#'   real-world separation.
#' @param altitude_col (altitude) Column with camera altitude above the
#'   seafloor, e.g. `"pose.pose.alt"`.
#' @param sensor_width_mm,focal_length_mm (altitude, pinhole model) Camera
#'   sensor width and focal length in mm. Provide these OR `hfov_deg`.
#' @param hfov_deg (altitude, FOV model) Horizontal field of view in
#'   degrees. Provide this OR `sensor_width_mm` + `focal_length_mm`.
#' @param image_width_px (altitude) Image width in pixels — should match
#'   `media_width` used in [build_patch_sf()].
#' @return A data frame with columns `media_id` and `scale`, suitable for
#'   passing as the `scale` argument to [calc_patch_metrics()],
#'   [class_metrics()], [landscape_metrics()], or [calc_fragstats()].
#' @export
calc_image_scale <- function(media, media_col = "media_id",
                             method = c("manual", "laser", "altitude"),
                             scale_col = "scale",
                             laser_px_distance_col = "laser_px_distance",
                             laser_separation_col = "laser_separation",
                             altitude_col = "pose.pose.alt",
                             sensor_width_mm = NULL, focal_length_mm = NULL,
                             hfov_deg = NULL, image_width_px = NULL) {
  method <- match.arg(method)

  scale <- switch(
    method,
    manual = {
      if (!scale_col %in% names(media)) {
        stop("calc_image_scale: scale_col '", scale_col, "' not found in `media`.", call. = FALSE)
      }
      media[[scale_col]]
    },
    laser = {
      req <- c(laser_px_distance_col, laser_separation_col)
      missing <- setdiff(req, names(media))
      if (length(missing) > 0) {
        stop("calc_image_scale: missing laser column(s): ", paste(missing, collapse = ", "),
             call. = FALSE)
      }
      media[[laser_separation_col]] / media[[laser_px_distance_col]]
    },
    altitude = {
      if (!altitude_col %in% names(media)) {
        stop("calc_image_scale: altitude_col '", altitude_col, "' not found in `media`.", call. = FALSE)
      }
      if (is.null(image_width_px)) {
        stop("calc_image_scale: image_width_px is required for method = 'altitude'.", call. = FALSE)
      }
      alt <- media[[altitude_col]]
      if (!is.null(hfov_deg)) {
        2 * alt * tan((hfov_deg * pi / 180) / 2) / image_width_px
      } else if (!is.null(sensor_width_mm) && !is.null(focal_length_mm)) {
        alt * sensor_width_mm / (focal_length_mm * image_width_px)
      } else {
        stop("calc_image_scale: provide either hfov_deg, or both sensor_width_mm and focal_length_mm, ",
             "for method = 'altitude'.", call. = FALSE)
      }
    }
  )

  data.frame(media_id = media[[media_col]], scale = as.numeric(scale))
}

#' Apply a per-media (or global) scale factor to raw pixel area/perimeter
#'
#' @param df Data frame with columns `media_id`, `area`, `perimeter` (in
#'   pixel units).
#' @param scale `NULL` (no scaling), a single numeric (applied to every
#'   row), or a data frame with columns `media_id`, `scale`.
#' @return `df` with `area`/`perimeter` converted if a scale was supplied.
#' @keywords internal
apply_scale <- function(df, scale) {
  if (is.null(scale)) return(df)

  if (is.data.frame(scale)) {
    req <- c("media_id", "scale")
    if (!all(req %in% names(scale))) {
      stop("apply_scale: `scale` data frame must have columns 'media_id' and 'scale'.", call. = FALSE)
    }
    idx <- match(df$media_id, scale$media_id)
    df$.scale <- scale$scale[idx]
    if (any(is.na(df$.scale))) {
      warning(sprintf(
        "apply_scale: %d patch(es) have no matching scale (media_id not found) — left unscaled (pixel units) for those rows.",
        sum(is.na(df$.scale))
      ))
      df$.scale[is.na(df$.scale)] <- 1
    }
  } else if (is.numeric(scale) && length(scale) == 1) {
    df$.scale <- scale
  } else {
    stop("apply_scale: `scale` must be NULL, a single numeric, or a data frame with media_id/scale columns.",
         call. = FALSE)
  }

  df$area <- df$area * df$.scale^2
  df$perimeter <- df$perimeter * df$.scale
  df$.scale <- NULL
  df
}

# ---------------------------------------------------------------------------
# 3. Patch-level metrics — pick-and-choose registry
# ---------------------------------------------------------------------------

#' Registry of available FRAGSTATS-style patch metrics
#'
#' Each entry is a function taking a data frame with `area` and `perimeter`
#' columns (already in real-world units, if a `scale` was applied) and
#' returning a numeric vector of the same length.
#'
#' \describe{
#'   \item{area}{Patch area.}
#'   \item{perimeter}{Patch perimeter.}
#'   \item{shape_index}{perimeter / (2*sqrt(pi*area)); 1 = circle,
#'     increasing with boundary convolution/elongation. Dimensionless —
#'     unaffected by pixel vs real-world units.}
#'   \item{frac_dim}{2*ln(0.25*perimeter) / ln(area); ~1 (simple) to ~2
#'     (convoluted). Not scale-invariant — apply `scale` first if you want
#'     it in real-world terms. NA for area <= 1.}
#'   \item{para}{Perimeter-area ratio.}
#'   \item{gyrate}{Radius of a circle with area equal to the patch.}
#' }
#' @export
patch_metric_registry <- list(
  area        = function(d) d$area,
  perimeter   = function(d) d$perimeter,
  shape_index = function(d) d$perimeter / (2 * sqrt(pi * d$area)),
  frac_dim    = function(d) ifelse(d$area > 1, 2 * log(0.25 * d$perimeter) / log(d$area), NA_real_),
  para        = function(d) d$perimeter / d$area,
  gyrate      = function(d) sqrt(d$area / pi)
)

#' Compute one or more FRAGSTATS-style patch-level metrics
#'
#' @param patches An sf data frame from [build_patch_sf()]. If it carries
#'   `valid_geom`/`touches_edge` flag columns from `build_patch_sf()`,
#'   they're preserved in the output alongside the requested metrics —
#'   filter on them explicitly if you didn't already remove flagged
#'   patches upstream.
#' @param metrics Character vector of metric names to compute, or `"all"`
#'   (default) for every metric in [patch_metric_registry].
#' @param keep_geometry If `TRUE` (default) returns an sf object; if
#'   `FALSE` returns a plain data frame.
#' @param scale Real-world scale to convert pixel-based area/perimeter
#'   before computing metrics: `NULL`, a single numeric, or a per-media
#'   data frame from [calc_image_scale()].
#' @return A data frame (sf or plain) with id columns `media_id`, `class`,
#'   `patch_id` (plus any flag columns present on `patches`), plus one
#'   column per requested metric.
#' @export
calc_patch_metrics <- function(patches, metrics = "all", keep_geometry = TRUE, scale = NULL) {
  available <- names(patch_metric_registry)

  if (identical(metrics, "all")) metrics <- available

  unknown <- setdiff(metrics, available)
  if (length(unknown) > 0) {
    stop(
      "calc_patch_metrics: unknown metric(s): ", paste(unknown, collapse = ", "),
      "\nAvailable metrics: ", paste(available, collapse = ", "),
      call. = FALSE
    )
  }

  base <- data.frame(
    media_id  = sf::st_drop_geometry(patches)$media_id,
    area      = as.numeric(sf::st_area(patches)),
    perimeter = as.numeric(sf::st_length(sf::st_boundary(patches)))
  )
  base <- apply_scale(base, scale)

  id_cols <- intersect(
    c("media_id", "class", "patch_id", "valid_geom", "n_border_vertices", "touches_edge"),
    names(patches)
  )
  out <- sf::st_drop_geometry(patches)[, id_cols, drop = FALSE]
  for (m in metrics) {
    out[[m]] <- patch_metric_registry[[m]](base)
  }

  if (isTRUE(keep_geometry)) {
    out <- sf::st_sf(out, geometry = sf::st_geometry(patches))
  }

  out
}

# ---------------------------------------------------------------------------
# 4. Class-level metrics (per media item x class)
# ---------------------------------------------------------------------------

#' Compute FRAGSTATS-style class-level metrics per media item
#'
#' @param patches An sf data frame from [build_patch_sf()].
#' @param media_area Optional data frame with columns `media_id`,
#'   `landscape_area`. If NULL, landscape area per media item is the sum
#'   of its (possibly scaled) patch areas.
#' @param scale Passed to [calc_patch_metrics()].
#' @return A data frame with one row per media_id x class.
#' @export
class_metrics <- function(patches, media_area = NULL, scale = NULL) {
  df <- calc_patch_metrics(patches, metrics = "all", keep_geometry = FALSE, scale = scale)

  if (is.null(media_area)) {
    media_area <- stats::aggregate(area ~ media_id, df, sum)
    names(media_area) <- c("media_id", "landscape_area")
  }

  df <- merge(df, media_area, by = "media_id", all.x = TRUE)

  agg <- df |>
    dplyr::group_by(.data$media_id, .data$class) |>
    dplyr::summarise(
      landscape_area   = dplyr::first(.data$landscape_area),
      n_patches        = dplyr::n(),
      total_area       = sum(.data$area),
      largest_patch    = max(.data$area),
      mean_patch_size  = mean(.data$area),
      sd_patch_size    = stats::sd(.data$area),
      total_edge       = sum(.data$perimeter),
      mean_shape_index = mean(.data$shape_index),
      mean_frac_dim    = mean(.data$frac_dim, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      pland          = 100 * .data$total_area / .data$landscape_area,
      lpi            = 100 * .data$largest_patch / .data$landscape_area,
      patch_density  = 10000 * .data$n_patches / .data$landscape_area,
      edge_density   = .data$total_edge / .data$landscape_area
    )

  as.data.frame(agg)
}

# ---------------------------------------------------------------------------
# 5. Landscape-level metrics (per media item)
# ---------------------------------------------------------------------------

#' Compute FRAGSTATS-style landscape-level metrics per media item
#'
#' @param patches An sf data frame from [build_patch_sf()].
#' @param media_area Passed to [class_metrics()].
#' @param scale Passed to [class_metrics()] / [calc_patch_metrics()].
#' @return A data frame with one row per media_id.
#' @export
landscape_metrics <- function(patches, media_area = NULL, scale = NULL) {
  cls <- class_metrics(patches, media_area, scale)

  cls |>
    dplyr::group_by(.data$media_id) |>
    dplyr::summarise(
      landscape_area      = dplyr::first(.data$landscape_area),
      n_classes           = dplyr::n_distinct(.data$class),
      n_patches           = sum(.data$n_patches),
      patch_density       = 10000 * sum(.data$n_patches) / dplyr::first(.data$landscape_area),
      largest_patch_index = max(.data$lpi),
      mean_patch_size     = sum(.data$total_area) / sum(.data$n_patches),
      edge_density        = sum(.data$total_edge) / dplyr::first(.data$landscape_area),
      shdi = -sum((.data$pland / 100) * log(.data$pland / 100)),
      sidi = 1 - sum((.data$pland / 100)^2),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      shei = ifelse(.data$n_classes > 1, .data$shdi / log(.data$n_classes), NA_real_)
    ) |>
    as.data.frame()
}

# ---------------------------------------------------------------------------
# 6. Convenience wrapper
# ---------------------------------------------------------------------------

#' Compute FRAGSTATS-style metrics from an annotation export in one call
#'
#' Runs the full pipeline — geometry reconstruction, validity/edge
#' flagging, scaling, and metric computation — across every image and
#' class present in `ann`.
#'
#' @inheritParams build_patch_sf
#' @param level Which output(s) to compute: any combination of `"patch"`,
#'   `"class"`, `"landscape"`, or `"all"` (default).
#' @param patch_metrics_wanted Character vector of patch metric names (see
#'   [patch_metric_registry]) to include in the `patch` output, or `"all"`.
#' @param media_area Passed to [class_metrics()] / [landscape_metrics()].
#' @param scale Real-world scale to apply before computing any metric:
#'   `NULL`, a single numeric, or a per-media data frame from
#'   [calc_image_scale()] — typically built from `pose.pose.alt` plus
#'   camera intrinsics, or laser point separation where available.
#' @return A list containing whichever of `patch`, `class`, `landscape`
#'   were requested via `level`.
#' @examples
#' \dontrun{
#' ann <- get_annotations_by_set(api, annotation_set_id = 5432)
#' media <- get_media_by_campaign(api, campaign_id = 123)
#'
#' img_scale <- calc_image_scale(media, method = "altitude",
#'                                altitude_col = "pose.pose.alt",
#'                                hfov_deg = 54, image_width_px = 4000)
#'
#' # flag (but keep) invalid/edge patches, exclude both from area stats
#' fs <- calc_fragstats(ann, media_width = 4000, media_height = 3000,
#'                       scale = img_scale,
#'                       remove_invalid = TRUE, remove_edge = TRUE)
#' fs$landscape
#' }
#' @export
calc_fragstats <- function(ann,
                           x_col = "point.x", y_col = "point.y",
                           polygon_col = "point.polygon",
                           label_col = "label.name",
                           media_col = "point.media.id",
                           media_width = 1, media_height = 1,
                           flag_invalid = TRUE, remove_invalid = FALSE, fix_invalid = FALSE,
                           flag_edge = TRUE, remove_edge = FALSE,
                           edge_vertex_threshold = 2, edge_tolerance = 1e-6,
                           level = "all",
                           patch_metrics_wanted = "all",
                           media_area = NULL,
                           scale = NULL) {
  if (identical(level, "all")) level <- c("patch", "class", "landscape")
  unknown_level <- setdiff(level, c("patch", "class", "landscape"))
  if (length(unknown_level) > 0) {
    stop("calc_fragstats: unknown level(s): ", paste(unknown_level, collapse = ", "),
         '\nValid levels: "patch", "class", "landscape", "all"', call. = FALSE)
  }

  patches <- build_patch_sf(ann, x_col, y_col, polygon_col, label_col, media_col,
                            media_width, media_height,
                            flag_invalid = flag_invalid, remove_invalid = remove_invalid,
                            fix_invalid = fix_invalid,
                            flag_edge = flag_edge, remove_edge = remove_edge,
                            edge_vertex_threshold = edge_vertex_threshold,
                            edge_tolerance = edge_tolerance)

  if (nrow(patches) == 0) {
    warning("calc_fragstats: no valid polygons found.", call. = FALSE)
    empty <- list(patch = patches, class = data.frame(), landscape = data.frame())
    return(empty[level])
  }

  out <- list()
  if ("patch" %in% level) {
    out$patch <- calc_patch_metrics(patches, metrics = patch_metrics_wanted, scale = scale)
  }
  if ("class" %in% level) {
    out$class <- class_metrics(patches, media_area, scale)
  }
  if ("landscape" %in% level) {
    out$landscape <- landscape_metrics(patches, media_area, scale)
  }

  out
}
