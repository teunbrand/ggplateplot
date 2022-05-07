# Constructor -------------------------------------------------------------

#' Define a new plate layout
#'
#' The `new_plate_spec()` function is used to define the specification of a
#' plate layout. The `custom_plate_spec()` is a wrapper around
#' `new_plate_spec()` that heuristically sets dimensions for quick and dirty
#' layouts.
#'
#' @param width Width of the plate in millimetres.
#' @param height Height of the plate in millimetres.
#' @param hor_spacing Horizontal spacing between wells in millimetres. If
#'   `NULL` (default), the `well_spacing` argument will be used instead.
#' @param ver_spacing Vertical spacing between wells in millimetres. If `NULL`
#'   (default), the `well_spacing` argument will be used instead.
#' @param well_spacing Spacing between wells in millimetres. Useful when
#'   horizontal and vertical spacing is identical.
#' @param well_diameter Diameter of a well in millimetres.
#' @param corner_size Size of the adjacent/opposite sides of the triangle, in
#'   millimeters, when plate has a corner cut off.
#' @param ncol The number of wells in horizontal direction.
#' @param nrow The number of wells in vertical direction.
#'
#' @return A `list` with dimensions that can be understood by the `specs`
#'   argument in `coord_plate()`.
#' @export
#'
#' @examples
#' # Precisely detail a new layout
#' rotated_96well <- new_plate_spec(
#'   width         = 85.5,
#'   height        = 127.8,
#'   well_spacing  = 9,
#'   well_diameter = 6.8,
#'   ncol          = 8,
#'   nrow          = 12
#' )
#'
#' ## Setup dummy data
#' df <- expand.grid(row = 1:12, col = LETTERS[1:8])
#' df$value <- seq_len(nrow(df))
#'
#' ## Use new spec in plot
#' ggplot(df, aes(col, row, fill = value)) +
#'   geom_well() +
#'   coord_plate(spec = rotated_96well)
#'
#' # Quickly sketch a new layout
#' square_2x2 <- custom_plate_spec(ncol = 2, nrow = 2, width = 100, height = 100)
#'
#' ## Setup dummy data
#' df <- expand.grid(row = 1:2, col = LETTERS[1:2])
#' df$value <- seq_len(nrow(df))
#'
#' ## Use spec in plot
#' ggplot(df, aes(col, row, fill = value)) +
#'   geom_well() +
#'   coord_plate(spec = square_2x2)
new_plate_spec <- function(
  width         = 127.8,
  height        = 85.5,
  hor_spacing   = NULL,
  ver_spacing   = NULL,
  well_spacing  = 9,
  well_diameter = 6.8,
  corner_size   = 8,
  ncol          = 12L,
  nrow          = 8L
) {
  # Well dimensions
  hor_spacing  <- hor_spacing %||% well_spacing
  ver_spacing  <- ver_spacing %||% well_spacing
  well_spacing <- min(hor_spacing, ver_spacing)

  # Padding
  pad_width  <- (width  - (ncol - 1) * hor_spacing) / 2
  pad_height <- (height - (nrow - 1) * ver_spacing) / 2

  list(
    width         = width,
    height        = height,
    pad_width     = pad_width,
    pad_height    = pad_height,
    hor_spacing   = hor_spacing,
    ver_spacing   = ver_spacing,
    well_spacing  = well_spacing,
    well_diameter = well_diameter,
    corner_size   = corner_size,
    ncol          = ncol,
    nrow          = nrow
  )
}

#' @export
#' @rdname new_plate_spec
custom_plate_spec <- function(
  ncol = 12L,
  nrow = 8L,
  width  = 127.8,
  height = 85.5
) {

  well_width  <- (0.8 * width) / ncol
  well_height <- (0.8 * height) / nrow
  diameter    <- min(well_width, well_height) * 0.9

  new_plate_spec(
    width  = width,
    height = height,
    hor_spacing = well_width,
    ver_spacing = well_height,
    well_diameter = diameter,
    corner_size = 0.1 * min(width, height),
    ncol = ncol,
    nrow = nrow
  )
}

# Specifications ----------------------------------------------------------

spec_6well <- new_plate_spec(
  width         = 127.8,
  height        = 85.5,
  hor_spacing   = 40,
  ver_spacing   = 38,
  well_diameter = 35,
  ncol          = 3,
  nrow          = 2
)

spec_12well <- new_plate_spec(
  width         = 127.8,
  height        = 85.5,
  well_spacing  = 26,
  well_diameter = 22.8,
  ncol          = 4,
  nrow          = 3
)

spec_24well <- new_plate_spec(
  width         = 127.8,
  height        = 85.5,
  well_spacing  = 19,
  well_diameter = 16.5,
  ncol          = 6,
  nrow          = 4
)

spec_48well <- new_plate_spec(
  width         = 127.8,
  height        = 85.5,
  well_spacing  = 13,
  well_diameter = 10.7,
  ncol          = 8,
  nrow          = 6
)

spec_96well <- new_plate_spec(
  width         = 127.8,
  height        = 85.5,
  well_spacing  = 9,
  well_diameter = 6.8,
  ncol          = 12,
  nrow          = 8
)

spec_384well <- new_plate_spec(
  width         = 127.8,
  height        = 85.5,
  well_spacing  = 4.5,
  well_diameter = 3.65,
  ncol          = 24,
  nrow          = 16
)

all_specs <- list(
  "6"   = spec_6well,
  "12"  = spec_12well,
  "24"  = spec_24well,
  "48"  = spec_48well,
  "96"  = spec_96well,
  "384" = spec_384well
)

# Choosing ----------------------------------------------------------------

choose_wellplate <- function(n) {
  if (n %in% c(6, 12, 24, 48, 96, 384)) {
    name <- paste0("spec_", n, "well")
    obj  <- get(name, envir = asNamespace("ggplateplot"), mode = "list")
    return(obj)
  } else {
    rlang::abort(paste0(
      "A layout specification for ", n, " observation(s) ",
      "has not been implemented."
    ))
  }
}
