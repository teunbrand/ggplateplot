
# User function -----------------------------------------------------------

#' Wells
#'
#' This geoms draws circles in a fixed size.
#'
#' @inheritParams ggplot2::geom_point
#'
#' @details The sized is fixed such that a `size = 1` corresponds to 1 unit
#'   on the x- or y-axis, whichever one has the larger range. Alternatively,
#'   when combined with `coord_plate()`, setting `size = 1` is equivalent to
#'   setting the size to a well's diameter.
#'
#' @return A `Layer` object that can be added to a plot.
#' @export
#'
#' @examples
#' NULL
geom_well <- function(
  mapping  = NULL,
  data     = NULL,
  stat     = "identity",
  position = "identity",
  ...,
  na.rm       = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
) {
  layer(
    data     = data,
    mapping  = mapping,
    stat     = stat,
    geom     = GeomWell,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params   = list(
      na.rm = na.rm,
      ...
    )
  )
}

# Class -------------------------------------------------------------------

GeomWell <- ggproto(
  NULL, Geom,

  # Aesthetic settings
  required_aes    = c("x", "y"),
  non_missing_aes = c("size"),
  default_aes     = aes(
    size  = 1,  colour = NA, fill = "black",
    alpha = NA, stroke = 0.5, linetype = 1
  ),

  # Drawing function
  draw_panel = function(data, panel_params, coord, na.rm = FALSE) {

    coords <- coord$transform(data, panel_params)
    stroke_size <- coords$stroke
    stroke_size[is.na(stroke_size)] <- 0

    size <- coords$size

    # If coord has specifications, adopt size based on well diameter
    if (!is.null(coord$specs)) {
      size <- size * with(coord$specs, well_diameter / height) / 2
    } else {

      # Otherwise, take 1 unit distance on widest axis as size
      dx <- 1 / diff(panel_params$x$continuous_range)
      dy <- 1 / diff(panel_params$y$continuous_range)
      size <- pmin(size * dx, size * dy) / 2
    }

    # Create graphical object
    grob <- circleGrob(
      x  = coords$x,
      y  = coords$y,
      r  = size,
      gp = gpar(
        col  = alpha(coords$colour, coords$alpha),
        fill = alpha(coords$fill,   coords$alpha),
        lwd  = stroke_size,
        lty  = coords$linetype
      )
    )

    # Rename grob
    grob$name <- grobName(grob, "well")
    grob
  }
)
