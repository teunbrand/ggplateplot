# User function -----------------------------------------------------------

#' Plate coordinates
#'
#' This coordinate functions makes your plot look more like a microwell plate.
#'
#' @inheritParams ggplot2::coord_cartesian
#' @param corner The place to draw the notched corner of a plate. Can be one of
#'   `"left"`, `"right"`, `"top"`, `"bottom"`, `"topleft"`,
#'   `"topright"`, `"bottomleft"`, `"bottomright"`, `"none"` or `"all"`.
#' @param specs Either a `list` with plate dimensions or a `numeric(1)` with
#'   one of the common formats, e.g. `6`, `12`, `24`, `48`, `96` or `384` wells.
#'
#' @return A `Coord` object that can be added to a plot.
#' @export
#'
#' @details The `panel.background` theme element now draws a polygon instead
#'  of a rectangle in case `corner != "none"`. Moreover, the `panel.grid.major`
#'  element is used to draw circles at well positions and `panel.grid.minor` is
#'  used to draw regular grid lines. Actual minor breaks are ignored.
#'
#'  The dimensions of most standard plates are roughly based on technical data
#'  sheets available on the Eppendorf website, a manufacturer of cell culture
#'  consumables.
#'
#' @examples
#' # Make basic plot
#' df <- data.frame(x = 1:4, y = 1:4)
#'
#' p <- ggplot(df, aes(x = x, y = y)) +
#'   geom_well()
#'
#' # Using spec and limits to change layout
#' p + coord_plate(spec = 96, xlim = c(1, 12), ylim = c(1, 8))
#' p + coord_plate(spec = 48, xlim = c(1, 8), ylim = c(1, 6))
#'
#' # Changing where the corner is placed
#' p + coord_plate(spec = 24, xlim = c(1, 6), ylim = c(1, 4),
#'                 corner = "left")
coord_plate <- function(
    xlim    = NULL,
    ylim    = NULL,
    expand  = TRUE,
    default = FALSE,
    clip    = "off",
    corner  = "right",
    specs   = 96
) {
  if (!is.list(specs)) {
    specs <- choose_wellplate(specs)
  }
  ratio <- specs$ver_spacing / specs$hor_spacing

  ggproto(
    NULL,
    CoordPlate,
    limits  = list(x = xlim, y = ylim),
    ratio   = ratio,
    expand  = expand,
    clip    = clip,
    corner  = corner,
    specs   = specs,
    default = default
  )
}

# Class -------------------------------------------------------------------

CoordPlate <- ggproto(
  NULL, CoordFixed,

  render_fg = function(self, panel_params, theme) {
    specs  <- self$specs
    corner <- self$corner
    plate_background(
      theme, "panel.border",
      corner, specs$corner_size / specs$height
    )
    # element_render(theme, "panel.border")
  },

  render_bg = function(self, panel_params, theme) {
    guide_wells(
      theme,
      panel_params$x$break_positions_minor(),
      panel_params$x$break_positions(),
      panel_params$y$break_positions_minor(),
      panel_params$y$break_positions(),
      specs  = self$specs,
      corner = self$corner
    )
  },

  modify_scales = function(self, scales_x, scales_y) {
    specs <- self$specs
    x_expand <- expansion(mult = with(specs, pad_width / (width - 2 * pad_width)))
    y_expand <- expansion(mult = with(specs, pad_height / (height - 2 * pad_height)))

    lapply(scales_x, function(sc) {
      sc$expand <- replace_waiver(sc$expand, x_expand)
      if (!sc$is_discrete()) {
        sc$breaks <- replace_waiver(sc$breaks, seq_len(specs$ncol))
      }
      invisible()
    })
    lapply(scales_y, function(sc) {
      sc$expand <- replace_waiver(sc$expand, y_expand)
      if (!sc$is_discrete()) {
        sc$breaks <- replace_waiver(sc$breaks, seq_len(specs$nrow))
      } else {
        sc$limits <- sc$limits %||% rev
      }
      invisible()
    })
    invisible()
  }
)

# Guides ------------------------------------------------------------------

guide_wells <- function(
  theme,
  x.minor,
  x.major,
  y.minor,
  y.major,
  specs,
  corner = "right"
) {

  background <- plate_background(
    theme, "panel.background",
    corner, specs$corner_size / specs$height
  )

  xlines     <- element_render(
    theme, "panel.grid.minor.x",
    x = rep(x.major, each = 2),
    y = rep(0:1, length(x.major)),
    id.lengths = rep(2, length(x.major))
  )
  ylines     <- element_render(
    theme, "panel.grid.minor.y",
    x = rep(0:1, length(y.major)),
    y = rep(y.major, each = 2),
    id.lengths = rep(2, length(y.major))
  )

  well_pos <- expand.grid(x = x.major, y = y.major)
  mindim   <- with(specs, min(height, width))
  radius   <- 0.5 * (specs$well_spacing / mindim)
  elem     <- calc_element("panel.grid.major", theme)

  if (inherits(elem, "element_blank")) {
    wells <- zeroGrob()
  } else {
    wells <- circleGrob(
      x = well_pos$x,
      y = well_pos$y,
      r = radius,
      gp = gpar(
        col  = elem$colour,
        fill = NA,
        lwd  = protect_zero_length(elem$size * .pt),
        lty  = elem$linetype
      )
    )
  }

  ans <- grobTree(
    background,
    xlines,
    ylines,
    wells
  )
  ans$name <- grobName(ans, "grill")
  ans

}

plate_background <- function(theme, element, corner, corner_size) {
  elem <- calc_element(element, theme)
  if (inherits(elem, "element_blank")) {
    return(zeroGrob())
  }

  quarter_circle  <- seq(0, 0.5 * pi, length.out = 9)
  rounded_x <- unit((3 - cos(quarter_circle) * 3) / 85.5, "snpc")
  rounded_y <- unit((3 - sin(quarter_circle) * 3) / 85.5, "snpc")

  fwd <- unit(c(corner_size, 0), "snpc")
  rev <- unit(c(0, corner_size), "snpc")

  if (corner %in% c("topright", "right", "top", "all")) {
    x <- unit(c(1, 1), "npc") - fwd
    y <- unit(c(1, 1), "npc") - rev
  } else {
    x <- unit(1, "npc") - rounded_y
    y <- unit(1, "npc") - rounded_x
  }

  if (corner %in% c("bottomright", "right", "bottom", "all")) {
    nx <- unit(c(1, 1), "npc") - rev
    ny <- unit(c(0, 0), "npc") + fwd
  } else {
    nx <- unit(1, "npc") - rounded_x
    ny <- unit(0, "npc") + rounded_y
  }
  x <- unit.c(x, nx)
  y <- unit.c(y, ny)

  if (corner %in% c("bottomleft", "left", "bottom", "all")) {
    nx <- unit(c(0, 0), "npc") + fwd
    ny <- unit(c(0, 0), "npc") + rev
  } else {
    nx <- unit(0, "npc") + rounded_y
    ny <- unit(0, "npc") + rounded_x
  }
  x <- unit.c(x, nx)
  y <- unit.c(y, ny)

  if (corner %in% c("topleft", "left", "top", "all")) {
    nx <- unit(c(0, 0), "npc") + rev
    ny <- unit(c(1, 1), "npc") - fwd
  } else {
    nx <- unit(0, "npc") + rounded_x
    ny <- unit(1, "npc") - rounded_y
  }
  x <- unit.c(x, nx)
  y <- unit.c(y, ny)

  gp <- gpar(
    lwd  = protect_zero_length(elem$size * .pt),
    col  = elem$colour,
    fill = elem$fill,
    lty  = elem$linetype
  )

  pathGrob(
    x = x, y = y,
    gp = gp
  )
}
