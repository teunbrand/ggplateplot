# Generic -----------------------------------------------------------------

#' Plate plots
#'
#' @inheritParams ggplot2::ggplot
#'
#' @return A `ggplot` object.
#' @export
#'
#' @examples
#' NULL
ggplateplot <- function(
  data    = NULL,
  mapping = aes(),
  ...,
  environment = parent.frame()
) {
  UseMethod("ggplateplot", data)
}

# Methods -----------------------------------------------------------------

#' @export
ggplateplot.default <- function(
  data    = NULL,
  mapping = aes(),
  ...,
  environment = parent.frame()
) {
  p <- ggplot(data = data, mapping = mapping, ..., environment = environment)
  if (!is.null(dim(data))) {
    d <- dim(data)
    p <- p + coord_plate(specs = d[1], default = TRUE)
  }
  p
}

#' @export
ggplateplot.array <- function(
  data = NULL,
  mapping = aes(),
  ...,
  environment = parent.frame()
) {
  dim  <- dim(data)
  data <- melt_array(data)

  if (is.null(mapping$x)) {
    mapping$x <- new_quosure(parse_expr(colnames(data)[2]))
  }
  if (is.null(mapping$y)) {
    mapping$y <- new_quosure(parse_expr(colnames(data)[1]))
  }

  p <- ggplot(data = data, mapping = mapping, ..., environment = environment)
  p <- p + coord_plate(specs = prod(dim[1:2]), default = TRUE)
  p

}

# Helpers -----------------------------------------------------------------

melt_array <- function(data) {

  dim <- dim(data)
  nms <- dimnames(data)

  # Get indices
  indices <- lapply(seq_along(dim), function(margin) {
    as.vector(slice.index(data, margin))
  })

  # Lookup dimnames with indices when appropriate
  if (length(nms)) {
    is_named <- lengths(nms) > 0
    indices[is_named] <- Map(
      factor,
      x      = indices[is_named],
      labels = nms[is_named]
    )
  }

  # Give indices variable names
  varnames <- names(nms) %||% rep("", length(dim))
  is_empty <- which(!nzchar(varnames) | varnames == "value")
  varnames[is_empty] <- paste0("Var", is_empty)
  names(indices) <- varnames

  # Convert to data.frame
  ans <- list2DF(indices)
  ans$value <- as.vector(data)

  return(ans)
}
