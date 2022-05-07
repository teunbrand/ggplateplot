replace_waiver <- function(x, y) {
  if (inherits(x, "waiver")) {
    return(y)
  } else {
    return(x)
  }
}

protect_zero_length <- function(x, alt = NULL) {
  if (length(x) == 0) {
    return(alt)
  } else {
    return(x)
  }
}
