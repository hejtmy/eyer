#' Returns empty eyer object
#'
#' @description
#' EyerObject is a list with predetermined fields for consistent functioning of other
#' functions within the Eyer package. Lots of functions in the eyer package can
#' work on passed "raw" data, such as fixation dataframes etc., but the benefit of
#' preprocessing and parsing data inside the eyer object is that the object
#' is inherently validated and processed and functions know what to expect. This
#' data science design has been implemented from similar approaches found in Matlab
#' and Python packages.
#'
#' EyerObject has the following fields
#'
#' - data
#'    - events
#'    - fixations
#'    - diameter
#'    - gaze
#' - info: list with eyetracker specific settings, such as recording frequency, recorded eye etc.
#'    - start_time: start time of the first recording in datetime
#'    - resolution: monitor resolution as a list with width, height fields
#'    - eyetracker: string with name of the recording device
#'
#' Fixations have obligatory columns: time (commonly time since start), x, y, duration
#' Gaze have obligatory columns: time (commonly time since start), x, y
#' Events have columns: time, x, y
#'
#' @return eyer object
#' @export
#'
#' @examples
EyerObject <- function() {
  obj <- list()
  obj$data <- list()
  obj$data$events <- data.frame()
  obj$data$fixations <- data.frame()
  obj$data$gaze <- data.frame()
  obj$info <- list(
    resolution = list(width = NA, height = NA),
    eyetracker = character(0),
    start_time = NA
  )
  class(obj) <- append(class(obj), "eyer")
  return(obj)
}

#' Is obj eyer object?
#'
#' @param obj object to check
#'
#' @return logical
#' @export
#'
#' @examples
is.eyer <- function(obj) {
  return("eyer" %in% attributes(obj)$class)
}

#' Returns if passed object is valid eyer object
#'
#' @param obj Object to be tested
#'
#' @return logical stating if the object is valid
#' @export
#'
#' @examples
is_valid_eyer <- function(obj) {
  if (!is.eyer(obj)) {
    warning("object doesn't have eyer class")
    return(FALSE)
  }
  if (length(obj$info$start_time) != 1) {
    warning("object doesn't have valid start time")
    return(FALSE)
  }
  ## FIXATIONS
  # Checks validity of fixations in case there is one
  if (nrow(obj$data$fixations) > 0) {
    required_fixation_columns <- c("x", "y", "time", "duration")
    if (!all(required_fixation_columns %in% names(obj$data$fixations))) {
      warning("fixations don't have required", required_fixation_columns, " columns")
      return(FALSE)
    }
  }
  ## GAZE
  if (nrow(obj$data$gaze) > 0) {
    required_gaze_columns <- c("x", "y", "time")
    if (!all(required_gaze_columns %in% names(obj$data$gaze))) {
      warning("gaze positions don't have required", required_gaze_columns, " columns")
      return(FALSE)
    }
  }
  return(TRUE)
}

#' Defines AreaObject
#'
#' @details AreaObject is a list of class area with given fields. It is used by
#' plotting functions and mainly add_areas.
#' currently it only supports square objects
#'
#' @param name name of the area
#' @param x x limits of the area as numeric(2)
#' @param y y limits of the area as numeric(2)
#'
#' @return AreaObject
#' @export
#'
#' @examples
AreaObject <- function(name, x = numeric(2), y = numeric(2)) {
  obj <- list()
  obj$name <- name
  obj$type <- "square"
  obj$points <- list()
  obj$points$xmin <- x[1]
  obj$points$xmax <- x[2]
  obj$points$ymin <- y[1]
  obj$points$ymax <- y[2]
  class(obj) <- append(class(obj), "area")
  return(obj)
}

is_valid_area <- function(obj) {
  if (!("area" %in% attributes(obj)$class)) {
    return(FALSE)
  }
  if (obj$points$xmin == obj$points$xmax) {
    return(FALSE)
  }
  if (obj$points$ymin == obj$points$ymax) {
    return(FALSE)
  }
  return(TRUE)
}
