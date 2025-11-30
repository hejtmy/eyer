#' Approximates x, y resolution from given coordinates to designated
#'
#' @param obj Object to do transformations on
#' @param resolution defined as a list with width and height in pixels.
#' Ex: list(width=1920, height=1080)
#' @param target defined as a list with width and height in pixels.
#' Ex: list(width=1920, height=1080)
#'
#' @return data frame with fixations with changed resolution. Can still yield out of bounds
#' @export
#'
#' @examples
change_resolution <- function(obj, resolution, target, ...) {
  UseMethod("change_resolution")
}

#' @export
change_resolution.eyer <- function(obj, resolution, target) {
  if (nrow(obj$data$fixations) > 0) obj$data$fixations <- change_resolution.data.frame(obj$data$fixations, resolution, target)
  if (nrow(obj$data$gaze) > 0) obj$data$gaze <- change_resolution.data.frame(obj$data$gaze, resolution, target)
  obj$info$resolution <- target
  return(obj)
}

#' @export
change_resolution.data.frame <- function(df, resolution, target) {
  # validations and check for errors
  df$x <- round(df$x / resolution$width * target$width)
  df$y <- round(df$y / resolution$height * target$height)
  return(df)
}

#'  Removes points out of resolution boundary
#'
#' @param obj object
#' @param replace what should the out of bounds values be replaced with? if NULL, rows are deleted. NULL by default
#' @param resolution defined as a list with width and height in pixels. e.g: list(width=1920, height=1080)
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
remove_out_of_bounds <- function(obj, replace = NULL, resolution = NULL, ...) {
  UseMethod("remove_out_of_bounds")
}

#'  Removes points out of resolution boundary
#'
#' @param obj \code{\link{EyerObject}}
#' @param replace what should the out of bounds values be replaced with? if NULL, rows are deleted. NULL by default
#' @param resolution defined as a list with width and height in pixels. e.g: list(width = 1920, height = 1080)
#'
#' @return
#' @export
#'
#' @examples
remove_out_of_bounds.eyer <- function(obj, replace, resolution) {
  if (is.null(resolution)) resolution <- obj$info$resolution
  if (is.null(resolution)) {
    warning("Object doesn't have resolution value in info and no resulotion passed, returning unmodified object")
    return(obj)
  }
  if (nrow(obj$data$fixations) > 0) obj$data$fixations <- remove_out_of_bounds.data.frame(obj$data$fixations, replace, resolution)
  if (nrow(obj$data$gaze) > 0) obj$data$gaze <- remove_out_of_bounds.eyer(obj$data$gaze, replace, resolution)
  return(obj)
}

#' Removes points out of resolution boundary
#'
#' @param df table with x, y, columns
#' @param replace what should the out of bounds values be replaced with? if NULL, rows are deleted. NULL by default
#' @param resolution defined as a list with width and height in pixels. e.g: list(width=1920, height=1080)
#'
#' @return eyer object with replaced values in gaze and fixations
#' @export
#'
#' @examples
remove_out_of_bounds.data.frame <- function(df, replace, resolution) {
  if (is.null(replace)) {
    df <- df[df$x < df$width & df$y < df$height, ]
    df <- df[df$x > 0 & df$y > 0, ]
  } else {
    df[df$x < df$width & df$y < df$height, c("x", "y")] <- c(replace, replace)
    df[df$x > 0 & df$y > 0, c("x", "y")] <- c(replace, replace)
  }
  return(df)
}

#' Flips the given axis to its negative
#'
#' @details this function flips given axis (x or y) anchored to the anchor value.
#'
#' @description Some eyetrackers output its data with inversed values. E.g. Eyelink returns data with
#' 0,0 in the left top corner, but visualisations make more sense with 0,0 being projected to the
#' left bottom corner. So we need to "flip" the Y axis. But we also need to define the new "anchor".
#' In our case, if we want the current 0,0 to become left top corner, we want to reanchor the "y" axis with 0
#' to be at current height (e.g. 1080).
#'
#' @param obj \code{\link{EyerObject}}
#' @param axis string of which axis to flip. c("x", "y")
#' @param anchor what is the value of new 0? Needs to be defined
#' @return
#' @export
#'
#' @examples
flip_axis <- function(obj, axis, anchor) {
  if (!is.eyer(obj)) {
    warning("passed object is not eyer")
    return(NULL)
  }
  if (!(axis %in% c("y", "x"))) {
    warning("can only flip x and y axes")
    return(NULL)
  }
  for (position_data in EYE_POSITION_DATA_FIELDS) {
    df <- obj$data[[position_data]]
    if (is.null(df)) next
    df[[axis]] <- anchor - df[[axis]]
    obj$data[[position_data]] <- df
  }
  return(obj)
}

#' Downsamples data
#'
#' @param obj object to downsample
#' @param n picks every nth recording
#' @param ...
#'
#' @return downsampled object
#' @export
#'
#' @examples
downsample <- function(obj, n, ...) {
  UseMethod("downsample")
}

#' Downsamples eyer gaze data
#'
#' @param obj \code{\link{EyerObject}}
#' @param n picks every nth recording
#' @param ...
#'
#' @return EyerObject with downsampled gaze data
#' @export
#'
#' @examples
downsample.eyer <- function(obj, n, ...) {
  obj$data$gaze <- downsample.data.frame(obj$data$gaze, n)
  return(obj)
}

#' @export
downsample.data.frame <- function(df, n) {
  if (nrow(df) < 1) {
    return(df)
  }
  df <- df[seq(1, nrow(df), n), ]
  return(df)
}

#' Add area column to object data
#'
#' @details Adds new column to the fixations and gaze data frame from the area
#' @description The newly added column has a string with the name of the area, or NA_character_
#'
#' @param obj \code{\link{EyerObject}}
#' @param areas list of \code{\link{AreaObject}}. Needs to be a list with area objects inside, otherwise it won't parse well
#'
#' @return modified \code{\link{EyerObject}} or the object back if something doesn't work
#' @export
#'
#' @examples
add_area_column <- function(obj, areas) {
  if (!is.eyer(obj)) {
    warning("passed object is not eyer")
    return(obj)
  }
  for (eye_data_field in EYE_POSITION_DATA_FIELDS) {
    df <- obj$data[[eye_data_field]]
    if (is.null(df)) next
    df$area <- NA_character_
    for (area in areas) {
      if (!is_valid_area(area)) next
      in_area <- is_in_area(df$x, df$y, area)
      df$area[in_area] <- area$name
    }
    obj$data[[eye_data_field]] <- df
  }
  return(obj)
}

#' Recalibrates eyetracking data
#'
#' @details AS the eyetracking data may get shifted towards certain point,
#'
#' @param obj \code{\link{EyerObject}}
#' @param new_zero numeric(2) vector in which to shift the data gets subtracted from the x and y axis
#' @param times numeric(2) vector to define which part of the data to select
#' @param raw_times commonly, the eyer data times are 0 based with the starting time being saved in `info$start_time`.
#' Most operations are then calculated using these 0 based timings. If raw_times is set to TRUE,
#' data are filtered with acknowledging obj$info$start_time.
#' @return Eyer object with preprocessed data fields
#' @export
#'
#' @examples
recalibrate_eye_data <- function(obj, new_zero, times = c(), raw_times = FALSE) {
  # validate new zero
  # validate times
  if (raw_times) times <- times - obj$info$start_time
  for (eye_data_field in EYE_POSITION_DATA_FIELDS) {
    df <- obj$data[[eye_data_field]]
    if (is.null(df)) next
    if (length(times) == 0) {
      selected <- TRUE
    } else {
      selected <- df$time >= times[1] & df$time < times[2]
    }
    df$x[selected] <- df$x[selected] - new_zero[1]
    df$y[selected] <- df$y[selected] - new_zero[2]
    obj$data[[eye_data_field]] <- df
  }
  return(obj)
}


#' Allows marking of eye data
#'
#' @details Adds a new column of mark- allows adding erroneous recordings etc.
#'
#' @param obj \code{\link{EyerObject}}
#' @param mark string with the name of the mark
#' @param times numeric(2) vector to define which part of the data to select
#' @param raw_times commonly, the eyer data times are 0 based with the starting time being saved in `info$start_time`.
#' Most operations are then calculated using these 0 based timings. If raw_times is set to TRUE,
#' data are filtered with acknowledging obj$info$start_time.
#'
#' @return Eyer object with preprocessed data fields
#'
#' @export
#'
#' @examples
mark_eye_data <- function(obj, mark, times = c(), raw_times = FALSE) {
  if (raw_times) times <- times - obj$info$start_time
  for (eye_data_field in EYE_POSITION_DATA_FIELDS) {
    df <- obj$data[[eye_data_field]]
    if (is.null(df)) next
    if (!("mark" %in% colnames(df))) df$mark <- ""
    selected <- df$time >= times[1] & df$time < times[2]
    df$mark[selected] <- mark
    obj$data[[eye_data_field]] <- df
  }
  return(obj)
}
