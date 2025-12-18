#' Analyses time spent in each area
#'
#' @details returns summative data froma with columns: area, n,
#' duration, duration.na, ratio
#' @description the totals int he data frame do not correspond to
#' the total row, as the NA data are omited.
#'
#' @param obj \code{\link{EyerObject}}
#' @param areas list of \code{\link{AreaObject}}. Only necessary if
#' the areas haven\'t been added previously
#' with the \code{\link{add_area_column}}. If areas are present, 
#' the column is recalculated.
#'
#' @return data.frame with calculated information
#' @export
#'
#' @examples
analyse_fixation_areas <- function(obj, areas = list()) {
  if (length(areas) != 0) obj <- add_area_column(obj, areas)
  fixations <- obj$data$fixations
  areas <- unique(fixations$area[!is.na(fixations$area)])
  total <- sum(fixations$duration, na.rm=TRUE)
  df <- data.frame(area = "total",
                   n = nrow(fixations),
                   duration = total,
                   duration.na = sum(is.na(fixations$duration)),
                   ratio = 1,
                   stringsAsFactors = FALSE)
  for (area in areas) {
    ls <- list(area = area)
    area_fixations <- fixations[fixations$area == area & !is.na(fixations$area), ]
    ls$n <- nrow(area_fixations)
    ls$duration <- sum(area_fixations$duration, na.rm = TRUE)
    ls$duration.na <- sum(is.na(area_fixations$duration))
    ls$ratio <- ls$duration/total
    df <- rbind(df, ls)
  }
  return(df)
}

#' Analyses number of gaze points in each area
#'
#' @details returns summative data froma with columns: area, n, ratio
#' @description the totals int he data frame do not correspond to the total row, as the NA data are omited.
#'
#' @param obj \code{\link{EyerObject}}
#' @param areas list of \code{\link{AreaObject}}. Only necessary if the areas haven\'t been added previously
#' with the \code{\link{add_area_column}}. If areas are present, the column is recalculated.
#'
#' @return data.frame with calculated information
#' @export
#'
#' @examples
analyse_gaze_areas <- function(obj, areas = list()) {
  if(length(areas) != 0) obj <- add_area_column(obj, areas)
  gaze <- obj$data$gaze
  areas <- unique(gaze$area[!is.na(gaze$area)])
  total <-  nrow(gaze)
  df <- data.frame(area = "total",
                   n = total,
                   ratio = 1,
                   stringsAsFactors = FALSE)
  for(area in areas){
    area_gaze <- gaze[gaze$area == area & !is.na(gaze$area), ]
    ls <- list(area = area,
               n = nrow(area_gaze),
               ratio = nrow(area_gaze)/total)
    df <- rbind(df, ls)
  }
  return(df)
}
