#' Loads data from a folder. Loads preprocessed events, gaze and fixations files
#'
#' @description eyer allows saving of preprocessed data with the function `eyer_save()`
#' this function then allows to load directly those prepared files into its structure. This function
#' returns NULL if there is no [name]_eyer_info.json file found in the directory.
#'
#' @param folder what directory should be searcher? Not recursive
#' @param name name of the files to find in case there are multiple saved eyer objects in the folder
#'
#' @return EyerObject with filled in data or NULL if no files are found
#' @export
#'
#' @examples
load_eyer_data <- function(folder, name = "", ...) {
  obj <- EyerObject()
  # checks if there are already computed files
  ls_filepaths <- find_preprocessed_files(folder, name)
  # if (override) delete_preprocessed_files(ls_filepaths)
  if (is.null(ls_filepaths$info)) {
    warning("There are no info files of name ", name, " in folder", folder, "Returning NULL")
    return(NULL)
  }
  obj <- load_preprocessed_files(obj, ls_filepaths)
  return(obj)
}

#' returns filepaths of preprocessed fixations and events
#'
#' @param folder what directory to search in? Expects one fixation and one event to be present
#'
#' @return list with "fixations" and "events" fileds containing filepaths
#' @examples
find_preprocessed_files <- function(folder, name = "") {
  # actually search for it
  ls <- list()
  for (field in ALL_DATA_FIELDS) {
    preprocessed_ptr <- paste0(name, "_eyer_", field)
    ls[[field]] <- find_file_or_null(folder, preprocessed_ptr)
  }
  info_ptr <- paste0(name, "_eyer_info.json")
  ls$info <- find_file_or_null(folder, info_ptr)
  return(ls)
}

#' @param folder where to look
#' @param ptr pattern to search for
#' @noRd
find_file_or_null <- function(folder, ptr, allow_multiples = FALSE) {
  ptr <- paste("*", ptr, sep = "")
  filepath <- list.files(folder, pattern = ptr, full.names = TRUE)
  if (length(filepath) == 0) {
    return(NULL)
  }
  if (length(filepath) == 1) {
    return(filepath)
  }
  if (allow_multiples) {
    return(filepath)
  } else {
    return(NULL)
  }
}

load_preprocessed_files <- function(obj, ls_filepaths) {
  for (field in ALL_DATA_FIELDS) {
    if (!is.null(ls_filepaths[[field]])) {
      message("Loading preprocessed ", field, " log at ", ls_filepaths[[field]])
      obj$data[[field]] <- read.table(ls_filepaths[[field]], sep = ";", header = TRUE)
    }
  }
  if (!is.null(ls_filepaths$info)) {
    message("Loading preprocessed info log ", ls_filepaths$gaze)
    obj$info <- jsonlite::read_json(ls_filepaths$info, simplifyVector = TRUE)
  }
  return(obj)
}

delete_preprocessed_files <- function(ls_filepaths) {
  for (field in ALL_DATA_FIELDS) {
    if (!is.null(ls_filepaths[[field]]) && file.exists(ls_filepaths[[field]])) {
      message("Removing preprocessed ", field, " log ", ls_filepaths[[field]])
      file.remove(ls_filepaths[[field]])
    }
  }
}
