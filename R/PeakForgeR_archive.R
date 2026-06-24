#' PeakForgeR_archive.R
#'
#' File archiving functions for moving raw files and processed data
#' to archive directories after processing is complete.

#.----
#Archive Raw Files ----

###Primary Function----
#' Archive Raw Files
#'
#' This function moves raw files (wiff and mzML) to an archive directory after processing is complete.
#' Note: \code{msConvert_mzml_output} is intentionally not archived; it is retained in place for downstream use.
#' \code{MStargetR_logs} is also intentionally \emph{not} archived: the per-plate
#' msConvert/Docker logs written there must remain in place so the later
#' qcCheckR console logs are written alongside them in the same
#' \code{MStargetR_logs} folder, rather than being split off into \code{archive/}.
#'
#' @param project_directory Path to the directory for the project parsed from PeakForgeR.
#' @return None. The function performs the archive operation and a message upon successful completion.
#' @keywords internal
#' @examples
#' \dontrun{
#' archive_raw_files("path/to/project_directory")
#' }
archive_raw_files <- function(project_directory) {
  project_directory <- validate_project_directory(project_directory)
  # MStargetR_logs is deliberately left in place (not archived) so the
  # per-plate logs stay co-located with the qcCheckR logs written later.
  archive_files(project_directory, "raw_data")
  #archive_files(project_directory, "msConvert_mzml_output")
  message("\n PeakForgeR has finished running all plates.")
}

###Sub Functions----

#' Archive Files
#'
#' This function moves a specified folder to the archive directory within the project directory.
#' @keywords internal
#' @param project_directory The directory of the project.
#' @param folder_name The name of the folder to be archived.
#' @return invisible(NULL)
#' @examples
#' \dontrun{
#' archive_files(project_directory, folder_name)
#' }
archive_files <- function(project_directory, folder_name) {
  message("Archiving folder: ", folder_name, " to ",
          file.path(project_directory, "archive"))
  move_folder(
    file.path(project_directory, folder_name),
    file.path(project_directory, "archive")
  )
}

#Move Folder Functions----

###Primary Function----
#' move_folder
#' This function moves a folder from the source directory to the destination directory, waits until files are not in use, and then deletes the source directory.
#' @keywords internal
#' @param source_dir Directory path for the folder to copy.
#' @param dest_dir Directory path for the folder to be copied to.
#' @param max_wait Maximum wait time in seconds before giving up on a locked file.
#'   The function stops as soon as *either* `max_wait` seconds have elapsed *or*
#'   `max_retries` attempts have been made (whichever comes first).
#'   With the default 0.5 s sleep, `max_retries = 30` trips at ~15 s.
#' @param max_retries Maximum number of retry attempts per file before giving up. Default 30.
#' @return None. The function performs the move operation and a message upon successful completion.
#' @examples
#' \dontrun{
#' move_folder(source_dir = "path/to/source",
#'             dest_dir = "path/to/destination",
#'             max_wait = 60,
#'             max_retries = 30)
#' }
move_folder <- function(source_dir,
                        dest_dir,
                        max_wait = 60,
                        max_retries = 30) {
  message("Moving folder: ", source_dir, " -> ", dest_dir)
  dirs_valid <- validate_directories(source_dir, dest_dir)
  if (isFALSE(dirs_valid)) {
    return(invisible(FALSE))
  }
  files_to_copy <- copy_files(source_dir, dest_dir)
  if (length(files_to_copy) == 0) {
    message("No files to move in: ", source_dir)
    return(invisible(TRUE))
  }
  wait_until_files_free(files_to_copy, max_wait, max_retries)
  delete_source_directory(source_dir)
  message("Folder move complete: ", source_dir)
  return(TRUE)
}

###Sub Functions----

#' validate_directories
#'
#' This function validates the existence of the source directory and creates the destination directory if it does not exist.
#' @keywords internal
#' @param source_dir Directory path for the folder to copy.
#' @param dest_dir Directory path for the folder to be copied to.
#' @return None. The function performs directory validation.
#' @examples
#' \dontrun{
#' validate_directories(source_dir = "path/to/source", dest_dir = "path/to/destination")
#' }
validate_directories <- function(source_dir, dest_dir) {
  if (!dir.exists(source_dir)) {
    message("Source directory does not exist: ", source_dir, " - skipping move.")
    return(invisible(FALSE))
  }
  if (!dir.exists(dest_dir)) {
    dir.create(dest_dir, recursive = TRUE)
  }
  invisible(TRUE)
}


#' copy_files
#' This function copies files from the source directory to the destination directory.
#'
#' Uses \code{overwrite = TRUE} so re-archiving into an existing destination
#' (e.g. a re-run of the same project) succeeds. Transient failures (common
#' on OneDrive / network drives where the destination holds a brief sync lock)
#' are retried with a short backoff before stopping.
#' @keywords internal
#' @param source_dir Directory path for the folder to copy.
#' @param dest_dir Directory path for the folder to be copied to.
#' @param max_retries Integer number of attempts per file. Default 3.
#' @param retry_delay Seconds to sleep between attempts. Default 0.5.
#' @return A vector of file paths that were copied.
#' @examples
#' \dontrun{
#' copy_files(source_dir = "path/to/source", dest_dir = "path/to/destination")
#' }
copy_files <- function(source_dir, dest_dir, max_retries = 3, retry_delay = 0.5) {
  files_to_copy <- list.files(source_dir, full.names = TRUE)

  if (length(files_to_copy) == 0) {
    return(character(0))
  }

  if (!dir.exists(dest_dir)) {
    stop("Destination directory does not exist: ", dest_dir,
         call. = FALSE)
  }

  dest_paths <- file.path(dest_dir, basename(files_to_copy))

  # overwrite = TRUE so re-runs can re-archive into an existing archive dir
  # without each pre-existing file causing file.copy to return FALSE.
  # Retry transient failures (common on OneDrive / network drives where
  # the destination briefly holds a sync placeholder lock).
  success <- file.copy(files_to_copy, dest_paths, overwrite = TRUE)
  attempt <- 1L
  while (!all(success) && attempt < max_retries) {
    failed_idx <- which(!success)
    Sys.sleep(retry_delay)
    retry_success <- file.copy(files_to_copy[failed_idx],
                               dest_paths[failed_idx],
                               overwrite = TRUE)
    success[failed_idx] <- retry_success
    attempt <- attempt + 1L
  }

  if (!all(success)) {
    failed_files <- files_to_copy[!success]
    stop("Failed to copy ", length(failed_files), " file(s) to ", dest_dir,
         " after ", max_retries, " attempt(s):\n",
         paste("  -", basename(failed_files), collapse = "\n"),
         call. = FALSE)
  }
  return(files_to_copy)
}

#' wait_until_files_free
#' This function waits until files are not in use by attempting to rename them.
#' @keywords internal
#' @param files_to_copy A vector of file paths to check.
#' @param max_wait Maximum wait time (in seconds) for the system prior to deleting the moved folder. Default is 60
#' @param max_retries Maximum number of attempts to try move/delete prior to error. Default is 30.
#' @return None. The function waits until files are free.
#' @examples
#' \dontrun{
#' wait_until_files_free(files_to_copy, max_wait = 60, max_retries = 30)
#' }
wait_until_files_free <- function(files_to_copy,
                                  max_wait = 60,
                                  max_retries = 30) {
  for (test_file in files_to_copy) {
    retries <- 0
    start_time <- Sys.time()
    while (TRUE) {
      if (!file.exists(test_file))
        break

      if (file.access(test_file, mode = 2) == 0) {
        break
      }

      if (as.numeric(Sys.time() - start_time, units = "secs") > max_wait ||
          retries >= max_retries) {
        stop("File still in use after waiting (max_wait=", max_wait, "s, retries=", retries, "): ",
             test_file, call. = FALSE)
      }

      Sys.sleep(0.5)
      retries <- retries + 1
    }
  }
}

#' delete_source_directory
#' This function deletes the source directory after files have been moved.
#' @keywords internal
#' @param source_dir Directory path for the folder to delete.
#' @return None. The function deletes the source directory.
#' @examples
#' \dontrun{
#' delete_source_directory(source_dir = "path/to/source")
#' }
delete_source_directory <- function(source_dir) {
  unlink(source_dir, recursive = TRUE, force = TRUE)
  if (dir.exists(source_dir)) {
    stop("Failed to delete source directory after copying: ", source_dir,
         ". Files were copied successfully but the source directory could not be removed. ",
         "Check file locks and permissions.",
         call. = FALSE)
  } else {
    message("Successfully moved and deleted: ", source_dir)
  }
}
