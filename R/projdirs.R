#' Create Default Folder Structure for a New Project
#'
#' @param projectname name of the new project
#' @keywords project workflow
#'
projdirs <- function(projectname){
  curr_warn_option <- getOption('warn')
  options(warn=-1)
  folders <- c("analysis", "data", "feather", "graphics", "metadata", "queries", "R", "reports", "requirements")
  dp <- dff(dirpath = folders, dirpath2 = NA)
  wd <- getwd()
  for (i in 1:length(folders)){
    dp$dirpath2[i] <- fp(wd, folders[i])
  }
  
  # Handle case of existing folder
  existing_paths <- dff(dirpath = list.dirs(path = here::here(), 
                                          full.names = TRUE, recursive = TRUE))
  
  # drop non-visible paths
  droprows <- grep("Rproj", existing_paths$dirpath)
  if (length(droprows) > 0) existing_paths <- dff(dirpath = existing_paths[-droprows, ])
  if (nrow(existing_paths) > 0) {
    existing_paths$existing <- 1
    dp <- left_join(dp, existing_paths, by = "dirpath")
    dp <- dp %>% 
      filter(is.na(existing))
  }
  curr_warn_option <- getOption('warn')
  
    if (nrow(dp) > 0) for (i in 1:nrow(dp)){
    dir.create(dp$dirpath[i])
    }
  options(warn = curr_warn_option)
}

