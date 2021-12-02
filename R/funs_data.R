
# Handle OSF data ---------------------------------------------------------

#' Download data from OSF
#'
#' Download a data file from the OSF Storage of a project and place it in a
#' specified folder. There's no need to specify a filename;
#' `osfr::osf_download()` automatically uses the name of the file from OSF (e.g.
#' it will download [https://osf.io/y36f8/](https://osf.io/y36f8/) and name it
#' `intercept.rds`).
#'
#' @param osf_id alphanumeric code for the file to be downloaded; comes from the
#'   URL of the file (e.g., for [https://osf.io/y36f8/](https://osf.io/y36f8/),
#'   the id is `y36f8`)
#' @param out_dir path to a folder where the file should be saved
#'
#' @return The path of the downloaded file.
#' @export
#' @md
#'
#' @examples
#' \dontrun{
#' get_from_osf(osf_id = y36f8,
#'              out_dir = here::here("data", "raw_data", "posterior_draws"))
#' }
get_from_osf <- function(osf_id, out_dir) {
  library(osfr)

  get_osf <- osf_retrieve_file(osf_id) %>%
    osf_download(path = out_dir,
                 conflicts = "overwrite",
                 progress = TRUE)

  return(get_osf$local_path)
}

# Static branching magic!
# Create a dataframe of filenames and their corresponding OSF ids. Technically
# the filenames aren't necessary, since `osf_files()` will automatically name
# the downloaded files with their correct names, but adding the names here lets
# us use them as {targets} names in the pipeline
osf_files <- tribble(
  ~name, ~osf_id,
  "charity_voluntarism", "6zmfd",
  "demographics", "q7gjv",
  "intercept", "y36f8",
  "political_ideology", "6hk3y",
  "public_affairs", "hyr5x",
  "public_political_social_charity_demo", "yzmfw",
  "public_political_social_charity", "8pb53",
  "public_political_social", "dxr2s",
  "public_political", "q6f78",
  "social_views", "45jvs"
)

# Create targets for each of the files in the `osf_files` dataset. Make sure
# that `osf_file_targets` is included in the main {targets} pipeline list.
osf_file_targets <- tar_map(
  values = osf_files,
  names = name,
  tar_target(
    file,
    get_from_osf(osf_id = osf_id,
                 out_dir = here_rel("data", "raw_data", "posterior_draws")),
    format = "file"
  )
)
