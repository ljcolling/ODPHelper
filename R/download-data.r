######################################################################
##  Functions for downloading the data for each of the four papers  ##
######################################################################

# List the file / node information

node_file_info <- list(
  geniole = list(
    node = "3jhr7",
    output_folder = "data"
  ),
  yousif = list(
    node = "a54dg",
    output_folder = "data"
  ),
  obaidi = list(
    node = "zny2m",
    output_folder = "data"
  ),
  hilgard = list(
    repo = "https://github.com/Joe-Hilgard/vvg-2d4d/archive/master.zip",
    output_folder = "data"
  )
)

download_osf <- function(node, output_folder) {
  node_object <- osfr::osf_retrieve_node(node)
  node_files <- osfr::osf_ls_files(node_object)
  output_folder <- file.path(".", output_folder)
  dir.create(output_folder)
  osfr::osf_download(x = node_files, path = output_folder)
  hashes <- tools::md5sum(list.files(path = output_folder, pattern = ".",full.names = T))
  return(list(node = node, files = node_files$name, hashes = hashes))
}

download_geniole <- purrr::partial(download_osf,
  node = node_file_info$geniole$node,
  output_folder = node_file_info$geniole$output_folder
)

download_yousif <- purrr::partial(download_osf,
  node = node_file_info$yousif$node,
  output_folder = node_file_info$yousif$output_folder
)


download_obaidi <- purrr::partial(download_osf,
  node = node_file_info$obaidi$node,
  output_folder = node_file_info$obaidi$output_folder
)

download_hilgard <- function() {
  hilgard <- node_file_info$hilgard$repo
  output_folder <- node_file_info$hilgard$output_folder
  download.file(hilgard, destfile = file.path(output_folder, "master.zip"))
  hashes <- tools::md5sum(file.path(output_folder, "master.zip"))
  unzip(
    zipfile = file.path(".", output_folder, "master.zip"),
    exdir = file.path(".", output_folder)
  )
  # remove unnecessary files
  file.remove(file.path(output_folder, "master.zip"))

  # files that need to be built
  built_files <- c(
    "aggressed-condition_hist.png", "AggressionTable.txt",
    "clean_data.txt", "Difficulty-EFA_hist.png", "Figure1.png",
    "Figure2a.png", "Figure2b.png", "full_data.RData", "full_data.txt",
    "Gamevars.txt", "Results.docx", "Rplots.pdf", "session_info.txt",
    "SuppFigure1.png", "Supplement.pdf", "violence-condition_hist.png"
  )

  purrr::map(built_files, function(x) file.remove(file.path(output_folder, "vvg-2d4d-master", x)))

  return(list(
    node = stringr::str_remove(node_file_info$hilgard$repo,"/archive/master.zip"),
    files = node_file_info$hilgard$repo,
    hashes = hashes))

}



