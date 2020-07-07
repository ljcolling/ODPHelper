######################################################################
##  Functions for downloading the data for each of the four papers  ##
######################################################################

# List the file / node information

node_file_info <- list(
  geniole = list(
    node = "3jhr7",
    output_folder = "geniole_data"
  ),
  yousif = list(
    node = "a54dg",
    output_folder = "yousif_data"
  ),
  obaidi = list(
    node = "zny2m",
    output_folder = "obaidi_data"
  ),
  hilgard = list(
    repo = "https://github.com/Joe-Hilgard/vvg-2d4d/archive/master.zip",
    output_folder = "hilgard_data"
  )
)

download_osf <- function(node, output_folder) {
  requireNamespace(osfr)
  node_object <- osfr::osf_retrieve_node(node)
  node_files <- osfr::osf_ls_files(node_object)
  osfr::osf_download(x = node_files, path = file.path(".", output_folder))
  return(list(node = node, files = node_files$name))
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
  utils::download.file(hilgard, destfile = file.path(".", output_folder, "master.zip"))
  unzip(
    zipfile = file.path(".", output_folder, "master.zip"),
    exdir = file.path(".", output_folder)
  )
  # remove unnecessary files
  file.remove(file.path(".", output_folder, "master.zip"))

  # files that need to be built
  built_files <- c(
    "aggressed-condition_hist.png", "AggressionTable.txt",
    "clean_data.txt", "Difficulty-EFA_hist.png", "Figure1.png",
    "Figure2a.png", "Figure2b.png", "full_data.RData", "full_data.txt",
    "Gamevars.txt", "Results.docx", "Rplots.pdf", "session_info.txt",
    "SuppFigure1.png", "Supplement.pdf", "violence-condition_hist.png"
  )

  map(built_files, function(x) file.remove(file.path(".", output_folder, "vvg-2d4d-master", x)))
}



