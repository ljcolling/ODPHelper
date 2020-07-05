######################################################################
##  Functions for downloading the data for each of the four papers  ##
######################################################################


download_osf <- function(node, output_folder) {
  requireNamespace(osfr)
  node_object <- osfr::osf_retrieve_node(node)
  node_files <- osfr::osf_ls_files(node_object)
  osfr::osf_download(x = node_files, path = file.path(".", output_folder))
}


download_geniole <- purrr::partial(download_osf,
  node = "3jhr7",
  output_folder = "geniole"
)

download_yousif <- purrr::partial(download_osf,
  node = "a54dg",
  output_folder = "yousif"
)

download_obaidi <- purrr::partial(download_osf,
  node = "zny2m",
  output_folder = "obaidi"
)

download_hilgard <- function() {
  hilgard <- "https://github.com/Joe-Hilgard/vvg-2d4d/archive/master.zip"
  output_folder <- "hilgard"
  download.file(hilgard, destfile = file.path(".", output_folder, "master.zip"))
  unzip(
    zipfile = file.path(".", output_folder, "master.zip"),
    exdir = file.path(".", output_folder)
  )
  # remove unnecessary files
  file.remove(file.path(".", output_folder, "master.zip"))

  # files that need to be built
  built_files <- c("aggressed-condition_hist.png", "AggressionTable.txt",
                   "clean_data.txt", "Difficulty-EFA_hist.png", "Figure1.png",
                   "Figure2a.png", "Figure2b.png", "full_data.RData", "full_data.txt",
                   "Gamevars.txt", "Results.docx", "Rplots.pdf", "session_info.txt",
                   "SuppFigure1.png", "Supplement.pdf", "violence-condition_hist.png")

  map(built_files,function(x) file.remove(file.path(".", output_folder,"vvg-2d4d-master",x)))

}
