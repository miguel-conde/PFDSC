#' Function getDataFiles()
#' 
#' @param dataDirectory string; where to store the downloaded file
#' @param zipFile       string; name of the zip file to download
#' @param myURL         string; url to download the zipped file from

getDataFiles <- function (dataDirectory, zipFile, myURL) {
  if(!file.exists(dataDirectory)) {
    dir.create(dataDirectory)
  }
  
  if (!file.exists(zipFile)) {
    download.file(url = myURL, destfile = zipFile, method = "auto")
  }
  
  unzip(zipfile = zipFile, overwrite = FALSE, exdir = dataDirectory)
  unzip(zipfile = zipFile, list = TRUE)

}

