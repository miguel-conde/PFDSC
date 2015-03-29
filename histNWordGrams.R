#'
#' Function histNWordGrams()
#' 
#' This functions plots an N_c (frequency of frequency counts) histNWordGrams 
#' from the file data in a Term Document Matrix
#' 
#' @param tdm Term Document MAtrix
#' @param inFile string; text file in TDM to use. If inFile = "TOTAL", use all files
#' in TDM
#' @param log logical; select logarithmic Y-scale
#' @param freq logical; if TRUE, use count; if FALSE, use density.
#' @param ... args for 'plot'
#' @param ret select if the function must return the counts og histogram bins
#' @return if 'ret' == TRUE, returns the counts og histogram bins
#' breaks number of breaks in histogram. If no specified, use the number of
#' terms in inFile
#' @example histNWordGrams(tdm_1G, "sample_twitter.txt", freq = FALSE, 
#'                         breaks = 1000, xlim = c(0, 0.003))

histNWordGrams <- function(tdm, inFile = "TOTAL", log = TRUE, 
                           breaks = NULL, ret = FALSE, freq = TRUE, ...) {
  m_tdm <- as.matrix(tdm)
  
  if (inFile == "TOTAL") 
    m_tdm <- cbind(m_tdm, TOTAL = apply(m_tdm,1,sum))
  else 
    m_tdm <- m_tdm[m_tdm[ , inFile] > 0, ]
  
  if (is.null(breaks)) 
    brks <- dim(m_tdm)[1]
  else
    brks <- breaks
  
  # Density is the percentage of each term count over the total number of
  # terms in the vocabulary
  if (freq == FALSE)
    m_tdm <- m_tdm / apply(m_tdm,2,sum)
  
  m_test <- m_tdm[, inFile] > 0
  
  h <- hist(m_tdm[m_test, inFile], plot = FALSE, breaks = brks)
  
  if (log == TRUE)
    h$counts <- log10(h$counts+1)
  
  if (freq == TRUE) {
    tit <- paste0(inFile, " Terms\nFrequency of Frequency Counts (Nc)")
    xlab = paste0(inFile, " Count")
    ylab = "log10(Frequency)"
  }
  else {
    tit <- paste0(inFile, " Terms\nDensity of Frequency Counts (Nc)")
    xlab = paste0(inFile, " %Count")
    ylab = "log10(Density)"
  }

  plot(h, ylab = ylab, xlab = xlab, main = tit, ...)
  
  if (ret == TRUE)
    return(h$counts)
}

#'
#' Function statsHistNWordGrams()
#' 
#' This function calculates statistics about the frequency of frequency counts 
#' in a file from a Term Document Matrix
#' 
#' @param tdm Term Document MAtrix
#' @param inFile string;  file whose statistics are going to be calculated
#' @param freq logical;   if TRUE, the statistics are calculated on 
#' frequencies, the counts component of the result; if FALSE, probability 
#' densities, component density, are used.
#' 
statsHistNWordGrams <- function(tdm, inFile = "TOTAL", freq = TRUE) {
  
  m_tdm <- as.matrix(tdm)
  
  if (inFile == "TOTAL") 
    m_tdm <- cbind(m_tdm, TOTAL = apply(m_tdm,1,sum))
  else 
    m_tdm <- m_tdm[m_tdm[ , inFile] > 0, ]
  
  brks <- dim(m_tdm)[1]
  
  m_test <- m_tdm[, inFile] > 0
  
  h <- hist(m_tdm[m_test, inFile], plot = FALSE, breaks = brks)
  
  if (freq == TRUE)
    counts <- h$counts[h$counts > 0]
  else
    counts <- h$density[h$counts > 0]
  
  meanNc <- mean(counts)
  varNC <- var(counts)
  sdNc <- sd(counts)
  seNc <- sdNc / sqrt(length(counts))
  
  if (freq == TRUE)
    results <- paste0("\n", 
                      paste(toupper(inFile),"Nc statistics:\n\n"),
                      sprintf("\tNc mean:               %02.2f\n", meanNc),
                      sprintf("\tNc variance:           %02.2f\n", varNC),
                      sprintf("\tNc standard deviation: %02.2f\n", sdNc),
                      sprintf("\tNc standard error:     %02.2f\n", seNc),
                      "---------------------------------------\n\n")
  else
    results <- paste0("\n", 
                      paste(toupper(inFile),"Nc statistics:\n\n"),
                      sprintf("\tNc mean:               %e\n", meanNc),
                      sprintf("\tNc variance:           %e\n", varNC),
                      sprintf("\tNc standard deviation: %e\n", sdNc),
                      sprintf("\tNc standard error:     %e\n", seNc),
                      "---------------------------------------\n\n")
  writeLines(results)
  
  return(h$counts)
  
}