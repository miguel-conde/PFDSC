
#'
#' Function statsNWordGrams()
#' 
#' This function calculates N-WordGrams statistics from Term-Document Matrixes
#' 
#' @param tdm
#' @param inFile string; the Corpus file whose data must be used to calculate statistics
#' If inFile = "TOTAL" all Corpus statistics are calculated
#' @param count numeric; count number beyond which a term is considered 
#' Frequent Term
#' @param seq numeric vector; vector of probabilities with values to calculate
#' quantiles
#' @param freq logical; if TRUE, use each term count; if FALSE, use the % of
#' counts over the total document length (tokens)
#' @return
#' 
#' @details
#' 
statsNWordGrams <- function(tdm, inFile = "TOTAL", 
                            count = 50, s = seq(0.5,1,0.01), freq = TRUE)
{
  m_tdm <- as.matrix(tdm)
  
  if (inFile == "TOTAL") 
    m_tdm <- cbind(m_tdm, TOTAL = apply(m_tdm,1,sum))
  else 
    m_tdm <- m_tdm[m_tdm[ , inFile] > 0, ]
  
  tokens  <- sum(m_tdm[,inFile])
  types   <- dim(m_tdm)[1]
  
  if (inFile == "TOTAL")
    freqTerms <- findFreqTerms(tdm, count)
  else
    freqTerms <- findFreqTerms(tdm[, inFile], count)
  
  coverageFreqTerms <- tail(cumsum(m_tdm[freqTerms, inFile]) / tokens, 1)
  percentFreqTerms <- sum(m_tdm[ , inFile] >= count) / sum(m_tdm[ , inFile] > 0)
  
  tokensFT  <- sum(m_tdm[freqTerms, inFile])
  typesFT   <- length(m_tdm[freqTerms, inFile])
  
  # Density is the count of each term over the total length (number of tokens)
  # of the document
  if (freq == FALSE)
    # See http://stackoverflow.com/questions/20596433/how-to-divide-each-row-of-a-matrix-by-elements-of-a-vector-in-r
    m_tdm <- t(t(m_tdm) / apply(m_tdm,2,sum))
  
  q <- quantile(m_tdm[, inFile], probs = s)
  
  mnFreq    <- mean(m_tdm[ , inFile])
  varFreq   <- var(m_tdm[ , inFile])
  sdFreq    <- sd(m_tdm[ , inFile])
  seFreq    <- sdFreq/sqrt(types)
  mnFreqFT  <- mean(m_tdm[freqTerms, inFile])
  varFreqFT <- var(m_tdm[freqTerms, inFile])
  sdFreqFT  <- sd(m_tdm[freqTerms, inFile])
  seFreqFT  <- sdFreqFT/sqrt(typesFT)
  
  results <- paste0("\n", 
                    paste(toupper(inFile),"term counts statistics:\n\n"),
                    "Centiles:\n")
  
  writeLines(results)
  
  print(q)
  
  if (freq == TRUE)
    results <- paste0("\n",
                      "All Terms (frequencies):\n",
                      sprintf("  Number of Types:              %d\n", types),
                      sprintf("  Number of Tokens:             %d\n", tokens),
                      sprintf("  Tokens / Types:               %02.2f\n", tokens/types),
                      sprintf("  Frequency Mean:               %02.2f\n", mnFreq),
                      sprintf("  Frequency Variance:           %02.2f\n", varFreq),
                      sprintf("  Frequency Standard Deviation: %02.2f\n", sdFreq),
                      sprintf("  Frequency Standard Error:     %02.2f\n\n", seFreq),
                      sprintf("Frequent Terms (count >= %d):\n", count),
                      sprintf("  Number of Types:              %d\n", typesFT),
                      sprintf("  Number of Tokens:             %d\n", tokensFT),
                      sprintf("  Tokens / Types:               %02.2f\n", tokensFT/typesFT),
                      sprintf("  Frequency Mean:               %02.2f\n", mnFreqFT),
                      sprintf("  Frequency Variance:           %02.2f\n", varFreqFT),
                      sprintf("  Frequency Standard Deviation: %02.2f\n", sdFreqFT),
                      sprintf("  Frequency Standard Error:     %02.2f\n\n", seFreqFT),
                      sprintf("  Frequent Terms Percentage:    %02.2f %%\n", 
                              100*percentFreqTerms),
                      sprintf("  Frequent Terms Coverage:      %02.2f %%\n\n", 
                              100*coverageFreqTerms),
                      "--------------------\n\n")
  else
    results <- paste0("\n",
                      "All Terms (densities):\n",
                      sprintf("  Number of Types:              %d\n", types),
                      sprintf("  Number of Tokens:             %d\n", tokens),
                      sprintf("  Tokens / Types:               %02.2f\n", tokens/types),
                      sprintf("  Frequency Mean:               %e\n", mnFreq),
                      sprintf("  Frequency Variance:           %e\n", varFreq),
                      sprintf("  Frequency Standard Deviation: %e\n", sdFreq),
                      sprintf("  Frequency Standard Error:     %e\n\n", seFreq),
                      sprintf("Frequent Terms (count >= %d):\n", count),
                      sprintf("  Number of Types:              %d\n", typesFT),
                      sprintf("  Number of Tokens:             %d\n", tokensFT),
                      sprintf("  Tokens / Types:               %02.2f\n", tokensFT/typesFT),
                      sprintf("  Frequency Mean:               %e\n", mnFreqFT),
                      sprintf("  Frequency Variance:           %e\n", varFreqFT),
                      sprintf("  Frequency Standard Deviation: %e\n", sdFreqFT),
                      sprintf("  Frequency Standard Error:     %e\n\n", seFreqFT),
                      sprintf("  Frequent Terms Percentage:    %02.2f %%\n", 
                              100*percentFreqTerms),
                      sprintf("  Frequent Terms Coverage:      %02.2f %%\n\n", 
                              100*coverageFreqTerms),
                      "--------------------\n\n")
  
  writeLines(results)
}

