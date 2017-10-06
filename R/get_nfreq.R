#' Calculates the neighborhood frequency of a word based on Levenshtein edit distance of 1
#'
#' @param stimuli A character vector containing words or nonwords to calculate the degree of.
#' @param database A dataframe that must contain a \code{Phono} column that contains phonological transcriptions and a \code{Frequency} column with log frequencies.
#' @return A dataframe with \code{stimuli} and corresponding neighborhood frequency (average frequency of its neighbors).
#' @examples
#' # load a database first
#' # get_nfreq(stimuli = c('xbet', 'gEt', 'hWs', 'xgEnst'), database = data)
#'
get_nfreq <- function(stimuli, database) {
  #stimuli = character vector of words to calculate degree
  #database = corpus

  #check that stimuli is a character vector
  if (class(stimuli) != 'character' || is.vector(stimuli) == F) {
    stop('Warning! Stimuli is either not of character type or is not a vector.')
  }

  #check that the database is correctly input
  if (is.data.frame(database) == F) {
    stop('Warning! Database is not a dataframe type.')
  }

  if ('Phono' %in% colnames(database) == F || 'Frequency' %in% colnames(database) == F) {
    stop('Warning! Data does not contain a "Phono" or "Frequency" column.')
  }

  #initialize a data frame to save data to
  output <- as.data.frame(matrix(0, ncol = 2, nrow = length(stimuli)))
  colnames(output) <- c('Stimuli', 'NeighborFreq')

  for (i in 1:length(stimuli)) {
    # save word to output
    output$Stimuli[i] <- stimuli[i]
    # save degree to output
    temp <- as.data.frame(levenshtein.neighbors(stimuli[i], data$Phono)[[1]])
    colnames(temp) <- 'Phono'
    temp <- suppressWarnings(temp %>% dplyr::left_join(data, by = 'Phono'))
    output$NeighborFreq[i] <- mean(temp$Frequency)
  }

  return(output)

}

# test pieces

#get_nfreq(stimuli = 'xbet', database = data)

#get_nfreq(stimuli = c('xbet', 'gEt', 'hWs', 'xgEnst'), database = data)
