% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_degree.R
\name{get_degree}
\alias{get_degree}
\title{Calculates the degree or neighborhood density of a word based on Levenshtein edit distance of 1}
\usage{
get_degree(stimuli, database)
}
\arguments{
\item{stimuli}{A character vector containing words or nonwords.}

\item{database}{A dataframe that must contain a \code{Phono} column that contains phonological transcriptions.}
}
\value{
A dataframe with \code{stimuli} and corresponding degree.
}
\description{
Calculates the degree or neighborhood density of a word based on Levenshtein edit distance of 1
}
\examples{
# load a database first
# get_degree(stimuli = c('xbet', 'gEt', 'hWs', 'xgEnst'), database = data)

}
