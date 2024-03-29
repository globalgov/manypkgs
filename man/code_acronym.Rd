% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/code_agreements.R
\name{code_acronym}
\alias{code_acronym}
\title{Code Acronym for Titles}
\usage{
code_acronym(title)
}
\arguments{
\item{title}{A character vector of treaty title}
}
\description{
Codes an acronym for agreement titles to facilitate comparison.
}
\details{
Codes acronyms that are 4 to 6 digits long.
For shorter treaty titles, six words or less, acronym includes first letter
of each word.
For longer treaty titles, seven words or more, acronym includes first letter
of first word in title, followed by the number of words in the title,
and first letter of last word in title.
}
\examples{
\dontrun{
IEADB <- dplyr::slice_sample(manyenviron::agreements$IEADB, n = 10)
code_acronym(IEADB$Title)
}
}
