% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/code_lineage.R
\name{code_entity}
\alias{code_entity}
\title{Code Agreement Entity}
\usage{
code_entity(title)
}
\arguments{
\item{title}{Treaty titles}
}
\value{
The region of the agreement
}
\description{
Code Agreement Entity
}
\examples{
\dontrun{
title <- sample(manyenviron::agreements$IEADB$Title, 30)
code_entity(title)
}
}
