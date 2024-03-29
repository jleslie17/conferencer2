% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AddUnderscores.R
\name{AddUnderscores}
\alias{AddUnderscores}
\title{Put underscores between words in a data frame.}
\usage{
AddUnderscores(df)
}
\arguments{
\item{df}{A data frame.}
}
\value{
A dataframe with underscores between word and spaces after commas.
}
\description{
This function is used to insert underscores between words in a data frame. It
is used primarily to handle columns that respresent responses to survey
questions. For entries that have several words separated by commas, as in
the case of a "check all that apply" otpion, this will maintain the space
behind the comma.
}
\examples{
df <- data.frame(full_name = c('John Doe', 'Mary Jane'),
                 activities = c('martial arts, hockey', 'horseback riding'),
                 zip = c(19610, 23302))

AddUnderscores(df)

}
