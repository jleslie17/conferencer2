#' Put underscores between words in a data frame.
#'
#' This function is used to insert underscores between words in a data frame. It
#' is used primarily to handle columns that respresent responses to survey
#' questions. For entries that have several words separated by commas, as in
#' the case of a "check all that apply" otpion, this will maintain the space
#' behind the comma.
#'
#' @param df A data frame.
#'
#' @return A dataframe with underscores between word and spaces after commas.
#'
#' @examples
#' df <- data.frame(full_name = c('John Doe', 'Mary Jane'),
#'                  activities = c('martial arts, hockey', 'horseback riding'),
#'                  zip = c(19610, 23302))
#'
#' AddUnderscores(df)
#'
#'@export
AddUnderscores <- function(df){
        for(i in names(df)){
                df[,i] <- gsub(" ", "_", df[,i])
                df[,i] <- gsub(",_", ", ", df[,i])
        }
        return(df)
}

