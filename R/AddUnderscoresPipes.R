#' Put underscores between words in a data frame when separated by pipes.
#'
#' This function is used to insert underscores between words in a data frame. It
#' is used primarily to handle columns that respresent responses to survey
#' questions. This function is specific for columns in which multiple responses
#' are separated by the pipe character.
#'
#' @param df A data frame.
#'
#' @return A dataframe with underscores between word and spaces after commas.
#'
#' @examples
#' df <- data.frame(full_name = c('John Doe', 'Mary Jane'),
#'                  activities = c('martial arts | hockey', 'horseback riding'),
#'                  zip = c(19610, 23302))
#' AddUnderscoresPipes(df)
#'
#' @export
AddUnderscoresPipes <- function(df){
        for(i in names(df)){
                df[,i] <- gsub(" ", "_", df[,i])
                df[,i] <- gsub("_\\|_", ", ", df[,i])
        }
        return(df)
}

