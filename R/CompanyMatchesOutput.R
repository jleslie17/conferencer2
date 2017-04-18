#' Puts the matches for each sponsor into a three-column data frame
#'
#' This function is used to put the information about matches into a dataframe
#' with line returns between matches. It is used to produce an output table
#' that can be easily incorporated into the clients email system.
#'
#' @param delegate_data A data frame.
#'
#' @param sponsor_data A data frame. Column 4 contains the name, column 2
#' contains the email address.
#'
#' @param Delegates_to_meet A data frame. Each column corresponds to a sponsor
#' with the matches listed in rows.
#'
#' @return A three-column dataframe. Each row corresponds to a sponsor.
#' Matches appear in the third column and are separated by line returns.
#'
#' @examples
#'
#' @export

# library(xlsx)

GetCompanyMatchesOutput <- function(delegate_data, sponsor_data, Delegates_to_meet){
        Sponsor_info <- sponsor_data[,c(4, 2)] #FirstName, Surname, email
        CompanyMatchesOutput <- data.frame(matrix(ncol = 3, nrow = nrow(sponsor_data)))
        names(CompanyMatchesOutput) <- c('Name', 'email', 'Matches')
        CompanyMatchesOutput$Name <- Sponsor_info$Contact.name
        CompanyMatchesOutput$email <- Sponsor_info$Email.address

        for(i in 1:nrow(CompanyMatchesOutput)){
                Matches <- Delegates_to_meet[,i]
                Matches <- Matches[Matches != '']
                if(length(Matches) != 0){
                        CompanyMatchesOutput$Matches[i] <- paste(Matches, collapse = '\n')
                }
        }
        return(CompanyMatchesOutput)
}
