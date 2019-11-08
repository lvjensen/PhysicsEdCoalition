
#' @title ByProgram
#' @description Creates an table of wrangled data from the Physics Teacher Education Coalition websites organized by Program(e.g. University of Arizona, Brigham Young University idaho, etc.)
#' @param Year is digit numeric number of the year
#' @param with_summary defaults to TRUE. Adds a summary (total) row and the bottom of the table.

#' @export
#' @importFrom dplyr mutate select case_when %>% group_by summarise bind_rows filter left_join
#' @importFrom janitor adorn_totals
#' @importFrom downloader download
#' @importFrom readxl read_excel
#' @importFrom stringr str_to_lower str_detect


ByProgram <- function(Year, with_summary = TRUE) {

  alldata <- PrepData(Year)
  Programdata <- NULL

  if (with_summary == TRUE) {

    ProgramData <- alldata %>%
      mutate(PreparedByMajor = TotalPreppedm, PreparedByArea = TotalPreppeda, PreparedBySubject = TotalPreppeds) %>%
      select(c(1, 2, 3, 4, 9, 10, 11)) %>%
      adorn_totals(name = 'USA Total')

  } else if (with_summary == FALSE) {

    ProgramData <- alldata %>%
      mutate(PreparedByMajor = TotalPreppedm, PreparedByArea = TotalPreppeda, PreparedBySubject = TotalPreppeds) %>%
      select(c(1, 2, 3, 4, 9, 10, 11))

  }

  return(ProgramData)

}


############ Old Version, Didn't have IPEDS number
#
# ByProgram <- function(Year, with_summary = TRUE) {
#
#   alldata <- PrepData(Year)
#   Programdata <- NULL
#
#   if (with_summary == TRUE) {
#
#     ProgramData <- alldata %>%
#       mutate(PreparedByMajor = TotalPreppedm, PreparedByArea = TotalPreppeda, PreparedBySubject = TotalPreppeds) %>%
#       select(c(1, 2, 3, 7, 8, 9)) %>%
#       adorn_totals(name = 'USA Total')
#
#   } else if (with_summary == FALSE) {
#
#     ProgramData <- alldata %>%
#       mutate(PreparedByMajor = TotalPreppedm, PreparedByArea = TotalPreppeda, PreparedBySubject = TotalPreppeds) %>%
#       select(c(1, 2, 3, 7, 8, 9))
#
#   }
#
#   return(ProgramData)
#
# }
