
#' @title ByProgramType
#' @description Creates a table of wrangled, aggregate data of completer counts from the Title II website in each degree completion category(Major, Subject, Area) organized by Program Type(IHE-based or not IHE-based)
#' @param Year is digit numeric number of the year
#' @param with_summary defaults to TRUE. Adds a summary (total) row and the bottom of the table.

#' @export

ByProgramType <- function(Year, with_summary = TRUE) {

  alldata <- PrepData(Year)
  alldata1 <- NULL

  if (with_summary == TRUE) {

    PTData <- alldata %>%
      group_by(State, ProgramType) %>%
      summarise(PreparedByMajor = sum(TotalPreppedm), PreparedByArea = sum(TotalPreppeda), PreparedBySubject = sum(TotalPreppeds))

    summary <- alldata %>%
      group_by(ProgramType) %>%
      summarise(PreparedByMajor = sum(TotalPreppedm), PreparedByArea = sum(TotalPreppeda), PreparedBySubject = sum(TotalPreppeds)) %>%
      mutate(State = 'USA') %>%
      select(c(5, 1:4)) %>%
      adorn_totals(name = 'USA Total')

    alldata1 <- bind_rows(PTData, summary)

  } else if (with_summary == FALSE) {

    alldata1 <- alldata %>%
      group_by(State, ProgramType) %>%
      summarise(PreparedByMajor = sum(TotalPreppedm), PreparedByArea = sum(TotalPreppeda), PreparedBySubject = sum(TotalPreppeds))

  }

  return(alldata1)

}
