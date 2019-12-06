#' @title ByState
#' @description Creates a table of wrangled, aggreagate data of completer counts from the Title II website in each degree completion category(Major, Subject, Area) and in each state.
#' @param Year is digit numeric number of the year
#' @param with_summary defaults to TRUE. Adds a summary (total) row and the bottom of the table.

#' @export


ByState <- function(Year, with_summary = TRUE) {

  alldata <- PrepData(Year)
  StateDate <- NULL

  if (with_summary == TRUE) {

    StateData <- alldata %>%
      group_by(State) %>%
      summarise(PreparedByMajor = sum(TotalPreppedm), PreparedByArea = sum(TotalPreppeda), PreparedBySubject = sum(TotalPreppeds)) %>%
      adorn_totals(name = 'USA Total')

  } else if (with_summary == FALSE) {

    StateData <- alldata %>%
      group_by(State) %>%
      summarise(PreparedByMajor = sum(TotalPreppedm), PreparedByArea = sum(TotalPreppeda), PreparedBySubject = sum(TotalPreppeds))

  }

  return(StateData)

}
