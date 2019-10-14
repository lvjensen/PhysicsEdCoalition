


ByState <- function(Year) {

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
