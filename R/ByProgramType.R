



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
