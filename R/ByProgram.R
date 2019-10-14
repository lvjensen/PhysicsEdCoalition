


ByProgram <- function(Year, with_summary = TRUE) {

  alldata <- PrepData(Year)
  Programdata <- NULL

  if (with_summary == TRUE) {

    ProgramData <- alldata %>%
      mutate(PreparedByMajor = TotalPreppedm, PreparedByArea = TotalPreppeda, PreparedBySubject = TotalPreppeds) %>%
      select(c(1, 2, 3, 7, 8, 9)) %>%
      adorn_totals(name = 'USA Total')

  } else if (with_summary == FALSE) {

    ProgramData <- alldata %>%
      mutate(PreparedByMajor = TotalPreppedm, PreparedByArea = TotalPreppeda, PreparedBySubject = TotalPreppeds) %>%
      select(c(1, 2, 3, 7, 8, 9))

  }

  return(ProgramData)

}
