#' @title PhysicsEdData
#' @description Creates a CSV file for desired version of wrangled data from the Physics Teacher Education Coalition report card website
#' @param Year is digit numeric number of the year
#' @param ByProgram is boolean to create csv file for ByProgram
#' @param ByProgramFileName is character, if ByProgram is TRUE the file will receive this name
#' @param ByProgramType is boolean to create csv file for ByProgramType
#' @param ByProgramTypeFileName is character, if ByProgramType is TRUE the file will receive this name
#' @param ByState is boolean to create csv file for ByProgram
#' @param ByStateFileName is character, if ByState is TRUE the file will receive this name

#' @export


PhysicsEdData <- function(Year, ByProgram = FALSE, ByProgramFileName = 'pfilename', ByProgramType = FALSE, ByProgramTypeFileName = 'ptfilename', ByState = FALSE, ByStateFileName = 'sfilename') {

  #Download Online File
  dataset <- tempfile(fileext = '.xls')
  download(paste0("https://title2.ed.gov/Public/DataTools/", Year, "/AllStates.xls"), mode = 'wb', destfile = dataset)



  read_tsv(dataset)
  dataset <- str_replace_all(dataset, '\\\\', '\\/')
  assign(paste0('Subject'), read_excel(dataset, sheet = 'PreparedBySubject'))
  assign(paste0('Major'), read_excel(dataset, sheet = 'PreparedByMajor'))
  assign(paste0('Area'), read_excel(dataset, sheet = 'PreparedByArea'))

  #Create Unique Program Names
  s1 <- unique(Subject$Program)
  a1 <- unique(Area$Program)
  m1 <- unique(Major$Program)

  allprog <- as.data.frame(unique(c(s1, a1, m1)))

  colnames(allprog) <- 'Program'


  allprog <- rbind(Major, Subject, Area) %>%
    select(State, Program, ProgramType)

  allprog <- allprog[!duplicated(allprog$Program),]

  #PreparedByArea Organization
  area2 <- Area %>%
    mutate(lowcat = str_to_lower(OtherSpecify),
           Physicsa = case_when(

             str_detect(lowcat, 'physics') |  str_detect(lowcat, 'astro') ~ 1

           )) %>%
    filter(Physicsa == 1) %>%
    group_by(Program) %>%
    summarise(TotalPreppeda = sum(Prepared))


  #PreparedByMajor Organization
  major2 <- Major %>%
    mutate(lowcatm = str_to_lower(Category),
           lowother = str_to_lower(OtherSpecify),
           Physicsm = case_when(

             str_detect(lowcatm, 'physics') | str_detect(lowcatm, 'astro')  | str_detect(lowother, 'physics')  | str_detect(lowcatm, 'astro')~ 1,

           )) %>%
    filter(Physicsm == 1) %>%
    group_by(Program) %>%
    summarise(TotalPreppedm = sum(Prepared))


  #PreparedBySubject
  subject2 <- Subject %>%
    mutate(lowcats = str_to_lower(Category),
           lowothers = str_to_lower(OtherSpecify),
           Physicss = case_when(

             str_detect(lowcats, 'physics') | str_detect(lowothers, 'physics') ~ 1

           )) %>%
    filter(Physicss == 1) %>%
    group_by(Program) %>%
    summarise(TotalPreppeds = sum(Prepared))

  naalldata <- allprog %>%
    left_join(area2, by = 'Program') %>%
    left_join(major2, by = 'Program') %>%
    left_join(subject2, by = 'Program') %>%
    mutate(allna = case_when(

      is.na(TotalPreppeda & is.na(TotalPreppedm) &is.na(TotalPreppeds)) ~ 'is na',
      TRUE ~ 'is not na'

    )) %>%
    filter(allna == 'is not na') %>%
    select(1:6)


  alldata <- naalldata %>%
    mutate(TotalPreppeds = case_when(

      is.na(TotalPreppeds) ~ 0,
      TRUE ~ TotalPreppeds

    ),
    TotalPreppedm = case_when(

      is.na(TotalPreppedm) ~ 0,
      TRUE ~ TotalPreppedm

    ),
    TotalPreppeda = case_when(

      is.na(TotalPreppeda) ~ 0,
      TRUE ~ TotalPreppeda

    ))

  pdata <- NULL
  ptdata <- NULL
  sdata <- NULL


  if (ByProgram == TRUE) {

    ProgramData <- alldata %>%
      mutate(PreparedByMajor = TotalPreppedm, PreparedByArea = TotalPreppeda, PreparedBySubject = TotalPreppeds) %>%
      select(c(1, 2, 3, 7, 8, 9)) %>%
      adorn_totals(name = 'USA Total')

    pdata <- ProgramData

    #write.csv(ProgramData, paste0(ByProgramFileName,".csv"), row.names = FALSE)

  }

  if (ByProgramType == TRUE) {

    PTData <- alldata %>%
      group_by(State, ProgramType) %>%
      summarise(PreparedByMajor = sum(TotalPreppedm), PreparedByArea = sum(TotalPreppeda), PreparedBySubject = sum(TotalPreppeds))

    summary <- alldata %>%
      group_by(ProgramType) %>%
      summarise(PreparedByMajor = sum(PreparedByMajor), PreparedByArea = sum(PreparedByArea), PreparedBySubject = sum(PreparedBySubject)) %>%
      mutate(State = 'USA') %>%
      select(c(5, 1:4)) %>%
      adorn_totals(name = 'USA Total')

    alldata1 <- bind_rows(PTData, summary)

    ptdata <- alldata1

    #write.csv(alldata1, paste0(ByProgramTypeFileName, ".csv"), row.names = FALSE)

  }

  if (ByState == TRUE) {

    StateData <- alldata %>%
      group_by(State) %>%
      summarise(PreparedByMajor = sum(TotalPreppedm), PreparedByArea = sum(TotalPreppeda), PreparedBySubject = sum(TotalPreppeds)) %>%
      adorn_totals(name = 'USA Total')

    sdata <- StateData

    #write.csv(StateData, paste0(ByStateFileName, ".csv"), row.names = FALSE)

  }

  return(pdtaa, ptdata, sdata)

}
