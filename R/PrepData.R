
#' @title PrepData
#' @description Creates a table of wrangled, aggregate data of counts from the Title II website in each degree completion category(Major, Subject, Area) and in each program(Arizona State University, Brigham Young University, etc.) and prepares it for use in other functions(ByState(), ByProgram(), ByProgramType())
#' @param Year is digit numeric number of the year
#' @param with_summary defaults to TRUE. Adds a summary (total) row and the bottom of the table.

#' @export
#'


PrepData <- function(Year) {

  #Download Online File
  dataset <- tempfile(fileext = '.xls')
  downloader::download(paste0("https://title2.ed.gov/Public/DataTools/", Year, "/AllStates.xls"), mode = 'wb', destfile = dataset)
  assign(paste0('Subject'), readxl::read_excel(dataset, sheet = 'PreparedBySubject'))
  assign(paste0('Major'), readxl::read_excel(dataset, sheet = 'PreparedByMajor'))
  assign(paste0('Area'), readxl::read_excel(dataset, sheet = 'PreparedByArea'))

  dataset2 <- tempfile(fileext = '.xls')
  download(paste0("https://title2.ed.gov/Public/DataTools/", Year, "/IPEDS_Crosswalk.xls"), mode = 'wb', destfile = dataset2)

  if (Year == 2012) {
    Ipeds <- read_excel(dataset2, sheet = 'All')
  } else {
    Ipeds <- read_excel(dataset2)
  }


  # colnames(Ipeds)[3] <- 'ProgramCode'
  # colnames(Ipeds)[4] <- 'Program'
  #
  # Ipeds <- Ipeds[!duplicated(Ipeds$ProgramCode),]

  if (Year %in% c(2012, 2013, 2014)) {

    Ipeds <- Ipeds %>%
      mutate(newcol = paste0(ProgramCode, Program))

    Ipeds <- Ipeds[!duplicated(Ipeds$newcol),]

    Ipeds <- Ipeds %>%
      select(ProgramCode, Program, IPEDS)

  } else if (Year >= 2017) {

    colnames(Ipeds)[3] <- 'ProgramCode'
    colnames(Ipeds)[4] <- 'Program'

    Ipeds <- Ipeds %>%
      mutate(newcol = paste0(ProgramCode, Program))

    Ipeds <- Ipeds[!duplicated(Ipeds$newcol),]

    Ipeds <- Ipeds %>%
      select(ProgramCode, Program, IPEDS)

  } else {

    colnames(Ipeds)[2] <- 'ProgramCode'
    colnames(Ipeds)[3] <- 'Program'

    Ipeds <- Ipeds %>%
      mutate(newcol = paste0(ProgramCode, Program))

    Ipeds <- Ipeds[!duplicated(Ipeds$newcol),]

    Ipeds <- Ipeds %>%
      select(ProgramCode, Program, IPEDS)

  }


  #Create Unique Program Names
  s1 <- unique(Subject$Program)
  a1 <- unique(Area$Program)
  m1 <- unique(Major$Program)

  allprog <- as.data.frame(unique(c(s1, a1, m1)))

  colnames(allprog) <- 'Program'

  allprog <- rbind(Major, Subject, Area) %>%
    dplyr::select(State, Program, ProgramType, ProgramCode)

  allprog <- allprog[!duplicated(allprog$Program),]

  #PreparedByArea Organization
  area2 <- Area %>%
    dplyr::mutate(lowcat = str_to_lower(OtherSpecify),
                  Physicsa = dplyr::case_when(

                    str_detect(lowcat, 'physics') |  str_detect(lowcat, 'astro') ~ 1

                  )) %>%
    dplyr::filter(Physicsa == 1) %>%
    dplyr::group_by(ProgramCode, Program) %>%
    dplyr::summarise(TotalPreppeda = sum(Prepared))


  #PreparedByMajor Organization
  major2 <- Major %>%
    dplyr::mutate(lowcatm = str_to_lower(Category),
                  lowother = str_to_lower(OtherSpecify),
                  Physicsm = dplyr::case_when(

                    str_detect(lowcatm, 'physics') | str_detect(lowcatm, 'astro')  | str_detect(lowother, 'physics')  | str_detect(lowcatm, 'astro')~ 1,

                  )) %>%
    dplyr::filter(Physicsm == 1) %>%
    dplyr::group_by(ProgramCode, Program) %>%
    dplyr::summarise(TotalPreppedm = sum(Prepared))


  #PreparedBySubject
  subject2 <- Subject %>%
    mutate(lowcats = str_to_lower(Category),
           lowothers = str_to_lower(OtherSpecify),
           Physicss = case_when(

             str_detect(lowcats, 'physics') | str_detect(lowothers, 'physics') ~ 1

           )) %>%
    filter(Physicss == 1) %>%
    group_by(ProgramCode, Program) %>%
    summarise(TotalPreppeds = sum(Prepared))

  naalldata <- allprog %>%
    left_join(area2, by = c('Program', 'ProgramCode')) %>%
    left_join(major2, by = c('Program', 'ProgramCode')) %>%
    left_join(subject2, by = c('Program', 'ProgramCode')) %>%
    mutate(allna = case_when(

      is.na(TotalPreppeda & is.na(TotalPreppedm) &is.na(TotalPreppeds)) ~ 'is na',
      TRUE ~ 'is not na'

    )) %>%
    #filter(allna == 'is not na') %>%
    select(1:7)


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

  alldata3 <- alldata %>%
    left_join(Ipeds, by = c('Program', 'ProgramCode')) %>%
    select(c(8, 1:7)) %>%
    mutate(ProgramType = case_when(

      ProgramType == 'Traditional' | ProgramType == 'Alternative, IHE-based' ~ 'IHE-based',
      TRUE ~ 'Not IHE-based'

    ))

  #To fix the extra rows given to years before 2015

  alldata4 <- alldata3 %>%
    mutate(newc = paste0(Program, ProgramType))

  alldata5 <- alldata4[!duplicated(alldata4$newc), ]

  alldata6 <- alldata5[,-9]


  return(alldata6)

}

#Don't join by just program code

####### Old Version, didn't have IPEDS number

# PrepData <- function(Year) {
#
#   #Download Online File
#   dataset <- tempfile(fileext = '.xls')
#   downloader::download(paste0("https://title2.ed.gov/Public/DataTools/", Year, "/AllStates.xls"), mode = 'wb', destfile = dataset)
#   assign(paste0('Subject'), readxl::read_excel(dataset, sheet = 'PreparedBySubject'))
#   assign(paste0('Major'), readxl::read_excel(dataset, sheet = 'PreparedByMajor'))
#   assign(paste0('Area'), readxl::read_excel(dataset, sheet = 'PreparedByArea'))
#
#   #Create Unique Program Names
#   s1 <- unique(Subject$Program)
#   a1 <- unique(Area$Program)
#   m1 <- unique(Major$Program)
#
#   allprog <- as.data.frame(unique(c(s1, a1, m1)))
#
#   colnames(allprog) <- 'Program'
#
#
#   allprog <- rbind(Major, Subject, Area) %>%
#     dplyr::select(State, Program, ProgramType)
#
#   allprog <- allprog[!duplicated(allprog$Program),]
#
#   #PreparedByArea Organization
#   area2 <- Area %>%
#     dplyr::mutate(lowcat = str_to_lower(OtherSpecify),
#            Physicsa = dplyr::case_when(
#
#              str_detect(lowcat, 'physics') |  str_detect(lowcat, 'astro') ~ 1
#
#            )) %>%
#     dplyr::filter(Physicsa == 1) %>%
#     dplyr::group_by(Program) %>%
#     dplyr::summarise(TotalPreppeda = sum(Prepared))
#
#
#   #PreparedByMajor Organization
#   major2 <- Major %>%
#     dplyr::mutate(lowcatm = str_to_lower(Category),
#            lowother = str_to_lower(OtherSpecify),
#            Physicsm = dplyr::case_when(
#
#              str_detect(lowcatm, 'physics') | str_detect(lowcatm, 'astro')  | str_detect(lowother, 'physics')  | str_detect(lowcatm, 'astro')~ 1,
#
#            )) %>%
#     dplyr::filter(Physicsm == 1) %>%
#     dplyr::group_by(Program) %>%
#     dplyr::summarise(TotalPreppedm = sum(Prepared))
#
#
#   #PreparedBySubject
#   subject2 <- Subject %>%
#     mutate(lowcats = str_to_lower(Category),
#            lowothers = str_to_lower(OtherSpecify),
#            Physicss = case_when(
#
#              str_detect(lowcats, 'physics') | str_detect(lowothers, 'physics') ~ 1
#
#            )) %>%
#     filter(Physicss == 1) %>%
#     group_by(Program) %>%
#     summarise(TotalPreppeds = sum(Prepared))
#
#   naalldata <- allprog %>%
#     left_join(area2, by = 'Program') %>%
#     left_join(major2, by = 'Program') %>%
#     left_join(subject2, by = 'Program') %>%
#     mutate(allna = case_when(
#
#       is.na(TotalPreppeda & is.na(TotalPreppedm) &is.na(TotalPreppeds)) ~ 'is na',
#       TRUE ~ 'is not na'
#
#     )) %>%
#     filter(allna == 'is not na') %>%
#     select(1:6)
#
#
#   alldata <- naalldata %>%
#     mutate(TotalPreppeds = case_when(
#
#       is.na(TotalPreppeds) ~ 0,
#       TRUE ~ TotalPreppeds
#
#     ),
#     TotalPreppedm = case_when(
#
#       is.na(TotalPreppedm) ~ 0,
#       TRUE ~ TotalPreppedm
#
#     ),
#     TotalPreppeda = case_when(
#
#       is.na(TotalPreppeda) ~ 0,
#       TRUE ~ TotalPreppeda
#
#     ))
#
#   return(alldata)
#
# }
