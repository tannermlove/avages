ages_1 <- function(x) {
  if (x == 2013) {
    prof_info <- system.file("extdata", "2013williamsprofs.txt", package = "avages")
  }
else if (x == 2012) {
    prof_info <- system.file("extdata", "2012williamsprofs.txt", package = "avages")
  }
  else if (x == 2011) {
    prof_info <- system.file("extdata", "2011williamsprofs.txt", package = "avages")
  }
  else if (x == 2010) {
    prof_info <- system.file("extdata", "2010williamsprofs.txt", package = "avages")
  }
  else if (x == 2009) {
    prof_info <- system.file("extdata", "2009williamsprofs.txt", package = "avages")
  }
  else if (x == 2008) {
    prof_info <- system.file("extdata", "2008williamsprofs.txt", package = "avages")
  }
  else if (x == 2007) {
    prof_info <- system.file("extdata", "2007williamsprofs.txt", package = "avages")
  }
  else if (x == 2006) {
    prof_info <- system.file("extdata", "2006williamsprofs.txt", package = "avages")
  }
  else if (x == 2005) {
    prof_info <- system.file("extdata", "2005williamsprofs.txt", package = "avages")
  }
  else if (x == 2004) {
    prof_info <- system.file("extdata", "2004williamsprofs.txt", package = "avages")
  }
  else {
    stop('argument not an integer on [2004, 2014]')
  }

  prof_info_charvec <- scan(prof_info, what = "character", quiet = TRUE)
  #transform plain text document with 2013 prof info into a vector of strings

  prof_info_charvec1 <- prof_info_charvec[!grepl("M.B.A.", prof_info_charvec, fixed = TRUE)]
  #only want to estimate ages with undergrad degrees; can't have MBA interfering with BA

  prof_info_charvec2 <- prof_info_charvec1[!grepl("A.B.D.", prof_info_charvec1, fixed = TRUE)]
  #only want to estimate ages with undergrad degrees; can't have ABD interfering with AB
  pos_of_BA <- grep("B.A.", prof_info_charvec2, fixed = TRUE)
  pos_of_BA_dates <- pos_of_BA + 1
  pos_of_BS <- grep("B.S.", prof_info_charvec2, fixed = TRUE)
  pos_of_BS_dates <- pos_of_BS + 1
  pos_of_BM <- grep("B.M.", prof_info_charvec2, fixed = TRUE)
  pos_of_BM_dates <- pos_of_BM + 1
  pos_of_BE <- grep("B.E.", prof_info_charvec2, fixed = TRUE)
  pos_of_BE_dates <- pos_of_BE + 1
  pos_of_AB <- grep("A.B.", prof_info_charvec2, fixed = TRUE)
  pos_of_AB_dates <- pos_of_AB + 1
  pos_of_BFA <- grep("—B.F.A.", prof_info_charvec2, fixed = TRUE)
  pos_of_BFA_dates <- pos_of_BFA + 1
  pos_of_BPhil <- grep("B.Phil", prof_info_charvec2, fixed = TRUE)
  pos_of_BPhil_dates <- pos_of_BPhil + 1
  pos_of_SB <- grep("S.B.", prof_info_charvec2, fixed = TRUE)
  pos_of_SB_dates <- pos_of_SB + 1
  pos_undergrad_dates <- c(pos_of_BA_dates, pos_of_BS_dates, pos_of_BM_dates, pos_of_BE_dates, pos_of_AB_dates, pos_of_BFA_dates, pos_of_BPhil_dates, pos_of_SB_dates)
  #dates that undergrad degrees were receieved always appear 1 word after degree titles in Williams catalogue

  years_charvec <- gsub("(", "", prof_info_charvec2[pos_undergrad_dates],
    fixed = TRUE)
  years_charvec1 <- gsub(")", "", years_charvec, fixed = TRUE)
  # get rid of parentheses around years so they can be converted from strings to integers

  years_charvec2 <- years_charvec1[!grepl("[a-z]", years_charvec1)]
  # gets rid of any words that may have slipped through

  years_charvec3 <- gsub("203", "2003", years_charvec2, fixed = TRUE)
  # because Williams 2012-2013 catalogue accidentally reports that A. Levitt received B.F.A in 202 rather than 2003

  years_charvec4 <- gsub("2022", "2002", years_charvec3, fixed = TRUE)
  # because Williams 2011-2012 catalogue accidentally reports that J. Joosten received BA in 2022 rather than 2002

  years_numvec <- strtoi(years_charvec4)
  # converts vector of strings into integers so that math can be done
  ages <- x - years_numvec + 22
  }
