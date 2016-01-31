#' Distribution of ages of professors at Williams College
#'
#' Returns a histogram of ages of professors at Williams College for any year from 2004 to 2014.
#'
#' For this to work properly, argument must be an integer representing a year from 2004 to 2014.
#'
#'@param \code{x} An integer on [2004, 2014]
#'
#'
# code copy/pasted from mean_age.R; only difference is rather than calling mean(ages), make histograms

age_hist <- function(x) {
  if (x == 2013) {
    prof_info <- system.file("extdata", "2013williamsprofs.txt", package = "avages")
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
    hist(ages, seq(20, 100), col = "red", main = "Prof Ages in 2013",
      xlab = "Age", ylim = c(0, 20))
  }
  # code the exact same as 2013 for 2012 thru 2004 because williams catalogue faculty sections were of same format. 2014 reported year before degree, so code is different.
  # ------------------------------------------------------------------------------

  else if (x == 2012) {
    prof_info <- system.file("extdata", "2012williamsprofs.txt", package = "avages")
    prof_info_charvec <- scan(prof_info, what = "character", quiet = TRUE)
    prof_info_charvec1 <- prof_info_charvec[!grepl("M.B.A.", prof_info_charvec,
      fixed = TRUE)]
    prof_info_charvec2 <- prof_info_charvec1[!grepl("A.B.D", prof_info_charvec1,
      fixed = TRUE)]
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
    years_charvec <- gsub("(", "", prof_info_charvec2[pos_undergrad_dates],
      fixed = TRUE)
    years_charvec1 <- gsub(")", "", years_charvec, fixed = TRUE)
    years_charvec2 <- years_charvec1[!grepl("[a-z]", years_charvec1)]
    years_charvec3 <- gsub("203", "2003", years_charvec2, fixed = TRUE)
    years_charvec4 <- gsub("2022", "2002", years_charvec3, fixed = TRUE)

    years_numvec <- strtoi(years_charvec4)
    ages <- x - years_numvec + 22
    hist(ages, seq(20, 100), col = "yellow", main = "Prof Ages in 2012",
      xlab = "Age")
  }
  else if (x == 2011) {
    prof_info <- system.file("extdata", "2011williamsprofs.txt", package = "avages")
    prof_info_charvec <- scan(prof_info, what = "character", quiet = TRUE)
    prof_info_charvec1 <- prof_info_charvec[!grepl("M.B.A.", prof_info_charvec,
      fixed = TRUE)]
    prof_info_charvec2 <- prof_info_charvec1[!grepl("A.B.D", prof_info_charvec1,
      fixed = TRUE)]
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
    years_charvec <- gsub("(", "", prof_info_charvec2[pos_undergrad_dates],
      fixed = TRUE)
    years_charvec1 <- gsub(")", "", years_charvec, fixed = TRUE)
    years_charvec2 <- years_charvec1[!grepl("[a-z]", years_charvec1)]
    years_charvec3 <- gsub("203", "2003", years_charvec2, fixed = TRUE)
    years_charvec4 <- gsub("2022", "2002", years_charvec3, fixed = TRUE)

    years_numvec <- strtoi(years_charvec4)
    ages <- x - years_numvec + 22
    hist(ages, seq(20, 100), col = "green", main = "Prof Ages in 2011",
      xlab = "Age", ylim = c(0, 20))
  }
  else if (x == 2010) {
    prof_info <- system.file("extdata", "2010williamsprofs.txt", package = "avages")
    prof_info_charvec <- scan(prof_info, what = "character", quiet = TRUE)
    prof_info_charvec1 <- prof_info_charvec[!grepl("M.B.A.", prof_info_charvec,
      fixed = TRUE)]
    prof_info_charvec2 <- prof_info_charvec1[!grepl("A.B.D", prof_info_charvec1,
      fixed = TRUE)]
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
    years_charvec <- gsub("(", "", prof_info_charvec2[pos_undergrad_dates],
      fixed = TRUE)
    years_charvec1 <- gsub(")", "", years_charvec, fixed = TRUE)
    years_charvec2 <- years_charvec1[!grepl("[a-z]", years_charvec1)]
    years_charvec3 <- gsub("203", "2003", years_charvec2, fixed = TRUE)
    years_charvec4 <- gsub("2022", "2002", years_charvec3, fixed = TRUE)

    years_numvec <- strtoi(years_charvec4)
    ages <- x - years_numvec + 22
    hist(ages, seq(20, 100), col = "violet", main = "Prof Ages in 2010",
      xlab = "Age", ylim = c(0, 20))
  }
  else if (x == 2009) {
    prof_info <- system.file("extdata", "2009williamsprofs.txt", package = "avages")
    prof_info_charvec <- scan(prof_info, what = "character", quiet = TRUE)
    prof_info_charvec1 <- prof_info_charvec[!grepl("M.B.A.", prof_info_charvec,
      fixed = TRUE)]
    prof_info_charvec2 <- prof_info_charvec1[!grepl("A.B.D", prof_info_charvec1,
      fixed = TRUE)]
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
    years_charvec <- gsub("(", "", prof_info_charvec2[pos_undergrad_dates],
      fixed = TRUE)
    years_charvec1 <- gsub(")", "", years_charvec, fixed = TRUE)
    years_charvec2 <- years_charvec1[!grepl("[a-z]", years_charvec1)]
    years_charvec3 <- gsub("203", "2003", years_charvec2, fixed = TRUE)
    years_charvec4 <- gsub("2022", "2002", years_charvec3, fixed = TRUE)

    years_numvec <- strtoi(years_charvec4)
    ages <- x - years_numvec + 22
    hist(ages, seq(20, 100), col = "orange", main = "Prof Ages in 2009",
      xlab = "Age", ylim = c(0, 20))
  }
  else if (x == 2008) {
    prof_info <- system.file("extdata", "2008williamsprofs.txt", package = "avages")
    prof_info_charvec <- scan(prof_info, what = "character", quiet = TRUE)
    prof_info_charvec1 <- prof_info_charvec[!grepl("M.B.A.", prof_info_charvec,
      fixed = TRUE)]
    prof_info_charvec2 <- prof_info_charvec1[!grepl("A.B.D", prof_info_charvec1,
      fixed = TRUE)]
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
    years_charvec <- gsub("(", "", prof_info_charvec2[pos_undergrad_dates],
      fixed = TRUE)
    years_charvec1 <- gsub(")", "", years_charvec, fixed = TRUE)
    years_charvec2 <- years_charvec1[!grepl("[a-z]", years_charvec1)]
    years_charvec3 <- gsub("203", "2003", years_charvec2, fixed = TRUE)
    years_charvec4 <- gsub("2022", "2002", years_charvec3, fixed = TRUE)

    years_numvec <- strtoi(years_charvec4)
    ages <- x - years_numvec + 22
    hist(ages, seq(20, 100), col = "blue", main = "Prof Ages in 2008",
      xlab = "Age", ylim = c(0, 20))
  }
  else if (x == 2007) {
    prof_info <- system.file("extdata", "2007williamsprofs.txt", package = "avages")
    prof_info_charvec <- scan(prof_info, what = "character", quiet = TRUE)
    prof_info_charvec1 <- prof_info_charvec[!grepl("M.B.A.", prof_info_charvec,
      fixed = TRUE)]
    prof_info_charvec2 <- prof_info_charvec1[!grepl("A.B.D", prof_info_charvec1,
      fixed = TRUE)]
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
    years_charvec <- gsub("(", "", prof_info_charvec2[pos_undergrad_dates],
      fixed = TRUE)
    years_charvec1 <- gsub(")", "", years_charvec, fixed = TRUE)
    years_charvec2 <- years_charvec1[!grepl("[a-z]", years_charvec1)]
    years_charvec3 <- gsub("203", "2003", years_charvec2, fixed = TRUE)
    years_charvec4 <- gsub("2022", "2002", years_charvec3, fixed = TRUE)

    years_numvec <- strtoi(years_charvec4)
    ages <- x - years_numvec + 22
    hist(ages, seq(20, 100), col = "pink", main = "Prof Ages in 2007",
      xlab = "Age", ylim = c(0, 20))
  }
  else if (x == 2006) {
    prof_info <- system.file("extdata", "2006williamsprofs.txt", package = "avages")
    prof_info_charvec <- scan(prof_info, what = "character", quiet = TRUE)
    prof_info_charvec1 <- prof_info_charvec[!grepl("M.B.A.", prof_info_charvec,
      fixed = TRUE)]
    prof_info_charvec2 <- prof_info_charvec1[!grepl("A.B.D", prof_info_charvec1,
      fixed = TRUE)]
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
    years_charvec <- gsub("(", "", prof_info_charvec2[pos_undergrad_dates],
      fixed = TRUE)
    years_charvec1 <- gsub(")", "", years_charvec, fixed = TRUE)
    years_charvec2 <- years_charvec1[!grepl("[a-z]", years_charvec1)]
    years_charvec3 <- gsub("203", "2003", years_charvec2, fixed = TRUE)
    years_charvec4 <- gsub("2022", "2002", years_charvec3, fixed = TRUE)

    years_numvec <- strtoi(years_charvec4)
    ages <- x - years_numvec + 22
    hist(ages, seq(20, 100), col = "cyan", main = "Prof Ages in 2006",
      xlab = "Age", ylim = c(0, 20))
  }
  else if (x == 2005) {
    prof_info <- system.file("extdata", "2005williamsprofs.txt", package = "avages")
    prof_info_charvec <- scan(prof_info, what = "character", quiet = TRUE)
    prof_info_charvec1 <- prof_info_charvec[!grepl("M.B.A.", prof_info_charvec,
      fixed = TRUE)]
    prof_info_charvec2 <- prof_info_charvec1[!grepl("A.B.D", prof_info_charvec1,
      fixed = TRUE)]
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
    years_charvec <- gsub("(", "", prof_info_charvec2[pos_undergrad_dates],
      fixed = TRUE)
    years_charvec1 <- gsub(")", "", years_charvec, fixed = TRUE)
    years_charvec2 <- years_charvec1[!grepl("[a-z]", years_charvec1)]
    years_charvec3 <- gsub("203", "2003", years_charvec2, fixed = TRUE)
    years_charvec4 <- gsub("2022", "2002", years_charvec3, fixed = TRUE)

    years_numvec <- strtoi(years_charvec4)
    ages <- x - years_numvec + 22
    hist(ages, seq(20, 100), col = "purple", main = "Prof Ages in 2005",
      xlab = "Age", ylim = c(0, 20))
  }
  else if (x == 2004) {
    prof_info <- system.file("extdata", "2004williamsprofs.txt", package = "avages")
    prof_info_charvec <- scan(prof_info, what = "character", quiet = TRUE)
    prof_info_charvec1 <- prof_info_charvec[!grepl("M.B.A.", prof_info_charvec,
      fixed = TRUE)]
    prof_info_charvec2 <- prof_info_charvec1[!grepl("A.B.D", prof_info_charvec1,
      fixed = TRUE)]
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
    years_charvec <- gsub("(", "", prof_info_charvec2[pos_undergrad_dates],
      fixed = TRUE)
    years_charvec1 <- gsub(")", "", years_charvec, fixed = TRUE)
    years_charvec2 <- years_charvec1[!grepl("[a-z]", years_charvec1)]
    years_charvec3 <- gsub("203", "2003", years_charvec2, fixed = TRUE)
    years_charvec4 <- gsub("2022", "2002", years_charvec3, fixed = TRUE)

    years_numvec <- strtoi(years_charvec4)
    ages <- x - years_numvec + 22
    hist(ages, seq(20, 100), col = 128, main = "Prof Ages in 2004",
      xlab = "Age", ylim = c(0, 20))
  }
  else if (x == 2014) {
    prof_info <- system.file("extdata", "2014williamsprofs.txt", package = "avages")
    prof_info_charvec <- scan(prof_info, what = "character", quiet = TRUE)
    #transform plain text document with 2014 prof info into a vector of strings
    prof_info_charvec1 <- prof_info_charvec[!grepl("MBA", prof_info_charvec, fixed = TRUE)]
    #only want to estimate ages with undergrad degrees; can't have MBA interfering with BA
    prof_info_charvec2 <- prof_info_charvec1[!grepl("MBA", prof_info_charvec1, fixed = TRUE)]

    pos_of_BA <- grep("BA", prof_info_charvec2, fixed = TRUE)
    pos_of_BA_dates <- pos_of_BA - 1
    pos_of_BS <- grep("BS", prof_info_charvec2, fixed = TRUE)
    pos_of_BS_dates <- pos_of_BS - 1
    pos_of_BM <- grep("BM", prof_info_charvec2, fixed = TRUE)
    pos_of_BM_dates <- pos_of_BM - 1
    pos_of_BE <- grep("BE", prof_info_charvec2, fixed = TRUE)
    pos_of_BE_dates <- pos_of_BE - 1
    pos_of_AB <- grep("AB", prof_info_charvec2, fixed = TRUE)
    pos_of_AB_dates <- pos_of_AB - 1
    pos_of_BFA <- grep("BFA", prof_info_charvec2, fixed = TRUE)
    pos_of_BFA_dates <- pos_of_BFA - 1
    pos_of_BPhil <- grep("BPhil", prof_info_charvec2, fixed = TRUE)
    pos_of_BPhil_dates <- pos_of_BPhil - 1
    pos_of_SB <- grep("SB", prof_info_charvec2, fixed = TRUE)
    pos_of_SB_dates <- pos_of_SB + 1
    pos_undergrad_dates <- c(pos_of_BA_dates, pos_of_BS_dates, pos_of_BM_dates, pos_of_BE_dates, pos_of_AB_dates, pos_of_BFA_dates, pos_of_BPhil_dates, pos_of_SB_dates)
    # dates of undergrad degrees always appear one word before degree title in Williams catalogue

    years_charvec <- gsub(",", "", prof_info_charvec2[pos_undergrad_dates])
    # get rid of commas after years so vector can be converted from strings to integers

    years_numvec <- strtoi(years_charvec)
    # converts vector of strings into integers so math can be done
    ages <- x - years_numvec + 22
    hist(ages, seq(20, 100), col = 644, main = "Prof Ages in 2014",
      xlab = "Age", ylim = c(0, 20))
  }
  else {
    stop('argument not an integer on [2004, 2014]')
  }
}
