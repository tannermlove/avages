test2014 <- test_that("Estimated ages for 2014 are valid ages", {
  prof_info <- system.file("extdata", "2014williamsprofs.txt", package = "avages")
  prof_info_charvec <- scan(prof_info, what = "character", quiet = TRUE)
  prof_info_charvec1 <- prof_info_charvec[!grepl("MBA", prof_info_charvec,
    fixed = TRUE)]
  pos_of_BA <- grep("BA", prof_info_charvec1, fixed = TRUE)
  pos_of_BA_dates <- pos_of_BA - 1
  pos_of_BS <- grep("BS", prof_info_charvec1, fixed = TRUE)
  pos_of_BS_dates <- pos_of_BS - 1
  pos_of_BM <- grep("BM", prof_info_charvec1, fixed = TRUE)
  pos_of_BM_dates <- pos_of_BM - 1
  pos_of_BE <- grep("BE", prof_info_charvec1, fixed = TRUE)
  pos_of_BE_dates <- pos_of_BE - 1
  pos_of_AB <- grep("AB", prof_info_charvec1, fixed = TRUE)
  pos_of_AB_dates <- pos_of_AB - 1
  pos_of_BFA <- grep("BFA", prof_info_charvec1, fixed = TRUE)
  pos_of_BFA_dates <- pos_of_BFA - 1
  pos_of_BPhil <- grep("BPhil", prof_info_charvec1, fixed = TRUE)
  pos_of_BPhil_dates <- pos_of_BPhil - 1
  pos_undergrad_dates <- c(pos_of_BA_dates, pos_of_BS_dates, pos_of_BM_dates,
    pos_of_BE_dates, pos_of_AB_dates, pos_of_BPhil_dates, pos_of_BFA_dates)
  years_charvec <- gsub(",", "", prof_info_charvec1[pos_undergrad_dates])
  years_numvec <- strtoi(years_charvec)
  ages <- 2014 - years_numvec + 22

  expect_true(min(ages) > 22)
  expect_true(max(ages) < 95)


})


test2013 <- test_that("Estimated ages for 2013 are valid ages", {
  prof_info <- system.file("extdata", "2013williamsprofs.txt", package = "avages")
  prof_info_charvec <- scan(prof_info, what = "character", quiet = TRUE)
  prof_info_charvec1 <- prof_info_charvec[!grepl("M.B.A.", prof_info_charvec,
    fixed = TRUE)]
  pos_of_BA <- grep("B.A.", prof_info_charvec1, fixed = TRUE)
  pos_of_BA_dates <- pos_of_BA + 1
  pos_of_BS <- grep("B.S.", prof_info_charvec1, fixed = TRUE)
  pos_of_BS_dates <- pos_of_BS + 1
  pos_of_BM <- grep("B.M.", prof_info_charvec1, fixed = TRUE)
  pos_of_BM_dates <- pos_of_BM + 1
  pos_of_BE <- grep("B.E.", prof_info_charvec1, fixed = TRUE)
  pos_of_BE_dates <- pos_of_BE + 1
  pos_of_AB <- grep("A.B.", prof_info_charvec1, fixed = TRUE)
  pos_of_AB_dates <- pos_of_AB + 1
  pos_of_BFA <- grep("—B.F.A.", prof_info_charvec1, fixed = TRUE)
  pos_of_BFA_dates <- pos_of_BFA + 1
  pos_of_BPhil <- grep("B.Phil", prof_info_charvec1, fixed = TRUE)
  pos_of_BPhil_dates <- pos_of_BPhil + 1

  pos_undergrad_dates <- c(pos_of_BA_dates, pos_of_BS_dates, pos_of_BM_dates,
    pos_of_BE_dates, pos_of_AB_dates, pos_of_BFA_dates, pos_of_BPhil_dates)

  years_charvec <- gsub("(", "", prof_info_charvec1[pos_undergrad_dates],
    fixed = TRUE)
  years_charvec1 <- gsub(")", "", years_charvec, fixed = TRUE)
  years_charvec2 <- years_charvec1[!grepl("[a-z]", years_charvec1)]
  years_charvec3 <- gsub("203", "2003", years_charvec2, fixed = TRUE)
  years_charvec4 <- gsub("2022", "2002", years_charvec3, fixed = TRUE)
  years_charvec5 <- years_charvec4[years_charvec4 != 0]
  years_numvec <- strtoi(years_charvec5)
  ages <- 2013 - years_numvec + 22

  expect_true(min(ages) > 22)
  expect_true(max(ages) < 95)


})



test2012 <- test_that("Estimated ages for 2012 are valid ages", {
  prof_info <- system.file("extdata", "2012williamsprofs.txt", package = "avages")
  prof_info_charvec <- scan(prof_info, what = "character", quiet = TRUE)
  prof_info_charvec1 <- prof_info_charvec[!grepl("M.B.A.", prof_info_charvec,
    fixed = TRUE)]
  pos_of_BA <- grep("B.A.", prof_info_charvec1, fixed = TRUE)
  pos_of_BA_dates <- pos_of_BA + 1
  pos_of_BS <- grep("B.S.", prof_info_charvec1, fixed = TRUE)
  pos_of_BS_dates <- pos_of_BS + 1
  pos_of_BM <- grep("B.M.", prof_info_charvec1, fixed = TRUE)
  pos_of_BM_dates <- pos_of_BM + 1
  pos_of_BE <- grep("B.E.", prof_info_charvec1, fixed = TRUE)
  pos_of_BE_dates <- pos_of_BE + 1
  pos_of_AB <- grep("A.B.", prof_info_charvec1, fixed = TRUE)
  pos_of_AB_dates <- pos_of_AB + 1
  pos_of_BFA <- grep("—B.F.A.", prof_info_charvec1, fixed = TRUE)
  pos_of_BFA_dates <- pos_of_BFA + 1
  pos_of_BPhil <- grep("B.Phil", prof_info_charvec1, fixed = TRUE)
  pos_of_BPhil_dates <- pos_of_BPhil + 1

  pos_undergrad_dates <- c(pos_of_BA_dates, pos_of_BS_dates, pos_of_BM_dates,
    pos_of_BE_dates, pos_of_AB_dates, pos_of_BFA_dates, pos_of_BPhil_dates)

  years_charvec <- gsub("(", "", prof_info_charvec1[pos_undergrad_dates],
    fixed = TRUE)
  years_charvec1 <- gsub(")", "", years_charvec, fixed = TRUE)
  years_charvec2 <- years_charvec1[!grepl("[a-z]", years_charvec1)]
  years_charvec3 <- gsub("203", "2003", years_charvec2, fixed = TRUE)
  years_charvec4 <- gsub("2022", "2002", years_charvec3, fixed = TRUE)
  years_charvec5 <- years_charvec4[years_charvec4 != 0]
  years_numvec <- strtoi(years_charvec5)
  ages <- 2012 - years_numvec + 22

  expect_true(min(ages) > 22)
  expect_true(max(ages) < 95)


})


test2011 <- test_that("Estimated ages for 2011 are valid ages", {
  prof_info <- system.file("extdata", "2011williamsprofs.txt", package = "avages")
  prof_info_charvec <- scan(prof_info, what = "character", quiet = TRUE)
  prof_info_charvec1 <- prof_info_charvec[!grepl("M.B.A.", prof_info_charvec,
    fixed = TRUE)]
  pos_of_BA <- grep("B.A.", prof_info_charvec1, fixed = TRUE)
  pos_of_BA_dates <- pos_of_BA + 1
  pos_of_BS <- grep("B.S.", prof_info_charvec1, fixed = TRUE)
  pos_of_BS_dates <- pos_of_BS + 1
  pos_of_BM <- grep("B.M.", prof_info_charvec1, fixed = TRUE)
  pos_of_BM_dates <- pos_of_BM + 1
  pos_of_BE <- grep("B.E.", prof_info_charvec1, fixed = TRUE)
  pos_of_BE_dates <- pos_of_BE + 1
  pos_of_AB <- grep("A.B.", prof_info_charvec1, fixed = TRUE)
  pos_of_AB_dates <- pos_of_AB + 1
  pos_of_BFA <- grep("—B.F.A.", prof_info_charvec1, fixed = TRUE)
  pos_of_BFA_dates <- pos_of_BFA + 1
  pos_of_BPhil <- grep("B.Phil", prof_info_charvec1, fixed = TRUE)
  pos_of_BPhil_dates <- pos_of_BPhil + 1

  pos_undergrad_dates <- c(pos_of_BA_dates, pos_of_BS_dates, pos_of_BM_dates,
    pos_of_BE_dates, pos_of_AB_dates, pos_of_BFA_dates, pos_of_BPhil_dates)

  years_charvec <- gsub("(", "", prof_info_charvec1[pos_undergrad_dates],
    fixed = TRUE)
  years_charvec1 <- gsub(")", "", years_charvec, fixed = TRUE)
  years_charvec2 <- years_charvec1[!grepl("[a-z]", years_charvec1)]
  years_charvec3 <- gsub("203", "2003", years_charvec2, fixed = TRUE)
  years_charvec4 <- gsub("2022", "2002", years_charvec3, fixed = TRUE)
  years_charvec5 <- years_charvec4[years_charvec4 != 0]
  years_numvec <- strtoi(years_charvec5)
  ages <- 2011 - years_numvec + 22

  expect_true(min(ages) > 22)
  expect_true(max(ages) < 95)


})


test2010 <- test_that("Estimated ages for 2010 are valid ages", {
  prof_info <- system.file("extdata", "2010williamsprofs.txt", package = "avages")
  prof_info_charvec <- scan(prof_info, what = "character", quiet = TRUE)
  prof_info_charvec1 <- prof_info_charvec[!grepl("M.B.A.", prof_info_charvec,
    fixed = TRUE)]
  pos_of_BA <- grep("B.A.", prof_info_charvec1, fixed = TRUE)
  pos_of_BA_dates <- pos_of_BA + 1
  pos_of_BS <- grep("B.S.", prof_info_charvec1, fixed = TRUE)
  pos_of_BS_dates <- pos_of_BS + 1
  pos_of_BM <- grep("B.M.", prof_info_charvec1, fixed = TRUE)
  pos_of_BM_dates <- pos_of_BM + 1
  pos_of_BE <- grep("B.E.", prof_info_charvec1, fixed = TRUE)
  pos_of_BE_dates <- pos_of_BE + 1
  pos_of_AB <- grep("A.B.", prof_info_charvec1, fixed = TRUE)
  pos_of_AB_dates <- pos_of_AB + 1
  pos_of_BFA <- grep("—B.F.A.", prof_info_charvec1, fixed = TRUE)
  pos_of_BFA_dates <- pos_of_BFA + 1
  pos_of_BPhil <- grep("B.Phil", prof_info_charvec1, fixed = TRUE)
  pos_of_BPhil_dates <- pos_of_BPhil + 1

  pos_undergrad_dates <- c(pos_of_BA_dates, pos_of_BS_dates, pos_of_BM_dates,
    pos_of_BE_dates, pos_of_AB_dates, pos_of_BFA_dates, pos_of_BPhil_dates)

  years_charvec <- gsub("(", "", prof_info_charvec1[pos_undergrad_dates],
    fixed = TRUE)
  years_charvec1 <- gsub(")", "", years_charvec, fixed = TRUE)
  years_charvec2 <- years_charvec1[!grepl("[a-z]", years_charvec1)]
  years_charvec3 <- gsub("203", "2003", years_charvec2, fixed = TRUE)
  years_charvec4 <- gsub("2022", "2002", years_charvec3, fixed = TRUE)
  years_charvec5 <- years_charvec4[years_charvec4 != 0]
  years_numvec <- strtoi(years_charvec5)
  ages <- 2009 - years_numvec + 22

  expect_true(min(ages) > 22)
  expect_true(max(ages) < 95)


})
