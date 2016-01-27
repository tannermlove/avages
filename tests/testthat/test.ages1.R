test2009 <- test_that("Estimated ages for 2009 are valid ages", {
  prof_info <- system.file("extdata", "2009williamsprofs.txt", package = "avages")
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


test2008 <- test_that("Estimated ages for 2008 are valid ages", {
  prof_info <- system.file("extdata", "2008williamsprofs.txt", package = "avages")
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
  ages <- 2008 - years_numvec + 22

  expect_true(min(ages) > 22)
  expect_true(max(ages) < 95)


})



test2007 <- test_that("Estimated ages for 2007 are valid ages", {
  prof_info <- system.file("extdata", "2007williamsprofs.txt", package = "avages")
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
  ages <- 2007 - years_numvec + 22

  expect_true(min(ages) > 22)
  expect_true(max(ages) < 95)


})


test2006 <- test_that("Estimated ages for 2006 are valid ages", {
  prof_info <- system.file("extdata", "2006williamsprofs.txt", package = "avages")
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
  ages <- 2006 - years_numvec + 22

  expect_true(min(ages) > 22)
  expect_true(max(ages) < 95)


})


test2005 <- test_that("Estimated ages for 2005 are valid ages", {
  prof_info <- system.file("extdata", "2005williamsprofs.txt", package = "avages")
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
  ages <- 2005 - years_numvec + 22

  expect_true(min(ages) > 22)
  expect_true(max(ages) < 95)


})


test2004 <- test_that("Estimated ages for 2004 are valid ages", {
  prof_info <- system.file("extdata", "2004williamsprofs.txt", package = "avages")
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
  ages <- 2004 - years_numvec + 22

  expect_true(min(ages) > 22)
  expect_true(max(ages) < 95)


})
