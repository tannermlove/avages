#' Age vector generator 2
#'
#' Generates a vector filled with integers representing the ages of professors at Williams College in 2014.
#'
#' For this to work properly, argument must be the integer 2014.
#'
#'@param \code{x} the integer 2014
#'
ages_2 <- function(x) {
    prof_info <- system.file("extdata", "2014williamsprofs.txt", package = "avages")
    prof_info_charvec <- scan(prof_info, what = "character", quiet = TRUE)
    #transform plain text document with 2014 prof info into a vector of strings
    prof_info_charvec1 <- prof_info_charvec[!grepl("MBA", prof_info_charvec, fixed = TRUE)]
    #only want to estimate ages with undergrad degrees; can't have MBA interfering with BA
    prof_info_charvec2 <- prof_info_charvec1[!grepl("ABD", prof_info_charvec1, fixed = TRUE)]
    #only want to estimate ages with undergrad degrees; can't have ABD interfering with AB

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

}
