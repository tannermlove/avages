#' Oldest professor at Williams College
#'
#' Returns the name(s) and age(s) of the oldest professor(s) at Williams college in a year from 2004 to 2014.
#'
#' For this to work properly, argument must be an integer representing a year from 2004 to 2014.
#'
#'@param \code{x} An integer on [2004, 2014]
#'

oldst <- function(x) {
  if (x == 2013) {
    paste("Henry J. Bruton, age", max(ages_1(x)))
  }
  else if (x == 2012) {
    paste("Henry J. Bruton, age", max(ages_1(x)))
  }
  else if (x == 2011) {
    paste("Henry J. Bruton, age", max(ages_1(x)))
  }
  else if (x == 2010) {
    paste("Henry J. Bruton, age", max(ages_1(x)))
  }
  else if (x == 2009) {
    paste("Henry J. Bruton, age", max(ages_1(x)))
  }
  else if (x == 2008) {
    paste("Henry J. Bruton, age", max(ages_1(x)))
  }
  else if (x == 2007) {
    paste("Henry J. Bruton, age", max(ages_1(x)))
  }
  else if (x == 2006) {
    paste("Henry J. Bruton, age", max(ages_1(x)))
  }
  else if (x == 2005) {
    paste("Henry J. Bruton, age", max(ages_1(x)))
  }
  else if (x == 2004) {
    paste("Henry J. Bruton, age", max(ages_1(x)))
  }
  else if (x == 2014) {
    paste("Charles B. Dew and Donald deB. Beaver, age", max(ages_2(x)))
  }
  else {
    stop('argument not an integer on [2004, 2014]')
  }
}
