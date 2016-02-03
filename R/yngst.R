#' Youngest professor at Williams College
#'
#' Returns the name(s) and age(s) of the youngest professor(s) at Williams college in a year from 2004 to 2014.
#'
#' For this to work properly, argument must be an integer representing a year from 2004 to 2014.
#'
#'@param \code{x} An integer on [2004, 2014]
#'


yngst <- function(x) {
    if (x == 2013) {
        paste("Daniel Greenberg, age", min(ages_1(x)))
    } else if (x == 2012) {
        paste("Daniel Greenberg, age", min(ages_1(x)))
    } else if (x == 2011) {
        paste("Daniel Greenberg age", min(ages_1(x)))
    } else if (x == 2010) {
        paste("Donald Brooks, Marshall K. Creighton, and Christopher Himes, age", min(ages_1(x)))
    } else if (x == 2009) {
        paste("Nicholas T. Goodbody and Karen Russell, age", min(ages_1(x)))
    } else if (x == 2008) {
        paste("Charles N. Howard, age", min(ages_1(x)))
    } else if (x == 2007) {
        paste("Robert Michelin, age", min(ages_1(x)))
    } else if (x == 2006) {
        paste("Zafrir Levy, age", min(ages_1(x)))
    } else if (x == 2005) {
        paste("Zafrir Levy, age", min(ages_1(x)))
    } else if (x == 2004) {
        paste("Zafrir Levy, age", min(ages_1(x)))
    } else if (x == 2014) {
        paste("Sarah A. Mirseyedi, age", min(ages_2(x)))
    } else {
        stop("argument not an integer on [2004, 2014]")
    }
}

