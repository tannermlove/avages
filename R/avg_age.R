#' Average age of professors/lecturers at Williams College.
#'
#' Returns the average age of professors and lecturers at Williams College in any year from 2004 to 2014.
#'
#' For this to work properly, argument must be an integer representing a year from 2004 to 2014.
#'
#'@param \code{x} An integer on [2004, 2014]
#'

avg_age <- function(x) {
    if (x == 2004 || x == 2005 || x == 2006 || x == 2007 || x == 2008 || x == 2009 ||
        x == 2010 || x == 2011 || x == 2012 || x == 2013) {
        mean(ages_1(x))
    } else if (x == 2014) {
        mean(ages_2(x))
    } else {
        stop("argument not an integer on [2004, 2014]")
    }
}
