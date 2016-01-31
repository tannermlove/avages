age_hist1 <- function(x) {
  if (x == 2013) {
    hist(ages_1(x), seq(20, 100), col = "red", main = "Prof Ages in 2013",
      xlab = "Age", ylim = c(0, 20))
  }
  else if (x == 2012) {
    hist(ages_1(x), seq(20, 100), col = "yellow", main = "Prof Ages in 2012",
      xlab = "Age", ylim = c(0, 20))
  }
  else if (x == 2011) {
    hist(ages_1(x), seq(20, 100), col = "green", main = "Prof Ages in 2011",
      xlab = "Age", ylim = c(0, 20))
  }
  else if (x == 2010) {
    hist(ages_1(x), seq(20, 100), col = "violet", main = "Prof Ages in 2010",
      xlab = "Age", ylim = c(0, 20))
  }
  else if (x == 2009) {
    hist(ages_1(x), seq(20, 100), col = "orange", main = "Prof Ages in 2009",
      xlab = "Age", ylim = c(0, 20))
  }
  else if (x == 2008) {
    hist(ages_1(x), seq(20, 100), col = "blue", main = "Prof Ages in 2008",
      xlab = "Age", ylim = c(0, 20))
  }
  else if (x == 2007) {
    hist(ages_1(x), seq(20, 100), col = "pink", main = "Prof Ages in 2007",
      xlab = "Age", ylim = c(0, 20))
  }
  else if (x == 2006) {
    hist(ages_1(x), seq(20, 100), col = "cyan", main = "Prof Ages in 2006",
      xlab = "Age", ylim = c(0, 20))
  }
  else if (x == 2005) {
    hist(ages_1(x), seq(20, 100), col = "purple", main = "Prof Ages in 2005",
      xlab = "Age", ylim = c(0, 20))
  }
  else if (x == 2004) {
    hist(ages_1(x), seq(20, 100), col = 128, main = "Prof Ages in 2004",
      xlab = "Age", ylim = c(0, 20))
  }
  else if (x == 2014) {
    hist(ages_2(x), seq(20, 100), col = 644, main = "Prof Ages in 2014",
      xlab = "Age", ylim = c(0, 20))
  }
  else {
    stop('argument not an integer on [2004, 2014]')
  }
}
