graphy <- function() {
  ages1 <- c(avg_age(2004), avg_age(2005), avg_age(2006), avg_age(2007), avg_age(2008), avg_age(2009), avg_age(2010), avg_age(2011), avg_age(2012), avg_age(2013), avg_age(2014));
  ggplot2::qplot(seq(2004:2014), ages1)
}

