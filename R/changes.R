age_change <- function() {
  ages1 <- c(avg_age(2004), avg_age(2005), avg_age(2006), avg_age(2007), avg_age(2008), avg_age(2009), avg_age(2010), avg_age(2011), avg_age(2012), avg_age(2013), avg_age(2014))
  # generate mean ages as y values

  years1 <- 2004:2014
  # generate x values

  profdata <- data.frame(years1, ages1)
  # create a data frame

  library(ggplot2, warn.conflicts = FALSE, quietly = TRUE)
  plot <- ggplot(profdata, aes(years1, ages1)) + geom_point(color="darkviolet")
  plot <- plot + ggtitle("Change in Mean Age from 2004 to 2014")
  plot <- plot + theme(plot.title = element_text(size = 15, face = "bold", vjust = 1, hjust = 0.5))
  #title graph

  plot <- plot + labs(x="Year", y="Age (yrs)")
  # label axes

  plot <- plot + theme(axis.text.x = element_text(angle = 50, size = 10, face = "bold", color = "darkviolet"))
  plot <- plot + theme(axis.title.x = element_text(face = "bold", hjust = 0.5, vjust = 0.8))
  # format x axis

  plot <- plot + theme(axis.text.y = element_text(size = 10, face = "bold", color = "darkviolet"))
  plot <- plot + theme(axis.title.y = element_text(face = "bold", hjust = 0.5, vjust = 0.8))
  plot <- plot + ylim(c(0, 60))
  # format y axis

  plot <- plot + theme(plot.background = element_rect(fill = "white"))
  plot <- plot + theme(panel.background = element_rect(fill = "gold1"))
  # format plot/panel backgrounds

  plot
}



