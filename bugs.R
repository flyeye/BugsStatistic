# closed bugs analisis

library("xlsx", lib.loc="~/R/win-library/3.4")

bugs <- read.xlsx(file = "closed_bugs.xlsx", sheetIndex = 1, header = TRUE )

median(bugs$hours)
mean(bugs$hours)
sd(bugs$hours)

# "black swan" probability (out of 3-sigma ) 
(1-pnorm(mean(bugs$hours) + 3*sd(bugs$hours), mean = mean(bugs$hours), sd = sd(bugs$hours)))*100

quantile(bugs$hours, probs = 0.5)
quantile(bugs$hours, probs = 0.99)

library(moments)
skewness(bugs$hours)
kurtosis(bugs$hours)

# http://www.statisticshowto.com/jarque-bera-test/
# https://ru.wikipedia.org/wiki/%D0%A2%D0%B5%D1%81%D1%82_%D0%A5%D0%B0%D1%80%D0%BA%D0%B5_%E2%80%94_%D0%91%D0%B5%D1%80%D0%B0
# https://www.rdocumentation.org/packages/tsoutliers/versions/0.3/topics/jarque.bera.test

jarque.test(bugs$hours)

library(stats)
shapiro.test(bugs$hours)

library(nortest)


ggplot(data = bugs) + geom_histogram(aes(x = hours), fill = "gray70", colour = "gray10") +
  scale_x_continuous(breaks=seq(0,41,by=1)) +
  theme_bw()

x <- seq(0,40,0.1)
y <- dnorm(x, mean = mean(bugs$hours), sd = sd(bugs$hours))

ggplot() + geom_line(aes(x, y)) + theme_bw() +
  scale_x_continuous(breaks=seq(0,41,by=1)) 

x2 <- seq(0.0, 0.99, 0.01)
y2 <- qnorm(x2, mean = mean(bugs$hours), sd = sd(bugs$hours))
y3 <- quantile(bugs$hours, probs = x2)  
ggplot() + geom_line(aes(x2, y2)) + geom_line(aes(x2, y3)) + theme_bw() +
  scale_x_continuous(breaks=seq(0,1,by=0.1)) 
  
