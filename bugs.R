# closed bugs analisis

library("xlsx", lib.loc="~/R/win-library/3.4")

#bugs <- read.xlsx2(file = "closed_bugs.xlsx", sheetIndex = 1, header = TRUE, colClasses = c("numeric", "POSIXct","numeric", "character"))
bugs <- read.xlsx(file = "closed_bugs.xlsx", sheetIndex = 1)
bugs$hours <- as.numeric(bugs$hours)
#bugs$id <- as.numeric(bugs$id)
bugs <- bugs[bugs$hours!=0,]

saveRDS(bugs, "bugs.RDS")
#bugs <- readRDS("bugs.RDS")

median(bugs$hours)
bugs_m <-mean(bugs$hours)
bugs_m
bugs_sd <-sd(bugs$hours)
bugs_sd


# "три сигмы" по правому краю
mean(bugs$hours) + sd(bugs$hours)*3

# вероятности тех или иных событий при нормальном распределении
p_40 <- pnorm(40, mean = bugs_m, sd = bugs_sd)
p_16 <- pnorm(16, mean = bugs_m, sd = bugs_sd)
p_8 <- pnorm(8, mean = bugs_m, sd = bugs_sd)
p_4 <- pnorm(4, mean = bugs_m, sd = bugs_sd)

p_40 - p_16
p_16 - p_8
p_8 - p_4


#=================
  
# доля вклада багов разной сложности и вероятности появления багов разной сложности исходя из фактической частотности событий

total <- sum(bugs$hours)
#общее количество трудозатрат на баги за весь период
print(total)

# ---
# трудозатраты на простые баги их доля от общих трудозатрат
bugs_s <- sum(bugs$hours[bugs$hours<4])
print(bugs_s)
print(bugs_s/total)
# доля таких багов от общего количества
qs <- sum(bugs$hours<4)/nrow(bugs)
print(qs)

sum(bugs$hours<4)*4 - bugs_s

# трудозатраты на 4-часовые баги их доля от общих трудозатрат
bugs_m <- sum(bugs$hours[bugs$hours==4])
print(bugs_m)
print(bugs_m/total)
# доля таких багов от общего количества
qm <- sum(bugs$hours==4)/nrow(bugs)
print(qm)

# трудозатраты на сложные баги их доля от общих трудозатрат
bugs_b <- sum(bugs$hours[bugs$hours>4])
print(bugs_b)
print(bugs_b/total)
# доля таких багов от общего количества
qb <- sum(bugs$hours>4)/nrow(bugs)
print(qb)
# ---

bugs_sum <- matrix(0, ncol = 3, nrow = 4)
colnames(bugs_sum) <- c("t<4", "t=4", "t>4")
rownames(bugs_sum) <- c("N", "H (hours)", "N, %", "H, %")

# трудозатраты на простые баги их доля от общих трудозатрат
bugs_sum[1,1] <- sum(bugs$hours<4)
#тудозатраты на прострые баги
bugs_sum[2,1] <- sum(bugs$hours[bugs$hours<4])

# трудозатраты на баги средней сложности и их доля от общих трудозатрат
bugs_sum[1,2] <- sum(bugs$hours==4)
#тудозатраты 
bugs_sum[2,2] <- sum(bugs$hours[bugs$hours==4])

# трудозатраты на баги средней сложности и их доля от общих трудозатрат
bugs_sum[1,3] <- sum(bugs$hours>4)
#тудозатраты 
bugs_sum[2,3] <- sum(bugs$hours[bugs$hours>4])

# доля таких багов от общего количества в процентах
bugs_sum[3,] <- 100 * bugs_sum[1,]/nrow(bugs)
# доля трудозатрат от общих трудозатрат в процентах
bugs_sum[4,] <- 100 * bugs_sum[2,]/total

print(bugs_sum,digits = 3)

# ==== баланс

# фактически недоучтенное (недопланированное) время на спринтах
t1 <- bugs_sum[2,3] - bugs_sum[1,3]*4
print(t1)

# фактически перепланированное время на спринтах (время, которое было бы сэкономлено, если бы исходная оценка была стандартной - 4 часа)
t2 <- bugs_sum[1,1]*4 - bugs_sum[2,1]
print(t2)

# баланс в виде доли от общего объема в процентах
print(100*(t1-t2)/(total), digits = 3)

# контроль
sum(bugs$hours<4)/nrow(bugs) + sum(bugs$hours==4)/nrow(bugs)+ sum(bugs$hours>4)/nrow(bugs)

# ===== Черный лебедь

quantile(bugs$hours, probs = 0.5)

# 1-сигма
quantile(bugs$hours, probs = 0.683)
qnorm(0.683, mean = bugs_m, sd = bugs_sd)

# 2-сигмы
quantile(bugs$hours, probs = 0.954)
qnorm(0.954, mean = bugs_m, sd = bugs_sd)

# 3-сигмы
quantile(bugs$hours, probs = 0.997)
qnorm(0.997, mean = bugs_m, sd = bugs_sd)

# "black swan" probability (out of 3-sigma ) in percents
(1-pnorm(mean(bugs$hours) + 3*sd(bugs$hours), mean = mean(bugs$hours), sd = sd(bugs$hours)))*100
sum(bugs$hours>=16)/nrow(bugs)*100

# ================================
# сводная таблица по спринтам

stages <- bugs %>% group_by(sprint) %>% summarise(mean = mean(hours), median = median(hours), n = n(), total = sum(hours))

index <- seq(1,nrow(stages),1)
ggplot(data = stages) + theme_bw() + scale_x_continuous(breaks=seq(0,80,by=10)) +
  geom_line(aes(x = index, y = n))   
              
mean(stages$n)
median(stages$n)
sd(stages$n)

mean(stages$total)
median(stages$total)
sd(stages$total)

mean(stages$mean)
median(stages$mean)
sd(stages$mean)

sprints <- spread(bugs, sprint, hours)
sprints[is.na(sprints)] <- 0
sprints <- sprints[,-1]
sprints <- sprints[,-1]
sprints <- t(as.matrix(sprints))

def_est <- 4
sprints_s <- sprints
sprints_s[sprints_s>=def_est] <- 0
sprints_b <- sprints
sprints_b[sprints_b<=def_est] <- 0

sprints_stat <- cbind.data.frame(rowSums(sprints<def_est & sprints>0), 
                                 rowSums(sprints==def_est), 
                                 rowSums(sprints>def_est), 
                                 rowSums(sprints>0),
                                 rowSums(sprints_s), 
                                 rowSums(sprints_b), 
                                 rowSums(sprints))
rownames(sprints_stat) <- rownames(sprints)
colnames(sprints_stat) <- c("small", "middle", "big", "all", "total_s", "total_b", "total")

# сэкономленное время
sprints_stat$t1 <- sprints_stat$small*def_est - sprints_stat$total_s
# недопалнированное время
sprints_stat$t2 <- sprints_stat$big*def_est - sprints_stat$total_b

sprints_stat$balance <-  sprints_stat$t1 + sprints_stat$t2

# суммарный баланс по всем спринтам относительно общего объема трудозатрат в процентах (контрольно), должны выйти на цифру близкую к 0, как мы это видели раньше
print(100*abs(sum(sprints_stat$balance))/total, digits = 3)
# средний баланс
print(mean(sprints_stat$balance), digits = 2)
# медианный баланс
print(median(sprints_stat$balance), digits = 3)
# среднеквадратическое отклонение
print(sd(sprints_stat$balance), digits = 3)
# доли спринтов с положительным балансом
100*sum(abs(sprints_stat$balance)<=4)/nrow(sprints_stat)


ggplot(data = sprints_stat) + geom_histogram(aes(x = balance), fill = "gray70", colour = "gray10") +
  scale_x_continuous(breaks=seq(-60,60,by=5)) +
  theme_bw()

sprints_stat$name <- rownames(sprints_stat)
#sprints_stat$name[sprints_stat$name == "Sprint 8"] <- "Sprint 08"
sprints_stat <- arrange(sprints_stat, name)

index <- seq(1,nrow(sprints_stat),1)
ggplot(data = sprints_stat) + theme_bw() + scale_x_continuous(breaks=seq(0,80,by=10)) +
  #geom_line(aes(x = index, y = sprints_stat$total_s)) + 
  #geom_line(aes(x = index, y = sprints_stat$total_b)) +
  geom_line(aes(x = index, y = sprints_stat$t1), colour = "green", size = 0.5) +  # сэкономленное время
  geom_line(aes(x = index, y = sprints_stat$t2), colour = "red", size = 0.5) + # недопланированное время
  geom_line(aes(x = index, y = sprints_stat$balance), size = 1)
  


# ====================================
# гистрограмма с попыткой ручной нормализации

bugs_total <- bugs %>%
  group_by(hours) %>%
  summarise(total = sum(hours), n = n())
bugs_total$nf <- bugs_total$n/sum(bugs_total$n)

bugs_total$nfn[1] <- bugs_total$nf[1]/0.2
bugs_total$nfn[2] <- bugs_total$nf[2]/0.7
bugs_total$nfn[3] <- bugs_total$nf[3]/1
bugs_total$nfn[4] <- bugs_total$nf[4]/1.5
bugs_total$nfn[5] <- bugs_total$nf[5]/3
bugs_total$nfn[6] <- bugs_total$nf[6]/6
bugs_total$nfn[7] <- bugs_total$nf[7]/8
bugs_total$nfn[8] <- bugs_total$nf[8]/12
bugs_total$nfn[8] <- bugs_total$nf[8]/28


#   ===============================
#   моделируем выборку с усреднением по бинам, проверяем по критерию Хи-квадрат (или критерий Пирсона)

z <- rnorm(nrow(bugs), mean = mean(bugs$hours), sd = sd(bugs$hours))
mean(z)
sd(z)
sum(z>=16)
sum(z>=12)


# 0, 0.4, 0.6, 1.4, 2.7, 5.2, 

chi_c <- c(pnorm(0.2, mean = bugs_m, sd = bugs_sd),
           pnorm(0.75, mean = bugs_m, sd = bugs_sd) - pnorm(0.2, mean = bugs_m, sd = bugs_sd),
           pnorm(1.5, mean = bugs_m, sd = bugs_sd) - pnorm(0.75, mean = bugs_m, sd = bugs_sd),
           pnorm(3, mean = bugs_m, sd = bugs_sd) - pnorm(1.5, mean = bugs_m, sd = bugs_sd),
           pnorm(6, mean = bugs_m, sd = bugs_sd) - pnorm(3, mean = bugs_m, sd = bugs_sd),
           pnorm(12, mean = bugs_m, sd = bugs_sd) - pnorm(6, mean = bugs_m, sd = bugs_sd),
           pnorm(20, mean = bugs_m, sd = bugs_sd) - pnorm(12, mean = bugs_m, sd = bugs_sd),
           1 - pnorm(20, mean = bugs_m, sd = bugs_sd))
           
#chi_c <- chi_c*nrow(bugs)

chi_o <- c(sum((z<=0.25)), 
             sum((z>0.25) & (z<=0.75)), 
             sum((z>0.75) & (z<=1.5)), 
             sum((z>1.5) & (z<=3)), 
             sum((z>3) & (z<=6)), 
             sum((z>6) & (z<=12)),
             sum((z>12) & (z<=20)), 
             sum((z>20)) )


# K = 18.54, p = 0.05

sum(((chi_o[1:8] - chi_c[1:8]*length(z))^2)/(chi_c[1:8]*length(z)))
chisq.test(x = chi_o, p = chi_c)

chi_o <- c(sum(bugs$hours==0.1), 
             sum(bugs$hours==0.5), 
             sum(bugs$hours==1), 
             sum(bugs$hours==2), 
             sum(bugs$hours==4), 
             sum(bugs$hours==8),
             sum(bugs$hours==16), 
             sum(bugs$hours>=24))
#chi_o <- chi_o/nrow(bugs)


sum(((chi_o[1:8] - chi_c[1:8]*length(z))^2)/(chi_c[1:8]*length(z)))
sum(((chi_o[1:7] - chi_c[1:7]*length(z))^2)/(chi_c[1:7]*length(z)))
sum(((chi_o[2:6] - chi_c[2:6]*length(z))^2)/(chi_c[2:6]*length(z)))

chisq.test(x = chi_o, p = chi_c)

# chisq.test(x = chi_o[2:6], y = chi_c[2:6])

# ===============


ggplot() + geom_histogram(aes(x = z), fill = "gray70", colour = "gray10") +
  scale_x_continuous(breaks=seq(0,41,by=1)) +
  theme_bw()

# =========

z[(z<=0)] <- 0 
z[(z>0) & (z<=0.25)] <- 0.1 
z[(z>0.25) & (z<=0.75)] <- 0.5 
z[(z>0.75) & (z<=1.5)] <- 1
z[(z>1.5) & (z<=3)] <- 2 
z[(z>3) & (z<=6)] <- 4 
z[(z>6) & (z<=12)] <- 8
z[(z>12) & (z<=20)] <- 16 
z[(z>20) & (z<=32)] <- 24 
z[(z>32) & (z<=60)] <- 40
z <- z[z>0]

z <- rnorm(1000, mean = 0, sd = 4)
mean(z)
sd(z)
sum(z>=15)
sum(z>=10)
skewness(z)
kurtosis(z)
jarque.test(z)
shapiro.test(z)
ggplot() + geom_histogram(aes(x = z), fill = "gray70", colour = "gray10") +
  scale_x_continuous(breaks=seq(-15,15,by=1)) +
  theme_bw()

z[(z>-15) & (z<=-10)] <- mean(z[(z>-15) & (z<=-10)]) 
z[(z>-10) & (z<=-9)] <- mean(z[(z>-10) & (z<=-9)])
z[(z>-9) & (z<=-8)] <- mean(z[(z>-9) & (z<=-8)]) 
z[(z>-8) & (z<=-7)] <- mean(z[(z>-8) & (z<=-7)]) 
z[(z>-7) & (z<=-6)] <- mean(z[(z>-7) & (z<=-6)]) 
z[(z>-6) & (z<=-5)] <- mean(z[(z>-6) & (z<=-5)])
z[(z>-5) & (z<=-4)] <- mean(z[(z>-5) & (z<=-4)]) 
z[(z>-4) & (z<=-3)] <- mean(z[(z>-4) & (z<=-3)])
z[(z>-3) & (z<=-2)] <- mean(z[(z>-3) & (z<=-2)]) 
z[(z>-2) & (z<=-1)] <- mean(z[(z>-2) & (z<=-1)]) 
z[(z>-1) & (z<=0)] <- mean(z[(z>-1) & (z<=0)]) 
z[(z>0) & (z<=1)] <- mean(z[(z>0) & (z<=1)]) 
z[(z>1) & (z<=2)] <- mean(z[(z>1) & (z<=2)])
z[(z>2) & (z<=3)] <- mean(z[(z>2) & (z<=3)]) 
z[(z>3) & (z<=4)] <- mean(z[(z>3) & (z<=4)])
z[(z>4) & (z<=5)] <- mean(z[(z>4) & (z<=5)]) 
z[(z>5) & (z<=6)] <- mean(z[(z>5) & (z<=6)]) 
z[(z>6) & (z<=7)] <- mean(z[(z>6) & (z<=7)]) 
z[(z>7) & (z<=8)] <- mean(z[(z>7) & (z<=8)]) 
z[(z>8) & (z<=9)] <- mean(z[(z>8) & (z<=9)])
z[(z>9) & (z<=10)] <- mean(z[(z>9) & (z<=10)]) 
z[(z>10) & (z<=15)] <- mean(z[(z>10) & (z<=15)])

mean(z)
sd(z)
skewness(z)
kurtosis(z)
jarque.test(z)
shapiro.test(z)


z[(z>-15) & (z<=-10)] <- mean(z[(z>-15) & (z<=-10)]) 
z[(z>-10) & (z<=-7)] <- mean(z[(z>-10) & (z<=-7)])
z[(z>-7) & (z<=-5)] <- mean(z[(z>-7) & (z<=-5)]) 
z[(z>-5) & (z<=-3)] <- mean(z[(z>-5) & (z<=-3)]) 
z[(z>-3) & (z<=-2)] <- mean(z[(z>-3) & (z<=-2)]) 
z[(z>-2) & (z<=-1)] <- mean(z[(z>-2) & (z<=-1)])
z[(z>-1) & (z<=-0.5)] <- mean(z[(z>-1) & (z<=-0.5)])
z[(z>-0.5) & (z<=0)] <- mean(z[(z>-0.5) & (z<=0)]) 
z[(z>0) & (z<=0.5)] <- mean(z[(z>0) & (z<=0.5)]) 
z[(z>0.5) & (z<=1)] <- mean(z[(z>0.5) & (z<=1)])
z[(z>1) & (z<=2)] <- mean(z[(z>1) & (z<=2)])
z[(z>2) & (z<=3)] <- mean(z[(z>2) & (z<=3)]) 
z[(z>3) & (z<=5)] <- mean(z[(z>3) & (z<=5)])
z[(z>5) & (z<=7)] <- mean(z[(z>5) & (z<=7)]) 
z[(z>7) & (z<=10)] <- mean(z[(z>7) & (z<=10)])
z[(z>10) & (z<=15)] <- mean(z[(z>10) & (z<=15)])

# ================

# просто гистограмма в виде плотности вероятности
ggplot(data = bugs) + geom_histogram(aes(x = hours), fill = "gray70", colour = "gray10") +
  scale_x_continuous(breaks=seq(0,41,by=1)) +
  theme_bw()

# ==============

# гистограмма с суммой часов
ggplot(data = bugs) + geom_col(aes(x = hours, y = hours), fill = "gray70", colour = "gray10") +
  scale_x_continuous(breaks=seq(0,41,by=1)) +
  theme_bw()


# плотность вероятности 
x <- seq(0,40,0.1)
y <- dnorm(x, mean = mean(bugs$hours), sd = sd(bugs$hours))

ggplot() + theme_bw() +
  geom_line(aes(x, y)) +
  geom_line(aes(x = bugs_total$hours, y = bugs_total$nf)) +
  scale_x_continuous(breaks=seq(0,41,by=1))


ggplot() + 
  geom_line(aes(x, y)) +
  geom_line(aes(x = bugs_total$hours, y = bugs_total$nfn)) + theme_bw() +
  scale_x_continuous(breaks=seq(0,41,by=1))

ggplot() + 
  geom_line(aes(x = bugs_total$hours, y = model_h)) + theme_bw() +
  scale_x_continuous(breaks=seq(0,41,by=1))

ggplot() + 
  geom_line(aes(x = bugs_total$hours, y = bugs_total$nfn)) + theme_bw() +
  geom_line(aes(x = bugs_total$hours, y = model_h)) + theme_bw() +
  scale_x_continuous(breaks=seq(0,41,by=1))


# тестирование на нормальность

library(moments)
skewness(bugs$hours)
kurtosis(bugs$hours)

# http://www.statisticshowto.com/jarque-bera-test/
# https://ru.wikipedia.org/wiki/%D0%A2%D0%B5%D1%81%D1%82_%D0%A5%D0%B0%D1%80%D0%BA%D0%B5_%E2%80%94_%D0%91%D0%B5%D1%80%D0%B0
# https://www.rdocumentation.org/packages/tsoutliers/versions/0.3/topics/jarque.bera.test


library(nortest)
library(stats)

jarque.test(bugs$hours)
shapiro.test(bugs$hours)



# ===============


x2 <- seq(0.0, 0.99, 0.01)
y2 <- qnorm(x2, mean = mean(bugs$hours), sd = sd(bugs$hours))
y3 <- quantile(bugs$hours, probs = x2)  
ggplot() + geom_line(aes(x2, y2)) + geom_line(aes(x2, y3)) + theme_bw() +
  scale_x_continuous(breaks=seq(0,1,by=0.1)) 
  
