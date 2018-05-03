#Boston "Hubway" trip data for March 2018
par(mfrow = c(1, 1))

#saving csv file into RDS

hubway_data <-
  read.csv2(
    "201803_hubway_tripdata.csv",
    header = TRUE,
    sep = ",",
    quote = "\"'"
  )
saveRDS(hubway_data, "hubway_tripdata.rds")

data1 <- readRDS("hubway_tripdata.rds")

summary(data1$tripduration / 60)


##highest frequency stations for trip start
freq1 <- 1000
st1000 <-table(data1$start.station.name)[table(data1$start.station.name) > freq1]
df1 <- data.frame(st1000)

barplot(
  st1000,
  col = "green",
  ylim = c(0, 2500),
  las = 2,
  ylab = 'Trip Start Frequency',
  cex.names = 0.35,
  cex.axis = 0.7,
  main = paste("Trip Starts w. Freq greater than ", freq1)
)

k <- subset(data1, data1$start.station.name %in% df1$Var1)
summary(k$tripduration / 60)

##low range frequency stations for trip start
freq2 <- 500
stlow <-table(data1$start.station.name)[table(data1$start.station.name) < freq2]
df2 <- data.frame(stlow)

barplot(
  stlow,
  col = "green",
  ylim = c(0, 500),
  las = 2,
  ylab = 'Trip Start Frequency',
  cex.names = 0.35,
  cex.axis = 0.7,
  main = paste("Trip Starts w. Freq less than ", freq2)
)

k1 <- subset(data1, data1$start.station.name %in% df2$Var1)
summary(k1$tripduration / 60)

#by Gender
x <- table(data1$gender)

#pie charting gender proportion
slice.labels <- c('Unidentified', 'Male', 'Female')
slice.percent <- round((x / sum(x) * 100))
slice.labels <- paste(slice.labels, slice.percent, "%")

pie(x, labels = slice.labels,
    col = c('purple', 'blue', 'pink'),
    main = "Trips vs Gender")


#by Birth year
x2 <- subset(data1,!data1$birth.year %in% 'NULL')
x21 <- table(x2$birth.year)

barplot(
  x21,
  col = rainbow(40),
  ylab = 'trip freq',
  ylim = c(0, 4000),
  xlab = 'birth year',
  main = 'Trips vs. Demographics'
)


#By time of the day
cout.df <- cbind(data1, counter = 1)

freq.df <-
  aggregate(cout.df$counter ~ format(as.POSIXct(data1$starttime),
                                     "%H"), data = data1, sum)

colnames(freq.df) <- c("hourofDay", "freq")

freq.df1 <- transform(freq.df,
                      hourofDay = as.numeric(hourofDay),
                      freq = as.numeric(freq))

barplot(
  freq.df1$freq,
  col = rainbow(40),
  ylab = 'trip freq',
  beside = TRUE,
  ylim = c(0, 8000),
  las = 1,
  xlab = "hour of day",
  main = 'Trips vs. time of day',
  names.arg = freq.df1$hourofDay
)


#boxplot of trip duration
boxplot(data1$tripduration, main = "Entire dataset trip duration Boxplot")

freq2.df <-
  aggregate(data1$tripduration / (60 * 60) ~ format(as.POSIXct(data1$starttime),
                                                    "%D"),
            data = data1,
            sum)

colnames(freq2.df) <- c("Day", "Time(hrs)")

#temp data obtained from USClimate site
#https://www.usclimatedata.com/climate/boston/massachusetts/united-states/usma0046/2018/3

temp <-
  c(
    60.1,
    43,
    43,
    41,
    44.1,
    37,
    39,
    37.9,
    37,
    39.9,
    44.1,
    42.1,
    41,
    43,
    37.9,
    42.1,
    33.1,
    37.9,
    39,
    39,
    42.1,
    42.1,
    46,
    39,
    39.9,
    42.1,
    54,
    50,
    64,
    57
  )

#length(freq2.df$Day)
#length(temp)
freq3.df <- cbind(freq2.df, temp)
#freq3.df


xx<- barplot(
  freq3.df$`Time(hrs)`,
  ylab = 'trip Hours',
  beside = TRUE,
  ylim = c(0, 2000),
  las = 1,
  xlab = "Day of month",
  main = 'Day Trip freq. vs Day with daily temperature reference'
)
text(x = xx, y = freq3.df$temp, label = freq3.df$temp, pos = 3, cex = 0.8, col = "red")
axis(1, at=xx, labels=freq3.df$Day, tick=FALSE, las=2, line=-0.5, cex.axis=0.5)

#Boxplots for specific dates, Minutes of trip
sub.data2 <-
  subset(data1, format(as.POSIXct(data1$starttime), "%D") %in% '03/02/18')
boxplot(sub.data2$tripduration/60, main="Trip Duration(minutes) on 03/02/18")

sub.data15 <-
  subset(data1, format(as.POSIXct(data1$starttime), "%D") %in% '03/15/18')
boxplot(sub.data15$tripduration/60, main="Trip Duration(minutes) on 03/15/18")

sub.data31 <-
  subset(data1, format(as.POSIXct(data1$starttime), "%D") %in% '03/31/18')
boxplot(sub.data31$tripduration/60, main="Trip Duration(minutes) on 03/31/18")


# central limit theorem
##
par(mfrow = c(2, 2))

samples <- 10000
xbar <- numeric(samples)

for (size in c(10000, 15000, 20000, 25000)) {
  for (i in 1:samples) {
    xbar[i] <-
      mean(sample(data1$tripduration/60, size = size, replace = TRUE))
    
  }
  
  hist(
    xbar,
    prob = TRUE,
    ylim = c(0, 1),
    main = paste("Sample Size =", size)
  )
  
  cat("Sample Size = ",
      size,
      " Mean = ",
      mean(xbar),
      " SD = ",
      sd(xbar),
      "\n")
}




###
#Sampling
library(sampling)

# srswr
set.seed(100)
s <- srswr(70, nrow(data1))

rows <- (1:nrow(data1))[s != 0]
rows <- rep(rows, s[s != 0])

sample.1 <- data1[rows,]
head(sample.1)
boxplot(sample.1$tripduration/60, main='srswr(n=70)- trip duration in minutes')


# srswor
s <- srswor(70, nrow(data1))

rows2 <- (1:nrow(data1))[s != 0]
rows2 <- rep(rows2, s[s != 0])

sample.2 <- data1[rows2,]
boxplot(sample.2$tripduration/60, main='srswor(n=70)- trip duration in minutes')

#Systematic sampling
N <- nrow(data1)
n <- 100

k <- floor(N / n)
k
r <- sample(k, 1)
r

# select every kth item
s <- seq(r, by = k, length = n)

sample.3 <- data1[s,]
boxplot(sample.3$tripduration/60, main='Systematic sampling(n=100)- trip duration in minutes')

##
#Stratified Sampling
st.1 <- strata(
  data1,
  stratanames = c("gender"),
  size = rep(3, 4),
  method = "srswor",
  description = TRUE
)
st.1

st.sample1 <- getdata(data1, st.1)
head(st.sample1)
boxplot(st.sample1$tripduration/60, main='Stratified Sampling- trip duration in minutes')

par(mfrow = c(1, 1))

#Cluster Sampling
cl <- cluster(data1, c("gender"), size = 2,
              method = "srswor")

cl.sample <- getdata(data1, cl)
boxplot(cl.sample$tripduration/60, main='Cluster Sampling- trip duration in minutes')

#by Gender
c1x <- table(cl.sample$gender)

#pie charting gender proportion
slice.labels <- c('Unidentified', 'Male', 'Female')
slice.percent <- round((c1x / sum(c1x) * 100))
slice.labels <- paste(slice.labels, slice.percent, "%")

pie(c1x, labels = slice.labels, col = c('purple', 'blue', 'pink'),
    main = "Cluster sampling with 2 random gender picks on the data set")



###plot confidence
plot.confidence <- function (conf = 90) {
  alpha <- 1 - conf/100
  z <- qnorm(1 - alpha/2)
  print(z, digits=4)
  
  f1 <- curve(dnorm(x), from=-3, to=3, 
              xaxt="n", yaxt="n",
              xlab = "z")
  
  title(paste("Confidence =", conf, "%"))
  
  axis(side=1, at=c(-z, 0, z), las=0,
       labels=formatC(c(-z, 0, z), digits=3))
  
  polygon(f1$x, f1$y, col="lightblue")
  
  x.1 <- seq(-3, -z, by = 0.05)
  y.1 <- dnorm(x.1)
  x.1 <- c(x.1, -z, -z)
  y.1 <- c(y.1, dnorm(-z), dnorm(-3))
  
  polygon(x.1, y.1, col="white")
  
  x.2 <- seq(3, z, by = -0.05)
  y.2 <- dnorm(x.2)
  
  x.2 <- c(x.2, z, z)
  y.2 <- c(y.2, dnorm(z), dnorm(3))
  
  polygon(x.2, y.2, col="white")
  
  # lines(c(0,0), c(dnorm(-3), dnorm(0)), lty=2)
  
  text(0, 0.2, 1-alpha)
  text(-2.6, 0.2, alpha/2)
  text(2.6, 0.2, alpha/2)
  
  return (z)
}

#confidence intervals for 80 and 90%

pop.mean <- mean(data1$tripduration/60)
pop.sd <- sd(data1$tripduration/60)

conf <- c(80, 90)
conf

alpha <- 1 - conf / 100
alpha

qnorm(alpha / 2)
qnorm(1 - alpha / 2)

for (i in alpha) {
  str <- sprintf("%2d%% Conf Level (alpha = %.2f), z: %.2f , %.2f",
                 100 * (1 - i),
                 i,
                 qnorm(i / 2),
                 qnorm(1 - i / 2))
  cat(str, "\n")
}


par(mfrow = c(1, 2))
for (i in conf) {
  plot.confidence(i)
}


sample.size <- 1000

sample.data <- sample(data1$tripduration/60, size = sample.size)

sd.sample.means <- pop.sd / sqrt(sample.size)
sd.sample.means

xbar <- mean(sample.data)
xbar

#CI for the confidence levels 80%, 90%
for (i in alpha) {
  str <- sprintf(
    "%2d%% Conf Level (alpha = %.2f), CI = %.2f - %.2f",
    100 * (1 - i),
    i,
    xbar - qnorm(1 - i / 2) * sd.sample.means,
    xbar + qnorm(1 - i / 2) * sd.sample.means
  )
  cat(str, "\n")
}

zvals <- qnorm(1 - alpha / 2)
par(mfrow = c(1, 2))

samples <- 20

for (z in zvals) {
  for (i in 1:samples) {
    sample.data.1 <- sample(data1$tripduration/60, size = sample.size)
    xbar[i] <- mean(sample.data.1)
    str <- sprintf(
      "%2d: xbar = %.2f, CI = %.2f - %.2f",
      i,
      xbar[i],
      xbar[i] - 2 * sd.sample.means,
      xbar[i] + 2 * sd.sample.means
    )
    cat(str, "\n")
    }
  
  xbar
  matplot(
    rbind(xbar - z * 2 * sd.sample.means, xbar + z * 2 * sd.sample.means),
    rbind(1:samples, 1:samples),
    type = "l",
    lty = 1
  )
  abline(v = pop.mean)
  points(xbar, 1:samples, pch = 19)
  
}


## saving project workspace after the fact
save.image(file = "lamichhane_finalproj.RData")
