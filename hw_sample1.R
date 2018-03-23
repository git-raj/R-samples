#Part1
library(UsingR)
primes

#Use the diff function to compute the
#differences between successive primes.
diffPrime <- diff(primes, lag=1)
diffPrime

#Show the frequencies of these differences.
tabDiffP <- table(diffPrime)

#Show the barplot of these differences.
barplot(tabDiffP, main = 'Successive Prime nos. difference frequency',
        border = FALSE, col =rainbow(14), ylim = c(0,80))

#Part2
coins
coi <- table(coins)

#a) How many coins are there of each denomination?
sumCoin <- margin.table(coi,2)
sumCoin

#b) What is the total value of the coins for each denomination?

x <- apply(coi, 2, sum)
m1 <- matrix(x, ncol=1,nrow = 4, byrow=TRUE)
df<- data.frame(names(x), m1*as.numeric(names(x)))
colnames(df)<- c('coin denomination', 'total coin value')
df #answer

#c) What is the total value of all the coins?
apply(data.frame(coins$value), 2, sum)

#d) Show the barplot for the number of coins by year.
barplot(margin.table(coi,1), col = rainbow(64))

##Part3
south

#a) Show the stem plot of the data. What do you interpret from this plot?
stem(south)
##--> It looks like the murder rates is high in 10s for the southern cities.

#b) Show the five number summary of the data. Calculate the lower and
#upper ends of the outlier ranges. What are the outliers in the data?

f <- fivenum(south)
f   #fivenumber summary

c(f[2] - 1.5*(f[4] - f[2]), f[4] + 1.5*(f[4] - f[2]))
# Data below 1 or above 25 are outliers. In the data set, there is no
#lower end outliers but a few higher end outliers, 29 and 33.
south[south>=f[4] + 1.5*(f[4] - f[2])] #outliers on higher end

south[south<=f[2] - 1.5*(f[4] - f[2])] #outliers on lower end; none

#c) Show the horizontal boxplot of the data along with the appropriate labels on the plot.
boxplot(x=south, col=hcl(0), xaxt = "n",
        main = "murder summary",xlab='murders', horizontal = TRUE, add=FALSE)
axis(side = 1, at =f, labels = TRUE, las=2)

##Part4
pi2000

#a) How many times each of the digits 0 to 9 occur in this dataset?
table(pi2000)

#b) Show the percentages of their frequencies.
prop.table(table(pi2000))

#c) Show the histogram of the data.

hist(pi2000,breaks=seq(0,10,1), right=F, col=hcl(35),
     ylim=c(0,450), xlim = c(0,10),
     main= paste("Histogram of digits in first 2000 numbers of pi"),
     axes=TRUE, plot= TRUE, labels=TRUE,
     las=1)


#Part5
#a) Using cbind, create the matrix for the above data.

x<- cbind(c(25,20),c(10,40),c(15,30))
x
#b) Set the row names for the data.
rownames(x)<- c("Men", "Women")

#c) Set the column names for the data.
colnames(x)<- c("NFL","NBA","NHL")

#d) Now, add the dimension variables Gender and Sport to the data.
dimnames(x)<- list(Gender=rownames(x), Sport=colnames(x))
x
#e) Show the marginal distributions for the Gender and the Sport.
#margin distribution for gender
marginG<- margin.table(x,1)
marginG

#margin distribution for sport
marginS<- margin.table(x,2)
marginS

#f) Show the result of adding margins to the data.
y<- addmargins(x)
y

#g) Show the proportional data separately for Gender and Sport.
prop.table(x,1)

# proportional data for Sport
prop.table(x,2)

#h) Using appropriate colors, show the mosaic plot for the data. Also show
#the barplot for Gender and Sport separately with the bars side by side. Add
#legend to the plots.

#mosiac plot
mosaicplot(x, 
           main='US Sports by Gender',
           color = c('blue', 'pink', 'yellow'),
           las=1)

#side by side barplots
par(mfrow=c(1,2))
barplot(marginG, col=c('blue', 'pink'), ylim = c(0,100))
barplot(marginS, col=c('blue', 'pink', 'yellow'), ylim = c(0,60))

par(mfrow=c(1,1))

##Part6
midsize
#a) Show the pair wise plots for all the variables.
pairs(midsize, main='Midsize car prices', pch=20)


##Part7
MLBattend

head(MLBattend)
#a) Extract the wins for the teams BAL, BOS, DET, LA, PHI into the respective vectors.
#x<- subset(MLBattend,MLBattend$franchise %in% c('BAL', 'BOS', 'DET', 'LA', 'PHI'), select= c(franchise,wins))

sBAL<- subset(MLBattend,MLBattend$franchise %in% c('BAL'), select= c(franchise,wins))
BAL<- sBAL$wins

sBOS<- subset(MLBattend,MLBattend$franchise %in% c('BOS'), select= c(franchise,wins))
BOS <-sBOS$wins

sDET<- subset(MLBattend,MLBattend$franchise %in% c('DET'), select= c(franchise,wins))
DET<- sDET$wins

sLA<- subset(MLBattend,MLBattend$franchise %in% c('LA'), select= c(franchise,wins))
LA<- sLA$wins

sPHI<- subset(MLBattend,MLBattend$franchise %in% c('PHI'), select= c(franchise,wins))
PHI<- sPHI$wins

#b) Create a data frame of five columns using these vectors. Use the team names for the columns

df<- cbind(data.frame(BAL, BOS, DET, LA, PHI))
df

#c) Show the boxplot of the data frame.
boxplot(df, main='Franchise wins')


##Part8

senate <- read.csv('http://kalathur.com/cs544/data/senate.csv', header=TRUE)
senate
house <- read.csv('http://kalathur.com/cs544/data/house.csv', header=TRUE)
house

#a) How many members are there by party affiliation in the senate? How
#many members are there by party affiliation in the house?

#aS <- aggregate(senate$Party, by=list(senate$Party), FUN= length)

aS <- summary(senate$Party)
aS

aH <- summary(house$Party)
aH

#  b) Show the barplots of results from a). Use the appropriate colors for the
#respective parties (maybe, purple for independents)
par(mfrow=c(1,2))

barplot(aS, col = c('blue', 'purple', 'red'), 
        main='Senators by party affiliation')

barplot(aH, col = c('blue', 'red'), 
        main='Senators by party affiliation')


#c) Who is the longest serving member in the senate?
subset(senate, senate$Years_in_office == max(senate$Years_in_office))

#Who is the longest serving member in the house?
subset(house, house$Years_in_office == max(house$Years_in_office))

#  d) How many senators have their term ending in 2019?
#subset the senate table for all term ends containing 2019.
x <- subset(senate, senate$Term_ends %in% 
         grep('2019', senate$Term_ends, value = TRUE, fixed = TRUE))
x
#summarize the subset table to see the term ends for 2019
summary(x$Term_ends) ## In 2019, term ends for34 senators .


#gsub(pattern, replacement, x, ignore.case = FALSE, perl = FALSE,
#     fixed = FALSE, useBytes = FALSE)

#  e) How many representatives are there from each state?

x <- summary(house$State)
x
#  f) Which state has the highest number of representatives?
which(x== max(x)) #returns state name and the index in table

#  g) Which states have the least number of representatives?
which(x== min(x)) #returns state name and the index in table
