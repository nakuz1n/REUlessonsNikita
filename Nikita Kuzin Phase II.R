#libraries
library(readr)
library(ggplot2)

#Working directory
getwd()
setwd("/Users/nakuzin/KU/Biology Research/Visualization")

# Functions
# Vadim's function
serialAgg=function (x, AggCats, AggTarg = NULL, FUN = function(x) mean(x, na.rm = TRUE)){
  if (is.null(AggTarg)) {
    if (is.numeric(AggCats)) 
      AggTarg = (1:ncol(x))[-AggCats]
    if (is.character(AggCats)) 
      AggTarg = colnames(x)[!colnames(x) %in% AggCats]
  }
  Numbers = prod(apply(t(t(x[, AggCats])), 2, is.numeric))
  ncat = length(AggCats)
  if (ncat == 1) 
    Cats = as.character(x[, AggCats])
  else Cats = codify(x[, AggCats])
  agged = as.matrix(aggregate(x[, AggTarg], by = list(Cats), FUN = FUN))
  if (ncat > 1) 
    agged = cbind(matrix(unlist(strsplit(agged[, 1], 
                                         "_")), ncol = ncat, byrow = TRUE), matrix(agged[, 
                                                                                         -1], ncol = ncol(agged) - 1))
  if (Numbers) 
    agged = t(apply(agged, 1, as.numeric))
  colnames(agged) = colnames(cbind(x[, c(AggCats[1], AggCats)], 
                                   x[, c(AggTarg, AggTarg[1])]))[c(1, 3:(ncol(agged) + 1))]
  agged
}

codify=function (x, cols = 1:ncol(x), sep = "_") 
  as.matrix(cbind(Index = apply(x[, cols], 1, paste0, collapse = sep),  x[, -cols]))

# My function which plots the biomass over time for each Latitude between 36 and 44
biomassbyLat <- function(data, species){
  # new df of your species only
  df <- data[which(data$spnm == species),]
  # add a column of the Latitude rounded to a degree
  df$latr <- round(df$DECDEG_BEGLAT)
  # change a EXPCATCHWT to log(1+EXPCATCHWT) column
  df$logEXPCATCHWT <- log(1+df$EXPCATCHWT)
  # density and biomass agreggated by year and Latitude
  biomass_yl <- serialAgg(df, AggCats = c("GMT_YEAR", "latr", "fall"), AggTarg = "logEXPCATCHWT")
  # create a list of numerical and character Latitudes except for 35
  Lat <- sort(unique(biomass_yl[,2]))
  Lat <- Lat[-1]
  Lat_chr <- as.character(Lat)
  # split the screen
  par(mfrow=c(3,3))
  # for() loop that plots the biomass over time for each Latitude between 36 and 44
  for (i in Lat) {
    # create a character main title for the plot
    main_title <- paste("Latitude =", Lat_chr[i-35])
    # plot biomass in the fall for Latitude i
    plot(biomass_yl[biomass_yl[,2] == i & biomass_yl[,3] == 1, c(1,4)], type = "l", lwd=2, 
         ylim = c(min(biomass_yl[,4]), max(biomass_yl[,4])),
         main = main_title,
         xlab = "Year",
         ylab = "Biomass")
    # add biomass in the spring for latitude i
    lines(biomass_yl[biomass_yl[,2] == i & biomass_yl[,3] == 0, c(1,4)], lwd=2, col = 2)
    # add a horizontal line at year 2007
    abline(v=2007.5,col=4,lty=2)
  }
}

# import data
data <- read.csv("/Users/nakuzin/KU/Biology Research/Data/NOAA_EcosystemData_10-6-2023.csv")

# list of species
ListofSpecies <- c("SMOOTH DOGFISH", "SEA RAVEN", "NORTHERN SEAROBIN", 
                   "GOOSEFISH", "AMERICAN LOBSTER", "SEA SCALLOP", 
                   "LONGFIN SQUID", "LONGHORN SCULPIN", "NORTHERN SHORTFIN SQUID")

#plot all the species
for (species in ListofSpecies){
  biomassbyLat(data, species)
  print(species)
}

par(mfrow=c(1,1))

#fasteR Lesson 16

# function to calculate the mean of elements greater than 
mgd <- function(x,d) mean(x[x > d]) 

mgd(Nile, 1200)

# function to calculate the range of a vector
rng <- function(y) max(y) - min(y)
rng(Nile)

# function to count the number of elements greater than
cgd <- function(x, d) sum(x > d)
cgd(Nile, 1200)

# function to count the number of zeros in a vector
n0 <- function(x) sum(x == 0)
n0(Nile)

# function to draw a histogram for elements less than a specified value
hld <- function(x, d) hist(x[x < d])
hld(Nile, 1200)

# Save the 'mgd'
save(mgd, file = 'mean_greater_than_d')

# Load the 'mgd'
load('mean_greater_than_d')

# Print the 'mgd'
mgd


#fasteR Lesson 17

pima <- read.csv('http://heather.cs.ucdavis.edu/FasteR/data/Pima.csv',header=TRUE)


# for loop for number of 0 in each column
for (i in 1:9) print(sum(pima[,i] == 0))

#replace 0's with N/A
for (i in 2:6) pima[pima[,i] == 0,i] <- NA

for (i in 2:6) {
  zeroIndices <- which(pima[,i] == 0)
  pima[zeroIndices,i] <- NA
}

#function breaks after first sum that exceeds s
f <- function(n,s) 
{
  tot <- 0
  for (i in 1:n) {
    tot <- tot + i^3
    if (tot > s) {
      print(i)
      break
    }
    if (i == n) print('failed')
  }
}

f(100,345)


#fasteR Lesson 18

# function that replaces 0s by NAs in specified columns in general data frames
zerosToNAs <- function(d,cols){
  for (j in cols) {
    NArows <- which(d[,j] == 0)
    d[NArows,j] <- NA
  }
  d
}

# function that prints number of NAs in each columns

countNAs <- function(dfr) {
  noNAs <- rep(0, times = ncol(dfr))
  for (j in 1:ncol(dfr)) {
    noNAs[i] <- sum(is.na(dfr[,i]))
  }
  noNAs
}

countNAs(pima)

#fasteR Lesson 20

# Create a vector of the same length as Nile to store the new values
nile <- numeric(length(Nile)) 

#Function to achieve the same result as the ifelse example 
for (i in 1:length(Nile)) {
  if (Nile[i] > 1150) {
    nile[i] <- 3
  } else if (Nile[i] < 800) {
    nile[i] <- 1
  } else {
    nile[i] <- 2
  }
}

table(nile)


#fasteR Lesson 21

load(url('https://github.com/matloff/fasteR/blob/master/data/mlb.RData?raw=true'))

# Round the ages to the nearest integer
age <- round(mlb$Age)

# Use tapply to find the mean weight for each age group
taout <- tapply(mlb$Weight, age, mean)
taout

# Plot the mean weight against age for ages 23 through 35
plot(23:35, taout[3:15])

# Find the mean weight for each position
meanWeightByPosition <- tapply(mlb$Weight, mlb$Position, mean)
meanWeightByPosition

# Plot the number of players at each age group
ageCounts <- table(age)
barplot(ageCounts, xlab = "Age", ylab = "Number of Players", main = "Number of Players by Age Group")


#fasteR Lesson 22

# Linear regression of weight on age
mlb.lm <- lm(Weight ~ Age, data = mlb)
summary(mlb.lm)

# Plot the original scatter plot with trend
plot(mlb$Age, mlb$Weight)
abline(mlb.lm)

# Fit a linear model of the regression of MPG against weight (wt)
mtcars.lm <- lm(mpg ~ wt, data = mtcars)
summary(mtcars.lm)

# Calculate the estimated effect of 100 pounds of extra weight
estimatedEffect <- coef(mtcars.lm)["wt"] * 0.1
estimatedEffect


#Hands On R 11

#Define die values
die <- c(1, 2, 3, 4, 5, 6)
#all possible combos
rolls <- expand.grid(die, die)
# sum column
rolls$value <- rolls$Var1 + rolls$Var2
head(rolls, 3)

#Assign probability
prob <- c("1" = 1/8, "2" = 1/8, "3" = 1/8, "4" = 1/8, "5" = 1/8, "6" = 3/8)
prob

rolls$Var1
prob[rolls$Var1]

# Prob of rolling firts variable
rolls$prob1 <- prob[rolls$Var1]
head(rolls, 3)

# Prob of rolling second variable
rolls$prob2 <- prob[rolls$Var2]
head(rolls, 3)

# Prob of rolling the pair

rolls$prob <- rolls$prob1 * rolls$prob2
head(rolls, 3)

# Expected value
sum(rolls$value * rolls$prob)
head(rolls, 3)



# WHEEL

#create a wheel with all the possible combinations
wheel <- c("DD", "7", "BBB", "BB", "B", "C", "0")
combos <- expand.grid(wheel, wheel, wheel, stringsAsFactors = FALSE)
combos

#make it a function 
get_symbols <- function() {
  wheel <- c("DD", "7", "BBB", "BB", "B", "C", "0")
  sample(wheel, size = 3, replace = TRUE, 
         prob <- c("DD" = 0.03, "7" = 0.03, "BBB" = 0.06, 
                   "BB" = 0.1, "B" = 0.25, "C" = 0.01, "0" = 0.52))
  }

#assign prob to each variable
combos$prob1 <- prob[combos$Var1]
combos$prob2 <- prob[combos$Var2]
combos$prob3 <- prob[combos$Var3]
head(combos, 3)

#add the total probability
combos$prob <- combos$prob1 * combos$prob2 * combos$prob3
head(combos, 3)

# see the total score
sum(combos$prob)
symbols <- c(combos[1, 1], combos[1, 2], combos[1, 3])
score(symbols)

#Do the same thing but in the loop
combos$prize <- NA
head(combos, 3)
for (i in 1:nrow(combos)) {
  symbols <- c(combos[i, 1], combos[i, 2], combos[i, 3])
  combos$prize[i] <- score(symbols)
}
head(combos, 3)

# new score function
sum(combos$prize * combos$prob)
score <- function(symbols) {
  diamonds <- sum(symbols == "DD")
  cherries <- sum(symbols == "C")
  # identify case
  # since diamonds are wild, only nondiamonds 
  # matter for three of a kind and all bars
  slots <- symbols[symbols != "DD"]
  same <- length(unique(slots)) == 1
  bars <- slots %in% c("B", "BB", "BBB")
  
  # assign prize
  if (diamonds == 3) {
    prize <- 100
  } else if (same) {
    payouts <- c("7" = 80, "BBB" = 40, "BB" = 25,
                 "B" = 10, "C" = 10, "0" = 0)
    prize <- unname(payouts[slots[1]])
  } else if (all(bars)) {
    prize <- 5
  } else if (cherries > 0) {
    # diamonds count as cherries
    # so long as there is one real cherry
    prize <- c(0, 2, 5)[cherries + diamonds + 1]
  } else {
    prize <- 0
  }
  
  # double for each diamond
  prize * 2^diamonds
}

# expectation it in loop
for (i in 1:nrow(combos)) {
  symbols <- c(combos[i, 1], combos[i, 2], combos[i, 3])
  combos$prize[i] <- score(symbols)
}
sum(combos$prize * combos$prob)


#While loop vs repeat loop

plays_till_broke <- function(start_with) {
  cash <- start_with
  n <- 0
  while (cash > 0) {
    cash <- cash - 1 + play()
    n <- n + 1
  }
  n
}

plays_till_broke(100)

plays_till_broke <- function(start_with) {
  cash <- start_with
  n <- 0
  repeat {
    cash <- cash - 1 + play()
    n <- n + 1
    if (cash <= 0) {
      break
    }
  }
  n
}

plays_till_broke(100)

