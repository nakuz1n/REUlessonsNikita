
#Faster R
##Lesson 1

# Add two numerical values, click ctrl+enter to see the result in the console
1+1
#There are preloaded df in R such as Nile, mean function will give you the mean value of the vector
mean(Nile)
#We can also use print function to see the output in the console
print(mean(Nile))
#length function returns the length of the element
length(Nile)
#histogramm returns the histogramm of the element
hist(Nile)
# breaks will let you adjust the number of bars
hist(Nile, breaks = 20)
#Slicing i.e. single element of the vector, multiple elements of the vector, quicker way of geting continuous values
Nile[2]
Nile[c(2,5,6)]
Nile[2:6]
Nile[81:100]
# save a piece of vector as a separate vector
n81100 <- Nile[81:100]
#mean and standard deviation functions
mean(n81100)
sd(n81100)
#different indicies in different vectors give the same value
n81100[1]
Nile[81]
# very usefull length function
length(Nile)
length(n81100)


#Hands-On R
##Section 2

# error message
#5 % 4

#Using R as a calculator

2 * 3 
4 - 1
6/(3*2)

# Exercise 1
10 + 2
## 12

12 * 3
## 36

36 - 6
## 30

30 / 3
## 10

# Storing values in Objects
a <- 1
a
## 1

a + 2
## 3

## Section 2

#creating a die
die <- 1:6
die
## 1 2 3 4 5 6

# R is case sensitive Name and name are two different objects
Name <- 1
name <- 0
Name + 1
## 2

# R will overwrite your objects
my_number <- 1
my_number 
## 1

my_number <- 999
my_number
## 999

# function ls() dives you all the names you have in your environment
ls()

die - 1
## 0 1 2 3 4 5

die / 2
## 0.5 1.0 1.5 2.0 2.5 3.0

die * die
## 1  4  9 16 25 36

# vector recycling: if one of the vectors shorter than another R will repeat it until it reached the length or give a warning message
1:2
## 1 2

1:4
## 1 2 3 4

die
## 1 2 3 4 5 6

die + 1:2
## 2 4 4 6 6 8

die + 1:4
## 2 4 6 8 6 8

#matrix multiplication uses the following: %*% - inner product, %o% outter product
die %*% die
## 91

die %o% die

#Function for rounding the value
round(3.1415)
## 3

#Function for factorials
factorial(3)
## 6

#composition of functions
mean(1:6)
## 3.5

mean(die)
## 3.5

round(mean(die))

#Sample() is a function randomly selects numbers from a verctor
sample(x = die, size = 1)
## 2

sample(x = die, size = 1)
## 1

sample(x = die, size = 1)
## 6

# If you put the wrong argument in the function you will get an error, 
# Use args() to check the function's arguments
round(3.1415, corners = 2)
args(round)
round(3.1415)
round(3.1415, digits = 2)

# use arguments names while using f-ns because if you won't R will assign your 
# input to the arguments in the order of arguments, but it is not always the case. 
# Moreover if you specify the argument name before puttin the input you can use any order.
sample(die, 1)
sample(size = 1, x = die)


# On default settings sample() does not have replacement, to include it we set 
# argument replace = TRUE
sample(die, size = 2)
sample(die, size = 2, replace = TRUE)

# create your "dice" by assigning the function to it. BUt if you call dice multiple 
# times, it will always return the first generated value i.e. this way you record 
# value not a function
dice <- sample(die, size = 2, replace = TRUE)
dice
sum(dice)

dice
dice
dice

# construct a f-n
roll <- function() {
  die <- 1:6
  dice <- sample(die, size = 2, replace = TRUE)
  sum(dice)
}

# calls the f-n
roll()
# describes the f-n
roll

# return the values
dice
1 + 1
sqrt(2)

# store the value
dice <- sample(die, size = 2, replace = TRUE)
two <- 1 + 1
a <- sqrt(2)

# create an arguments for your function

roll2 <- function(bones) {
  dice <- sample(bones, size = 2, replace = TRUE)
  sum(dice)
}

# now it requires the input
roll2(bones = 1:4)
roll2(bones = 1:6)
roll2(1:20)

# You can set a default argument in the f-n. Now it won't ask for any input
roll2 <- function(bones = 1:6) {
  dice <- sample(bones, size = 2, replace = TRUE)
  sum(dice)
}

roll2()

## Section 3























