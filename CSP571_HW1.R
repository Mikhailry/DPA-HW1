# CSP/MATH 571
# Homework 1
# Note you must show all your code to receive credit. Some of these questions could be 
# solved without code, but the point here is to practice doing basic data manipulation in R
# and to start thinking about how to break down data analysis tasks into steps.


# Question 1: Create a variable named "myName" and assign to have a value of your
# preferred name. Create a varaible named "myEmail" and assign it to have a value
# of your email.
myName <- "Michael"
myEmail <- "mrybalchenko@hawk.iit.edu"


# Question 2: Create a vector of integers from 99 to 10000 (inclusive). Assign
# the variable myVector. Randomly reorder that vector. 
# Write your own functions to sum, calculate the min value, the max value and the median value.
# You do not need to implement your own sorting algorithms. 
# Return the sum, min, max, and median of this vector and assign it below.
# 
# Note: in practice, you should usually use the predefined functions that R provides to 
# compute summary statistics. However, we can use this as an opportunity to practice our R
# while having an easy way to check for mistakes by comparing our function output with the 
# default R function output. 

#Create a vector of integers from 99 to 10000 (inclusive)
#and randomly reorder it
myVector <- c(99:10000)
set.seed(1) #set the seed of random generator to reproduce the random object
myVector <- sample (myVector) 
#simple way> myVector <- sample(c(99:10000))

#function to sum elements
mySumFunc <- function(elements){
  
  count <- 0
  for (i in 1:length(elements)){   #also can use> for (e in elements){count <- count + e}
    count <- count + elements[i]
  }
  return (count) #sum of all elements
}

#function to find min element
myMinFunc <- function(elements){
  return (sort(elements)[1])
}

#function to find max element
myMaxFunc <- function(elements){
  return (sort(elements, decreasing = TRUE)[1])
}

#function to find median element
myMedianFunc <- function(elements){
  
  elements_sorted <- sort(elements) #sorting elements
  count <- length(elements_sorted) #count number of elements
  
  if (count %% 2 == 0){
  return((elements_sorted[count/2]+elements_sorted[count/2+1])/2)
  } else {
    return (elements[count/2])
  }
}


#sum of the vector
mySumFunc(myVector)
#min value
myMinFunc(myVector)
#max value
myMaxFunc(myVector)
#median
myMedianFunc(myVector)

# Question 3: Write a function that accepts a number as an input returns
# TRUE if that number is divisible by 127 FALSE if that number is not divisible
# by 127.  For example, divis(127*5) should return TRUE and divis(80)
# should return FALSE. Hint: %% is the modulo operator in R.
divis <- function(number){
  if (number%%127 == 0){
    return (TRUE)
  } else {
    return (FALSE)
  }
}

divis(127*5)
divis(80)

# Question 4: Using the function you wrote for Question 3 and the vector you
# defined in Question 2, deterine how many integers between 100 and 10000 are
# divisible by 127. Assign it to the variable below.

div_count <- 0    #initialize variable
#myVector <- sort(myVector)   #sorting can be done for easier validation
for (i in myVector){
  if (divis(i) & i>=100) {  #i>=100 in case we will be checking if number is divisible by 3 or 33 etc.
    div_count <- div_count + 1 
    #print (paste("for i=", i, "div_count=", div_count)) #for validation
  }
}
countDivis <- div_count #assigning result to countDivis variable.
#Note: could have used it in loop

##Alternative solution
#newVector <- myVector %%127 == 0
#cc <- length(myVector[newVector])


# Question 5: Using the vector of names below, write code to return the 9th
# last name in the vector.
names <- c("Kermit Chacko",
           "Eleonore Chien",
           "Genny Layne",
           "Willene Chausse",
           "Taylor Lyttle",
           "Tillie Vowell",
           "Carlyn Tisdale",
           "Antione Roddy",
           "Zula Lapp",
           "Delphia Strandberg",
           "Barry Brake",
           "Warren Hitchings",
           "Krista Alto",
           "Stephani Kempf",
           "Sebastian Esper",
           "Mariela Hibner",
           "Torrie Kyler")

ninthLastName <- unlist(strsplit(names[9], " "))[2]

# Question 6: Using the vector "names" from Question 5, write code to
# determine how many last names start with L.

result <- 0 #initialize variable
lastNames <- sapply(strsplit(names, " "), tail,1) #extract lastnames
for (lastname in lastNames){ 
  if (substr(lastname, start=1, stop = 1) == "L"){ 
    #increment result variable for every lastname starting with "L"
    result <- result + 1
  }
}

countLastNameStartsWithL <- result

# Question 7: Using the vector "names" from Question 5, write code to create a
# list that allows the user to input a first name and retrieve the last name.
# For example, nameMap["Krista"] should return "Alto".

lastnameslist <- sapply(strsplit(names, " "), simplify = TRUE, tail,1) #extract lastnames
firstnameslist <- sapply(strsplit(names, " "), simplify=TRUE, head,1) #extract firstnames

nameMap <- list() #define empty list
for (fn in lastnameslist){ #add values to the list
  nameMap <- c(nameMap, fn)
}

names(nameMap) <- firstnameslist #add names

nameMap["Krista"] 


# Question 8: Load in the "Adult" data set from the UCI Machine Learning
# Repository. http://archive.ics.uci.edu/ml/datasets/Adult
# Load this into a dataframe. Rename the variables to be the proper names
# listed on the website. Name the income attribute (">50K", "<=50K") to be
# incomeLevel
#load data set into a dataframe
df <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data", sep=',', header=F, stringsAsFactors = F)
#add column names
colnames(df) <- c("age", "workclass", "fnlwgt", "education", "education-num", "marital-status", "occupation", "relationship", "race", "sex", "capital-gain", "capital-loss", "hours-per-week", "native-country", "incomelevel")

# Question 9: Create a new variable called workSector. Label all government
# employees as "government", all self-employeed employees as "selfEmployed",
# all Private employees as "Private" and everyone else as "Other".
# Enter the number of government employees in the text here, as well as showiing the code below: 

#Number of "government employees" = 4351
df$workSector <- ifelse(grepl("gov", df$workclass),"government",
                        ifelse(grepl("Self", df$workclass),"selfEmployed",
                               ifelse(grepl("Private", df$workclass),"Private","Other")))

ngov <- sum(df$workSector == "government")
#ngov1 <- length(df$workSector[df$workSector == "government"])

# Question 10: Create a histogram of the 'age'. Hint: You may need to convert
# age to be numeric first. Save this histogram and include it with your
# submission

#install.packages("ggplot2")
#load package
library("ggplot2")
#qplot(df$age, geom="histogram", binwidth=2, main="Histogram of the 'age'", xlab="Age")
ggplot(df, aes(df$age)) + geom_histogram(breaks=seq(16, 100, by = 2), col="red", fill="green", alpha = .3) + 
  labs(title="Histogram of the 'Age'") +
  labs(x="Age", y="Count")

# Question 11: Determine the top 3 occupations with the highest average hours-per-week
# Hint: One way to do this is to use tapply
# List the occupations in the comments, as well as showing the code you used to determine that.
#Top Avg_hours
#Farming-fishing    46.98994
#Exec-managerial    44.98770
#Transport-moving   44.65623

occAvgHours <- tapply(df$`hours-per-week`, df$occupation, mean, na.rm=T)
occ_df <- data.frame(Avg_hours=sort(occAvgHours, decreasing=TRUE))

# Question 12: Your friend works for the government and claims that in order to make more money, you have to work
# longer hours. Use this data set to determine if your friend is right. State your conclusion in the comments.
#Governers with higher income (>50K) on average work 43.87 hours a week, which is 5 hours a week more than 
#governers with lower income (<=50) working on average 38.98 hours a week.

df$incomelevel_fact <- factor(df$incomelevel)
levels(df$incomelevel_fact) <- c("<=50K", ">50K")
summary(df$incomelevel_fact)

df_gov <- subset(df, subset=df$workSector=="government")
lowerIncMean_subset <- subset(df_gov, subset = df_gov$incomelevel_fact=="<=50K")
lowerIncMean <- mean(lowerIncMean_subset[["hours-per-week"]])
higherIncMean_subset <- subset(df_gov, subset = df_gov$incomelevel_fact==">50K")
higherIncMean <- mean(higherIncMean_subset[["hours-per-week"]])

print(paste("On average governers with income '<=50K' work for", lowerIncMean))
print(paste("On average governers with income '>50K' work for", higherIncMean))
