### Week 1

# Intro on R, RStudio, and basic orientation to interface: 
# Script files vs. Console
# Environment
# File browser
# Help
# Plot window


#==========================================================


# Basic mathematical operations in R
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# addition
3+1

# subtraction
4-1

# multiplication
5*2 

# division
5/2

# exponents
5^3



#==========================================================

# Assignment 
# = vs. <-
# Objects in environment
test <- 2



#==========================================================

# Commenting



#==========================================================

# Get and set working directories
getwd()
setwd("C:/Users/sf204/Google Drive/Teaching/Columbia")

# First function! Get and set working directory


# Get help
?getwd()


# List files
list.files()


#==========================================================


# Vectors (with numeric data types)
numbers <- c(1, 2, 3, 4)
strings <- c("apples", "oranges")

# A vector in R is a collection of values of the same data type
weights <- c(5.9, 7.2, 6.3)


# sum values
sum(weights)

# average values
mean(weights)
median(weights)



#==========================================================


# Vectors (with character data types)

frogs <- c("Hyla avivoca", "Hyla gratiosa", "Hyla versicolor")
frogs

# Operations on character vectors
sum(frogs)


#==========================================================


# Key functions to inspect objects in R
# str, summary, View
str(frogs)
summary(frogs)


#==========================================================


# Sequences
seq(1:10)
seq(from = 1, to = 12, by = 3)

# sequence 1 through 3

# sequence 1 through 729 (i.e., 3^6)

# seq() function, look up help






#==========================================================


# Indexing

# Indexing on vector
# One value
frogs[3]

# Multiple values
frogs[c(1,3)]



#==========================================================


# What happens if we try to combine data types into one vector?

mashup <- c(16.9, "Hyla gratiosa")
str(mashup)




#==========================================================


# Data frames

# Data frames can hold collections of different data types

d <- data.frame(frogs, weights, stringsAsFactors = FALSE) 

str(d)
summary(d)
View(d)


# Inspect and change column names in a data frame
colnames(d)
colnames(d)[colnames(d)=="frogs"] <- "frog_species"
colnames(d)[colnames(d)=="frog_species"] <- "frogs"



# $ operator
d$frogs


# Indexing with data frames
# [rows, columns]
d[2,2]

# all rows, second column
# third row, second column
d[,2]
d[1,]
d$weights[2]
d[2,2]
d[["weights"]][1]


# What if we want to add data to the data frame?

frogs2 <- c(frogs, "Hyla chrysoscelis")
frogs2

weights2 <- c(weights, NA)
d2 <- data.frame(frogs2, weights2, stringsAsFactors = FALSE) 


# Easier way
d[nrow(d) + 1,] <- c("Hyla chrysoscelis", NA)


# What happens when calculating the mean on a column with NA?
mean(d2$weights) # doesn't work!
mean(d2$weights, na.rm = TRUE) # works!

