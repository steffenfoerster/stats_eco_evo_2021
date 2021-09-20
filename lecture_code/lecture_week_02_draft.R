### Lecture 2, 09/20

### Reminder: subsetting in base R ####

x <- c(25.4, 26.3, 23.2, 28)


# to get one vector element


# to get multiple vector elements




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


### Installing packages ####
# Install dplyr


# Loading the package


# Getting package help



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


### Importing Ellenton Bay snake capture data ####
# read in data/ebay_snake_captures.csv, save as d



# Look at the data in different ways (head, summary, dim, colnames)



# Rename columns to be more informative
# c("date", "time", "trap_type", "species", "num", "comments")



# rename num to number, base R syntax





# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


### Introduction to dplyr ####

## 1) sorting (asc/desc)



# sorting, multiple columns



# sort() in base R (works on columns/vectors)




## 2) selecting columns

# select species, date, and number column by name


# select by index



# base R analog






## 3) filter()
# species == "Nerodia fasciata", time >= 1200, time <= 1300




# The analogous base R functionality uses brackets 
# and indexing to subset





## 4) mutate()

# create new column that codes whether trap has multiple individuals
# values: "yes" and "no"
# dplyr way




# base R way





# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# distinct() & unique()






# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


## The pipe operator ####
# only want observations from coffee can traps
# only return the "species", "date", and "time" columns
# sort the rows by "species" name





# Pipe on other functions (non-dplyr), like head()







# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Commenting out lines ####

d %>%
  filter(trap_type == "coffee can") %>%
  select(species, date, time) %>%
  arrange(species, time) %>%
  View()





# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


### group_by() & summarize() ####

# group by species, get total (sum) of numbers





# arrange the data frame by "total_observations"





# group by species and trap type






# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Loops ####

beginning <- "Coding"

endings <- c("is useful!", "is fun!", 
             "is great to talk about at social events!")

## Combine first word with each of the ending phrases

# non-loop example using paste()



# Loop through vector






