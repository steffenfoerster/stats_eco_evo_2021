### Lecture 2, 09/20

### Reminder: subsetting in base R ####

x <- c(25.4, 26.3, 23.2, 28)


# to get one vector element
x[3]


# to get multiple vector elements
x[c(2,4)]



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


### Installing packages ####
# Install dplyr
install.packages("dplyr")

# Loading the package
library(dplyr)

# Getting package help
?dplyr


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


### Importing Ellenton Bay snake capture data ####
# read in data/ebay_snake_captures.csv, save as d
df <- read.csv("data/ebay_snake_captures.csv",
               stringsAsFactors = FALSE)


# Look at the data in different ways (head, summary, dim, colnames)
str(df)
summary(df)
head(df)

# Rename columns to be more informative
# c("date", "time", "trap_type", "species", "num", "comments")
colnames(df) <- c("date", "time", "trap_type", "species", "num", "comments")

# rename num to number, base R syntax
colnames(df)[colnames(df)=="num"] <- "number"




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


### Introduction to dplyr ####

## 1) sorting (asc/desc)
df <- arrange(df, desc(number))


# sorting, multiple columns
df <- arrange(df, desc(number), species)


# sort() in base R (works on columns/vectors)
sort(df$species)



## 2) selecting columns

# select species, date, and number column by name
df_sub <- select(df, species, date, time)


# base R analog
# select by index
df_sub <- df[, c("species", "date", "time")]





## 3) filter()
# species == "Nerodia fasciata", time >= 1200, time <= 1300
df_species1 <- filter(df, 
                      species == "Nerodia fasciata",
                      time >= 1200, 
                      time <= 1300)



# The analogous base R functionality uses brackets 
# and indexing to subset
d[d$species=="Nerodia fasciata" & d$time >= 1200 & d$time <= 1300, ]




## 4) mutate()

# create new column that codes whether trap has multiple individuals
# values: "yes" and "no"
# dplyr way
df <- mutate(df, 
             multiple_individuals = ifelse(number > 1, "yes", "no"))





# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# distinct() & unique()
distinct(df, species)
unique(df$species) # base R




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


## The pipe operator ####
# only want observations from coffee can traps
# only return the "species", "date", and "time" columns
# sort the rows by "species" name

df2 <- df %>% 
  filter(trap_type == "coffee can") %>% 
  select(species, date, time) %>% 
  arrange(species)



# Pipe on other functions (non-dplyr), like head()
df %>% 
  filter(trap_type == "coffee can") %>% 
  select(species, date, time) %>% 
  arrange(species)
# View()



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Commenting out lines ####

df %>%
  filter(trap_type == "coffee can") %>%
  select(species, date, time) %>%
  # arrange(species, time) %>%
  View()





# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


### group_by() & summarize() ####

# group by species, get total (sum) of numbers
df %>% 
  group_by(species) %>% 
  summarise(sum = sum(number), 
            median = median(number))




# arrange the data frame by "total_observations"
df %>% 
  group_by(species) %>% 
  summarise(sum = sum(number), 
            median = median(number)) %>% 
  arrange(desc(sum))




# group by species and trap type
df %>% 
  group_by(species, trap_type) %>% 
  summarise(sum = sum(number), 
            median = median(number)) %>% 
  arrange(species, desc(sum)) %>% 
  View()





# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Loops ####

beginning <- "Coding"

endings <- c("is useful!", 
             "is fun!", 
             "is great to talk about at social events!")

## Combine first word with each of the ending phrases

# non-loop example using paste()
paste(beginning, endings)


# Loop through vector
for(i in 1:length(endings)){
  paste(beginning, endings[i])
}





