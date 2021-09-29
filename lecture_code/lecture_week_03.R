### Formatting dates

df <- read.csv("data/ebay_snake_captures.csv",
               stringsAsFactors = FALSE)

?strptime # look up the date format abbreviation codes
df$Date2 <- as.Date(df$Date, format = "%d-%b-%y")



### Plotting with R
### ~~~~~~~~~~~~~~~~

# Load packages dplyr and ggplot2
library(dplyr)
library(ggplot2)
# install.packages("ggplot2")

# Load iris dataset
# Use data() to show all available datasets
data("iris")




#==========================================================
### Base plots


### Plotting continuous data on both the x- and y-axes
### Scatter plots using base plot() function

# Plot petal length on x axis, petal width on y axis
plot(iris$Petal.Length, iris$Petal.Width)


# Formula version (y over x) 
plot(iris$Petal.Length ~ iris$Petal.Width)



# Data argument with formula version
plot(data = iris, Petal.Length ~ Petal.Width)



### Customization
# Main title
# Axis labels
plot(data = iris, Petal.Length ~ Petal.Width, 
     main = "Plot title", 
     xlab = "Petal length", 
     ylab = "Petal width")



# Axis limits
plot(data = iris, Petal.Length ~ Petal.Width, 
     main = "Plot title", 
     xlab = "Petal width", 
     ylab = "Petal lengths", 
     xlim = c(0, 3), 
     ylim = c(0, 10))




# pch argument (point type)
plot(data = iris, Petal.Length ~ Petal.Width, 
     main = "Plot title", 
     xlab = "Petal width", 
     ylab = "Petal lengths",
     pch = 2)





#==========================================================
### ggplot plots

# Implementation of scatter plots with ggplot2
# Refer to reading for today on grammar of graphics used by ggplot2
# Main elements of a graph
# - aesthetics
# - geometries
# - facets
# - statistics
# - coordinates
# - themes



# Initialize plot with data and mapping
p <- ggplot(iris, 
       aes(x = Petal.Width, y = Petal.Length))


# Add data points
p <- p + geom_point()



# Modify the scales, e.g. axis limits
p + scale_x_continuous(limits = c(0, 10)) + 
  scale_y_continuous(limits = c(0, 10))




# Add titles
# Note: I prefer using labs() instead of ggtitle, xlab, and ylab etc. 
p <- p + labs(title = "Sample title", 
         subtitle = "Sample subtitle", 
         caption = "Sample caption", 
         x = "Petal width", 
         y = "Petal length")




# Style the plot using theme()
# See ?ggplot2 and look under theme_ for available complete themes
# For more themes, install ggthemes package

p <- p + theme_minimal()




#==========================================================


# Combine geometries in same graph
# E.g., add a geom_smooth
?geom_smooth
p + geom_smooth()


# Remove error bands
p + geom_smooth(se = FALSE)



# Use method = "lm" instead
p + geom_smooth(method = "lm", se = FALSE)





#==========================================================

# Add additional mappings for existing variables
summary(iris$Species)


# Assign color based on species
ggplot(iris, aes(x = Petal.Width, y = Petal.Length, 
                 color = Species)) + 
  labs(title = "Sample title", 
       subtitle = "Sample subtitle", 
       caption = "Sample caption", 
       x = "Petal width", 
       y = "Petal length") + 
  geom_point() + 
  theme_minimal()



# Create different panels (plots), one for each species
# facet_wrap: vars(variable), nrow, ncol
# facet_grid: vars(variable), rows, cols, scales, 

ggplot(iris, aes(x = Petal.Width, y = Petal.Length)) + 
  labs(title = "Sample title", 
       subtitle = "Sample subtitle", 
       caption = "Sample caption", 
       x = "Petal width", 
       y = "Petal length") + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) + 
  theme_minimal() + 
  facet_wrap(vars(Species), nrow = 1)





#==========================================================


### Combine dplyr pipelines with ggplot function
# Simple: feed in dataframe
iris %>% ggplot(aes(x = Petal.Width, y = Petal.Length)) + 
  labs(title = "Sample title", 
       subtitle = "Sample subtitle", 
       caption = "Sample caption", 
       x = "Petal width", 
       y = "Petal length") + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) + 
  theme_minimal() + 
  facet_wrap(vars(Species), nrow = 1)



# More: manipulate data then plot
# Exclude virginica species
iris %>% 
  filter(Species != "setosa") %>% 
  ggplot(aes(x = Petal.Width, y = Petal.Length)) + 
  labs(title = "Sample title", 
       subtitle = "Sample subtitle", 
       caption = "Sample caption", 
       x = "Petal width", 
       y = "Petal length") + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) + 
  theme_minimal() + 
  facet_wrap(vars(Species), nrow = 1)



# Plot mean values by species
# geom_bar vs. geom_col
iris %>% 
  group_by(Species) %>% 
  summarise(mean.plength = mean(Petal.Length)) %>% 
  ggplot(aes(x = Species, y = mean.plength)) + 
  geom_bar(stat = "identity")



# Boxplot
iris %>% 
  ggplot(aes(x = Species, y = Petal.Length)) + 
  geom_boxplot()




# Even better: show all the raw data when feasible, species by Petal.Width
iris %>% 
  ggplot(aes(x = Species, y = Petal.Length)) + 
  geom_boxplot() + 
  geom_point()


# Add a jitter
iris %>% 
  ggplot(aes(x = Species, y = Petal.Length)) + 
  geom_boxplot() + 
  geom_jitter() + 
  theme_minimal()




# Limit jitter to x-axis (width)
iris %>% 
  ggplot(aes(x = Species, y = Petal.Length)) + 
  geom_boxplot() + 
  geom_jitter(width = 0.5, height = 0) + 
  theme_minimal()




