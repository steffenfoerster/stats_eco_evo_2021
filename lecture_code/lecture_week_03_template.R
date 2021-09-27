### Formatting dates

df <- read.csv("data/ebay_snake_captures.csv",
               stringsAsFactors = FALSE)




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



# Formula version (y ~ x) 




# Data argument with formula version




### Customization
# Main title
# Axis labels main, xlab, ylab




# Axis limits, xlim, ylim





# pch argument (point type)






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



# Initialize plot with data and aesthetic mapping



# Add data points




# Modify the scales, e.g. axis limits





# Add titles
# Note: I prefer using labs() instead of ggtitle, xlab, and ylab etc. 





# Style the plot using theme()
# See ?ggplot2 and look under theme_ for available complete themes
# For more themes, install ggthemes package





#==========================================================


# Combine geometries in same graph
# E.g., add a geom_smooth
?geom_smooth



# Remove error bands




# Use method = "lm" instead of default






#==========================================================

# Add additional mappings for existing variables, like species


# Assign color based on species




# Create different panels (plots), one for each species
# facet_wrap: vars(variable), nrow, ncol
# facet_grid: vars(variable), rows, cols, scales, 







#==========================================================


### Combine dplyr pipelines with ggplot function
# Simple: feed in dataframe




# More: manipulate data then plot
# Exclude setosa species




# Plot mean values by species
# geom_bar vs. geom_col




# Boxplot





# Even better: show all the raw data when feasible, species by Petal.Width



# Add a jitter





# Limit jitter to x-axis (width)






