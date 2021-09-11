###########################################################################
#Workshop: Data visualisation with ggplot2
#Author: Denisse Fierro Arcos
#Date of creation: 2021-07-24
#Details: 
#In this script we will go through the basics on how to create a plot using
#ggplot2
###########################################################################

 
# Loading libraries -------------------------------------------------------
library(tidyverse) #contains ggplot2 and other useful libraries
library(palmerpenguins) #library with data about penguins in Antarctica


# Loading data ------------------------------------------------------------
#We will data about animal rescues from the London Fire Brigade. More 
#information about the  dataset here: https://bit.ly/3kwK2OL 
rescues <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-29/animal_rescues.csv')

#We can check the structure of our data
glimpse(rescues)


# Creating a simple graph -------------------------------------------------
#Let's say we want to check the total number of rescues per year
rescues %>% 
  #We count rescues by year
  count(cal_year) %>% 
  #Now we call ggplot, and define the variables that will be shown in the X
  #and Y axes
  ggplot(aes(x = cal_year, y = n))+
  #Now we define the type of graph that we want to create 
  geom_col()

#This is actual the basic skeleton to create any plot in ggplot


#A different way to do the graph above is to perform calculations and save
#the results in a new variable
rescues_yr <- rescues %>% 
  #Count rescues per year
  count(cal_year)
#Check the results
rescues_yr

#Call ggplot and apply the basic skeleton
ggplot(data = rescues_yr)+
  #Remember to define the type of graph. You can also define the axes within
  #the graph type
  geom_col(mapping = aes(x = cal_year, y = n))

#For comparison, we could use barplot from base R
#We define the 
barplot(height = rescues_yr$n, names.arg = rescues_yr$cal_year)


# Changing the aesthetics of a graph --------------------------------------
#Aesthetics includes colour, shape and size of graph elements

#We could try to change the colour of the bars in the previous graph based
#on the type of animal rescue
rescues %>% 
  #We count rescues per year and per animal type
  count(cal_year, animal_group_parent) %>% 
  #We define axes as before, but now we use the option fill to change the 
  #bar colour depending on the animal rescued
  ggplot(aes(x = cal_year, y = n, fill = animal_group_parent))+
  geom_col()

#You may have noticed that 'cat' is repeated twice in our graph. This is
#because it is not written exactly in the same way: cat vs Cat
#We can change this
rescues <- rescues %>% 
  #We can ensure all strings are capitalised the same whay with str_to_sentence
  mutate(animal_group_parent = str_to_sentence(
    #Then we can change all cases when Unknown is present to simply Unknown
    case_when(str_detect(animal_group_parent, "Unknown*") ~ "Unknown",
              #Everything else will remain the same
              T ~ animal_group_parent)))

#We can check the results
rescues %>%
  #Find the unique values for the animal type column
  distinct(animal_group_parent) %>% 
  #Print all values in the console
  pull()

#If we create the plot again with the clean data, it looks better
rescues %>% 
  #We count rescues per year and per animal type
  count(cal_year, animal_group_parent) %>% 
  #We define axes as before, but now we use the option fill to change the 
  #bar colour depending on the animal rescued
  ggplot(aes(x = cal_year, y = n, fill = animal_group_parent))+
  geom_col()


#Let's try a dot plot now and change the size of the dots
#This time we will use some penguin data
glimpse(penguins) #Part of palmerpenguins package

#We apply the same skeleton
penguins %>% 
  #Now we include the parameter size, which will change based on the species
  ggplot(aes(x = body_mass_g, y = bill_length_mm, size = species))+
  #Now we choose the dot graph
  geom_point()

#But we can change multiple aesthetics in the same graph
penguins %>% 
  drop_na() %>% 
  #We will change the size, color, shape, and transparency (alpha)
  ggplot(aes(x = body_mass_g, y = bill_length_mm, size = body_mass_g, 
             color = species, shape = sex, alpha = bill_length_mm))+
  geom_point()

#Remember that the aesthetics can be defined under the graph type, rather
#than globally as we did before
penguins %>% 
  drop_na() %>% 
  #The skeleton for a graph is the same, but we are now include the
  #aesthetics under the graph type (geom_point)
  ggplot()+
  geom_point(aes(x = body_mass_g, y = bill_length_mm, size = body_mass_g, 
                 color = species, shape = sex, alpha = bill_length_mm))

#You can also set an aesthetic to a particular value
penguins %>% 
  #Note that aesthetics is now outside aes, this is because we are not using
  #information from the dataset, but setting it manually
  ggplot()+
  geom_point(aes(x = body_mass_g, y = bill_length_mm), color = "red", shape = 0)

#If you want to remember the options available under a graph type, try using ?
?geom_point


# Creating facets (graph within a graph) ----------------------------------
#Let's assume that we want to create a single graph with multiple smaller
#graphs within. We can do this with facet_*. 
#Let's try with the rescue data
rescues %>% 
  count(cal_year, animal_group_parent) %>% 
  #Let's define axes
  ggplot(aes(x = cal_year, y =  n))+
  #Now we can choose the graph type
  geom_col()+
  #Finally the facets - Note that we use ~ to separate "rows" ~ "columns" 
  facet_wrap(~animal_group_parent)

#We can use two variables to order our graphs. Let's try the penguin data
penguins %>% 
  ggplot(aes(x = body_mass_g, y = bill_length_mm))+
  geom_point()+
  facet_grid(island~species)


# More graph types --------------------------------------------------------
#We have seen a dot plot
penguins %>% 
  ggplot()+
  geom_point(aes(x = body_mass_g, y = bill_length_mm))

#But we can choose to plot a line of best fit to describe a trend
penguins %>% 
  ggplot()+
  geom_smooth(aes(x = body_mass_g, y = bill_length_mm))

#We could show the trend per species with different line types
penguins %>% 
  ggplot()+
  geom_smooth(aes(x = body_mass_g, y = bill_length_mm, linetype = species))


# More than one geometry within a graph -----------------------------------
penguins %>% 
  #We can define the axes globally
  ggplot(aes(x = body_mass_g, y = bill_length_mm))+
  #But the aesthetic will vary with each geometry type
  geom_smooth(aes(linetype = species, color = species))+
  geom_point(aes(color = species))
#You can check the different fits available with 
?geom_smooth

#We could also use different data subsets for different geometries
penguins %>% 
  #We keep the same axes for both geometries
  ggplot(aes(x = body_mass_g, y = bill_length_mm))+
  #We select different data subsets for each geometry and different 
  geom_point(aes(color = species))+
  #Selecting data for females only and showing it in purple
  geom_smooth(data = filter(penguins, sex == "female"), color = "purple")+
  #Male data shown in green and with no confidence intervals in grey
  geom_smooth(data = filter(penguins, sex == "male"), color = "#029534", se = F)


# More about bar graphs ---------------------------------------------------
#We can actually use geom_bar directly without having to count data before
#plotting
rescues %>% 
  ggplot(aes(x = cal_year))+
  geom_bar()

#Another way of doing it is to calculate counts first
rescues %>% 
  count(cal_year) %>% 
  #Create the plot
  ggplot(aes(x = cal_year, y = n))+
  #But you must ensure that the statistic is specified as identity, which means
  #to be used as it is
  geom_bar(stat = "identity")

#We can also calculate proportions directly with ggplot
rescues %>% 
  ggplot(aes(x = cal_year, y = stat(prop)))+
  geom_bar()

#We can check results are ok
rescues %>% 
  count(cal_year) %>% 
  mutate(prop = n/sum(n))

#We can also change the line color for each group
rescues %>% 
  ggplot(aes(x = cal_year))+
  geom_bar(aes(fill = animal_group_parent), col = "blue")

#We can do more statistical calculations with stat
penguins %>% 
  ggplot()+
  #We select the axes
  stat_summary(aes(x = species, y = body_mass_g),
               #Include the functions we want to use to calculate max, min and mean
               fun.min = min,
               fun.max = max,
               fun = mean)


# Adjusting position of graph elements ------------------------------------
#Changing column position
rescues %>% 
  #Let's select just one year, 2020
  filter(cal_year == 2020) %>% 
  #Start a plot
  ggplot()+
  #Set axis, and change position so boxes are next to each other rather than stacked (default)
  geom_bar(aes(x = factor(1), fill = animal_group_parent), position = "dodge", colour = "grey")

#You can also change the position of dots
#This is the dot graph without any changes
penguins %>% 
  ggplot(aes(x = body_mass_g, y = bill_length_mm))+
  geom_point()
#There is some overlapping in the dots

#We can use position = 'jitter' to add some random noise and show dots better
penguins %>% 
  ggplot(aes(x = body_mass_g, y = bill_length_mm))+
  geom_point(position = "jitter")

#Or we could also use the geom_jitter option
penguins %>% 
  ggplot(aes(x = body_mass_g, y = bill_length_mm))+
  geom_jitter()


# Coordinate system -------------------------------------------------------
#Flipping axes
rescues %>% 
  ggplot(aes(x = cal_year))+
  geom_bar(aes(fill = animal_group_parent), col = "grey")+
  #Simply add coord_flip
  coord_flip()

#Simple maps
#We can search for Australia in the map_data
map_data("world", region = "australia", exact = F) %>% 
  #Provide longitude and latitude as axes, we add groups because there is more
  #than one geometry included in Australia
  ggplot(aes(long, lat, group = group))+
  #Now we can show it as a polygon
  geom_polygon()

#We could improve the map a little by adding a projection
map_data("world", region = "australia", exact = F) %>% 
  ggplot(aes(long, lat, group = group))+
  geom_polygon(fill = "#bce9b6", colour = "#9a979f")+
  #We can use a variety of projection with this function
  coord_quickmap()

#Polar coordinates
#Useful if we want a bar chart
rescues %>% 
  ggplot(aes(x = animal_group_parent))+
  #We start with a bar graph, we set the colour fill to vary with animal type
  #width 1 fills all bars to the same width and we will remove the legend
  geom_bar(aes(fill = animal_group_parent), width = 1, show.legend = F)+
  #Now we assign a polar projection
  coord_polar()

#Let's compare to a bar graph
rescues %>% 
  ggplot(aes(x = animal_group_parent))+
  geom_bar(aes(fill = animal_group_parent))
#The same animals show the tallest bars

#Bar graph
penguins %>% 
  ggplot()+
  #We create a bar plot, use factor 1 because we do not want an x axis
  geom_bar(aes(x = factor(1), fill = species))+
  #Theta refers to the exes that will be used to create the circle
  coord_polar(theta = "y")
