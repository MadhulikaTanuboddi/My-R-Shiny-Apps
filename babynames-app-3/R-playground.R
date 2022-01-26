#babynames
# A data frame with five variables: year, sex, name, n and prop (n divided by total number of applicants in that year, which means proportions are of people of that sex with that name born in that year).


library(babynames) # Baby Names dataset:
library(ggplot2) # For Data visualization & Graphs
library(dplyr) # For data wrangling and manipulation
library(stringr) # For strings and regex

# Winston
# Given a name, Understand if its popular in 1800 or 2000 etc.


# Save the babynames data into baby_data:

#baby_data <- data.frame(babynames)
baby_data <- as_tibble(babynames)

# Preview the data:
head(baby_data); 
tail(baby_data)

# See the structure
str(baby_data)

# Fix the column names
colnames(baby_data) <- c("Year", "Sex", "Name", "Count", "Proportion")
head(baby_data)

baby_data

# Retrieve specific row based on a condition
baby_data_filter<- filter(baby_data, Name %in% c("Ashley", "Ashleigh"))
baby_data_filter<- filter(baby_data, Name %in% c("Manoj"))
head(baby_data_filter)
tail(baby_data_filter)

# Top 20 baby names
sorted_names <- baby_data %>% group_by(Name, Sex) %>% summarise(Total = sum(Count)) %>%
                arrange(desc(Total))

sorted_names <- as_tibble(sorted_names)
sorted_names

# I want the top twenty baby names, subset the data and select the top twenty rows.
top_twenty_baby <- sorted_names[1:20, ]
top_twenty_baby

top_twenty_baby$Name <- factor(top_twenty_baby$Name, levels = top_twenty_baby$Name[order(top_twenty_baby$Total)])
top_twenty_baby$Sex <- as.factor(top_twenty_baby$Sex)


# App test commands
selected_years <- baby_data %>%
                             filter(year >= 1880, year <= 2017, name == "Anna")
