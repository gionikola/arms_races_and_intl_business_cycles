
# Install and load all necessary packages
if (!require("pacman")) install.packages("pacman")
library(pacman)
## Note: The `p_load()` function from the pacman package is a convenient way to 
## install (if necessary) and load packages all at once. You can think of this
## as an alternative to the normal `library(ggplot2); library(here);...` way of
## loading R packages.
pacman::p_load(ggplot2, 
               magrittr, 
               Hmisc, 
               AER, 
               ISLR,
               tidyr,
               dplyr, 
               readr)

# Import dataset
data <- read_csv("real_military_spending_cleaned.csv")

# Store the dataset as a dataframe
data <- as.data.frame(data)

# Restrict data to post-1988
data <- data %>% filter(year > 1987)

# Copy data into data2
data2 <- data

# Gather country names
names <- names(data2)
names <- names[c(2:length(names))]
country <- names

# Transform to logs
for (i in c(2:length(data2[1,]))) {
  data2[,i] <- log(data2[,i])
}

# Transform to first differences
for (i in c(2:length(data2[1,]))) {
  data2[,i] <- 100*c(NA, diff(data2[,i], differences = 1))
}

# Remove sparsely observed columns
data3 <- data2 %>%
          select(-c(africa, north_africa, sub_saharan, somalia,
                    americas, central_america_and_the_caribbean, 
                    north_america, south_america,
                    asia_and_oceania, central_asia, east_asia, north_korea, 
                    south_asia, south_east_asia, oceania, 
                    europe, central_europe, german_dr, 
                    yugoslavia, eastern_europe, ussr, western_europe,
                    middle_east, syria, north_yemen))

# Convert data from wide to long form
data4 <- reshape(data3,
                 direction = "long",
                 varying = list(names(data3)[2,length(names(data3))]),
                 v.names = "Value",
                 idvar = c("Country"))
