Data Exploration
================
Gio Nikolaishvili
</br>18 January 2021

### The Data

Load the raw data.

``` r
# Import dataset
data <- read_csv("https://raw.githubusercontent.com/gnikolaiuoregon/military_cycles/main/real_military_spending_cleaned.csv?token=ASLTQLXG4CKJDXPMTHSTPA3ABNBFW")

# Store the dataset as a dataframe
data <- as.data.frame(data)

# Restrict data to post-1988
data <- data %>% filter(year > 1987)
```

Check the set of top-10 military spenders for the years 2019, 2010, and
2000.

``` r
# Order data by military expenditure
data2019 <- data %>% filter(year == 2019)

# Create new crossectional dataset of expenditure by country for 2019
names <- names(data2019)
names <- names[c(2:length(names))]
country <- names
mil_exp <- c(1:length(names))
for(i in 1:length(names)) {
  mil_exp[i] <- data2019[1,i+1]
}
data2019 <- data.frame(country, mil_exp)

# Arrange new dataset by military expenditure (descending order)
data2019 <- data2019 %>% arrange(desc(mil_exp))

# Keep top-10 spenders
topspend2019 <- data2019[c(1:10),]

# Plot top-10 military expenditure timeplot
ggplot(topspend2019, aes(x=country, y=mil_exp, fill=country)) +
  geom_col() +
  theme_classic() +
  labs(x = "Country", y= "Military Spending ($M)") +
  scale_y_continuous(name="Military Spending ($M)", limits=c(0, 1000000)) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  ggtitle("Top Military Spenders of 2019")
```

![](data_exploration_files/figure-gfm/exp_plots1-1.png)<!-- -->

``` r
# Save the names of top-10 spenders of 2019
top10spend_2019 <- topspend2019$country
top10spend_2019 <- as.vector(top10spend_2019[c(1:10)])

###
###
###
# Repeat the same code as in previous chunk but for 2010
###
###
###

# Order data by military expenditure
data2010 <- data %>% filter(year == 2010)

# Create new crossectional dataset of expenditure by country for 2019
names <- names(data2010)
names <- names[c(2:length(names))]
country <- names
mil_exp <- c(1:length(names))
for(i in 1:length(names)) {
  mil_exp[i] <- data2010[1,i+1]
}
data2010 <- data.frame(country, mil_exp)

# Arrange new dataset by military expenditure (descending order)
data2010 <- data2010 %>% arrange(desc(mil_exp))

# Keep top-10 spenders
topspend2010 <- data2010[c(1:10),]

# Plot top-10 military expenditure timeplot
ggplot(topspend2010, aes(x=country, y=mil_exp, fill=country)) +
  geom_col() +
  theme_classic() +
  labs(x = "Country", y= "Military Spending ($M)") +
  scale_y_continuous(name="Military Spending ($M)", limits=c(0, 1000000)) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  ggtitle("Top Military Spenders of 2010")
```

![](data_exploration_files/figure-gfm/exp_plots1-2.png)<!-- -->

``` r
###
###
###
# Repeat the same code as in previous chunk but for 2000
###
###
###

# Order data by military expenditure
data2000 <- data %>% filter(year == 2000)

# Create new crossectional dataset of expenditure by country for 2019
names <- names(data2000)
names <- names[c(2:length(names))]
country <- names
mil_exp <- c(1:length(names))
for(i in 1:length(names)) {
  mil_exp[i] <- data2000[1,i+1]
}
data2000 <- data.frame(country, mil_exp)

# Arrange new dataset by military expenditure (descending order)
data2000 <- data2000 %>% arrange(desc(mil_exp))

# Keep top-10 spenders
topspend2000 <- data2000[c(1:10),]

# Plot top-10 military expenditure timeplot
ggplot(topspend2000, aes(x=country, y=mil_exp, fill=country)) +
  geom_col() +
  theme_classic() +
  labs(x = "Country", y= "Military Spending ($M)") +
  scale_y_continuous(name="Military Spending ($M)", limits=c(0, 1000000)) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  ggtitle("Top Military Spenders of 2000")
```

![](data_exploration_files/figure-gfm/exp_plots1-3.png)<!-- -->

Take the top-10 2019 spenders (usa, china, india, russia, saudi\_arabia,
france, germany, uk, japan, south\_korea) and create various types of
plots of their military expenditure over the period of 1992-2019.

``` r
# Filter for the top-10 2019 spenders from the original dataset
data_topspenders <- data[,top10spend_2019]
year <- c(1988:2019)
data_topspenders <- mutate(data_topspenders, year)

# Gather 1992 data in single dataframe 
topspendtemp <- data_topspenders %>% filter( year == 1988) 
topspendtemp <- as.vector(topspendtemp)
names(topspendtemp) <- NULL
expenditure <- topspendtemp[c(1:10)]
expenditure <- matrix(expenditure, ncol=1, byrow=TRUE)
year <- c(1:10)
for (i in c(1:10)){
  year[i] = 1988
}
topspendtemp <- data.frame(expenditure, top10spend_2019, year)
topspenders <- topspendtemp 

# Iterate over all other years to gather into dataframe
for (t in c(1989:2019)){
  
  topspendtemp <- data_topspenders %>% filter( year == t) 
  topspendtemp <- as.vector(topspendtemp)
  names(topspendtemp) <- NULL
  expenditure <- topspendtemp[c(1:10)]
  expenditure <- matrix(expenditure, ncol=1, byrow=TRUE)
  year <- c(1:10)
  for (i in c(1:10)){
    year[i] = t
  }
  topspendtemp <- data.frame(expenditure, top10spend_2019, year)
  topspenders <- rbind(topspenders, topspendtemp) 
  
}

# Convert all numeric topspenders variables to double
topspenders$year <- as.double(topspenders$year)
topspenders$expenditure <- as.double(topspenders$expenditure)

# Filter topspenders datasets for year > 1991
topspenders9119 <- topspenders %>% filter(year > 1991)

# Make sure all numeric topspenders9119 variables are doubles as well
topspenders9119$year <- as.double(topspenders9119$year)
topspenders9119$expenditure <- as.double(topspenders9119$expenditure)

###
###
###
# Plot top spenders' expenditure 1991-2019
###
###
###

# Military expenditure ratio timeplot
ggplot(topspenders9119, aes(x= year, y = expenditure, fill=top10spend_2019)) +
  geom_area(position = "fill") +
  geom_area( position = 'fill', colour="black") +
  theme_classic() +
  labs(x = "Year", y = "Ratio of Combined Total Expenditure") +
  ggtitle("Military Expenditure Across Top Military Spenders, 1991-2019") +
  guides(fill=guide_legend(title="Country"))
```

![](data_exploration_files/figure-gfm/exp_plots2-1.png)<!-- -->

``` r
# Stacked military expenditure timeplot
ggplot(topspenders9119, aes(x= year, y = expenditure, fill=top10spend_2019)) +
  geom_area( position = 'stack') +
  geom_area( position = 'stack', colour="black") +
  theme_classic() +
  labs(x = "Year", y = "Combined Total Expenditure ($M)") +
  ggtitle("Military Expenditure Across Top Military Spenders, 1991-2019") +
  guides(fill=guide_legend(title="Country")) 
```

![](data_exploration_files/figure-gfm/exp_plots2-2.png)<!-- -->

``` r
# Create country-specific datasets for later use
chinaspend_df <- topspenders9119 %>% filter(top10spend_2019 == "china")
francespend_df <- topspenders9119 %>% filter(top10spend_2019 == "france")
germanyspend_df <- topspenders9119 %>% filter(top10spend_2019 == "germany")
indiaspend_df <- topspenders9119 %>% filter(top10spend_2019 == "india")
japanspend_df <- topspenders9119 %>% filter(top10spend_2019 == "japan")
russiaspend_df <- topspenders9119 %>% filter(top10spend_2019 == "russia")
saudi_arabiaspend_df <- topspenders9119 %>% filter(top10spend_2019 == "saudi_arabia")
south_koreaspend_df <- topspenders9119 %>% filter(top10spend_2019 == "south_korea")
ukspend_df <- topspenders9119 %>% filter(top10spend_2019 == "uk")
usaspend_df <-topspenders9119 %>% filter(top10spend_2019 == "usa")

# Top 10 dataset w/o US and China
topspenders9119_nousachina <- rbind(francespend_df, germanyspend_df)
topspenders9119_nousachina <- rbind(topspenders9119_nousachina, indiaspend_df)
topspenders9119_nousachina <- rbind(topspenders9119_nousachina, japanspend_df)
topspenders9119_nousachina <- rbind(topspenders9119_nousachina, russiaspend_df)
topspenders9119_nousachina <- rbind(topspenders9119_nousachina, saudi_arabiaspend_df)
topspenders9119_nousachina <- rbind(topspenders9119_nousachina, south_koreaspend_df)
topspenders9119_nousachina <- rbind(topspenders9119_nousachina, ukspend_df)

# Military expenditure timeplot
ggplot(topspenders9119_nousachina, aes(x= year, y = expenditure, color = top10spend_2019, group = top10spend_2019)) +
  geom_line(size=1.5) +
  theme_classic() +
  labs(x = "Year", y = "Total Expenditure ($M)") +
  ggtitle("Military Expenditure of Top Military Spenders (w/o US & China), 1991-2019") +
    scale_color_discrete(name = "Country")
```

![](data_exploration_files/figure-gfm/exp_plots2-3.png)<!-- -->

Transform the original dataset into one with growth rates.

``` r
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

# The following is essentially the same code as the previous chunk

# Gather 1992 data in single dataframe 
data2temp <- data2 %>% filter( year == 1988) 
data2temp <- as.vector(data2temp)
names(data2temp) <- NULL
expenditure <- data2temp[c(2:length(data2temp))]
expenditure <- matrix(expenditure, ncol=1, byrow=TRUE)
year <- c(2:length(data2temp))
for (i in c(1:length(year))){
  year[i] = 1988
}
data2temp <- data.frame(expenditure, country, year)
data2_new <- data2temp 

# Iterate over all other years to gather into dataframe
for (t in c(1989:2019)){
  
  data2temp <- data2 %>% filter( year == t) 
  data2temp <- as.vector(data2temp)
  names(data2temp) <- NULL
  expenditure <- data2temp[c(2:length(data2temp))]
  expenditure <- matrix(expenditure, ncol=1, byrow=TRUE)
  year <- c(1:length(expenditure))
  for (i in c(1:length(year))){
    year[i] = t
  }
  data2temp <- data.frame(expenditure, country, year)
  data2_new <- rbind(data2_new, data2temp) 
  
}

# Convert all numeric topspenders variables to double
data2_new$year <- as.double(data2_new$year)
data2_new$expenditure <- as.double(data2_new$expenditure)

# Filter topspenders datasets for year > 1991
data2_new <- data2_new %>% filter(year > 1991)
```

Plot top-10 military spender growth rates.

``` r
# Create country-specific datasets for later use
chinaspend2_df <- data2_new %>% filter(country == "china")
francespend2_df <- data2_new %>% filter(country == "france")
germanyspend2_df <- data2_new %>% filter(country == "germany")
indiaspend2_df <- data2_new %>% filter(country == "india")
japanspend2_df <- data2_new %>% filter(country == "japan")
russiaspend2_df <- data2_new %>% filter(country == "russia")
saudi_arabiaspend2_df <- data2_new %>% filter(country == "saudi_arabia")
south_koreaspend2_df <- data2_new %>% filter(country == "south_korea")
ukspend2_df <- data2_new %>% filter(country == "uk")
usaspend2_df <- data2_new %>% filter(country == "usa")

# Top 10 dataset w/o US and China
data2_top10 <- rbind(francespend2_df, germanyspend2_df)
data2_top10 <- rbind(data2_top10, indiaspend2_df)
data2_top10 <- rbind(data2_top10, japanspend2_df)
data2_top10 <- rbind(data2_top10, russiaspend2_df)
data2_top10 <- rbind(data2_top10, saudi_arabiaspend2_df)
data2_top10 <- rbind(data2_top10, south_koreaspend2_df)
data2_top10 <- rbind(data2_top10, ukspend2_df)
data2_top10 <- rbind(data2_top10, usaspend2_df)
data2_top10 <- rbind(data2_top10, chinaspend2_df)

# Create a timeplot of top-10 spender expenditure growth rates
ggplot(data2_top10, aes(x= year, y = expenditure, color = country, group = country)) +
  geom_line(size=1.5) +
  theme_classic() +
  labs(x = "Year", y = "Total Expenditure Growth Rate (%)") +
  ggtitle("Military Expenditure Growth Rates of Top Military Spenders, 1991-2019") +
    scale_color_discrete(name = "Country")
```

![](data_exploration_files/figure-gfm/exp_plots3-1.png)<!-- -->

### Preliminary Factor Analysis

Apply simple principal component analysis to estimate expenditure growth
rate factors among the top 10 spenders.

``` r
#
# Estimate factors f_{1,t} and f_{2,t}
# (Similar to Jeremy's code)
#

# Form a data matrix
X1 = data2[,"china"]
X2 = data2[,"usa"]
X3 = data2[,"uk"]
X4 = data2[,"france"]
X5 = data2[,"germany"]
X6 = data2[,"india"]
X7 = data2[,"japan"]
X8 = data2[,"russia"]
X9 = data2[,"saudi_arabia"]
X10 = data2[,"south_korea"]
X <- data.frame(X1,X2,X3,X4,X5,X6,X7,X8,X9,X10)
names(X) <- NULL
X <- as.matrix(X)

# Center and scale the matrix
X = scale(X, center = TRUE, scale = TRUE)

# Form the principal component based weight matrix for dynamic factors.
pca = prcomp(na.omit(X), scale = TRUE)
weights = pca$rotation 

# Form the principal component based estimates of dynamic factors
factors = X %*% weights 

# Capture the first two factors
factor1 = factors[,1]
factor2 = factors[,2]

#
# Put factors together into dataframe 
#
year = c(1988:2019)

# Create dataframe for factor1
factor_value <- factor1
factor_name <- c(1:length(factor_value))
for (i in c(1:length(factor_name))){
  factor_name[i] = "1st Component"
}
factor1_df <- data.frame(year, factor_value, factor_name)

# Create dataframe for factor2
factor_value <- factor2
factor_name <- c(1:length(factor_value))
for (i in c(1:length(factor_name))){
  factor_name[i] = "2nd Component"
}
factor2_df <- data.frame(year, factor_value, factor_name)

# rBind factor 1 & 2 dataframes
factors <- rbind(factor1_df, factor2_df)

# Create timeplot for factors 1 & 2
ggplot(factors, aes(x= year, y = factor_value, color = factor_name, group = factor_name)) +
  geom_line(size=1.5) +
  theme_classic() +
  labs(x = "Year", y = "Factor Value") +
  ggtitle("Military Expenditure Growth Factors of Top Military Spenders, 1991-2019") +
    scale_color_discrete(name = "Factor")
```

![](data_exploration_files/figure-gfm/simple_pca-1.png)<!-- -->
