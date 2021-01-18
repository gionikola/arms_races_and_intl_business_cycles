
# Import raw github file reader package
library(readr)
library(ggplot2)
library(dplyr)

# Import dataset
data <- read_csv("https://raw.githubusercontent.com/gnikolaiuoregon/military_cycles/main/real_military_spending_cleaned.csv?token=ASLTQLXG4CKJDXPMTHSTPA3ABNBFW")
data <- as.data.frame(data)


# Transform to logs
for (i in c(2:length(data[1,]))) {
  data[,i] <- log(data[,i])
}

# Transform to first differences
for (i in c(2:length(data[1,]))) {
data[,i] <- 100*c(NA, diff(data[,i], differences = 1))
}

# Plot 
ggplot(data, aes(x=year)) + 
  ylab("Growth Rate (%)") +
  xlab("Time") +
  ggtitle('Military Expenditure Timeplots') +
  geom_line(aes(y=georgia),linetype="solid", size=0.75, color = "red") +
  geom_line(aes(y=usa),linetype="solid", size=0.75, color = "green") +
  scale_color_grey() +
  theme_classic()

# Plot region growth rates
ggplot(data, aes(x=year)) + 
  ylab("Growth Rate (%)") +
  xlab("Time") +
  ggtitle('Military Expenditure Growth') +
  geom_line(aes(y=usa),linetype="solid", size=0.75, color = "red") +
  geom_line(aes(y=china),linetype="solid", size=0.75, color = "green") +
  scale_color_grey() +
  theme_classic()

# Filter for years > 1988
data88 <- data %>% filter(year > 1987)

# Plot region growth rates
ggplot(data88, aes(x=year)) + 
  ylab("Growth Rate (%)") +
  xlab("Time") +
  ggtitle('Military Expenditure Growth') +
  geom_line(aes(y=usa, color = "red")) +
  geom_line(aes(y=china)) +
  geom_line(aes(y=uae)) +
  geom_line(aes(y=israel)) +
  geom_line(aes(y=russia, color = "blue")) +
  theme_classic()

# Check correlations
cor(data88$usa, data88$russia, "pairwise.complete.obs")
cor(data88$usa, data88$south_korea, "pairwise.complete.obs")
cor(data88$usa, data88$israel, "pairwise.complete.obs")
cor(data88$usa, data88$uae, "pairwise.complete.obs")
cor(data88$usa, data88$uk, "pairwise.complete.obs")
cor(data88$usa, data88$canada, "pairwise.complete.obs")







