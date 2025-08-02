# set working directory
setwd("C:/Users/Emma Tergunwa/Desktop/PalmoriaData")
getwd()

# load library
library(tidyr)
library(readr)
library(ggplot2)
library(tidycharts)
library(tidyselect)
library(tidyverse)
library(dplyr)

# Load data
Palmo <- read.csv("C:/Users/Emma Tergunwa/Desktop/PalmoriaData/PalmoriaData1.csv")

# check head and tail of Palmo data
head(Palmo)
tail(Palmo)

# check dimension of Palmo data
dim(Palmo)

# check the data structure of Palmo
str(Palmo)

# NOTE: gsub("[$,]", "", ...) removes both dollar signs ($) and commas (,)
# as.numeric(...) converts the cleaned text to numbers
# Clean Salary column, remove dollar sign
Palmo$Salary <- as.numeric(gsub("[$,]", "", Palmo$Salary))

# Clean Totalpaid column, remove dollar sign
Palmo$TotalPaid <- as.numeric(gsub("[$,]", "", Palmo$TotalPaid))

# Clean BonusAmount column, remove dollar sign and comma
Palmo$BonusAmount <- as.numeric(gsub("[$,]", "", Palmo$BonusAmount))

# Check whether the dollar sign removed
summary(Palmo$Salary)
summary(Palmo$Totalpaid)
summary(Palmo$BonusAmount)

str(Palmo)

# export Palmo as csv doc
write.csv(Palmo, "Palmo.csv", row.names = FALSE)

# QUESTION ONE
# What is the gender distribution in the organization? distill to Location and Departments?
#  Group by Location, Department, and Gender, then count
gender_distribution <- Palmo %>%
  group_by(Location, Department, Gender) %>%
  summarise(Count = dplyr::n(), .groups = 'drop') %>%
  arrange(Location, Department, Gender)

# view the result
print(gender_distribution)

# Bar plot of Ratings by Gender
# x = as.factor(Rating): Treats Rating as a category, not a number.
# fill = Gender: Colors the bars by Gender (e.g., Male, Female).
# position = "dodge" places bars for different genders side-by-side instead of stacking them.
ggplot(Palmo, aes(x = as.factor(Rating), fill = Gender)) +
  geom_bar(position = "dodge") +
  labs(title = "Rating Distribution by Gender", x = "Rating", y = "Count") +
  theme_minimal()

# Boxplot to compare Rating spread across Gender
ggplot(Palmo, aes(x = Gender, y = Rating, fill = Gender)) +
  geom_boxplot() +
  labs(title = "Rating Spread by Gender", x = "Gender", y = "Rating") +
  theme_minimal()

# QUESTION TWO
# View unique values
unique(Palmo$Salary)
str(Palmo$Salary)
summary(Palmo$Salary)

# Check for gender pay gap
Palmo %>%
  group_by(Gender) %>%
  summarise(
    Count = n(),
    Average_Salary = mean(Salary, na.rm = TRUE),
    Median_Salary = median(Salary, na.rm = TRUE)
  )

# identify the affected department
Palmo %>%
  group_by(Department, Gender) %>%
  summarise(
    Avg_Salary = mean(Salary, na.rm = TRUE),
    Count = n()
  ) %>%
  arrange(Department)

# check by region
Palmo %>%
  group_by(Location, Gender) %>%
  summarise(
    Avg_Salary = mean(Salary, na.rm = TRUE),
    Count = n()
  ) %>%
  arrange(Location)

# boxplot salary by gender
ggplot(Palmo, aes(x = Gender, y = Salary, fill = Gender)) +
  geom_boxplot() +
  labs(title = "Salary Distribution by Gender", x = "Gender", y = "Salary") +
  theme_minimal()

# By department
ggplot(Palmo, aes(x = Department, y = Salary, fill = Gender)) +
  geom_boxplot() +
  theme_minimal() +
  coord_flip() +
  labs(title = "Salary by Department and Gender")

# QUESTION THREE
# A recent regulation was adopted which requires manufacturing companies to pay employees a minimum of $90,000
# 1 Does Palmoria meet this requirement?
# 2 Show the pay distribution of employees grouped by a band of $10,000. For example:
# 3 How many employees fall into a band of $10,000 – $20,000, $20,000 – $30,000,etc.?
# 4 Also visualize this by regions

unique(Palmo$Department)

# Create salary bands
breaks <- seq(10000, ceiling(max(Palmo$Salary, na.rm = TRUE) / 10000) * 10000 + 10000, by = 10000)
labels <- paste0(
  "$", head(breaks, -1),
  "–$", tail(breaks, -1)
)

Palmo$Salary_Band <- cut(
  Palmo$Salary,
  breaks = breaks,
  labels = labels,
  include.lowest = TRUE,
  right = FALSE
)

# Count by band
salary_band_counts <- Palmo %>%
  group_by(Salary_Band) %>%
  summarise(Count = n())

print(salary_band_counts)

# Visualize 
ggplot(Palmo, aes(x = Salary_Band, fill = Location)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Salary Band Distribution by Region",
    x = "Salary Band",
    y = "Number of Employees"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# CASE QUESTION
# QUESTION 5
# Bonus and total pay
# Calculate bonus amount
#  Calculate the amount to be paid as a bonus to individual employees
# Calculate the total amount to be paid to individual employees (salary inclusive of of bonus)                                                              bonus)
# Total amount to be paid out per region and company-wide 

# 5.1 Salary plus Bonus paid
Palmo$TotalPaid <- Palmo$Salary + Palmo$BonusAmount
head(Palmo[, c("Salary", "BonusAmount", "TotalPaid")])
# summary of data
summary(Palmo$BonusAmount)
summary(Palmo$TotalPaid)

# 5.2 Total Payout Per Region
# Group by region and calculate total payouts
region_payout <- Palmo %>%
  group_by(Location) %>%
  summarise(
    Total_Regional_Payout = sum(TotalPaid, na.rm = TRUE),
    Total_Employees = n()
  )

# View result
print(region_payout)

# 5.3 Company-Wide Total Payout
company_payout <- Palmo %>%
  summarise(
    Total_Company_Payout = sum(TotalPaid, na.rm = TRUE),
    Total_Employees = n()
  )

# View result
print(company_payout)

# Export the Results
write.csv(region_payout, "Region_Payout_Summary.csv", row.names = FALSE)
write.csv(company_payout, "Company_Payout_Summary.csv", row.names = FALSE)

sink("console_output.txt")
summary(Palmo$Salary)
print(region_payout)
sink()





























