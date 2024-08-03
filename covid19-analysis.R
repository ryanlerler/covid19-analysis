## Task 1 Data Preparation

# Load the data
covid19_malaysia <- read.csv('covid19_malaysia.csv', header=TRUE)

# Check for missing values
missing_values <- sum(is.na(covid19_malaysia))

if (missing_values == 0) {
  print('There are no missing values in the dataset.')
} else {
  print('There are some missing values in the dataset.')
}

# Check if each column's data type is correct
print(str(covid19_malaysia)) # print the structure of entire df including data types of all columns

sno_type <- typeof(covid19_malaysia[[1]][1])
date_type <- typeof(covid19_malaysia[[2]][1])
state_type <- typeof(covid19_malaysia[[3]][1])
cases_new_type <- typeof(covid19_malaysia[[4]][1])
cases_import_type <- typeof(covid19_malaysia[[5]][1])
cases_recovered_type <- typeof(covid19_malaysia[[6]][1])
cases_active_type <- typeof(covid19_malaysia[[7]][1])
cases_unvax_type <- typeof(covid19_malaysia[[8]][1])

if (sno_type =='integer' && date_type =='character' && state_type == 'character' && cases_new_type == 'integer' && cases_import_type == 'integer' && cases_recovered_type == 'integer' && cases_active_type == 'integer' && cases_unvax_type == 'integer') {
  print('Data types in all columns are correct')
} else {
  print('Data type(s) in some column(s) is/ are incorrect')    
  }

## Task 2 Data Exploration


# Summary, no. of records, max. no. of confirmed and cured cases
summary <- summary(covid19_malaysia)
print("Summary of the dataset's statistics: ")
print(summary)

row_count <- nrow(covid19_malaysia)
print("Total number of record in the dataset: ")
print(row_count)

max_confirmed <- max(covid19_malaysia$cases_new)
day_max_confirmed <- covid19_malaysia$date[covid19_malaysia$cases_new==max_confirmed]
print("The day with the highest number of confirmed cases: ")
print(day_max_confirmed)

max_cured <- max(covid19_malaysia$cases_recovered)
day_max_cured <- covid19_malaysia$date[covid19_malaysia$cases_recovered==max_cured]
print("The day with the highest number of cured cases: ")
print(day_max_cured)

# Distribution of COVID-19 cases
library(ggplot2)
ggplot(covid19_malaysia, aes(x=date, y=cases_new)) + geom_line()


## Task 3 Data Manipulation

# Total no. of confirmed and cured cases by state
print('Total no. of confirmed cases by state: ')
for(i in unique(covid19_malaysia$state)){
  print(paste(i, ':' , sum(covid19_malaysia$cases_new[covid19_malaysia$state==i])))
}

print('Total no. of cured cases by state: ')
for(i in unique(covid19_malaysia$state)){
  print(paste(i, ':' , sum(covid19_malaysia$cases_recovered[covid19_malaysia$state==i])))
}

# Total no. of confirmed cases by year
total_cases_count_2020 = sum(covid19_malaysia$cases_new[1:5472])
total_cases_count_2021 = sum(covid19_malaysia$cases_new[5473:11312])
total_cases_count_2022 = sum(covid19_malaysia$cases_new[11313:17152])
total_cases_count_2023 = sum(covid19_malaysia$cases_new[17153:22992])
total_cases_count_2024 = sum(covid19_malaysia$cases_new[22993:26000])

# Average no. of confirmed cases per month by year
average_cases_per_month_2020 = total_cases_count_2020/12
average_cases_per_month_2021 = total_cases_count_2021/12
average_cases_per_month_2022 = total_cases_count_2022/12
average_cases_per_month_2023 = total_cases_count_2023/12 
average_cases_per_month_2024 = total_cases_count_2024/7 # Only 7-month of data in year 2024

# Total no. of cured cases by year
total_cured_count_2020 = sum(covid19_malaysia$cases_recovered[1:5472])
total_cured_count_2021 = sum(covid19_malaysia$cases_recovered[5473:11312])
total_cured_count_2022 = sum(covid19_malaysia$cases_recovered[11313:17152])
total_cured_count_2023 = sum(covid19_malaysia$cases_recovered[17153:22992])
total_cured_count_2024 = sum(covid19_malaysia$cases_recovered[22993:26000])

# Average no. of cured cases per month by year
average_cured_per_month_2020 = total_cured_count_2020/12
average_cured_per_month_2021 = total_cured_count_2021/12
average_cured_per_month_2022 = total_cured_count_2022/12
average_cured_per_month_2023 = total_cured_count_2023/12
average_cured_per_month_2024 = total_cured_count_2024/7 # Only 7-month of data in year 2024

# Total no. of confirmed and cured cases from Jan 2020 to Jul 2024
grand_total_cases_count = sum(covid19_malaysia$cases_new)
grand_total_cured_count = sum(covid19_malaysia$cases_recovered)

# Average no. of confirmed and cured cases per month from Jan 2020 to Jul 2024
grand_total_month_count = sum((2023-2020+1)*12+7) # Only 7-month of data in year 2024
grand_average_cases_per_month = grand_total_cases_count/grand_total_month_count
grand_average_cured_per_month = grand_total_cured_count/grand_total_month_count

## Task 4 Data Analysis

# Average no. of confirmed cases per month by year
average_cases_per_month <- data.frame(
  year = c(2020, 2021, 2022, 2023, 2024),
  average_cases = c(
    average_cases_per_month_2020,
    average_cases_per_month_2021,
    average_cases_per_month_2022,
    average_cases_per_month_2023,
    average_cases_per_month_2024
  )
)

ggplot(average_cases_per_month, aes(x = year, y = average_cases)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "Average Number of Confirmed Cases per Month (2020-2024)",
       x = "Year",
       y = "Average Number of Cases per Month") +
  theme_minimal()

# Average no. of cured cases per month by year
average_cured_per_month <- data.frame(
  year = c(2020, 2021, 2022, 2023, 2024),
  average_cured = c(
    average_cured_per_month_2020,
    average_cured_per_month_2021,
    average_cured_per_month_2022,
    average_cured_per_month_2023,
    average_cured_per_month_2024
  )
)

ggplot(average_cured_per_month, aes(x = year, y = average_cured)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "Average Number of Cured Cases per Month (2020-2024)",
       x = "Year",
       y = "Average Number of Cured Cases per Month") +
  theme_minimal()

# Average no. of confirmed and cured cases per month from Jan 2020 to Jul 2024
average_cases_summary <- data.frame(
  case_type = c("Confirmed Cases", "Cured Cases"),
  average_cases = c(grand_average_cases_per_month, grand_average_cured_per_month)
)

ggplot(average_cases_summary, aes(x = case_type, y = average_cases, fill = case_type)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Average Number of Confirmed and Cured Cases per Month (Jan 2020 - Jul 2024)",
    x = "Case Type",
    y = "Average Number of Cases"
  ) +
  scale_fill_manual(values = c("Confirmed Cases" = "blue", "Cured Cases" = "green")) +
  theme_minimal()

# Examine the relationship between confirmed and cured cases
ggplot(covid19_malaysia, aes(x = cases_new, y = cases_recovered)) +
  geom_point(color = 'blue') +
  geom_smooth(method = 'lm', color = 'red', se = FALSE) +  # Add regression line
  labs(
    title = "Correlation between Confirmed and Cured Cases",
    x = "Number of Confirmed Cases",
    y = "Number of Cured Cases"
  ) +
  theme_minimal()

correlation <- cor(covid19_malaysia$cases_new, covid19_malaysia$cases_recovered)
print(paste("Correlation coefficient:", correlation))