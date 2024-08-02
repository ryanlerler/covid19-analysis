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
  print('Data type(s) in some column(s) is/ are not correct')    
  }


## Task 2 Data Exploration


summary <- summary(covid19_malaysia)
row_count <- nrow(covid19_malaysia)

max_confirmed <- max(covid19_malaysia$cases_new)
day_max_confirmed <- covid19_malaysia$date[covid19_malaysia$cases_new==max_confirmed]

max_cured <- max(covid19_malaysia$cases_recovered)
day_max_cured <- covid19_malaysia$date[covid19_malaysia$cases_recovered==max_cured]

# Create visualizations to show the distribution of COVID-19 cases
library(ggplot2)
ggplot(covid19_malaysia, aes(x=date, y=cases_new)) + geom_line()


## Task 3 Data Manipulation

# Aggregate the data to calculate the average number of cured, deaths, and confirmed cases all over countries.
total_cases_count = sum(covid19_malaysia$cases_new)
total_cured_count = sum(covid19_malaysia$cases_recovered)
total_month_count = sum((2023-2020+1)*12+7)

# Calculate the average number of cases for each month.
average_cases_per_month = total_cases_count/total_month_count


## Task 4 Data Analysis

