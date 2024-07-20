## Task 1 Data Preparation

# Check for missing values
missing_values <- sum(is.na(covid19_malaysia))

if (missing_values == 0) {
  print('There are no missing values.')
} else {
  print('There are some missing values.')
}

# Check if each column's data types is correct
print(str(covid19_malaysia[[2]]=='date'))

