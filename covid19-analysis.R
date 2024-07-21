## Task 1 Data Preparation

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

