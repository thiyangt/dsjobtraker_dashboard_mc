library(tidyverse)
library(readxl)
library(tm)

data <- read_csv("DSjobtracker_2022.csv")

# Filter the columns

column_names <- c("Job_Level", "Duplicate/Unique", "Experience UL (in years)", 
                  "Employment type",
                  "Work Setting", "Salary UL", "Modelling", "VBA macro", "EDA",                                                 
                  "Hypothesis Testing", "Data Analytics", "Statistics",                                           
                  "Data Cleaning", "Survey Design",  "Dashboard Creation",                                   
                  "Data Governance", "Data Stratergy", "Data Policy",                                          
                  "Data Engineering", "Software_development_process", "Data Structuring",
                  "Vertica", "SOQL", "SOSL", "Quickbooks", "Outlook", "Data Studio",
                  "PostgreSQL", "Shopify", "Looker", "Snowflake", "MixPanel",
                  "EC2", "EMR", "Athena", "DynamoDB", "Safe Habour", "Glue",                                                 
                  "Lambda", "RDS", "Kinesis", "ECS", "Fargate", "Analytical Skills",                                    
                  "Intepretation", "Project Management", "Entrepreneurial Skills",                               
                  "Motivation", "Time Management", "Report Writting",  "Multi-tasking",
                  "Flash_Actionscript", "Wordpress", "Recruitments", "Payrolls",                                             
                  "Branding", "ETL/ELT", "VMware", "Azure", "Saleforce Data Loader",                                
                  "Copado Data Deployer", "Talend open studio",                                  
                  "Experience with remote sensing", "Shell", "Go", "PyTorch",                                             
                  "ArcGIS", "PostGIS", "STATA", "Leadership", "Collaboration Skills",                                 
                  "Research Skills", "Crystal Reports", "CIMA", "ACCA", "CIM",                                                   
                  "CA", "CFA", "Chartered Engineer",
                  "Educational qualifications (if they need more than 1)", "Diploma",
                  "Mphil")

data1 <- data %>% select(-column_names)

# Renaming column names

data1 <- data1 %>% rename( "Salary"="Salary LL", 
                         "English Needed"="English proficiency", "BSc_needed"="BSc",
                         "MSc_needed"="MSc", "PhD_needed"="Dphil/PhD","Job_Field"="Industry", "Minimum Experience in Years"="Experience LL (in years)","Job_Category"="Job_Field"
                        )

data1 <- data1 %>% mutate(Knowledge_in = rep(NA, 360),`English proficiency description` = rep(NA,360),
                          Location=rep(NA,360))


# Creating new variables

# Categorizing experience variable

# data1 <- data1 %>% mutate(Experience_Category = case_when(
#   `Experience LL (in years)` %in% c(0,0.5,1,2) ~ "Two or less years",
#   `Experience LL (in years)` %in% c(3,4,5) ~ "More than 2 and less than 5 years",
#   `Experience LL (in years)` %in% c(6:10) ~ "More than 5 and less than 10 years",
#   `Experience LL (in years)` %in% c(11,12) ~ "More than 10 years",
#   `Experience LL (in years)` %in% c("NA", NA) ~ "Unknown or Not needed"
#   
# ))

# Removing experience column
#data1 <- data1 %>% select(-`Experience LL (in years)`)


# Categorizing payment frequency

data1 <- data1 %>% mutate(`Payment Frequency` = case_when(
  `Payment Frequency` == "NA" ~ NA,
  `Payment Frequency` == "Monthly" ~ "monthly",
  `Payment Frequency` == "Hourly" ~ "hourly",
  `Payment Frequency` %in% c("Annually", "Annual") ~ "annual",
  `Payment Frequency` == "Daily" ~ "daily",
  `Payment Frequency` == "Yearly" ~ "yearly"
  
))

# Categorizing location column

data1 <- data1 %>% mutate(Country = case_when(
  Country %in% c("Sri Lanka", "Sri lanka") ~ "Sri Lanka",
  Country %in% c("NA", NA, "EMEA", "South Asia") ~ NA,
  Country %in% c("UAE", "Dubai, UAE", "United Arab Emirates") ~ "United Arab Emirates",
  Country %in% c("USA", "United States of America", "United States") ~ "United States",
  Country %in% c("UK", "England", "Ireland", "United Kingdom") ~ "United Kingdom",
  Country %in% c("Austra", "Austrailia", "Australia") ~ "Australia",
  Country == "Singapore" ~ "Singapore",
  Country == "New Zealand" ~ "New Zealand",
  Country == "Tunisia" ~ "Tunisia",
  Country == "Japan" ~ "Japan",
  Country == "Denmark" ~ "Denmark",
  Country == "Hungary" ~ "Hungary",
  Country == "India" ~ "India",
  Country == "Germany" ~ "Germany",
  Country == "Greece" ~ "Greece",
  Country == "Lithuania" ~ "Lithuania",
  Country == "Sweden" ~ "Sweden",
  Country == "Poland" ~ "Poland",
  Country == "Malaysia" ~ "Malaysia",
  Country == "Spain" ~ "Spain",
  Country == "Belgium" ~ "Belgium",
  Country == "Vietnam, Thailand, Indonesia" ~ "Thailand",
  Country == "Ecuador" ~ "Ecuador",
  Country == "Netherlands" ~ "Netherlands",
  Country == "Finland" ~ "Finland",
  Country == "Romania" ~ "Romania",
  Country == "Mexico" ~ "Mexico",
  Country == "Italy" ~ "Italy",
  Country == "Virginia" ~ "Virginia",
  Country == "Czech Republic" ~ "Czech Republic",
  Country == "Turkey" ~ "Turkey",
  Country == "China" ~ "China",
  Country == "Canada" ~ "Canada",
  Country == "Switzerland" ~ "Switzerland",
  Country == "Russia" ~ "Russia",
  Country == "Malta" ~ "Malta",
  Country == "Qatar" ~ "Qatar",
  Country == "Portugal" ~ "Portugal",
  Country == "Bulgaria" ~ "Bulgaria"
))



# Country code

data1 <- data1 %>% mutate(country_code = case_when(
  Country == "United States"~ "USA",
  Country == "Sri Lanka"~ "LKA",
  Country == "Australia"~ "AUS",
  Country == "United Kingdom"~ "GBR",
  Country == "Canada"~ "CAN",
  Country == "United Arab Emirates"~ "ARE",
  Country == "India"~ "IND",
  Country == "United States"~ "USA",
  Country == "Sweden"~ "SWE",
  Country == "China"~ "CHN",
  Country == "Singapore"~ "SGP",
  Country == "New Zealand"~ "NZL",
  Country == "Spain"~ "ESP",
  Country == "Russia"~ "RUS",
  Country == "Ireland"~ "IRL",
  Country == "Japan"~ "JPN",
  Country == "Greece"~ "GRC",
  Country == "Belgium"~ "BEL",
  Country == "Ecuador"~ "ECU",
  Country == "Mexico"~ "MEX",
  Country == "Turkey"~ "TUR",
  Country == "Switzerland"~ "CHE",
  Country == "Qatar"~ "QAT",
  Country == "Belgium"~ "BEL",
  Country == "Denmark"~ "DNK",
  Country == "Germany"~ "DEU",
  Country == "Poland"~ "POL",
  Country == "Netherlands"~ "NLD",
  Country == "Italy"~ "ITA",
  Country == "Portugal"~ "PRT",
  Country == "Hungary"~ "HUN",
  Country == "Lithuania"~ "LTU",
  Country == "Malaysia"~ "MYS",
  Country == "Thailand"~ "THA",
  Country == "Finland"~ "FIN",
  Country == "Virginia"~ "VIR",
  Country == "Bulgaria"~ "BGR",
  Country == "Ireland"~ "IRL",
  Country == "Tunisia"~ "TUN",
  Country == "Romania" ~ "ROU",
  Country == "Czech Republic"~ "CZE",
  Country == "Malta"~ "MLT"
))

# Education qualification

data1$Educational_qualifications <- paste(data1$BSc_needed,data1$MSc_needed,data1$PhD_needed,sep=",")

# Export dataset

write_csv(data1, "DStidy_2022.csv")
