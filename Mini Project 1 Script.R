#load tidyverse, psych, dplyr, pwr, and janitor packages
library(tidyverse)
library(psych)
library(dplyr)
library(pwr)
library(janitor)

#Check working directory
getwd()

#Read data file and assign it to an object
edata <- read_csv("Election Legitimacy MTurk.csv")

#View dataset
view(edata)

#Get starting sample size
nrow(edata)

#Step 1: Exclude participants who are missing demographic information
demographics <- edata |> 
  drop_na(Age_w1, Female_w1, Latino_w1, Black_w1, Asian_w1, White_w1, Native_w1, Citizen_w1, Born_w1, Educ_w1, Income_w1)

#Count new N after removing missing demographics
nrow(demographics)

#Count how many participants were removed in Step 1
nrow(edata) - nrow(demographics)

#Step 2: Exclude participants older than 65
under65 <- demographics |> 
  filter(Age_w1 <= 65)

#Count new N after removing participants older than 65
nrow(under65)

#Count how many participants were removed in Step 2
nrow(demographics) - nrow(under65)

#Step 3: Exclude participants who are missing values for National Identity AND Collective Narcissism from Wave 1 AND Wave 2
analytic_sample <- under65 |> 
  drop_na(NID_w1, NID_w2, Coll_Narc_w1, Coll_Narc1_w1, Coll_Narc2_w1, Coll_Narc3_w1, Coll_Narc4_w1, Coll_Narc5_w1, Coll_Narc_w2, Coll_Narc1_w2, Coll_Narc2_w2, Coll_Narc3_w2, Coll_Narc4_w2, Coll_Narc5_w2)

#Count new N
nrow(analytic_sample)

#Check structure of new object
head(analytic_sample)

#Count how many participants were removed in Step 3
nrow(under65) - nrow(analytic_sample)

#Step 4: Check for outliers

#Part 1: Plot age data
ggplot(analytic_sample, aes(y = Age_w1)) +
  geom_boxplot(fill = "steelblue", outlier.color = "red", outlier.size = 3) +
  labs(title = "Boxplot to Check for Age Outliers", y = "Age (Years)") +
  theme_minimal()

#Part 2: Create a total National Identity variable and total Collective Narcissism variable
analytic_sample <- analytic_sample |> 
  mutate(NID_total = rowMeans(across(c(NID_w1, NID_w2)), na.rm = TRUE),
         Coll_Narc_total = rowMeans(across(c(Coll_Narc_w1, Coll_Narc1_w1, Coll_Narc2_w1, Coll_Narc3_w1, Coll_Narc4_w1, Coll_Narc5_w1, Coll_Narc_w2, Coll_Narc1_w2, Coll_Narc2_w2, Coll_Narc3_w2, Coll_Narc4_w2, Coll_Narc5_w2)), na.rm = TRUE))

#Part 3: Plot the NID and CN totals

#National Identity
ggplot(analytic_sample, aes(y = NID_total)) +
  geom_boxplot(fill = "lightblue", outlier.color = "red", outlier.shape = 16) +
  labs(title = "Boxplot of National Identity Total", y = "Scores") +
  theme_minimal()

#Collective Narcissism
ggplot(analytic_sample, aes(y = Coll_Narc_total)) +
  geom_boxplot(fill = "lightpink", outlier.color = "red", outlier.shape = 16) +
  labs(title = "Boxplot of Collective Narcissism Total", y = "Scores") +
  theme_minimal()

#Count how many participants were excluded from the analytic sample
nrow(edata) - nrow(analytic_sample)

#Step 5: Recode Demographic Variables
#Print a summary of each demographic variable column
summary(analytic_sample$Female_w1) #gender
summary(analytic_sample$Asian_w1) #ethnicity has multiple variables that need regrouping into a single variable
summary(analytic_sample$Black_w1)
summary(analytic_sample$Latino_w1)
summary(analytic_sample$Native_w1)
summary(analytic_sample$White_w1)
summary(analytic_sample$Other_text_w1)
summary(analytic_sample$Educ_w1) #education
summary(analytic_sample$Income_w1) #family income
summary(analytic_sample$Citizen_w1) #citizenship
summary(analytic_sample$Born_w1) #birthplace

#Check the first 20 rows
head(analytic_sample$Female_w1, 20) #gender
head(analytic_sample$Asian_w1, 20) #ethnicity has multiple variables that need regrouping into a single variable
head(analytic_sample$Black_w1, 20)
head(analytic_sample$Latino_w1, 20)
head(analytic_sample$Native_w1, 20)
head(analytic_sample$White_w1, 20)
head(analytic_sample$Other_text_w1, 20)
head(analytic_sample$Educ_w1, 20) #education
head(analytic_sample$Income_w1, 20) #family income
head(analytic_sample$Citizen_w1, 20) #citizenship
head(analytic_sample$Born_w1, 20) #birthplace

#Convert decimals to match codebook scales
#Recode Education to the 1-7 scale
analytic_sample$educ2 <- round(analytic_sample$Educ_w1 * 7)
#Recode Income to the 1-11 scale
analytic_sample$income2 <- round(analytic_sample$Income_w1 * 10) + 1

#Check the first 20 rows
head(analytic_sample$educ2, 20)
head(analytic_sample$income2, 20)

#Check for missing values
analytic_sample |> 
  select(Age_w1, Female_w1, educ2, income2, Asian_w1, Black_w1, Latino_w1, Native_w1, White_w1, Citizen_w1, Born_w1) |> 
  summarise(across(everything(), list(
    missing_count = ~sum(is.na(.)),
    missing_percent = ~round(mean(is.na(.)) * 100, 2)
  ))) |> 
  pivot_longer(everything(), 
               names_to = c("Variable", ".value"), 
               names_pattern = "(.*)_(missing_count|missing_percent)")

#Recode demographics to categorical nominal variables
analytic_sample <- analytic_sample |> 
  mutate(
    #Recode gender
    gender = recode_values(
      Female_w1,
      0 ~ "Male",
      1 ~ "Female",
      default = "Other"),
    #Combine ethnicity indicators and regroup into one variable
    ethnicity = case_when(
      Latino_w1 == 1 ~ "Latino",
      Black_w1 == 1  ~ "Black",
      Asian_w1 == 1  ~ "Asian",
      White_w1 == 1  ~ "White",
      Native_w1 == 1 ~ "Native American",
      Other_w1 == 1  ~ "Other"),
    #Recode education
    education = recode_values(
      educ2,
      1 ~ "Grade School",
      2 ~ "Some High School",
      3 ~ "High School Diploma or Equivalent",
      4 ~ "Some College",
      5 ~ "Associate’s Degree",
      6 ~ "Bachelor’s Degree",
      7 ~ "Master’s Degree",
      8 ~ "Advanced Degree"),
    #Recode Income
    income = recode_values(
      income2, 
      1  ~ "Less than $10,000",
      2  ~ "$10,000-$19,999",
      3  ~ "$20,000-$29,999",
      4  ~ "$30,000-$39,999",
      5  ~ "$40,000-$49,999",
      6  ~ "$50,000-$59,999",
      7  ~ "$60,000-$69,999",
      8  ~ "$70,000-$79,999",
      9  ~ "$80,000-$89,999",
      10 ~ "$90,000-$99,999",
      11 ~ "$100,000 or greater"),
    #Recode citizenship status
    citizenship = recode_values(
      Citizen_w1, 
      1  ~ "Yes",
      0  ~ "No"),
    #Recode birthplace
    born_US = recode_values(
      Born_w1,
      1 ~ "Yes",
      0 ~ "No"))

#Check for missing values
analytic_sample |> 
  select(Age_w1, gender, education, income, ethnicity, citizenship, born_US) |> 
  summarise(across(everything(), list(
    missing_count = ~sum(is.na(.)),
    missing_percent = ~round(mean(is.na(.)) * 100, 2)
  ))) %>%
  pivot_longer(everything(), 
               names_to = c("Variable", ".value"), 
               names_pattern = "(.*)_(missing_count|missing_percent)")

#Step 6: Print Demographics
#Age
  mean(analytic_sample$Age_w1, na.rm = TRUE) #Mean
  sd(analytic_sample$Age_w1, na.rm = TRUE) #Standard Deviation
  range(analytic_sample$Age_w1, na.rm = TRUE) #Range
#Gender
analytic_sample |>  
  tabyl(gender) |>  
  adorn_pct_formatting()
#Ethnicity
analytic_sample |>  
  tabyl(ethnicity) |>  
  adorn_pct_formatting()
#Education
analytic_sample |>  
  tabyl(education) |>  
  adorn_pct_formatting()
#Income
analytic_sample |>  
  tabyl(income) |>  
  adorn_pct_formatting()
#Citizenship
analytic_sample |>  
  tabyl(citizenship) |>  
  adorn_pct_formatting()
#Birthplace
analytic_sample |>  
  tabyl(born_US) |>  
  adorn_pct_formatting()

#Run a sensitivity analysis
#Print final sample size
sampl_n <- nrow(analytic_sample)

#Run a sensitivity power analysis
sens_result <- pwr.r.test(
  n = sampl_n, 
  sig.level = 0.05, 
  power = 0.80, 
  r = NULL)

#Print results
print(sens_result)
