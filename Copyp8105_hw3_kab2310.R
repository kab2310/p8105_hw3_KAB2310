---
  title: "P8105 Homework 3"
author: "Kamiah Brown"
data: 2024-10-16
output: github_document
---
```{r}
install.packages(
  c("tidyverse", "knitr", "rmarkdown", "janitor", "broom", "here", "readxl", 
    "haven", "rnoaa", "ggridges", "ggthemes", "leaflet", "viridis", "skimr", 
    "rvest", "httr", "flexdashboard", "devtools", "usethis", "modelr", "mgcv", 
    "tidytext", "shiny", "patchwork", "glmnet", "plotly"))

remotes::install_github("p8105/p8105.datasets")
remotes::install_github("ropensci/rnoaa")

```

#Set up 
```{r}
 library(tidyverse)
library(ggridges)
library(haven)
library(knitr)
library(dplyr)
library(lubridate)
library(tidyr)
library(janitor)
library(ggplot2)
library(readr)
```

#Problem 1

```{r}
library(p8105.datasets)
data("ny_noaa")
```
This dataset was collected by the NOAA National Climiatic Data Center on August 15, 2017. The New York NOAA data set contains information from 2017, with 'r nrow(ny_noaa)' observations and 'r ncol(ny_noaa)' variables. Overall, the data points are 
'r (nrow(ny_noaa)*ncol(ny_noaa))'. Variables include weather station id, date of observation,  (tenths of mm), snowfall (mm), snow depth (mm), and min and max temperature (tenths of degrees C). 

```{r}
str(ny_noaa)
head(ny_noaa)
```

```{r}
ny_noaa_cleaned <- ny_noaa |> 
  clean_names() |>                                
  mutate(
    year = year(date),                            
    month = month(date),
    day = day(date),
    tmax = as.numeric(tmax),                      
    tmin = as.numeric(tmin),                     
    prcp = as.numeric(prcp),    
    snow = as.numeric(snow)                     
  ) |> 
  mutate(
    tmax = tmax / 10,                             
    tmin = tmin / 10,
    prcp = prcp / 10,           
    snow = snow / 10                             
  ) |> 
  drop_na() |>                                
  distinct()                                      
           
head(ny_noaa_cleaned)

summary(ny_noaa_cleaned[, c("tmax", "tmin", "prcp", "snow")])
```

```{r}
snowfall_freq <- table(ny_noaa_cleaned$snow)
snowfall_freq_q1 <- sort(snowfall_freq, decreasing = TRUE)

head(snowfall_freq_q1)
```

The commonly observed snowfall value is 0 as it appears 1,112,758 times. 
Its common to have no snowfall considering some areas may havwe warmer climates or warmer seasons where snow is rare/ not common. 

#Two Panel Plots 
```{r}
janjul_data <- ny_noaa_cleaned |>
  filter(month == 1 | month == 7) |>
  group_by(id, year, month) |>
  summarize(avg_tmax = mean(tmax, na.rm = TRUE)) 

ggplot(janjul_data, aes(x = year, y = avg_tmax, color = as.factor(id))) +
  geom_line() +
  facet_wrap(~ month, scales = "free_y") + 
  labs(title = "Average Max Temperature in January and July Across Stations",
       x = "Year", y = "Average Max Temperature (°C)", color = "Station ID") +
  theme_minimal() +
  theme(legend.position = "none")  
```

The left panel shows the average max temp for January from 1980 to 2010. Most of the stations appear to range between -10 and 10 C. After 2000, the temp seems to increase compared to before 2000. There are a few outliers where the temp is between -15 and -20. These stations might be located in colder regions. January has more variablity as well compared to July. 

The right panel shows the average max temp for July from 1980 to 2010. Most of the stations appear to range from 20 to 35C. Unlike January, the temp in July appears to be more stable with only a few outliers before 1990. The outlier seems to dip below -15 C. I expected these temps because July tends to be much warmer than Jan. 

#Panel 1: tmax vs tmin for the full dataset
```{r}
ggplot(ny_noaa_cleaned, aes(x = tmin, y = tmax)) +
  geom_bin2d(bins = 50) +  
  scale_fill_viridis_c() +  
  labs(
    x = "Minimum Temperature (°C)",
    y = "Maximum Temperature (°C)",
    title = "Relationship Between Tmax and Tmin"
  ) +
  theme_minimal()
```

#Panel 2:distribution of snowfall values greater than 0 and less than 100 separately by year
```{r}
snowfall <- ny_noaa_cleaned |>
  filter(snow > 0 & snow < 100)  

ggplot(snowfall, aes(x = snow)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") + 
  facet_wrap(~ year, scales = "free_y") + 
  labs(
    x = "Snowfall (mm)",
    y = "Count",
    title = "Distribution of Snowfall Between 0 and 100 mm by Year"
  ) +
  theme_minimal()
```

#problem 2
```{r}
demographic_data <- read_csv('nhanes_covar.csv', skip = 1, col_names = c("SEQN", "sex", "age", "BMI", "education"))
accelerometer_data <- read_csv('nhanes_accel.csv')
```

```{r}
colnames(demographic_data)

demographic_data_clean <- demographic_data |>
  mutate(
    age = as.numeric(age),
    BMI = as.numeric(BMI),
    sex = factor(sex, levels = c("1", "2"), labels = c("Male", "Female")),
    education = factor(education, levels = c("1", "2", "3"), labels = c("Less than high school", "High school", "Some college"))
  ) |>
  filter(age >= 21) |>  
  drop_na()              

str(demographic_data_clean)

education_gender_table <- demographic_data_clean |>
  group_by(education, sex) |>
  summarise(count = n()) |>
  ungroup()
```

#Create table
```{r}
gender_education_table <- table(demographic_data_clean$education, demographic_data_clean$sex)

gender_education_table
```
There is no gender difference among those who have less than high school education as there are 27 men and 28 women. There are more men than women with a high school education (35 v 23). There are more women with some college compared to men (59 v.56). 
The high school category has a greater difference compared to the 'high school' and 'some college' category. 

#Create visualization
```{r}
ggplot(final_data, aes(x = age, fill = sex)) +
  geom_histogram(binwidth = 5, position = "dodge", alpha = 0.8) + 
  facet_wrap(~ education, scales = "free_y") +  
  labs(
    title = "Age Distribution by Gender and Education Category",
    x = "Age",
    y = "Count",
    fill = "Gender"
  ) +
  theme_minimal()
```
The graph shows the age distribution by gender and education level, categorized into 'less than high school,' 'high school,' and 'some college.' Among those with less than a high school education, the graph reveals that a significant portion of individuals fall in the 60-80 age range, indicating that older individuals are more likely to have less formal education. However, there is also a notable presence of individuals in the 40-60 age group. This suggests that lower education levels are more common among older generations, potentially reflecting historical trends where fewer individuals had access to or pursued higher education. The smaller number of younger individuals in this category suggests that younger generations are receiving higher levels of education, likely due to increased scholastic opportunities and societal changes that promotes education. In the high school category, there are noticeable peaks in the 40-60 age group, suggesting that high school completion is a common level of education across a wide range of ages. Men tend to be more represented in the younger age groups (ages 20-40) compared to women, indicating that younger men are more likely to complete high school compared to their women counterparts. However, in the 70-80 age group, women surpass men, which may reflect historical trends where women returned to education later in life. This difference between genders in the older age group suggests that women in past generations may have had fewer opportunities for early education but returned later to complete their high school education (also displayed in the ‘less than high school’. Lastly, in the some college category, the graph shows a significant concentration of younger individuals, particularly women in the 20-40 age range. This implies that there has been a shift in pursuing higher education beyond high school, especially among younger generations. Women, in particular, are more likely to pursue "some college" education early in life, suggesting that societal trends have shifted towards encouraging women to seek higher education opportunities at younger ages.

#data cleaning; forgot to merge data set together 
```{r}
accelerometer_long <- accelerometer_data |>
  pivot_longer(cols = starts_with("min"), 
               names_to = "minute", 
               values_to = "MIMS")

total_activity_data <- accelerometer_long |>
  group_by(SEQN) |>
  summarise(total_activity = sum(MIMS, na.rm = TRUE))

total_activity_data <- total_activity_data |>
  mutate(SEQN = as.character(SEQN))

demographic_data_clean <- demographic_data_clean |>
  mutate(SEQN = as.character(SEQN))

final_data <- total_activity_data |>
  left_join(demographic_data_clean, by = "SEQN")

head(final_data)
```

#plotting activities
```{r}
ggplot(final_data, aes(x = age, y = total_activity, color = sex)) +
  geom_point(alpha = 0.6) +  
  geom_smooth(method = "lm", se = FALSE) +  
  facet_wrap(~ education, scales = "free_y") +  
  labs(
    title = "Total Activity by Age and Gender, Faceted by Education Level",
    x = "Age",
    y = "Total Activity (MIMS)",
    color = "Gender"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
```
In less than high school, men and women have similar activity levels when they are younger (20-40), but as they age, women's activity tends to be lower than men. There tends to be a decline in activity as age increase, particularly among men which shows that men and women become less physically active as their age increase. 

In high school, women with a high school education tend to be more active with a gradual decline afterwards. Overall, women with a high school education tend to be more active than men. Men tend to be less active and also have a less noticeable decline in activity compared to women. 

For some college, activity levels tend to remain stable across ages among women and men. There tends to be more consistent activity, but women are more likely to be active than men in this category. This also shows that unlike those with a 'less than high school' and 'high school' education, those with 'some college' education are consistently active throughout their life. 

```{r}
ggplot(final_data, aes(x = as.numeric(minute), y = MIMS, color = sex)) +
  geom_line(alpha = 0.6) + 
  geom_smooth(method = "loess", se = FALSE) +  
  facet_wrap(~ education, scales = "free_y") +  
  labs(
    title = "24-Hour Activity Time Courses by Education Level and Gender",
    x = "Time (Minutes from Midnight)",
    y = "Activity (MIMS)",
    color = "Gender"
  ) +
  theme_minimal() + 
  theme(legend.position = "bottom")
```

#three-panel plot that shows the 24-hour activity time courses for each education level and use color to indicate sex
```{r}
accelerometer_long <- accelerometer_data |>
  pivot_longer(cols = starts_with("min"),  
               names_to = "minute",       
               values_to = "MIMS") 

accelerometer_long <- accelerometer_long |>
  mutate(minute = as.numeric(gsub("min", "", minute)))

accelerometer_long <- accelerometer_long |>
  mutate(SEQN = as.character(SEQN))  

demographic_data_clean <- demographic_data_clean |>
  mutate(SEQN = as.character(SEQN)) 

final_data <- accelerometer_long |>
  left_join(demographic_data_clean, by = "SEQN")

ggplot(final_data, aes(x = minute, y = MIMS, color = sex)) +
  geom_line(alpha = 0.6) +  
  geom_smooth(method = "lm", se = FALSE) +  
  facet_wrap(~ education, scales = "free_y") +  
  labs(
    title = "24-Hour Activity Time Courses by Education Level and Gender",
    x = "Time (Minutes from Midnight)",
    y = "Activity (MIMS)",
    color = "Gender"
  ) +
  theme_minimal() + 
  theme(legend.position = "bottom")