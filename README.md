# World-Life-Expectancy
## Background: This project seeks to find the trend for world life expectancy and create model that predicts life expectancy using different predictors such as GDP, fertility rate, etc.
## Result: The world is heading to higher life expectancy and lower fertility.


R-programming language
```{r}
getwd()
```


# Setup
Do whatever setup you do here, such as loading libraries

```{r setup, message=FALSE}
# Load standard libraries
library("tidyverse")
library("dplyr")
library("margins")
```


# Question 1 

## Question 1.1.1
Your code/explanations here

Life expectancy measures how long a person is expected to live. Period life expectancy assumes mortality rates are the same in the future, whereas cohort life expectancy assumes changes in future mortality rates.


## Question 1.1.2
Your code/explanations here
```{r}
getwd()
data <- read.delim("/Users/kuan-weilin/data/gapminder.csv.bz2")
```

I removed all the NAs in life expectancy, year, country name, fertility rate, and blank values in region.

```{r}
summary(data)

clean_data <- data %>%
  filter(!is.na(lifeExpectancy)) %>%
  filter(!is.na(time)) %>%
  filter(!is.na(name)) %>%
  filter(!is.na(fertilityRate)) %>%
  filter(region != "")
```


## Question 1.1.3

### a

Total number of countries in this data set after cleaning is 203
```{r}
n_distinct(clean_data$name)
```
### b

First year is 1960, last year is 2019.
```{r}
range(clean_data$time) 
```

### c
The lowest life expectancy is 18.9 in Cambodia, while the highest is 85.41 in San Marino.

```{r}
range(clean_data$lifeExpectancy)

clean_data %>%
  filter(lifeExpectancy == 85.41707 | lifeExpectancy == 18.90700)
```

### d
According to Wikipedia, Cambodia suffered the lowest mortality rate in 1977 because of Cambodian genocide as well as stravation and forced labor.


## Question 1.1.4

I chose additional countries such as South Africa and Spain because I think it'd be interesting to include at least 1 country for every continent to see the life expectancy across different regions/ continents.

```{r}
selected_countries <- clean_data %>%
  filter(name %in% c("China", "Cambodia", "United States of America", "Korea, Republic of", "South Africa", "Spain"))
```

```{r}
ggplot(clean_data, aes(x = time, y = lifeExpectancy, group = name)) +
  geom_line(color = "gray", alpha = 0.5) +
  geom_line(data = selected_countries, aes(color = name)) +
  labs(x = "Year", y = "LifeExpectancy", title = "Life Expectancy Over Years")+
  labs(color = "Country") +
  theme_minimal()
```
## Question 1.1.5

As the year (x-axis) increases, the life expectancy (y-axis) also increases. The margin of increase in life expectancy is especially large in developing countries such as Cambodia and China and not as large in U.S and Spain. According to Wikipedia, there is an obvious drop of life expectancy in 1970s for Cambodia (due to starvation and forced labor), and there is another drop in South Africa (due to HIV) in early 2000s.

## Question 1.1.6

```{r}
ggplot() +
  geom_path (data=clean_data, aes(x=fertilityRate, y=lifeExpectancy, group = name), color = 'gray',arrow =arrow (length = unit(0.25, "inches")), size = 1)+
  geom_path(data = selected_countries, aes(x=fertilityRate, y=lifeExpectancy, color = name), arrow =arrow (length = unit(0.25, "inches")), size = 1) +
  labs(x = "FertilityRate", y = "LifeExpectancy", title = "Life Expectancy & Fertility Rate") +
  labs(color = "Country") +
  theme_minimal()
```
## Question 1.1.7

The world is going to lower fertility rate and higher life expectancy. One possible interpretation can be as the world's medicine and hospital care become more and more advanced in treating illness, people across the world are living longer.

# Question 1.2

## Question 1.2.1

The life expectancy should not be logged because life expectancy has a definite lower and upper bound unlike prices or income. Also, the deviations from normality are not very large as shown in below graph, it is not a fat-tailed distribution.

```{r}
hist(clean_data$lifeExpectancy, col="skyblue", breaks=30)

```

## Question 1.2.2

```{r}
range(clean_data$time)
```

```{r}

clean_data$year = clean_data$time-2000
m <- lm(formula = lifeExpectancy ~ year, data = clean_data)
summary(m)


```

## Question 1.2.3
For time, it contained a wide spectrum of time all the way from 1960 to 2019. Subtracting time by 2000 will allow us to use year 2000 as reference and our betas will be more interpretative.

## Question 1.2.4
In the year of 2000, the worlds' avg life expectancy is 67.33 (Beta0). With 1 unit increase in year, the life expectancy will increase by 0.308.

## Question 1.2.5

```{r}
m2 <- lm(formula = lifeExpectancy ~ year + region, data = clean_data)
summary(m2)



```
## Question 1.2.6

```{r}
55.86 + 15.93
55.86+ 12.22
55.86 + 20.89
55.86 +13.65
```
The time coefficient has a value of 0.30, which shows that as time increase by 1 unit, life expectancy will increase by 0.30. The reference category is region-Africa. In the year of 2000, the life expectancy in Africa is 55.86 years. For region Americas, the life expectancy is 55.86+ 12.22= 71.79 . For region Asia, the life expectancy is 49.75 + 12.23 = 68.08. For Europe, the life expectancy is 55.86 + 20.89 = 76.75. Lastly, for region Oceania, the life expectancy is 55.86 +13.65= 69.51. All the coefficients are statistically significant at 1% as their p-values are smaller than 0.01. This model is different with higher R square and lower beta0 values than previous model because we incorporated a new independent variable - region.

## Question 1.2.7

```{r}
m3 <- lm(formula = lifeExpectancy ~ year + region + log(GDP_PC) + fertilityRate, data = clean_data)
summary(m3)
```

## Question 1.2.8
GDP tells us that with an unit increase in a country's GDP, the life expectancy will increase by 2.49027 , signifying a positive correlation. For fertility, an unit increase in fertility will decrease life expectancy by -2.23512, signifying a negative correlation.

## Question 1.2.9

Adding new variables can change the model because some of these variables may be correlated with one another. This correlation is known as multicollinearity. Additionally, adding new independent variables can change each variable's impact or relationship to the dependent variable with changes to coefficients and significance.

## Question 1.2.10

As shown in model 3 (m3), which has the highest r-square and thus is a model that better predicts the outcome of the dependent variable, region- Americas has the highest life expectancy. Below is the life expectancy based on raw data and not the model, showing Europe has higher life expectancy than U.S.

```{r}
clean_data %>%
  group_by(region) %>%
  summarize(avg_life_expectancy = mean(lifeExpectancy))
```





