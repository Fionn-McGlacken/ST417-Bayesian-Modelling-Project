ST417 Bayesian Modelling Project
================
Fionn McGlacken 19388186, Alex Horkan 19461736, Hugo Mead 19419556
2023-01-06

# Introduction

This project was completed for ST417 Introduction to Bayesian Modelling.
It was developed on github

<https://github.com/Fionn-McGlacken/ST417-Bayesian-Modelling-Project>

# Aim

## Research question:

What are the distributions of time, distance, and cost of travel for
University of Galway students, and how do they vary with respect to
confounding variables such as weather, day, time, traffic, age, city or
town, and mode of transport?

## Motivation:

We are in an accommodation crisis. Third-level students without access
to accommodation near their universities commute longer hours, over
greater distances, at higher costs. We want to understand this
empirically.

## Background Information

The target population of our study is University of Galway students. The
data used in our study was surveyed from University of Galway students.
We collected data on time, distance, cost, weather, day, time, traffic,
age, location and mode of transport.

## List of Aims

In order to explore the effects of the different variables associated
with commuting, we have set a list of aims we wish to explore throughout
the project.

1.  Investigate the relationship between time, distance, and commuting
    costs.
2.  Investigate the effects of confounding variables, such as weather,
    day, time, traffic, age, and mode of transport.
3.  Calculate point and interval estimates of time, distance, and cost
    to travel.
4.  Investigate if the average time, distance, and cost of travel for
    students have increased over previous years.
5.  Investigate the most expensive city or town to travel from, on
    average.
6.  Investigate prevailing weather, the most common mode of transport,
    and traffic patterns.
7.  Estimate the carbon footprint of travel for University of Galway
    students.
8.  Compare differences between year groups and see which year has the
    greatest travel times.

# Data and Prior Description

## Subjective Opinion

In our opinion, the time, distance and commuting costs should be the
easiest to analyse as they are the most straightforward variables to
measure. We believe that the confounding variables will be more
difficult to analyse as they are harder to measure. For example, weather
and traffic data must be sourced online.

## Data Sampling

### Survey

The data we used for the purposes of this analysis was collected via an
online questionnaire which was distributed amongst University of Galway
students.

<https://docs.google.com/forms/d/1aJ-uViRoRLn2aS7wT3EdJs-szgU1ARKo895fsDlbGL4/edit>

We asked students what they think the average time, cost, distance, mode
of transport for students is to inform our prior distributions. Then, we
asked students about the time, cost, distance and mode of transport in
their travel to university to inform the likelihood distributions. We
also asked students about their yeargroup and origin.

We received a total of 74 responses comprised mostly of 3rd and 4th-year
students, with the remaining being made up of 1st and 2nd-year as well
as postgraduate students.

### Additional Data

We collected additional weather data online. Unfortunately, we could not
find reliable traffic data.

## Limitations: Issues in Data Sampling

There were some issues collecting data:

### Biased Data

Firstly, it was easiest to collect data from students we know. This
means that our data is not representative of the entire student
population. For example, 3rd and 4th year students are overrepresented
in our study. Additionally, it may be the case that students who are
most affected by the commuting crisis are not present in this study as
they were not accessible. Therefore, results may be more positive than
in reality.

### Messy Data

Secondly, we did not impose any constraints on how students can enter
the data. This lead to a lot of messy data that needed to be cleaned.
For example, one student wrote ‘half an hour to 40 minutes’ for commute
time. To analyse this, the value must be converted to the mean of the
range.

### Missing Traffic Data

We could not find reliable traffic data online.

``` r
data <- read.csv("data.csv")

# est short for estimated
# naming columns
new_names <- c(
  "timestamp",
  "est_time",
  "est_distance",
  "est_cost",
  "est_transport",
  "year_group",
  "time",
  "start_time",
  "distance",
  "cost",
  "transport",
  "origin")

colnames(data) <- new_names
```

## Cleaning Data

### Functions for Cleaning Data

Defining functions to clean data.

``` r
# removes letters from entries
strip_data <- function(column) {
  matcher <- "[^0-9.-]"
  cond <- grepl(matcher, column)
  stripped_rows <- str_remove_all(column[cond], matcher)
  column[cond] <- stripped_rows
  return(column)
}

# removes letters and scales entries by scalar e.g. '80 cents' to '0.8' (euro)
scale_data <- function(column, to_match, scalar) {
  matcher <- paste(to_match, collapse = "|")
  cond <- grepl(matcher, column)
  stripped_rows <- str_remove_all(column[cond], "[^0-9.-]")
  stripped_rows <- as.numeric(stripped_rows)
  column[cond] <- stripped_rows * scalar
  return(column)
}

# replaces range entries by average of range e.g. '7-10' to '8.5'
avg_range <- function(column) {
  matcher <- "^(\\d+)-(\\d+)$"
  cond <- grepl(matcher, column)
  column[cond] <- rowMeans(read.table(text = column[cond],
     sep = "-", header = FALSE), na.rm = TRUE)
  return(column)
}

# replaces range entries by scaled average of range
# e.g. '2-4hrs' to '180' (minutes)
scale_and_avg_range <- function(column, to_match, scalar) {
  matcher <- paste(to_match, collapse = "|")
  cond <- grepl(matcher, column)
  stripped_rows <- str_remove_all(column[cond], to_match)
  table <- read.table(text = stripped_rows, sep = "-", header = FALSE) * scalar
  column[cond] <- rowMeans(table, na.rm = TRUE)
  return(column)
}
```

### Cleaning Data

``` r
data <- data %>% mutate(timestamp = as.Date(timestamp))
data <- data %>% mutate(day = weekdays(timestamp))

data$est_time <- sub("to", "-", data$est_time)
data$est_time <- sub("half an hour", "30", data$est_time)
hours <- c("hours", "hrs")
data$est_time <- scale_and_avg_range(data$est_time, hours, 60)
data$est_time <- strip_data(data$est_time)
data$est_time <- avg_range(data$est_time)
data <- data %>% mutate(est_time = as.numeric(est_time))

data$est_distance <- sub("or", "-", data$est_distance)
data$est_distance <- strip_data(data$est_distance)
data$est_distance <- avg_range(data$est_distance)
data <- data %>% mutate(est_distance = as.numeric(est_distance))
```

    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion

``` r
data$est_cost <- sub("/", "-", data$est_cost)
cents <- c("cents")
data$est_cost <- scale_data(data$est_cost, cents, 0.01)
data$est_cost <- strip_data(data$est_cost)
data$est_cost <- avg_range(data$est_cost)
data <- data %>% mutate(est_cost = as.numeric(est_cost))

data$time <- strip_data(data$time)
data <- data %>% mutate(time = as.numeric(time))
data$start_time <- as.POSIXct(paste(data$timestamp, data$start_time)
                                    , format = "%Y-%m-%d %H:%M")

# value entered in wrong column
data[52, 9] <- data[52, 10]
data$distance <- sub("Less than a kilometre", "1", data$distance)
data$distance <- strip_data(data$distance)
data <- data %>% mutate(distance = as.numeric(distance))

data[52, 10] <- NA
data[63, 10] <- "0"
zeros <- c("Nothing", "None")
for (i in zeros) {
  data$cost <- sub(i, "0", data$cost)
}
data$cost <- gsub("Less than a kilometre", paste(zeros, collapse = "|")
                  , data$cost)
costs <- c("cent")
data$cost <- scale_data(data$cost, costs, 0.01)
data[3, 10] <- 0.8
data <- data %>% mutate(cost = as.numeric(cost))
```

    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion

``` r
# cleaned data
write.csv(data, "cleaned_data.csv")
```

Next, excel was used to clean location data and add weather data.

``` r
data <- read.csv("origin_weather_data.csv")
head(data)
```

    ##   X  timestamp est_time est_distance est_cost est_transport year_group time
    ## 1 1 21/11/2022       30            3      3.0   Bus / Coach          4   10
    ## 2 2 21/11/2022       15           20      5.0   Bus / Coach          3   15
    ## 3 3 21/11/2022       30           10      0.8           Car          3   25
    ## 4 4 21/11/2022       15           10      3.0          Walk          3    5
    ## 5 5 21/11/2022       15            3      1.5   Bus / Coach          3   20
    ## 6 6 21/11/2022       20           10      6.0   Bus / Coach          2   10
    ##         start_time distance cost   transport               origin    day
    ## 1 21/11/2022 12:45      0.6  0.0        Walk  Eyre square, Galway Monday
    ## 2 21/11/2022 13:40      2.0  0.0        Walk       Galway, Galway Monday
    ## 3 21/11/2022 11:00      5.0  0.8 Bus / Coach       Galway, Galway Monday
    ## 4 21/11/2022 02:50      5.0  0.0         Car       Galway, Galway Monday
    ## 5 21/11/2022 11:45      4.5  1.2 Bus / Coach Knocknacarra, Galway Monday
    ## 6 21/11/2022 11:00     10.0  2.0 Bus / Coach       Galway, Galway Monday
    ##   temp_celcius precipitation_mm perc_humidity wind_kmh  weather
    ## 1            9                7          0.76     36.0    Light
    ## 2            9                7          0.72     36.0    Light
    ## 3            8                7          0.76     36.0 Moderate
    ## 4            9                7          0.72     36.0    Light
    ## 5            8                7          0.76     50.4 Moderate
    ## 6            9                7          0.76     50.4 Moderate

## Data Analysis

Our data can be divided into three sections - Estimated data (to inform
priors) - Actual data (to inform likelihoods) - Participant data (to
measure confounds)

We have four estimates (est) to inform our priors - est_time -
est_distance - est_cost - est_transport

We have four types of actual data to inform our likelihoods - time -
distance - cost - transport

We have 10 types of participant data to measure confounds - timestamp -
year_group - start_time - origin - day - temp_celcius -
precipitation_mm - perc_humidity - wind_kmh - weather (light, moderate,
heavy)

### Data Exploration

To begin our analysis, we will explore the data to get a better
understanding of the data we are working with.

Firstly we will look at the location and cost data to see which
city/town is the most expensive to commute from.

``` r
data$cost[is.na(data$cost)] <- 0
commute_data <- select(data, origin, cost)

# Group data by origin
commute_data_by_origin <- group_by(commute_data, origin)

# Calculate summary statistics for cost by origin
cost_summary <- summarize(commute_data_by_origin,
                          mean_cost = mean(cost))

# Reorder origins based on mean cost
cost_summary$origin <- reorder(cost_summary$origin, cost_summary$mean_cost, decreasing = TRUE)

# Sort data by mean cost
cost_summary <- arrange(cost_summary, desc(mean_cost))

head(cost_summary, 10)
```

    ## # A tibble: 10 × 2
    ##    origin                 mean_cost
    ##    <fct>                      <dbl>
    ##  1 "Tuam, Galway"              12  
    ##  2 "Athlone, Westmeath"        10  
    ##  3 "Ardrahan, Galway"           8  
    ##  4 "Knocknacarra, Galway"       5.3
    ##  5 "Galway , Galway"            5  
    ##  6 "Rosscahill, Galway"         5  
    ##  7 "Knocknacarra Galway"        3.6
    ##  8 "Renmore, Galway"            3  
    ##  9 "Craughwell, Galway "        2  
    ## 10 "Salthill, Galway "          2

``` r
# Create a bar plot showing mean cost by origin
ggplot(head(cost_summary, 10), aes(x = origin, y = mean_cost)) +
  geom_bar(stat = "identity") +
  labs(x = "Origin", y = "Mean Cost")
```

![](ST417-Bayesian-Modelling-Project_files/figure-gfm/location%20vs%20cost-1.png)<!-- -->

“NA” values present in cost were changed to 0 due to the locations being
in Galway, we can assume respondents entered “NA” instead of 0.

Next, we will explore the weather, traffic and mode of transport data to
see which ones are the most common.

``` r
# Select relevant columns
commute_data_2 <- select(data, weather, transport)

# Create contingency table
contingency_table <- addmargins(table(commute_data_2$weather, commute_data_2$transport))

# View contingency table
contingency_table
```

    ##           
    ##            Bike Bus / Coach Car Train Walk Sum
    ##   Light       2           7  11     2   16  38
    ##   Moderate    3          13   3     2   13  34
    ##   Sum         5          20  14     4   29  72

``` r
# Create stacked bar plot showing number of observations by transport and weather
ggplot(commute_data_2, aes(x = transport, fill = weather)) +
  geom_bar() +
  labs(x = "Transport", y = "Weather")
```

![](ST417-Bayesian-Modelling-Project_files/figure-gfm/weather,%20transport%20and%20traffic-1.png)<!-- -->

We can see by the output and plot that the most common weather is light,
and the most common mode of transport is Walking, with the next most
common being Bus/Coach.

Now we will now compare the differences between year groups and see
which has the greatest travel distance, time and cost.

``` r
# Select relevant columns
commute_data_3 <- select(na.omit(data), year_group, distance, time, cost)

# View summary statistics for all variables by year group
group_by(commute_data_3, year_group) %>%
  summarize(mean_distance = mean(distance),
            sd_distance = sd(distance),
            mean_time = mean(time),
            sd_time = sd(time),
            mean_cost = mean(cost),
            sd_cost = sd(cost))
```

    ## # A tibble: 5 × 7
    ##   year_group     mean_distance sd_distance mean_time sd_time mean_cost sd_cost
    ##   <chr>                  <dbl>       <dbl>     <dbl>   <dbl>     <dbl>   <dbl>
    ## 1 2                       3.7        3.70       16      5.48     1.2      1.79
    ## 2 3                       8.09      19.8        23.7   30.0      0.792    2.39
    ## 3 4                      12.1       17.7        28.7   22.7      1.93     4.01
    ## 4 Higher Diploma          1.4        0.141      17.5    3.54     0        0   
    ## 5 Master's                3.6        1.64       27.3    4.62     1.63     1.82

``` r
# Create box plots showing distribution of distance, time, and cost by year group
ggplot(commute_data_3, aes(x = year_group, y = distance)) +
  geom_boxplot() +
  labs(x = "Year Group", y = "Distance")
```

![](ST417-Bayesian-Modelling-Project_files/figure-gfm/year%20group%20vs%20distance,%20time%20and%20cost-1.png)<!-- -->

``` r
ggplot(commute_data_3, aes(x = year_group, y = time)) +
  geom_boxplot() +
  labs(x = "Year Group", y = "Time")
```

![](ST417-Bayesian-Modelling-Project_files/figure-gfm/year%20group%20vs%20distance,%20time%20and%20cost-2.png)<!-- -->

``` r
ggplot(commute_data_3, aes(x = year_group, y = cost)) +
  geom_boxplot() +
  labs(x = "Year Group", y = "Cost")
```

![](ST417-Bayesian-Modelling-Project_files/figure-gfm/year%20group%20vs%20distance,%20time%20and%20cost-3.png)<!-- -->

As we can see from the box plots and summary statistics, the 4th year
group has the greatest travel distance, time and cost. This is might be
due to the fact that the 4th year group is the year group with the most
respondents.

### Time

#### est_time

est_time is the estimated average commute time.

``` r
est_time <- na.omit(data$est_time)
summary(est_time)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##    8.50   20.00   30.00   35.32   40.00  180.00

``` r
par(mfrow = c(1, 2))
plot(density(est_time), main = "Estimated Commute Time", xlab = "Time (minutes)") # nolint
boxplot(est_time, main = "Estimated Commute Time", xlab = "Time (minutes)")
```

![](ST417-Bayesian-Modelling-Project_files/figure-gfm/est_time%20plots-1.png)<!-- -->

The density and box plots of the prior data show that the opinion of our
respondents ranges wildly, with the majority of respondents estimating a
commute time of less than 50 minutes for the average University of
Galway student. This estimated range of possible commute times provides
us with a good staring point, from which we can use our actual data to
more precisely approximate the average commute time.

#### Time prior

We will use the est_time data to inform our prior for the average
commute time.

From est_time, it appears the data are approximately normally
distributed. Therefore, a normal prior will be used, with $\mu =$ mean
of est_time, $\sigma =$ standard deviation of est_time.

``` r
# mu <- mean(est_time)
# sigma <- sd(est_time)
# set.seed(1)
# Prior <- rtruncnorm(71, a=0, b=Inf, mu, sigma)
# par(mfrow=c(1,2))
# plot(density(Prior), main = 'Prior for average time', xlab="Time (minutes)")
# boxplot(Prior, main="Estimated Commute Time", xlab="Time (minutes)")

theta_time <- est_time
Prior <- dnorm(theta_time, mean = mean(theta_time), sd = sd(theta_time)) # nolint
bayes_df_time <- data.frame(theta_time, Prior)
summary(bayes_df_time)
```

    ##    theta_time         Prior          
    ##  Min.   :  8.50   Min.   :8.200e-08  
    ##  1st Qu.: 20.00   1st Qu.:1.066e-02  
    ##  Median : 30.00   Median :1.181e-02  
    ##  Mean   : 35.32   Mean   :1.144e-02  
    ##  3rd Qu.: 40.00   3rd Qu.:1.330e-02  
    ##  Max.   :180.00   Max.   :1.351e-02

``` r
prob_plot(bayes_df_time)
```

![](ST417-Bayesian-Modelling-Project_files/figure-gfm/time%20prior-1.png)<!-- -->

#### Time posterior

We will now update the prior distribution with the likelihood of the
data to get a posterior distribution.

``` r
xbar_time <- mean(data$time, na.rm = TRUE)
n_time <- NROW(data$time)
sigma_time <- sd(data$time, na.rm = TRUE)
se_time <- sigma_time / sqrt(n_time)

# likelihood calculation
likelihood_time <- dnorm(xbar_time, mean = est_time, sd = se_time)
bayes_df_time <- data.frame(est_time, Prior, likelihood_time)

# posterior calculation
bayes_df_time$Product <- bayes_df_time$Prior * bayes_df_time$likelihood_time # nolint
bayes_df_time$Posterior <- bayes_df_time$Product / sum(bayes_df_time$Product)
summary(bayes_df_time)
```

    ##     est_time          Prior           likelihood_time        Product         
    ##  Min.   :  8.50   Min.   :8.200e-08   Min.   :0.0000000   Min.   :0.000e+00  
    ##  1st Qu.: 20.00   1st Qu.:1.066e-02   1st Qu.:0.0000001   1st Qu.:1.200e-09  
    ##  Median : 30.00   Median :1.181e-02   Median :0.0275262   Median :3.251e-04  
    ##  Mean   : 35.32   Mean   :1.144e-02   Mean   :0.0212781   Mean   :2.689e-04  
    ##  3rd Qu.: 40.00   3rd Qu.:1.330e-02   3rd Qu.:0.0311170   3rd Qu.:4.137e-04  
    ##  Max.   :180.00   Max.   :1.351e-02   Max.   :0.1415943   Max.   :1.800e-03  
    ##    Posterior        
    ##  Min.   :0.000e+00  
    ##  1st Qu.:6.000e-08  
    ##  Median :1.703e-02  
    ##  Mean   :1.408e-02  
    ##  3rd Qu.:2.167e-02  
    ##  Max.   :9.429e-02

``` r
# prior and posterior comparison
prior_post_plot(bayes_df_time, "Time")
```

![](ST417-Bayesian-Modelling-Project_files/figure-gfm/time_posterior-1.png)<!-- -->

After updating the prior with the data we collected, we can see that the
posterior distribution is much more concentrated around the mean of the
data, with a mean of around 30 minutes and a standard deviation of
around 5 minutes. The updated posterior for the avereage commute time
greatly reduced the variability of the distribution. This is to be
expected, as the data we collected is much more precise than the
estimates we used to inform our prior.

### Distance

#### Distance Prior

Using the est_distance data, we can now inform our prior for the average
commute distance.

``` r
theta_dist <- na.omit(data$est_distance)
Prior <- dnorm(theta_dist, mean = mean(theta_dist), sd = sd(theta_dist)) # nolint
bayes_df_dist <- data.frame(theta_dist, Prior)
summary(bayes_df_dist)
```

    ##    theta_dist        Prior          
    ##  Min.   : 1.00   Min.   :2.555e-05  
    ##  1st Qu.: 4.00   1st Qu.:1.746e-02  
    ##  Median : 7.50   Median :1.935e-02  
    ##  Mean   :14.76   Mean   :1.779e-02  
    ##  3rd Qu.:20.00   3rd Qu.:2.156e-02  
    ##  Max.   :80.00   Max.   :2.252e-02

``` r
prob_plot(bayes_df_dist)
```

![](ST417-Bayesian-Modelling-Project_files/figure-gfm/distance%20prior-1.png)<!-- -->

#### Distance Posterior

With the prior for commute distance established, we can now update the
prior with the likelihood of the data to get a posterior distribution.

``` r
xbar_dist <- mean(data$distance, na.rm = TRUE)
n_dist <- NROW(data$distance)
sigma_dist <- sd(data$distance, na.rm = TRUE)
se_dist <- sigma_dist / sqrt(n_dist)

# likelihood calculation
likelihood_dist <- dnorm(xbar_dist, mean = theta_dist, sd = se_dist)
bayes_df_dist <- data.frame(theta_dist, Prior, likelihood_dist)

# posterior calculation
bayes_df_dist$Product <- bayes_df_dist$Prior * bayes_df_dist$likelihood_dist # nolint
bayes_df_dist$Posterior <- bayes_df_dist$Product / sum(bayes_df_dist$Product)
summary(bayes_df_dist)
```

    ##    theta_dist        Prior           likelihood_dist        Product         
    ##  Min.   : 1.00   Min.   :2.555e-05   Min.   :0.000e+00   Min.   :0.000e+00  
    ##  1st Qu.: 4.00   1st Qu.:1.746e-02   1st Qu.:1.100e-07   1st Qu.:2.000e-09  
    ##  Median : 7.50   Median :1.935e-02   Median :2.482e-03   Median :5.126e-05  
    ##  Mean   :14.76   Mean   :1.779e-02   Mean   :3.911e-02   Mean   :8.283e-04  
    ##  3rd Qu.:20.00   3rd Qu.:2.156e-02   3rd Qu.:2.513e-02   3rd Qu.:4.862e-04  
    ##  Max.   :80.00   Max.   :2.252e-02   Max.   :1.788e-01   Max.   :3.885e-03  
    ##    Posterior        
    ##  Min.   :0.000e+00  
    ##  1st Qu.:4.000e-08  
    ##  Median :8.840e-04  
    ##  Mean   :1.429e-02  
    ##  3rd Qu.:8.385e-03  
    ##  Max.   :6.700e-02

``` r
# prior and posterior comparison
prior_post_plot(bayes_df_dist, "Distance")
```

![](ST417-Bayesian-Modelling-Project_files/figure-gfm/distance%20posterior-1.png)<!-- -->

We can see from the plots that the posterior distribution is much more
concentrated around the mean of the data, with a mean of around 10km and
a standard deviation of around 3km. Again we can see that the variabiliy
of the distribution has be reduced greatly by updating the prior with
the data we collected.

### Cost

#### Cost Prior

In order to inform our prior for the average commute cost, we will use
the est_cost data.

``` r
theta_cost <- na.omit(data$est_cost)
Prior <- dnorm(theta_cost, mean = mean(theta_cost), sd = sd(theta_cost)) # nolint
bayes_df_cost <- data.frame(theta_cost, Prior)
summary(bayes_df_cost)
```

    ##    theta_cost         Prior          
    ##  Min.   : 0.000   Min.   :1.230e-06  
    ##  1st Qu.: 3.000   1st Qu.:5.814e-02  
    ##  Median : 5.000   Median :7.719e-02  
    ##  Mean   : 5.842   Mean   :7.545e-02  
    ##  3rd Qu.: 7.000   3rd Qu.:9.684e-02  
    ##  Max.   :25.000   Max.   :9.890e-02

``` r
prob_plot(bayes_df_cost)
```

![](ST417-Bayesian-Modelling-Project_files/figure-gfm/cost%20prior-1.png)<!-- -->

#### Cost Posterior

Updating the prior with the likelihood of the data to get a posterior
distribution, should give us a better idea of the average commute cost.

``` r
xbar_cost <- mean(data$cost, na.rm = TRUE)
n_cost <- NROW(data$cost)
sigma_cost <- sd(data$cost, na.rm = TRUE)
se_cost <- sigma_cost / sqrt(n_cost)

# likelihood calculation
likelihood_cost <- dnorm(xbar_cost, mean = theta_cost, sd = se_cost)
bayes_df_cost <- data.frame(theta_cost, Prior, likelihood_cost)

# posterior calculation
bayes_df_cost$Product <- bayes_df_cost$Prior * bayes_df_cost$likelihood_cost # nolint
bayes_df_cost$Posterior <- bayes_df_cost$Product / sum(bayes_df_cost$Product)
summary(bayes_df_cost)
```

    ##    theta_cost         Prior           likelihood_cost        Product         
    ##  Min.   : 0.000   Min.   :1.230e-06   Min.   :0.0000000   Min.   :0.000e+00  
    ##  1st Qu.: 3.000   1st Qu.:5.814e-02   1st Qu.:0.0000000   1st Qu.:0.000e+00  
    ##  Median : 5.000   Median :7.719e-02   Median :0.0000000   Median :0.000e+00  
    ##  Mean   : 5.842   Mean   :7.545e-02   Mean   :0.0730226   Mean   :4.228e-03  
    ##  3rd Qu.: 7.000   3rd Qu.:9.684e-02   3rd Qu.:0.0005512   3rd Qu.:4.255e-05  
    ##  Max.   :25.000   Max.   :9.890e-02   Max.   :1.0141281   Max.   :5.618e-02  
    ##    Posterior        
    ##  Min.   :0.0000000  
    ##  1st Qu.:0.0000000  
    ##  Median :0.0000000  
    ##  Mean   :0.0140845  
    ##  3rd Qu.:0.0001418  
    ##  Max.   :0.1871731

``` r
# prior and posterior comparison
prior_post_plot(bayes_df_cost, "Cost")
```

![](ST417-Bayesian-Modelling-Project_files/figure-gfm/cost%20posterior-1.png)<!-- -->

Just like we have seen in the previous plots, the posterior distribution
has a much lower variability than the prior distribution. We can see
that the distribution has moved to become more concentrated around the
mean of the data, with a mean commute cost of around €10 and a standard
deviation of roughly €3.

With the posterior distributions for the average commute time, distance
and cost established, we can now use these distributions to make
predictions about the commute time, distance and cost.

### Point Estimates and 95% Credible Intervals

Using our posterior distributions, we will now to generate point
estimates and 95% credible intervals for the average commute time,
distance and cost. This will enable us to predict range of likely
commute times, distances and costs, as well as the most likely commute
time, distance and cost, for the average University College Galway
student.

``` r
m0_time <- mean(data$est_time, na.rm = TRUE)
m0_dist <- mean(data$est_distance, na.rm = TRUE)
m0_cost <- mean(data$est_cost, na.rm = TRUE)
s0_time <- sd(data$est_time, na.rm = TRUE)
s0_dist <- sd(data$est_distance, na.rm = TRUE)
s0_cost <- sd(data$est_cost, na.rm = TRUE)
```

``` r
post_mean_time <- ((1 / (se_time^2) + 1 / (s0_time^2))^(-1)) * (xbar_time / (se_time^2) + m0_time / (s0_time^2)) # nolint
post_sd_time <- sqrt((1 / (se_time^2) + 1 / (s0_time^2))^(-1))

post_mean_dist <- (1 / (se_dist^2) + 1 / (s0_dist^2))^(-1) * (xbar_dist / (se_dist^2) + m0_dist / (s0_dist^2)) # nolint
post_sd_dist <- sqrt((1 / (se_dist^2) + 1 / (s0_dist^2))^(-1))

post_mean_cost <- (1 / (se_cost^2) + 1 / (s0_cost^2))^(-1) * (xbar_cost / (se_cost^2) + m0_cost / (s0_cost^2)) # nolint
post_sd_cost <- sqrt((1 / (se_cost^2) + 1 / (s0_cost^2))^(-1))
```

``` r
# 95% credible intervals
set.seed(1)
time_sims <- rnorm(10000, mean = post_mean_time, sd = post_sd_time)
time_point_estimate <- mean(time_sims)
time_point_estimate
```

    ## [1] 25.17104

``` r
time_ci95 <- quantile(time_sims, probs = c(0.025, 0.975))
time_ci95
```

    ##     2.5%    97.5% 
    ## 19.51797 30.76657

``` r
ggplot(data.frame(time_sims), aes(x = time_sims)) +
  geom_density(fill = "darkgray", alpha = 0.2) +
  geom_vline(xintercept = time_ci95, col = "red", lwd = 1) +
  ggtitle("Time") +
  xlab("Time (min)") +
  ylab("Density")
```

![](ST417-Bayesian-Modelling-Project_files/figure-gfm/95%20credible%20intervals-1.png)<!-- -->

``` r
dist_sims <- rnorm(10000, mean = post_mean_dist, sd = post_sd_dist)
dist_point_estimate <- mean(dist_sims)
dist_point_estimate
```

    ## [1] 9.175626

``` r
dist_ci95 <- quantile(dist_sims, probs = c(0.025, 0.975))
dist_ci95
```

    ##      2.5%     97.5% 
    ##  5.371109 13.171538

``` r
ggplot(data.frame(dist_sims), aes(x = dist_sims)) +
  geom_density(fill = "darkgray", alpha = 0.2) +
  geom_vline(xintercept = dist_ci95, col = "red", lwd = 1) +
  ggtitle("Distance") +
  xlab("Distance (km)") +
  ylab("Density")
```

![](ST417-Bayesian-Modelling-Project_files/figure-gfm/95%20credible%20intervals-2.png)<!-- -->

``` r
cost_sims <- rnorm(10000, mean = post_mean_cost, sd = post_sd_cost)
cost_point_estimate <- mean(cost_sims)
cost_point_estimate
```

    ## [1] 1.521084

``` r
cost_ci95 <- quantile(cost_sims, probs = c(0.025, 0.975))
cost_ci95
```

    ##     2.5%    97.5% 
    ## 0.748881 2.297163

``` r
ggplot(data.frame(cost_sims), aes(x = cost_sims)) +
  geom_density(fill = "darkgray", alpha = 0.2) +
  geom_vline(xintercept = cost_ci95, col = "red", lwd = 1) +
  ggtitle("Cost") +
  xlab("Cost (EUR)") +
  ylab("Density")
```

![](ST417-Bayesian-Modelling-Project_files/figure-gfm/95%20credible%20intervals-3.png)<!-- -->

The 95% credible intervals and Point Estimates suggest that:

- The average time of a commute should be between 19.5 and 30.76
  minutes, with a commute time of 25.17 minutes on average.
- The average distance of a commute should be between 5.37 and 13.17
  kilometres, with a commute length of 9.17 kilometres on average.
- The average cost of a commute should be between €0.89 and €2.52, with
  a cost of €1.71 for an average student’s commute to the University of
  Galway.

## Modelling with MCMC

### Posterior

Next, we decided to use the rjags package in order to define a Markov
Chain Monte Carlo (MCMC) model to generate a new set of samples for new
posterior distributions for the average commute time, distance and cost.
This will enable us to see how representitive the data we collected when
compared to a much larger sample size. We will do this by comparing the
results of the rjags model to the results of the initial Bayesian
analysis.

``` r
# define model
model <- "
model {
  for (i in 1:N) {
    time[i] ~ dnorm(mu_time, sigma_time)
    distance[i] ~ dnorm(mu_dist, sigma_dist)
    cost[i] ~ dnorm(mu_cost, sigma_cost)
  }
  mu_time ~ dnorm(m0_time, sigma0_time)
  mu_dist ~ dnorm(m0_dist, sigma0_dist)
  mu_cost ~ dnorm(m0_cost, sigma0_cost)
  sigma_time ~ dgamma(a0_time, b0_time)
  sigma_dist ~ dgamma(a0_dist, b0_dist)
  sigma_cost ~ dgamma(a0_cost, b0_cost)
}
"
```

``` r
# data
model_data <- list(
  N = nrow(data),
  time = data$time,
  distance = data$distance,
  cost = data$cost,
  m0_time = m0_time,
  m0_dist = m0_dist,
  m0_cost = m0_cost,
  sigma0_time = 1 / (s0_time^2),
  sigma0_dist = 1 / (s0_dist^2),
  sigma0_cost = 1 / (s0_cost^2),
  a0_time = 1 / (se_time^2),
  a0_dist = 1 / (se_dist^2),
  a0_cost = 1 / (se_cost^2),
  b0_time = 1 / (se_time^2) * xbar_time,
  b0_dist = 1 / (se_dist^2) * xbar_dist,
  b0_cost = 1 / (se_cost^2) * xbar_cost
)
```

After defining the values of all variables in the model definition, we
will now compile the model and the data, including the starting values
as well as setting the number of chains to 3.

``` r
# compile model
jags_model_mc <- jags.model(
  textConnection(model),
  data = model_data,
  n.chains = 3
)
```

    ## Compiling model graph
    ##    Resolving undeclared variables
    ##    Allocating nodes
    ## Graph information:
    ##    Observed stochastic nodes: 215
    ##    Unobserved stochastic nodes: 7
    ##    Total graph size: 235
    ## 
    ## Initializing model

``` r
# burn in model
update(jags_model_mc, 1000)
```

After a burn-in period of 1,000 iterations, we simulated 10,000 new
values from the posterior data. From these simulations, we can plot
trace and density plots of each feature.

``` r
# simulate posterior
sims_mc <- coda.samples(
  jags_model_mc,
  variable.names = c("mu_time", "mu_dist", "mu_cost",
                     "sigma_time", "sigma_dist", "sigma_cost"),
  n.iter = 10000
)
```

``` r
# plot sims
plot(sims_mc)
```

![](ST417-Bayesian-Modelling-Project_files/figure-gfm/sims_mc%20plot-1.png)<!-- -->![](ST417-Bayesian-Modelling-Project_files/figure-gfm/sims_mc%20plot-2.png)<!-- -->

The trace plots all show good stability and mixing throughout the
iterations, and do not show any clear patterns throughout, this suggests
that the model has reached convergence. The density plots appear to be
smooth distributions, suggesting that the simulations were
representative of all possible values in the posterior distributions.

``` r
# summary
summary(sims_mc)
```

    ## 
    ## Iterations = 1001:11000
    ## Thinning interval = 1 
    ## Number of chains = 3 
    ## Sample size per chain = 10000 
    ## 
    ## 1. Empirical mean and standard deviation for each variable,
    ##    plus standard error of the mean:
    ## 
    ##                 Mean        SD  Naive SE Time-series SE
    ## mu_cost     1.513017 0.3684352 2.127e-03      2.144e-03
    ## mu_dist     9.194199 2.0475011 1.182e-02      1.175e-02
    ## mu_time    25.188780 2.8327872 1.636e-02      1.636e-02
    ## sigma_cost  0.104033 0.0159760 9.224e-05      9.435e-05
    ## sigma_dist  0.003404 0.0005766 3.329e-06      3.373e-06
    ## sigma_time  0.001758 0.0002935 1.695e-06      1.722e-06
    ## 
    ## 2. Quantiles for each variable:
    ## 
    ##                 2.5%       25%       50%       75%     97.5%
    ## mu_cost     0.786074  1.267233  1.515841  1.757361  2.238687
    ## mu_dist     5.126950  7.827758  9.194745 10.565870 13.257725
    ## mu_time    19.642979 23.308967 25.196777 27.072928 30.787986
    ## sigma_cost  0.074975  0.092828  0.103267  0.114282  0.137488
    ## sigma_dist  0.002370  0.002999  0.003371  0.003783  0.004611
    ## sigma_time  0.001229  0.001550  0.001743  0.001947  0.002378

``` r
# gelman-rubin statistic
gelman.diag(sims_mc)
```

    ## Potential scale reduction factors:
    ## 
    ##            Point est. Upper C.I.
    ## mu_cost             1          1
    ## mu_dist             1          1
    ## mu_time             1          1
    ## sigma_cost          1          1
    ## sigma_dist          1          1
    ## sigma_time          1          1
    ## 
    ## Multivariate psrf
    ## 
    ## 1

``` r
gelman.plot(sims_mc)
```

![](ST417-Bayesian-Modelling-Project_files/figure-gfm/sims_mc%20gelman-rubin-1.png)<!-- -->

A Gelman-Rubin statistic, where the estimated variance of the parameter
is a weighted sum of the between and within chain variances, of R≅1
suggests that the chains have converged. The parameters all have values
of R=1 and the plots show convergence around 1 so this suggests that the
3 Markov chains in the model have converged. This can also be see in the
Gelman-Rubin plot, where the lines are all close to 1.

``` r
# autocorrelation
autocorr.plot(sims_mc[[1]])
```

![](ST417-Bayesian-Modelling-Project_files/figure-gfm/sims_mc%20autocorr-1.png)<!-- -->

The autocorrelation plots show very minimal autocorrelation across all
features in the model.

With MCMC model appearing to have converged, we can generate 95%
credible intervals for each feature in the model. This can be compared
to the 95% credible intervals generated from the initial Bayesian
analysis.

``` r
chains <- data.frame(sims_mc[[1]])

# 95% credible intervals
CI_time <- quantile(chains$mu_time, probs = c(0.025, 0.975)) # nolint
CI_dist <- quantile(chains$mu_dist, probs = c(0.025, 0.975)) # nolint
CI_cost <- quantile(chains$mu_cost, probs = c(0.025, 0.975)) # nolint
CI_time
```

    ##     2.5%    97.5% 
    ## 19.70046 30.67995

``` r
CI_dist
```

    ##      2.5%     97.5% 
    ##  5.227262 13.267000

``` r
CI_cost
```

    ##      2.5%     97.5% 
    ## 0.7610047 2.2379638

``` r
# 95% CI plots
# time
ggplot(chains, aes(x = mu_time)) +
  geom_density(fill = "darkgray", alpha = 0.2) +
  geom_vline(xintercept = CI_time, col = "red", lwd = 1) +
  ggtitle("Time") +
  xlab("Time (min)") +
  ylab("Density")
```

![](ST417-Bayesian-Modelling-Project_files/figure-gfm/95%20CI%20plot-1.png)<!-- -->

``` r
# distance
ggplot(chains, aes(x = mu_dist)) +
  geom_density(fill = "darkgray", alpha = 0.2) +
  geom_vline(xintercept = CI_dist, col = "red", lwd = 1) +
  ggtitle("Distance") +
  xlab("Distance (km)") +
  ylab("Density")
```

![](ST417-Bayesian-Modelling-Project_files/figure-gfm/95%20CI%20plot-2.png)<!-- -->

``` r
# cost
ggplot(chains, aes(x = mu_cost)) +
  geom_density(fill = "darkgray", alpha = 0.2) +
  geom_vline(xintercept = CI_cost, col = "red", lwd = 1) +
  ggtitle("Cost") +
  xlab("Cost (EUR)") +
  ylab("Density")
```

![](ST417-Bayesian-Modelling-Project_files/figure-gfm/95%20CI%20plot-3.png)<!-- -->

The 95% credible interval plots show that the average student takes
between 19.5 and 30.8 minutes to travel between 5.2 and 13.2 kilometres
at a cost of between €0.89 and €2.51. This is very similar to the
predictions made from the original posterior distribution.

### Modelling

``` r
data <- read.csv("origin_weather_data.csv")
data[is.na(data)] <- 0
```

#### Predicting Cost of Commute

##### One hot encoded model

Firstly, we present a failed model featuring one hot encoded categorical
variables. We could not get this model to work but include it as a point
of interest.

``` r
ohe_transport <- model.matrix(~ transport + 0, data = data)
ohe_day <- model.matrix(~ day + 0, data = data)
ohe_weather <- model.matrix(~ weather + 0, data = data)
# ohe_origin <- model.matrix(~ origin + 0, data = data)
model_data <- select(data, -c("transport", "day", "weather"))
model_data <- cbind(model_data, ohe_transport, ohe_day, ohe_weather)
model_data[is.na(model_data)] <- 0
```

``` r
cost_model <- "model{
  for (i in 1:length(Y)){
    Y[i] ~ dnorm(m[i], u^(-2))
    m[i] <- a + b*X1[i] + c*X2[i] + d*X3[i] + e*X4[i] + f*X5[i] + g*X6[i] + h*X7[i] + j*X8[i] + k*X9[i] + l*X10[i] + m*X11[i] + n*X12[i] + o*X13[i] + p*X14[i] + q*X15[i] + r*X16[i] + s*X17[i] + t*X18[i] + u
  }

  m0_dist <- 14.76
  s0_dist <- 17.71457
  m0_time <- 35.3169
  s0_time <- 29.52084

  a ~ dunif(-100, 100) #intercept
  b ~ dnorm(m0_time, s0_time^(-2)) # time (min)
  c ~ dnorm(m0_dist, s0_dist^(-2)) # distance (km)
  d ~ dnorm(7, 2^(-2)) # temperature (degrees celcius)
  e ~ dnorm(6, 1^(-2)) # precipitation (mm)
  f ~ dnorm(0.75, 0.1^(-2)) # percent humidity
  g ~ dnorm(0.5, 0.1^(-2)) # wind speed (km/h)

  h ~ dunif(-10, 10) # bike
  j ~ dunif(-10, 10) # bus / coach
  k ~ dunif(-10, 10) # car
  l ~ dunif(-10, 10) # train
  m ~ dunif(-10, 10) # walk

  n ~ dunif(-10, 10) # friday
  o ~ dunif(-10, 10) # monday
  p ~ dunif(-10, 10) # thursday
  q ~ dunif(-10, 10) # tuesday
  r ~ dunif(-10, 10) # wednesday

  s ~ dunif(-10, 10) # light weather
  t ~ dunif(-10, 10) # moderate weather

  u ~ dunif(0, 100) # error
  }"
```

``` r
var_data <- list(
  Y = model_data$cost,
  X1 = model_data$time,
  X2 = model_data$distance,
  X3 = model_data$temp_celcius,
  X4 = model_data$precipitation_mm,
  X5 = model_data$perc_humidity,
  X6 = model_data$wind_kmh,
  X7 = model_data$transportBike,
  X8 = model_data$`transportBus / Coach`,
  X9 = model_data$transportCar,
  X10 = model_data$transportTrain,
  X11 = model_data$transportWalk,
  X12 = model_data$dayFriday,
  X13 = model_data$dayMonday,
  X14 = model_data$dayThursday,
  X15 = model_data$dayTuesday,
  X16 = model_data$dayWednesday,
  X17 = model_data$weatherLight,
  X18 = model_data$weatherModerate
)
```

``` r
print('This code did not work')
```

    ## [1] "This code did not work"

``` r
# cost_model_jags <- jags.model(textConnection(cost_model),
#   data = var_data,
#   inits = list(
#     .RNG.name = "Wichmann-Hill",
#     .RNG.seed = 12345))

# sim_cost <- update(cost_model_jags, 10000)

# n <- 10000
# sims_cost <- coda.samples(cost_model_jags,
#   variable.names = c("a", "b", "c", "d", "e", "f", "g", "h", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u"),
#   n.iter = n)
```

#### Regular model

``` r
cost_model <- "model{
  for (i in 1:length(Y)){
    Y[i] ~ dnorm(m[i], s^(-2))
    m[i] <- a + b*X1[i] + c*X2[i] + d*X3[i] + e*X4[i] + f*X5[i] + g*X6[i] + s
  }

  m0_dist <- 14.76
  s0_dist <- 17.71457
  m0_time <- 35.3169
  s0_time <- 29.52084

  a ~ dunif(0, 100) #intercept
  b ~ dnorm(m0_time, s0_time^(-2)) # time (min)
  c ~ dnorm(m0_dist, s0_dist^(-2)) # distance (km)
  d ~ dnorm(7, 2^(-2)) # temperature (degrees celcius)
  e ~ dnorm(6, 1^(-2)) # precipitation (mm)
  f ~ dnorm(0.75, 0.1^(-2)) # percent humidity
  g ~ dnorm(0.5, 0.1^(-2)) # wind speed (km/h)
  s ~ dunif(0, 100) # error
  }"

cost_model_jags <- jags.model(textConnection(cost_model),
  data = list(Y = data$cost,
    X1 = data$time,
    X2 = data$distance,
    X3 = data$temp_celcius,
    X4 = data$precipitation_mm,
    X5 = data$perc_humidity,
    X6 = data$wind_kmh),
  n.chains = 3)
```

    ## Compiling model graph
    ##    Resolving undeclared variables
    ##    Allocating nodes
    ## Graph information:
    ##    Observed stochastic nodes: 72
    ##    Unobserved stochastic nodes: 8
    ##    Total graph size: 694
    ## 
    ## Initializing model

``` r
sim_cost <- update(cost_model_jags, 10000)

n <- 10000
sims_cost <- coda.samples(cost_model_jags,
  variable.names = c("a", "b", "c", "d", "e", "f", "g", "s"),
  n.iter = n)
```

##### Diagnostics

The gelman diagnostic shows that the chains have converged nicely.
However, the autocorrelation is high. This is likely due to high
multicollinearity in the data.

Unfortunately, we cannot conclude from these results that there is a
relationship between the cost of commute and time or distance, as the
95% credible intervals for the coefficients overlap 0. This may be due
to confounding variables such as mode of transportation, which would
have been accounted for in the one hot encoded model.

However, we can say that the confounding variables of temperature and
precipitation have a negative effect on the cost of commute. While this
makes sense for rain, it makes less sense for temperature and may be due
to random noise in the data.

``` r
summary(sims_cost)
```

    ## 
    ## Iterations = 11001:21000
    ## Thinning interval = 1 
    ## Number of chains = 3 
    ## Sample size per chain = 10000 
    ## 
    ## 1. Empirical mean and standard deviation for each variable,
    ##    plus standard error of the mean:
    ## 
    ##       Mean      SD  Naive SE Time-series SE
    ## a  0.93687 0.88630 0.0051170      0.0278154
    ## b  0.01669 0.02751 0.0001588      0.0005319
    ## c  0.06417 0.03971 0.0002293      0.0007587
    ## d -0.72643 0.28844 0.0016653      0.0143571
    ## e -0.13844 0.15722 0.0009077      0.0054219
    ## f  0.74049 0.10001 0.0005774      0.0006033
    ## g  0.08256 0.04451 0.0002570      0.0020020
    ## s  3.03947 0.27966 0.0016146      0.0047889
    ## 
    ## 2. Quantiles for each variable:
    ## 
    ##         2.5%       25%      50%      75%    97.5%
    ## a  2.599e-02  0.283645  0.66721  1.32054  3.34196
    ## b -3.810e-02 -0.001523  0.01700  0.03516  0.07009
    ## c -1.293e-02  0.037453  0.06384  0.09033  0.14300
    ## d -1.310e+00 -0.920960 -0.71936 -0.52294 -0.19114
    ## e -4.551e-01 -0.242460 -0.13689 -0.03096  0.16464
    ## f  5.456e-01  0.672827  0.73982  0.80792  0.93686
    ## g  5.652e-05  0.050929  0.08137  0.11272  0.17197
    ## s  2.558e+00  2.842774  3.01767  3.21230  3.65464

``` r
plot(sims_cost)
```

![](ST417-Bayesian-Modelling-Project_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->![](ST417-Bayesian-Modelling-Project_files/figure-gfm/unnamed-chunk-13-2.png)<!-- -->

``` r
gelman.diag(sims_cost)
```

    ## Potential scale reduction factors:
    ## 
    ##   Point est. Upper C.I.
    ## a          1       1.01
    ## b          1       1.00
    ## c          1       1.00
    ## d          1       1.00
    ## e          1       1.00
    ## f          1       1.00
    ## g          1       1.00
    ## s          1       1.00
    ## 
    ## Multivariate psrf
    ## 
    ## 1

``` r
gelman.plot(sims_cost)
```

![](ST417-Bayesian-Modelling-Project_files/figure-gfm/unnamed-chunk-13-3.png)<!-- -->

``` r
autocorr.plot(sims_cost[[1]])
```

![](ST417-Bayesian-Modelling-Project_files/figure-gfm/unnamed-chunk-13-4.png)<!-- -->

Additional models can be seen below, where the independent variable is
swapped with one of the dependent variables: time or distance. These
models also show great convergence and high autocorrelation.

#### Predicting Time of Commute

``` r
time_model <- "model{
  for (i in 1:length(Y)){
    Y[i] ~ dnorm(m[i], s^(-2))
    m[i] <- a + b*X1[i] + c*X2[i] + d*X3[i] + e*X4[i] + f*X5[i] + g*X6[i] + s
  }

  m0_dist <- 14.76
  s0_dist <- 17.71457
  m0_cost <- 5.842254
  s0_cost <- 4.030727

  a ~ dunif(0, 100) #intercept
  b ~ dnorm(m0_cost, s0_cost^(-2)) # cost (EUR)
  c ~ dnorm(m0_dist, s0_dist^(-2)) # distance (km)
  d ~ dnorm(7, 2^(-2)) # temperature (degrees celcius)
  e ~ dnorm(6, 1^(-2)) # precipitation (mm)
  f ~ dnorm(0.75, 0.1^(-2)) # percent humidity
  g ~ dnorm(0.5, 0.1^(-2)) # wind speed (km/h)
  s ~ dunif(0, 100) # error
  }"

time_model_jags <- jags.model(textConnection(time_model),
  data = list(Y = data$cost,
    X1 = data$est_cost,
    X2 = data$est_distance,
    X3 = data$temp_celcius,
    X4 = data$precipitation_mm,
    X5 = data$perc_humidity,
    X6 = data$wind_kmh),
  n.chains = 3)
```

    ## Compiling model graph
    ##    Resolving undeclared variables
    ##    Allocating nodes
    ## Graph information:
    ##    Observed stochastic nodes: 72
    ##    Unobserved stochastic nodes: 8
    ##    Total graph size: 675
    ## 
    ## Initializing model

``` r
sim_time <- update(time_model_jags, 10000)

n <- 10000
sims_time <- coda.samples(time_model_jags,
  variable.names = c("a", "b", "c", "d", "e", "f", "g", "s"),
  n.iter = n)
```

``` r
summary(sims_time)
```

    ## 
    ## Iterations = 11001:21000
    ## Thinning interval = 1 
    ## Number of chains = 3 
    ## Sample size per chain = 10000 
    ## 
    ## 1. Empirical mean and standard deviation for each variable,
    ##    plus standard error of the mean:
    ## 
    ##       Mean      SD  Naive SE Time-series SE
    ## a  1.10599 0.98956 0.0057132      0.0339510
    ## b  0.27433 0.11261 0.0006501      0.0018943
    ## c  0.04325 0.02720 0.0001570      0.0003898
    ## d -0.74231 0.27841 0.0016074      0.0143013
    ## e -0.23092 0.15289 0.0008827      0.0054128
    ## f  0.74109 0.09988 0.0005767      0.0006033
    ## g  0.06908 0.04273 0.0002467      0.0020284
    ## s  2.83803 0.25829 0.0014912      0.0043578
    ## 
    ## 2. Quantiles for each variable:
    ## 
    ##        2.5%      25%      50%      75%    97.5%
    ## a  0.032757  0.35998  0.84034  1.56933  3.72299
    ## b  0.053155  0.19863  0.27532  0.35016  0.49365
    ## c -0.009705  0.02503  0.04316  0.06136  0.09658
    ## d -1.309007 -0.91983 -0.73214 -0.55557 -0.21799
    ## e -0.536116 -0.32887 -0.22748 -0.13085  0.06193
    ## f  0.544407  0.67445  0.74096  0.80797  0.93646
    ## g -0.011590  0.04025  0.06793  0.09688  0.15669
    ## s  2.390919  2.65630  2.81784  2.99840  3.40143

``` r
plot(sims_time)
```

![](ST417-Bayesian-Modelling-Project_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->![](ST417-Bayesian-Modelling-Project_files/figure-gfm/unnamed-chunk-15-2.png)<!-- -->

``` r
gelman.diag(sims_time)
```

    ## Potential scale reduction factors:
    ## 
    ##   Point est. Upper C.I.
    ## a          1       1.00
    ## b          1       1.00
    ## c          1       1.00
    ## d          1       1.01
    ## e          1       1.01
    ## f          1       1.00
    ## g          1       1.01
    ## s          1       1.00
    ## 
    ## Multivariate psrf
    ## 
    ## 1

``` r
gelman.plot(sims_time)
```

![](ST417-Bayesian-Modelling-Project_files/figure-gfm/unnamed-chunk-15-3.png)<!-- -->

``` r
autocorr.plot(sims_time[[1]])
```

![](ST417-Bayesian-Modelling-Project_files/figure-gfm/unnamed-chunk-15-4.png)<!-- -->

#### Predicting Distance of Commute

``` r
dist_model <- "model{
  for (i in 1:length(Y)){
    Y[i] ~ dnorm(m[i], s^(-2))
    m[i] <- a + b*X1[i] + c*X2[i] + d*X3[i] + e*X4[i] + f*X5[i] + g*X6[i] + s
  }

  m0_time <- 35.3169
  s0_time <- 29.52084
  m0_cost <- 5.842254
  s0_cost <- 4.030727

  a ~ dunif(0, 100) #intercept
  b ~ dnorm(m0_cost, s0_cost^(-2)) # cost (EUR)
  c ~ dnorm(m0_time, s0_time^(-2)) # time (min)
  d ~ dnorm(7, 2^(-2)) # temperature (degrees celcius)
  e ~ dnorm(6, 1^(-2)) # precipitation (mm)
  f ~ dnorm(0.75, 0.1^(-2)) # percent humidity
  g ~ dnorm(0.5, 0.1^(-2)) # wind speed (km/h)
  s ~ dunif(0, 100) # error
  }"

dist_model_jags <- jags.model(textConnection(dist_model),
  data = list(Y = data$cost,
    X1 = data$est_cost,
    X2 = data$est_time,
    X3 = data$temp_celcius,
    X4 = data$precipitation_mm,
    X5 = data$perc_humidity,
    X6 = data$wind_kmh),
  n.chains = 3)
```

    ## Compiling model graph
    ##    Resolving undeclared variables
    ##    Allocating nodes
    ## Graph information:
    ##    Observed stochastic nodes: 72
    ##    Unobserved stochastic nodes: 8
    ##    Total graph size: 667
    ## 
    ## Initializing model

``` r
sim_dist <- update(dist_model_jags, 10000)

n <- 10000

sims_dist <- coda.samples(dist_model_jags,
  variable.names = c("a", "b", "c", "d", "e", "f", "g", "s"),
  n.iter = n)
```

``` r
summary(sims_dist)
```

    ## 
    ## Iterations = 11001:21000
    ## Thinning interval = 1 
    ## Number of chains = 3 
    ## Sample size per chain = 10000 
    ## 
    ## 1. Empirical mean and standard deviation for each variable,
    ##    plus standard error of the mean:
    ## 
    ##       Mean      SD  Naive SE Time-series SE
    ## a  1.14526 1.04486 6.032e-03      0.0363263
    ## b  0.36803 0.08896 5.136e-04      0.0011126
    ## c  0.01122 0.01306 7.542e-05      0.0001581
    ## d -0.78057 0.27989 1.616e-03      0.0148198
    ## e -0.25973 0.15574 8.992e-04      0.0056409
    ## f  0.74014 0.10038 5.796e-04      0.0005955
    ## g  0.07285 0.04307 2.487e-04      0.0019905
    ## s  2.87500 0.26277 1.517e-03      0.0051176
    ## 
    ## 2. Quantiles for each variable:
    ## 
    ##        2.5%       25%      50%      75%    97.5%
    ## a  0.031364  0.356766  0.84322  1.63130  3.86920
    ## b  0.190545  0.309151  0.36931  0.42791  0.54388
    ## c -0.014800  0.002619  0.01118  0.02001  0.03683
    ## d -1.369572 -0.960284 -0.77046 -0.58851 -0.26961
    ## e -0.574685 -0.360817 -0.25737 -0.15463  0.03954
    ## f  0.543637  0.672419  0.73991  0.80727  0.93617
    ## g -0.005794  0.043257  0.07163  0.09985  0.16420
    ## s  2.418264  2.691124  2.85336  3.03612  3.45067

``` r
plot(sims_dist)
```

![](ST417-Bayesian-Modelling-Project_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->![](ST417-Bayesian-Modelling-Project_files/figure-gfm/unnamed-chunk-17-2.png)<!-- -->

``` r
gelman.diag(sims_dist)
```

    ## Potential scale reduction factors:
    ## 
    ##   Point est. Upper C.I.
    ## a       1.00       1.00
    ## b       1.00       1.00
    ## c       1.00       1.00
    ## d       1.01       1.02
    ## e       1.00       1.01
    ## f       1.00       1.00
    ## g       1.01       1.02
    ## s       1.00       1.00
    ## 
    ## Multivariate psrf
    ## 
    ## 1.01

``` r
gelman.plot(sims_dist)
```

![](ST417-Bayesian-Modelling-Project_files/figure-gfm/unnamed-chunk-17-3.png)<!-- -->

``` r
autocorr.plot(sims_dist[[1]])
```

![](ST417-Bayesian-Modelling-Project_files/figure-gfm/unnamed-chunk-17-4.png)<!-- -->

# Conclusion
