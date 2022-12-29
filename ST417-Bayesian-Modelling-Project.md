ST417 Bayesian Modelling Project
================
Fionn McGlacken 19388186
2022-12-18

``` r
data <- read.csv("data.csv")

# est short for estimated
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

### Applying Functions to Data

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

``` r
theta_time <- na.omit(data$est_time)
prior_time <- dnorm(theta_time, mean = mean(theta_time), sd = sd(theta_time))
bayes_df_time <- data.frame(theta_time, prior_time)
bayes_df_time
```

    ##    theta_time   prior_time
    ## 1        30.0 1.329650e-02
    ## 2        15.0 1.066423e-02
    ## 3        30.0 1.329650e-02
    ## 4        15.0 1.066423e-02
    ## 5        15.0 1.066423e-02
    ## 6        20.0 1.181202e-02
    ## 7        30.0 1.329650e-02
    ## 8        35.0 1.351314e-02
    ## 9        30.0 1.329650e-02
    ## 10       40.0 1.334494e-02
    ## 11       20.0 1.181202e-02
    ## 12       30.0 1.329650e-02
    ## 13       15.0 1.066423e-02
    ## 14       20.0 1.181202e-02
    ## 15       40.0 1.334494e-02
    ## 16       20.0 1.181202e-02
    ## 17       40.0 1.334494e-02
    ## 18       30.0 1.329650e-02
    ## 19       15.0 1.066423e-02
    ## 20       15.0 1.066423e-02
    ## 21       20.0 1.181202e-02
    ## 22       30.0 1.329650e-02
    ## 23       20.0 1.181202e-02
    ## 24       20.0 1.181202e-02
    ## 25       15.0 1.066423e-02
    ## 26       40.0 1.334494e-02
    ## 27       15.0 1.066423e-02
    ## 28       25.0 1.271335e-02
    ## 29       30.0 1.329650e-02
    ## 30       20.0 1.181202e-02
    ## 31       37.5 1.347702e-02
    ## 32       30.0 1.329650e-02
    ## 33       20.0 1.181202e-02
    ## 34       30.0 1.329650e-02
    ## 35       20.0 1.181202e-02
    ## 36       60.0 9.527364e-03
    ## 37       30.0 1.329650e-02
    ## 38       15.0 1.066423e-02
    ## 39       10.0 9.355705e-03
    ## 40       20.0 1.181202e-02
    ## 41       40.0 1.334494e-02
    ## 42       60.0 9.527364e-03
    ## 43       60.0 9.527364e-03
    ## 44       30.0 1.329650e-02
    ## 45       49.0 1.213751e-02
    ## 46       50.0 1.194158e-02
    ## 47       20.0 1.181202e-02
    ## 48       90.0 2.430531e-03
    ## 49        8.5 8.945224e-03
    ## 50       17.5 1.126378e-02
    ## 51       25.0 1.271335e-02
    ## 52       25.0 1.271335e-02
    ## 53       45.0 1.280615e-02
    ## 54       60.0 9.527364e-03
    ## 55      180.0 8.219475e-08
    ## 56       30.0 1.329650e-02
    ## 57       30.0 1.329650e-02
    ## 58       20.0 1.181202e-02
    ## 59       30.0 1.329650e-02
    ## 60       20.0 1.181202e-02
    ## 61       25.0 1.271335e-02
    ## 62      180.0 8.219475e-08
    ## 63       30.0 1.329650e-02
    ## 64       45.0 1.280615e-02
    ## 65       45.0 1.280615e-02
    ## 66       75.0 5.475204e-03
    ## 67       20.0 1.181202e-02
    ## 68       60.0 9.527364e-03
    ## 69       45.0 1.280615e-02
    ## 70       60.0 9.527364e-03
    ## 71       20.0 1.181202e-02

``` r
prob_plot(bayes_df_time)
```

![](ST417-Bayesian-Modelling-Project_files/figure-gfm/time%20prior-1.png)<!-- -->

``` r
xbar_time <- mean(data$time, na.rm = TRUE)
n_time <- NROW(data$time)
sigma_time <- sd(data$time, na.rm = TRUE)
se_time <- sigma_time / sqrt(n_time)

# likelihood calculation
likelihood_time <- dnorm(xbar_time, mean = theta_time, sd = se_time)
bayes_df_time <- data.frame(theta_time, prior_time, likelihood_time)

# posterior calculation
bayes_df_time$Product <- bayes_df_time$prior_time * bayes_df_time$likelihood_time # nolint
bayes_df_time$Posterior <- bayes_df_time$Product / sum(bayes_df_time$Product)
bayes_df_time
```

    ##    theta_time   prior_time likelihood_time       Product     Posterior
    ## 1        30.0 1.329650e-02    3.111704e-02  4.137478e-04  2.167277e-02
    ## 2        15.0 1.066423e-02    2.286116e-04  2.437967e-06  1.277046e-04
    ## 3        30.0 1.329650e-02    3.111704e-02  4.137478e-04  2.167277e-02
    ## 4        15.0 1.066423e-02    2.286116e-04  2.437967e-06  1.277046e-04
    ## 5        15.0 1.066423e-02    2.286116e-04  2.437967e-06  1.277046e-04
    ## 6        20.0 1.181202e-02    2.752618e-02  3.251396e-04  1.703133e-02
    ## 7        30.0 1.329650e-02    3.111704e-02  4.137478e-04  2.167277e-02
    ## 8        35.0 1.351314e-02    2.921481e-04  3.947838e-06  2.067941e-04
    ## 9        30.0 1.329650e-02    3.111704e-02  4.137478e-04  2.167277e-02
    ## 10       40.0 1.334494e-02    1.171818e-07  1.563784e-09  8.191353e-08
    ## 11       20.0 1.181202e-02    2.752618e-02  3.251396e-04  1.703133e-02
    ## 12       30.0 1.329650e-02    3.111704e-02  4.137478e-04  2.167277e-02
    ## 13       15.0 1.066423e-02    2.286116e-04  2.437967e-06  1.277046e-04
    ## 14       20.0 1.181202e-02    2.752618e-02  3.251396e-04  1.703133e-02
    ## 15       40.0 1.334494e-02    1.171818e-07  1.563784e-09  8.191353e-08
    ## 16       20.0 1.181202e-02    2.752618e-02  3.251396e-04  1.703133e-02
    ## 17       40.0 1.334494e-02    1.171818e-07  1.563784e-09  8.191353e-08
    ## 18       30.0 1.329650e-02    3.111704e-02  4.137478e-04  2.167277e-02
    ## 19       15.0 1.066423e-02    2.286116e-04  2.437967e-06  1.277046e-04
    ## 20       15.0 1.066423e-02    2.286116e-04  2.437967e-06  1.277046e-04
    ## 21       20.0 1.181202e-02    2.752618e-02  3.251396e-04  1.703133e-02
    ## 22       30.0 1.329650e-02    3.111704e-02  4.137478e-04  2.167277e-02
    ## 23       20.0 1.181202e-02    2.752618e-02  3.251396e-04  1.703133e-02
    ## 24       20.0 1.181202e-02    2.752618e-02  3.251396e-04  1.703133e-02
    ## 25       15.0 1.066423e-02    2.286116e-04  2.437967e-06  1.277046e-04
    ## 26       40.0 1.334494e-02    1.171818e-07  1.563784e-09  8.191353e-08
    ## 27       15.0 1.066423e-02    2.286116e-04  2.437967e-06  1.277046e-04
    ## 28       25.0 1.271335e-02    1.415943e-01  1.800138e-03  9.429412e-02
    ## 29       30.0 1.329650e-02    3.111704e-02  4.137478e-04  2.167277e-02
    ## 30       20.0 1.181202e-02    2.752618e-02  3.251396e-04  1.703133e-02
    ## 31       37.5 1.347702e-02    8.677610e-06  1.169483e-07  6.125939e-06
    ## 32       30.0 1.329650e-02    3.111704e-02  4.137478e-04  2.167277e-02
    ## 33       20.0 1.181202e-02    2.752618e-02  3.251396e-04  1.703133e-02
    ## 34       30.0 1.329650e-02    3.111704e-02  4.137478e-04  2.167277e-02
    ## 35       20.0 1.181202e-02    2.752618e-02  3.251396e-04  1.703133e-02
    ## 36       60.0 9.527364e-03    6.143457e-35  5.853096e-37  3.065945e-35
    ## 37       30.0 1.329650e-02    3.111704e-02  4.137478e-04  2.167277e-02
    ## 38       15.0 1.066423e-02    2.286116e-04  2.437967e-06  1.277046e-04
    ## 39       10.0 9.355705e-03    8.111535e-08  7.588913e-10  3.975194e-08
    ## 40       20.0 1.181202e-02    2.752618e-02  3.251396e-04  1.703133e-02
    ## 41       40.0 1.334494e-02    1.171818e-07  1.563784e-09  8.191353e-08
    ## 42       60.0 9.527364e-03    6.143457e-35  5.853096e-37  3.065945e-35
    ## 43       60.0 9.527364e-03    6.143457e-35  5.853096e-37  3.065945e-35
    ## 44       30.0 1.329650e-02    3.111704e-02  4.137478e-04  2.167277e-02
    ## 45       49.0 1.213751e-02    3.191297e-17  3.873441e-19  2.028971e-17
    ## 46       50.0 1.194158e-02    1.470049e-18  1.755470e-20  9.195436e-19
    ## 47       20.0 1.181202e-02    2.752618e-02  3.251396e-04  1.703133e-02
    ## 48       90.0 2.430531e-03   6.127925e-117 1.489411e-119 7.801774e-118
    ## 49        8.5 8.945224e-03    4.046520e-09  3.619703e-11  1.896058e-09
    ## 50       17.5 1.126378e-02    3.720408e-03  4.190585e-05  2.195095e-03
    ## 51       25.0 1.271335e-02    1.415943e-01  1.800138e-03  9.429412e-02
    ## 52       25.0 1.271335e-02    1.415943e-01  1.800138e-03  9.429412e-02
    ## 53       45.0 1.280615e-02    2.008028e-12  2.571510e-14  1.346998e-12
    ## 54       60.0 9.527364e-03    6.143457e-35  5.853096e-37  3.065945e-35
    ## 55      180.0 8.219475e-08    0.000000e+00  0.000000e+00  0.000000e+00
    ## 56       30.0 1.329650e-02    3.111704e-02  4.137478e-04  2.167277e-02
    ## 57       30.0 1.329650e-02    3.111704e-02  4.137478e-04  2.167277e-02
    ## 58       20.0 1.181202e-02    2.752618e-02  3.251396e-04  1.703133e-02
    ## 59       30.0 1.329650e-02    3.111704e-02  4.137478e-04  2.167277e-02
    ## 60       20.0 1.181202e-02    2.752618e-02  3.251396e-04  1.703133e-02
    ## 61       25.0 1.271335e-02    1.415943e-01  1.800138e-03  9.429412e-02
    ## 62      180.0 8.219475e-08    0.000000e+00  0.000000e+00  0.000000e+00
    ## 63       30.0 1.329650e-02    3.111704e-02  4.137478e-04  2.167277e-02
    ## 64       45.0 1.280615e-02    2.008028e-12  2.571510e-14  1.346998e-12
    ## 65       45.0 1.280615e-02    2.008028e-12  2.571510e-14  1.346998e-12
    ## 66       75.0 5.475204e-03    8.911020e-70  4.878965e-72  2.555680e-70
    ## 67       20.0 1.181202e-02    2.752618e-02  3.251396e-04  1.703133e-02
    ## 68       60.0 9.527364e-03    6.143457e-35  5.853096e-37  3.065945e-35
    ## 69       45.0 1.280615e-02    2.008028e-12  2.571510e-14  1.346998e-12
    ## 70       60.0 9.527364e-03    6.143457e-35  5.853096e-37  3.065945e-35
    ## 71       20.0 1.181202e-02    2.752618e-02  3.251396e-04  1.703133e-02

``` r
# prior and posterior comparison
prior_post_plot(bayes_df_time)
```

![](ST417-Bayesian-Modelling-Project_files/figure-gfm/time%20posterier-1.png)<!-- -->

``` r
theta_dist <- na.omit(data$est_distance)
prior_dist <- dnorm(theta_dist, mean = mean(theta_dist), sd = sd(theta_dist))
bayes_df_dist <- data.frame(theta_dist, prior_dist)
bayes_df_dist
```

    ##    theta_dist   prior_dist
    ## 1         3.0 1.806677e-02
    ## 2        20.0 2.155656e-02
    ## 3        10.0 2.172206e-02
    ## 4        10.0 2.172206e-02
    ## 5         3.0 1.806677e-02
    ## 6        10.0 2.172206e-02
    ## 7         4.0 1.872681e-02
    ## 8         3.5 1.840116e-02
    ## 9         2.5 1.772433e-02
    ## 10       15.0 2.251851e-02
    ## 11       15.0 2.251851e-02
    ## 12       15.0 2.251851e-02
    ## 13        3.0 1.806677e-02
    ## 14        5.0 1.934921e-02
    ## 15        5.0 1.934921e-02
    ## 16        5.0 1.934921e-02
    ## 17       10.0 2.172206e-02
    ## 18        4.0 1.872681e-02
    ## 19        4.0 1.872681e-02
    ## 20        3.5 1.840116e-02
    ## 21       18.0 2.214702e-02
    ## 22        2.0 1.737453e-02
    ## 23        1.0 1.665566e-02
    ## 24        3.0 1.806677e-02
    ## 25       40.0 8.161101e-03
    ## 26        2.0 1.737453e-02
    ## 27       10.0 2.172206e-02
    ## 28       10.0 2.172206e-02
    ## 29        1.5 1.701807e-02
    ## 30        5.0 1.934921e-02
    ## 31       25.0 1.905549e-02
    ## 32        5.0 1.934921e-02
    ## 33       25.0 1.905549e-02
    ## 34        2.0 1.737453e-02
    ## 35       20.0 2.155656e-02
    ## 36        3.0 1.806677e-02
    ## 37        7.0 2.046021e-02
    ## 38       80.0 2.554681e-05
    ## 39       10.0 2.172206e-02
    ## 40       10.0 2.172206e-02
    ## 41       20.0 2.155656e-02
    ## 42       75.0 6.942001e-05
    ## 43        5.0 1.934921e-02
    ## 44        5.0 1.934921e-02
    ## 45       15.0 2.251851e-02
    ## 46        3.0 1.806677e-02
    ## 47       40.0 8.161101e-03
    ## 48        7.0 2.046021e-02
    ## 49        7.0 2.046021e-02
    ## 50        2.0 1.737453e-02
    ## 51       10.0 2.172206e-02
    ## 52       35.0 1.172478e-02
    ## 53       45.0 5.245582e-03
    ## 54       70.0 1.741942e-04
    ## 55        5.0 1.934921e-02
    ## 56        5.0 1.934921e-02
    ## 57        8.0 2.093909e-02
    ## 58        7.0 2.046021e-02
    ## 59        2.0 1.737453e-02
    ## 60       10.0 2.172206e-02
    ## 61       60.0 8.636435e-04
    ## 62       20.0 2.155656e-02
    ## 63       20.0 2.155656e-02
    ## 64        4.2 1.885447e-02
    ## 65       45.0 5.245582e-03
    ## 66        2.0 1.737453e-02
    ## 67       30.0 1.555471e-02
    ## 68       21.0 2.116584e-02
    ## 69       20.0 2.155656e-02
    ## 70        5.0 1.934921e-02

``` r
prob_plot(bayes_df_dist)
```

![](ST417-Bayesian-Modelling-Project_files/figure-gfm/distance%20prior-1.png)<!-- -->

``` r
xbar_dist <- mean(data$distance, na.rm = TRUE)
n_dist <- NROW(data$distance)
sigma_dist <- sd(data$distance, na.rm = TRUE)
se_dist <- sigma_dist / sqrt(n_dist)

# likelihood calculation
likelihood_dist <- dnorm(xbar_dist, mean = theta_dist, sd = se_dist)
bayes_df_dist <- data.frame(theta_dist, prior_dist, likelihood_dist)

# posterior calculation
bayes_df_dist$Product <- bayes_df_dist$prior_dist * bayes_df_dist$likelihood_dist # nolint
bayes_df_dist$Posterior <- bayes_df_dist$Product / sum(bayes_df_dist$Product)
bayes_df_dist
```

    ##    theta_dist   prior_dist likelihood_dist       Product     Posterior
    ## 1         3.0 1.806677e-02    2.082309e-03  3.762060e-05  6.488084e-04
    ## 2        20.0 2.155656e-02    1.051059e-07  2.265722e-09  3.907486e-08
    ## 3        10.0 2.172206e-02    1.788449e-01  3.884879e-03  6.699899e-02
    ## 4        10.0 2.172206e-02    1.788449e-01  3.884879e-03  6.699899e-02
    ## 5         3.0 1.806677e-02    2.082309e-03  3.762060e-05  6.488084e-04
    ## 6        10.0 2.172206e-02    1.788449e-01  3.884879e-03  6.699899e-02
    ## 7         4.0 1.872681e-02    8.170387e-03  1.530053e-04  2.638744e-03
    ## 8         3.5 1.840116e-02    4.252260e-03  7.824650e-05  1.349447e-03
    ## 9         2.5 1.772433e-02    9.594424e-04  1.700547e-05  2.932780e-04
    ## 10       15.0 2.251851e-02    2.881651e-03  6.489048e-05  1.119107e-03
    ## 11       15.0 2.251851e-02    2.881651e-03  6.489048e-05  1.119107e-03
    ## 12       15.0 2.251851e-02    2.881651e-03  6.489048e-05  1.119107e-03
    ## 13        3.0 1.806677e-02    2.082309e-03  3.762060e-05  6.488084e-04
    ## 14        5.0 1.934921e-02    2.512655e-02  4.861788e-04  8.384686e-03
    ## 15        5.0 1.934921e-02    2.512655e-02  4.861788e-04  8.384686e-03
    ## 16        5.0 1.934921e-02    2.512655e-02  4.861788e-04  8.384686e-03
    ## 17       10.0 2.172206e-02    1.788449e-01  3.884879e-03  6.699899e-02
    ## 18        4.0 1.872681e-02    8.170387e-03  1.530053e-04  2.638744e-03
    ## 19        4.0 1.872681e-02    8.170387e-03  1.530053e-04  2.638744e-03
    ## 20        3.5 1.840116e-02    4.252260e-03  7.824650e-05  1.349447e-03
    ## 21       18.0 2.214702e-02    1.300931e-05  2.881176e-07  4.968903e-06
    ## 22        2.0 1.737453e-02    4.159497e-04  7.226930e-06  1.246363e-04
    ## 23        1.0 1.665566e-02    6.512220e-05  1.084653e-06  1.870603e-05
    ## 24        3.0 1.806677e-02    2.082309e-03  3.762060e-05  6.488084e-04
    ## 25       40.0 8.161101e-03    6.573280e-52  5.364520e-54  9.251703e-53
    ## 26        2.0 1.737453e-02    4.159497e-04  7.226930e-06  1.246363e-04
    ## 27       10.0 2.172206e-02    1.788449e-01  3.884879e-03  6.699899e-02
    ## 28       10.0 2.172206e-02    1.788449e-01  3.884879e-03  6.699899e-02
    ## 29        1.5 1.701807e-02    1.696723e-04  2.887495e-06  4.979801e-05
    ## 30        5.0 1.934921e-02    2.512655e-02  4.861788e-04  8.384686e-03
    ## 31       25.0 1.905549e-02    8.678293e-15  1.653692e-16  2.851972e-15
    ## 32        5.0 1.934921e-02    2.512655e-02  4.861788e-04  8.384686e-03
    ## 33       25.0 1.905549e-02    8.678293e-15  1.653692e-16  2.851972e-15
    ## 34        2.0 1.737453e-02    4.159497e-04  7.226930e-06  1.246363e-04
    ## 35       20.0 2.155656e-02    1.051059e-07  2.265722e-09  3.907486e-08
    ## 36        3.0 1.806677e-02    2.082309e-03  3.762060e-05  6.488084e-04
    ## 37        7.0 2.046021e-02    1.144173e-01  2.341003e-03  4.037316e-02
    ## 38       80.0 2.554681e-05   2.763793e-267 7.060610e-272 1.217679e-270
    ## 39       10.0 2.172206e-02    1.788449e-01  3.884879e-03  6.699899e-02
    ## 40       10.0 2.172206e-02    1.788449e-01  3.884879e-03  6.699899e-02
    ## 41       20.0 2.155656e-02    1.051059e-07  2.265722e-09  3.907486e-08
    ## 42       75.0 6.942001e-05   4.184718e-231 2.905031e-235 5.010044e-234
    ## 43        5.0 1.934921e-02    2.512655e-02  4.861788e-04  8.384686e-03
    ## 44        5.0 1.934921e-02    2.512655e-02  4.861788e-04  8.384686e-03
    ## 45       15.0 2.251851e-02    2.881651e-03  6.489048e-05  1.119107e-03
    ## 46        3.0 1.806677e-02    2.082309e-03  3.762060e-05  6.488084e-04
    ## 47       40.0 8.161101e-03    6.573280e-52  5.364520e-54  9.251703e-53
    ## 48        7.0 2.046021e-02    1.144173e-01  2.341003e-03  4.037316e-02
    ## 49        7.0 2.046021e-02    1.144173e-01  2.341003e-03  4.037316e-02
    ## 50        2.0 1.737453e-02    4.159497e-04  7.226930e-06  1.246363e-04
    ## 51       10.0 2.172206e-02    1.788449e-01  3.884879e-03  6.699899e-02
    ## 52       35.0 1.172478e-02    6.862965e-37  8.046678e-39  1.387738e-37
    ## 53       45.0 5.245582e-03    1.425193e-69  7.475966e-72  1.289312e-70
    ## 54       70.0 1.741942e-04   1.434326e-197 2.498512e-201 4.308958e-200
    ## 55        5.0 1.934921e-02    2.512655e-02  4.861788e-04  8.384686e-03
    ## 56        5.0 1.934921e-02    2.512655e-02  4.861788e-04  8.384686e-03
    ## 57        8.0 2.093909e-02    1.694184e-01  3.547466e-03  6.117994e-02
    ## 58        7.0 2.046021e-02    1.144173e-01  2.341003e-03  4.037316e-02
    ## 59        2.0 1.737453e-02    4.159497e-04  7.226930e-06  1.246363e-04
    ## 60       10.0 2.172206e-02    1.788449e-01  3.884879e-03  6.699899e-02
    ## 61       60.0 8.636435e-04   1.954676e-138 1.688144e-141 2.911389e-140
    ## 62       20.0 2.155656e-02    1.051059e-07  2.265722e-09  3.907486e-08
    ## 63       20.0 2.155656e-02    1.051059e-07  2.265722e-09  3.907486e-08
    ## 64        4.2 1.885447e-02    1.043002e-02  1.966526e-04  3.391489e-03
    ## 65       45.0 5.245582e-03    1.425193e-69  7.475966e-72  1.289312e-70
    ## 66        2.0 1.737453e-02    4.159497e-04  7.226930e-06  1.246363e-04
    ## 67       30.0 1.555471e-02    1.622043e-24  2.523042e-26  4.351263e-25
    ## 68       21.0 2.116584e-02    6.555457e-09  1.387518e-10  2.392926e-09
    ## 69       20.0 2.155656e-02    1.051059e-07  2.265722e-09  3.907486e-08
    ## 70        5.0 1.934921e-02    2.512655e-02  4.861788e-04  8.384686e-03

``` r
# prior and posterior comparison
prior_post_plot(bayes_df_dist)
```

![](ST417-Bayesian-Modelling-Project_files/figure-gfm/distance%20posterior-1.png)<!-- -->

``` r
theta_cost <- na.omit(data$est_cost)
prior_cost <- dnorm(theta_cost, mean = mean(theta_cost), sd = sd(theta_cost))
bayes_df_cost <- data.frame(theta_cost, prior_cost)
bayes_df_cost
```

    ##    theta_cost   prior_cost
    ## 1         3.0 7.718878e-02
    ## 2         5.0 9.683788e-02
    ## 3         0.8 4.526016e-02
    ## 4         3.0 7.718878e-02
    ## 5         1.5 5.540087e-02
    ## 6         6.0 9.889950e-02
    ## 7         2.0 6.283649e-02
    ## 8         5.0 9.683788e-02
    ## 9         3.0 7.718878e-02
    ## 10        6.0 9.889950e-02
    ## 11        4.0 8.915902e-02
    ## 12        5.0 9.683788e-02
    ## 13        2.0 6.283649e-02
    ## 14        6.0 9.889950e-02
    ## 15        5.0 9.683788e-02
    ## 16        5.0 9.683788e-02
    ## 17        5.0 9.683788e-02
    ## 18        2.0 6.283649e-02
    ## 19        5.0 9.683788e-02
    ## 20        3.0 7.718878e-02
    ## 21       10.0 5.814039e-02
    ## 22        4.5 9.363682e-02
    ## 23        2.5 7.018179e-02
    ## 24        0.0 3.462055e-02
    ## 25        2.0 6.283649e-02
    ## 26        7.0 9.497554e-02
    ## 27        5.0 9.683788e-02
    ## 28        5.0 9.683788e-02
    ## 29        7.0 9.497554e-02
    ## 30        1.0 4.809927e-02
    ## 31       10.0 5.814039e-02
    ## 32        5.0 9.683788e-02
    ## 33        3.0 7.718878e-02
    ## 34       15.0 7.492592e-03
    ## 35        3.0 7.718878e-02
    ## 36       10.0 5.814039e-02
    ## 37        5.0 9.683788e-02
    ## 38        5.0 9.683788e-02
    ## 39       25.0 1.230565e-06
    ## 40        5.0 9.683788e-02
    ## 41        5.0 9.683788e-02
    ## 42       10.0 5.814039e-02
    ## 43       12.5 2.529821e-02
    ## 44        5.0 9.683788e-02
    ## 45        4.0 8.915902e-02
    ## 46        4.0 8.915902e-02
    ## 47        6.0 9.889950e-02
    ## 48       10.0 5.814039e-02
    ## 49        6.0 9.889950e-02
    ## 50        5.0 9.683788e-02
    ## 51        5.0 9.683788e-02
    ## 52        5.0 9.683788e-02
    ## 53       10.0 5.814039e-02
    ## 54       10.0 5.814039e-02
    ## 55       12.0 3.081293e-02
    ## 56        3.0 7.718878e-02
    ## 57        5.0 9.683788e-02
    ## 58        2.0 6.283649e-02
    ## 59        5.0 9.683788e-02
    ## 60        1.0 4.809927e-02
    ## 61       10.0 5.814039e-02
    ## 62       15.0 7.492592e-03
    ## 63        2.0 6.283649e-02
    ## 64       10.0 5.814039e-02
    ## 65        6.0 9.889950e-02
    ## 66        2.0 6.283649e-02
    ## 67       10.0 5.814039e-02
    ## 68        6.0 9.889950e-02
    ## 69        7.0 9.497554e-02
    ## 70        4.0 8.915902e-02
    ## 71       10.0 5.814039e-02

``` r
prob_plot(bayes_df_cost)
```

![](ST417-Bayesian-Modelling-Project_files/figure-gfm/cost%20prior-1.png)<!-- -->

``` r
xbar_cost <- mean(data$cost, na.rm = TRUE)
n_cost <- NROW(data$cost)
sigma_cost <- sd(data$cost, na.rm = TRUE)
se_cost <- sigma_cost / sqrt(n_cost)

# likelihood calculation
likelihood_cost <- dnorm(xbar_cost, mean = theta_cost, sd = se_cost)
bayes_df_cost <- data.frame(theta_cost, prior_cost, likelihood_cost)

# posterior calculation
bayes_df_cost$Product <- bayes_df_cost$prior_cost * bayes_df_cost$likelihood_cost # nolint
bayes_df_cost$Posterior <- bayes_df_cost$Product / sum(bayes_df_cost$Product)
bayes_df_cost
```

    ##    theta_cost   prior_cost likelihood_cost       Product     Posterior
    ## 1         3.0 7.718878e-02    4.916232e-03  3.794780e-04  9.593465e-04
    ## 2         5.0 9.683788e-02    5.104159e-15  4.942759e-16  1.249564e-15
    ## 3         0.8 4.526016e-02    1.083789e-01  4.905248e-03  1.240080e-02
    ## 4         3.0 7.718878e-02    4.916232e-03  3.794780e-04  9.593465e-04
    ## 5         1.5 5.540087e-02    8.971016e-01  4.970021e-02  1.256456e-01
    ## 6         6.0 9.889950e-02    7.452719e-25  7.370701e-26  1.863364e-25
    ## 7         2.0 6.283649e-02    6.914028e-01  4.344532e-02  1.098328e-01
    ## 8         5.0 9.683788e-02    5.104159e-15  4.942759e-16  1.249564e-15
    ## 9         3.0 7.718878e-02    4.916232e-03  3.794780e-04  9.593465e-04
    ## 10        6.0 9.889950e-02    7.452719e-25  7.370701e-26  1.863364e-25
    ## 11        4.0 8.915902e-02    9.572604e-08  8.534840e-09  2.157666e-08
    ## 12        5.0 9.683788e-02    5.104159e-15  4.942759e-16  1.249564e-15
    ## 13        2.0 6.283649e-02    6.914028e-01  4.344532e-02  1.098328e-01
    ## 14        6.0 9.889950e-02    7.452719e-25  7.370701e-26  1.863364e-25
    ## 15        5.0 9.683788e-02    5.104159e-15  4.942759e-16  1.249564e-15
    ## 16        5.0 9.683788e-02    5.104159e-15  4.942759e-16  1.249564e-15
    ## 17        5.0 9.683788e-02    5.104159e-15  4.942759e-16  1.249564e-15
    ## 18        2.0 6.283649e-02    6.914028e-01  4.344532e-02  1.098328e-01
    ## 19        5.0 9.683788e-02    5.104159e-15  4.942759e-16  1.249564e-15
    ## 20        3.0 7.718878e-02    4.916232e-03  3.794780e-04  9.593465e-04
    ## 21       10.0 5.814039e-02    8.032317e-90  4.670021e-91  1.180613e-90
    ## 22        4.5 9.363682e-02    4.621576e-11  4.327497e-12  1.094021e-11
    ## 23        2.5 7.018179e-02    1.218975e-01  8.554983e-03  2.162759e-02
    ## 24        0.0 3.462055e-02    2.808134e-04  9.721913e-06  2.457767e-05
    ## 25        2.0 6.283649e-02    6.914028e-01  4.344532e-02  1.098328e-01
    ## 26        7.0 9.497554e-02    2.979900e-37  2.830176e-38  7.154880e-38
    ## 27        5.0 9.683788e-02    5.104159e-15  4.942759e-16  1.249564e-15
    ## 28        5.0 9.683788e-02    5.104159e-15  4.942759e-16  1.249564e-15
    ## 29        7.0 9.497554e-02    2.979900e-37  2.830176e-38  7.154880e-38
    ## 30        1.0 4.809927e-02    2.662725e-01  1.280751e-02  3.237827e-02
    ## 31       10.0 5.814039e-02    8.032317e-90  4.670021e-91  1.180613e-90
    ## 32        5.0 9.683788e-02    5.104159e-15  4.942759e-16  1.249564e-15
    ## 33        3.0 7.718878e-02    4.916232e-03  3.794780e-04  9.593465e-04
    ## 34       15.0 7.492592e-03   1.094379e-228 8.199736e-231 2.072950e-230
    ## 35        3.0 7.718878e-02    4.916232e-03  3.794780e-04  9.593465e-04
    ## 36       10.0 5.814039e-02    8.032317e-90  4.670021e-91  1.180613e-90
    ## 37        5.0 9.683788e-02    5.104159e-15  4.942759e-16  1.249564e-15
    ## 38        5.0 9.683788e-02    5.104159e-15  4.942759e-16  1.249564e-15
    ## 39       25.0 1.230565e-06    0.000000e+00  0.000000e+00  0.000000e+00
    ## 40        5.0 9.683788e-02    5.104159e-15  4.942759e-16  1.249564e-15
    ## 41        5.0 9.683788e-02    5.104159e-15  4.942759e-16  1.249564e-15
    ## 42       10.0 5.814039e-02    8.032317e-90  4.670021e-91  1.180613e-90
    ## 43       12.5 2.529821e-02   3.018756e-151 7.636914e-153 1.930664e-152
    ## 44        5.0 9.683788e-02    5.104159e-15  4.942759e-16  1.249564e-15
    ## 45        4.0 8.915902e-02    9.572604e-08  8.534840e-09  2.157666e-08
    ## 46        4.0 8.915902e-02    9.572604e-08  8.534840e-09  2.157666e-08
    ## 47        6.0 9.889950e-02    7.452719e-25  7.370701e-26  1.863364e-25
    ## 48       10.0 5.814039e-02    8.032317e-90  4.670021e-91  1.180613e-90
    ## 49        6.0 9.889950e-02    7.452719e-25  7.370701e-26  1.863364e-25
    ## 50        5.0 9.683788e-02    5.104159e-15  4.942759e-16  1.249564e-15
    ## 51        5.0 9.683788e-02    5.104159e-15  4.942759e-16  1.249564e-15
    ## 52        5.0 9.683788e-02    5.104159e-15  4.942759e-16  1.249564e-15
    ## 53       10.0 5.814039e-02    8.032317e-90  4.670021e-91  1.180613e-90
    ## 54       10.0 5.814039e-02    8.032317e-90  4.670021e-91  1.180613e-90
    ## 55       12.0 3.081293e-02   1.111943e-137 3.426222e-139 8.661724e-139
    ## 56        3.0 7.718878e-02    4.916232e-03  3.794780e-04  9.593465e-04
    ## 57        5.0 9.683788e-02    5.104159e-15  4.942759e-16  1.249564e-15
    ## 58        2.0 6.283649e-02    6.914028e-01  4.344532e-02  1.098328e-01
    ## 59        5.0 9.683788e-02    5.104159e-15  4.942759e-16  1.249564e-15
    ## 60        1.0 4.809927e-02    2.662725e-01  1.280751e-02  3.237827e-02
    ## 61       10.0 5.814039e-02    8.032317e-90  4.670021e-91  1.180613e-90
    ## 62       15.0 7.492592e-03   1.094379e-228 8.199736e-231 2.072950e-230
    ## 63        2.0 6.283649e-02    6.914028e-01  4.344532e-02  1.098328e-01
    ## 64       10.0 5.814039e-02    8.032317e-90  4.670021e-91  1.180613e-90
    ## 65        6.0 9.889950e-02    7.452719e-25  7.370701e-26  1.863364e-25
    ## 66        2.0 6.283649e-02    6.914028e-01  4.344532e-02  1.098328e-01
    ## 67       10.0 5.814039e-02    8.032317e-90  4.670021e-91  1.180613e-90
    ## 68        6.0 9.889950e-02    7.452719e-25  7.370701e-26  1.863364e-25
    ## 69        7.0 9.497554e-02    2.979900e-37  2.830176e-38  7.154880e-38
    ## 70        4.0 8.915902e-02    9.572604e-08  8.534840e-09  2.157666e-08
    ## 71       10.0 5.814039e-02    8.032317e-90  4.670021e-91  1.180613e-90

``` r
# prior and posterior comparison
prior_post_plot(bayes_df_cost)
```

![](ST417-Bayesian-Modelling-Project_files/figure-gfm/cost%20posterior-1.png)<!-- -->

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
time_sims <- rnorm(10000, mean = post_mean_time, sd = post_sd_time)
time_point_estimate <- mean(time_sims)
time_point_estimate
```

    ## [1] 25.22175

``` r
time_ci95 <- quantile(time_sims, probs = c(0.025, 0.975))
time_ci95
```

    ##     2.5%    97.5% 
    ## 19.70202 30.75898

``` r
normal_interval(0.95, time_ci95)
```

![](ST417-Bayesian-Modelling-Project_files/figure-gfm/95%20credible%20intervals-1.png)<!-- -->

``` r
dist_sims <- rnorm(10000, mean = post_mean_dist, sd = post_sd_dist)
dist_point_estimate <- mean(dist_sims)
dist_point_estimate
```

    ## [1] 9.189396

``` r
dist_ci95 <- quantile(dist_sims, probs = c(0.025, 0.975))
dist_ci95
```

    ##      2.5%     97.5% 
    ##  5.208868 13.170990

``` r
normal_interval(0.95, dist_ci95)
```

![](ST417-Bayesian-Modelling-Project_files/figure-gfm/95%20credible%20intervals-2.png)<!-- -->

``` r
cost_sims <- rnorm(10000, mean = post_mean_cost, sd = post_sd_cost)
cost_point_estimate <- mean(cost_sims)
cost_point_estimate
```

    ## [1] 1.700958

``` r
cost_ci95 <- quantile(cost_sims, probs = c(0.025, 0.975))
cost_ci95
```

    ##     2.5%    97.5% 
    ## 0.888957 2.498599

``` r
normal_interval(0.95, cost_ci95)
```

![](ST417-Bayesian-Modelling-Project_files/figure-gfm/95%20credible%20intervals-3.png)<!-- -->

``` r
# # 95% credible intervals plot
# x <- seq(from = 0, to = 100, length.out = 1000)

# priorx_time <- dnorm(x, mean = mean(data$est_time, na.rm = TRUE), sd = sd(data$est_time, na.rm = TRUE)) # nolint
# priorx_dist <- dnorm(x, mean = mean(data$est_distance, na.rm = TRUE), sd = sd(data$est_distance, na.rm = TRUE)) # nolint
# priorx_cost <- dnorm(x, mean = mean(data$est_cost, na.rm = TRUE), sd = sd(data$est_cost, na.rm = TRUE)) # nolint

# datax_time  <- dnorm(x, mean = mean(data$time, na.rm = TRUE), sd = se_time)
# datax_dist  <- dnorm(x, mean = mean(data$distance, na.rm = TRUE), sd = se_dist)
# datax_cost  <- dnorm(x, mean = mean(data$cost, na.rm = TRUE), sd = se_cost)

# postx_time  <- dnorm(x, mean = post_mean_time, sd = post_sd_time)
# postx_dist  <- dnorm(x, mean = post_mean_dist, sd = post_sd_dist)
# postx_cost  <- dnorm(x, mean = post_mean_cost, sd = post_sd_cost)

# plot(x, priorx_time, type = "l", col = "blue", lwd = 2, xlab = "Time (min)", ylab = "Density", main = "Prior and Posterior Distributions") # nolint
# lines(x, datax_time, col = "red", lwd = 2)
# lines(x, postx_time, col = "green", lwd = 2)

# legend("topright", legend = c("Prior", "Data", "Posterior"), col = c("blue", "red", "green"), lwd = 2) # nolint
```

``` r
x <- seq(-60, 80, length = 150)

priorx_time <- dnorm(x, mean = mean(data$est_time, na.rm = TRUE),
                     sd = sd(data$est_time, na.rm = TRUE))
priorx_dist <- dnorm(x, mean = mean(data$est_distance, na.rm = TRUE),
                     sd = sd(data$est_distance, na.rm = TRUE))
priorx_cost <- dnorm(x, mean = mean(data$est_cost, na.rm = TRUE),
                     sd = sd(data$est_cost, na.rm = TRUE))

datax_time  <- dnorm(x, mean = mean(data$time, na.rm = TRUE), sd = se_time)
datax_dist  <- dnorm(x, mean = mean(data$distance, na.rm = TRUE), sd = se_dist)
datax_cost  <- dnorm(x, mean = mean(data$cost, na.rm = TRUE), sd = se_cost)

postx_time <- dnorm(x, mean = post_mean_time, sd = post_sd_time)
postx_dist <- dnorm(x, mean = post_mean_dist, sd = post_sd_dist)
postx_cost <- dnorm(x, mean = post_mean_cost, sd = post_sd_cost)

plot(x, priorx_time, type = "l", lwd = 3, xlim = c(-40, 60), ylim = c(0, 0.8),
     col = "red4", main = "", xlab = "theta", ylab = "")
lines(x, priorx_dist, col = "green4", lwd = 3)
lines(x, priorx_cost, col = "orange3", lwd = 3)

lines(x, datax_time, col = "black", lwd = 3)
lines(x, datax_dist, col = "black", lwd = 3)
lines(x, datax_cost, col = "black", lwd = 3)

lines(x, postx_time, col = "red", lwd = 3)
lines(x, postx_dist, col = "green", lwd = 3)
lines(x, postx_cost, col = "orange", lwd = 3)

legend("topright",
       c("Time Prior", "Distance Prior", "Cost Prior", "Data",
        "Time Post", "Distance Post", "Cost Post"),
       lty = 1, lwd = 3,
       col = c("red4", "green4", "orange3", "black", "red", "green", "orange")
       )
```

![](ST417-Bayesian-Modelling-Project_files/figure-gfm/distributions%20plot-1.png)<!-- -->

## Bayesian Analysis with rjags

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
data <- list(
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

``` r
# compile model
jags_model <- jags.model(
  textConnection(model),
  data = data,
  inits = list(.RNG.name = "base::Wichmann-Hill", .RNG.seed = 1989)
)
```

    ## Compiling model graph
    ##    Resolving undeclared variables
    ##    Allocating nodes
    ## Graph information:
    ##    Observed stochastic nodes: 207
    ##    Unobserved stochastic nodes: 15
    ##    Total graph size: 235
    ## 
    ## Initializing model

``` r
# burn in model
update(jags_model, 1000)
```

``` r
# simulate posterior
sims <- coda.samples(
  jags_model,
  variable.names = c("mu_time", "mu_dist", "mu_cost",
                     "sigma_time", "sigma_dist", "sigma_cost"),
  n.iter = 10000
)
```

``` r
# plot sims
plot(sims)
```

![](ST417-Bayesian-Modelling-Project_files/figure-gfm/sims%20plot-1.png)<!-- -->![](ST417-Bayesian-Modelling-Project_files/figure-gfm/sims%20plot-2.png)<!-- -->

``` r
# summary
summary(sims)
```

    ## 
    ## Iterations = 1001:11000
    ## Thinning interval = 1 
    ## Number of chains = 1 
    ## Sample size per chain = 10000 
    ## 
    ## 1. Empirical mean and standard deviation for each variable,
    ##    plus standard error of the mean:
    ## 
    ##                 Mean        SD  Naive SE Time-series SE
    ## mu_cost     1.711712 0.4108362 4.108e-03      4.108e-03
    ## mu_dist     9.177441 2.0488922 2.049e-02      2.049e-02
    ## mu_time    25.222705 2.8560561 2.856e-02      2.949e-02
    ## sigma_cost  0.094969 0.0153308 1.533e-04      1.554e-04
    ## sigma_dist  0.003397 0.0005688 5.688e-06      5.771e-06
    ## sigma_time  0.001763 0.0002961 2.961e-06      3.039e-06
    ## 
    ## 2. Quantiles for each variable:
    ## 
    ##                 2.5%       25%       50%       75%     97.5%
    ## mu_cost     0.905480  1.432644  1.715283  1.989193  2.513230
    ## mu_dist     5.111418  7.793923  9.174260 10.530034 13.236099
    ## mu_time    19.595055 23.313983 25.193080 27.133262 30.866511
    ## sigma_cost  0.067490  0.084089  0.094029  0.104970  0.126945
    ## sigma_dist  0.002380  0.003005  0.003368  0.003753  0.004620
    ## sigma_time  0.001227  0.001556  0.001748  0.001952  0.002377

``` r
# compile model
jags_model_mc <- jags.model(
  textConnection(model),
  data = data,
  n.chains = 3
)
```

    ## Compiling model graph
    ##    Resolving undeclared variables
    ##    Allocating nodes
    ## Graph information:
    ##    Observed stochastic nodes: 207
    ##    Unobserved stochastic nodes: 15
    ##    Total graph size: 235
    ## 
    ## Initializing model

``` r
# burn in model
update(jags_model_mc, 1000)
```

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
    ## mu_cost     1.701224 0.4112582 2.374e-03      2.338e-03
    ## mu_dist     9.174625 2.0399177 1.178e-02      1.168e-02
    ## mu_time    25.195420 2.8327215 1.635e-02      1.578e-02
    ## sigma_cost  0.094955 0.0155784 8.994e-05      9.042e-05
    ## sigma_dist  0.003408 0.0005736 3.312e-06      3.366e-06
    ## sigma_time  0.001760 0.0002954 1.705e-06      1.709e-06
    ## 
    ## 2. Quantiles for each variable:
    ## 
    ##                 2.5%       25%       50%       75%     97.5%
    ## mu_cost     0.900265  1.426189  1.698803  1.974825  2.516755
    ## mu_dist     5.144719  7.816027  9.173807 10.540959 13.187189
    ## mu_time    19.664716 23.291962 25.206493 27.080669 30.763619
    ## sigma_cost  0.067100  0.083938  0.094159  0.104983  0.127903
    ## sigma_dist  0.002384  0.003004  0.003375  0.003778  0.004616
    ## sigma_time  0.001230  0.001552  0.001744  0.001952  0.002378

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

``` r
# autocorrelation
autocorr.plot(sims_mc[[1]])
```

![](ST417-Bayesian-Modelling-Project_files/figure-gfm/sims_mc%20autocorr-1.png)<!-- -->

``` r
chains <- data.frame(sims_mc[[1]])

# 95% credible intervals
CI_time <- quantile(chains$mu_time, probs = c(0.025, 0.975))
CI_dist <- quantile(chains$mu_dist, probs = c(0.025, 0.975))
CI_cost <- quantile(chains$mu_cost, probs = c(0.025, 0.975))
CI_time
```

    ##     2.5%    97.5% 
    ## 19.70192 30.78192

``` r
CI_dist
```

    ##      2.5%     97.5% 
    ##  5.081659 13.239030

``` r
CI_cost
```

    ##     2.5%    97.5% 
    ## 0.904729 2.520113

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
