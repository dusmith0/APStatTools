
# APStatTools 

**Description** The reason for this function is to make R accessible for many of the tools and calculation used in AP Statistics and Undergraduate Statistics, with little to know knowladge of R.

**Included Functions**
*build_dist()* will allow the user to build a number of distributions, shade in the area of interest, and display the probability of the shaded area. *build_qq()* will allow the user to build a quick QQ plot that can be tested against a number of distributions. *find_conf()* contains code that can calculate simple Wald Confidence Intervals *find_probs()* function can find various probabilities based on a number of distributions. *get_data()* is a wrapper function around many input methods R has to allow the user to easily and quickly input data into R for use. You will only need to know how to find your file's path. *get_measures()*  will allow the user to input data or data sets, and return a large list of statistical measures. *test()* can preform many of the statistical tests and confidence intervals used in AP Statistics. 

## Installing
```{r, eval = FALSE, echo = TRUE}
devtools::install_github("dusmith0/APStatTools", build_vignettes = TRUE)
```
If the above fails attempt to install the package without the vignette.
```{r, eval = FALSE, echo = TRUE}
devtools::install_github("dusmith0/APStatTools", build_vignettes = FASLE)
```
Then type the following command to load the package.
```{r, eval = FALSE, echo = TRUE}
library("APStatTools")
```
# The functions
## **build_dist()**

The purpose of this function is to allow users to build a number of common distributions, shade in regions that we may want to find proportions of, and display their probabilities. 

The function contains the "Normal", "Student's T", "Chi-Squared", "Binomial", "QQ", and "Regression" plots. 

It can calculate probabilities to the left, right, in between two points, or two tailed distributions. 

### How to use:

This function will accept the following arguments:
      Note: That the majority are not needed, and their default is provided. 
1. type="normal"
2. tail="left",
3. bound = NULL, 
4. df = 1, 
5. prob = .5, 
6. trials = 10, 
7. data = NULL, 
8. X = NULL, 
9. Y = NULL, 
10. display_prob = FALSE

*type = "distribution"* will allow you to adjust the distribution, where "distribution" can be any one of the following:

"normal", "t-dist", "chi-squared", "binomial", "QQ", "regression"

*tail = "direction"* will allow you to change in what direction is the area being calculated or shaded. "Direction" can be any one of the following:

 "left", "right", "inner", "outer" or "two", "left_not_equal", "right_not_equal", Note: "left_not_equal","left_not_equal","inner_not_equal", and "two_not_equal" are intended for the "binomial" only.
      
*bound = c(value)* will designate where the boundaries of the shaded region will go. 
If this value is left as NULL, it will produce the distribution. If you need an interior or two-tailed boundary, you can set bound = c(left, right). 

### Examples
To build the Standard Normal use:
```{r, eval = T, echo = T}
build_dist()
```


To build Student's t-distribution use: (Where df can be adjusted to any positive value).
```{r, eval = T, echo = T}
build_dist(type = "t-dist", df = 5)
```


You can also edit your image to show different shaded areas.
```{r, eval = T, echo = T}
# To allow for several graphs to be displayed.
par(mfrow = c(2,2))

build_dist(type = "normal", bound = -1)
build_dist(type = "normal", bound = c(-1,2), tail = "inner")
build_dist(type = "t-dist", bound = c(-1,0), tail = "two")
build_dist(type = "binomial",bound = c(6,12),
    tail = "outer", p = .5, trials = 20)
```

To add probably values turn display_prob = TRUE
```{r, eval = T, echo = T}
# To allow for several graphs to be displayed.
par(mfrow = c(2,2))

build_dist(type = "normal", bound = -1, display_prob = TRUE)
build_dist(type = "normal", bound = c(-1,2), tail = "inner", display_prob = TRUE)
build_dist(type = "t-dist", bound = c(-1,0), tail = "two", display_prob = TRUE)
build_dist(type = "binomial",bound = c(6,12),
    tail = "outer", p = .5, trials = 20, display_prob = TRUE)
```

## **build_qq()**

The purpose of this function is to allow users to build simple QQ plots in R.

### How to use:
This function only takes four arguments
1. data
2. vs_dist = "normal"
3. alpha = 1
4. beta = 1

vs_dist can be any of the following types: 
"normal","gamma","uniform","poisson","binomial","geometric","chi-squared"

### Examples:
```{r, eval = T, echo = T}
data <- rnorm(100,0,1)
par(mfrow = c(2,3))
build_QQ(data)
build_QQ(data, vs_dist = "gamma", alpha = 2, beta = 4)
build_QQ(data, vs_dist = "poisson", alpha = 10)
```


## **test()**

This is a rather large wrapper function that contains most tests run in AP Stats including:
1. One sample t-test on means called "t_test.one"
2. Two sample t-test on means called "t_test.two"
3. Paired t-test on means called "t_test.paired"
4. One sample z-test on proportions called "z_test.one"
5. Two sample z-test on proportions called "z-test.pooled"
6. Chi-Squared Goodness of Fit Test called "chi_squared.gof"
7. Chi-Squared Independence or Homogeneity tests called "chi_squared.ind"
8. One sample confidence Interval on means called "t_conf.one"
9. Two sample difference of means confidence interval called "t_conf.two"
10. One sample confidence Interval on proportions called "z_conf.one"
11. Two sample difference of proportions confidence interval called "z_conf.two"

This function contains a lot of arguments, many of which are not needed for any given test. Notable ones are below:
1. null = is used for the null hypothesis's measure. 
2. graph = TRUE will call build_dist() when applicable. 
3. expect_as_count should be set to TRUE if you have already calculated your expected table. The function will assume you are imputing expected proportions otherwise. 
4. row_totals should be set to TRUE if your expected and observed table (obs_table) have totals included. The function can handle totals if it knows it needs too. Otherwise you will still find calculations, but they will be very wrong. 

Note: Confidence Intervals can be found directly using find_conf() instead. Each test can also be found directly using the test name as a function. Such as:
```{r, eval = T, echo = T}
t_test.one(null = 10, x_bar = 11, sigma = 3, n = 25, tail = "left", graph = FALSE)
```


It is easiest to see what each test needs by typing in test("help")
```{r, eval = F, echo = T}
test("help")
```

### Examples
Here are some examples of using this function
```{r, eval = T, echo = T}
# One sample t=test
test(test = 'z_test.one', null = .4, p_hat = .6, n = 10, tail = "right", graph = TRUE)

```

```{r, eval = T, echo = T}
# Paired t-test
X <- c(10,11,12,13,14,15,16,17,18,19,20)
Y <- c(12,11,13,14,14,9,15,13,20,14,18)
test(test = 't_test.paired', null = 0, table_one = X, table_two = Y, tail = "two", graph = TRUE)
```

```{r, eval = T, echo = T}
# Chi-Squared test for independence on a 3X4 matrix
X <- matrix(rnorm(20,10,3), nrow = 4)
test(test = 'chi_squared.ind', obs_table = X, mat_totals = FALSE, graph = TRUE)
```

## **get_measures()**

get_measures can work well in two ways. It will allow you to input data sets you already have, or it will allow you to create a new data set. The function will then generate a large number of useful statistical measures. 

Arguments include:
1. X = NULL
2. input = FALSE
3. Y = NULL
4. regression = FALSE
5. outliers = FALSE
6. names = NULL

If you have data you would like to input, place that in X = data. This can be a vector of data, or a matrix. However, if you do not have a data set and need to type one in, leave X = NULL and set input = TRUE. This will prompt you to input your own data. You can name your data in the prompt window and it will be maintained. Note: sometimes the window will open but not pop up. Look at your dashboard on the bottom of your computer for a flashing icon. 

If you are using regression, you can input into either X, and Y, or use a matrix with two rows. Note though, that names will not work if you use X and Y as your inputs. 

The names argument will allow you to name your created data set for you. 

Outliers can be calculated using the "1.5 IQR" or "3sd Rule". The default is "1.5 IQR". Simply type outliers = "3sd Rule" if you would like to use that rule. 

### Examples

```{r, eval = T, echo = T}
# Single sample of data
data <- c(sample(1:100,15))
get_measures(X = data)
```

```{r, eval = T, echo = T}
#Matrix of two variables (It can be extended)
data <- matrix(sample(1:100, 50), ncol <- 25)
get_measures(X = data, outliers = TRUE, names = c("BOB","JILL"), regression = TRUE)

```

```{r, eval = F, echo = T}
#This will allow you to directly input your own data. 
get_measures(input=TRUE)

```



## **find_probs()**

find_probs does exactly that. Find probabilities on various distributions. It is the function that build_dist() uses to calculate its probabilities.

It can take a large number of arguments but needs very little to work. 

They include:
1. bound = NULL
2. type="normal"
3. tail="left"
4. mean = 0
5. sigma = 1
6. df = 1
7. prob = .5
8. trials = 10
9. inverse = FALSE

Type can be any of "normal", "t-dist", "chi-squared", "binomial".
Tail can be any of "left", "right", "inner", "outer" or "two", "left_not_equal", "right_not_equal". Note: "left_not_equal" and "left_not_equal" are intended for the "binomial" only. Also "out" and "two" will produce the same output.

Trials is designated for the number of trials for the "binomial" not trials of a simulation.

### Examples
```{r, eval=TRUE, echo = TRUE}
# Will assume Normal
find_probs(1.5)

# For multiple outputs for the Normal
find_probs(c(-3,-2,-1,0,1,2), tail = "left")

# Probability between two points of a chi-squared
find_probs(c(-1,3),type="chi-squared",tail="inner",df=4)

# Using a t-distribution
find_probs(c(-1,3),type="t-dist",tail="right",df=4)

 # To find the outside tailed probabilities
find_probs(c(2,6),type="binomial",tail="two")

# Finding inverses
find_probs(bound = .4, inverse = TRUE)
find_probs(bound = .8, mean = 10, sigma = 4, inverse = TRUE)
```


## *find_conf()*

This function uses only a few arguments. 
type can be any of "t_conf.one", "t_conf.two", "z_conf.one", "z_conf.two"
point is the point estimate
sigma = 0
level is the significance level. Being of any value between 0 and 1. 
n

### Example
```{r, eval = T, echo = T}
find_conf(type = "t_conf.one", point = 12, sigma = 3, level = c(.95), n = 20)
```

## *get_data* 

get_data() is a wrapper function that enables the user to input any data set of type .csv, .txt, .xls, and .xlsx. 

The function also includes several data files you can use. Use the following to see all them.
```{r, eval = F, echo = TRUE}
get_data(data_name = "help")
```

You can call any of the data sets using
```{r, eval = T, echo = TRUE}
get_data("potter.csv")
```
You can print the data by changing get_pdf = TRUE. Note thought that the function will then require you to input a desired save path.
```{r, eval = FALSE, echo = TRUE}
get_data("potter.csv", get_pdf = TRUE)
```

## *Included Simulations*

This packages includes a few simulation as well. They include:
1. Birthday_fun()
2. blackjack_bust_auto()
3. blackjack_bust()
4. qq_dem()
5. confidence_interval()
6. tanks()

### Specifics
#### confidence_intervals
The following can be modified mean, sigma, n, level, trials

#### blackjack_bust_auto()
trials can be adjusted to increase trial times.

#### Birthday_fun()
1. Iter = number of iterations
2. size = amount of people included in a room
3. plot = FALSE to suppress the graphics. 

```{r, eval = TRUE, echo = TRUE}
Birthday_fun(Iter = 20, size = c(5,10,20, 30))
```


