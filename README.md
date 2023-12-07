# APStatTools 

**Description** The reason for this function is to make R accessible for many of the tools and calculation used in AP Statistics and Undergraduate Statistics.

**Included Functions**
*build_dist()*  This will allow the user to build a number of distributions, shade in the area of interest, and display the probability of the shaded area. 
*build_qq()* This will allow the user to build a quick QQ plot that can be tested against a number of distributions. 
*find_conf()* This function contains code that can calculate simple Wald Confidence Intervals
*find_probs()* This function can find various probabilities based on a number of distributions.  
*get_data()* This is a wrapper function around many input methods R has to allow the user to easily and quickly input data into R for use. You will only need to know how to find your file's path. 
*get_measures()* This will allow the user to input data or data sets, and return a large list of statistical measures.
*test()* This can preform many of the statistical tests and confidence intervals used in AP Statistics. 

# The functions
## build.dist()

The purpose of this function is to allow users to build a number of common distributions, shade in regions that 
we may want to find proportions of, and display their probabilities. 

The function contains the "Normal", "Student's T", "Chi-Squared", "Binomial", "QQ", and "Regression" plots. 

It can calculate probabilities to the left, right, inbetween two points, or two tailed distributions. 

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

 "left", "right", "inner", "outer" or "two", "left_not_equal", "right_not_equal"
      Note: "left_not_equal","left_not_equal","inner_not_equal", and "two_not_equal" are intended for the "binomial" only.
      
*bound = c(value)* will designate where the boundaries of the shaded region will go. 
If this value is left as NULL, it will produce the distribution. If you need an interior or two-tailed boundry, you can set 
bound = c(left, right). 

### Examples

To build the Standard Normal use:
```{r, eval = T, echo = T}
build_dist()
```
![image](https://github.com/dusmith0/APStatTools/assets/128092158/952d129f-8761-40de-9a47-073669ce2fd9)

To build Student's t-distribution use: (Where df can be adjusted to any positive value).
```{r, eval = T, echo = T}
build_dist(type = "t-dist", df = 5)
```
![image](https://github.com/dusmith0/APStatTools/assets/128092158/ab99d2e3-6d80-4460-b692-6ccd00ae266e)

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






