##---------------------------------------------------------------------------##
## The following contains tests to run on my test() function ##


## simple test
null <- 10
x_bar <- 9
sd <- 2
n <- 15
tail="left"

z.test.one(null,x_bar,sd,n,tail="left")

## two sample
null <- 0
x_bar <- c(9,10)
sd <- c(2,3)
n <- c(15,16)
tail="left"

z.test.two(null,x_bar,sd,n,tail="right")
