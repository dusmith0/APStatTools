##---------------------------------------------------------------------------##
## The following contains tests to run on my test() function ##


## simple test
null <- 10
x_bar <- 9
sd <- 2
n <- 15
tail="left"

t.test.one(null,x_bar,sd,n,tail="left")

## two sample
null <- 0
x_bar <- c(9,10)
sd <- c(2,3)
n <- c(15,16)
tail="left"

t.test.two(null,x_bar,sd,n,tail="right")

## two sample proportion
null <- 0
p_hat <- c(.8,.6)
n <- c(200,225)
tail = "left"

z.test.pooled(p_hat,n,tail="left")


## Chi-Squared GOF test
null_table <- blood <- c(465,394,96,45)
expected_table <- expected <- c(.45,.40,.1,.05)

chi_squared_gof(null_table, expected_table)


## Paired T-test.
table_one <- males <- c(1902,1470,382,778,423,568,1375,682)
table_two <- females <- c(221,633,200,312,629,435,2098,283)

t_test_paired(null = 0,table_one,table_two,tail="two")

##Chi-Squared for independence tests
mat <- c(12,12,13,14,15,15,16,17,60)
null_table <- matrix(mat,ncol=3)
chi_squared_independence(null_table)

## Get data test
data_list <- list("Moneyball","Movies")
if(x == 2){
  bob <- "good"
}else{
  bob <- "bad"
}



