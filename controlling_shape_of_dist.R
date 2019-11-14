### Create a loglogistic data set

# install.packages("STAR")
# library(STAR)


install.packages("actuar")
library(actuar)


exp(dllogis(2, 3, 4, log = TRUE))
p <- (1:10)/10
# pllogis <- pllogis(qllogis(p, 2, 3), 2, 3)
pllogis(qllogis(p, shape = 2, rate = 3), 2, 3)
test1 <- pllogis(qllogis(p, shape = 2, rate = 3),shape = 2, rate = 3)
hist(test1)

## mean
mllogis(1, 2, 3)
## case with 1 - order/shape > 0
levllogis(10, 2, 3, order = 1)
## case with 1 - order/shape < 0
levllogis(10, 2/3, 3, order = 1)
###############################################################################################################################

p90 <- 5.0
p10 <- 1.0
scale = sqrt(p90 * p10)
shape = -2*log(3)/log(sqrt(p90 * p10)/ p90)
shape
test_rllogis <- rllogis(10000, shape = shape, scale = scale)
test_rllogis
hist(test_rllogis)
summary(test_rllogis)

test_rllogis <- rllogis(10000, shape = shape, scale = scale)
test_rllogis
hist(test_rllogis)
summary(test_rllogis)

test_normal <-rnorm(1000, 3.5, 0.1) 
hist(test_normal)
summary(test_normal)


######################################################################################################################################
##############   SHAPE ##################################
#the shape controls the shape of the distrbution
#smaller numbers will make the distribution skew to the left
#Ric values were making the data very skewed
test_rllogis6_10 <- rllogis(10000, shape = 6 , scale = 10, rate = 1)
hist(test_rllogis2)
summary(test_rllogis2)

test_rllogis5_10_1 <- rllogis(10000, shape = 5 , scale = 10, rate = 1)
hist(test_rllogis5_10_1)
summary(test_rllogis5_10_1)

##############   SCALE  ##################################
#controls the varaibility of the data, sharp curves with small tails or flats curves with long tails
# smaller values give you flatter curves
test_rllogis5_5_1 <- rllogis(10000, shape = 5 , scale = 5, rate = 1)
hist(test_rllogis5_5_1)
summary(test_rllogis5_5_1)


##########   RATE   ######################################
# Not really sure what this does higher values seem to make tails longers but very skinny at the end??
# perhaps not use it?
test_rllogis5_5_100 <- rllogis(10000, shape = 5 , scale = 5, rate = 100)
hist(test_rllogis5_5_100)
summary(test_rllogis5_5_100)

#### How to shift the distrbution  to left or right?
# multiple the distribution by a value to slide it right - needs to be positive (negative for left)
test_rllogis5_5_1_0.1 <- (rllogis(10000, shape = 5 , scale = 5, rate = 1))*0.1
hist(test_rllogis5_5_1_0.1)
summary(test_rllogis5_5_1_0.1)

test_rllogis5_5_1_10 <- (rllogis(10000, shape = 5 , scale = 5, rate = 1))*10
hist(test_rllogis5_5_1_10)
summary(test_rllogis5_5_1_10)

##### how do I get the range of values I want 
n = 1:1000
n
test_rllogis_n_values <- (rllogis(n, shape = 5 , scale = 5, rate = 1))*10
hist(test_rllogis_n_values)
summary(test_rllogis_n_values)

## this works I am creating 16000 data points with a min value of 2 and max of 5 stepping through at 0.2
n_2_5 <- rep(seq(2,5, by = 0.2),1000)

head(n_2_5)
summary(n_2_5)
str(n_2_5)
# I am using this as input for my distrbution curve
test_rllogis_n_2_5values <- (rllogis(n_2_5, shape = 5 , scale = 5))
hist(test_rllogis_n_2_5values)
summary(test_rllogis_n_2_5values)

#then I can multiple the whole dirstubution to side the yield in one direction....
