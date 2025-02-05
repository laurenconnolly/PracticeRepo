#Step 1
x <- 1.1
a <- 2.2
b <- 3.3
z <- c(x^a^b, (x^a)^b, 3*x^3 + 2*x^2 + 1)
print(z)

#Step 2
vec_2a <- c(seq(1:8), seq(from=7, to=1))
print(vec_2a)

vec_2b <- rep(1:5, times=1:5)
print(vec_2b)

vec_2c <- rep(1:5, times=5:1)
print(vec_2c)

#Step 3
xy_coordinates <- runif(2)
x_cor <- xy_coordinates[1]
y_cor <- xy_coordinates[2]

r_cor <- sqrt(x_cor^2 + y_cor^2)
print(r_cor)

theta_cor <- atan(y_cor/x_cor) 
print(theta_cor)

#Step 4
queue <- c("sheep", "fox", "owl", "ant")
print(queue) #starting queue

queue <- c(queue, "serpent")
print(queue) #a

queue <- queue[2:5]
print(queue) #b

queue <- c("donkey", queue)
print(queue) #c

queue <- queue[1:4]
print(queue) #d

queue <- c(queue[1:2], queue[4])
print(queue) #e

queue <- c(queue[1:2], "aphid", queue[3])
print(queue) #f

which(queue=="aphid") #g

#Step 5
s <- seq(1:100)
s2 <- which(s%%2 != 0 & s%%3 != 0  & s%%7 != 0)
print(s2)

