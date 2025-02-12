#Step 1
n_dims <- sample(3:10, 1, replace = TRUE)
print(n_dims)

vec1 <- 1:n_dims^2
print(vec1)  

vec1_shuffled <- sample(vec1)
print(vec1_shuffled)

m1 <- matrix(data=vec1_shuffled, nrow=n_dims, ncol=n_dims)
print(m1)

tm1 <- t(m1)
print(tm1) #columns became row and rows became columns

sum_tm1_r1 <- sum(tm1[1,])
print(sum_tm1_r1)
mean_tm1_r1 <- mean(tm1[1,])
print(mean_tm1_r1)

sum_tm1_r6 <- sum(tm1[6,])
print(sum_tm1_r6)
mean_tm1_r6 <- mean(tm1[6,])
print(mean_tm1_r6)

eigen_tm1 <- eigen(tm1)
typeof(eigen_tm1$values) #=complex
typeof(eigen_m1$vectors) #=complex


#Step 2
my_matrix <- matrix(sample(1:100, 16), nrow=4, ncol=4)
print(my_matrix)

set_logical <- sample(1:20,100, replace = TRUE)
my_logical <- set_logical > 10
print(my_logical)

my_letters <- sample(letters[seq(from=1, to=26)])
print(my_letters)

my_list <- list(my_matrix[2,2], my_logical[2], my_letters[2])
print(my_list)

typeof(my_matrix[2,2])
typeof(my_logical[2])
typeof(my_letters[2])

vec2 <- c(my_matrix[2,2], my_logical[2], my_letters[2])
typeof(vec2)


#Step 3
df <- data.frame(my_unis=sample(1:10, 26, replace=TRUE), my_letters=sample(LETTERS[seq(from=1, to=26)]))
print(df)

df$my_unis <- df$my_unis[replace(df$my_unis, sample(1:26, 4), NA)]
print(df)

which(is.na(df$my_unis))

df <- df[order(df$my_letters),]
print(df)







