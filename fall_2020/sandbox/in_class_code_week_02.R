
# Experimenting with build matrices from vectors:

# Think about:

# How does R populate the matrix with the data from the vector?
# What do the nrow, ncol, and byrow arguments do?

my_vec = 1:3
my_vec = c(1, 2, 3)

mat_1 = matrix(my_vec)
mat_1 = matrix(my_vec, byrow = TRUE)
mat_1 = matrix(my_vec, nrow = 1, ncol = 4, byrow = TRUE)

mat_2 = matrix(my_vec, nrow = 3, ncol = 4, byrow = FALSE)
mat_3 = matrix(my_vec, nrow = 3, ncol = 4, byrow = TRUE)
mat_3 = matrix(my_vec, nrow = 7, ncol = 5, byrow = TRUE)

mat_2
mat_3



my_vec = 1:1000
my_vec
mat_3 = matrix(my_vec, nrow = 7, ncol = 5, byrow = TRUE)
mat_3




