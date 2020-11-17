# Introductory 1

# First example

1 + 1

75/8

# Now, this is an object
number = 75/8

number <- 75/8

ls()

number
print(number)

show(number)

View(number)

# Functions like the ones above "do stuff" to objects

?ls


# Objects in R

# Vectors
# Matrices
# Containers
# # Dataframes
# # Lists

# R is vectorized
# This means no scalars
# Most basic unit of info is the atomic vector
# numeric, strings (character), logicals, many, many others

str(number)

numbers <- c(1, 2, 3, 4)
Numbers <- c("one", "two", "three")

numbers

numbers[3]

the_one_i_want = Numbers[3]
the_one_i_want

numbers[numbers > 2]
numbers[numbers == 2]

numbers[numbers != 2 & numbers >= 0]

data(iris)

View(iris)



