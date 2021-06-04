# Vector Creation with Variations
# Last updated 08.27.2018 @ 9:45am

# ----- Numeric Vectors -----

# Simple text vector

people <- c("Bob", "Mary", "Sue")
print(people)
str(people)
class(people)

# Simple numeric vector

height <- c(73, 62, 69)
print(height)
str(height)
class(height)

# Simple logic vector

logvec <- c(FALSE, TRUE, TRUE)
print(logvec)
logvec2 <- c(F, T, T)
print(logvec2)
sum(logvec)

# Name the elements of height using people

names(height) <- people
print(height)
names(height)

# Vector of the integers 1 through 10 using c

vector_1a <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
print(vector_1a)
class(vector_1a)

# We can make a sequence of integers using :

vector_1b <- 1:10
print(vector_1b)
class(vector_1b)

# We can also use the seq() function
# The default step size in seq() is 1
# We can abbreviate/omit some arguments
# Use ?seq to get help with the syntax

?seq

vector_1c <- seq(from = 1, to = 10, by = 1)
print(vector_1c)
class(vector_1c)

vector_1d <- seq(1, 10)
print(vector_1d)
class(vector_1d)

# integer vector created using a loop
# unnecessary for such a simple vector
# however, pay attention to structure

vector_1e <- integer(10)
for (i in 1:10) {
  vector_1e[i] <- i
}
print(vector_1e)

# numeric vector created using a loop
# unnecessary for such a simple vector
# however, pay attention to structure

vector_1f <- c(1, rep(0, 9))
for (i in 2:10) {
  vector_1f[i] <- vector_1f[i-1] + 1
}
print(vector_1f)

# numeric vector created using a loop
# prints out the vector at each step

vector_1g <- c(1, rep(0, 9))
for (i in 2:10) {
  vector_1g[i] <- vector_1g[i-1] + 1
  print(vector_1g)
}

# numeric vector created using a loop
# print vector at each step, fancier!

vector_1h <- c(1, rep(0, 9))
for (i in 2:10) {
  vector_1h[i] <- vector_1h[i-1] + 1
  print(paste("At the end of Loop ", i, " the vector is (", 
              vector_1h[1], ", ", 
              vector_1h[2], ", ", 
              vector_1h[3], ", ",
              vector_1h[4], ", ",
              vector_1h[5], ", ",
              vector_1h[6], ", ",
              vector_1h[7], ", ",
              vector_1h[8], ", ",
              vector_1h[9], ", ",
              vector_1h[10], ").",
              sep=""), quote=F)
}

# create empty vectors of different types

character(3)
integer(3)
numeric(3)
logical(3)
complex(3)

n <- 5
character(n)
integer(n)
numeric(n)
logical(n)
complex(n)

rep(1:3, 5)
rep(1:5, each = 5)

