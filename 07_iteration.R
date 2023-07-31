# 07 Iteration.R

# 7.1 Vectorized operations
library(tidyverse)
library(mdsr)
library(Lahman)
names(Teams)


str(Teams)
glimpse(Teams)

# compute averages using loop
averages <- NULL
for (i in 15:40) {
    averages[i - 14] <- mean(Teams[, i], na.rm = TRUE)
}
names(averages) <- names(Teams[15:40])
averages
# R programmers prefer to solve this type of problem by applying an operation to each element in a vector. This often requires only one line of code, with no appeal to indices.


# It is important to understand that the fundamental architecture of R is based
# on vectors. That is, in contrast to general-purpose programming languages like
# C++ or Python that distinguish between single items—like strings and integers—and
# arrays of those items, in R a “string” is just a character vector of length 1.
# There is no special kind of atomic object. Thus, if you assign a single “string”
# to an object, R still stores it as a vector. 
a <- 'a string'
b <- 12
is.vector(a)
is.vector(b)
length(a)
length(b)
# As a consequence of this construction, R is highly optimized for vectorized
# operations (see Appendix B for more detailed information about R internals).
# Loops, by their nature, do not take advantage of this optimization. Thus,
# R provides several tools for performing loop-like operations without actually
# writing a loop. 
# Many functions in R are vectorized. This means that they will perform an operation on every element of a vector by default. 
exp(1:10)
length(exp(1:10))
# Note that vectorized functions like exp() take a vector as an input, and return a vector of the same length as an output.

# This is importantly different behavior than so-called summary functions, which take a vector as an input, and return a single value.
mean(1:10)
length(mean(1:10))


# Other functions in R are not vectorized. They may assume an input that is a vector of length one, and fail or exhibit strange behavior if given a longer vector. For example, if() throws a warning if given a vector of length more than one.
if (c(TRUE, FALSE)) {
    cat('This is a book.')
}
# As you get more comfortable with R, you will develop intuition about which functions
# are vectorized. If a function is vectorized, you should make use of that
# fact and not iterate over it. The code below shows that computing the
# exponential of the first 10,000 integers by appealing to exp() as a
# vectorized function is much, much faster than using map_dbl() to
# iterate over the same vector. The results are identical.
x <- 1:1e7
bench::mark(
    exp(x),
    map_dbl(x, exp)
)

x <- 1:1e7
bench::mark(
    sin(x),
    map_dbl(x, sin)
)



# 7.2 Using across() with dplyr functions
