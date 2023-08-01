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
# The mutate() and summarize() can take advantage of an adverb called across() that applies operations programmatically.
Teams |> 
    summarize(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)))
# The across() adverb allows us to specify the set of variables that summarize() includes in different ways.
Teams |> 
    summarize(across(c(yearID, R:SF, BPF), \(x) mean(x, na.rm = TRUE)))



# 7.3 The map() family of functions
Teams |> 
    select(15:40) |> 
    map_dbl(mean, na.rm = TRUE)

Teams |> 
    select(15:40) |> 
    map_dbl(\(x) mean(x, na.rm = TRUE))

Teams |> 
    select(G) |> 
    map_dbl(mean, na.rm = TRUE)

Teams |> 
    select(G) |> 
    map_dbl(\(x) mean(x, na.rm = TRUE))


# 7.4 Iterating over a one-dimensional vector
# 7.4.1 Iterating a known function
angels <- Teams |> 
    filter(franchID == 'ANA') |> 
    group_by(teamID, name) |> 
    summarize(began = first(yearID),
              ended = last(yearID)) |> 
    arrange(began)
angels
# Now, suppose we want to find the length, in number of characters, of each of those team names. We could check each one manually using the function nchar():
angels_names <- angels |> 
    pull(name)

nchar(angels_names[1])
nchar(angels_names[2])
nchar(angels_names[3])
nchar(angels_names[4])

map_int(angels_names, nchar)

# The key difference between map_int() and map() is that the former will
# always return an integer vector, whereas the latter will always return
# a list. Recall that the main difference between lists and data.frames
# is that the elements (columns) of a data.frame have to have the same length,
# whereas the elements of a list are arbitrary. So while map() is
# more versatile, we usually find map_int() or one of the other variants
# to be more convenient when appropriate.

# However, the choice of the nchar() function was a bit silly, because nchar() is already vectorized. Thus, we can use it directly!
nchar(angels_names)


# 7.4.2 Iterating an arbitrary function
# One of the most powerful uses of iteration is that you can apply any function, including a function that you have defined 
top5 <- function(data, team_name) {
    data |> 
        filter(name == team_name) |> 
        select(teamID, yearID, W, L, name) |> 
        arrange(desc(W)) |> 
        head(n = 5)
}

angels_names |> 
    map(\(x) top5(data = Teams, team_name = x))

angels_names |> 
    map_df(\(x) top5(data = Teams, team_name = x))

# Alternatively, we can collect the results into a single data frame by using the map_dfr() function, which combines the data frames by row.
angels_names |> 
    map_dfr(top5, data = Teams) |> 
    group_by(teamID, name) |> 
    summarize(N = n(), 
              mean_wins = mean(W)) |> 
    arrange(desc(mean_wins))



# 7.5 Iteration over subgroups
# The group_modify() function in purrr allows you to apply an arbitrary function
# that returns a data frame to the groups of a data frame. That is, you will
# first define a grouping using the group_by() function, and then apply a
# function to each of those groups. Note that this is similar to map_dfr(),
# in that you are mapping a function that returns a data frame over a collection
# of values, and returning a data frame. But whereas the values used in map_dfr()
# are individual elements of a vector, in group_modify() they are groups defined
# on a data frame.

# 7.5.1 Example: Expected winning percentage
exp_wpct <- function(x) {
    return(1 / (1 + (1 / x) ^ 2))
}

TeamRuns <- Teams |> 
    filter(yearID >= 1954) |> 
    rename(RS = R) |> 
    mutate(WPct = W / (W + L),
           run_ratio = RS / RA) |> 
    select(yearID, teamID, lgID, WPct, run_ratio)

TeamRuns |> 
    ggplot(mapping = aes(x = run_ratio, y = WPct)) +
    geom_vline(xintercept = 1, color = 'darkgray', linetype = 2) +
    geom_hline(yintercept = 0.5, color = 'darkgray', linetype = 2) +
    geom_point(alpha = 0.2) +
    stat_function(fun = exp_wpct, linewidth = 1, color = 'blue') +
    xlab('Ratio of Runs Scored to Runs Allowed') +
    ylab('Winning Percentage')


TeamRuns |>
    nls(
        formula = WPct ~ 1/(1 + (1/run_ratio)^k), 
        start = list(k = 2)
    ) |>
    coef()

fit_k <- function(x) {
    mod <- nls(
        formula = WPct ~ 1/(1 + (1/run_ratio)^k), 
        data = x,
        start = list(k = 2)
    )
    return(tibble(k = coef(mod), n = nrow(x)))
}
fit_k(TeamRuns)

# Finally, we compute the decade for each year using mutate(), define the group using group_by(), and apply fit_k() to those decades. The use of the ~ tells R to interpret the expression in parentheses as a formula, rather than the name of a function. The .x is a placeholder for the data frame for a particular decade.
TeamRuns |> 
    mutate(decade = yearID %/% 10 * 10) |> 
    group_by(decade) |> 
    group_modify(~fit_k(.x))


# 7.5.2 Example: Annual leaders
hr_leader <- function(x) {
    # x is a subset of Teams for a single year and leage
    x |> 
        select(teamID, HR) |> 
        arrange(desc(HR)) |> 
        head(1)
}

Teams |> 
    filter(yearID == 1961 &
           lgID == 'AL') |> 
    hr_leader()

# We can use group_modify() to quickly find all the teams that led their league
# in home runs. Here, we employ the .keep argument so that the grouping
# variables appear in the computation.
hr_leaders <- Teams |> 
    group_by(yearID, lgID) |> 
    group_modify(~hr_leader(.x), .keep = TRUE)
tail(hr_leaders, 4)

# In this manner, we can compute the average number of home runs hit in a season by the team that hit the most.
hr_leaders |> 
    group_by(lgID) |> 
    summarize(mean_hr = mean(HR))


# We restrict our attention to the years since 1916, during which only the AL and NL leagues have existed.
hr_leaders |> 
    filter(yearID >= 1916) |> 
    group_by(lgID) |> 
    summarize(mean_hr = mean(HR))


# 7.6 Simulation
k_actual <- TeamRuns |> 
    group_by(yearID) |> 
    group_modify(~fit_k(.x))

TeamRuns |> 
    group_by(yearID) |> 
    group_modify(~count(.x))

k_actual |> 
    ungroup() |> 
    skim(k)

k_actual |> 
    ggplot(mapping = aes(x = k)) +
    geom_density() +
    xlab('Best fit epxonent for a single season')

# Since we only have 69 samples, we might obtain a better understanding of the sampling distribution of the mean by resampling—sampling with replacement—from these 69 values. 
# A simple way to do this is by mapping a sampling expression over an index of values. That is, we define n to be the number of iterations we want to perform, write an expression to compute the mean of a single resample, and then use map_dbl() to perform the iterations.
n <- 1e4
bootstrap <- 1:n |> 
    map_dbl(
        ~k_actual |> 
            pull(k) |> 
            sample(replace = TRUE) |> 
            mean()
    )

civals <- bootstrap |> 
    quantile(probs = c(0.025, 0.975))
civals


ggplot(data = enframe(bootstrap, value = "k"), aes(x = k)) + 
    geom_density() + 
    xlab("Distribution of resampled means") + 
    geom_vline(
        data = enframe(civals), aes(xintercept = value), 
        color = "red", linetype = 3
    )


# 7.7 Extended example: Factors associated with BMI
library(NHANES)

ggplot(NHANES, aes(x = Age, y = BMI)) +
    geom_point() + 
    geom_smooth()

# How can we programmatically produce an analogous image for all of the variables in NHANES? First, we’ll write a function that takes the name of a variable as an input, and returns the plot. Second, we’ll define a set of variables, and use map() to iterate our function over that list.
bmi_plot <- function(.data, x_var) {
    ggplot(.data, aes(y = BMI)) +
        aes_string(x = x_var) +
        geom_jitter(alpha = 0.3) +
        geom_smooth() +
        labs(
            title = paste('BMI by', x_var),
            subtitle = 'NHANES',
            caption = 'US National Center for Health Statistics (NCHS)'
        )
}
# The use of the aes_string() function is necessary for ggplot2 to understand
# that we want to bind the x aesthetic to the variable whose name is
# stored in the x_var object, and not a variable that is named x_var.



# We can then call our function on a specific variable.
bmi_plot(NHANES, "Age")

# Or, we can specify a set of variables and then map() over that set. Since map() always returns a list, and a list of plots is not that useful, we use the wrap_plots() function from the patchwork package to combine the resulting list of plots into one image.
c("Age", "HHIncomeMid", "PhysActiveDays", 
  "TVHrsDay", "AlcoholDay", "Pulse") |>
    map(bmi_plot, .data = NHANES) |>
    patchwork::wrap_plots(ncol = 2)
