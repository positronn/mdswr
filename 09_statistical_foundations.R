# 9 Statistical Foundations
# Statistical methods quantify patterns and their strength.

# 9.1.1 Example: Setting travel policy by sampling from the population
# Suppose you were asked to help develop a travel policy for business
# travelers based in New York City. Imagine that the traveler has a meeting
# in San Francisco (airport code SFO) at a specified time . The policy
# to be formulated will say how much earlier than an acceptable flight
# should arrive in order to avoid being late to the meeting due to a flight delay.

library(tidyverse)
library(mdsr)
library(nycflights13)

SF <- flights |> 
    filter(dest == 'SFO',
           !is.na(arr_delay))
SF

# We’re going to work with just a sample from this population. 
set.seed(101)
sf_25 <- SF |> 
    slice_sample(n = 25)

sf_25
# A simple (but näive) way to set the policy is to look for the longest flight delay and insist that travel be arranged to deal with this delay.
sf_25 |> 
    skim(arr_delay)

# The maximum delay is 103 minutes, about 2 hours. So, should our travel policy be that the traveler should plan on arriving in SFO about 2 hours ahead? In our example world, we can look at the complete set of flights to see what was the actual worst delay in 2013.
SF |> 
    skim(arr_delay)
# Notice that the results from the sample are different from the results
# for the population. In the population, the longest delay was 1,007
# minutes—almost 17 hours. This suggests that to avoid missing a
# meeting, you should travel the day before the meeting. Safe enough, 
# but then
#   an extra travel day is expensive in terms of lodging, meals, and the traveler’s time;
#   even at that, there’s no guarantee that there will never be a delay of more than 1,007 minutes.

# A sensible travel policy will trade off small probabilities of being late against the savings in cost and traveler’s time. For instance, you might judge it acceptable to be late just 2% of the time—a 98% chance of being on time. 
sf_25 |> 
    summarize(q98 = quantile(arr_delay, p = 0.98))
# A delay of 68 minutes is more than an hour. The calculation is easy, but how good is the answer? This is not a question about whether the percentile was calculated properly—that will always be the case for any competent data scientist. The question is really along these lines: Suppose we used a 90-minute travel policy. How well would that have worked in achieving our intention to be late for meetings only 2% of the time?
# With the population data in hand, it’s easy to answer this question. 
SF |> 
    group_by(arr_delay < 90 ) |> 
    count() |> 
    mutate(pct = n / nrow(SF))
# The 90-minute policy would miss its mark 5% of the time, much worse than we intended. To correctly hit the mark 2% of the time, we will want to increase the policy from 90 minutes to what value?
SF |> 
    summarize(q98 = quantile(arr_delay, p = 0.98))
# It should have been about 150 minutes.


# 9.2 Sample statistics
# Ultimately we need to figure out the reliability of a sample statistic from the sample itself.
n <- 25
SF |> 
    slice_sample(n = n) |> 
    summarize(mean_arr_delay = mean(arr_delay))

SF |>
    slice_sample(n = n) |>
    summarize(mean_arr_delay = mean(arr_delay))
# Perhaps it would be better to run many trials (though each one would require considerable effort in the real world)
n <- 25
trials <- 500
sf_25_means <- 1:trials |> 
    map(~ SF |> 
            slice_sample(n = n) |> 
            summarize(mean_arr_delay = mean(arr_delay))) |> 
    list_rbind() |> 
    mutate(n = n)

head(sf_25_means)

sf_25_means |> 
    skim(mean_arr_delay)

# The sampling distribution is the collection of the sample statistic
# from all of the trials. We carried out 1000 trials here, but the exact
# number of trials is not important so long as it is large.

sf_25_means |> 
    summarize(
        x_bar = mean(mean_arr_delay),
        se = sd(mean_arr_delay)
    ) |> 
    mutate(
        ci_lower = x_bar - 2 * se,
        ci_upper = x_bar + 2 * se
    )

# or using t-test
sf_25_means |> 
    pull(mean_arr_delay) |> 
    t.test()
# This vocabulary can be very confusing at first. Remember that
# “standard error” and “confidence interval” always refer to the
# sampling distribution, not to the population and not to a single
# sample. The standard error and confidence intervals are two
# different, but closely related, forms for describing the reliability
# of the calculated sample statistic.

n <- 100
trials <- 500
sf_100_means <- 1:trials |>
    map_dfr(
        ~ SF |>
            slice_sample(n = n) |>
            summarize(mean_arr_delay = mean(arr_delay))
    ) |>
    mutate(n = n)

n <- 200
sf_200_means <- 1:trials |>
    map_dfr(
        ~ SF |>
            slice_sample(n = n) |>
            summarize(mean_arr_delay = mean(arr_delay))
    ) |>
    mutate(n = n)

sf_25_means |> 
    bind_rows(sf_100_means, sf_200_means) |> 
    ggplot(mapping = aes(x = mean_arr_delay)) +
    geom_histogram(bins = 30) +
    facet_grid(~ n) +
    xlab('Sample mean')

# Both sampling distributions are centered at the same value.
# A larger sample size produces a standard error that is smaller. That is, a larger sample size is more reliable than a smaller sample size.
# For large sample sizes, the shape of the sampling distribution tends to bell-shaped. In a bit of archaic terminology, this shape is often called the normal distribution. Indeed, the distribution arises very frequently in statistics, but there is nothing abnormal about any other distribution shape. 
