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



# 9.3 The bootstrap
# In the previous examples, we had access to the population data and so we could find the sampling distribution by repeatedly sampling from the population. In practice, however, we have only one sample and not the entire population. The bootstrap is a statistical method that allows us to approximate the sampling distribution even without access to the population.
# The logical leap involved in the bootstrap is to think of our sample itself as if it were the population. Just as in the previous examples we drew many samples from the population, now we will draw many new samples from our original sample. This process is called resampling: drawing a new sample from an existing sample.
three_flights <- SF |>
    slice_sample(n = 3, replace = FALSE) |>
    select(year, month, day, dep_time)
three_flights

three_flights |>
    slice_sample(n = 3, replace = TRUE)

three_flights |>
    slice_sample(n = 3, replace = TRUE)


# Let’s use bootstrapping to estimate the reliability of the mean arrival time calculated on a sample of size 200. (Ordinarily this is all we get to observe about the population.)
n <- 200
orig_sample <- SF |> 
    slice_sample(n = n,
                 replace = FALSE)
# Now, with this sample in hand, we can draw a resample (of that sample size) and calculate the mean arrival delay.
orig_sample |> 
    slice_sample(n = n, replace = TRUE) |> 
    summarize(mean_arr_delay = mean(arr_delay))

# By repeating this process many times, we’ll be able to see how much variation there is from sample to sample: 
sf_200_bs <- 1:trials |> 
    map(
        ~ orig_sample |> 
            slice_sample(n = n, replace = TRUE) |> 
            summarize(mean_arr_delay = mean(arr_delay))
    ) |> 
    list_rbind() |> 
    mutate(n = n)

sf_200_bs |> 
    skim(mean_arr_delay)
# Ordinarily, we wouldn’t be able to check this result. But because we have access to the population data in this example, we can. Let’s compare our bootstrap estimate to a set of (hypothetical) samples of size from the original SF flights (the population).
# size n = 200
sf_200_pop <- 1:trials |> 
    map(
        ~ SF |> 
            slice_sample(n = n, replace = TRUE) |> 
            summarize(mean_arr_delay = mean(arr_delay))
    ) |> 
    list_rbind() |> 
    mutate(n = n)

sf_200_pop |> 
    skim(mean_arr_delay)
# Notice that the population was not used in the bootstrap (sf_200_bs), just the original sample. What’s remarkable here is that the standard error calculated using the bootstrap (3.1 minutes) is a reasonable approximation to the standard error of the sampling distribution calculated by taking repeated samples from the population (3.3 minutes).
# The distribution of values in the bootstrap trials is called the bootstrap distribution. It’s not exactly the same as the sampling distribution, but for moderate to large sample sizes and sufficient number of bootstraps it has been proven to approximate those aspects of the sampling distribution that we care most about, such as the standard error and quantiles

# 9.3.1 Example: Setting travel policy
orig_sample |>
    summarize(q98 = quantile(arr_delay, p = 0.98))

# We can check the reliability of that estimate using bootstrapping. 
n <- nrow(orig_sample)
sf_200_bs <- 1:trials |>
    map(
        ~ orig_sample |>
            slice_sample(n = n, replace = TRUE) |>
            summarize(q98 = quantile(arr_delay, p = 0.98))
    ) |> 
    list_rbind() 

sf_200_bs |>
    skim(q98)
# The bootstrapped standard error is about 48 minutes.
# The corresponding 95% confidence interval is 152 +- 96  minutes.
# A policy based on this would be practically a shot in the dark: unlikely to hit the target.

# One way to fix things might be to collect more data, hoping to get a more reliable estimate of the percentile. Imagine that we could do the work to generate a sample with cases.
set.seed(1001)
n_large <- 10000
sf_10000_bs <- SF |> 
    slice_sample(n = n_large, replace = FALSE)

sf_200_bs <- 1:trials |> 
    map(
        ~ sf_10000_bs |> 
            slice_sample(n = n_large, replace = TRUE) |> 
            summarize(q98 = quantile(arr_delay, p = 0.98))
    ) |> 
    list_rbind()

sf_200_bs |> 
    skim(q98)
# The standard deviation is much narrower, 154 +- 8 minutes.
# Having more data makes it easier to better refine estimates,
# particularly in the tails.


# 9.4 Outliers
