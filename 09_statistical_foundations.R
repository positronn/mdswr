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
# One place where more data is helpful is in identifying unusual or extreme events: outliers. Suppose we consider any flight delayed by 7 hours (420 minutes) or more as an extreme event (see Section 15.5). While an arbitrary choice, 420 minutes may be valuable as a marker for seriously delayed flights.
SF |> 
    filter(arr_delay >= 420) |> 
    select(month, day, dep_delay, arr_delay, carrier)

# Most of the very long delays (five of seven) were in July, and Virgin America (VX) is the most frequent offender. Immediately, this suggests one possible route for improving the outcome of the business travel policy we have been asked to develop. We could tell people to arrive extra early in July and to avoid VX.
# But let’s not rush into this. The outliers themselves may be misleading. These outliers account for a tiny fraction of the flights into San Francisco from New York in 2013. That’s a small component of our goal of having a failure rate of 2% in getting to meetings on time. And there was an even more extremely rare event at SFO in July 2013: the crash-landing of Asiana Airlines flight 214. We might remove these points to get a better sense of the main part of the distribution.
SF |> 
    filter(arr_delay < 420) |> 
    ggplot(mapping = aes(x = arr_delay)) +
    geom_histogram(binwidth = 15) +
    labs(x = 'Arrival delay (in minutes)')

# Note that the large majority of flights arrive without any delay or a delay of less than 60 minutes. Might we be able to identify patterns that can presage when the longer delays are likely to occur? The outliers suggested that month or carrier may be linked to long delays. Let’s see how that plays out with the large majority of data. 
SF |> 
    mutate(long_delay = arr_delay > 60) |> 
    group_by(month, long_delay) |> 
    count() |> 
    pivot_wider(names_from = month, values_from = n) |> 
    tibble()

# We see that June and July (months 6 and 7) are problem months.
SF |> 
    mutate(long_delay = arr_delay > 60) |> 
    group_by(carrier, long_delay) |> 
    count() |> 
    pivot_wider(names_from = carrier, values_from = n) |> 
    tibble()

# Delta Airlines (DL) has reasonable performance. These two simple analyses hint at a policy that might advise travelers to plan to arrive extra early in June and July and to consider Delta as an airline for travel to SFO (see Section 15.5 for more discussion of which airlines seem to have fewer delays in general).

# 9.5 Statistical models: Explaining variation
# In the previous section, we used month of the year and airline to narrow down the situations in which the risk of an unacceptable flight delay is large. Another way to think about this is that we are explaining part of the variation in arrival delay from flight to flight.
# Statistical modeling provides a way to relate variables to one another. Doing so helps us better understand the system we are studying.
#  To illustrate modeling, let’s consider another question from the airline delays data set: What impact, if any, does scheduled time of departure have on expected flight delay? Many people think that earlier flights are less likely to be delayed, since flight delays tend to cascade over the course of the day. Is this theory supported by the data?
# We first begin by considering time of day. In the nycflights13 package, the flights data frame has a variable (hour) that specifies the scheduled hour of departure.
SF |> 
    group_by(hour) |> 
    count() |> 
    pivot_wider(names_from = hour, values_from = n) |> 
    tibble()

SF |> 
    group_by(hour) |> 
    count() |> 
    ggplot(mapping = aes(x = factor(hour), y = n)) +
    geom_bar(stat = 'identity') +
    xlab('Hour') +
    ylab('Number of flights')

SF |> 
    filter(arr_delay > 60) |> 
    group_by(hour) |> 
    count() |> 
    ggplot(mapping = aes(x = factor(hour), y = n)) +
    geom_bar(stat = 'identity') +
    xlab('Scheduled hour of departure') +
    ylab('Number of flights with delay') +
    ggtitle('Delays > 60 mins by Hour')


# Let’s examine how the arrival delay depends on the hour. We’ll do this in two ways: first using standard box-and-whisker plots to show the distribution of arrival delays; second with a kind of statistical model called a linear model that lets us track the mean arrival delay over the course of the day. 
SF |> 
    ggplot(aes(x = hour, y = arr_delay)) +
    geom_boxplot(alpha = 0.1, aes(group = hour)) +
    geom_smooth(method = 'lm', formula = y ~ x) +
    geom_smooth(method = 'lm', formula = y ~ x + I(x**2), color = 'darkred') +
    xlab('Scheduled hour of departure') +
    ylab("Arrival delay (minutes)") + 
    coord_cartesian(ylim = c(-30, 120)) 

# creating a model
mod1 <- lm(arr_delay ~ hour, data = SF)
broom::tidy(mod1)

mod2 <- lm(arr_delay ~ hour + I(hour ** 2), data = SF)
broom::tidy(mod2)


# Can we do better? What additional factors might help to explain flight delays? Let’s look at departure airport, carrier (airline), month of the year, and day of the week. Some wrangling will let us extract the day of the week (dow) from the year, month, and day of month. We’ll also create a variable season that summarizes what we already know about the month: that June and July are the months with long delays. These will be used as explanatory variables to account for the response variable: arrival delay.
library(lubridate)
SF <- SF |> 
    mutate(day = as_date(time_hour),
           dow = as.character(wday(day, label = TRUE)),
           season = if_else(month %in% 6:7, 'summer', 'other month'))

# Now we can build a model that includes variables we want to use to explain arrival delay.
mod3 <- lm(arr_delay ~ hour + origin + carrier + season + dow, data = SF)
broom::tidy(mod3)

# The numbers in the “estimate” column tell us that we should add 4.1 minutes to the average delay if departing from JFK (instead of EWR, also known as Newark, which is the reference group). Delta has a better average delay than the other carriers. Delays are on average longer in June and July (by 25 minutes), and on Sundays (by 5 minutes). Recall that the Aviana crash was in July.
# The model also indicates that Sundays are associated with roughly 5 minutes of additional delays; Saturdays are 6 minutes less delayed on average. (Each of the days of the week is being compared to Friday, chosen as the reference group because it comes first alphabetically.) The standard errors tell us the precision of these estimates; the p-values describe whether the individual patterns are consistent with what might be expected to occur by accident even if there were no systemic association between the variables.
# In this example, we’ve used lm() to construct what are called linear models. Linear models describe how the mean of the response variable varies with the explanatory variables. They are the most widely used statistical modeling technique, but there are others. In particular, since our original motivation was to set a policy about business travel, we might want a modeling technique that lets us look at another question: What is the probability that a flight will be, say, greater than 100 minutes late? Without going into detail, we’ll mention that a technique called logistic regression 


# 9.6 Confounding and accounting for other factors
# There are many times when correlations do imply causal relationships
# (beyond just in carefully conducted randomized trials). A major concern
# for observational data is whether the true associations are being
# distorted by other factors that may be the actual determinants of the
# observed relationship between two factors. Such other factors may
# confound the relationship being studied.

# Randomized trials in scientific experiments are considered the gold standard
# for evidence-based research. Such trials, sometimes called A/B tests, are
# commonly undertaken to compare the effect of a treatment (e.g., two different
# forms of a Web page). By controlling who receives a new intervention and who
# receives a control (or standard treatment), the investigator ensures that, on
# average, all other factors are balanced between the two groups. This allows them
# to conclude that if there are differences in the outcomes measured at the end of
# the trial, they can be attributed to the application of the treatment. (It’s
# worth noting that randomized trials can also have confounding if subjects don’t
# comply with treatments or are lost on follow-up.)


# Let’s consider an example of confounding using observational data on average teacher salaries (in 2010) and average total SAT scores for each of the 50 United States. The SAT (Scholastic Aptitude Test) is a high-stakes exam used for entry into college. Are higher teacher salaries associated with better outcomes on the test at the state level? If so, should we adjust salaries to improve test performance?
sat_2010 <- SAT_2010 |> 
    mutate(salary = salary / 1000)

sat_plot <- sat_2010 |> 
    ggplot(mapping = aes(x = salary, y = total)) +
    geom_point() +
    geom_smooth(method = 'lm', formula = y ~ x) +
    ylab('Average total score on the SAT') +
    xlab('Average teacher salary (thousands of USD')

sat_plot

sat_mod1 <- lm(total ~ salary, data = sat_2010)
broom::tidy(sat_mod1)

# Lurking in the background, however, is another important factor. The percentage of students who take the SAT in each state varies dramatically (from 3% to 93% in 2010). We can create a variable called SAT_grp that divides the states into two groups.
sat_2010 |> 
    skim(sat_pct)

sat_2010 |> 
    ggplot(mapping = aes(x = sat_pct)) +
    geom_histogram(binwidth = 5, alpha = 0.5, color = 'black', fill='steelblue')

sat_2010 <- sat_2010 |> 
    mutate(sat_grp = if_else(sat_pct <= 27, 'low', 'high'))

sat_2010 |> 
    group_by(sat_grp) |> 
    count()

# never seen this operator before: %+%
# might be of ggplots definition
sat_plot %+% sat_2010 +
    aes(color = sat_grp) +
    scale_color_brewer("% taking\nthe SAT", palette = "Set2")

sat_2010 |> 
    ggplot(mapping = aes(x = salary, y = total, color = sat_grp)) +
    geom_point() +
    geom_smooth(method = 'lm', formula = y ~ x) +
    geom_smooth(mapping = aes(color = 'No group'), method = 'lm', formula = y ~ x) +
    ylab('Average total score on the SAT') +
    xlab('Average teacher salary (thousands of USD)') +
    scale_color_brewer("% taking\nthe SAT", palette = "Set2")
# simpsons paradox can be demosntrated here in this plot:
#  * Among states with a low percentage taking the SAT, teacher salaries and SAT scores are positively associated.
#  * Among states with a high percentage taking the SAT, teacher salaries and SAT scores are positively associated
#  * Among all states, salaries and SAT scores are negatively associated.
# Addressing confounding is straightforward if the confounding variables are measured.
# Stratification is one approach (as seen above). 


#  Using techniques developed in Section 7.5, we can derive the coefficients of the linear model fit to the two separate groups.
sat_2010 |> 
    group_by(sat_grp) |> 
    group_modify(~broom::tidy(lm(total ~ salary, data = .x)))

#  Multiple regression is another technique. Let’s add the sat_pct variable as an additional predictor into the regression model.
sat_mod2 <- lm(total ~ salary + sat_pct, data = sat_2010)
broom::tidy(sat_mod2)

#  We now see that the slope for Salary is positive and statistically significant when we control for sat_pct. This is consistent with the results when the model was stratified by SAT_grp.

# We still can’t really conclude that teacher salaries cause improvements in SAT scores. However, the associations that we observe after accounting for the confounding are likely more reliable than those that do not take those factors into account.



# 9.7 The perils of p-values
# We close with a reminder of the perils of null hypothesis statistical testing. Recall that a p-value is defined as the probability of seeing a sample statistic as extreme (or more extreme) than the one that was observed if it were really the case that patterns in the data are a result of random chance. This hypothesis, that only randomness is in play, is called the null hypothesis.
