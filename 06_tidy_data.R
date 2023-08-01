# 076 Tidy data
library(googlesheets4)
library(mdsr)
library(tidyverse)
gs4_deauth()

hiv_key <- "1kWH_xdJDM4SMfT_Kzpkk-1yuxWChfurZuWYjfmv51EA"
hiv <- read_sheet(hiv_key) |> 
    rename(Country = 1) |> 
    filter(
        Country %in% c('United States', 'France', 'South Africa')
    ) |> 
    select(Country, `1979`, `1989`, `1999`, `2009`) |> 
    unnest(cols = c(`2009`)) |> 
    mutate(across(matches('[0-9]'), as.double))

hiv


hiv |> 
    pivot_longer(-Country, names_to = 'Year', values_to = 'hiv_rate')



popular_names <- babynames |> 
    group_by(sex, name) |> 
    summarize(total_births = sum(n), .groups = 'drop') |> 
    arrange(desc(total_births))



BP_wide <- tribble(
    ~subject, ~before, ~after,
    'BHO', 160, 115,
    'GWB', 120, 135,
    'WJC', 105, 145
)
BP_wide

BP_narrow <- tribble(
    ~subject, ~when, ~sbp,
    'BHO', 'before', 160,
    'GWB', 'before', 120,
    'WJC', 'before', 105,
    'BHO', 'after', 115,
    'GWB', 'after', 135,
    'WJC', 'after', 145
)


BP_wide |> 
    mutate(change = after - before)

# pivoting wider
# 
# The pivot_wider() function converts a data table from narrow to wide. 
# The values_from argument is the name of the variable in the narrow format that
#   is to be divided up into multiple variables in the resulting wide format. 
# The names_from argument is the name of the variable in the narrow format that
#   identifies for each case individually which column in the wide format will
#   receive the value.
BP_narrow |> 
    pivot_wider(names_from = when, values_from = sbp)


# pivoting longer
#
BP_wide |> 
    pivot_longer(-subject, names_to = 'when', values_to = 'sbp')


# list-columns
#
BP_full <-
    tribble(
        ~subject, ~when, ~sbp, ~dbp, ~date,
        'BHO', 'before', 160, 69, '2007-06-19',
        'GWB', 'before', 120, 54, '1998-04-21',
        'BHO', 'before', 155, 65, '2005-11-08',
        'WJC', 'after', 145, 75, '2002-11-15',
        'WJC', 'after', NA, 65, '2010-03-26',
        'WJC', 'after', 130, 60, '2013-09-15',
        'GWB', 'after', 135, NA, '2009-05-08',
        'WJC', 'before', 105, 60, '1990-08-17',
        'BHO', 'after', 115, 78, '2017-06-04'
    )

BP_full |>
    group_by(subject, when) |>
    summarize(mean_sbp = mean(sbp, na.rm = TRUE))

# But what if we want to do additional analysis on the blood pressure data? The individual observations are not retained in the summarized output. Can we create a summary of the data that still contains all of the observations?
# One simplistic approach would be to use paste() with the collapse argument to condense the individual operations into a single vector.
BP_summary <- BP_full |> 
    group_by(subject, when) |> 
    summarize(
        sbps = paste(sbp, collapse = ', '),
        dbps = paste(dbp, collapse = ', '),
        .groups = 'drop'
    )
BP_summary

# This can be useful for seeing the data, but you can’t do much computing on it,
# because the variables sbps and dbps are character vectors. As a result,
# trying to compute, say, the mean of the systolic blood pressures won’t work as 
# you hope it might. Note that the means computed below are wrong.
BP_summary |>
    mutate(mean_sbp = mean(parse_number(sbps)))


# Instead, the nest() function will collapse all of the ungrouped variables
# in a data frame into a tibble (a simple data frame). This creates a new
# variable of type list, which by default has the name data. Each element of
# that list has the type tibble. Although you can’t see all of the data in the
# output printed here, it’s all in there. Variables in data frames that have
# type list are called list-columns.
BP_nested <- BP_full |> 
    group_by(subject, when) |> 
    nest()
BP_nested

BP_nested |> 
    pull(data)

BP_nested |> 
    mutate(sbp_list = pull(data))

# The problem is that data is not a tibble. Rather, it’s a list of tibbles. To get around this, we need to use the map() function,
BP_nested <- BP_nested |> 
    mutate(sbp_list = map(data, pull, sbp))
BP_nested

# Again, note that sbp_list is a list, with each item in the list being a vector
# of type double. These vectors need not have the same length! We
# can verify this by isolating the sbp_list variable with the pluck() function.
BP_nested |> 
    pluck('sbp_list')
# pluck => Safely get or set an element deep within a nested data structure
#

# Because all of the systolic blood pressure readings are contained within this list, a further application of map() will allow us to compute the mean.
BP_nested <- BP_nested |> 
    mutate(sbp_mean = map(sbp_list, mean, na.rm = TRUE))
BP_nested

BP_nested |> 
    unnest(cols = c(sbp_list))

BP_nested |> 
    unnest(cols = c(sbp_mean))

BP_nested |> 
    unnest(cols = c(sbp_list, sbp_mean))
# This computation gives the correct mean blood pressure for each subject at each time point.
# On the other hand, an application of unnest() to the sbp_list variable, which has more than one observation for each row, results in a data frame with one row for each observed subject on a specific date. This transforms the data back into the same unit of observation as BP_full.
BP_nested |> 
    unnest(cols = c(sbp_list))
