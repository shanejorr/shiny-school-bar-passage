###########################################################################
#
# This script creates the datasets use (admissions and bar passagage sets)
# The datasets are read from s3 buckets and combined
#
###########################################################################

library(tidyverse)

# read in and transform admissions data -----------
admissions <- read_csv('https://s3.amazonaws.com/aba-disclosure-509/admissions.csv') %>%
  # select needed variables
  select(schoolname, calendaryear, uggpa50, lsat50) %>%
  # scale gpa and lsat and add together, then convert to percent rank (between 0 and 1)
  # needed so we can create a combined scaled column
  mutate(scaled_admissions = round(percent_rank(scale(uggpa50) + scale(lsat50)),2),
         # add two to calendar year, to create bar year
         firsttimebaryear = calendaryear + 2) %>%
  # no longer need calendar year, since we will join datasets on bar year
  select(-calendaryear) %>%
  # remove crazy outliers that have something wrong
  filter(uggpa50 > 2 & lsat50 > 130)

# read in and transform bar data ----------- 
bar <- read_csv('https://s3.amazonaws.com/aba-disclosure-509/bar_pass_jurisd.csv') %>%
  select(-schoolid, -calendaryear) %>%
  # we want pass rates by school and year
  group_by(schoolname, firsttimebaryear) %>%
  # create columns that a re total takers and passers for each school / year
  # will be used to find weighted school / state difference
  mutate(total_takers = sum(takers),
         total_passers = sum(passers),
         # calculate weighted difference for each school / state/ year combination
         # first calculate percentage of total takers for school who took in given state
         # then multiply this number by the school / state difference
         state_perc_takers = (takers / total_takers) * passpctdiff) %>%
  # find overall school pass rate and overall school - state difference rate
  summarize(pass_rate = sum(passers) / sum(takers),
            pass_diff = sum(state_perc_takers)) %>%
  # we want pass rates in whole numbers
  mutate_at(vars(pass_rate, pass_diff),
            funs(as.integer(. * 100)))

# merge these two datasets with inner join because we only want
# rows with all information
full <- inner_join(admissions, bar, by = c('schoolname', 'firsttimebaryear')) %>%
  rename(year = firsttimebaryear) %>%
  # sort by school name and year
  arrange(schoolname, year)

# write out dataset
write_rds(full, 'data/bar_passage.rds')
