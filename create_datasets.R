###########################################################################
#
# This script creates the datasets use (admissions and bar passagage sets)
# The datasets are read from s3 buckets and combined
#
###########################################################################

library(tidyverse)

# we need to scale LSAT and GPA; plotly oddly plots columns scaled with the 'scale' function
# so we will use a custom function to scale columns
custom_scale <- function(x) (x - mean(x, na.rm = T)) / sd(x, na.rm = T)

# read in and transform admissions data -----------
admissions <- read_csv('https://s3.amazonaws.com/aba-disclosures/admissions.csv.gz') %>%
  # select needed variables
  select(schoolname, calendaryear, uggpa50, lsat50)%>%
  # remove crazy outliers that have something wrong
  filter(uggpa50 > 2 & lsat50 > 130) %>%
  # scale gpa and lsat and add together, then scale this number
  # when scaling the sum don't use scale function because it creates odd side effects in plots
  # needed so we can create a combined GPA / LSAT scaled column
  mutate(scaled_admissions = custom_scale(uggpa50) + custom_scale(lsat50),
         scaled_admissions = round(custom_scale(scaled_admissions), 2),
         # add two to calendar year, to create bar year
         firsttimebaryear = calendaryear + 2) %>%
  # no longer need calendar year, since we will join datasets on bar year
  select(-calendaryear) 

# read in and transform bar data ----------- 
bar <- read_csv('https://s3.amazonaws.com/aba-disclosures/bar_pass_jur.csv.gz') %>%
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
            ~(as.integer(. * 100)))

# merge these two datasets with inner join because we only want
# rows with all information
full <- inner_join(admissions, bar, by = c('schoolname', 'firsttimebaryear')) %>%
  rename(year = firsttimebaryear) %>%
  # sort by school name and year
  arrange(schoolname, year) %>%
  # remove Puerto Rico schools because they sit as outliers
  # remove Univ. of Wisconsin and Marquette because in Wisconsin, all Wisconsin law
  # school grads are automatically counted as passing the bar
  filter(!str_detect(schoolname, "^Inter Amer|^Pontifical|Puerto Rico|Wisconsin|Marquette"))

# write out dataset
write_rds(full, 'data/bar_passage.rds')
