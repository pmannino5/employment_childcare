# This script replicates and extends Jason Furman, Melissa Kearney, and Wilson Powell's analysis of childcare and employment - 
# https://www.piie.com/blogs/realtime-economic-issues-watch/how-much-have-childcare-challenges-slowed-us-jobs-market

# They calculate the % change in employment to population ratio between Q1-2020 and Q1-2021 for men and women with and without 
# a bachelors degree and with at least 1 child less than 13 years old (who needs childcare) compared to all other adults. 
# They then do a counterfactual analysis that asks how the total YOY employment rate for men and women would change if parents 
# with kids less than 13 had the employment rates of all other adults.

# One potential shortcoming here is that all other adults might not be the right counterfactual, but maybe parents of kids 13 and over 
# would be a better counterfactual as we are still looking at parents, but parents that don't have to provide childcare.

# This script replicates their first graph and then shows how it would be different if instead of using all adults as a comparison, 
# we use parents with kids over 13 as a comparison.

# It then replicates their counterfactual analysis in table 1 for employment-population ratio changes. 
# The outcome doesn't EXACTLY match their results, but it's very close. I then redo the counterfactual, 
# but use the employment-population ratio changes for parents with kids over 13 as the counterfactual. 

# Need to clean and document code better - still pretty stream of consciousness coding

##### Set up environment #####

library(dplyr)
library(ggplot2)
library(tidyr)
library(gt)
`%notin%` <- Negate(`%in%`)
setwd('/Users/petermannino/Documents/pandemic_employment')

##### Load CPS Data and Recode Variables #####

# NOTE: To load data, you must download both the extract's data and the DDI
# and also set the working directory to the folder with these files (or change the path below).

if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")

ddi <- read_ipums_ddi("cps_00001.xml")
data <- read_ipums_micro(ddi)

# recode and filter
epop <- data %>% 
  mutate(young_child = if_else(YNGCH<13,'<13',
                             if_else(YNGCH == 99, 'no_children', '>=13')),
         young_child_orig = if_else(YNGCH<13,'<13','All Other Adults'),
         education = if_else(EDUC >= 110, 'Bachelor+', 'Less_Than_Bachelor'),
         employment = if_else(EMPSTAT %in% c(01,10,12),1,0),
         month_year = lubridate::make_datetime(year=YEAR, month=MONTH),
         weighted_emp = employment * WTFINL) %>%
  filter(LABFORCE != 0 & SEX != 9 & EMPSTAT != 0 & EDUC %notin% c(000,001,999))

age_bucket <- function(x) {
  if (x >= 55) {
    return('55+')
  }
  else if (x >= 40) {
    return('40-54')
  }
  else if (x >= 25) {
    return('25-39')
  }
  else if (x >= 16) {
    return('16-24')
  }
  else return(NA)
}

epop$age_bucket <- apply(X=epop['AGE'], FUN = age_bucket, MARGIN = 1)
  
##### Replicate the first graph and create revised ##### 

### Replicate the graph exactly 

# prep data for graph of employment rate change for parents with young kids v all other adults
epop %>% filter(MONTH %in% c(1,2,3)) %>%
  group_by(SEX, young_child_orig, education, YEAR) %>%
  summarize(epop = sum(weighted_emp)/sum(WTFINL)) %>%
  tidyr::spread(key = YEAR, value=epop) %>%
  mutate(epop_pct_change = (`2021`-`2020`)/`2020`) %>%
  # graph:
  ggplot(mapping = aes(x=education, y=epop_pct_change, fill=young_child_orig)) + 
  geom_col(position='dodge') +
  facet_wrap(~ as_factor(SEX)) + labs(title='Original Graph', subtitle = "All other adults as comparison") +
  theme(axis.text.x = element_text(size=6))

ggsave(filename = "epop_change_replication.png", device='png', height = 3, width = 6, units="in")

### Revised graph using parents of older kids as a comparison

#  prep data for graph of employment rate change for parents with young kids v parents of older kids
epop %>% filter(MONTH %in% c(1,2,3)) %>%
  group_by(SEX, young_child, education, YEAR) %>%
  summarize(epop = sum(weighted_emp)/sum(WTFINL)) %>%
  tidyr::spread(key = YEAR, value=epop) %>%
  mutate(epop_pct_change = (`2021`-`2020`)/`2020`) %>%
  filter(young_child != 'no_children') %>%
  # graph:
  ggplot(mapping = aes(x=education, y=epop_pct_change, fill=young_child)) + 
  geom_col(position='dodge') +
  facet_wrap(~ as_factor(SEX)) + labs(title='New Graph', subtitle = "Parents of older children as comparison") +
  theme(axis.text.x = element_text(size=6))

ggsave(filename = "epop_change_revised.png", device='png', height = 3, width = 6, units="in")

### New graph of epop changes over time

epop %>% filter(AGE > 25 & AGE <= 64) %>% 
  group_by(SEX, young_child, month_year) %>%
  summarize(epops = sum(weighted_emp)/sum(WTFINL)) %>% 
  data.table() %>%
  group_by(SEX, young_child) %>%
  mutate(pct_change = epops/epops[1]) %>% ungroup() %>%
  filter(SEX == 2) %>% 
  
  ggplot(mapping = aes(x=month_year, y=pct_change, color=young_child)) +
  geom_line() + labs(title='Employment to Population Ratio', subtitle = "% Change from Jan 2020")


##### Replicate and extend counterfactual analysis #####

### Replicate the first table using parents of young kids and all other adults

# The authors create age-sex-education cells and then calculate how different the employment-population ratio would be if 
# parents of young kids had the same change in employment rates as all other adults within each cell.

# actual epop rate changes for men and women
actual <- epop %>% filter(MONTH %in% c(1,2,3) & !is.na(age_bucket)) %>%
  group_by(YEAR, SEX) %>%
  summarize(epop = sum(weighted_emp)/sum(WTFINL)) %>%
  spread(key=YEAR, value = epop) %>%
  mutate(`Actual Percent Change` = (`2021`- `2020`)/`2020`)

View(actual)

# generate the counterfactual epop rate changes for the replication analysis
cntrfct <- epop %>% filter(MONTH %in% c(1,2,3) & !is.na(age_bucket)) %>%
  group_by(SEX, young_child_orig, education, YEAR, age_bucket) %>%
  summarize(epop = sum(weighted_emp)/sum(WTFINL)) %>% ungroup() %>%
  pivot_wider(names_from = YEAR,
              values_from = epop) %>%
  mutate(pct_change = (`2021`-`2020`)/`2020`) %>%
  filter(young_child_orig == 'All Other Adults') %>%
  select(SEX, age_bucket, education, pct_change)

# calculate the counterfactual assuming parents with kids < 13 had the same epop change as everyone else
cntrfct_result <- epop %>% filter(MONTH %in% c(1,2,3) & !is.na(age_bucket)) %>%
  group_by(SEX, young_child_orig, education, YEAR, age_bucket) %>%
  summarize(epop = sum(weighted_emp)/sum(WTFINL),
            weighted_emp = sum(weighted_emp),
            weight = sum(WTFINL)) %>%
  pivot_wider(names_from = YEAR,
              values_from = c(epop, weighted_emp, weight)) %>%
  left_join(cntrfct, by = c('SEX' = 'SEX', 
                            'education' = 'education', 
                            'age_bucket' = 'age_bucket')) %>%
  mutate(new_2021_epop =epop_2020+(epop_2020 * pct_change),
         new_2021_weighted_emp = new_2021_epop*weight_2021) %>%
  group_by(SEX) %>% summarize(emp_rate_2020 = sum(weighted_emp_2020)/sum(weight_2020),
                              emp_rate_2021 = sum(new_2021_weighted_emp)/sum(weight_2021),
                              `Counterfactual Percent Change` = (emp_rate_2021-emp_rate_2020)/emp_rate_2020)

# Merge results into table and export table
replicate_results_table <- actual %>%
  select(SEX, `Actual Percent Change`) %>%
  left_join(cntrfct_result %>% select(SEX, `Counterfactual Percent Change`)) %>% 
  mutate(SEX = as_factor(SEX)) %>%
  gt() %>% fmt_percent(columns = c(`Actual Percent Change`,`Counterfactual Percent Change`), decimals = 3) %>%
  tab_options(table.font.size = 8) %>% tab_header(title="Counterfactual Replication Results")

gtsave(data=replicate_results_table, filename = 'replication_results.png')

# The results of this analysis match very closely, though not exactly with what FKP reported in their analysis. 
# Basically, childcare seems to have very little impact on changes in epop rates because the difference in employment 
# rates between parents of young kids and everyone else is pretty small and because the population of employed parents 
# with young kids is pretty small, so assuming higher employment rates on this group has little impact on total epop changes of men and women.

### A revised counterfactual using parents of older kids

# This does the same thing as above, but breaks out adults into parents of young kids, older kids, 
# and no kids and assumes the counterfactual where the parents of young kids have the same YOY epop ratio 
# change as the parents of old kids (instead of all othe adults). The problem now is that some of the cells are pretty small. 
# In particular the 16-24 year old cell with kids older than 13 is really small (a 16 year old can't have a kid over 13 years old), 
# so epop changes are huge (100% decrease in one case) and does not reflect reality. So for this analysis I remove the 16-24 age group, 
# and then run the revised counterfactual exercise. 

# actual epop rate changes for men and women excluding 16-24 year olds
actual_revise <- epop %>% filter(MONTH %in% c(1,2,3) & !is.na(age_bucket)  & age_bucket != '16-24') %>%
  group_by(YEAR, SEX) %>%
  summarize(epop = sum(weighted_emp)/sum(WTFINL)) %>%
  spread(key=YEAR, value = epop) %>%
  mutate(`Actual Percent Change` = (`2021`- `2020`)/`2020`)


# generate the counterfactual epop rate changes for revised analysis using parents with kids >=13 as cf
cntrfct_revise <- epop %>% filter(MONTH %in% c(1,2,3) & !is.na(age_bucket) & age_bucket != '16-24') %>%
  group_by(SEX, young_child, education, YEAR, age_bucket) %>%
  summarize(epop = sum(weighted_emp)/sum(WTFINL)) %>% ungroup() %>%
  pivot_wider(names_from = YEAR,
              values_from = epop) %>%
  mutate(pct_change = (`2021`-`2020`)/`2020`) %>%
  filter(young_child == '>=13') %>%
  select(SEX, age_bucket, education, pct_change)


# calculate the counterfactual assuming parents with kids < 13 had the same epop change as parents with kids > 13
cntrfct_result_revise <- epop %>% filter(MONTH %in% c(1,2,3) & !is.na(age_bucket) & age_bucket != '16-24') %>%
  group_by(SEX, young_child, education, YEAR, age_bucket) %>%
  summarize(epop = sum(weighted_emp)/sum(WTFINL),
            weighted_emp = sum(weighted_emp),
            weight = sum(WTFINL)) %>%
  pivot_wider(names_from = YEAR,
              values_from = c(epop, weighted_emp, weight)) %>%
  left_join(cntrfct_revise, by = c('SEX' = 'SEX', 
                            'education' = 'education', 
                            'age_bucket' = 'age_bucket')) %>%
  mutate(pct_change = if_else(young_child == '<13', pct_change, (epop_2021-epop_2020)/epop_2020),
         new_2021_epop =epop_2020+( epop_2020 * pct_change),
         new_2021_weighted_emp = new_2021_epop*weight_2021) %>%
  group_by(SEX) %>% summarize(emp_rate_2020 = sum(weighted_emp_2020)/sum(weight_2020),
                              emp_rate_2021 = sum(new_2021_weighted_emp)/sum(weight_2021),
                              `Counterfactual Percent Change` = (emp_rate_2021-emp_rate_2020)/emp_rate_2020)

# Merge results into table and export table
revised_replicate_results_table <- actual_revise %>%
  select(SEX, `Actual Percent Change`) %>%
  left_join(cntrfct_result_revise %>% select(SEX, `Counterfactual Percent Change`)) %>% 
  mutate(SEX = as_factor(SEX)) %>%
  gt() %>% fmt_percent(columns = c(`Actual Percent Change`,`Counterfactual Percent Change`), decimals = 3) %>%
  tab_options(table.font.size = 8) %>% tab_header(title="Revised Counterfactual Replication Results")

gtsave(data=revised_replicate_results_table, filename = 'revised_replication_results.png')
