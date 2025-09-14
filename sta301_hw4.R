library(tidyverse)
library(mosaic)
options(scipen = 999)
theme_set(theme_minimal())
library(scales)
library(moderndive)
library(effectsize)

# 1 Slide 1
holiday %>%
  group_by(artist) %>%
  summarise(artistnum = n()) %>%
  arrange(desc(artistnum))

# 1) Pentatonix, Mariah Carey, Elvis Presley

holiday %>%
  group_by(track) %>%
  summarise(tracknum = n()) %>%
  arrange(desc(tracknum))

# 2) Sleigh Ride, Have Yourself a
# Merry Little Christmas, Silver Bells

holiday %>%
  arrange(desc(energy))

# 3) Merry Christmas, Happy Holidays; NSYNC

holiday %>%
  arrange(valence) %>%
  select(artist, track)

# 4) Chestnuts Roasting on an Open Fire; Alexis French

# 1 Slide 2
ggplot(holiday, aes(x = key, y = danceability)) +
  geom_boxplot() + 
  geom_jitter(alpha = 0.2) + 
  labs(title = "Christmas Songs Have Around Average Danceability",
       x = "Scale (Key)",
       y = "Danceability (0-1)") + 
  coord_flip()

median(~danceability, data = holiday)
IQR(~danceability, data = holiday)


# 2 Slide 1
shelter %>%
  filter(animal_breed == "BEAGLE") %>%
  group_by(month) %>%
  summarise(beaglenum = n()) %>%
  arrange(beaglenum)

# June, December

shelter %>%
  filter(animal_type == "WILDLIFE") %>%
  group_by(month) %>%
  summarise(wdlfnum = n()) %>%
  arrange(desc(wdlfnum))

# May, March

breeds = shelter %>%
  group_by(animal_breed) %>%
  summarize(animals = n(),
            adoption_rate = sum(outcome_type == 'ADOPTION')/animals)

breeds %>%
  filter(animals >= 25) %>%
  arrange(desc(adoption_rate))

# Alaskan Husky, Border Collie

# 2 Slide 2
shelter = shelter %>%
  mutate(dog = ifelse(animal_type == 'DOG', 'dog', 'not_dog'),
         cat = ifelse(animal_type == 'CAT', 'cat', 'not_cat'),
         stray = ifelse(intake_type == 'STRAY', 'stray', 'other_intake'),
         surrendered = ifelse(intake_type == 'OWNER SURRENDER', 'surrendered', 'other_intake'))

prop(~dog, data = shelter)
# 1. 0.75
xtabs(~cat + dog, data = shelter) %>%
  prop.table()
#2. 0.03
xtabs(~stray + dog, data = shelter) %>%
  prop.table(margin = 1)
#3. 0.27
prop(~surrendered, data = shelter)
prop(~cat, data = shelter)
xtabs(~surrendered + cat, data = shelter) %>%
  prop.table(margin = 1)
xtabs(~surrendered + cat, data = shelter) %>%
  prop.table(margin = 2)
#4 It appears that an animal being surrendered by its owner is nearly independent 
#  of it being a cat. The probability of an animal being surrendered is approximately
#  0.28. Given the animal was a cat, the probability of the cat being surrendered was approximately 
#  0.27. Given the animal was not a cat, the probability of the animal being surrendered was approximately 0.28.
#  Therefore, P(A) = P(A | B) = P(A | not B), indicating independence.

ggplot(shelter) +
  geom_bar(aes(x = surrendered, fill = cat),
           position = "dodge") +
  labs(title = "Cats are Not More Likely to be Surrendered by their Owners",
       x = "Surrendered by Owner or Other",
       y = "Number of Animals")

# Using visual evidence from the plot, this supports the conclusion about independence
# between the variables cat and surrendered because the number of cats surrendered compared
# to other animals being surrendered has a similar distribution to the number of cats given to the
# shelter through other means and other animals given to the shelter through other means.

# 3 Summer is Coming
power_lm1 = lm(power ~ temperature + weekday + temperature:weekday, data = ERCOT)
get_regression_table(power_lm1)
# 1. With 95% confidence, power consumption can be lower, on average, by 1813 megawatts or higher 
#    by 48 megawatts on a weekend relative to on a weekday, adjusting for temperature.
# 2. With 95% confidence, we expect an increase of between 279 and 291 megawatts when the 
#    temperature increases by one degree on a weekday.
# 3. The main effect of temperature on power consumption is not statistically significant.
#    However, the decrease in power consumption is practically significant, as a decrease 
#    of 882 megawatts and range between -1813 and 48 megawatts is substantial. The interaction
#    between temperature and weekends on power consumption is not statistically significant. The
#    interval from a decrease of 12 megawatts or increase of 10 megawatts suggests that weekend 
#    and temperature are not context-specific in affecting power consumption.

# Slide 2
ggplot(ERCOT, aes(x = temperature, y = power)) +
  geom_point(alpha = 0.2) + 
  geom_smooth(method = 'lm') +
  facet_wrap(~weekday, nrow = 2) + 
  labs(title = "Positive Relationship between Temperature and Power Consumption",
       x = "Temperature (degrees Fahrenheit)",
       y = "Peak Power Consumption (megawatts)")

rsquared(power_lm1)
sd(resid(power_lm1))

# When looking at the plots for temperature versus power consumption, both weekdays
# and weekends exhibit a positive correlation, indicating that as temperature increases,
# peak power consumption also increases. Supporting this, approximately 
# 0.70 of variation in peak power consumption is due to change in temperature. However, 
# the residual standard error for predictions of peak power consumption
# is approximately 1151 megawatts.

# 4 Redlining
FAIR_lm1 = lm(FAIR ~ minority, data = redlining)

ggplot(redlining, aes(x = minority, y = FAIR)) +
  geom_point() +
  geom_smooth(method = 'lm') + 
  labs(title = "Weak Correlation Between Minority Percentage and FAIR Plan Renewals",
       x = "Percentage of Minority Residents (%)",
       y = "New & Renewals of FAIR (per 100 housing units)")

coef(FAIR_lm1)
rsquared(FAIR_lm1)

# There appears to be a weak positive relationship between percentage of minority
# residents living in ZIP codes in Chicago and new FAIR plan policies and renewals
# per 100 housing units. The overall relationship conveys an increase of 0.014 FAIR
# plan policies for every percentage increase of minority residents. The percentage of 
# variation in FAIR policies explained by the regression on minority is approximately
# 0.52.

FAIR_lm2 = lm(FAIR ~ minority + fires + age + income, data = redlining)
get_regression_table(FAIR_lm2)

standardize_parameters(FAIR_lm2)

# The minority and fires partial relationships for this model are statistically 
# significant. With 95% confidence, a one standard deviation increase in minorities 
# is associated with FAIR plans increasing by 0.13 to 0.74 standard deviations, and a one
# standard deviation in fires is associated with an increase of 0.06 to 0.59 standard deviations
# of FAIR plans.
#
# Because standardized coefficients are independent of the variables' units of measurement, the minority partial relationship in this model seems to be the most
# practically significant, as its standardized coefficient is the greatest and
# associated with having the biggest effect on FAIR plans and renewals.
#
# When standardizing the minority coefficient, measurement units of percentage are standardized. We expect, with 95% confidence,
# that a one standard deviation increase in minorities within Chicago is associated with a 0.43
# standard deviation increase in FAIR plans and renewals (per 100 housing units). This is the results for a partial relationship, 
# where all other variables in the multiple regression model are adjusted for (held constant). 

#
# These results do constitute evidence of racial discrimination in the private home insurance market. When
# comparing standardized coefficients (adjusting for other variables), the 0.43 increase in standard deviation of FAIR is greater than the 
# 0.32 increase in standard deviation of FAIR due to the number of fires per 100 housing units. This could indicate that
# minority residents are being discriminated against, as a greater increase in FAIR policies in a ZIP code suggests that 
# residents are accessing private insurance at lower per capita rates.