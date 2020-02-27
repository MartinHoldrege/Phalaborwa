# calibration of piccaro

library(tidyverse)

# read in data ------------------------------------------------------------

# initial calibration (prob not good)
dfa1 <- read.csv("data_raw\\picarro_output\\HIDS2246_HT_IsoWater_20200122_205103_calibration.csv",
         as.is = TRUE)

dfa1

# 2nd calibration (no good--b/ septum missing--h20 ppm too low)
dfb1 <- read.csv("data_raw\\picarro_output\\HIDS2246_HT_IsoWater_20200127_235356_calibration.csv",
                 as.is = TRUE)

# 3rd calibration--h2o ppm ok--I'm not sure if usercal was really 0, 1
dfc1 <- read.csv("data_raw\\picarro_output\\output_20200221_calibration.csv",
             as.is = TRUE) %>% 
  as_tibble()

# 4th calibration--h2o ppm ok--usercal was 3rd cal slope/intercepth
dfd1 <- read.csv("data_raw\\picarro_output\\output_20200223_calibration.csv",
                 as.is = TRUE) %>% 
  as_tibble()

# correct labels ----------------------------------------------------------

# actual labels of vials run for initial callibration (ie 20200122)
labs <- c("Dummy", "Low", "High", "Tap", "Low", "Med", "High", "Tap", "Med",
          "High")
dfa1$cor_id <- labs[dfa1$Sample]

# values from calibration samples:
d2O_vals <- c("Low" = 12.5, "Med" = 247, "High" = 745)

d18O_vals <- c("Low" = -15.2, "Med" = -16.7, "High" = -1.7)

dfa2 <- dfa1 %>% 
  mutate(d2O_true = d2O_vals[cor_id],
         d18O_true = d18O_vals[cor_id]) %>% 
  filter(Ignore != -1)

dfb2 <- dfb1 %>% 
  mutate(Identifier.1 = str_replace_all(Identifier.1, "\\s", ""),
         Port = str_replace_all(Port, "\\s", "")) %>% 
  filter(Ignore != -1) 


# data correction
# wrong labels (vials checked)

dfb2$Identifier.1[dfb2$Port == "1-21"]  <- "Tap"
dfb2$Identifier.1[dfb2$Port == "1-22"]  <- "Low"
dfb2$Identifier.1[dfb2$Port == "1-23"]  <- "High"

dfb2 <- dfb2 %>% 
  mutate(d2O_true = d2O_vals[Identifier.1],
         d18O_true = d18O_vals[Identifier.1])

# processing 3rd calibration attempt
dfc2 <- dfc1 %>% 
  mutate(Identifier.1 = str_replace_all(Identifier.1, "\\s", ""),
         Port = str_replace_all(Port, "\\s", ""),
         d2O_true = d2O_vals[Identifier.1],
         d18O_true = d18O_vals[Identifier.1]) %>% 
  filter(Ignore != -1, H2O_Mean > 15000) 

# processing 4th calibration attempt
dfd2 <- dfd1 %>% 
  mutate(Identifier.1 = str_replace_all(Identifier.1, "\\s", ""),
         Port = str_replace_all(Port, "\\s", ""),
         d2O_true = d2O_vals[Identifier.1],
         d18O_true = d18O_vals[Identifier.1]) %>% 
  filter(Ignore != -1, H2O_Mean > 15000) 

# regressions first calibration----------------------------------------------

# hydrogen
names(dfa2)
lm1 <- lm(dfa2$d2O_true ~ dfa2$d.D_H.Mean)
plot(dfa2$d.D_H.Mean, dfa2$d2O_true)
abline(lm1)

summary(lm1)

lm1$coefficients

# oxygen
lm2 <- lm(dfa2$d18O_true ~ dfa2$d.18_16.Mean)
plot(dfa2$d18O_true ~ dfa2$d.18_16.Mean)
abline(lm2)

summary(lm2)

lm2$coefficients

# regressions 2nd calibration----------------------------------------------

# these are what should be used
dfb2 %>% 
  filter(Identifier.1 %in% c("Low", "Med", "High")) %>% 
ggplot(aes(d.D_H.Mean)) +
  geom_histogram() +
  facet_wrap(~Identifier.1)

# hydrogen
names(dfb2)
lm3 <- lm(dfb2$d2O_true ~ dfb2$d.D_H.Mean)
plot(dfb2$d.D_H.Mean, dfb2$d2O_true)
abline(lm3)

summary(lm3)

lm3$coefficients

# oxygen
lm4 <- lm(dfb2$d18O_true ~ dfb2$d.18_16.Mean)
plot(dfb2$d18O_true ~ dfb2$d.18_16.Mean)
abline(lm4)

summary(lm4)

lm4$coefficients

# regressions 3rd calibration----------------------------------------------

# these are what should be used
dfc2 %>% 
  filter(Identifier.1 %in% c("Low", "Med", "High")) %>% 
  ggplot(aes(d.D_H.Mean)) +
  geom_histogram() +
  facet_wrap(~Identifier.1, scales = 'free')

# hydrogen
names(dfc2)
lm3 <- lm(dfc2$d2O_true ~ dfc2$d.D_H.Mean)
plot(dfc2$d.D_H.Mean, dfc2$d2O_true)
abline(lm3)

summary(lm3)

lm3$coefficients


# regressions 4th calibration----------------------------------------------


# hydrogen
names(dfd2)
lmd1 <- lm(dfd2$d2O_true ~ dfd2$d.D_H.Mean)
plot(dfd2$d.D_H.Mean, dfd2$d2O_true)
abline(lmd1)

summary(lmd1)

lmd1$coefficients

# this calibration occured with lm3$cofficients as usercal values.
# calculating the combined slope/intercept

new_slope <- lm3$coefficients[[2]]*lmd1$coefficients[[2]]

new_intercept <- lm3$coefficients[[1]]*lmd1$coefficients[[2]] +lmd1$coefficients[[1]]

# test---
# undoing the effect of the current slope/intercept
# raw obs is what obs would be with slope = 1, intercept = 0
raw_obs <- (dfd2$d.D_H.Mean -lm3$coefficients[[1]])/lm3$coefficients[[2]]

# predicted with new slope intercept
adj_obs <- raw_obs*new_slope + new_intercept
plot(adj_obs, dfd2$d2O_true)
abline(0, 1)
new_slope
new_intercept
