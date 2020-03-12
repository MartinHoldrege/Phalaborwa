# martin holdrege

# script started 3/9/20

# processing picarro output for chemcorrect

# this file for loading them all, based on 03_Process4ChemCorrectAll



# dependencies ---------------------------------------------------------------

library(tidyverse)


# read in data ------------------------------------------------------------


# order of jobs run by picarro (for correcting labels)
jobs1 <- readxl::read_xlsx("data_raw/picarro_jobs.xlsx", sheet = "data")

# picarrow output

output_paths <- list.files("data_raw/picarro_output",
                           pattern =  "output_\\d{8}_[Pp]hal\\d.csv",
                           full.names = TRUE)

phal_names <- str_extract(output_paths, "Phal\\d")
sum(duplicated(phal_names)) # check for duplicates

names(output_paths) <- phal_names # assign before sorting
phal_names <- sort(phal_names)

output_paths <- output_paths[phal_names] # sorting now

raw1 <- map(output_paths, read_csv)

# sample descriptions

rear_paths <- list.files("data_processed/sample_descriptions",
                           pattern =  "[Pp]hal\\d_1.csv",
                         full.names = TRUE)

rear_paths <- sort(rear_paths)
names(rear_paths) <- phal_names

rear <- map(rear_paths, read_csv)

front_paths <- list.files("data_processed/sample_descriptions",
                         pattern =  "[Pp]hal\\d_2.csv",
                         full.names = TRUE)
front_paths <- sort(front_paths)
names(front_paths) <- phal_names

front <- map(front_paths, read_csv)


# combine sample descriptions ---------------------------------------------

# to be joined back in with output data where sample descriptions
# got messed up
descript <- pmap(list(rear, front, phal_names), function(x, y, name) {
  bind_rows(x, y) %>% 
    mutate(Vial = str_pad(Vial, width = 2, side = "left", pad = "0"),
           Port = paste(Tray, Vial, sep = "-"),
           run = name) %>% 
    select(-Tray, -Vial)
}) %>% 
  bind_rows()

# process jobs file -------------------------------------------------------

tray_lookup <- c("r" = 1, "f" = 2)
raw1
jobs1

jobs2 <- jobs1 %>% 
  mutate(id = 1:nrow(.)) %>% 
  group_by(id) %>% 
  nest() %>% 
  mutate(new_cols = map(data, function(df) {
    tray_num <- tray_lookup[df$tray]
    vial_num <- df$start:df$finish %>% 
      as.character() %>% 
      str_pad(width = 2, side = "left", pad = "0")
    Port <- paste(tray_num, vial_num, sep = "-")
    Inj_Nr <- 1:df$count
    out <- expand.grid(Inj_Nr = Inj_Nr, Port = Port, stringsAsFactors = FALSE)
    out
  })) %>% 
  select(-data) %>% 
  ungroup() %>% 
  unnest(cols = "new_cols") %>% 
  mutate(Line = 1:nrow(.)) %>% 
  select(-id)


# combine in output data --------------------------------------------------

raw2 <- map2(raw1, phal_names, function(df, name) {
  df$run <- name
  df
}) %>% 
  bind_rows()

nrow(raw2) 
nrow(jobs2)

clean1 <- left_join(raw2, jobs2, by = c("Line"), suffix = c("", "_cor")) %>% 
  select(-Port, -`Inj Nr`, -matches("Identifier")) %>% 
  rename(Port = Port_cor, # just keeping the good versions of these
         `Inj Nr` = Inj_Nr)  %>% 
  left_join(descript, by = c("Port", "run"))


# discard bad rows --------------------------------------------------------

# true standard values
d2O_vals <- c("Low" = 12.5, "Medium" = 247, "High" = 745)


# the first round of phal1 is no good. 
clean3 <- clean1 %>% 
  filter(Ignore == -1, Good == 1, H2O_Mean > 15000 & !is.na(H2O_Mean))

nrow(clean3)

# discarding samples with only 1 rep left
clean4 <- clean3 %>% 
  group_by(Port, run) %>% 
  mutate(n = n()) %>% 
  # only 1 obs not good enough
  filter(n != 1 | `Identifier 1` %in% names(d2O_vals)) %>% 
  #  select(-n) %>% 
  ungroup() %>% 
  # first 3 high standards tend to be bad--so throwing out
  filter(!(`Inj Nr` %in% 1:3)) 

nrow(clean4)


non_standard <- clean4 %>% 
  filter(!`Identifier 1` %in% names(d2O_vals))

hist(non_standard$`d(D_H)Mean`)



# check standards ---------------------------------------------------------

check <- clean4 %>% 
  filter(`Identifier 1` %in% names(d2O_vals)) %>% 
  mutate(d2O_true = d2O_vals[`Identifier 1`])

# need two standards each
n_stds <- check %>% 
  group_by(`Identifier 1`) %>% 
  summarize(n = n())

if (any(n_stds$n < 2) | length(n_stds$n) < 3) {
  stop("Insufficient num of standards")
}

lm_check <- lm(d2O_true ~ `d(D_H)Mean`, data = check)
summary(lm_check)

plot(d2O_true ~ `d(D_H)Mean`, data = check)
abline(lm_check, col = "blue")
abline(0, 1)


# check SD/slope ----------------------------------------------------------------

DH_summary <- clean4 %>% 
  group_by(Port, `Identifier 1`, `Identifier 2`, run) %>% 
  nest() %>% 
  # lm slope
  mutate(lm_slope = map_dbl(data, function(df) {
    y <- df$`d(D_H)Mean`
    x <- seq_along(y)
    mod <- lm(y~x)
    mod$coefficients[["x"]] # return slope
  }),
  # rank correlation
  cor = map_dbl(data, function(df) {
    y <- df$`d(D_H)Mean`
    x <- seq_along(y)
    cor(x, y, method = "spearman")
  }),
  # delta values ie diff between subsequent values
  delta = map(data, function(df) {
    x <-  diff(df$`d(D_H)Mean`)
    delta <- tibble(
      delta = c(NA_real_, x), # diff
      next_delta = c(x, NA_real_) # next diff
    )
    delta
  })
  ) %>% 
  unnest(cols = c("data", "delta")) %>% 
  mutate(mean = mean(`d(D_H)Mean`),
         median = median(`d(D_H)Mean`),
         sd = sd(`d(D_H)Mean`),
         n = n()
  ) %>% 
  select(matches("Identifier"), Port, lm_slope, mean, median, sd, `d(D_H)Mean`,
         Line, n, cor, delta, next_delta, run)

DH_summary %>% 
  #  filter(sd >5 | abs(lm_slope) > 5) %>% 
  arrange(desc(abs(lm_slope))) 

DH_summary2 <- DH_summary %>% 
  filter(!`Identifier 1` %in% names(d2O_vals))


# possible bad samples--ie high variability but low slope
# slope not meaningful for standards (how calc here)
DH_summary2 %>% 
  filter(sd > 4 & abs(lm_slope) < 5) %>% 
  arrange(desc(sd))  





# which vials discarded ---------------------------------------------------

# vials that had their data discarded
discarded <- anti_join(clean1, clean4, by = "Port") %>% 
  select(Port, `Identifier 1`) %>% 
  .[!duplicated(.), ]

discarded


# modeling the next diff --------------------------------------------------

# can we predict what the change in next value will be based on the changes from the previous?

DH_cor1 <- DH_summary2 %>% 
  filter(abs(cor) == 1)

lm_delta <- lm(next_delta ~ delta, data = DH_cor1)
summary(lm_delta)
plot(next_delta ~ delta, data = DH_cor1)
abline(lm_delta)

