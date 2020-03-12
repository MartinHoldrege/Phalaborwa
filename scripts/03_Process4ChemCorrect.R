# martin holdrege

# script started 2/19/20

# processing picarro output for chemcorrect
# ie creating "clean" files

# dependencies ---------------------------------------------------------------

library(tidyverse)


# read in data ------------------------------------------------------------

set_num <- 6

# order of jobs run by picarro (for correcting labels)
jobs1 <- readxl::read_xlsx("data_raw/picarro_jobs.xlsx", sheet = "data")

# picarrow output

# problem if multiple files for same set_num (ie set_num = 1)
output_file <- list.files(
  path = "data_raw/picarro_output",
  pattern = paste0("output_\\d{8}_Phal", set_num, ".csv"))

output_file

raw1 <- read_csv(file.path("data_raw/picarro_output", output_file))

# sample descriptions

rear <- read_csv(paste0("data_processed/sample_descriptions/Phal", 
                        set_num, "_1.csv"))
front <- read_csv(paste0("data_processed/sample_descriptions/Phal", 
                         set_num, "_2.csv"))


# combine sample descriptions ---------------------------------------------

# to be joined back in with output data where sample descriptions
# got messed up
descript <- bind_rows(rear, front) %>% 
  mutate(Vial = str_pad(Vial, width = 2, side = "left", pad = "0"),
         Port = paste(Tray, Vial, sep = "-")) %>% 
  select(-Tray, -Vial)


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


nrow(raw1) 
nrow(jobs2)

clean1 <- left_join(raw1, jobs2, "Line", suffix = c("", "_cor")) %>% 
  select(-Port, -`Inj Nr`, -matches("Identifier")) %>% 
  rename(Port = Port_cor, # just keeping the good versions of these
         `Inj Nr` = Inj_Nr)  %>% 
  left_join(descript, by = "Port")

clean2 <- clean1[, names(raw1)] # back to original order
  


# discard bad rows --------------------------------------------------------

# true standard values
d2O_vals <- c("Low" = 12.5, "Medium" = 247, "High" = 745)


# the first round of phal1 is no good. 
clean3 <- clean2 %>% 
  filter(Ignore == 0, Good == 1, H2O_Mean > 15000 & !is.na(H2O_Mean))
nrow(clean2)
nrow(clean3)

# discarding samples with only 1 rep left
clean4 <- clean3 %>% 
  group_by(Port) %>% 
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
  group_by(Port, `Identifier 1`, `Identifier 2`) %>% 
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
         Line, n, cor, delta, next_delta)

DH_summary %>% 
#  filter(sd >5 | abs(lm_slope) > 5) %>% 
  arrange(desc(abs(lm_slope))) %>% 
  View()

DH_summary2 <- DH_summary %>% 
  filter(!`Identifier 1` %in% names(d2O_vals))


# possible bad samples--ie high variability but low slope
# slope not meaningful for standards (how calc here)
DH_summary2 %>% 
  filter(sd > 4 & abs(lm_slope) < 5) %>% 
  arrange(desc(sd))  %>% 
  View()


# prep chem correct files -------------------------------------------------

# chem correct files can only have 250 lines
n <- nrow(clean4)
n_files <- ceiling(n/250)

first <- 1
seqs <- list()
for (i in 1:n_files) {
  ith_seq <- first:min(n, i*250)
  first <- max(ith_seq) + 1
  seqs[[i]] <- ith_seq
}
seqs
clean5 <- clean4 %>% 
  select(-n)
dfs_4chem <- map(seqs, function(rows) {
  df <- clean5[rows, ]
  df$Line <- 1:nrow(df)
  df
})


# save files --------------------------------------------------------------

clean_paths <- paste0(str_replace(output_file, ".csv$", ""), 
                      "_clean_", 1:n_files, ".csv")

map2(dfs_4chem, clean_paths, function(df, path) {
  write_csv(df, file.path("data_processed/clean_4cc", path))
})


# which vials discarded ---------------------------------------------------

# vials that had their data discarded
discarded <- anti_join(clean1, clean4, by = "Port") %>% 
  select(Port, `Identifier 1`) %>% 
  .[!duplicated(.), ]

discarded

file <- paste0(str_replace(output_file, ".csv$", ""), "_bad_vials.csv")
file
write_csv(discarded, file.path("data_processed/bad_vials", file))




