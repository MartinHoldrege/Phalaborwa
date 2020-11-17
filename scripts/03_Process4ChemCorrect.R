# martin holdrege

# script started 2/19/20

# processing picarro output for chemcorrect
# ie creating "clean" files

# dependencies ---------------------------------------------------------------

library(tidyverse)


# read in data ------------------------------------------------------------


# * picarro output --------------------------------------------------------


# order of jobs run by picarro (for correcting labels)
jobs1 <- readxl::read_xlsx("data_raw/picarro_jobs.xlsx", sheet = "data")

# picarrow output

output_paths <- list.files(path = "data_raw/picarro_output",
                           pattern = "output_\\d{8}_Phal\\d{1,2}\\.csv",
                           full.names = TRUE)

phal_names <- str_extract(output_paths, "[Pp]hal\\d{1,2}")
names(output_paths) <- phal_names
raw1 <- map(output_paths, read_csv)

# dealing with phal15 seperately (no col names)
raw1$Phal15 <- read_csv(output_paths["Phal15"], col_names = FALSE)
names(raw1$Phal15) <- names(raw1$Phal14)


# * sample descriptions ---------------------------------------------------

rear_paths <- list.files(path = "data_processed/sample_descriptions",
                             pattern = "Phal\\d{1,2}_1\\.csv",
                             full.names = TRUE)

front_paths <- list.files(path = "data_processed/sample_descriptions",
                         pattern = "Phal\\d{1,2}_2\\.csv",
                         full.names = TRUE)

names(rear_paths) <- str_extract(rear_paths, "[Pp]hal\\d{1,2}")
names(front_paths) <- str_extract(front_paths, "[Pp]hal\\d{1,2}")

# making sure order is the same as picarro output above
rear_paths <- rear_paths[phal_names]
front_paths <- front_paths[phal_names]

# none missing?
stopifnot(
  length(rear_paths) == length(phal_names),
  length(front_paths) == length(phal_names)
)

rear <- map(rear_paths, read_csv)
front <- map(front_paths, read_csv)

# combine sample descriptions ---------------------------------------------

# to be joined back in with output data where sample descriptions
# got messed up

descript <- map2(rear, front, function(x, y) {
  bind_rows(x, y) %>% 
    mutate(Vial = str_pad(Vial, width = 2, side = "left", pad = "0"),
           Port = paste(Tray, Vial, sep = "-")) %>% 
    select(-Tray, -Vial)
})


# process jobs file -------------------------------------------------------

tray_lookup <- c("r" = 1, "f" = 2)



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

nrow(jobs2)

clean2 <- map2(raw1, descript, function(x, y) {
  out <- left_join(x, jobs2, by = "Line", suffix = c("", "_cor")) %>% 
    select(-Port, -`Inj Nr`, -matches("Identifier")) %>% 
    rename(Port = Port_cor, # just keeping the good versions of these
           `Inj Nr` = Inj_Nr)  %>% 
    left_join(y, by = "Port")
  
  out <- out[, names(x)] # back to original order
  out
})

# discard bad rows --------------------------------------------------------

# true standard values
d2O_vals <- c("Low" = 12.5, "Medium" = 247, "High" = 745)


# the first round of phal1 is no good. 
clean3 <- map(clean2, .f = filter, 
              Ignore == 0, Good == 1, H2O_Mean > 15000 & !is.na(H2O_Mean))

# discarding samples with only 1 rep left
clean4 <- map(clean3, function(df) {
  df %>% 
    group_by(Port) %>% 
    mutate(n = n()) %>% 
    # only 1 obs not good enough
    filter(n != 1 | `Identifier 1` %in% names(d2O_vals)) %>% 
    #  select(-n) %>% 
    ungroup() %>% 
    # first 3 high standards tend to be bad--so throwing out
    filter(!(`Inj Nr` %in% 1:3)) 
})

clean3$Phal1$`Identifier 1` %>% unique()

non_standard <- map(clean4, .f = filter,
                    !`Identifier 1` %in% names(d2O_vals))
                    

hist(non_standard$Phal16$`d(D_H)Mean`)

# check standards ---------------------------------------------------------

check <- map(clean4, function(df) {
  df %>% 
    filter(`Identifier 1` %in% names(d2O_vals)) %>% 
    mutate(d2O_true = d2O_vals[`Identifier 1`])
})
# need two standards each
n_stds <- map(check, function(df) {
  df %>% 
    group_by(`Identifier 1`) %>% 
    summarize(n = n())
})

walk2(n_stds, names(n_stds), function(x, y) {
  if (any(x$n < 2) | length(x$n) < 3) {
    warning("Insufficient num of standards in ",y)
  }
})


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
  arrange(desc(abs(lm_slope)))

DH_summary2 <- DH_summary %>% 
  filter(!`Identifier 1` %in% names(d2O_vals))


# possible bad samples--ie high variability but low slope
# slope not meaningful for standards (how calc here)
DH_summary2 %>% 
  filter(sd > 4 & abs(lm_slope) < 5) %>% 
  arrange(desc(sd))


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




