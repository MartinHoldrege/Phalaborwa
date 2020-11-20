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
    out$Job <- df$Job # each id belongs to 1 job
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
  # join in correct port and id from descript
  out <- left_join(x, jobs2, by = "Line", suffix = c("", "_cor")) %>% 
    select(-Port, -`Inj Nr`, -matches("Identifier")) %>% 
    rename(Port = Port_cor, # just keeping the good versions of these
           `Inj Nr` = Inj_Nr)  %>% 
    left_join(y, by = "Port") %>% 
    mutate(Sample = str_extract(Port, "\\d+$"),
           Sample = as.numeric(Sample))
  
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


lm_check <- map(check, function(df) {
  lm(d2O_true ~ `d(D_H)Mean`, data = df)
})

# check linear mods
R2 <- map_dbl(lm_check, function(x) summary(x)$r.squared)
R2
#slopes should be 1
beta <- map_dbl(lm_check, function(x) x$coefficients[2])
beta
hist(beta)

# check SD/slope ----------------------------------------------------------------

# code removed check commits before 11/16/20

# prep chem correct files -------------------------------------------------

# chem correct files can only have 250 lines.
# my idea here is to include all the standards in both files


dfs_4chem <- map(clean4, function(x) {
  # df with standards only (included in all chem correct files)
  stds_only <- x %>% 
    filter(`Identifier 1` %in% names(d2O_vals)) %>% 
    select(-n)
  
  # df with no standards
  no_stds <- x %>% 
    filter(!`Identifier 1` %in% names(d2O_vals)) %>% 
    select(-n)
  
  # appending standards to rest of data (up to 250 rows)
  # then looping to add additional dataframes until with <= 250 rows
  # until no more rows
  
  df_list <- list()
  df_num <- 1
  while(nrow(no_stds) > 1) {
    end_row <- min(nrow(no_stds), 250 - nrow(stds_only))
    
    # do't want a specific sample split over two output files
    end_id <- no_stds[end_row, ]$`Identifier 1`
    
    if (end_id != "Tap") {
      
      which_id <- which(no_stds$`Identifier 1` == end_id)
      end_row <- if(max(which_id) <= 250 - nrow(stds_only)) {
        end_row 
      } else {
        min(which_id) -1 # in this case 'stop early' so don't split observations
        # across two dfs
      }
    }
    
    rows <- 1:end_row
    
    df <- bind_rows(stds_only, no_stds[rows, ])
    df$Line <- 1:nrow(df)
    
    df_list[[df_num]] <- df
    
    # prep for next loop cycle:
    
    # remove rows already saved 
    no_stds <- no_stds[-rows, ]
    
    df_num <- df_num + 1
  }
  return(df_list)
})

# num rows in each chem correct file
map(dfs_4chem, function(dfs) map_dbl(dfs, nrow))
n_files <- map_dbl(dfs_4chem, length)

# an example where first file for chem correct <250 so that
# vial doesn't get 'split'
tail(dfs_4chem$Phal3[[1]])

# save files --------------------------------------------------------------
# everyting after / ie just file name
short_paths <- str_extract(output_paths, "[^/]+$")
names(short_paths) <- names(output_paths)

clean_paths <- map2(short_paths, n_files, function(x, y) {
  paste0(str_replace(x, ".csv$", ""), "_clean_", 1:y, ".csv")
})

# for each run write 1 or more files to chem correct
if (FALSE) {
map2(dfs_4chem, clean_paths, function(dfs, paths) {
  map2(dfs, paths, function(df, path) {
    write_csv(df, file.path("data_processed/clean_4cc", path))
  })
})
}
# which vials discarded ---------------------------------------------------

# vials that had their data discarded

discarded <- map2(clean2, clean4, function(x, y ) {
  anti_join(x, y, by = "Port") %>% 
    select(Port, `Identifier 1`) %>% 
    .[!duplicated(.), ]
})

discarded <- bind_rows(discarded, .id = "run")
tail(discarded)
if (FALSE) {
  write_csv(discarded, "data_processed/bad_vials/Phal_bad_vials.csv")
}



