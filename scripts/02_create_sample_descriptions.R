# Martin Holdrege

# script started 1/13/19

# this script is for creating sample description files for use with Picarro



# which set (run) to focus on
set_num <- 4


# packages ----------------------------------------------------------------

library(tidyverse)
library(readxl)

# load files --------------------------------------------------------------

# vial info
info1 <- read.csv("data_processed/PhalSorted_v1.csv")

# info about vials in tray
tray1 <-read_xlsx("data_raw/Phal_unjoined4run.xlsx", sheet = "data")


# prep tray file ----------------------------------------------------------

names(tray1)

# just the set of interest
tray2 <- tray1 %>% 
  filter(set == set_num)

cols1 <- tray2 %>% 
  select(tray...2, vial...3, identifier...4) %>% 
  rename(tray = tray...2, vial = vial...3, identifier_1 = identifier...4)

cols2 <- tray2 %>% 
  select(tray...5, vial...6, identifier...7) %>% 
  rename(tray = tray...5, vial = vial...6, identifier_1 = identifier...7) %>% 
  drop_na(vial)

tray3 <- bind_rows(cols1, cols2)


# prep vial info ----------------------------------------------------------

info2 <- info1 %>% 
  mutate(date = as.Date(date),
         plot = str_replace_all(plot, "\\s+", ""),
         taxon = str_replace_all(taxon, "\\s+", ""),
         identifier_2 = paste(plot, taxon, depth, sep = "_"),
         tube = as.character(tube))

dup_vals <- info1$tube[duplicated(info1$tube)]
dup_rows1 <- info2 %>% 
  filter(tube %in% dup_vals) %>% 
  arrange(tube) 

dup_rows2 <- dup_rows1 %>% 
  select(tube, identifier_2)

dup_rows3 <-  dup_rows2 %>% 
  group_by(tube) %>% 
  mutate(n = n(),
         tube_let = paste0(tube, LETTERS[1:unique(n)]),
         identifier_2 = rep(paste(identifier_2, collapse = "_or_"), unique(n))
  ) %>% 
  ungroup() %>% 
  select(-tube) %>% 
  rename(tube = tube_let) 

dup_tubes <- dup_rows1[duplicated(dup_rows2), ]$tube %>% 
  unique()

# tube num and identifier 2
id2 <- info2 %>% 
  # removing duplicated tubes
  filter(!tube %in% dup_tubes) %>% 
  # adding reformated dup tubes back in (now have unique id's) 
  bind_rows(dup_rows3) %>% 
  select(tube, identifier_2) %>% 
  rename(identifier_1 = tube) %>% 
  as_tibble()

sum(duplicated(id2)) # should be none


# join id2  ---------------------------------------------------------------

stand_vec <- c("Tap" = "Tap", "Dummy" = "Standard", "Low" = "Standard",
               "Medium" = "Standard", "High" = "Standard")

tray4 <- tray3 %>% 
  left_join(id2, by = "identifier_1") %>% 
  mutate(identifier_2 = ifelse(is.na(identifier_2),
                               stand_vec[identifier_1],
                               identifier_2)
         )

# should be 0:
sum(is.na(tray4$identifier_2))

# saving files ------------------------------------------------------------

tray5 <- tray4 %>% 
  rename(Tray = tray, Vial = vial, `Identifier 1` = identifier_1,
         `Identifier 2` = identifier_2)

out1 <- tray5 %>% 
  filter(Tray == 1)
nrow(out1) # 54

out2 <- tray5 %>% 
  filter(Tray == 2)
nrow(out2) # 50

out1_path <- paste0("data_processed/sample_descriptions/Phal",
                    set_num, "_1.csv")


out2_path <- paste0("data_processed/sample_descriptions/Phal",
                    set_num, "_2.csv")

if (FALSE){
  write_csv(out1, out1_path)
  write_csv(out2, out2_path)
}

# calibration in file -----------------------------------------------------

id_tray1 <- rep(c("Tap", "Low", "Med", "High"), 5)
id_tray1 <- c(id_tray1, "Low", "High", rep("Tap", 32))
out1_cal <- out1 %>%
  mutate(`Identifier 1` = id_tray1,
         `Identifier 2` = id_tray1)

id_tray2 <- c(rep("Tap", 49), "Med")
out2_cal <- out2 %>%
  mutate(`Identifier 1` = id_tray2,
         `Identifier 2` = id_tray2)

# write_csv(out1_cal,"data_processed/sample_descriptions/Phal_calibration_1.csv")
# write_csv(out2_cal,"data_processed/sample_descriptions/Phal_calibration_2.csv")
