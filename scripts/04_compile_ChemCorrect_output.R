# martin holdrege

# script started 11/24/20

# compiling chemcorrect output of picarro data.

# The goal here is to extract the necessary columns from the chemcorrect
# .xls files, combine together and then add in sample descriptions

# additionally, add sample descriptions to the raw picarro output data. 

# dependencies ------------------------------------------------------------

library(tidyverse)
library(readxl)

# read in data ------------------------------------------------------------

# * chemcorrect output ----------------------------------------------------

cc_paths <- list.files("data_processed/chemcorrect_output",
                       full.names = TRUE)

run_name <- str_extract(cc_paths, "[Pp]hal\\d+_\\d")
names(cc_paths) <- run_name

# check if multiple files for a given run
if(any(duplicated(run_name))) {
  warning("duplicated chemcorrect output files")
}

raw1 <- map(cc_paths, read_xls, sheet = "Summary", skip = 3)

# source data that went into chem correct (ie uncorrected/unaveraged)
# want to use raw 180 values
cc_source <- map(cc_paths, read_xls, sheet = "Source", skip = 2)

# * sample descriptions ---------------------------------------------------

sample_desc <- read_csv("data_processed/PhalSorted_v1.csv")


# * duplicated vial nums -------------------------------------------------

# vials ids that are not unique to a plot/spp
dup <- read_csv("data_processed/Phalaborwa_duplicated_tube_num.csv") %>% 
  mutate(tube = as.character(tube))


# * 'raw' picarro output --------------------------------------------------

pic_output1 <- read_csv("data_processed/Phal_combined_picarro_output.csv")

# clean -------------------------------------------------------------------

# avg raw 180 values

cc_source_mean <- map(cc_source, function(df) {
  df %>% 
    group_by(Sample, `Identifier 1`) %>% 
    summarize(raw_18o_mean = mean(as.numeric(`d(18_16)Mean`))) %>% 
    mutate(Sample = as.numeric(Sample))
})

raw2 <- map2(raw1, cc_source_mean, .f = left_join, 
             by = c("Sample", "Name" = "Identifier 1"))

# combining chem correct files
cc1 <- bind_rows(raw2, .id = "run")

names(cc1) <- janitor::make_clean_names(names(cc1))

cc2 <- cc1 %>% 
  rename(cal_18o_mean = calibrated_d_sup_18_sup_o_mean,
         cal_2h_mean = calibrated_d_sup_2_sup_h_mean)

# remove standards
cc3 <- cc2 %>% 
  filter(!name %in% c("Low", "Medium", "High", "Dummy", "Tap"))


# * fix vial nums -------------------------------------------------------

# these are now corrected in the phal google doc but weren't correct
# when run through chemcorrect. 
cc3[cc3$run == "Phal7_1" & cc3$sample == 42, ]$name <- "1698"
cc3[cc3$run == "Phal15_1" & cc3$sample == 30, ]$name <- "2935"


# check for duplicates
dup_cc_name <- cc3$name[duplicated(cc3$name)] %>% 
  unique()

# 2526 is a genuine duplicate (that should have been labled A, B)
dup_cc_name

sample_desc <- sample_desc %>% 
  mutate(tube = as.character(tube))



# join chemcorrect output w/ descriptions ---------------------------------

# combined file
comb1 <- cc3 %>% 
  mutate(cc_file = cc_paths[run],
         #removing A, B etcof duplicate samples b/ those unique
         # vial letters were not in original data
         id4join = str_replace(name, "[AaBbCcDd]$", "")) %>% 
  select(name, id4join, raw_18o_mean, cal_18o_mean, cal_2h_mean, cc_file) %>% 
  left_join(sample_desc, by = c("id4join" = "tube")) %>% 
  rename(vial_id = name,
         original_id = id4join) %>% 
  select(-cc_file, cc_file)  # least important col last


nrow(cc3)
nrow(comb1)

# joined file exluding vials with non-unique taxa etc. 
comb_good1 <- comb1 %>% 
  filter(!original_id %in% dup$tube)

# file that has non -unqiue vials
comb_dup <-  comb1 %>% 
  filter(original_id %in% dup$tube)

# should only be one duplicated vial
comb_good1$vial_id %>% duplicated() %>% sum

# n good samples
length(unique(comb_good1$vial_id))


# join picarro output with sample descriptions -----------------------------

sample_desc
pic_output2 <- pic_output1 %>% 
  mutate(original_id = str_replace(`Identifier 1`, "[AaBbCcDd]$", "")) %>% 
  # only join in sample labels for unique vials
  left_join(filter(sample_desc, !tube %in% dup$tube), by = c("original_id" = "tube"))

if (nrow(pic_output1) != nrow(pic_output2)) {
  warning("rows added in left join")
}


# save files --------------------------------------------------------------

write_csv(comb_good1, "data_processed/Phal_combined_cc_output.csv")

write_csv(comb_dup, "data_processed/Phal_combined_cc_output_non-unique-vials.csv")

write_csv(pic_output2, "data_processed/Phal_combined_picarro_output_w_descripts.csv")
