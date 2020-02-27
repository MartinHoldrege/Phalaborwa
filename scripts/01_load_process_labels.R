# martin holdrege

# script started 12/16/19

# For pulling together label data



# packages ----------------------------------------------------------------

library(tidyverse)
library(readxl)


# load data ---------------------------------------------------------------

grass1 <- read_xlsx('data_raw/PhalaborwaSorted (2).xlsx', 
                    sheet = "grass",
                    skip = 1)

tree1 <- read_xlsx('data_raw/PhalaborwaSorted (2).xlsx', 
                    sheet = "tree",
                    skip = 1)



# combine sheets ----------------------------------------------------------

head(grass1)
tail(grass1)
dim(grass1)
grass2 <- grass1
names(grass2) <- names(grass1) %>% 
  str_replace("[^A-z]+$", "") %>% 
  str_replace_all("[^A-z]", "_")

grass2$group <- "grass"

head(tree1)
tail(tree1)
dim(tree1)
tree2 <- tree1
names(tree2) <- names(tree1) %>% 
  str_replace("[^A-z]+$", "") %>% 
  str_replace_all("[^A-z]", "_")

tree2 <- tree2 %>% 
  rename(date = 'date_sampled')
tree2$group = "tree"

dim(grass2)
dim(tree2)
both <- bind_rows(grass2, tree2)

dup_vals <- both$tube[duplicated(both$tube)]
dup_rows <- both %>% 
  filter(tube %in% dup_vals) %>% 
  arrange(tube) 

#write_csv(dup_rows, "data_processed/Phalaborwa_duplicated_tube_num.csv")
write_csv(both, "data_processed/PhalSorted_v1.csv")

# dates -------------------------------------------------------------------

both$date %>% unique() %>% sort()


# bin tube numbers  for sorting -------------------------------------------

sort1 <- both %>% 
  select(N, tube) %>% 
  arrange(tube) %>% 
  mutate(cut100 = cut(tube, 
                      breaks = seq(from = 0, to =  3900, by = 100),
                      right = FALSE),
         cut25 = cut(tube, 
                     breaks = seq(from = 0, to =  3900, by = 20),
                     right = FALSE),
         tube_pad = as.character(tube),
         tube_pad = ifelse(tube %in% dup_vals,
                           paste0("*", tube_pad),
                           tube_pad),
         tube_pad = str_pad(tube_pad, 
                            width = 6,
                            side = "right"),

         )

cut100u <- unique(sort1$cut100)

# sink("tube_labels_for_sort.txt")

for(cut in cut100u) {
  sort_sub <- sort1 %>% 
    filter(cut100 == cut)
  cut25_sub <- unique(sort_sub$cut25)
  for(c25 in cut25_sub) {
    vals <- sort_sub %>% 
      filter(cut25==c25) %>% 
      pull(tube_pad)
    print(paste(vals, collapse = "", sep = ""))
    print("")
    print("")
  }
  print("------------------------------")
  print("")

}
# sink()

