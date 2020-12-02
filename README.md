# Phalaborwa
pre and post processing phalaborwa picarro data

## Description of folders:
scripts >
  where my R scripts live
  
data_raw >
  misc raw data files (eg sample descriptions)--idea is that these 
  are files that I don't edit--just load into R
  picarro_output >
    raw output files from picarro
    
data_processed >
  output files from some analysis etc.
  e.g. when something is done to a raw data file this is where it ends
  up.
  bad_vials >
    each picarro run data from some vials is thrown out(e.g. those that
    were flagged by picarro or had all missing values),
    these are csvs documenting from what vials data was discarded prior
    to further cleaning
  cean_4cc >
    files that have been pre-processed for chem correct
  sample_descriptions >
    sample descriptions that have been cleaned up in R such that they can be
    read by the picarro. 
    
## description of select data files

`data_processed/Phal_combined_picarro_output.csv`--the raw combined picarro output 
(Id's are correct)

`data_processed/Phal_combined_picarro_output_w_descriptions.csv`--the raw combined picarro output 
(Id's are correct). Sample descriptions joined in (sample descriptions left blank 
for vials that don't have unique descriptions)

`data_processed/Phal_combined_cc_output.csv`--the combined chemcorrect output
with sample descriptions joined in. Vials that don't have unique sample descriptions
are not included here. 


## Descriptions of scripts

Note: leading numbers on script names denotes the order in which the scripts are used.

`standard_calculation.R` --script used to calculate slope/intercept  to be 
put into the picarro.

`01_load_process_labels.R` --process sample (vial number) descriptions

`02_create_sample_discriptions.R` --Used to make sample description files for 
each run of the picarro. 

`03_Process4ChemCorrect.R` --process the picarro output so that it can be used
by chemcorrect. Also makes a combined file of the raw picarro output. The raw
csv files from the picarro have wrong identifiers for some samples b/ of bug in autosampler.
This script fixes those. 

`04_compile_ChemCorrect_output.R` --pulls together the data from the chemcorrect
.xls files, and joins in sample descriptions. Also joins sample descriptions
to combined 'raw' picarro output. 


## Misc notes:
 Phal1-phal17 ran through chem correct on 11/20/20. Note that phal1-phal9 
 and phal11-phal13 were run with option 'injections to ignore' = 1. The rest with 0. Going forward using 0 is maybe better (although I'm not sure it makes a difference). 