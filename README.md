# Phalaborwa
pre and post processing phalaborwa picarro data

Description of folders:
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
    each picarro run data from some vials is thrown out,
    these are csvs documenting from what vials data was discarded prior
    to further cleaning
  cean_4cc >
    files that have been pre-processed for chem correct
  sample_descriptions >
    sample descriptions that have been cleaned up in R such that they can be
    read by the picarro. 

Misc notes:
 Phal1-phal17 ran through chem correct on 11/20/20. Note that phal1-phal9 
 and phal11-phal13 were run with option 'injections to ignore' = 1. The rest with 0. Going forward using 0 is better (although I'm not sure it makes a difference). 
