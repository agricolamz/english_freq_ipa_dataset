setwd("/home/agricolamz/work/scripts/r/english_freq_ipa_dataset")
library(tidyverse)


# load data ----------------------------------------------------------------
us <- read_tsv("https://raw.githubusercontent.com/open-dict-data/ipa-dict/master/data/en_US.txt", col_names = FALSE)
uk <- read_tsv("https://raw.githubusercontent.com/open-dict-data/ipa-dict/master/data/en_UK.txt", col_names = FALSE)
freq <- read_csv("unigram_freq.csv")

# download udpipe model ----------------------------------------------------
library(udpipe)
udpipe_download_model("english-ewt")
en_ud <- udpipe_load_model("english-ewt-ud-2.5-191206.udpipe")

# merge everything together ------------------------------------------------
us %>% 
  rename(us = X2) %>% 
  left_join(uk) %>% 
  rename(uk = X2) %>% 
  left_join(freq, by = c("X1" = "word")) %>% 
  rename(lemma = X1) %>% 
  na.omit() %>% 
  mutate(us = str_remove_all(us, "\\s"), 
         us = str_split(us, ",")) %>% 
  unnest_longer(us) %>% 
  group_by(lemma) %>% 
  slice(1) %>% 
  ungroup() ->
  result
  
udpipe_annotate(en_ud, result$lemma, parser = "none") %>% 
  as.data.frame() %>% 
  select(lemma, upos) %>% 
  right_join(result) %>% 
  na.omit() %>% 
  distinct() %>% 
  group_by(lemma) %>% 
  slice(1) %>% 
  arrange(-count) %>% 
  write_csv("english_ipa_freq_pos.csv")


