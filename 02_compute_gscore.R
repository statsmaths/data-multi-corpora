library(tidyverse)
library(stringi)

# helper functions
read_n_parts <- function(fname, n)
{
  output <- vector("list", n)
  for (k in seq_len(n))
  {
    output[[k]] <- read_csv(sprintf("%s_part%02d.csv.bz2", fname, k))
  }
  bind_rows(output)
}

# which dataset to work with?
prefix <- "fr"
type <- "news"

# read the data
docs <- read_csv(sprintf("output/%s_%s_docs.csv.bz2", prefix, type))
anno <- read_n_parts(sprintf("output/%s_%s_anno", prefix, type), n = 3L)

# run the analysis
anno %>%
  filter(lag(token) %in% c("un", "une"), lead(token) %in% "de") %>%
  group_by(lemma) %>%
  summarise(n = n()) %>%
  arrange(desc(n))
