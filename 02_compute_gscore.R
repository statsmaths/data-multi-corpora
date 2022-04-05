library(tidyverse)
library(stringi)

# helper functions
read_n_parts <- function(data, fname, n)
{
  output <- vector("list", n)
  for (k in seq_len(n))
  {
    output[[j]] <- read_csv(sprintf("%s_part%02d.csv.bz2", fname, k))
  }
  bind_rows(output)
}

# which dataset to work with?
prefix <- "sv"
type <- "news"

# read the data
docs <- read_csv("output", sprintf("%s_%s_docs.csv.bz2", prefix, type))
anno <- read_n_parts("output", sprintf("%s_%s_anno", prefix, type), n = 3L)

# run the analysis
