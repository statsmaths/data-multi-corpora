library(tidyverse)
library(stringi)
options(dplyr.summarise.inform = FALSE)

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

create_gscore <- function(counts)
{
  counts <- comb %>%
    group_by(key, lemma) %>%
    summarise(o11 = n()) %>%
    group_by(lemma) %>%
    mutate(o12 = sum(o11) - o11) %>%
    group_by(key) %>%
    mutate(o21 = sum(o11) - o11) %>%
    ungroup() %>%
    mutate(o22 = sum(o11) - o11 - o12 - o21)

  N <- counts$o11 + counts$o12 + counts$o21 + counts$o22
  e11 <- (counts$o11 + counts$o12) * ((counts$o11 + counts$o21) / N)
  e12 <- (counts$o11 + counts$o12) * ((counts$o12 + counts$o22) / N)
  e21 <- (counts$o21 + counts$o22) * ((counts$o11 + counts$o21) / N)
  e22 <- (counts$o21 + counts$o22) * ((counts$o12 + counts$o22) / N)

  counts$gscore <- 2 * (
    counts$o11 * log(counts$o11 / e11) +
    counts$o12 * log(counts$o12 / e12) +
    counts$o21 * log(counts$o21 / e21) +
    counts$o22 * log(counts$o22 / e22)
  )

  counts <- counts[counts$o11 > e11,]
  counts <- filter(counts, !is.na(gscore))
  counts <- arrange(counts, desc(gscore))
  counts
}

simple_gscore <- function(o11, o12, o21, o22)
{
  N <- o11 + o12 + o21 + o22
  e11 <- (o11 + o12) * ((o11 + o21) / N)
  e12 <- (o11 + o12) * ((o12 + o22) / N)
  e21 <- (o21 + o22) * ((o11 + o21) / N)
  e22 <- (o21 + o22) * ((o12 + o22) / N)

  gscore <- 2 * (
    o11 * log(o11 / e11) +
    o12 * log(o12 / e12) +
    o21 * log(o21 / e21) +
    o22 * log(o22 / e22)
  )

  gscore
}
