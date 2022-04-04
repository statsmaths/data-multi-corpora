library(tidyverse)
library(cleanNLP)
library(stringi)

meta <- c(
  "de" = "deu_news_2020_1M",
  "fr" = "fra_news_2020_1M",
  "jp" = "jpn_news_2011_1M",
  "pt" = "por_news_2020_1M",
  "ru" = "rus_news_2020_1M",
  "sv" = "swe_news_2020_1M"
)

lname <- c(
  "de" = "german-gsd",
  "fr" = "french-gsd",
  "jp" = "japanese-gsd",
  "pt" = "portuguese-gsd",
  "ru" = "russian-gsd",
  "sv" = "swedish-lines"
)

for (j in seq_along(meta))
{
  prefix <- names(meta)[j]
  type <- stri_sub(stri_extract_first(meta[j], regex = "_[a-z]+_"), 2L, -2L)
  dout <- file.path("output", sprintf("%s_%s_docs.csv.gz", prefix, type))
  tout <- file.path("output", sprintf("%s_%s_anno.csv.gz", prefix, type))

  if (!file.exists(dout))
  {
    docs <- read_delim(
      file.path(meta[j], paste0(meta[j], "-sentences.txt")),
      delim = "\t",
      col_names = c("doc_id", "text"),
      quote = ""
    )

    cnlp_init_udpipe(lname[j])
    anno <- cnlp_annotate(docs)$token

    write_csv(docs, dout)
    write_csv(anno, tout)
  }
}
