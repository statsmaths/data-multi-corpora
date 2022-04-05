library(tidyverse)
library(reticulate)
library(cleanNLP)
library(stringi)

# USE THESE TO SETUP SPACY IN THE TERMINAL
# python3 -m venv env
# source env/bin/activate
# pip3 install spacy
# pip3 install cleannlp
# python -m spacy download pt_core_news_sm
# python -m spacy download ru_core_news_sm
# python -m spacy download ja_core_news_sm
# python -m spacy download fr_core_news_sm
# python -m spacy download de_core_news_sm

# set the virtual environment in R
use_virtualenv("./env", required = TRUE)

# helper function
save_n_parts <- function(data, fname, n)
{
  id <- rep(seq_len(n), each = ceiling(nrow(data) / 3))[seq_len(nrow(data))]
  for (k in seq_len(n))
  {
    fout <- sprintf("%s_part%02d.csv.bz2", fname, k)
    write_csv(data[id == k,], fout)
  }
}

# read metadata about all of the datasets we want to parse
meta <- read_csv("input/meta.csv")

# cycle over each dataset
for (j in seq_len(nrow(meta)))
{
  prefix <- meta$lcode[j]
  type <- meta$type[j]
  dout <- file.path("output", sprintf("%s_%s_docs.csv.bz2", prefix, type))
  tout <- file.path("output", sprintf("%s_%s_anno", prefix, type))

  if (!file.exists(dout))
  {
    cat(sprintf("Working on %s\n", meta$dname[j]))
    docs <- read_delim(
      file.path(meta$dname[j], paste0(meta$dname[j], "-sentences.txt")),
      delim = "\t",
      col_names = c("doc_id", "text"),
      quote = ""
    )

    if (meta$model_type[j] == "spacy")
    {
      cnlp_init_spacy(meta$model[j])
    } else {
      cnlp_init_udpipe(meta$model[j])
    }

    anno <- cnlp_annotate(docs)$token

    write_csv(docs, dout)
    save_n_parts(anno, tout, n = 3)
  }
}
