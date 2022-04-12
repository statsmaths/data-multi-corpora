source("funs.R")

##############################################################################
# load the data
prefix <- "fr"
type <- "news"

# read the data
docs <- read_csv(sprintf("output/%s_%s_docs.csv.bz2", prefix, type))
anno <- read_n_parts(sprintf("output/%s_%s_anno", prefix, type), n = 3L)

##############################################################################
# 1. near synonyms

comb <- anno %>%
  filter(lemma %in% c("élevé", "haut")) %>%
  select(doc_id, sid, tid = tid_source, key = lemma) %>%
  inner_join(anno, by = c("doc_id", "sid", "tid")) %>%
  filter(upos == "NOUN") %>%
  select(lemma, key)

counts <- create_gscore(comb)
group_split(group_by(counts, key))

##############################################################################
# 2. she said / he said

comb <- anno %>%
  filter(lemma %in% c("garçon", "fille")) %>%
  filter(relation == "nsubj") %>%
  select(doc_id, sid, tid = tid_source, key = lemma) %>%
  inner_join(anno, by = c("doc_id", "sid", "tid")) %>%
  filter(upos == "VERB") %>%
  select(lemma, key)

counts <- create_gscore(counts)
group_split(group_by(counts, key))

##############################################################################
# 3. a _____ of

anno %>%
  filter(lag(lemma) %in% c("un", "une"), lead(lemma) == "de") %>%
  group_by(lemma) %>%
  summarise(n = n()) %>%
  arrange(desc(n))
