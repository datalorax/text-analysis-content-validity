library(tidyverse)
library(rio)
library(here)
library(janitor)
library(tidytext)
library(topicmodels)
library(ggradar)


standards <- import(here("data", "8gradescience.xlsx"), 
                    setclass = "tbl_df") %>% 
  clean_names()

webbwords <- import(here("data", "stopwords-webb.xlsx"),
                    setclass = "tbl_df")

items <- import(here("data", "G8_Sci_Items.xlsx"),
                setclass = "tbl_df") %>% 
  clean_names()

# Create the document term matrix
dtm <- standards %>% 
  select(domain, ngss_standard) %>% 
  unnest_tokens(word, ngss_standard) %>% 
  anti_join(stop_words) %>% 
  anti_join(webbwords) %>% 
  filter(word != "explanation", # Filter out any additional words we don't want in there
         word != "results") %>% 
  group_by(domain) %>% 
  count(word) %>% 
  ungroup() %>% 
  cast_dtm(domain, word, n)

# Code below produces the bar charts showing most important words within each 
# topic 
#
# tm <- LDA(dtm, k = 7, control = list(seed = 1234)) %>% 
#   tidy()
# 
# top_terms <- tm %>%
#   group_by(topic) %>%
#   top_n(5, beta) %>%
#   ungroup() %>%
#   arrange(topic, -beta)
# 
# top_terms %>%
#   mutate(term = reorder(term, beta)) %>%
#   ggplot(aes(term, beta, fill = factor(topic))) +
#   geom_col(alpha = 0.8, show.legend = FALSE) +
#   facet_wrap(~topic, scales = "free_y") +
#   coord_flip()

# Train the model
tm_raw <- LDA(dtm, k = 7, control = list(seed = 1234))

# Create a document term matrix for items 
idtm <- items %>% 
  select(item_id, prompt) %>% 
  unnest_tokens(word, prompt) %>% 
  anti_join(stop_words) %>% 
  anti_join(webbwords) %>% 
  group_by(item_id) %>% 
  count(word) %>% 
  ungroup() %>% 
  cast_dtm(item_id, word, n)

# Predict the topic for each item
posteriors <- posterior(tm_raw, newdata = idtm)

# NOTE: To create the radar charts, you may need to keep it in this format
# I'm note sure. The below is a good ggplot format.

probs <- posteriors$topics %>% 
  as.data.frame() %>% 
  mutate(item = rownames(.)) %>% 
  tbl_df() %>% 
  gather(topic, probability, -item)

# Less than ideal ggplot solution with coord_polar
theme_set(theme_minimal())
probs %>% 
  mutate(topic = as.numeric(topic),
         level = str_extract(item, "L|M|H"),
         level = factor(level, 
                        levels = c("L", "M", "H"))) %>% 
  ungroup() %>% 
  ggplot(aes(topic, probability, group = item)) +
    geom_line(lwd = 1.2, alpha = 0.2, color = "cornflowerblue") +
    geom_point(color = "gray60", alpha = 0.2) +
    coord_polar() +
    scale_color_viridis_d() +
    facet_wrap(~level) +
    guides(color = "none")

