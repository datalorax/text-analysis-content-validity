dtm2 <- standards %>% 
  select(code, ngss_standard) %>% 
  unnest_tokens(word, ngss_standard) %>% 
  anti_join(stop_words) %>% 
  anti_join(webbwords) %>% 
  filter(word != "explanation", # Filter out any additional words we don't want in there
         word != "results") %>% 
  group_by(code) %>% 
  count(word) %>% 
  ungroup() %>% 
  cast_dtm(code, word, n)

# Evaluate fit of 2 to 25 topics
search_standards <- FindTopicsNumber(dtm2, 
                                     topics = 2:25,
                                     metrics = c("Griffiths2004", 
                                                 "CaoJuan2009", 
                                                 "Arun2010", 
                                                 "Deveaud2014"),
                                     method = "Gibbs",
                                     control = list(seed = 77),
                                     mc.cores = 4L)

stats_standards <- apply(search_standards[ ,-1], 
      2, 
      function(x) rescale(x, c(0, 1), range(x))) %>%
  as_tibble() %>%
  rowid_to_column("Topics") %>%
  gather(metric, val, -Topics) %>%
  mutate(criterion = ifelse(metric == "CaoJuan2009" |
                            metric == "Arun2010",
                            "min",
                            "max"))

p1 <- ggplot(filter(stats_standards, criterion == "min"),
       aes(Topics, val)) +
  annotate("rect",
           xmin = 3, xmax = 6,
           ymin = 0, ymax = 1,
           fill = "magenta",
           alpha = 0.2) +
  geom_line(lwd = 1.2, 
            color = "cornflowerblue") +
  geom_point(color = "gray40") +
  facet_wrap(~metric) +
  labs(x = NULL,
       y = NULL,
       title = "Minimize") +
  theme(plot.title = element_text(hjust = 0.5,
                                  vjust = -3,
                                  face = "bold"),
        panel.spacing = unit(2, "lines"))


p2 <- ggplot(filter(stats_standards, criterion == "max"),
       aes(Topics, val)) +
  annotate("rect",
           xmin = 3, xmax = 6,
           ymin = 0, ymax = 1,
           fill = "magenta",
           alpha = 0.2) +
  geom_line(lwd = 1.2, 
            color = "cornflowerblue") +
  geom_point(color = "gray40") +
  facet_wrap(~metric) +
  labs(x = "Number of Topics",
       y = NULL,
       title = "Maximize") +
  theme(plot.title = element_text(hjust = 0.5,
                                  vjust = -3,
                                  face = "bold"),
        panel.spacing = unit(2, "lines"))

p1 / p2

ggsave("topic_selection/standard-level/topic-selection.pdf", 
       width = 6.5, 
       height = 9)

# Evaluate 6:12 topics substantively
tm_standards <- map(3:16, ~LDA(dtm2, k = ., control = list(seed = 1234)))
tidied_standards <- map_df(tm_standards, 
                       ~tidy(.) %>%
                          group_by(topic) %>%
                          arrange(desc(beta)) %>%
                          slice(1:15) %>%
                          ungroup() %>%
                          mutate(topic = factor(topic, 
                                            levels = 1:16,
                                            labels = paste("Topic", 1:16))),
                          .id = "extracted")

gammas_max <- map(tm_standards, 
                 ~tidy(., "gamma") %>%
                    group_by(document) %>%
                    filter(gamma == max(gamma)) %>%
                    rename(assigned = topic) %>%
                    select(-gamma))

gammas <- map2_df(tm_standards, gammas_max,
     ~left_join(tidy(., "gamma"), .y) %>%
        ungroup() %>%
        mutate(document = str_sub(document, 4)),
        .id = "extracted")

gammas <- gammas %>%
  nest(-extracted) %>%
  mutate(heatmap = map(data, ~
            ggplot(., aes(topic, fct_reorder(document, assigned))) +
              geom_tile(aes(fill = gamma),
                        color = "gray40") +
              scale_fill_viridis_c(name = "Gamma\n ", 
                                   option = "magma") +
              scale_x_continuous(breaks = 1:16, labels = 1:16) +
              labs(x = "Topic", 
                   y = "Standard Code") +
              coord_cartesian(expand = FALSE) +
              theme(legend.position = "bottom",
                    axis.text.y = element_text(size = 8),
                    legend.key.width = unit(2, "cm"))))

dir.create("topic_selection/standard-level/heatmaps-standards_topics")
walk2(gammas$heatmap, 
      paste0("topic_selection/standard-level/heatmaps-standards_topics/solution_", 
             as.numeric(gammas$extracted) + 2,
             ".pdf"),
      ~ggsave(.y, .x, 
              device = "pdf",
              width = 6.5, 
              height = 9))

word_plots <- tidied_standards %>%
  nest(-extracted) %>%
  mutate(words = map(data, 
                    ~mutate(., 
                      reordered = reorder_within(term, beta, topic)) %>%
                    ggplot(aes(reordered, beta)) +
                      geom_col(fill = "#6272F8",
                               alpha = 0.7) +
                      coord_flip() +
                      scale_x_reordered(name = "") +
                      facet_wrap(~topic, scales = "free_y")))

dir.create("topic_selection/standard-level/word_freq")

walk2(word_plots$words, 
      paste0("topic_selection/standard-level/word_freq/solution_", 
             as.numeric(word_plots$extracted) + 2,
             ".pdf"),
      ~ggsave(.y, .x, 
              device = "pdf",
              width = 24,
              height = 12))