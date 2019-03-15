# Evaluate fit of 2 to 25 topics
search_standards <- FindTopicsNumber(dtm, 
                                     topics = 2:25,
                                     metrics = c("Griffiths2004", 
                                                 "CaoJuan2009", 
                                                 "Arun2010", 
                                                 "Deveaud2014"),
                                     method = "Gibbs",
                                     control = list(seed = 77),
                                     mc.cores = 2L)

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
  geom_line(lwd = 1.2, 
            color = "cornflowerblue") +
  facet_wrap(~metric) +
  labs(x = NULL,
       y = NULL,
       title = "Minimize") +
  theme(plot.title = element_text(hjust = 0.5,
                                  face = "bold"))


p2 <- ggplot(filter(stats_standards, criterion == "max"),
       aes(Topics, val)) +
  geom_line(lwd = 1.2, 
            color = "cornflowerblue") +
  facet_wrap(~metric) +
  labs(x = "Number of Topics",
       y = NULL,
       title = "Maximize") +
  theme(plot.title = element_text(hjust = 0.5,
                                  face = "bold"))

p1 / p2

# Evaluate 6:12 topics substantively
tm_standards <- map(6:12, ~LDA(dtm, k = ., control = list(seed = 1234)))
tidied_standards <- map_df(tm_standards, 
                       ~tidy(.) %>%
                          group_by(topic) %>%
                          arrange(desc(beta)) %>%
                          slice(1:15) %>%
                          ungroup() %>%
                          mutate(topic = factor(topic, 
                                            levels = 1:12,
                                            labels = paste("Topic", 1:12))),
                          .id = "extracted")

plots <- tidied_standards %>%
  nest(-extracted) %>%
  mutate(plot = map(data, 
                    ~mutate(., 
                      reordered = reorder_within(term, beta, topic)) %>%
                    ggplot(aes(reordered, beta)) +
                      geom_col(fill = "#6272F8",
                               alpha = 0.7) +
                      coord_flip() +
                      scale_x_reordered(name = "") +
                      facet_wrap(~topic, scales = "free_y")))

#dir.create("topic_selection")

walk2(plots$plot, 
      paste0("topic_selection/solution_", 
             as.numeric(plots$extracted) + 5,
             ".pdf"),
      ~ggsave(.y, .x, 
              device = "pdf",
              width = 24,
              height = 12))

