## -----------------------------------------------------------------------------
c(0, .3, 0.91, 0.95, .99, .999, .9999, .99999, 1) %>% 
  enframe() %>% 
  mutate(time_weight = as.character(value)
         , tw = map(value, ~get_time_weights(1:100, time_weight = .x) %>% 
                      enframe)) %>%
  select(time_weight, tw) %>% 
  unnest(tw) %>% 
  ggplot(aes(name, value, col = time_weight))+
  geom_line()+
  labs(x = "Time series index", y = "Weight")

