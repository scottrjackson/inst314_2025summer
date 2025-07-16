

anscombe_tidy <- anscombe %>% mutate(obs = rownames(anscombe) ) %>%
    pivot_longer(cols = x1:y4, names_to = "xy", values_to = "value") %>%
    extract(col="xy", into = c("dim", "dataset"), regex = "(\\w)(\\d)") %>%
    pivot_wider(names_from = "dim", values_from = "value")

anscombe_bigpoint <- ggplot(anscombe_tidy, aes(x, y)) + geom_point(size = 2) +
    facet_wrap(~ dataset) + theme_classic()

png("anscombe_bigpoint.png", width = 9, height = 6, units = "in", res = 300)
print(anscombe_bigpoint)
dev.off()


anscombe_bigpoint_line <- ggplot(anscombe_tidy, aes(x, y)) + geom_point(size = 2) +
    facet_wrap(~ dataset) + theme_classic() +
    geom_smooth(method = "lm", se = FALSE)

png("anscombe_bigpoint_line.png", width = 9, height = 6, units = "in", res = 300)
print(anscombe_bigpoint_line)
dev.off()
