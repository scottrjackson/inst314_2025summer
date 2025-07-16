library(tidyverse)
library(HistData)

pl_men <- filter(PearsonLee, gp %in% "fs")
xtabs(~ par + chl, PearsonLee)
xtabs(~ par + chl, pl_men)
xtabs(~ frequency, pl_men)
xtabs(~ child + parent, pl_men)

ggplot(pl_men, aes(parent, child)) + geom_point()
ggplot(pl_men, aes(parent, child)) + geom_point(aes(size = frequency))

expand_data <- function(pl_row) {
    child <- pl_row$child
    parent <- pl_row$parent
    freq <- pl_row$frequency * 4
    expanded_data <- data.frame(child = rep(child, freq),
                                parent = rep(parent, freq))
    return(expanded_data)
}

pl_men_obs <- select(pl_men, child, parent, frequency) %>% rowwise() %>%
    do(expand_data(.))

head(pl_men_obs)

colnames(pl_men_obs) <- c("son_height", "father_height")
pl_men_obs$observation <- 1:nrow(pl_men_obs)
pl_men_obs <- select(pl_men_obs, c("observation", "father_height", "son_height"))

ggplot(pl_men_obs, aes(father_height, son_height)) + geom_point() +
    theme_minimal() + xlab("father's height (inches)") + ylab("son's height (inches)")

ggplot(pl_men_obs, aes(father_height, son_height)) + geom_jitter() + geom_smooth(method = "lm") + geom_abline(slope = 1, intercept = 0, color = "red", linetype = 2)

pl_men_counted <- pl_men_obs %>% group_by(son_height, father_height) %>%
    summarize(count = n()) %>% ungroup()

ggplot(pl_men_counted, aes(father_height, son_height)) + geom_point(aes(size = count))

summary(lm(son_height ~ father_height, data = pl_men_obs))

cor(pl_men_obs$father_height, pl_men_obs$son_height)

pl_men_obs$son_height.c <- pl_men_obs$son_height - mean(pl_men_obs$son_height)
pl_men_obs$father_height.c <- pl_men_obs$father_height - mean(pl_men_obs$father_height)
pl_men_obs$son_height.cfather <- pl_men_obs$son_height - mean(pl_men_obs$father_height)

summary(lm(son_height ~ father_height.c, data = pl_men_obs))
summary(lm(son_height.cfather ~ father_height.c, data = pl_men_obs))

mean(pl_men_obs$father_height)

ggplot(pl_men_counted, aes(father_height, son_height)) + geom_point(aes(size = count)) +
    geom_smooth(method = "lm", formula = y ~ x, aes(weight = count), se = FALSE) +
    geom_abline(intercept = 1, slope = 1, color = "red", linetype = 2)



###################
ggplot(pl_men_obs, aes(father_height, son_height)) + geom_point() +
    theme_classic() + xlab("father's height (inches)") + ylab("son's height (inches)")

ggplot(pl_men_obs, aes(father_height, son_height)) + geom_jitter() +
    theme_classic() + xlab("father's height (inches)") + ylab("son's height (inches)")
 
ggplot(pl_men_obs, aes(father_height, son_height)) + geom_jitter(alpha = .2) +
    theme_classic() + xlab("father's height (inches)") + ylab("son's height (inches)")


pl_men_counted[pl_men_counted$father_height == 68.5, ]

ggplot(pl_men_counted, aes(father_height, son_height)) + geom_point(aes(size = count)) +
    theme_classic() + xlab("father's height (inches)") + ylab("son's height (inches)")


ggplot(pl_men_counted, aes(father_height, son_height)) + geom_point(aes(size = count)) +
    geom_smooth(method = "lm", formula = y ~ x, aes(weight = count), se = FALSE) +
    geom_abline(intercept = 1, slope = 1, color = "red", linetype = 2)

pl_men_counted[pl_men_counted$father_height == 68.5, ] %>% summarize(total = sum(count))

####################################
schema_data <- data.frame(x = c(10, 40, 75), y = c(25, 50, 67))

ggplot(schema_data, aes(x, y)) +
    geom_point() +
    annotate("segment", x = 0, y = 25, xend = 8, yend = 25, arrow = arrow()) +
    annotate("segment", x = 10, y = 0, xend = 10, yend = 23, arrow = arrow()) +
    annotate("text", x = 5, y = 30, label = "x = 10") +
    annotate("text", x = 13, y = 10, label = "y = 25", angle = 270) +
    annotate("text", x = 20, y = 25, label = "point [10, 25]") +
    annotate("segment", x = 0, y = 50, xend = 38, yend = 50, arrow = arrow()) +
    annotate("segment", x = 40, y = 0, xend = 40, yend = 48, arrow = arrow()) +
    annotate("text", x = 20, y = 53, label = "x = 40") +
    annotate("text", x = 43, y = 25, label = "y = 50", angle = 270) +
    annotate("text", x = 50, y = 50, label = "point [40, 50]") +
    annotate("segment", x = 0, y = 67, xend = 73, yend = 67, arrow = arrow()) +
    annotate("segment", x = 75, y = 0, xend = 75, yend = 65, arrow = arrow()) +
    annotate("text", x = 37.5, y = 70, label = "x = 75") +
    annotate("text", x = 78, y = 33.5, label = "y = 67", angle = 270) +
    annotate("text", x = 85, y = 67, label = "point [75, 67]") +
    theme_classic() +
    scale_x_continuous(breaks = seq(0, 100, 10), limits = c(0, 100)) +
    scale_y_continuous(breaks = seq(0, 100, 10), limits = c(0, 100))

