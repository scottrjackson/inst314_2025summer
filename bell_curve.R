library(ggplot2)

x <- seq(-3, 3, .01)
y_dens <- dnorm(x)

plot_data <- data.frame(x, y_dens)

png("bell_curve.png", height = 1, width = 1, units = "in", res = 300)
ggplot(plot_data, aes(x, y_dens)) + geom_line() + theme_void() +
	geom_vline(aes(xintercept = 0), linetype = 2) +
	annotate("text", x = 0.4, y = 0.1, label = "mu", parse = TRUE) +
	geom_segment(x = 0, y = dnorm(1), xend = 1, 
		linetype = 1, arrow = arrow(ends="both", length = unit(0.05, "inches"))) +
	annotate("text", x = 1.35, y = dnorm(1), label = "sigma", parse = TRUE)
dev.off()

