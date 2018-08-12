# Load required packages
library(ggplot2)

# Construct data for plotting
x <- seq(from = 0, to = 15, length = 1000)
y <- dgamma(x, shape = 2, scale = 2)
df <- data.frame(x, y)

# Plot density
ggplot(df, aes(x, y)) +
  geom_line(size = 1) +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        # panel.background = element_blank(),
        # axis.line = element_line()
        panel.background = element_rect(
          colour = "black",
          size = 1
        )
  )

# Save plot
ggsave("illustrations/gamma-distribution.png", width = 6, height = 3)

# Plot histogram
set.seed(2228)
ggplot(data.frame(x = rgamma(1000, shape = 2, scale = 2)), aes(x)) +
  geom_histogram(color = "white", bins = 40) +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        # panel.background = element_blank(),
        # axis.line = element_line()
        panel.background = element_rect(
          colour = "black",
          size = 1
        )
  )

# Save plot
ggsave("illustrations/gamma-sample.png", width = 6, height = 3)
