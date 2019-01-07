# Load required packages
library(dplyr)
library(lattice)
library(plotly)

# Ames housing data
ames <- AmesHousing::make_ames()

# Training data
temp <- ames %>%
  filter(Year_Sold <= 2008) %>%
  # mutate(Log_Sale_Price = log(Sale_Price)) %>%
  select(x1 = Gr_Liv_Area, x2 = Year_Built, y = Sale_Price)

# Fitted models
fit1 <- lm(y ~ x1 + x2, data = temp)
fit2 <- lm(y ~ x1 * x2, data = temp)

# Regression plane data
x1 <- seq(from = min(trn$x1), to = max(trn$x1), length = 100)
x2 <- seq(from = min(trn$x2), to = max(trn$x2), length = 100)
reg_plane <- expand.grid(x1 = x1, x2 = x2)
reg_plane$y1 <- predict(fit1, newdata = reg_plane)
reg_plane$y2 <- predict(fit2, newdata = reg_plane)

# Level plots
p1 <- levelplot(
  x = y1 ~ x1 * x2, 
  data = reg_plane, 
  col.regions = viridis::viridis(100),
  xlab = "Gr_Liv_Area",
  ylab = "Year_Built",
  zlab = "log(Sale_Price)"
)
p2 <- levelplot(
  x = y2 ~ x1 * x2, 
  data = reg_plane, 
  col.regions = viridis::viridis(100),
  xlab = "Gr_Liv_Area",
  ylab = "Year_Built",
  zlab = "log(Sale_Price)"
)
gridExtra::grid.arrange(p1, p2, nrow = 1)

# Plotly -----------------------------------------------------------------------

# Draw (interactive) 3-D scatterplot w/ fitted regression plane
p1 <- plot_ly() %>%
  # Add (3D) scatterplot
  add_markers(x = trn$x, y = trn$y, z = trn$z, opacity = 0.3, size = 5) %>%
  # Add fitted regression plane
  add_surface(x = x, y = y, z = z, showscale = FALSE) %>%
  # Add axis labels and titles
  layout(
    scene = list(
      zaxis = list(title = "Log Sale_Price"),
      yaxis = list(title = "Year_Built"),
      xaxis = list(title = "Gr_Liv_Area")
    )
  ) %>%
  # Remove legend
  hide_guides()

# Export as static image
orca(p1, file = "ames-main-effects.png", scale = 0.7)
system("mv ames-main-effects.png illustrations")

# Draw (interactive) 3-D scatterplot w/ fitted regression plane
p2 <- plot_ly() %>%
  # Add (3D) scatterplot
  add_markers(x = trn$x, y = trn$y, z = trn$z, opacity = 0.3, size = 5) %>%
  # Add fitted regression plane
  add_surface(x = x, y = y, z = z2, showscale = FALSE) %>%
  # Add axis labels and titles
  layout(
    scene = list(
      zaxis = list(title = "Log Sale_Price"),
      yaxis = list(title = "Year_Built"),
      xaxis = list(title = "Gr_Liv_Area")
    )
  ) %>%
  # Remove legend
  hide_guides()

# Export as static image
orca(p2, file = "ames-interaction.png", scale = 0.7)
system("mv ames-interaction.png illustrations")

