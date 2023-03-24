library(tidyverse)
library(png)

img <- readPNG("../data/car.png")

mtcars %>%
  rownames_to_column() %>%
  mutate(wt = round(wt * 1000 * 0.453592, 2),
         kw = hp * 0.7457) %>%
  ggplot(aes(x = wt, y = reorder(rowname, wt), fill = kw)) +
  geom_col() +
  geom_point() +
  xlab("Svoris tonomis") +
  ylab("") +
  scale_fill_gradient(low = "green", high = "red")+
  theme_light() + 
  annotation_raster(img, xmin = 1000, xmax = 2500, ymin = 0, ymax = 10) 

ggsave("cars_test.png", dpi = 1000)