

library("ggplot2")
library("ggimage")

set.seed(2017 - 02 - 21)
d <- data.frame(
  x = rnorm(10),
  y = rnorm(10),
  image = sample(
    c(
      "https://www.r-project.org/logo/Rlogo.png",
      "https://jeroenooms.github.io/images/frink.png"
    ),
    size = 10,
    replace = TRUE
  )
)

d$icon = sample(c('power', 'wifi', 'pie-graph', 'usb'), 10, replace =TRUE)
ggplot(d, aes(x, y)) + geom_icon(aes(image = icon), color = "grey") + viridis::scale_color_viridis(discrete = F)
