airquality
vis_dat(airquality)
airquality <- na.omit(airquality)
model <- lm(Ozone ~ Solar.R, data = airquality)

## I chose Solar. R because the ozone/ atmosphere is relative to the solar radiation that the earth holds in(?). 

summary(model)
plot(model)

## I think this is a valid model because the p-value is 0.0001793 and the R^2 value is 0.1213. 
## The R^2 valued shows that 12% of the variability in the ozone levels can be further explained by solar radiation. 

library(broom)
a <- augment(model, data = airquality)

library(ggplot2)
ggplot(a, aes(x = Ozone, y = .fitted)) +
  geom_point() + 
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(
    title = "Actual VS Predicted Ozone Levels",
    x = "Actual Ozone",
    y = "Predicted Ozone",
    subtitle = paste("Correlation:", round(cor(a$Ozone, a$.fitted), 2))
  ) +
  theme_minimal()
