airquality
vis_dat(airquality)
airquality <- na.omit(airquality)
model <- lm(Ozone ~ Solar.R, data = airquality)

## I chose Solar. R because the ozone/ atmosphere is relative to the solar radiation that the earth holds in(?). 

summary(model)
plot(model)


