options(scipen = 999)
library(ggplot2)
library(RColorBrewer)
data("midwest", package = "ggplot2")

#init ggplot
ggplot(midwest, aes(x = area, y = poptotal))

#Scatterplot
ggplot(midwest, aes(x= area, y = poptotal)) + geom_point()

#Scatter plot with smoothing layer
ggplot(midwest, aes(x = area, y = poptotal)) + geom_point()+geom_smooth(method = "lm")

# Delete points outside the range (or Outliers)
g <- ggplot(midwest, aes(x= area, y = poptotal)) + geom_point() + geom_smooth(method = "lm")
g + xlim(c(0,0.1)) + ylim(c(0,1000000))

#Zoom in without deleting the points outside the limits
g1 <- g + coord_cartesian(xlim = c(0,0.1), ylim = c(0,1000000))

#Change the Title and Axis Labels
g1 + labs(title = "Area vs Population", subtitle = "From midwest dataset", y= "Population", x = "Area",
          caption = "Midwest Demographics")
or
g1 + labs(title = "Area vs Population", subtitle = "From midwest dataset") + xlab("Area") + ylab("Population")

#Full Plot
ggplot(midwest, aes(x=area, y=poptotal)) + 
  geom_point() + 
  geom_smooth(method="lm") + 
  coord_cartesian(xlim=c(0,0.1), ylim=c(0, 1000000)) + 
  labs(title="Area Vs Population", subtitle="From midwest dataset", y="Population", x="Area", caption="Midwest Demographics")

#Change the Color and Size to Static

ggplot(midwest, aes(x = area, y = poptotal)) + geom_point(col = "steelblue", size=3)+
  geom_smooth(method = "lm", col = "firebrick")+
  coord_cartesian(xlim = c(0,0.1), ylim = c(0,1000000))+
  labs(title="Area vs Population", subtitle = "From midwest dataset", y = "Population", x= "Area",
       caption = "Midwest Demographics")

#Change Color to Reflect Categories
 gg<- ggplot(midwest, aes(x = area, y = poptotal)) + geom_point(aes(col = state), size=3)+
  geom_smooth(method = "lm", col = "firebrick")+
  coord_cartesian(xlim = c(0,0.1), ylim = c(0,1000000))+
  labs(title="Area vs Population", subtitle = "From midwest dataset", y = "Population", x= "Area",
       caption = "Midwest Demographics")
plot(gg)

#Remove Legend
gg + theme(legend.position = "None")

#Change Color Palette
gg + scale_color_brewer(palette = "Set1")

#Change X and Y Axis Text and its Location

#Change breaks
gg + scale_x_continuous(breaks = seq(0,0.1,0.01))

#Change breaks and labels
gg + scale_x_continuous(breaks = seq(0,0.1,0.01), labels = letters[1:11])

#Reverse scale
gg + scale_x_reverse()

# Customized texts for Axis Labels

gg + scale_x_continuous(breaks = seq(0,0.1,0.01), labels = sprintf("%1.2f%%", seq(0,0.1,0.01)))+
  scale_y_continuous(breaks = seq(0,100000,20000000), labels = function(x) {paste0(x/1000,'K')})

#Changing theme
theme_set(theme_classic())
gg

#Adding theme layer itself
gg + theme_bw()+ labs(subtitle = "BW Theme")
gg + theme_classic() + labs(subtitle = "Classic Theme")
