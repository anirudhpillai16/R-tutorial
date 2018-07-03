options(scipen = 999)
library(ggplot2)
data("midwest", package = "ggplot2")
theme_set(theme_bw())

gg <- ggplot(midwest, aes(x=area, y= poptotal))+
  geom_point(aes(col = state, size = popdensity))+
  geom_smooth(method = "loess", se =F) + xlim(c(0,0.1)) + ylim(c(0,500000))+
  labs(title = "Area vs Population", y= "Population", x = "Area", caption = "Source: midwest")
plot(gg)

#Modify Theme Component 

gg + theme(plot.title=element_text(size=20, 
                                   face="bold", 
                                   family="American Typewriter",
                                   color="tomato",
                                   hjust=0.5,
                                   lineheight=1.2),  # title
           plot.subtitle=element_text(size=15, 
                                      family="American Typewriter",
                                      face="bold",
                                      hjust=0.5),  # subtitle
           plot.caption=element_text(size=15),  # caption
           axis.title.x=element_text(vjust=10,  
                                     size=15),  # X axis title
           axis.title.y=element_text(size=15),  # Y axis title
           axis.text.x=element_text(size=10, 
                                    angle = 30,
                                    vjust=.5),  # X axis text
           axis.text.y=element_text(size=10))  # Y axis text
#Change legend with labs

gg+ labs(color = "State", size = "Density")

#Change legend using guides

gg + guides(color = guide_legend("State"), size = guide_legend("Density"))

#Change legend using scale_aesthetic_vartype

gg + scale_color_discrete(name ="State") + scale_size_continuous(name = "Density", guide = FALSE)

#Change legend labels and point colors for categories 

gg + scale_color_manual(name="State", 
                        labels = c("Illinois", 
                                   "Indiana", 
                                   "Michigan", 
                                   "Ohio", 
                                   "Wisconsin"), 
                        values = c("IL"="blue", 
                                   "IN"="red", 
                                   "MI"="green", 
                                   "OH"="brown", 
                                   "WI"="orange"))
#Change order of legend

gg + guides(colour = guide_legend(order = 1),
            size = guide_legend(order =2 ))

#Style Legend, Title and Key

gg + theme(legend.title = element_text(size=12, color = "firebrick"), 
           legend.text = element_text(size=10),
           legend.key=element_rect(fill='springgreen')) + 
  guides(colour = guide_legend(override.aes = list(size=2, stroke=1.5))) 


#No Legend

gg +theme(legend.position = "None") + labs(subtitle = "No Legend")

#Legend to the left

gg + theme(legend.position = "left") + labs(subtitle = "Legend on the Left")

#Legend at the bottom and horizontal

gg + theme(legend.position = "bottom", legend.box = "horizontal") + labs(subtitle = "Legend at Bottom")


#Legend at bottom-right, inside the plot

gg + theme(legend.title = element_text(size=12, color = "salmon", face="bold"),
           legend.justification=c(1,0), 
           legend.position=c(0.95, 0.05),  
           legend.background = element_blank(),
           legend.key = element_blank()) + 
  labs(subtitle="Legend: Bottom-Right Inside the Plot")


gg + theme(legend.title = element_text(size=12, color = "salmon", face="bold"),
           legend.justification=c(0,1), 
           legend.position=c(0.05, 0.95),
           legend.background = element_blank(),
           legend.key = element_blank()) + 
  labs(subtitle="Legend: Top-Left Inside the Plot")

# Filter required rows.
midwest_sub <- midwest[midwest$poptotal > 300000, ]
midwest_sub$large_county <- ifelse(midwest_sub$poptotal > 300000, midwest_sub$county, "")


# Base Plot
gg <- ggplot(midwest, aes(x=area, y=poptotal)) + 
  geom_point(aes(col=state, size=popdensity)) + 
  geom_smooth(method="loess", se=F) + xlim(c(0, 0.1)) + ylim(c(0, 500000)) + 
  labs(title="Area Vs Population", y="Population", x="Area", caption="Source: midwest")

# Plot text and label

gg + geom_text(aes(label = large_county), size =2, data = midwest_sub) + labs(subtitle = "With ggplot2:: geom_text")+
  theme(legend.position = "None")

gg + geom_text(aes(label = large_county), size = 2, data = midwest_sub, alpha = 0.25) + labs(substitute= "With ggplot2:: geom_text")+
  theme(legend.position = "None")


library(ggrepel)
gg + geom_text_repel(aes(label=large_county), size=2, data=midwest_sub) + labs(subtitle="With ggrepel::geom_text_repel") + theme(legend.position = "None") 


gg + geom_label_repel(aes(label=large_county), size=2, data=midwest_sub) + labs(subtitle="With ggrepel::geom_label_repel") + theme(legend.position = "None")  


#How to Add Annotations Anywhere inside Plot
library(grid)
my_text <- "This text is at x=0.7 and y=0.8!"
my_grob = grid.text(my_text, x = 0.7, y = 0.8, gp = gpar(col = "firebrick", fontsize = 14, fontface = "bold"))
gg + annotation_custom(my_grob)


#Flip the X and Y axis

gg + coord_flip()

#Reverse Scale of Axis

gg + scale_x_reverse() + scale_y_reverse()


#Faceting: Draw multiple plots within one figure

data(mpg, package = "ggplot2")
g <- ggplot(mpg, aes(x = displ, y = hwy)) + geom_point()+
  labs(title = "hwy vs displ", caption = "Source: mpg") +
  geom_smooth(method = "lm", se = FALSE)
plot(g)


# Base Plot
g <- ggplot(mpg, aes(x=displ, y=hwy)) + 
  geom_point() + 
  geom_smooth(method="lm", se=FALSE) + 
  theme_bw()  # apply bw theme

# Facet wrap with common scales
 g + facet_wrap(~class, nrow = 3) + labs(title = "hwy vs displ", caption = "Source:mpg", subtitle = "
                                         Ggplot2 - Faceting - Multiple plots in one figure")
 
 # Facet wrap with free scales
g + facet_wrap( ~ class, scales = "free") + labs(title="hwy vs displ", caption = "Source: mpg", subtitle="Ggplot2 - 
                                                  Faceting - Multiple plots in one figure with free scales")  

#Add Facet grid
g1 <-g + facet_grid(manufacturer ~class)
plot(g1)

#Facet grid with Cyclinder
g2 <- g + facet_grid(cyl ~ class)
plot(g2)

#Multiple plots in same figure
library(gridExtra)
gridExtra::grid.arrange(g1,g2, ncol = 2)

#Change Plot Background
g + theme(panel.background = element_rect(fill = 'khaki'),
          panel.grid.major = element_line(colour = "burlywood", size=1.5),
          panel.grid.minor = element_line(colour = "tomato", 
                                          size=.25, 
                                          linetype = "dashed"),
          panel.border = element_blank(),
          axis.line.x = element_line(colour = "darkorange", 
                                     size=1.5, 
                                     lineend = "butt"),
          axis.line.y = element_line(colour = "darkorange", 
                                     size=1.5)) +
  labs(title="Modified Background", 
       subtitle="How to Change Major and Minor grid, Axis Lines, No Border")


# Change Plot Margins -----------------------------------------------
g + theme(plot.background=element_rect(fill="salmon"), 
          plot.margin = unit(c(2, 2, 1, 1), "cm")) +  # top, right, bottom, left
  labs(title="Modified Background", subtitle="How to Change Plot Margin")  
#How to Remove Major and Minor Grid, Change Border, Axis Title, Text and Ticks
g + theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          panel.border = element_blank(),
          axis.title = element_blank(), 
          axis.text = element_blank(),
          axis.ticks = element_blank()) +
  labs(title="Modified Background", subtitle="How to remove major and minor axis grid, border, axis title, text and ticks") 

# Add an Image in Background
library(png)
img <- png::readPNG("C:/R/R Tutorials/Performance by Operator.png")  # source: https://www.r-project.org/
g_pic <- rasterGrob(img, interpolate=TRUE)

g + theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          plot.title = element_text(size = rel(1.5), face = "bold"),
          axis.ticks = element_blank()) + 
  annotation_custom(g_pic, xmin=5, xmax=7, ymin=30, ymax=45)
