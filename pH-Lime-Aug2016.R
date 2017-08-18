# Scatter plot of pH data lime


library(ggplot2)
library(ggthemr)


# Example data: "scatter.txt" (tab separated)
# Week	pH	Treatment	Location
# 0	5.58	Zero	Soil
# 2	5.245	Zero	Rhizo
# 4	5	Zero	Soil
# 5	4.9025	Zero	Soil
# 0	4.9	Zero	Rhizo
# 2	4.85	Zero	Rhizo
# 4	4.845	Zero	Rhizo
# 5	5.1375	Zero	Soil
# 0	5.17	High	Soil
# 2	5.0425	High	Soil
# 4	5.3025	High	Rhizo
# 5	5.155	High	Rhizo
# 6	5.6275	High	Soil

poo <-read.table("phAll.txt", header = TRUE, sep='\t')

# ie four variables: week, pH, treatment, location


k <- ggplot(poo, aes(x=Week,y=pH)) + 
  geom_point(aes(color=Treatment, shape=shapeL),
             size = 5) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) 

k # print and check

# change colours 
k2 <- k + theme_bw() + 
  theme(text = element_text(size=18, color = "black")) 



## Editing title to make space beneath it and to change size and font face

#k2 + theme(plot.title = element_text(size=12, face="bold", vjust=2)) 

library(grid)

K3 <- k2 + theme(axis.text.x=element_text(size=14, vjust=0.5, color = "black"), 
                 axis.text.y=element_text(size=14, vjust=0.5, color = "black"),
                 axis.title.x=element_text(size=16, vjust=0.25, face = "bold", color="black"),
                 axis.title.y=element_text(size=16, vjust=1, face = "bold", color="black"),
                 legend.title=element_text(size=16, vjust=1),
                 legend.text=element_text(size=16, vjust=0.5))

k4 <- K3 +  guides(color = guide_legend(override.aes = list(size=7))) 

k5 <- k4 +   theme(legend.key = element_rect(size = 5),
                   legend.key.size = unit(1.5, 'lines')) 
k5


k6 <- k5 + labs(color="   Lime\nTreatment\n")
k6  


# change colours
dark_cols <- c("black", "darkgreen", "goldenrod1", "darkorange3", "blue4", 
               "lightslateblue", "seagreen1", "indianred1", 'lightgoldenrod1', 
               "chartreuse2", "mediumturquoise", "yellow1", "orchid", "deeppink2", 
               "magenta4")


DarkCols1 <- c("#555555", dark_cols)
# remove previous effects:
ggthemr_reset()
# Define colours for your figures with define_palette
darkCols <- define_palette(
  swatch = DarkCols1, # colours for plotting points and bars
  gradient = c(lower = DarkCols1[1L], upper = DarkCols1[2L]), #upper and lower colours for continuous colours
  background = "white" #defining a grey-ish background 
)
# set the theme for your figures:
ggthemr(darkCols)


# reprint figure

k6



## redo with adding my rhizosphere ph data to plot


poo <- read.table("all-ph-incCam.txt", header = TRUE, sep='\t')

# set factor order to make shapes better. 

poo[["shapeL"]] <- setFactorOrder(poo[["shapeL"]], c("Edna", "Cam"))

# libraries needed:

library(ggplot2)


k <- ggplot(poo, aes(x=Week,y=pH)) + 
  geom_point(aes(color=Treatment, shape=shapeL),
             size = 5) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) 

k # print and check

# change colours 
k2 <- k + theme_bw() + 
  theme(text = element_text(size=18, color = "black")) 


## now add axis labels and title   
#k3 <- k2 +  labs(x = "Week of trial") 

## Editing title to make space beneath it and to change size and font face

#k2 + theme(plot.title = element_text(size=12, face="bold", vjust=2)) 

library(grid)

K3 <- k2 + theme(axis.text.x=element_text(size=14, vjust=0.5, color = "black"), 
                 axis.text.y=element_text(size=14, vjust=0.5, color = "black"),
                 axis.title.x=element_text(size=16, vjust=0.25, face = "bold", color="black"),
                 axis.title.y=element_text(size=16, vjust=1, face = "bold", color="black"),
                 legend.title=element_text(size=16, vjust=1),
                 legend.text=element_text(size=16, vjust=0.5))

k4 <- K3 +  guides(color = guide_legend(override.aes = list(size=7))) 

k5 <- k4 +   theme(legend.key = element_rect(size = 5),
                   legend.key.size = unit(1.5, 'lines')) 
k5


k6 <- k5 + labs(color="   Lime\nTreatment\n")
k6  


# turn off legend for cam/edna ph doer
k7 = k6 + guides(shape=FALSE)
                  

k7

# export 


