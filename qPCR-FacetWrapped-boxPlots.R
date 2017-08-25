
d = read.table("qPCR-cpsWithLImetmt.txt", sep = "\t", row.names=1, header=T)


d[["NucType"]] <- setFactorOrder(d[["NucType"]], c("DNA", "cDNA"))

#dm[["Growth"]] <- setFactorOrder(dm[["Growth"]], c("Ripening", "Heading", "Elongation"))


## load ggplot

library(ggplot2)
library(grid)
library(ggthemr)

##### set two colours i liked as palette using ggthemr (and for this one my fill was only going
## to be one of two colours (one for lime  and one for control)

# change colours
dark_cols <- c("gray22", "darkgreen", "goldenrod1", "darkorange3", "blue4", 
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





plot = ggplot(d, aes(x=GrowthStage, y=CopiesPerG, fill=factor(LimeTmt))) + 
  geom_boxplot(colour="black") +
  theme_classic() +
  theme(axis.line.x = element_line(color = "black"), axis.line.y = element_line(color = "black")) 



Cps = plot + facet_wrap(~NucType)

xx <- Cps + 
  theme(axis.title.x=element_blank()) +
  theme(axis.text.x=element_text(size=16, colour="black")) +
  labs(y = "Gene copies per g") +
  theme(axis.text.y=element_text(size=14, angle = 45, colour="black")) +
  theme(axis.title.y=element_text(size=18, colour="black")) +
  #theme(axis.text.y=element_text(size=9)) +
  theme(legend.title = element_text(size=21)) +
  theme(legend.text=element_text(size=16)) +
  labs(fill="  Lime \ntreatment") 
xx
loo <- xx + theme(legend.key.size = unit(2, "cm"),
                  strip.text.x = element_text(size = 18, colour = "black"),# change font of facet label
)
loo



fancy_scientific <- function(l) {
  # turn in to character string in scientific notation
  l <- format(l, scientific = TRUE)
  # quote the part before the exponent to keep all the digits
  l <- gsub("^(.*)e", "'\\1'e", l)
  # turn the 'e+' into plotmath format
  l <- gsub("e", "%*%10^", l)
  # return this as an expression
  parse(text=l)
}




look <- loo + scale_y_continuous(labels=fancy_scientific, 
                                 breaks = scales::pretty_breaks(n = 12))
look

## Links used to make this code:
## for most: