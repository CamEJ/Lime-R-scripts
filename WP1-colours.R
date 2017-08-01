## WP1 cols

# elongation = palegreen1
# heading = springgreen4
# ripening = goldenrod1
# rest of cols just extras

dark_cols <- c("springgreen1", "darkgreen", "goldenrod1", "black", "darkorange3", "blue4", 
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

