## Helper functions for Rainfall CS project

# 1. multiplot function from http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_%28ggplot2%29/
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# 2. read in ozdata and stations Rdata
oz.map <- read.csv("ozdata.csv")
load("stations.Rdata")

# 3. Some old code
# # load the different tables into a data frame
# df_main <- sqlQuery(db, "select * from main_table",as.is = c(1,2,6,7),
#                     stringsAsFactors=F)
# df_regr_results <- sqlQuery(db, "select * from regr_results")
# df_regr_stats <- sqlQuery(db, "select * from regr_stats")

# here code that runs when app is launched
# This is some old code just to get the BOM stations into a Rdata file
# stations <- read.fwf("20140617_AllBOMstations.txt", 
#                      widths=c(7,6,43,7,7,9,10,16,4,10,9,7),
#                     skip = 3,
#               na.strings = c("..",".....","null"))
# colnames(stations) <-c("Site","Dist","Site_name","Start","End","Lat",
#                        "Lon","Source","STA","Height_m","Bar_ht","WMO")
# save(stations, file="Stations.Rdata")
# this could be moved to a helper script

