################################################################################
#                                                                              #
# Purpose:       Bullet Graphs                                                 #
#                                                                              #
# Author:        Mark Kurzeja                                                  #
# Contact:       mtkurzej@umich.edu                                            #
# Client:        Mark Kurzeja                                                  #
#                                                                              #
# Code created:  2018-12-23                                                    #
# Last updated:  2018-12-23                                                    #
#                                                                              #
# Comment:       Stephen Few introduced the concept of Bullet Graphs - simple  #
#                plots that convey information about a measure and relative    #
#                thresholds. This function aims to implement them in ggplot2   #
#                so that one use them for visual analysis                      #
#                                                                              #
################################################################################

rm(list = ls())
library(magrittr)
library(ggplot2)
library(RColorBrewer)

################################################################################
#                                                                              #
#                       Bullet_Chart Function Definition                       #
#                                                                              #
################################################################################

Bullet_Chart <- function(label, 
                         xmin, xmax, 
                         this_measure, 
                         break1, break2, 
                         comp_measure = NA, 
                         bigger_is_better = TRUE, 
                         palette = c("Greys", "Blues", "Greens", "Set1", "Reds", "Purples", "Whites"), 
                         whitePosition = NULL, # Integer for location of White
                         enforceAspectRatio = FALSE,
                         displayAsPercent = FALSE) {
  
  ################# Set up the box parameters ##################
  plotting_dims <- data.frame(
    xmin = xmin, xmax = xmax,
    ymin = 0, ymax = 3,
    this_measure = this_measure,
    med_lower = break1, med_upper = break2,
    measure_lower = 1, measure_upper = 2
  )
  
  # Shift the location of the bad box if bigger is better
  if(bigger_is_better) {
    plotting_dims$bad_lower = xmin
    plotting_dims$bad_upper = break1
  } else {
    plotting_dims$bad_lower = break2
    plotting_dims$bad_upper = xmax
  }
  
  # This is the little bar that acts as a comp measure
  marker = data.frame(x = c(comp_measure, comp_measure),
                      y = c(.6, 2.4))
  
  ############## Determine the Color Palette ###############
  
  palette = match.arg(palette)
  
  if(palette == "Whites") {
    colors = rep("White",3)
  } else {
    colors = brewer.pal(3, palette) 
  }
  
  # If the palette is set 1, then the red should be the first color
  # So that it plots the bad ranges well
  if (palette == "Set1") {
    colors = rev(colors)
  } 
  
  # Include white at the position specified 
  if (!is.null(whitePosition)) {
    # White is selected in the palette - for each position in {1,2,3}
    # make that position in the vector white
    if (1 %in% whitePosition) {
      colors[1] = "white"
    }
    if (2 %in% whitePosition) {
      colors[2] = "white"
    }
    if (3 %in% whitePosition) {
      colors[3] = "white"
    }
  }
  
  ################# Compile the GGPlot2 Object #################
  # Plot the base color (lightest) that contains most of the information
  result <- ggplot() + 
    geom_rect(data = plotting_dims, 
              aes(xmin = xmin, 
                  xmax = xmax, 
                  ymin = ymin, 
                  ymax = ymax), 
              fill = colors[1], 
              alpha = 1)
  
  # Plot the Darker of the qualitive bands
  result <- result +
    geom_rect(data = plotting_dims, 
              aes(xmin = bad_lower, 
                  xmax = bad_upper, 
                  ymin = ymin, 
                  ymax = ymax), 
              fill = colors[3])  
  
  # Plot the middle shade of the qualitive bands
  result <- result + 
    geom_rect(data = plotting_dims, 
              aes(xmin = med_lower, 
                  xmax = med_upper, 
                  ymin = ymin, 
                  ymax = ymax), 
              fill = colors[2])
  
  # Draw the outside border again
  result <- result + 
    geom_rect(data = plotting_dims, 
              aes(xmin = xmin, 
                  xmax = xmax, 
                  ymin = ymin, 
                  ymax = ymax), 
              alpha = 0,
              color = "black")
  
  # Plot the comp measure if it is desired
  if (!is.na(comp_measure)) {
    result <- result + geom_path(data = marker, aes(x = x, y = y), color = "black", size = 2)
  }
  
  # Plot the middle black bar which is responsible for 
  # displaying the main point of the data
  result <- result + 
    geom_rect(data = plotting_dims, 
              aes(xmin = xmin, 
                  xmax = this_measure, 
                  ymin = measure_lower, 
                  ymax = measure_upper), 
              fill = "black",
              color = "black") 
  
  # Label the x-axis
  label_height = -0.75
  if (displayAsPercent) {
    string_breaks <- c(xmin, break1, break2, xmax) %>% {sprintf("%.0f%%", . * 100)}
  } else {
    string_breaks <- c(xmin, break1, break2, xmax) %>% {sprintf("%.0f", .)}
  }
  result <- result + geom_text(aes(x = xmin, y = label_height, label = string_breaks[1], hjust = "left"), color = "grey50")
  result <- result + geom_text(aes(x = break1, y = label_height, label = string_breaks[2]), color = "grey50")
  result <- result + geom_text(aes(x = break2, y = label_height, label = string_breaks[3]), color = "grey50")
  result <- result + geom_text(aes(x = xmax, y = label_height, label = string_breaks[4], hjust = "right"), color = "grey50")
  
  # Add the title 
  result <- result + scale_y_continuous(limits = c(-1,4))
  result <- result + geom_text(aes(x = x, y = y, label = label, fontface = "bold", hjust = "left"), size = 5, data.frame(x = xmin, y = 3.9, label = label))
  
  # Remove all of the thematic elements from the chart that are basically junk
  result <- result + 
    theme_minimal() + # Specify a minimal theme 
    labs(y = NULL, 
         x = NULL) + # Clear out both of the labels
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.y = element_blank(),
      axis.text.x = element_blank(),
      # plot.margin = unit(c(-0.5,-0.5,-0.5,-0.5),"cm"),
      strip.background = element_blank()
    )
  
  # If there is a desire to enforce an aspect ratio, do a 1:10 ratio and plot
  if (enforceAspectRatio) {
    result <- result + theme(aspect.ratio = 1/10)
  }
  result
}

################################################################################
#                                                                              #
#                                   Testing                                    #
#                                                                              #
################################################################################

# Bullet_Chart(label = "Temp", xmin = 0, xmax = 100, this_measure = 5, break1 = 33, break2 = 66,
#              palette = "Set1", bigger_is_better = F)

################################################################################
#                                                                              #
#                                 Grob Testing                                 #
#                                                                              #
################################################################################

dat <- readxl::read_xlsx("./bullet_charts_input.xlsx")
plot_title = "November 2018 Spending Metrics"


rlist <- list()
for (i in seq_len(nrow(dat))) {
  td <- dat[i,]
  mychart <- Bullet_Chart(label = td$label, xmin = td$xmin, xmax = td$xmax, this_measure = td$this_measure,
                          comp_measure = td$comp_measure, bigger_is_better = td$bigger_is_better,
                          break1 = td$break1, break2 = td$break2, palette = td$palette,
                          displayAsPercent = td$displayAsPercent)
  rlist[[i]] <- mychart
}


library(grid)
library(gridExtra)
grid.arrange(grobs = rlist, padding = unit(0.0, "cm"), ncol = 1, top = textGrob(plot_title,gp=gpar(fontsize=20,font=3)))

