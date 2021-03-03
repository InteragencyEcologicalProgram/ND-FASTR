
PlotNMDS = function(NMDS1, # results of the NMDS you want to plot
                    data, # Environmental data (not species matrix)
                    textp = T, # do you want to plot text for the species centroids?
                    group, # Group you want to put elipses around (must be a factor)
                    group2= NA, # Group you want to vary by color (must be a factor)
                    group3 = NA, #Group you want to vary by point type (must be a factor)
                    taxa = NA) { # Vector of relative percentages of a given taxa from your species matrix, used to scale the points.
  require(vegan)
  #set up colors
  cols = c("red", "blue", "green", "purple", "yellow", 
           "pink1", "brown", "orangered", "black", "cyan")
  #set up shapes
  shapes = c(1,2,3,4,5,6)
  #set up groups
  names(data)[(which(names(data)== group))] = "group"
  names(data)[(which(names(data)== group2))] = "group2"
  names(data)[(which(names(data)== group3))] = "group3"
  #number of groups
  data = droplevels(data)
  m = length(levels(data$group))
  n = length(levels(data$group2))
  o = length(levels(data$group3))
  
  #set parameters so the legend is outside the plot area
  par(xpd=NA,oma=c(3,0,0,0)) 
  
  # plot the sample points
  plot(NMDS1, type="n", shrink = F, cex=1, xlim=c(-2, 2), ylim = c(-2, 2))
  if (is.na(taxa[1])) points(NMDS1, pch=as.numeric(data$group3), cex=1, col=cols[as.numeric(data$group2)])  else {
    # scale the points by relative abundance of a certain taxa
    scalef = max(taxa) - min(taxa)
    points(NMDS1, pch=as.numeric(data$group3), cex=.1 + taxa/mean(taxa), col="blue")
  }
  
  # plot text for species, if desired
  if(textp) text(NMDS1, dis="species", cex=.8) 
  
  # draw a hull around each group
  for (i in 1:m) {
    
    with(data, ordiellipse(NMDS1, kind = "sd", conf = 0.95, groups = group, 
                           show = levels(group)[i], col = "black", lwd=2, lty = 1:length(group)))
  }
  

  legend(par("usr")[1],par("usr")[3], 
         legend = c(levels(data$group3), levels(data$group), levels(data$group2)),
                col = c(rep("black", (m + o)), cols[1:n]),
                lty = c(rep(NA, o), 1:length(levels(data$group)), rep(NA, n)),
                pch = c(shapes[1:o], rep(NA, m), rep(1, n)), lwd=2, bty = "o")
#  legend(par("usr")[1]+1,par("usr")[3], legend = levels(data$group), 
 #        pch = shapes[1:length(levels(data$group))], col = "blue", bty = "n")
  
}



PlotNMDS2 = function(NMDS1, # results of the NMDS you want to plot
                    data, # Environmental data (not species matrix)
                    textp = T, # do you want to plot text for the species centroids?
                    group, # Group you want to put hulls around (must be a factor)
                    group2= NA, # Group you want to vary by color (must be a factor)
                    group3 = NA, #Group you want to vary by point type (must be a factor)
                    taxa = NA) { # Vector of relative percentages of a given taxa from your species matrix, used to scale the points.
  require(vegan)
  #set up colors
  cols = c("red", "blue", "green", "purple", "yellow", 
           "pink1", "brown", "orangered", "black", "cyan")
  #set up shapes
  shapes = c(1,2,3,4,5,6)
  #set up groups
  names(data)[(which(names(data)== group))] = "group"
  names(data)[(which(names(data)== group2))] = "group2"
  names(data)[(which(names(data)== group3))] = "group3"
  #number of groups
  data = droplevels(data)
  m = length(levels(data$group))
  n = length(levels(data$group2))
  o = length(levels(data$group3))
  
  #set parameters so the legend is outside the plot area
  par(xpd=NA,oma=c(3,0,0,0)) 
  
  # plot the sample points
  plot(NMDS1, type="n", shrink = F, cex=1, xlim=c(-2, 2), ylim = c(-2, 2))
  if (is.na(taxa[1])) points(NMDS1, pch=as.numeric(data$group3), cex=1, col=cols[as.numeric(data$group2)])  else {
    # scale the points by relative abundance of a certain taxa
    scalef = max(taxa) - min(taxa)
    points(NMDS1, pch=as.numeric(data$group3), cex=.1 + taxa/mean(taxa), col="blue")
  }
  
  # plot text for species, if desired
  if(textp) text(NMDS1, dis="species", cex=.8) 
  
  # draw a hull around each group
  for (i in 1:m) {
    
    with(data, ordihull(NMDS1, groups = group, 
                           show = levels(group)[i], col = "black", lwd=2, lty = 1:length(group)))
  }
  
  
  legend(par("usr")[1],par("usr")[3], 
         legend = c(levels(data$group3), levels(data$group), levels(data$group2)),
         col = c(rep("black", (m + o)), cols[1:n]),
         lty = c(rep(NA, o), 1:length(levels(data$group)), rep(NA, n)),
         pch = c(shapes[1:o], rep(NA, m), rep(1, n)), lwd=2, bty = "o")
  #  legend(par("usr")[1]+1,par("usr")[3], legend = levels(data$group), 
  #        pch = shapes[1:length(levels(data$group))], col = "blue", bty = "n")
  
}