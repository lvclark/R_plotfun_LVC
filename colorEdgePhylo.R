# Lindsay V. Clark, March 29, 2016
# Feel free to use or modify this code under the GNU General Public License v.3.

# Function to assist with plotting a phylogenetic tree such as that produced by the
# R package ape.  This function generates a vector of colors that can be passed
# to the edge.color argument of plot.phylo.  Colors will highlight monophyletic
# groups as specified by the user.  Handy if groups of individuals have been identified
# by some other clustering method (such as Discriminant Analysis of Principal Components),
# or if groups of individuals are known from sampling location etc.

## Example use:
#data(bird.families) # a phylogenetic tree included with ape
#plot(bird.families, type = "unrooted")

#famnames <- bird.families$tip.label
## random set of groups for this example; you should construct this vector differently based
## on what you know about your taxa.  You can have as many groups as you want, not just two.
#mygrp <- c(rep(1,10), rep(2, 10), rep(1, 17), rep(2, 20), rep(1, 20),
#           rep(2, 18), rep(1, 9), rep(2, 33))
#names(mygrp) <- famnames
#mycol <- c("red", "blue") # groups 1 should be red; group 2 blue

#myedgecol <- colorEdgePhylo(bird.families, mygrp, mycol)
#plot(bird.families, type = "unrooted", edge.col = myedgecol)
######

# phy is a "phylo" object produced by ape functions such as nj.
# grp is a named integer vector.  The names should match the tip labels in
#   phy, and the integers indicate to which group the individual belongs.
# col is a character vector indicating colors to output.  The order should correspond
#   to the group numbers in grp.
colorEdgePhylo <- function(phy, grp, col = rainbow(max(grp))){
    if(any(is.na(grp))){
        warning("NA values found in grp.")
        # make NA a new group to color black
        mxgrp <- max(grp, na.rm = TRUE)
        grp[is.na(grp)] <- mxgrp + 1
        col[mxgrp + 1] <- "black"
    }
    # make it throw an error for invalid colors
    blah <- col2rgb(col[unique(grp)])

    # number of edges
    nedge <- dim(phy$edge)[1]
    # vector to contain edge colors
    edgecol <- rep("black", nedge)
    # vector containing tip numbers ordered by grp
    mytips <- match(names(grp), phy$tip.label)
    if(any(is.na(mytips))){
        stop("names(grp) must match phy$tip.label.")
    }

    # assign edge colors, looping through groups
    for(g in unique(grp)){
        # tips that are in this group
        thesetips <- mytips[grp == g]
        myedges <- integer(0)
        leftovers <- integer(0)
        # find edges making monophyletic clades within this group
        while(length(thesetips) > 0){
            theseedges <- (1:nedge)[phy$edge[,2] %in% thesetips]
            newnodes <- phy$edge[theseedges, 1]
            tab <- table(c(leftovers, newnodes))
            thesetips <- as.integer(names(tab))[as.vector(tab==2)]
            leftovers <- as.integer(names(tab))[as.vector(tab==1)]
            myedges <- c(myedges,theseedges)
        }
        edgecol[myedges] <- col[g]
    }

    return(edgecol)
}
