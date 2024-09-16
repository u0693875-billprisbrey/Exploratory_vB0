# A second file
# 9.16.2024

# The purpose of this file is to practice with pulling and pushing branches.

dotsPlot <- function(){plot(sample(1:100, 20),
                 sample(1:100, 20),
                 col = viridis::viridis(20),
                 cex = sample(1:20),
                 xlab = "",
                 ylab = "",
                 xaxt = "n",
                 yaxt = "n"
                 )
}


dotsPlot2 <- function(){
  
  incomingPar <- par()
  
  # Exclude read-only parameters
  incomingPar <- incomingPar[!names(incomingPar) %in% c("cin", "cra", "csi", "cxy", "din", "page")]
  
  par(mar = c(0,0,0,0))
  plot(sample(1:100, 20),
                            sample(1:100, 20),
                            col = viridis::viridis(20),
                            cex = sample(1:20),
                            bty = "n",
                            xlab = "",
                            ylab = "",
                            xaxt = "n",
                            yaxt = "n"
)
  
  par(incomingPar)
  
}

# next, I want to email this to me.
# Some daily check that everything is working as it should.

