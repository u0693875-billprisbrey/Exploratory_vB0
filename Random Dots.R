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
  
  par(mar = c(0,0,0,0))
  plot(sample(1:100, 20),
                            sample(1:100, 20),
                            col = viridis::viridis(20),
                            cex = sample(1:20),
                            xlab = "",
                            ylab = "",
                            xaxt = "n",
                            yaxt = "n"
)
}

