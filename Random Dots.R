# A second file
# 9.16.2024

# The purpose of this file is to practice with pulling and pushing branches.

dotsPlot <- function(){plot(sample(1:100, 20),
                 sample(1:100, 20),
                 col = viridis::viridis(20),
                 cex = sample(1:20),
                 pch = sample(c(rep(1,10),19), 20, replace = TRUE),
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
  
  # fill circles according to day of the month (a kind of count-down calendar)
  
  max_days <- Sys.Date() |> 
         (\(x) as.Date(cut(x, "month")))() |> 
         (\(x) as.Date(cut(x + 31, "month")) - 1)() |> 
         (\(x) as.numeric(format(x, "%d")))()
  
  current_day <- as.numeric(format(Sys.Date(), "%d"))
  
  shapeFill <- rep(1,max_days)
  shapeFill[1:current_day] <- 19
  
  par(mar = c(0,0,0,0))
  plot(sample(1:100, max_days),
                            sample(1:100, max_days),
                            col = viridis::viridis(max_days),
                            cex = sample(1:max_days),
                            pch = shapeFill,
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

# It would also be nice to turn some of these into solid plots.

# And I'm working on filling up the dots

# Now I've decided to fill up the dots according to a calendar of the days

# I want to put the date across the top
# And I want to make the dots smaller --- they are too big

# I want to experiment with creating and pushing to a branch


# In the branch commit, this is "dotsPlot3"
# I am re-naming it here to try and keep track of my commits
# Because it would seem that I pulled an earlier commit,
# or failed to save this to the main branch

dotsPlot4 <- function(){
  
  incomingPar <- par()
  
  # Exclude read-only parameters
  incomingPar <- incomingPar[!names(incomingPar) %in% c("cin", "cra", "csi", "cxy", "din", "page")]
  
  # fill circles according to day of the month (a kind of count-down calendar)
  
  max_days <- Sys.Date() |> 
    (\(x) as.Date(cut(x, "month")))() |> 
    (\(x) as.Date(cut(x + 31, "month")) - 1)() |> 
    (\(x) as.numeric(format(x, "%d")))()
  
  current_day <- as.numeric(format(Sys.Date(), "%d"))
  
  shapeFill <- rep(1,max_days)
  shapeFill[1:current_day] <- 19
  
  # Adjust size of circles
  shapeSize <- sample(1:10, max_days, replace = TRUE)
  
  # establish the x-y values
  shapeX <- sample(1:100, max_days)
  shapeY <- sample(1:100, max_days)
  
  par(mar = c(0,0,0,0))
  plot(shapeX,
       shapeY,
       col = viridis::viridis(max_days),
       cex = shapeSize,
       pch = shapeFill,
       bty = "n",
       xlab = "",
       ylab = "",
       xaxt = "n",
       yaxt = "n"
  )
  
  # Publish date across the top
  
  text(x=0.5*max(shapeX), y = 0.96*max(shapeY), Sys.Date(), font = 4, col = "gray15", cex = 1.2)
  
  par(incomingPar)
  
  # trouble-shooting text placement
  # The x placement seems to move around
  # return(list(shapeX,shapeY))
  
}