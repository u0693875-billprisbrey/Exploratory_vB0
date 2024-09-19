# A second file
# 9.16.2024

# The purpose of this file is to practice with pulling and pushing branches.

# Adding a new push on 9.18.2024 to see if it asks for my credentials again


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

# This makes larger circles less likely
# re=named to dotsPlot7

# re-naming back to dotsPlot # I guess this has to be a commit comment

dotsPlot <- function(){
  
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
  
  # Adjust size of circles, with options for size probability
  
  phi <- 1.618
  
  fibs <- c(1,1,2,3,5,8,13,21,34,55,89,144,233)
  fibProb <- rev(fibs[-length(fibs)]/sum(fibs[-length(fibs)]))
  
  fibs_ratio <- fibs[-1] / fibs[-length(fibs)] # Ratio of consecutive Fibonacci numbers
  fibProb_ratio <- fibs_ratio / sum(fibs_ratio) # Normalize into probabilities
  
  golden_powers <- sapply(0:11, function(x) phi^x)
  goldenProb <- rev(golden_powers / sum(golden_powers))
  
  equalProb <- rep(0.1,12)
  largeProb <- c(rep(0.1,7), seq(from=0.1, to = 0.01, length.out = 5) )
  
  # let's add a random element to the sizing of this
  
  # Generate random value and find interval
  rand_value <- runif(1)
  index <- findInterval(rand_value, vec = seq(from=0, to = 1, length.out = 6))
  
  # Use switch to choose the appropriate function
  shapeSize <- switch(index,
                      sample(1:12, max_days, prob = fibProb_ratio, replace = TRUE),
                      sample(1:12, max_days, prob = equalProb, replace = TRUE),
                      sample(1:12, max_days, prob = goldenProb, replace = TRUE),
                      sample(1:12, max_days, prob = largeProb, replace = TRUE),
                      sample(1:12, max_days, prob = fibProb, replace = TRUE)
  )
  
  
  # shapeSize <- sample(1:12, max_days, prob = fibProb_ratio, replace = TRUE)
  
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
  
  text(x=0.618*max(shapeX), y = 0.96*max(shapeY), Sys.Date(), font = 4, col = "gray15", cex = 1.2)
  
  par(incomingPar)
  
  # trouble-shooting text placement
  # The x placement seems to move around
  # return(list(shapeX,shapeY))
  
}