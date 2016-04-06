library(gmodels)
data <- read.csv("FULLbatch_original_mturk.csv", header = TRUE, fill = TRUE, sep = ",", quote = "\"", dec = ".")

subset1 <- subset(data, select = c())
                  
attach(data)
subset1$time <- WorkTimeInSeconds
subset1$clicked <- Answer.clickedLink
subset1$familiarity <- Answer.familiarity
subset1$makesSense <- Answer.makeSense
subset1$cheated <- Answer.cheated
detach(data)

subset1$cheated <- ifelse(subset1$cheated == 1, TRUE, FALSE)
subset1$clicked <- ifelse(subset1$clicked == 1, TRUE, FALSE)


CrossTable(subset1$cheated, subset1$clicked, expected = FALSE, prop.chisq = FALSE, format = "SPS") 
CrossTable(subset1$cheated, subset1$makesSense, expected = FALSE, prop.chisq = FALSE, format = "SPS")
CrossTable(subset1$clicked, subset1$familiarity, expected = FALSE, prop.chisq = FALSE, format = "SPS")

subset1$familiarity <- ifelse(subset1$familiarity == "yes", as.logical(TRUE), as.logical(FALSE))
subset1$makesSense <- ifelse(subset1$makesSense == "yes", as.logical(TRUE), as.logical(FALSE))

#mean time cheated
with(subset1, mean(time[cheated == 1]))

#mean time did not cheat
with(subset1, mean(time[cheated == 0]))


#Mean Time with familiartiy
with(subset1, mean(time[familiarity == 1], na.rm=TRUE))

#Mean Time without familiarity
with(subset1, mean(time[familiarity == 0], na.rm = TRUE)) 


#Length vs Time Answering
subset1$answer <- data$Answer.answer

record_length <- function(x) {
   x <- iconv(x, "latin1", "UTF-8")
  out <- nchar(x["answer"], "chars", "TRUE", "FALSE")
  
}

subset1$answerLength <- apply(subset1, 1, record_length)

attach(subset1)
plot(time, answerLength, main="Answer Length vs. Time", 
     xlab="Time (seconds)", ylab="Answer Length (characters)", pch=".")
abline(lm(answerLength~time), col="red") # regression line (y~x)
detach(subset1)

#####Mean Answer Length - with familiarity
with(subset1, mean(answerLength[familiarity == 1], na.rm = TRUE))

#####Mean Answer Length - without familiarity
with(subset1, mean(answerLength[familiarity == 0], na.rm = TRUE))

#####Mean Answer Length - makes sense
with(subset1, mean(answerLength[makesSense == 1], na.rm = TRUE))

#####Mean Answer Length - does not make sense
with(subset1, mean(answerLength[makesSense == 0], na.rm = TRUE))

subset2 <- subset(data, select = c())

attach(data)
subset2$time <- WorkTimeInSeconds
subset2$question <- Input.question
subset2$answer <- Answer.answer
detach(data)

