library(gmodels)
QA_data2 <- read.csv("QA_gathering_merged_2.csv", header = TRUE, fill = TRUE, sep = ",", quote = "\"", dec = ".")

df <- subset(QA_data2, select = c())

attach(QA_data2)
df$time.full <- WorkTimeInSeconds
df$time.answer <- Answer.answering_time
df$time.read <- Answer.reading_time
df$clicked <- Answer.clickedLink
df$familiarity <- Answer.familiar
df$makesSense <- Answer.makes_sense
df$cheated <- Answer.cheated
df$answer.30 <- Answer.30seconds
df$answer.40 <- Answer.40seconds
df$answer.50 <- Answer.50seconds
df$answer.60 <- Answer.60seconds
detach(QA_data2)

record_length <- function(x, name) {
  x <- iconv(x, "latin1", "UTF-8")
  out <- ifelse(is.na(x[name]), NA, nchar(x[name], type = "chars", allowNA = "FALSE"))
}

df$answerLength.30 <- apply(df, 1, record_length, "answer.30")
df$answerLength.40 <- apply(df, 1, record_length, "answer.40")
df$answerLength.50 <- apply(df, 1, record_length, "answer.50")
df$answerLength.60 <- apply(df, 1, record_length, "answer.60")


###1. Crosstables
CrossTable(df$cheated, df$clicked, expected = FALSE, prop.chisq = FALSE, format = "SPS")
CrossTable(df$cheated, df$makesSense, expected = FALSE, prop.chisq = FALSE, format = "SPS")
CrossTable(df$clicked, df$familiarity, expected = FALSE, prop.chisq = FALSE, format = "SPS")

df$familiarity <- ifelse(df$familiarity == "yes", as.logical(TRUE), as.logical(FALSE))
df$makesSense <- ifelse(df$makesSense == "yes", as.logical(TRUE), as.logical(FALSE))
df$cheated <- ifelse(df$cheated == "TRUE", as.logical(TRUE), as.logical(FALSE))

###2. Mean Times

#####Mean Time - cheated
with(df, mean(time.full[cheated == 1], na.rm=TRUE))

#####Mean Time - did not cheat
with(df, mean(time.full[cheated == 0], na.rm=TRUE))


#####Mean Time - with familiarity
with(df, mean(time.full[familiarity == 1], na.rm=TRUE))


#####Mean Time - without familiarity
with(df, mean(time.full[familiarity == 0], na.rm = TRUE)) 

  
###3. Length Statistics
  
#####Length vs Time Answering
attach(df)
plot(time.read, time.answer, main="Time Spent Answering vs. Time Spent Reading", 
     xlab="Reading Time (seconds)", ylab="Answering Time (seconds)", pch=".")
abline(lm(time.answer~time.read), col="red") # regression line (y~x)
detach(df)

  
###4. Relative Mean Answer Lengths - 30 seconds
  
#####Mean Answer Length - with familiarity
with(df, mean(answerLength.30[familiarity == 1], na.rm = TRUE))

#####Mean Answer Length - without familiarity
with(df, mean(answerLength.30[familiarity == 0], na.rm = TRUE))

#####Mean Answer Length - makes sense
with(df, mean(answerLength.30[makesSense == 1], na.rm = TRUE))

#####Mean Answer Length - does not make sense
with(df, mean(answerLength.30[makesSense == 0], na.rm = TRUE))

  
###5. Means / Standard Deviations - Time
  
#####Mean/Standard Deviation of Time - reading
mean(df$time.read, na.rm = TRUE)
sd(df$time.read, na.rm = TRUE)


#####Mean/Standard Deviation of Time - answering
mean(df$time.answer, na.rm = TRUE)
sd(df$time.answer, na.rm = TRUE)

#####Mean/Standard Deviation of Time - full time
mean(df$time.full, na.rm = TRUE)
sd(df$time.full, na.rm = TRUE)

###6. Means / Standard Deviations - Length

#####Mean/Standard Deviation of Length - 30 seconds
mean(df$answerLength.30, na.rm = TRUE)
sd(df$answerLength.30, na.rm = TRUE)

###7. Histograms Based on Time

hist(df$time.read, 
     main = "Histogram of Reading Time",
     breaks= c(0, seq(5, 300, 5)),
     xaxt = 'n',
     xlab = "Time (seconds)"
)
axis(side=1, at=seq(0,1000, 30), cex.axis=0.7)

hist(df$time.answer, 
     main = "Histogram of Answering Time",
     breaks= c(0, seq(5, 300, 5)),
     xaxt = 'n',
     xlab = "Time (seconds)"
)
axis(side=1, at=seq(0,1000, 30), cex.axis=0.7)

df$time.question = df$time.read + df$time.answer

hist(df$time.question, 
     main = "Histogram of Question Answering Time \n (reading time + answering time)",
     breaks= c(0, seq(5, 300, 5) ), 
     xaxt= 'n',
     xlab = "Time (seconds)"
)
axis(side=1, at=seq(0,1000, 30), cex.axis=0.7)

###8. Central Density Function

readingColor <- rgb(1,0,0)
answeringColor <- rgb(0,1,0)
questionColor <- rgb(0,0,1)

plot(
  ecdf(df$time.question),
  main = "CDF of Question Answering-related Times",
  xlab = "Time (seconds)",
  col = questionColor,
  lwd = 3
)

plot(
  ecdf(df$time.read),
  col = readingColor,
  add = TRUE,
  lwd = 3
)

plot(
  ecdf(df$time.answer),
  col = answeringColor,
  add = TRUE,
  lwd = 3
)

legend('right', c('reading time', 'answering time', 'combined time'), fill=c(readingColor, answeringColor, questionColor), border=NA)


###9. Answer Length at Apecified Time Intervals

boxplot(df$answerLength.30, df$answerLength.40, df$answerLength.50, df$answerLength.60, names = c("30 Seconds", "40 Seconds", "50 Seconds", "60 Seconds"), yaxt="n", ylab = "Characters")
axis(side=2, at=seq(0,2500, 100), cex.axis = .6)