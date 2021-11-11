###Tutorial 8
##11/12/2021
##Alexis Waldschmidt

#set my new working directory
setwd("~/Intro to Biocomputing/Lesson5/Biocomp-Exercise08")

###Question 1
##create a line graph depicting the cumulative score for each team as a function
#   of time in the game. Use the score-by-score information from a game 
#   summarized in "UWvMSU_1-22-13.txt" to generate a graph.

#####Generate a matrix or dataframe with the cumulative score for each team whenever
#   either team scores.

#load in the data
game <- read.table("UWvMSU_1-22-13.txt", header = T, stringsAsFactors = F, sep = "")

##create two vectors with the team's cumulative score and put it in a dataframe 
##do an if else loop for each team
#create empty vectors
UW_totalscore <- 0
UW_scorevtime <- numeric(nrow(game))
MSU_totalscore <- 0
MSU_scorevtime <- numeric(nrow(game))

for(i in 1:nrow(game)){
  if(game$team[i] == "UW"){
    UW_totalscore <- UW_totalscore + game$score[i]
    UW_scorevtime[i] <- UW_totalscore 
    MSU_scorevtime[i] <- MSU_totalscore
  }else{
    MSU_totalscore <- MSU_totalscore + game$score[i]
    MSU_scorevtime[i] <- MSU_totalscore 
    UW_scorevtime[i] <- UW_totalscore
  }
} #end loop

##create a vector that contains the quarter time information
#create an empty vector
#quarter <- numeric(nrow(game))

#make an if then and for loop that contains the quarter time information
#for(i in 1:nrow(game)){
#  if(game$time[i] < 10){
#    quarter[i] <- "1st"
#  }else if(game$time[i] < 20 & game$time[i] > 10){
#    quarter[i] <- "2nd"
#  }else if(game$time[i] < 30 & game$time[i] > 20){
#    quarter[i] <- "3rd"
#  }else{
#    quarter[i] <- "4th"
#  }
#}#end for loop

quarter <- game$time/10

#create a dataframe that contains four columns: one for the quarter, one for the
#   exact time, and two for the cumulative score (one for each team)
game.df <- data.frame(quarter, game$time, UW_scorevtime, MSU_scorevtime)

####Create the plot using the function plot(x, y, type = 'l') where x and y are 
#   vectors and type = 'l' specifies a line graph. You can add a second line to
#   this graph with lines(x, y). Also, use the help file to figure out the other
#   argument to customize the line types if you would like.

#create a plot that is quarter vs score
plot(x = game.df$quarter, y = game.df$UW_scorevtime, type='l', col = "blue", xaxt = "n", xlab = "Quarter", ylab = "Points")
lines(x= game.df$quarter, y= game.df$MSU_scorevtime, type = 'l', col = "black", xlab = "Quarter", ylab = "Points")
axis(1, at = c(1, 2, 3, 4), labels = c("1st", "2nd", "3rd", "4th")) #to specify the x axis so it is in quarters


###Question 2
##Write a game called "guess my number". The computer will generate a random 
#   number between 1 and 100. The user types in a number and the computer replies
#   "lower" if the random number is lower than the guess, "higher" if the number
#   is higher, and "correct!" if the guess is correct. The player can continue 
#   guessing up to 10 times.
#   Hint: sample() is a function that allows for random selection from a vector
#   containing a set of integers

#get the computer to generate a random number between 1 and 100
guess_my_number <- function(x){var <- numeric(length = 10) #we will never actually put data into this function
random_num <- as.numeric(sample(1:100, size = 1))
print("I'm thinking of a number 1-100...")
print("Guess:")

for(i in 1:10){
  var[i] <-  as.integer(readline());
  if(var[i] < random_num){
    print("Higher")
    print("Guess:")
  }else if(var[i] > random_num){
    print("Lower")
    print("Guess:")
  }else{
    print("Correct!")
    break
  }
}
}

#now to play the game, run this:
guess_my_number()
