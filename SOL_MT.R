setwd("C:/Users/xkasp/Desktop/SUS/AAIA")

games <- read.csv("./training_games.csv", sep = ";", header = FALSE,
                  col.names = c('id', 'player1', 'deck1', 'player2', 'deck2', 'won'))

n <- dim(games)[1]
names(games)

games$result <- games$won == "PLAYER_0 WON"

games_won = function(player_name) {
  total1 <- dim(games[games$player1 == player_name,])[1]
  total2 <- dim(games[games$player2 == player_name,])[1]
  won1 <- dim(games[games$player1 == player_name & games$result,])[1]
  won2 <- dim(games[games$player2 == player_name & !games$result,])[1]
  
  return((won1+won2)/(total1+total2))
}

decks_won = function(deck_name) {
  total1 <- dim(games[games$deck1 == deck_name,])[1]
  total2 <- dim(games[games$deck2 == deck_name,])[1]
  won1 <- dim(games[games$deck1 == deck_name & games$result,])[1]
  won2 <- dim(games[games$deck2 == deck_name & !games$result,])[1]
  
  return((won1+won2)/(total1+total2))
}

players = matrix(,4,2)
players[,1] <- as.vector(levels(games$player1))
#cbind(players, names = as.vector(levels(games$player1)))
players[,2] <- unlist(lapply(players[,1], games_won))
rownames(players) <- as.list(players[,1])
colnames(players) <- c('names', 'won_games')

results <- read.csv("./testSubmissionTemplate.csv", sep = ";", header = FALSE,
                    col.names = c('bot', 'deck', 'result'))

#install.packages("rjson")
library(rjson)
#install.packages("statnet.common")
library(statnet.common)

card_params_json <- fromJSON(file = "./cards_collectible.json")
cards_num = length(card_params_json)[1]
cards_params_names <- unlist(lapply(card_params_json, function(row) {return(NVL(row$name, -1))}))
cards_params <- cbind(cards_params_names, unlist(lapply(card_params_json, function(row) {return(as.integer(NVL(row$cost, -1)))})))
cards_params <- cbind(cards_params, unlist(lapply(card_params_json, function(row) {return(as.integer(NVL(row$attack, -1)))})))
cards_params <- cbind(cards_params, unlist(lapply(card_params_json, function(row) {return(as.integer(NVL(row$health, -1)))})))
colnames(cards_params) <- c('name', 'cost', 'attack', 'health')

training_decks_json <- fromJSON(file = "./trainingDecks.json")
trains <- 400
training_deck_names <- lapply(training_decks_json, function(row) {return(row$deckName)})
training_decks <- cbind(unlist(training_deck_names), unlist(lapply(training_decks_json, function(row) {return(row$hero)})))
training_decks <- cbind(training_decks, matrix(0, trains, 4))
colnames(training_decks)[1:6] <- c('deckName', 'hero', 'total_health', 'total_attack', 'total_cost', 'win_ratio')
training_decks <- cbind(training_decks, matrix(0, trains, cards_num))
colnames(training_decks)[7:(cards_num+6)] <- cards_params[,1]

getCardIndex <- function(name) { return(which(cards_params == name))}

for (i in 1:trains) {
  deck_cards <- rownames(as.matrix(training_decks_json[[i]]$cards))
  total_health <- 0
  total_cost <- 0
  total_attack <- 0
  for (j in 1:length(deck_cards)) {
    cards_in_deck <- as.integer(training_decks_json[[i]]$cards[deck_cards[j]])
    training_decks[i,deck_cards[j]] <- cards_in_deck
    total_health <- total_health + (cards_in_deck * as.integer(cards_params[getCardIndex(deck_cards[j]), 'health']))
    total_cost <- total_cost + (cards_in_deck * as.integer(cards_params[getCardIndex(deck_cards[j]), 'cost']))
    total_attack <- total_attack + (cards_in_deck * as.integer(cards_params[getCardIndex(deck_cards[j]), 'attack']))
  }
  training_decks[i, 'total_health'] <- total_health
  training_decks[i, 'total_cost'] <- total_cost
  training_decks[i, 'total_attack'] <- total_attack
  training_decks[i, 'win_ratio'] <- as.numeric(decks_won(training_decks[i, 'deckName']))
}
test_decks_json <- fromJSON(file = "./testDecks.json")
tests <- 200
test_deck_names <- lapply(test_decks_json, function(row) {return(row$deckName)})
test_decks <- cbind(unlist(test_deck_names), unlist(lapply(test_decks_json, function(row) {return(row$hero)})))
test_decks <- cbind(test_decks, matrix(0, tests, 4))
colnames(test_decks)[1:6] <- c('deckName', 'hero', 'total_health', 'total_attack', 'total_cost', 'win_ratio')
test_decks <- cbind(test_decks, matrix(0, tests, cards_num))
colnames(test_decks)[7:(cards_num+6)] <- cards_params[,1]

for (i in 1:tests) {
  deck_cards <- rownames(as.matrix(test_decks_json[[i]]$cards))
  total_health <- 0
  total_cost <- 0
  total_attack <- 0
  for (j in 1:length(deck_cards)) {
    cards_in_deck <- as.integer(test_decks_json[[i]]$cards[deck_cards[j]])
    test_decks[i,deck_cards[j]] <- cards_in_deck
    total_health <- total_health + (cards_in_deck * as.integer(cards_params[getCardIndex(deck_cards[j]), 'health']))
    total_cost <- total_cost + (cards_in_deck * as.integer(cards_params[getCardIndex(deck_cards[j]), 'cost']))
    total_attack <- total_attack + (cards_in_deck * as.integer(cards_params[getCardIndex(deck_cards[j]), 'attack']))
  }
  test_decks[i, 'total_health'] <- total_health
  test_decks[i, 'total_cost'] <- total_cost
  test_decks[i, 'total_attack'] <- total_attack
}
#View(test_decks)
######################################################################################
library(mgcv)
library(ggplot2)
library(dplyr)
library(pROC)
library(visreg)
#View(training_decks_mod)
training_decks_mod<-as.data.frame(training_decks[,-1],num=2:6,stringsAsFactors = FALSE)
test_decks_mod<-as.data.frame(test_decks)
training_decks_mod<-training_decks_mod[,-c(5,6,7)]
for(i in 6:1619)
{
  training_decks_mod[,i]<-as.numeric(training_decks_mod[,i])
  test_decks_mod[,i+1]<-as.numeric(test_decks_mod[,i+1])
}
k<-0
for(i in 6:1619)
{
  if(sum(training_decks_mod[,6+k])==0)
  {
    training_decks_mod<-training_decks_mod[,-(6+k)]
    test_decks_mod<-test_decks_mod[,-(6+k+1)]
  }
 else
   k<-k+1
}
training_decks_mod$total_health<-as.numeric(training_decks_mod$total_health)
training_decks_mod$total_cost<-as.numeric(training_decks_mod$total_cost)
training_decks_mod$total_attack<-as.numeric(training_decks_mod$total_attack)
training_decks_mod$hero<-as.factor(training_decks_mod$hero)
training_decks_mod$win_ratio<-as.numeric(training_decks_mod$win_ratio)
lm <- lm(formula = win_ratio~total_health+total_cost+hero+total_attack, data = training_decks_mod)
summary(lm)
lm1 <- lm(formula = win_ratio~., data = training_decks_mod)
summary(lm1)
test_decks_mod$total_health<-as.numeric(test_decks_mod$total_health)
test_decks_mod$total_cost<-as.numeric(test_decks_mod$total_cost)
test_decks_mod$total_attack<-as.numeric(test_decks_mod$total_attack)
test_decks_mod$hero<-as.factor(test_decks_mod$hero)
test_decks_mod$win_ratio<-as.numeric(test_decks_mod$win_ratio)
test_decks_mod$win_ratio<-predict(lm1,test_decks_mod)
predict(lm,test_decks_mod)
?family
summary(test_decks_mod$win_ratio)
summary(training_decks_mod$win_ratio)
glm <- glm(formula = win_ratio~total_health+total_cost+hero+total_attack,data = training_decks_mod,family=quasibinomial(link = "logit"))
summary(glm)
test_decks_mod$win_ratio<-predict(glm,test_decks_mod)
glm1 <- glm(formula = win_ratio~.,data = training_decks_mod,family=quasibinomial(link = "logit"))
summary(glm1)
test_decks_mod$win_ratio<-predict(glm1,test_decks_mod)
summary(test_decks_mod$win_ratio)
gam <- gam(formula = win_ratio~total_health+total_cost+hero+total_attack, data = training_decks_mod,family=quasibinomial(link = "logit"),maxit=100)
summary(gam)
gam
glm
test_decks_mod$win_ratio<-predict(gam,test_decks_mod)
summary(test_decks_mod$win_ratio)
library(caret)
library(e1071)
require("class")
knn <- train(form=win_ratio~total_health+total_cost+hero+total_attack,method = "knn",preProcess = c("scale"),data = training_decks_mod,trControl = trainControl(method = "cv", number = 10))
knn
?train
summary(knn1)
test_decks_mod$win_ratio1<-predict(knn,test_decks_mod)
summary(test_decks_mod$win_ratio1)
knn1 <- train(form=win_ratio~.,method = "knn",data = training_decks_mod,trControl = trainControl(method = "cv", number = 10))
knn1
test_decks_mod$win_ratio1<-predict(knn1,test_decks_mod)
test_decks_mod$win_ratio1<-predict(knn1,test_decks_mod)
summary(test_decks_mod$win_ratio3)
install.packages("nnet")
library(nnet)
install.packages("neuralnet")
library(neuralnet)
nnet1<-nnet(form=win_ratio~total_health+total_cost+hero+total_attack,data = training_decks_mod,size=2)
test_decks_mod$win_ratio3<-predict(nnet1,test_decks_mod)
nnet<-nnet(form=win_ratio~.,data = training_decks_mod,size=2)
test_decks_mod$win_ratio2<-predict(nnet,test_decks_mod)
summary(test_decks_mod$win_ratio2)
summary(nnet)
#######################################################################################
###   TUTAJ MODYFIKUJE SI? RESULTS
#######################################################################################
View(results)
results<-results[,c(1,2,3)]
View(test_decks_mod)
summary(results$Result)
results$result <- 100 * as.numeric(players[results$bot, 2])
colnames(results)<-c("bot","deckName","Result")
test_decks_mod_res<-test_decks_mod[,c(1,6,337,338)]
resa<-merge(results,test_decks_mod_res,by="deckName")
View(resa)
summary(resa$win_ratio2)
summary(resa$win_ratio1)
summary(resa$win_ratio)
resa$win_ratio1<-as.numeric(resa$win_ratio1)*100
resa$win_ratio2<-as.numeric(resa$win_ratio2)*100
resa$result<-(((as.numeric(resa$Result))^14.8)*((as.numeric(resa$win_ratio2))^2.4)*((as.numeric(resa$win_ratio1))^(2.9))*(as.numeric(resa$win_ratio))^3.9)^(1/24)

resa$result<-(((as.numeric(resa$Result))^14.8)*((as.numeric(resa$win_ratio2))^2.4)*((as.numeric(resa$win_ratio1))^(2.9))*(as.numeric(resa$win_ratio))^4.9)^(1/23)
##wczesniej bylo 5 i 2 czy jakos tak
summary(resa$result)
resa<-resa[,c(2,1,7)]
###################################
###################################

results$result <- format(results$result, digits=1, nsmall=1)
resa$result<-format(resa$result, digits=1, nsmall=1)
write.table(results, file = "./submission.csv", sep = ";", col.names = FALSE, row.names = FALSE, quote = FALSE)
write.table(resa, file = "./submission.csv", sep = ";", col.names = FALSE, row.names = FALSE, quote = FALSE)

