#80: ---------------------------------------------------------------------------

# Library
library(tidyverse)
theme_set(theme_bw())

dat = read.csv("https://raw.githubusercontent.com/mclu/503-project/master/combined_cleaned.csv")
dat = dat[,-1]

vars = c("retweets", "favorites", "ss_compound", "topic", "delta", "presidency")
new_dat = dat[vars]

set.seed(1)
idx = sample(1:nrow(new_dat), size = floor(nrow(new_dat)*0.8))
train = new_dat[idx,]
train$delta = as.factor(train$delta)
test = new_dat[-idx,]

error_rate = vector()

# logistic regression
mod = glm(delta ~ retweets + favorites + ss + topic + presidency, 
          data = train, family = binomial)
summary(mod)
pred = predict(mod, test[, -5])
predProb = binomial()$linkinv(pred)
pred_log = rep(0, nrow(test))
pred_log[predProb > .5] = 1
table(pred_log, test$delta)
error_rate["logistic"] = sum(pred_log != test$delta) / nrow(test)

# Tree-based method
library(randomForest)
bagging = randomForest(delta ~ favorites + ss_compound + topic + presidency, 
                       data = train, mtry = ncol(train) - 1, 
                importance = TRUE, ntree = 1000)
bagging
bag_pred = predict(bagging, newdata = test)
table(bag_pred, test$delta)
error_rate['bagging'] = mean(bag_pred != test$delta)
varImpPlot(bagging)

rf = randomForest(delta ~ favorites + retweets + ss_compound + topic + presidency, 
                  data = train, mtry = floor(sqrt(5)), 
             importance = TRUE, ntree = 1000, cp = 0.004)
pred = predict(rf, newdata = test)
varImpPlot(rf)
-error_rate["RF"] = mean(pred != test$delta)

library(gbm)
ada = gbm(delta ~ retweets + ss + topic + presidency,
          data = train, distribution = "adaboost", n.trees = 5000,
          interaction.depth = 3)
pred = predict(ada, newdata = test, n.trees = 5000, type = "response")
ada_pred = ifelse(pred>0.5,1,0)
table(ada_pred,test$delta)
error_rate['adaboost'] = mean(ada_pred!=test$delta)

# SVM
library(e1071)
tune.out = tune(svm, delta ~ favorites + ss+ topic + presidency, data=train, 
                ranges=list(cost=c(0.1,1,10), kernel=c("linear", "radial")))
summary(tune.out) # linear kernel, cost = 0.1
test_pred = predict(tune.out$best.model, newdata = test)
error_rate['SVM'] = sum((test_pred != test$delta)) / dim(test)[1]

# NN
library(keras)
library(tensorflow)
# Build MLP (Multi Layer Perception)
model_fashion <- keras_model_sequential()
model_fashion %>%
  layer_flatten(input_shape = c(28, 28)) %>%
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dense(units = 10, activation = 'softmax')

# Compile the model
model_fashion %>% compile(
  optimizer = 'adam', 
  loss = 'sparse_categorical_crossentropy',
  metrics = c('accuracy')
)

# Train the model
train_labels = as.matrix(train_labels)
test_labels = as.matrix(test_labels)
MLP.history = model_fashion %>% 
  fit(train[,-5], as.matrix(train[,5]), epochs = 10, validation_split = 0.2, batch_size = 32)
plot(MLP.history) + theme_minimal()

# Evaluate accuracy
score <- model_fashion %>% evaluate(test_images, test_labels)
cat('Test loss:', score$loss, "\n")
cat('Test accuracy:', score$acc, "\n")

# Try using neg/pos/neu: ----
setwd("/Users/Amy/Downloads")
dat1 = read.csv("twitter_data_cleaned.csv")
dat1 = transform(dat1, ss = max.col(dat1[8:10]))
dat_combine = inner_join(dat1, dat[,c(1,13,14)])

vars = c("retweets", "favorites", "ss", "topic", "delta", "presidency")
new_dat = dat_combine[vars]

# Plot for words: ----
library(tidytext)
setwd("/Users/Amy/Desktop/Stat503/group_proj")
tweets = read.csv("./data/trumptweets_original.csv")
col = c("content", "date", "retweets", "favorites")
tweets = tweets[col]
tweets$day = as.Date(tweets$date, format = "%Y-%m-%d")
new_id = data.frame(day = unique(tweets$day), new_id = 1:length(unique(tweets$day)))
tweets = inner_join(new_id, tweets, by = "day")


tweets_words %>%
  count(word, sort = TRUE) %>%
  head(25) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_bar(stat = "identity") + guides(fill = FALSE) +
  ylab("Occurrences") + coord_flip()


tweets_words %>%
  select(new_id, word) %>%
  left_join(dat[, c(1, 13)], by = "new_id") %>%
  drop_na() %>%
  count(delta, word, sort = TRUE) %>%
  head(25) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n, fill = as.factor(delta))) +
  geom_bar(stat = "identity") + labs(fill = "S&P Indicator") +
  ylab("Occurrences") + coord_flip()


# Plot for delta: ----
theme_set(theme_bw())
dat %>% select(retweets, favorites, ss_compound, delta) %>%
  pivot_longer(cols = 1:3) %>%
  ggplot(aes(x = as.factor(delta), y = value)) +
  geom_boxplot() + xlab("S&P Indicator") +
  facet_wrap(name~., scales = "free_y")

# Plot for ss_compound: ----
g1 = ggplot(dat, aes(x = topic, y = ss_compound)) + 
  geom_boxplot()

g2 = ggplot(dat, aes(x = presidency, y = ss_compound)) + 
  geom_boxplot()

library(gridExtra)
grid.arrange(grobs = list(g1, g2), widths = c(2, 1))
  
# Pairs plot: ----
vars = c("retweets", "favorites", "ss_compound", "topic", "presidency")
pairs(dat[, vars], col = c("green", "red")[dat$delta+1],
      pch = c(1, 2)[dat$delta+1])
par(xpd = NA) # specifies where in the plotting device an object can actually be plotted. 
legend(.4, .55, as.vector(unique(dat$delta)),
       col = c("red", "green"), pch=1:2, cex = .5)


  
