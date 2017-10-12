library(dplyr)
df = read.csv("./data/quiz1_c1_w1.csv", header = TRUE, sep = ",")

### Question 1 
q1 = c(mean(df$PASTA), sd(df$PASTA))
q1

### Question 2 
order.scores = order(df$INCOME)
df = df[order.scores,]
df$rank <- rank(df$INCOME)
head(df, n = 1)
tail(df, n = 1)

### Question 3
q3 = df %>% group_by(HHID) %>% summarize(pasta_qty = sum(PASTA) )
max(q3$pasta_qty)

### Question 4 
q4 = subset(df, AREA == 4, select = c(HHID, INCOME))
q41 = distinct(q4, HHID, .keep_all = TRUE)
mean(q41$INCOME)

### Question 5
q5 = df %>% filter(AREA == 2) %>% filter(INCOME > 20000) %>%
      group_by(HHID) %>% 
      summarize(total_qty = sum(PASTA) ) %>% filter(total_qty > 30)
nrow(q5)

### Question 6
cor(df$PASTA, df$EXPOS)

### Question 7
q7 = df %>% group_by(HHID) %>%
      summarize(total = sum(PASTA))
q71 = distinct(q7, HHID, .keep_all = TRUE)
hist(q71$total, ylim = c(0,700), main = "Total purchase of pasta by Household", 
     xlab = "Purchases", ylab = "Frequencies", col = "blue")

### Question 8
q8 = df %>% group_by(TIME) %>% summarize( total_qty = sum(PASTA))
plot(q8$total_qty, main = "Time series of overall total purchase of pasta", col = "blue", xlab = "Time", ylab = "Quantity")

