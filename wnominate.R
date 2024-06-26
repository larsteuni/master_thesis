library(data.table)
library(wnominate)
library(dplyr)
library(tidyr)

data <- fread('C:/Eindproject_master/df_member_votes_v2.tsv')

View(data)

"
reshaped_data <- data %>%
  select(bioname, party, bill_id, cast_code)

pivot_data <- reshaped_data %>% 
  pivot_wider(names_from = bill_id, values_from = cast_code, values_fill = 0) %>%
  arrange(bioname)

matrix_data <- as.matrix(pivot_data)
"

topics <- split(data, data$topic_number)

results <- list()

for (number in names(topics)) {
  topic <- topics[[number]]
  
  reshaped_data <- topic %>%
    select(icpsr, party, bill_id, cast_code)
  
  reshaped_data$icpsr <- as.character(reshaped_data$icpsr)
  
  pivot_data <- reshaped_data %>% 
    pivot_wider(names_from = bill_id, values_from = cast_code, values_fill = 0)
  
  matrix_data <- as.matrix(pivot_data)

  
  ids <- matrix_data[,1]
  legData <- matrix(matrix_data[,2],length(matrix_data[,2]),1) 
  colnames(legData) <- "party" 
  matrix_data <- matrix_data[,-c(1,2)]
  
  rc <- rollcall(matrix_data, yea=c(1,2,3), nay=c(4,5,6), missing=c(7,8,9),
                 notInLegis=0, legis.names = ids, legis.data=legData) 
  
  result <- wnominate(rc, polarity=c('14454', '14679'))
  
  results[[paste0("result_", number)]] <- result
}

number <- topics[[1]]
number - 1

View(results)

View(results)
summary(result)
summary(result_1)

leg <- result$legislators
df_leg <- data.frame(leg)
df_leg <- cbind(df_leg, names)
View(df_leg)

summary(result)
plot(result)

plot.coords(result,cutline=14)
