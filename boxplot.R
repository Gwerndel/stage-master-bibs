setwd("~/Bureau/article stage")

library(ggplot2)

nbr_outils_total= 28208
nbr_outils_anot_totpic= 27678
nbr_outils_anot_op = 26738
nb_outils_format = 2588
outil_data = 3517

OPERATION =read.csv("operation_NV.csv",header = T)
FORMAT = read.csv("format.csv",header = T)
DATA =read.csv("data.csv",header = T)
TOPIC = read.csv("topic.csv",header = T)

OPERATION= (OPERATION$count/nbr_outils_anot_op)*100

tous = c(1:length(OPERATION))
i=1
for( i in tous){
  tous[i]='OPERATION'
  i= i+1
}
OPERATION_S=cbind(OPERATION,tous)

###
FORMAT=(FORMAT$count/nb_outils_format)*100
tous = c(1:length(FORMAT))
i=1
for( i in tous){
  tous[i]='FORMAT'
  i= i+1
}
FORMAT_S=cbind(FORMAT,tous)

DATA=(DATA$count/outil_data)*100
tous = c(1:length(DATA))
i=1
for( i in tous){
  tous[i]='DATA'
  i= i+1
}
DATA_S=cbind(DATA,tous)

TOPIC=(TOPIC$count/nbr_outils_anot_totpic)*100
tous = c(1:length(TOPIC))
i=1
for( i in tous){
  tous[i]='TOPIC'
  i= i+1
}
TOPIC_S=cbind(TOPIC,tous)



table= rbind(TOPIC_S,DATA_S,FORMAT_S,OPERATION_S)
summary(table)
table(table$tous)
table= as.data.frame(table)
table$TOPIC = as.numeric(table$TOPIC)
# Default plot
e <- ggplot(table, aes(x = tous, y =TOPIC ))
e + geom_boxplot()+
  theme_minimal()+
  xlab("") + ylab("% outils parmis ceux annotés par chaque sous-ontologie")
