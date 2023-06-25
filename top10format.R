setwd("~/Bureau/article stage")

library(ggplot2)
library(plyr)


# Histograme top 10 FORMAT*
tabForm = read.csv("format.csv",header = T)
tabForm <- tabForm[,-1] # On supprime la 1 colone

#hist(tabForm$count)
summary(tabForm)
ggplot(tabForm, aes(x=count)) + geom_histogram( color = "white",fill="steelblue",bins = 60)+ 
  ggtitle("format")+ 
  # geom_vline(xintercept=1.5585, linetype="dashed", color = "red")+
  # geom_vline(xintercept=0.5318 , linetype="dashed", color = "black")+
  xlab("%")
######################### Observation des 10 formats les plus utilisé

newdata <- tabForm[order(-tabForm$count),] # tri en fonction de la colonne mpg
order_10 = newdata[1:10,]

f <- ggplot(order_10, aes(x = reorder(label, -count), y = count))
f + geom_col() + 
  ggtitle("les 10 formats les plus utilisé") +
  xlab("format") + ylab("%")+
  theme(axis.text.x = element_text(angle=45, hjust=1, vjust = 1),
        axis.title = element_blank(),
        plot.margin = margin(t = 20, r = 10, b = 10, l = 10)) +
  geom_col(fill = "#FF7F24")+
  geom_text(aes(label=round(count, digits=1)), vjust=-0.3, size=3.5)
################################################################################
#top10 = read.csv("format_top_10.csv",sep= ";",header = T)
#summary(top10)
#top10$count=top10$count-top10$rang2.
#top10$rang2.=top10$rang2.-top10$only

#summary(top10)
# write.table(top10, "format_top_10_3.csv", row.names=FALSE, sep=";",dec=",", na=" ")
################################################################################
top10 = read.csv("format_top_10_4.csv",sep= ";",header = T)

#top10 <- top10[order(- top10$rang),]
top10 = arrange(top10, desc(top10$rang))

summary(top10)
table(top10$rang)

# Calculer la somme cumulée de len pour chaque format

#library(plyr)
#df_cumsum <- ddply(top10, "label",
#                   transform, label_ypos=cumsum(count))
#head(df_cumsum)
top10$count = (top10$count/nb_outils_format)*100


df_cumsum <- ddply(top10, "label",
                   transform, 
                   label_ypos=cumsum(count) - 0.5*count)

###########
f <- ggplot(top10, aes(x = reorder(label, -count), y = count))
f + geom_col() + 
  ggtitle("les 10 formats les plus utilisé") +
  xlab("format") + ylab("%")+
  theme(axis.text.x = element_text(angle=45, hjust=1, vjust = 1),
        axis.title = element_blank(),
        plot.margin = margin(t = 20, r = 10, b = 10, l = 10)) +
  geom_col(fill = "#FF7F24")+
  geom_text(aes(label=round(count, digits=1)), vjust=-0.3, size=3.5)
###########
ggplot(data=df_cumsum, aes(x=reorder(label, -count), y=count,fill= rang)) +
  geom_bar(stat="identity")+
  xlab("") + ylab("% d'annotation parmis les outils annotés avec FORMAT")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle=45, hjust=1, vjust = 1),#mettre les ecriture des x en diagonales
        plot.margin = margin(t = 20, r = 10, b = 10, l = 10))+
  geom_text(aes(y=label_ypos, label=round(count, digits=1)), vjust=-0.3, size=3.5)# ajoute les chiffre
################################################################################
# rang1 
# Observation des rang 1
tabForm = read.csv("format.csv",header = T)
summary(tabForm)
nb_outils_format = 2588


rang1 = subset(tabForm, tabForm$label == "Binary format"|
                 tabForm$label == "Format (by type of data)"|
                 tabForm$label == "HTML"|
                 tabForm$label == "JSON"|
                 tabForm$label == "RDF format"|
                 tabForm$label == "XML"|
                 tabForm$label == "YAML"|
                 tabForm$label == "Textual format"
)
rang1$count = (rang1$count/nb_outils_format)*100


f <- ggplot(rang1, aes(x = reorder(label, count), y = count))
f + geom_col() + 
  coord_flip()+
  theme_minimal()+
  xlab("") + ylab("% d'annotation parmis les outils annotés avec FORMAT")+
  theme(axis.text.x = element_text(angle=45, hjust=1, vjust = 1),
        
        plot.margin = margin(t = 20, r = 10, b = 10, l = 10)) +
  geom_col(fill = "#FF7F24")+
  geom_text(aes(label=round(count, digits=1)), vjust=-0.3, size=3.5)


###
rang1_S  = read.csv("format_rang1_seul.csv",sep= ",",header = T)
tous = c(1:8)
i=1
for( i in tous){
  tous[i]='seul'
  i= i+1
}
rang1_S=cbind(rang1_S,tous)
###
rang1_Modif = rang1
rang1_Modif$count= rang1$count - rang1_S$count

tous = c(1:8)
i=1
for( i in tous){
  tous[i]='accompagné'
  i= i+1
}
rang1_Modif=cbind(rang1_Modif,tous)


rang1_F = rbind(rang1_S,rang1_Modif)
rang1_F$count = (rang1_F$count/nb_outils_format)*100


#####

df_cumsum <- ddply(rang1_F, "label",
                   transform, label_ypos=cumsum(count))

ggplot(data=df_cumsum, aes(x=reorder(label, count), y=count,fill= tous)) +
  geom_bar(stat="identity")+
  coord_flip()+
  theme_minimal()+
  theme(axis.text.x = element_text(angle=45, hjust=1, vjust = 1),#mettre les ecriture des x en diagonales
        plot.margin = margin(t = 20, r = 10, b = 10, l = 10))+
  geom_text(aes(y=label_ypos, label=round(count, digits=1)), vjust=-0.3, size=3.5)+# ajoute les chiffre
  xlab("") + ylab("% d'annotation parmis les outils annotés avec FORMAT")

########################################################################################################################################################

top10 = read.csv("topic_top10_seul.csv",sep= ",",header = T)

###   %
  nbr_outils_anot_totpic= 27678
  top10$count = (top10$count/nbr_outils_anot_totpic)*100# a modif avec le nbr_outils_anot_totpic
###
  

df_cumsum <- ddply(top10, "label",
                   transform, 
                   label_ypos=cumsum(count) - 0.5*count)


ggplot(data=df_cumsum, aes(x=reorder(label, count), y=count,fill= rang)) +
  geom_bar(stat="identity")+
  coord_flip()+
  ylab("% d'outils bio.tools possédant une annotation TOPIC")+
  xlab("")+
  theme_minimal()+
  #theme(axis.text.x = element_text(angle=90, hjust=1, vjust = 1),#mettre les ecriture des x en diagonales
  #      plot.margin = margin(t = 20, r = 10, b = 10, l = 10))+
  geom_text(aes(y=count+1, label=round(count, digits=1)), vjust=-0.3, size=3.5)# ajoute les chiffre

###################################################################################
# feuille noeud TOPIC
topicFN =read.csv("TOPIC_Feuille_Noeud.csv",sep= ",",header = T)
topicFN <- topicFN[-1,]
# Compute the position of labels
BT = sum(topicFN$bio.tools)

topicFN$bio.tools= (topicFN$bio.tools/sum(topicFN$bio.tools)*100)

label_ypos<- topicFN$bio.tools - 0.5*topicFN$bio.tools

ggplot(topicFN, aes(x="", y=bio.tools, fill=TOPIC)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  theme_void()+
  geom_text(aes(y = label_ypos, label = round(bio.tools, digits=1)), color = "white", size=6)


topicFN$EDAM= (topicFN$EDAM/sum(topicFN$EDAM)*100)

label_ypos<- topicFN$EDAM - 0.5*topicFN$EDAM

ggplot(topicFN, aes(x="", y=EDAM, fill=TOPIC)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  theme_void()+
  geom_text(aes(y = label_ypos, label = round(EDAM, digits=1)), color = "white", size=6)
###################################################################################
# feuille noeud OPERATION
opFN =read.csv("OPERATION_Feuille_Noeud.csv",sep= ",",header = T)
opFN <- opFN[-1,]
# Compute the position of labels
BT = sum(opFN$bio.tools)

opFN$bio.tools= (opFN$bio.tools/sum(opFN$bio.tools)*100)

label_ypos<- opFN$bio.tools - 0.5*opFN$bio.tools

ggplot(opFN, aes(x="", y=bio.tools, fill=operation)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  theme_void()+
  geom_text(aes(y = label_ypos, label = round(bio.tools, digits=1)), color = "white", size=6)

