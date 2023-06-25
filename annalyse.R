setwd("~/Bureau/article stage")

library(ggplot2)
library(dplyr)
library(plyr)

nbr_outils_total= 28208
nbr_outils_anot_totpic= 27678
nbr_outils_anot_op = 26738
nb_outils_format = 2588
outil_data = 3517
label= c("TOPIC","OPERATION","FORMAT","DATA")
count= c(nbr_outils_anot_totpic,nbr_outils_anot_op,nb_outils_format,outil_data)
table=cbind(label,count)
table= as.data.frame(table)
table$count= as.numeric(table$count)
table$count = (table$count/nbr_outils_total )*100


f <- ggplot(table, aes(x = reorder(label, -count), y = count))
f + geom_col() + 
  xlab("")+
  ylab("% d'outil annoté")+
  geom_col(fill = "#FF7F24")+
  geom_text(aes(label=round(count, digits=1)), vjust=-0.3, size=3.5)
################################################################################################################

table =read.csv("operation_NV.csv",header = T)
table$count = (table$count/nbr_outils_anot_op )*100
summary(table)

########################
# Observation des rang 1
rang1 = subset(table, label == "Alignment"|
                 label == "Analysis"| 
                 label == "Annotation"|
                 label == "Calculation"|
                 label == "Classification"|
                 label == "Clustering"|
                 label == "Comparison"|
                 label == "Conversion"|
                 label == "Correlation"|
                 label == "Data handling"|
                 label == "Design"|
                 label == "Generation"|
                 label == "Indexing"|
                 label == "Mapping"|
                 label == "Modelling and simulation"|
                 label == "Optimisation and refinement"|
                 label == "Prediction and recognition"|
                 label == "Quantification"|
                 label == "Service management"|
                 label == "Validation"|
                 label == "Visualisation"
                 )

#hist(rang1$V2)





f <- ggplot(rang1, aes(x = reorder(label, -count), y = count))
f + geom_col() + 
  theme_minimal()+
  xlab("") + ylab("%% outils parmis ceux annotés avec OPERATION")+
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust = 1),
       # axis.title = element_blank(),
        plot.margin = margin(t = 20, r = 10, b = 10, l = 10)) +
  geom_col(fill = "#FF7F24")+
  geom_text(aes(label=round(count, digits=1)), vjust=-0.3, size=3.5)
########################
# observation des 10 operation les plus observée

newdata <- table[order(-table$count),] # tri en fonction de la colonne mpg
order_10 = newdata[1:10,]


f <- ggplot(order_10, aes(x = reorder(label, -count), y = count))
f + geom_col() + 
  theme_minimal()+
  xlab("") + ylab("%% outils parmis ceux annotés avec OPERATION")+
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust = 1),
       # axis.title = element_blank(),
        plot.margin = margin(t = 20, r = 10, b = 10, l = 10)) +
  geom_col(fill = "#E69F00")+
  geom_text(aes(label=round(count, digits=1)), vjust=-0.3, size=3.5)
#######
# top 10 SEUL
table =read.csv("operation_NV.csv",header = T)
newdata <- table[order(-table$count),] # tri en fonction de la colonne mpg
order_10 = newdata[1:10,]
summary(order_10)
order_10= order_10[,-c(1,2)]
#order_10 = rename(order_10,c("label" = "label", "accompagné"= "count"))

table2 =read.csv("Operation_top10_NON_SEUL.csv",header = T)
table2$count =nbr_outils_anot_op-table2$count
#table2 = rename(table2,c("label" = "label", "seul"= "count"))


tous = c(1:length(order_10))
i=1
for( i in tous){
  tous[i]='Accompagné'
  i= i+1
}
order_10= cbind(order_10,tous)

tous = c(1:length(table2))
i=1
for( i in tous){
  tous[i]='seul'
  i= i+1
}
table2= cbind(table2,tous)

top10 = rbind(table2,order_10)

#top10 = merge(order_10,table2, by = "label")
#top10_F =top10
#top10_F$accompagné=top10$accompagné-top10$seul
top10$count = (top10$count/nbr_outils_anot_op )*100

df_cumsum <- ddply(top10, "label",
                   transform, label_ypos=cumsum(count))

ggplot(data=df_cumsum, aes(x=reorder(label, -count), y=count,fill= tous)) +
  geom_bar(stat="identity")+
 # coord_flip()+
  theme_minimal()+
  theme(axis.text.x = element_text(angle=45, hjust=1, vjust = 1),#mettre les ecriture des x en diagonales
        plot.margin = margin(t = 20, r = 10, b = 10, l = 10))+
  geom_text(aes(y=label_ypos, label=round(count, digits=1)), vjust=-0.3, size=3.5)+# ajoute les chiffre
  xlab("") + ylab("% d'annotation parmis les outils annotés avec FORMAT")

########################
# observation de toutes les operation

ggplot(table, aes(x=count)) + geom_histogram( fill="steelblue",bins = 60)+ 
  ggtitle("operation")+ 
  geom_vline(xintercept=mean(table$count), linetype="dashed", color = "red")+
  geom_vline(xintercept=median(table$count) , linetype="dashed", color = "black")+
  xlab("%")

#################################################################################################################

# observation de tous les topics
topic = read.csv("topic.csv",header = T)
summary(topic)
topic$count = (topic$count/nbr_outils_anot_totpic)*100# a modif avec le nbr_outils_anot_totpic
########################
# Observation des rang 1
rang1 = subset(topic, label == "Biology"|
                 label == "Biomedical science"|
                 label == "Chemistry"|
                 label == "Computational biology"|
                 label == "Computer science"|
                 label == "Experimental design and studies"|
                 label == "Informatics"|
                 label == "Laboratory techniques"|
                 label == "Literature and language"|
                 label == "Mathematics"|
                 label == "Medicine"|
                 label == "Omics"|
                 label == "Physics"
               
)


rang1 <- rang1[order(-rang1$count),]


f <- ggplot(rang1, aes(x = reorder(label, -count), y = count))
f + geom_col() + 
  ggtitle("pourcentage d'outils utilisant un topic de rang 1") +
 
  theme(axis.text.x = element_text(angle=45, hjust=1, vjust = 1),
        axis.title = element_blank(),
        plot.margin = margin(t = 20, r = 10, b = 10, l = 10)) +
  geom_col(fill = "#FF7F24")+
  geom_text(aes(label=round(count, digits=1)), vjust=-0.3, size=3.5)
########################
# Observation des 10 topics les plus utilisé

newdata <- topic[order(-topic$count),] # tri en fonction de la colonne 
order_10 = newdata[1:10,]

f <- ggplot(order_10, aes(x = reorder(label, -count), y = count))
f + geom_col() + 
  ggtitle("les 10 topics les plus utilisé") +
  xlab("opération") + ylab("%")+
  theme(axis.text.x = element_text(angle=90, vjust = 1,hjust = 1),
        axis.title = element_blank(),
        plot.margin = margin(t = 20, r = 10, b = 10, l = 10)) +
  geom_col(fill = "#FF7F24")+
  geom_text(aes(label=round(count, digits=1)), vjust=-0.3, size=3.5)

######################
# Observation de tout les topic


hist(topic$count)
summary(topic)
ggplot(topic, aes(x=count)) + geom_histogram( color = "white",fill="steelblue",bins = 60)+ 
  ggtitle("Topic")+ 
  geom_vline(xintercept=mean(topic$count), linetype="dashed", color = "red")+
  geom_vline(xintercept=median(topic$count) , linetype="dashed", color = "black")+
  xlab("%")
####################################################################################################################
#FORMAT

tabForm = read.csv("format.csv",header = T)
summary(tabForm)
tabForm$count = (tabForm$count/nb_outils_format)*100# % sur juste les outiles possédant des annotation de format

##########################
#tous
summary(tabForm)
ggplot(tabForm, aes(x=count)) + geom_histogram( color = "white",fill="steelblue",bins = 60)+ 
  ggtitle("format")+ 
  geom_vline(xintercept=mean(tabForm$count), linetype="dashed", color = "red")+
  geom_vline(xintercept=median(tabForm$count) , linetype="dashed", color = "black")+
  xlab("%")
########################
# Observation des 10 formats les plus utilisé

newdata <- tabForm[order(-tabForm$count),] # tri en fonction de la colonne mpg
order_10 = newdata[1:10,]
order_10$count = (order_10$count/nb_outils_format)*100

f <- ggplot(order_10, aes(x = reorder(label, -count), y = count))
f + geom_col() + 
  theme_minimal()+
  xlab("") + ylab("% outils parmis ceux annotés avec FORMAT")+
  theme(axis.text.x = element_text(angle=45, hjust=1, vjust = 1),
        plot.margin = margin(t = 20, r = 10, b = 10, l = 10)) +
  geom_col(fill = "#FF7F24")+
  geom_text(aes(label=round(count, digits=1)), vjust=-0.3, size=3.5)

##write.table(order_10, "format_top_10.csv", row.names=FALSE, sep=";",dec=",", na=" ")
########################
# Observation des rang 1

rang1 = subset(tabForm, tabForm$label == "Binary format"|
                 tabForm$label == "Format (by type of data)"|
                 tabForm$label == "HTML"|
                 tabForm$label == "JSON"|
                 tabForm$label == "RDF format"|
                 tabForm$label == "XML"|
                 tabForm$label == "YAML"|
                 tabForm$label == "Textual format"
)
#write.table(rang1, "format_rang_1.csv", row.names=FALSE, sep=";",dec=",", na=" ")

f <- ggplot(rang1, aes(x = reorder(label, -count), y = count))
f + geom_col() + 
  ggtitle("pourcentage d'outils utilisant un topic de rang 1") +
  xlab("opération") + ylab("%")+
  theme(axis.text.x = element_text(angle=45, hjust=1, vjust = 1),
        axis.title = element_blank(),
        plot.margin = margin(t = 20, r = 10, b = 10, l = 10)) +
  geom_col(fill = "#FF7F24")+
  geom_text(aes(label=round(count, digits=1)), vjust=-0.3, size=3.5)
################################################################################################################
# DATA
outil_data = 3517
table =read.csv("data.csv",header = T)
summary(table)
table$count = (table$count/outil_data)*100
#########################
#tous
ggplot(table, aes(x=count)) + geom_histogram( color = "white",fill="steelblue",bins = 60)+ 
  ggtitle("Data")+ 
  # geom_vline(xintercept=mean(table$count), linetype="dashed", color = "red")+
  # geom_vline(xintercept=median(table$count) , linetype="dashed", color = "black")+
  xlab("count")
########################
# Observation des 10 DATA les plus utilisé

newdata <- table[order(-table$count),] # tri en fonction de la colonne mpg
order_10 = newdata[1:10,]

f <- ggplot(order_10, aes(x = reorder(label, -count), y = count))
f + geom_col() + 
 # ggtitle("les 10 data les plus utilisé") +
  theme_minimal()+
  xlab("") + ylab("% outils parmis ceux annotés avec DATA")+
  theme(axis.text.x = element_text(angle=45, hjust=1, vjust = 1),
        #axis.title = element_blank(),
        plot.margin = margin(t = 20, r = 10, b = 10, l = 10)) +
  geom_col(fill = "#FF7F24")+
  geom_text(aes(label=round(count, digits=1)), vjust=-0.3, size=3.5)
########################
# Observation des rang 1

rang1 = subset(table, label == "Alignment"|
                 label == "Biodiversity data"|
                 label == "Codon usage data"|
                 label == "Data index"|
                 label == "Data reference"|
                 label == "Database search results"|
                 label == "Ecological data"|
                 label == "Evidence"|
                 label == "Experimental measurement"|
                 label == "Expression data"|
                 label == "Hierarchy"|
                 label == "Identifier"|
                 label == "Image"|
                 label == "Keyword"|
                 label == "Map"|
                 label == "Map data"|
                 label == "Mathematical model"|
                 label == "Matrix"|
                 label == "Molecular property"|
                 label == "Molecular simulation data"|
                 label == "Ontology data"|
                 label == "Over-representation data"|
                 label == "Pathway or network"|
                 label == "Phylogenetic data"|
                 label == "Plot"|
                 label == "Query script"|
                 label == "Reaction data"|
                 label == "Regular expression"|
                 label == "Report"|
                 label == "Score"|
                 label == "Sequence"|
                 label == "Sequence attribute"|
                 label == "Sequence coordinates"|
                 label == "Sequence features"|
                 label == "Sequence features metadata"|
                 label == "Sequence set"|
                 label == "Sequence signature data"|
                 label == "Sequence variations"|
                 label == "Simulation"|
                 label == "Spectrum"|
                 label == "Structural profile"|
                 label == "Structure"|
                 label == "Taxonomy"|
                 label == "Text data"|
                 label == "Training material"
)
f <- ggplot(rang1, aes(x = reorder(label, count), y = count))
f + geom_col() + 
  #ggtitle("Nombre d'outils utilisant une data de rang 1") +
  theme_minimal()+
  coord_flip()+
  xlab("") + ylab("% outils parmis ceux annotés avec DATA")+
  theme(axis.text.x = element_text(angle=45, hjust=1, vjust = 1),
        #axis.title = element_blank(),
        plot.margin = margin(t = 20, r = 10, b = 10, l = 10)) +
  geom_col(fill = "#FF7F24")+
  geom_text(aes(label=round(count, digits=1)), vjust=-0.3, size=3.5)

#################################################################################
# DATA input
outil_data_input = 3405
table =read.csv("DATA_input.csv",header = T)
summary(table)
table$count = (table$count/outil_data_input)*100

#########################
newdata <- table[order(-table$count),] # tri en fonction de la colonne mpg
order_10 = newdata[1:10,]

f <- ggplot(order_10, aes(x = reorder(label, -count), y = count))
f + geom_col() + 
  # ggtitle("les 10 data les plus utilisé") +
  theme_minimal()+
  xlab("") + ylab("% outils parmis ceux annotés avec DATA")+
  theme(axis.text.x = element_text(angle=45, hjust=1, vjust = 1),
        #axis.title = element_blank(),
        plot.margin = margin(t = 20, r = 10, b = 10, l = 10)) +
  geom_col(fill = "#FF7F24")+
  geom_text(aes(label=round(count, digits=1)), vjust=-0.3, size=3.5)
########################
# Observation des rang 1

rang1 = subset(table, label == "Alignment"|
                 label == "Biodiversity data"|
                 label == "Codon usage data"|
                 label == "Data index"|
                 label == "Data reference"|
                 label == "Database search results"|
                 label == "Ecological data"|
                 label == "Evidence"|
                 label == "Experimental measurement"|
                 label == "Expression data"|
                 label == "Hierarchy"|
                 label == "Identifier"|
                 label == "Image"|
                 label == "Keyword"|
                 label == "Map"|
                 label == "Map data"|
                 label == "Mathematical model"|
                 label == "Matrix"|
                 label == "Molecular property"|
                 label == "Molecular simulation data"|
                 label == "Ontology data"|
                 label == "Over-representation data"|
                 label == "Pathway or network"|
                 label == "Phylogenetic data"|
                 label == "Plot"|
                 label == "Query script"|
                 label == "Reaction data"|
                 label == "Regular expression"|
                 label == "Report"|
                 label == "Score"|
                 label == "Sequence"|
                 label == "Sequence attribute"|
                 label == "Sequence coordinates"|
                 label == "Sequence features"|
                 label == "Sequence features metadata"|
                 label == "Sequence set"|
                 label == "Sequence signature data"|
                 label == "Sequence variations"|
                 label == "Simulation"|
                 label == "Spectrum"|
                 label == "Structural profile"|
                 label == "Structure"|
                 label == "Taxonomy"|
                 label == "Text data"|
                 label == "Training material"
)
f <- ggplot(rang1, aes(x = reorder(label, count), y = count))
f + geom_col() + 
  #ggtitle("Nombre d'outils utilisant une data de rang 1") +
  theme_minimal()+
  coord_flip()+
  xlab("") + ylab("% outils parmis ceux annotés avec DATA")+
  theme(axis.text.x = element_text(angle=45, hjust=1, vjust = 1),
        #axis.title = element_blank(),
        plot.margin = margin(t = 20, r = 10, b = 10, l = 10)) +
  geom_col(fill = "#FF7F24")+
  geom_text(aes(label=round(count, digits=1)), vjust=-0.3, size=3.5)
#################################################################################
# DATA output
outil_data_output = 2805
table =read.csv("DATA_output.csv",header = T)
summary(table)
table$count = (table$count/outil_data_output)*100

#########################
newdata <- table[order(-table$count),] # tri en fonction de la colonne mpg
order_10 = newdata[1:10,]

f <- ggplot(order_10, aes(x = reorder(label, -count), y = count))
f + geom_col() + 
  # ggtitle("les 10 data les plus utilisé") +
  theme_minimal()+
  xlab("") + ylab("% outils parmis ceux annotés avec DATA")+
  theme(axis.text.x = element_text(angle=45, hjust=1, vjust = 1),
        #axis.title = element_blank(),
        plot.margin = margin(t = 20, r = 10, b = 10, l = 10)) +
  geom_col(fill = "#FF7F24")+
  geom_text(aes(label=round(count, digits=1)), vjust=-0.3, size=3.5)
########################
# Observation des rang 1

rang1 = subset(table, label == "Alignment"|
                 label == "Biodiversity data"|
                 label == "Codon usage data"|
                 label == "Data index"|
                 label == "Data reference"|
                 label == "Database search results"|
                 label == "Ecological data"|
                 label == "Evidence"|
                 label == "Experimental measurement"|
                 label == "Expression data"|
                 label == "Hierarchy"|
                 label == "Identifier"|
                 label == "Image"|
                 label == "Keyword"|
                 label == "Map"|
                 label == "Map data"|
                 label == "Mathematical model"|
                 label == "Matrix"|
                 label == "Molecular property"|
                 label == "Molecular simulation data"|
                 label == "Ontology data"|
                 label == "Over-representation data"|
                 label == "Pathway or network"|
                 label == "Phylogenetic data"|
                 label == "Plot"|
                 label == "Query script"|
                 label == "Reaction data"|
                 label == "Regular expression"|
                 label == "Report"|
                 label == "Score"|
                 label == "Sequence"|
                 label == "Sequence attribute"|
                 label == "Sequence coordinates"|
                 label == "Sequence features"|
                 label == "Sequence features metadata"|
                 label == "Sequence set"|
                 label == "Sequence signature data"|
                 label == "Sequence variations"|
                 label == "Simulation"|
                 label == "Spectrum"|
                 label == "Structural profile"|
                 label == "Structure"|
                 label == "Taxonomy"|
                 label == "Text data"|
                 label == "Training material"
)
f <- ggplot(rang1, aes(x = reorder(label, count), y = count))
f + geom_col() + 
  #ggtitle("Nombre d'outils utilisant une data de rang 1") +
  theme_minimal()+
  coord_flip()+
  xlab("") + ylab("% outils parmis ceux annotés avec DATA")+
  theme(axis.text.x = element_text(angle=45, hjust=1, vjust = 1),
        #axis.title = element_blank(),
        plot.margin = margin(t = 20, r = 10, b = 10, l = 10)) +
  geom_col(fill = "#FF7F24")+
  geom_text(aes(label=round(count, digits=1)), vjust=-0.3, size=3.5)
################################################################################
# DATA = "data"
table =read.csv("DATA_data_2.csv",header = T)
outil_data = 3517
table$count=(table$count/outil_data)*100

ggplot(data=table , aes(x=format, y=count, fill=etat)) +
  geom_bar(stat="identity", position=position_dodge())+
  xlab("annotation 'data'") + ylab("% outils parmis ceux annotés avec DATA")+
 # geom_text(aes(label=, vjust=-0.3, size=3.5)
  geom_text(aes(label=round(count, digits=1)), vjust=1.6, color="black",
          position = position_dodge(0.9), size=3.5)+
  theme_minimal()

################################################################################################################
# FORMAT

outil_format = 2588

table =read.csv("format.csv",header = T)
summary(table)
table$count = (table$count/outil_format)*100
###########

ggplot(table, aes(x=count)) + geom_histogram( color = "white",fill="steelblue",bins = 60)+ 
  ggtitle("Format")+ 
  geom_vline(xintercept=mean(table$count), linetype="dashed", color = "red")+
  geom_vline(xintercept=median(table$count) , linetype="dashed", color = "black")+
  xlab("%")
################################################################################################################
# FORMAT input

input = 2323

table =read.csv("format_input.csv",header = T)
summary(table)

table$count = (table$count/input)*100

ggplot(table, aes(x=count)) + geom_histogram( color = "white",fill="steelblue",bins = 60)+ 
  ggtitle("Format")+ 
  geom_vline(xintercept=mean(table$count), linetype="dashed", color = "red")+
  geom_vline(xintercept=median(table$count) , linetype="dashed", color = "black")+
  xlab("%")
################################################################################################################
# FORMAT output
output = 2003

table =read.csv("format_output.csv",header = T)
summary(table)

table$count = (table$count/input)*100

ggplot(table, aes(x=count)) + geom_histogram( color = "white",fill="steelblue",bins = 60)+ 
  ggtitle("Format")+ 
  geom_vline(xintercept=mean(table$count), linetype="dashed", color = "red")+
  geom_vline(xintercept=median(table$count) , linetype="dashed", color = "black")+
  xlab("%")
########################
# Observation des 10 Format les plus utilisé

newdata <- table[order(-table$count),] # tri en fonction de la colonne mpg
order_10 = newdata[1:10,]

f <- ggplot(order_10, aes(x = reorder(label, -count), y = count))
f + geom_col() + 
  ggtitle("les 10 format output les plus utilisé") +
  xlab("dat") + ylab("count")+
  theme(axis.text.x = element_text(angle=45, hjust=1, vjust = 1),
        axis.title = element_blank(),
        plot.margin = margin(t = 20, r = 10, b = 10, l = 10)) +
  geom_col(fill = "#FF7F24")+
  geom_text(aes(label=round(count, digits=1)), vjust=-0.3, size=3.5)
