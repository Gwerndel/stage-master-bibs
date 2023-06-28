# stage-master-bibs
Ce dossier stock l'ensemble des codes utilisé pour faire les analyses de mon stage de M1 BIBS
Il contient des notebook avec les commandes SPARQL afin de récupèrer les données de Bio.tools et EDAM

liste_occurence :
Récupère la liste des TOPIC, OPERATION, FORMAT et DATA d'EDAM utilisé pour annoter les outils de bio.tools;

Profondeur :
Donne le nombre d'outils possédant des annotations de topic utilisé seul.

Pronfondeuroperation :
Donne le nombre d'outils possédant des annotations d'opération utilisé seul.

Profondeurformat :
Donne le nombre d'outils possédant des annotations de format utilisé seul.

data_seul :
Donne le nombre d'outils possédant des annotations de DATA utilisant la racine data seul.


Les codes R permettent à partir des données extraites la visualisation graphique.

annalyse.R :
Donne les codes pour refaire les figures.

top10format.R
Donne le code pour les figures feuille/noeud pour FORMAT et OPERATION

boxplot.R
Donne le code pour le boxplot comparant la dispersion des annotations des 4 sous-ontologies.
