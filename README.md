# stage-master-bibs
Ce dossier stock l'ensemble des codes utilisé pour faire les analyses de mon stage de M1 BIBS
Il contient des notebook avec les commandes SPARQL afin de récupérer les données de Bio.tools et EDAM
Pour faire mes requêtes, j'utilise le logiciel GraphDB, qui permet de faire des requêtes sur des ressources RDF en utilisant du SPARQL. L'ontologie EDAM et le BioSchemas de bio.tools doit être téléchargé et intégré dans un répertoire de travail de GraphDB
Ce travail a été inspiré par cette article :
 An evaluation of EDAM coverage in the Tools Ecosystem and prototype integration of Galaxy and WorkflowHub systems



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



Bibliographie
Lamothe, L., Jensen, J.R.B., Ienasescu, H., Gustafsson, O.J.R., Gaignard, A., Repchevsky, D., Svobodová, R., Raček, T., Antol, M., Palmblad, M., Kalaš, M., Menager, H., 2023. An evaluation of EDAM coverage in the Tools Ecosystem and prototype integration of Galaxy and WorkflowHub systems (preprint). BioHackrXiv. https://doi.org/10.37044/osf.io/79kje

