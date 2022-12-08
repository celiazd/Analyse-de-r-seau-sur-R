

#------- Célia Zaidi, M2 ENSP  -----------#


# Chargement des packages
library(FactoMineR)
library(sna)
library(network)
library(igraph)
library(dplyr)
library(reshape2)
library(statnet)
library(devtools)
library(concorR)
library(tergm)
library(ergm.count)

#répertoire de travail
setwd("/Users/celiazaidi/Desktop/ARS M2/cours")


#chargement du réseau
Twitch <- read.delim2("Blog.csv", header=TRUE, sep=";", row.names = 1)
Twitch <-as.network.matrix(Twitch,matrix.type="adjacency")
plot(Twitch)

#visualisation du réseau
Twitch2 <- as.matrix(Twitch)
reseau_twitch <-graph_from_adjacency_matrix(Twitch2)

plot.igraph(reseau_twitch, layout=layout.auto, 
            edge.color = "grey",
            edge.arrow.size = .1,
            edge.width=.3,
            edge.curved=0,
            vertex.color='blue',
            vertex.frame.color='blue',
            vertex.size = 3,
            vertex.label.cex = .5,
            vertex.label.color='black',
            vertex.shape = "circle",
            vertex.label.font = 1,
            vertex.label.dist = .7,
            main = "Réseau personnel des abonnées de NoisyButters sur Twitch.tv",
            sub = "Source : Twitch.tv, données recueillies par Alicia Gamez-Belmont en 2019",
            rescale=T,
            frame=T)

#Exporter en PNG
png("réseau_twitch.png", width=3000, height=2500, units="px", pointsize=40)
plot.igraph(reseau_twitch, layout=layout.auto, 
            edge.color = "grey",
            edge.arrow.size = .1,
            edge.width=.3,
            edge.curved=0,
            vertex.color='blue',
            vertex.frame.color='blue',
            vertex.size = 3,
            vertex.label.cex = .5,
            vertex.label.color='black',
            vertex.shape = "circle",
            vertex.label.font = 1,
            vertex.label.dist = .7,
            main = "Réseau personnel des abonnées de NoisyButters sur Twitch.tv",
            sub = "Source : Twitch.tv, données recueillies par Alicia Gamez-Belmont en 2019",
            rescale=T,
            frame=T)
dev.off()

# Description du réseau

twitch_desc <- data.frame(
  'Nb sommets' = gorder(reseau_twitch), # nombre de sommets
  'Nb arrêtes' = gsize(reseau_twitch), # nombre d'arêtes
  'Moy longeurs chemins' = mean_distance(reseau_twitch), # moyenne des longueurs des chemins
  'Densité graphe' = edge_density(reseau_twitch)*100, # densité du graphe
  Réciprocité = reciprocity(reseau_twitch, ignore.loops=FALSE), # réciprocité (pour graphes dirigés)
  Transitivité = transitivity(reseau_twitch, type="global")*100 # transitivité (clôture des triades)
)


write.csv2(twitch_desc, "twitch_desc.csv", row.names = T)




#On charge les attribtus en format .csv
Attributs <- read.delim2("blog_vertices.csv", sep=";",header=TRUE,row.names=1)

#On liste les noms de colonnes
nomscol<-colnames(Attributs)

#On crée un fichier qui dichotomise les variables catégorielles
#La fonction tab.disjonctif se trouve dans la library FactoMineR

Attributs$sexe <- as.factor(Attributs$sexe)
Attributs$jeu <- as.factor(Attributs$jeu)
Attributs$abonnes <- as.factor(Attributs$abonnes)

#ici on dichotomise les Va ex Sexe : Sexe1, Sexe2

data <- cbind(Attributs,tab.disjonctif(Attributs[,c(1)]), 
              tab.disjonctif(Attributs[,c(2)]),
              tab.disjonctif(Attributs[,c(3)]))

# On renomme les nouvelles colonnes
colnames(data)<-c(nomscol, "Homme", "Femme",     
                  "statut1", "statut2",
                  "statut3", "statut4","Musical",
                  "Rôle", "Combat","Tir", "Stratégie",
                  "Simulation","Plateforme","Autre")

# On crée des indicateurs de similarité

# Similarité de genre
Homme <- data$Homme%*%(t(data$Homme))
Femme <- data$Femme%*%(t(data$Femme))

# Similarité du nb d'abonnés 
statut1 <- data$statut1%*%(t(data$statut1))
statut2 <- data$statut2%*%(t(data$statut2))
statut3 <- data$statut3%*%(t(data$statut3))
statut4 <- data$statut4%*%(t(data$statut4))

# Similarité de la catégorie de jeu
Musical <- data$Musical%*%(t(data$Musical))
Rôle <- data$Rôle%*%(t(data$Rôle))
Combat <- data$Combat%*%(t(data$Combat))
Tir <- data$Tir%*%(t(data$Tir))
Stratégie <- data$Stratégie%*%(t(data$Stratégie))
Simulation <- data$Simulation%*%(t(data$Simulation))
Plateforme <- data$Plateforme%*%(t(data$Plateforme))
Autre <- data$Autre%*%(t(data$Autre))


# On crée une méta matrice (array) regroupant l'ensemble des données

VAR<-array(dim=c(18,98,98))

#similarité de genre
VAR[1,,]<-Homme
VAR[2,,]<- Femme

#similarité du sexe mais on ne sait pas duquel
VAR[3,,]<-Homme+Femme

VAR[4,,]<- statut1
VAR[5,,]<- statut2
VAR[6,,]<- statut3
VAR[7,,]<- statut4

#similarité du nb dabonnés mais on ne sait pas duquel
VAR[8,,]<-statut1+statut2+statut3+statut4

VAR[9,,]<- Musical
VAR[10,,]<- Rôle
VAR[11,,]<- Combat
VAR[12,,]<- Tir
VAR[13,,]<- Stratégie
VAR[14,,]<- Simulation
VAR[15,,]<- Plateforme
VAR[16,,]<- Autre
#similarité du jeu mais on ne sait pas duquel
VAR[17,,]<-Musical+Rôle+Combat+Tir+Stratégie+Simulation+Plateforme+Autre


#On ajoute le réseaux 
VAR[18,,]<-as.matrix(Twitch)


# On réalise les tests QAP :

#Similarité de sexe
qap_sexe <-qaptest(VAR,gcor,g1=18,g2=3)
summary(qap_sexe)
plot(qap_sexe)

#Similarité de statut
qap_abonnes <-qaptest(VAR,gcor,g1=18,g2=8)
summary(qap_abonnes)
plot(qap_abonnes)

#Similarité de jeu
qap_jeu <-qaptest(VAR,gcor,g1=18,g2=17)
summary(qap_jeu)
plot(qap_jeu)


#Graphe d'ensemble
resum <- par(mfrow = c(1, 3))
plot(qap_sexe)
plot(qap_abonnes)
plot(qap_jeu)
par(resum)