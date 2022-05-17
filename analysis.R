#====================================================================================================================================================================================
#====================================================================================================================================================================================
# Imports

library(readr)
library(dplyr)
library(stringr)
library(ggplot2)
library(reshape2)

netflix_titles <- read_csv("netflix_titles.csv", 
                           col_types = cols(date_added = col_date(format = "%B %d, %Y")))

disney_plus_titles <- read_csv("disney_plus_titles.csv",
                               col_types = cols(date_added = col_date(format = "%B %d, %Y")))

amazon_prime_titles <- read_csv("amazon_prime_titles.csv",
                                col_types = cols(date_added = col_date(format = "%B %d, %Y")))

#====================================================================================================================================================================================
#====================================================================================================================================================================================
# ANALYSE NETFLIX

# Transformation des durées des films en nombre (en enlevant le " min")

netflix_movie <- subset(netflix_titles,type =="Movie")

netflix_movie$duration <- gsub(" min","",as.character(netflix_movie$duration))
netflix_movie$duration
netflix_movie$duration <- as.double(netflix_movie$duration)
summary(netflix_movie$duration)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 3.00   87.00   98.00   99.58  114.00  312.00       3 

#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------

# Transformation des durées des séries en nombre de saisons (en enlevant le " min")

netflix_serie <- subset(netflix_titles,type =="TV Show")

netflix_serie$duration <- gsub(" Season","",as.character(netflix_serie$duration))
netflix_serie$duration <- gsub("s","",as.character(netflix_serie$duration))
netflix_serie$duration
netflix_serie$duration <- as.double(netflix_serie$duration)
summary(netflix_serie)

#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   1.000   1.000   1.000   1.765   2.000  17.000 

# On crée un roblochon??????
options(digits=2)
percent1 <- length(netflix_serie$duration)
percent2 <- length(netflix_movie$duration)
total = length(netflix_titles$duration)
percents = round(100*c(percent1,percent2)/total,2)

lbl = c(paste('TV SHOW,',percents[1],"%"), paste('MOVIE,',percents[2],"%"))
pie(percents,labels=lbl,main="Proportions de films et de séries TV sur Netflix", col=c('#FFD07F','#E26A2C'))



attach(netflix_movie$release_year)
## --- Analyse des années de parutions
# Créer un dataframe pour chaque type de données : total, films et séries
    ## Remarques : On enlève 2021 car on n'a pas assez de données
df = data.frame(subset(subset(netflix_movie,release_year <=2020),release_year >=2000)) 
df2 = data.frame(subset(subset(netflix_serie,release_year <=2020),release_year >=2000))
df3 = data.frame(subset(subset(netflix_titles,release_year <=2020),release_year >=2000))
x1 <- 2000:2020                

data <- data.frame(table(df$release_year))
data2 <- data.frame(table(df2$release_year))
data3 <- data.frame(table(df3$release_year))


plot(2000:2020,
     data3[,2],
     type = "l",
     # Set line type to line
     lwd = 1,
     xlab="Année de parution",
     ylab="Effectif", 
     main="Polygone d'effectif des films et séries en fonction de parution")
lines(2000:2020,
      data2[,2],
      type = "l",
      lwd = 1)
lines(2000:2020,
      data[,2],
      type = "l",
      lwd = 1)

polygon(c(2000,2000:2020,2020) ,c(0,data3[,2],0),col = rgb(29, 53, 87,max = 255, alpha = 100))
polygon(c(2000,2000:2020,2020) ,c(0,data[,2],0),col = rgb(69, 123, 157,max = 255, alpha = 100))
polygon(c(2000,2000:2020,2020) ,c(0,data2[,2],0),col = rgb(168, 218, 220,max = 255, alpha = 100))

legend(2000,1100, legend=c( "Total","Films", "Série"),lwd=10,
       col=c(rgb(29, 53, 87,max = 255, alpha = 100), rgb(69, 123, 157,max = 255, alpha = 100), rgb(168, 218, 220,max = 255, alpha = 100)), lty=1:2, cex=0.8)

netflix_movie$date_added =format(netflix_movie$date_added,'%Y')

## --- Analyse des années d'ajouts
# Créer un dataframe pour chaque type de données : total, films et séries
  ## Remarques : On enlève 2021 car on n'a pas assez de données
netflix_movie$date_added =format(netflix_movie$date_added,'%Y')
netflix_serie$date_added =format(netflix_serie$date_added,'%Y')
netflix_titles$date_added =format(netflix_titles$date_added,'%Y')

df = data.frame(subset(subset(netflix_movie,date_added <=2020),date_added >=2013)) 
df2 = data.frame(subset(subset(netflix_serie,date_added <=2020),date_added >=2013))
df3 = data.frame(subset(subset(netflix_titles,date_added <=2020),date_added >=2013))
               

data <- data.frame(table(df$date_added))
data2 <- data.frame(table(df2$date_added))
data3 <- data.frame(table(df3$date_added))

x1 <- 2013:2020

plot(x1,
     data3[,2],
     type = "l",     
     lwd = 1,
     xlab="Année de parution",
     ylab="Effectif", 
     main="Polygone d'effectif des films et séries en fonction de leur année d'ajout")
lines(x1,
      data2[,2],
      type = "l",     
      lwd = 1)
lines(x1,
      data[,2],
      type = "l",     
      lwd = 1)

polygon(c(2000,x1,2020) ,c(0,data3[,2],0),col = rgb(29, 53, 87,max = 255, alpha = 100))
polygon(c(2000,x1,2020) ,c(0,data[,2],0),col = rgb(69, 123, 157,max = 255, alpha = 100))
polygon(c(2000,x1,2020) ,c(0,data2[,2],0),col = rgb(168, 218, 220,max = 255, alpha = 100))

legend(2013,2000, legend=c( "Total","Films", "Série"),lwd=10,
       col=c(rgb(29, 53, 87,max = 255, alpha = 100), rgb(69, 123, 157,max = 255, alpha = 100), rgb(168, 218, 220,max = 255, alpha = 100)), lty=1:2, cex=0.8)





hist(table(netflix_serie$rating))
netflix_serie$rating <- gsub("84 min","",as.character(netflix_serie$rating))
netflix_serie$rating <- gsub("74 min","",as.character(netflix_serie$rating))
netflix_serie$rating <- gsub("84 min","",as.character(netflix_serie$rating))

table(netflix_serie$rating)

barplot(table(netflix_serie$rating), main="Répartition des séries sur Netflix en fonction des ratings")


hist(table(netflix_movie$rating))
netflix_movie$rating <- gsub("66 min","",as.character(netflix_movie$rating))
netflix_movie$rating <- gsub("84 min","",as.character(netflix_movie$rating))
netflix_movie$rating <- gsub("74 min","",as.character(netflix_movie$rating))

table(netflix_movie$rating)

barplot(table(netflix_movie$rating), main="Répartition des films sur Netflix en fonction des ratings")

#Audience visée des films

netflixPublicCibleFilms <- netflix_movie
netflixPublicCibleFilms$rating <- gsub("TV-14|PG-13","Teens",as.character(netflixPublicCibleFilms$rating))
netflixPublicCibleFilms$rating <- gsub("TV-PG|PG|TV-Y7-FV|TV-Y7","Older kids",as.character(netflixPublicCibleFilms$rating))
netflixPublicCibleFilms$rating <- gsub("TV-Y|TV-G|G","Kids",as.character(netflixPublicCibleFilms$rating))
netflixPublicCibleFilms$rating <- gsub("TV-MA|NR|UR|NC-17|R","Adults",as.character(netflixPublicCibleFilms$rating))

# Catégories uniques -> dans les films proposés, netflix cherche à avoir un catalogue diversifié, ayant des catégories pour tous les types de publics 
unique(na.omit(autre$rating))

barplot(main="Audience Visée des films selon la tranche d'âge",xlab="Tranche d'âge", ylab="Nombre de films", tail(sort(table(autre$rating),decreasing=FALSE),n=4))

#Audience visée des séries

autreSerie <- netflix_serie

autreSerie$rating <- gsub("TV-14|PG-13","Teens",as.character(autreSerie$rating))
autreSerie$rating <- gsub("TV-PG|PG|TV-Y7-FV|TV-Y7","Older kids",as.character(autreSerie$rating))
autreSerie$rating <- gsub("TV-Y|TV-G|G","Kids",as.character(autreSerie$rating))
autreSerie$rating <- gsub("TV-MA|NR|UR|NC-17|R","Adults",as.character(autreSerie$rating))

# Catégories uniques -> dans les séries aussi, netflix cherche à avoir un catalogue diversifié
unique(na.omit(autreSerie$rating))

barplot(main="Audience Visée des séries selon la tranche d'âge",xlab="Tranche d'âge", ylab="Nombre de séries", tail(sort(table(autreSerie$rating),decreasing=FALSE),n=4),col=c("green","yellow","orange","red"))


# Voir les pays avec le plus de films.


x <- netflix_titles

for (i in 1:length(netflix_titles$country))
{
  y <- str_split(netflix_titles$country[i],",", simplify = TRUE)
  netflix_titles$country[i] <- y[1,1]
}

barplot(main="TOP 10 des pays sur Netflix",ylab="Nombre de pays",xlab="Pays de production",head(sort(table(netflix_titles$country),decreasing=TRUE),n=10))

# Test de Shapiro


# Trier les Na pour les mettre en bas du tableau, et on les supprime
ShapiroTest <- netflix_movie[order(netflix_movie$duration),]
ShapiroTest <- head(ShapiroTest,-3)


shuffled_data  =  ShapiroTest[sample(1:nrow(ShapiroTest)),]
shuffled_data <- head(shuffled_data,-1200)
barplot(table(shuffled_data$duration)) 
# On s'attend à une loi distribuée normalement au vu du graphique
shapiro.test(shuffled_data$duration)
# --- 
# W = 1, p-value <2e-16
# Non symétrique, distribution qui s'écarte trop de la moyenne à gauche --> Distribution non normale




#====================================================================================================================================================================================
#====================================================================================================================================================================================
# ANALYSE Amazon Prime

# Transformation des durées des films en nombre (en enlevant le " min")

amazon_movie <- subset(amazon_prime_titles,type =="Movie")

amazon_movie$duration <- gsub(" min","",as.character(amazon_movie$duration))
amazon_movie$duration
amazon_movie$duration <- as.double(amazon_movie$duration)
summary(amazon_movie$duration)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   0      75      91      91      106    601

#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
#Audience visée des films
amazonPublicCible <- amazon_movie

amazonPublicCible$rating <- gsub("13+","Teens",as.character(amazonPublicCible$rating), fixed=TRUE)
amazonPublicCible$rating <- gsub("16+","Older Teens",as.character(amazonPublicCible$rating), fixed=TRUE)
amazonPublicCible$rating <- gsub("7+","Older kids",as.character(amazonPublicCible$rating),fixed=TRUE)
amazonPublicCible$rating <- gsub("18+","Adults",as.character(amazonPublicCible$rating), fixed=TRUE)

amazonPublicCible$rating <- gsub("TV-14|PG-13","Teens",as.character(amazonPublicCible$rating))
amazonPublicCible$rating <- gsub("AGES_16_|16","Older Teens",as.character(amazonPublicCible$rating))
amazonPublicCible$rating <- gsub("TV-PG|PG|TV-Y7-FV|TV-Y7","Older kids",as.character(amazonPublicCible$rating))
amazonPublicCible$rating <- gsub("TV-MA|NR|UR|NC-17|R|UNRATED|NOT_RATE|AGES_18_|18+","Adults",as.character(amazonPublicCible$rating))
amazonPublicCible$rating <- gsub("TV-Y|TV-G|ALL_AGES|ALL|G","Kids",as.character(amazonPublicCible$rating))

# Catégories uniques -> 
unique(na.omit(amazonPublicCible$rating))

barplot(main="Audience visée des films selon la tranche d'âge, Amazon",
        xlab="Tranche d'âge", 
        ylab="Nombre de films", 
        sort(table(amazonPublicCible$rating),
             decreasing=FALSE))



# Test de Shapiro Amazon
barplot(table(amazon_movie$duration))
shuffled_data2  =  amazon_movie[sample(1:nrow(amazon_movie)),]
shuffled_data2 <- head(shuffled_data2,-3000)
shapiro.test(shuffled_data2$duration)
# La durée des films sur amazon ne peut suivre une loi normale, rejet de l'hypothèse
summary(amazon_movie$duration)
summary(netflix_movie$duration)

# Test de Wilcoxon #Trouver les raisons
wilcox.test(amazon_movie$duration,netflix_movie$duration)
# W = 2e+07, p-value <2e-16

var.test(amazon_movie$duration,netflix_movie$duration,alternative = "greater")



#====================================================================================================================================================================================
#====================================================================================================================================================================================
# ANALYSE Disney+

# Transformation des durées des films en nombre (en enlevant le " min")

disney_movie <- subset(disney_plus_titles,type =="Movie")

disney_movie$duration <- gsub(" min","",as.character(disney_movie$duration))
disney_movie$duration
disney_movie$duration <- as.double(disney_movie$duration)
summary(disney_movie$duration)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  1    44      85      72      98     183 

#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
#Audience visée des films

disneyPublicCible <- disney_movie
disneyPublicCible$rating <- gsub("TV-14|PG-13","Teens",as.character(disneyPublicCible$rating))
disneyPublicCible$rating <- gsub("TV-PG|PG|TV-Y7-FV|TV-Y7","Older kids",as.character(disneyPublicCible$rating))
disneyPublicCible$rating <- gsub("TV-Y|TV-G|G","Kids",as.character(disneyPublicCible$rating))
disneyPublicCible$rating <- gsub("TV-MA|NR|UR|NC-17|R","Adults",as.character(disneyPublicCible$rating))


# Catégories uniques -> on peut voir qu'il n'y a pas de film dont l'audience visée est des adultes, disney+ semble donc avoir pour public cible
# les enfant et les adolescents principalement, cette plateforme ne souhaite pas de film qui ne s'adresseraient qu'aux personnes adultes. 
unique(na.omit(disneyPublicCible$rating))

barplot(main="Audience visée des films selon la tranche d'âge, Disney+",xlab="Tranche d'âge", ylab="Nombre de films", sort(table(disneyPublicCible$rating),decreasing=FALSE), col=c("green","orange","yellow"))





