#====================================================================================================================================================================================
#====================================================================================================================================================================================
# Imports


netflix_titles <- read_csv("netflix_titles.csv", stringsAsFactors = FALSE)
View(netflix_titles)

disney_plus_titles <- read_csv("disney_plus_titles.csv")
View(disney_plus_titles)

amazon_prime_titles <- read_csv("amazon_prime_titles.csv")
View(amazon_prime_titles)

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

table(netflix_serie$rating)

barplot(table(netflix_serie$rating), main="Répartition des séries sur Netflix en fonction des ratings")


hist(table(netflix_movie$rating))
netflix_movie$rating <- gsub("66 min","",as.character(netflix_movie$rating))

table(netflix_movie$rating)

barplot(table(netflix_movie$rating), main="Répartition des films sur Netflix en fonction des ratings")
