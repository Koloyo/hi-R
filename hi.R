library("ggplot2")
df = read.csv2("C:/Users/DELL/Desktop/Master1 STIC/Visualisation des données de l'information/data project.csv", sep=",")

df$annee
df$periode

df[df$periode == "Y", ]
dfY= df[df$periode == "Y", ]
ggplot(dfY, aes(x=annee, y=valeur)) + geom_line()
head(dfY)
unique(df$variable)

dfBruxelles= df[df$arrondissement=="Bruxelles-Capitale",]
dfBruxelles= dfBruxelles[dfBruxelles$typeBien=="appartements",]

dfBruxelles= dfBruxelles[dfBruxelles$variable	=="nbTotalTransactions",]

dfBruxelles$valeur<-as.integer(dfBruxelles$valeur)
pl <-ggplot(dfBruxelles, aes(x=annee, y=valeur)) + geom_line()
pl+ylab("Nombre total de transactions")+facet_wrap(periode ~ .)+ggtitle("Nombre total de transactions d'appartements à Bruxelles Capitale")+xlab("Années") + scale_y_continuous(breaks=seq(0,30000,5000))


df$valeur<-as.integer(df$valeur)
df2= df[df$variable	=="prixMoyen",]
pl2=ggplot(df2, aes(x=annee, y=valeur,fill=arrondissement))+geom_bar(stat = "identity")+ggtitle("Prix moyen de tout type d'habitations en Belgique par an")+xlab("Années")+ylab("Prix Moyen")
pl2

df3=df[df$variable=="prixMoyen",]
pl3=ggplot(df3,aes(x=periode,y=valeur))+geom_line()+xlab("periode")+facet_wrap(arrondissement~.)+ylab("Prix Moyen")+ggtitle("Prix moyen sur les périodes")
pl3