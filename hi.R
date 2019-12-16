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

install.packages('dplyr')
library('dplyr')
df3_s1s2 = df3[df3$periode == "Q1" | df3$periode == "Q2" | df3$periode == "Q3" | df3$periode == "Q4", ]
grp <- group_by(df3_s1s2, annee,periode)
df4 = summarise(grp, min=min(valeur, na.rm=TRUE), max=max(valeur, na.rm=TRUE))
df4 = summarise(grp, moyenne=mean(valeur, na.rm=TRUE), mediane=median(valeur, na.rm=TRUE), max=max(valeur, na.rm=TRUE), min=min(valeur, na.rm=TRUE) )
pl4=ggplot(df4,aes(x=factor(periode),y=mediane, group = 1))+geom_line()
pl4+facet_wrap(annee~.)
#commentaire
