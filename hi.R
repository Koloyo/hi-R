#install.packages('dplyr')
library('dplyr')
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

# Graph 3
df3=df[df$variable=="prixMoyen",]
df3=df3[df3$annee>2009,]

df3_s = df3[df3$periode == "S1" | df3$periode == "S2", ]
grp <- group_by(df3_s, annee, periode,arrondissement)
df3_s = summarise(grp, min=min(valeur, na.rm=TRUE), max=max(valeur, na.rm=TRUE))
df3_s = summarise(grp, moyenne=mean(valeur, na.rm=TRUE), mediane=median(valeur, na.rm=TRUE), max=max(valeur, na.rm=TRUE), min=min(valeur, na.rm=TRUE) )
pl3=ggplot(df3_s,aes(x=periode,y=moyenne,group=annee, color=annee))+geom_point()+xlab("periode")+facet_wrap(arrondissement~.)+ylab("Prix Moyen")+ggtitle("Evolution du prix moyen au cours des semestres selon les arrondissements")
pl3

# Graph 4 par période par année
df3=df[df$variable=="nbTotalTransactions",]
df3=df3[df3$annee>2009,]

df3_q = df3[df3$periode == "Q1" | df3$periode == "Q2" | df3$periode == "Q3" | df3$periode == "Q4", ]
grp <- group_by(df3_q, annee, periode, typeBien)
df4 = summarise(grp, min=min(valeur, na.rm=TRUE), max=max(valeur, na.rm=TRUE))
df4 = summarise(grp, moyenne=mean(valeur, na.rm=TRUE), mediane=median(valeur, na.rm=TRUE), max=max(valeur, na.rm=TRUE), min=min(valeur, na.rm=TRUE) )
pl4=ggplot(df4,aes(x=periode,y=mediane, group=typeBien, color=typeBien))+geom_line()+facet_wrap(annee~.)+ylab("nombre total de transactions")+ggtitle("Evolution du nombre total médian de transactions par quadrimestre")
pl4
