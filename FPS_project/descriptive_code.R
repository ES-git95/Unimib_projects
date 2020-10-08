df=read.csv("C:/Users/Eric/Desktop/HappinessAlcoholConsumption.csv")

df[,5]=df[,5]/1000

str(df)

#DESCRITTIVA
df=df[,-1]
#A.DESCRITTIVA UNIVARIATA
library(car)

#1.IL DATASET NON HA NA

#2.QUANTITATIVE: ISTOGRAMMI DI FREQUENZA E BOXPLOT
VAR_N=colnames(df)[4:9]

par(mfrow=c(3,2)) #istogrammi di frequenza
for(i in VAR_N){
  hist(as.numeric(df[,i]),main=i,col="lightblue",xlab=i,freq=F,breaks = 'FD')
  lines(density(df[,i]))
  rug(df[,i])
  }
par(mfrow=c(1,1))

as.numeric(df[,4])

with(df,{ #istogramma variabile di interesse
  hist(HappinessScore,freq=F,breaks='FD',col="green")
  lines(density(HappinessScore,bw = .7),col=2)
  rug(HappinessScore)
  box()
})


par(mfrow=c(3,2)) #boxplot
for(i in VAR_N){
  boxplot(df[,i],main=i,col="lightblue",pch=21,horizontal=TRUE)
  rug(df[,i])
}
par(mfrow=c(1,1))

ls=list() #outliers
for( i in VAR_N){
  ls[[i]]=df[Boxplot(df[,i],horizontal = TRUE),1]
}
ls

#creazione nuovo df con variabile "somma sostanze alcoliche"
alcoholic_sub=as.numeric(unlist(df[8]+df[9]+df[7]))

df_alcoholic_sub=df
df_alcoholic_sub=df[1:6]
df_alcoholic_sub['alcoholic substances']=alcoholic_sub

summary(df_alcoholic_sub)

#indici di sintesi
summary(df[4:9])

summary(df_alcoholic_sub[4:7])



#------------modifica GDP dataframe con df_1


df_1=read.csv2('C:/Users/Eric/Desktop/GDP_pro_capite.csv')

for(i in 1:length(df_1[,'Paese'])){
  for(j in 1:length(df[,'Country'])){
    if(as.vector(df_1[i,'Paese'])==as.vector(df[j,'Country'])){
      df[j,'GDP']=as.numeric(as.vector(df_1[i,'GDP.pro.capite']))
  }
  }
}
df=df[,-6]

df=df[,c(1,2,3,4,5,9,6,7,8)]
#--------------------------------------------

#3.QUALITATIVE:
#BARPLOT REGION
table(df$Region) #frequenze assolute Region
tab1=table(df$Region)/length(df$Region) #frequenze relative Region

par(mar = c(5, 9, 4, 2))
with(df,{
  tab1=table(df$Region)
  barplot(sort(tab1),main="Regioni",xlab="Tipologia",horiz=TRUE,las=1,cex.names = .6,col="Orange",xlim=c(0,30))
})
par(mar = c(5, 4, 4, 2))

#TORTA EMISFERO
tab2=table(df$Hemisphere)/length(df$Hemisphere)

categorie = levels(df$Hemisphere)
pct = round(as.vector(tab2)*100)
categorie = paste(categorie, pct)
categorie = paste(categorie,"%",sep="")
pie(tab2,labels = categorie,
    main="Emisfero (dati in %)",col=c('#eafa05','#3ca5de','#de3c4c'))

paste('a','b',sep='-')


#B.DESCRITTIVA BIVARIATA

#creazione classi per continente

Europa=df[which(grepl('Europe',df[,2])),]
Europa[,'Continent']='Europa'
America=df[which(grepl('America',df[,2])),]
America[,'Continent']='America'
Africa=df[which(grepl('Africa',df[,2])),]
Africa[,'Continent']='Africa'
Asia=df[which(grepl('Asia',df[,2])),]
Asia[,'Continent']='Asia'
Australia=df[which(grepl('Australia',df[,2])),]
Australia[,'Continent']='Australia'

df_country_sub=merge(Europa, America,  all = TRUE)
df_country_sub=merge(df_country_sub,Africa,all=TRUE)
df_country_sub=merge(df_country_sub,Asia,all=TRUE)
df_country_sub=merge(df_country_sub,Australia,all=TRUE)
df_country_sub #creazione data frame con sostanze alcoliche e continente

for(i in 1:length(df_alcoholic_sub[,'Country'])){
  for(j in 1:length(df_country_sub[,'Country'])){
    if(df_alcoholic_sub[i,'Country']==df_country_sub[j,'Country']){
      df_alcoholic_sub[i,'Continent']=df_country_sub[j,'Continent']
    }
  }
}
df_alcoholic_sub

#creazione variabile categoriale in classi rispetto a HAPPINESS SCORE
happiness_qual=cut(df$HappinessScore,breaks = c(3,4.5,6.5,8),labels=c('felicità bassa','felicità parziale','felicità piena'))


#creazione variabile categoriale in classi rispetto a REGION
Europa['Regio_qual']=rep('Europa',length(Europa['Region']))
America['Regio_qual']=rep('America',length(America['Region']))
Africa['Regio_qual']=rep('Africa',length(Africa['Region']))
Asia['Regio_qual']=rep('Asia',length(Asia['Region']))
Australia['Regio_qual']=rep('Australia',length(Australia['Region']))

region_qual=append(Europa$Regio_qual,c(America$Regio_qual,Africa$Regio_qual,Asia$Regio_qual,Australia$Regio_qual))
length(Australia[,'GDP'])

#creazione variabile categoriale in classi rispetto a GDP
with(df,{ #istogramma variabile di interesse
  hist(GDP,freq=F,breaks='FD',col="green")
  lines(density(GDP),col=2)
  rug(GDP)
  box()
})

library(car)
df[Boxplot(df$GDP,pch=20),c(1,6)]

gdp_qual=cut(df$GDP,breaks = c(0,5000,15000,40000,150000),labels=c('molto basso','basso','medio','alto'))
table(gdp_qual)

#creazione variabile categoriale in classi rispetto a HDI
HDI_qual=cut(df$HDI,breaks = c(0,0.554,0.699,.799,1),labels=c('molto arretrato','arretrato','avanzato','molto avanzato'))
table(HDI_qual)

#creazione variabile categoriale in classi rispetto a ALCOHOLIC SUBSTANCES
alch_qual=cut(df_alcoholic_sub$`alcoholic substances`,breaks = c(0,250,500,800),labels=c('basso consumo','medio consumo','alto consumo'))
table(alch_qual)


#CONNESSIONE
tab1=table(happiness_qual,gdp_qual) 
chi1=chisq.test(tab1) #esiste una fortissima dipendenza tra le due variabili
chi1$statistic
pchisq(chi$statistic,6)
chi_norm1<-chi1$statistic/(nrow(df)*min(nrow(tab1)-1,ncol(tab1)-1))
chi_norm1



tab2=table(happiness_qual,alch_qual)
chi2=chisq.test(tab2) #esiste una forte dipendenza tra le due variabili
chi2$statistic        
chi_norm2<-chi2$statistic/(nrow(df)*min(nrow(tab2)-1,ncol(tab2)-1))
chi_norm2



tab3=table(gdp_qual,alch_qual)
chi3=chisq.test(tab3) #esiste una forte dipendenza tra le due variabili
chi3$statistic        
chi_norm3<-chi3$statistic/(nrow(df)*min(nrow(tab3)-1,ncol(tab3)-1))
chi_norm3



#connessione tra le sostanze alcoliche

#BOXPLOT BIVARIATI

attach(df_alcoholic_sub)

#box rispetto a regione

par(mar = c(5, 6, 4, 2))
par(mfrow=c(2,2))
boxplot(HappinessScore ~ Continent, 
        main="Boxplot felicità per continente",
        horizontal = TRUE, col=c('#3bf5ad','#3bf5ef','#3bb7f5','#3b5af5','#7c3bf5'),pch=20,las=1)

boxplot(HappinessScore ~ alch_qual, 
        main="Boxplot felicità per consumo alcolico", 
        col=c('#3bf5ad','#3bf5ef','#3bb7f5'), 
        horizontal = TRUE,pch=20,las=1)

boxplot(HappinessScore~ gdp_qual, 
        main="Boxplot felicità per GDP",
        horizontal = TRUE,col=c('#3bf5ad','#3bf5ef','#3bb7f5','#3b5af5'),pch=20,las=1)

boxplot(HappinessScore~ HDI_qual, 
        main="Boxplot felicità per HDI",
        horizontal = TRUE,col=c('#3bf5ad','#3bf5ef','#3bb7f5','#3b5af5'),pch=20,las=1)
par(mfrow=c(1,1))
par(mar = c(5, 4, 4, 2))

boxplot(GDP ~ alch_qual, 
        main="Boxplot comparing gdp by region",horizontal=TRUE, col=rainbow(5))


boxplot(GDP ~ Continent, 
        main="Boxplot comparing hdi by region",
        horizontal = TRUE, col=rainbow(5))



boxplot(`alcoholic substances` ~ Continent, 
        main="Boxplot comparing alcoholic sub by region",
        horizontal = TRUE,col=rainbow(5))



#box rispetto a consumo alcolico #???
boxplot(HappinessScore ~ alch_qual, 
        main="Boxplot comparing happiness by alcohol", 
        col= rainbow(3), 
        horizontal = TRUE)

boxplot(GDP ~ alch_qual, 
        main="Boxplot comparing gdp by alcohol", 
        col= rainbow(3), 
        horizontal = TRUE)

boxplot(HDI ~ alch_qual, 
        main="Boxplot comparing hdi by alcohol", 
        col= rainbow(3), 
        horizontal = TRUE)


#INDICE DI GINI  #raggruppare per continenti
library(dplyr)
levels(df_alcoholic_sub$Region)=levels(df$Region)

df_grouped=df_alcoholic_sub %>%group_by(Region)

sum_gdp=sum(df_alcoholic_sub$GDP) #considero le relative sommando il gdp di ciasuno stato
df_grouped_gdp=df_grouped %>% summarise(gdp=sum(GDP)/sum_gdp) 

df_grouped_gdp_mean=df_grouped %>% summarise(gdp=mean(GDP)) #questa ipotesi sembra la più sensata
sum_gdp_mean=sum(df_grouped_gdp_mean$gdp) #considero le relative dalle medie di ciascuno stato
df_grouped_gdp_mean=df_grouped %>% summarise(gdp=mean(GDP)/sum_gdp_mean)

gini_gdp=1-sum(df_grouped_gdp_mean$gdp**2) #GINI GDP
k1=length(df_grouped_gdp_mean$gdp)
gini_gdp_norm=gini_gdp*(k1/(k1-1))



df_grouped_happiness=df_grouped %>% summarise(happiness=mean(HappinessScore)) 
sum_happiness_mean=sum(df_grouped_happiness$happiness)
df_grouped_happiness_mean=df_grouped %>% summarise(happiness=mean(HappinessScore)/sum_happiness_mean) 

gini_happiness=1-sum(df_grouped_happiness_mean$happiness**2) #GINI HAPPINESS
k2=length(df_grouped_happiness_mean$happiness)
gini_happiness_norm=gini_happiness*(k2/(k2-1))



df_grouped_alcohol_mean=df_grouped %>% summarise(alcohol=mean(`alcoholic substances`)) 
sum_alcohol=sum(df_grouped_alcohol_mean$`alcoholic substances`)
df_grouped_alcohol=df_grouped %>% summarise(alcohol=mean(`alcoholic substances`)/sum_alcohol) 

gini_alcohol=1-sum(df_grouped_alcohol_mean$alcohol**2) #GINI ALCOHOL
k3=length(df_grouped_alcohol_mean$alcohol)
gini_alcohol_norm=gini_alcohol*(k3/(k3-1))


#gini sulle singole sostanze alcoliche
mean1=mean(df$Beer_PerCapita)
mean2=mean(df$Spirit_PerCapita)
mean3=mean(df$Wine_PerCapita)
mean_sum=sum(mean1,mean2,mean3)

gini_alch_kind=1-sum((c(mean1,mean2,mean3)/mean_sum)**2)
k4=length(df$Beer_PerCapita)
gini_alch_kind_norm=gini_alch_kind*(k4/(k4-1))


hap_gini_tab=table(happiness_qual)/sum(table(happiness_qual))

gini_hap_qual=1-sum(as.vector(hap_gini_tab)**2)
k4=length(df$Beer_PerCapita)
gini_hap_qual_norm=gini_hap_qual*(k4/(k4-1))


hdi_gini_tab=table(HDI_qual)/sum(table(HDI_qual))

gini_hdi_qual=1-sum(as.vector(hdi_gini_tab)**2)
k4=length(df$Beer_PerCapita)
gini_hdi_qual_norm=gini_hdi_qual*(k4/(k4-1))


gdp_gini_tab=table(gdp_qual)/sum(table(gdp_qual))

gini_gdp_qual=1-sum(as.vector(gdp_gini_tab)**2)
k4=length(df$Beer_PerCapita)
gini_gdp_qual_norm=gini_gdp_qual*(k4/(k4-1))


alch_qual_gini_tab=table(alch_qual)/sum(table(alch_qual))

gini_alch_qual=1-sum(as.vector(alch_qual_gini_tab)**2)
k4=length(df$Beer_PerCapita)
gini_alch_qual_norm=gini_alch_qual*(k4/(k4-1))



#ANOVA #raggruppare per continente
df_grouped_continent=df_alcoholic_sub %>%group_by(df_alcoholic_sub$Continent)
df_grouped_continent_gdp_mean=df_grouped_continent %>%summarise(gdp_mean=mean(GDP))
gini_gdp_continent=1-sum(df_grouped_continent_gdp$gdp_mean**2) #GINI GDP
k1_1=length(df_grouped_continent_gdp_mean$gdp_mean)
gini_gdp_norm_1=gini_gdp*(k1_1/(k1_1-1)) #non raggruppo per continente perchè la variazione tra variabili è meno precisa


df_grouped_gdp_mean=df_grouped %>% summarise(gdp_mean=mean(GDP))

levels_region=levels(df_alcoholic_sub[,'Region'])


summary(aov(GDP~Region,df_alcoholic_sub)) 

df_grouped_happiness_mean=df_grouped %>% summarise(happiness_mean=mean(HappinessScore)) 

summary(aov(HappinessScore~Region,df_alcoholic_sub))

df_grouped_alcohol_mean=df_grouped %>% summarise(alcohol_mean=mean(`alcoholic substances`))

summary(aov(`alcoholic substances`~Region,df_alcoholic_sub))

#anova su consumo di ciascun alcolico per regione  #prova per continente
summary(aov(Beer_PerCapita~Region,df))

summary(aov(Spirit_PerCapita~Region,df))

summary(aov(Wine_PerCapita~Region,df))


#Coefficiente variazione CV
ls_cv=list()
for(i in 4:7){
  ls_cv[i-3]=cv(df_alcoholic_sub[,i])
}
ls_cv

ls_cv_1=list()#cv per le variabili alcoholiche
for(i in 4:9){
  ls_cv_1[i-3]=cv(df[,i])
}
ls_cv_1
cv(df[,4])

#BARPLOTS BIVARIATI
happiness_qual_1=cut(df$HappinessScore,breaks = c(3,5.3,5.7,8),labels=c('felicità bassa','felicità parziale','felicità piena'))
happiness_qual_1_estremi=as.vector(happiness_qual_1[-which(happiness_qual_1=='felicità parziale')])

alch_qual_1=alch_qual[-which(happiness_qual_1=='felicità parziale')]

cong_al.ha=table(alch_qual_1,happiness_qual_1_estremi)
cong_rel_al.ha=prop.table(cong_al.ha,1)

barplot(t(cong_rel_al.ha),names.arg = rownames(cong_rel_al.ha),legend=colnames(cong_rel_al.ha),col=c('Red','LightBlue'))


#gdp_qual_1=cut(df$GDP[-(which(scale(GDP)>0 & scale(GDP)<0.5))],breaks = c(0,19567.6,94921),labels=c('paesi più poveri','paesi più ricchi'))
#sum(table(df_alcoholic_sub$`alcoholic substances`[which(df_alcoholic_sub$`alcoholic substances`>=319 & df_alcoholic_sub$`alcoholic substances`<=372)]))
alch_qual_1=cut(df_alcoholic_sub$`alcoholic substances`[-which(df_alcoholic_sub$`alcoholic substances`>=319 & df_alcoholic_sub$`alcoholic substances`<=372)],breaks=c(0,320,700),labels=c('consumo minore','consumo maggiore'))
gdp_qual_1=gdp_qual[-which(df_alcoholic_sub$`alcoholic substances`>=319 & df_alcoholic_sub$`alcoholic substances`<=372)]
cong_gdp.al=table(gdp_qual_1,alch_qual_1) 
cong_rel_gdp.al=prop.table(cong_gdp.al,1)

barplot(t(1-cong_rel_gdp.al),names.arg = rownames(cong_rel_gdp.al),col=c('Red','LightBlue'))
legend(3.2,0.97,legend=c('consumo maggiore','consumo minore'),col=c('Red','LightBlue'),cex=.7,pch=15)


df$GDP[-(which(scale(GDP)>0))]
gdp_qual_1=gdp_qual[-which(happiness_qual_1=='felicità parziale')]

cong_gdp.ha=table(gdp_qual_1,happiness_qual_1_estremi)
cong_rel_gdp.ha=prop.table(cong_gdp.ha,1)

barplot(t(cong_rel_gdp.ha),names.arg = rownames(cong_rel_gdp.ha),legend=colnames(cong_rel_gdp.ha),col=c('Red','LightBlue'))


hdi_qual_1=HDI_qual[-which(happiness_qual_1=='felicità parziale')]

cong_hdi.ha=table(hdi_qual_1,happiness_qual_1_estremi)
cong_rel_hdi.ha=prop.table(cong_hdi.ha,1)

barplot(t(cong_rel_hdi.ha),names.arg = rownames(cong_rel_hdi.ha),legend=colnames(cong_rel_hdi.ha),col=c('Red','LightBlue'))



df1_grouped=df %>%group_by(df$Region)
df1_grouped_alch=df_grouped %>% summarise(`alcoholic substances`=sum(`alcoholic substances`))
df1_grouped_alch[order(df1_grouped_alch$`alcoholic substances`),]

df1_grouped_sum=df1_grouped %>% summarise(sum(Beer_PerCapita),sum(Spirit_PerCapita),sum(Wine_PerCapita))
df_prova=data.frame('Region'=c(rep('Australia and New Zealand',1002),rep('Central and Eastern Europe',11493),rep('Eastern Asia',1013),rep('Latin America and Caribbean',7019),rep('Middle East and Northern Africa',842),rep('North America',953),rep('Southeastern Asia',936),rep('Sub-Saharan Africa',3120),rep('Western Europe',10315)),
                    'Alcohol'=c(rep('Beer',464),rep('Spirit',151),rep('Wine',387),rep('Beer',4592),rep('Spirit',4638),rep('Wine',2263),rep('Beer',373),rep('Spirit',599),rep('Wine',41),rep('Beer',3668),rep('Spirit',2528),rep('Wine',823),rep('Beer',273),rep('Spirit',455),
                                rep('Wine',114),rep('Beer',489),rep('Spirit',280),rep('Wine',184),rep('Beer',398),rep('Spirit',523),rep('Wine',15),rep('Beer',2197),rep('Spirit',581),rep('Wine',342),rep('Beer',4329),rep('Spirit',2030),rep('Wine',3956)))

df_prova[,1]=ordered(df_prova$Region,levels=c('Middle East and Northern Africa','Southeastern Asia','North America','Australia and New Zealand','Eastern Asia','Sub-Saharan Africa','Latin America and Caribbean','Western Europe','Central and Eastern Europe'))
df_prova_1=df_prova[which(df_prova$Region=='Middle East' | df_prova$Region=='South Asia' | df_prova$Region=='North America' | df_prova$Region=='Australia-New Zeland' | df_prova$Region=='East Asia'),]
df_prova_1[,1]=ordered(df_prova_1$Region,levels=c('Middle East','South Asia','North America','Australia-New Zeland','East Asia'))

df_prova_2=df_prova[which( df_prova$Region=='Sub-Saharan Africa' | df_prova$Region=='Latin America' | df_prova$Region=='West Europe' | df_prova$Region=='Central-East Europe'),]
df_prova_2[,1]=ordered(df_prova_2$Region,levels=c('Sub-Saharan Africa','Latin America','West Europe','Central-East Europe'))

cong_al.bsw=table(df_prova$Region,df_prova$Alcohol) 
cong_al.bsw_1=table(df_prova_1$Region,df_prova_1$Alcohol)
cong_al.bsw_2=table(df_prova_2$Region,df_prova_2$Alcohol)

cong_al.bsw_mean=cong_al.bsw/c(sum(df$Region=='Australia and New Zealand'),sum(df$Region=='Central and Eastern Europe'),sum(df$Region=='Eastern Asia'),sum(df$Region=='Latin America and Caribbean'),sum(df$Region=='Middle East and Northern Africa'),sum(df$Region=='North America'),sum(df$Region=='Southeastern Asia'),sum(df$Region=='Sub-Saharan Africa'),sum(df$Region=='Western Europe'))
levels(df$Region)
cong_al.bsw_mean_ord=cong_al.bsw_mean[order(df2_grouped_alch$alch),]
addmargins(cong_al.bsw_mean_ord)

cong_rel_al.bsw=prop.table(cong_al.bsw,1) 


barplot(t(cong_al.bsw_1),horiz = T,las=2,cex.names = .6,xlim=c(0,1200),col=heat.colors(3:6),legend=colnames(cong_al.bsw_1))
barplot(t(cong_al.bsw_2),horiz = T,las=2,cex.names = .6,xlim=c(0,12000),col=heat.colors(3:6),legend=colnames(cong_al.bsw_2))

par(mar = c(5, 9, 4, 2))
barplot(t(cong_al.bsw_mean_ord),horiz = T,las=1,cex.names = .6,xlim=c(0,600),col=c('#e6b839','#60a5e6','#7a47cc'))
legend(450,4,legend=c('Beer','Spirit','Wine'),col=c('#e6b839','#60a5e6','#7a47cc'),cex=0.9,pch=15)
par(mar = c(5, 4, 4, 2))

#barplot per regione rispetto a quantità di alcohol
df2_grouped=df_alcoholic_sub %>%group_by(Region)
df2_grouped_alch=df2_grouped %>% summarise(alch=mean(`alcoholic substances`))
df2_grouped_alch_ord= df2_grouped_alch[order(df2_grouped_alch$alch),] 

barplot(t(df2_grouped_alch),horiz = T,las=2,cex.names = .6,xlim=c(0,12000),col=heat.colors(3:6),legend=colnames(df2_grouped_alch))


#TEST SU VARIANZA BEER SPIRIT WINE
test_1=(var(df$Beer_PerCapita)*(length(df$Beer_PerCapita)-1))/(var(df$Spirit_PerCapita)) #???
2*(1-pchisq(test_1,121))

test_2=(var(df$Beer_PerCapita)*(length(df$Beer_PerCapita)-1))/(var(df$Wine_PerCapita)) #???
2*(1-pchisq(test_2,121))

test_3=(var(df$Wine_PerCapita)*(length(df$Beer_PerCapita)-1))/(var(df$Spirit_PerCapita)) #???
2*(1-pchisq(test_3,121))


#valutare connessione tra variabili alcoholiche 
cong_al.bsw  #cambiare tabella
chi1_1=chisq.test(cong_al.bsw) 
#pchisq(chi$statistic,) #occhio a questo risultato
chi_norm1_1<-chi1_1$statistic/(36693*min(nrow(cong_al.bsw)-1,ncol(cong_al.bsw)-1))
chi_norm1_1

sum(sum(cong_al.bsw[,1]),sum(cong_al.bsw[,2]),sum(cong_al.bsw[,3]))

#matrice di correlazione df
df_cor=df[,4:9]
df_cor_2=df_alcoholic_sub[,4:7]

install.packages('GGally')
install.packages('ggplot2')
library(car)
library(ggplot2)
library(GGally)

ggcorr(df_cor, label = TRUE,label_size=4,legend.size = 9,angle=-45,cex=4.2,hjust=.5)

ggcorr(df_cor_2, label = TRUE,label_size=4,legend.size = 9,angle=-45,cex=3.2,hjust=.5)#scartare


