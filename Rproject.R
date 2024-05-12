#YumnaAlhyari
#900214295
setwd("~/")
library(readxl)
Palestine <- read_excel("Palestine.xlsx",
col_types = c("numeric", "numeric", "numeric",
"numeric", "text", "text", "text",
"text", "text", "text"), na = "\"\"")
View(Palestine)
Palestine[Palestine=='']<-NA
sum(Palestine$`Housing Units`,na.rm=T)
Sum(Palestine$`People Left Homeless`,na.rm=T)
sum(Palestine$`People Left Homeless`,na.rm=T)
sum(Palestine$`Minors Left Homeless`,na.rm=T)
df<-aggregate(as.data.frame(Palestine$`Housing Units`),by=as.data.frame(Palestine$Year),sum)
df1<-aggregate(as.data.frame(Palestine$`People Left Homeless`),by=as.data.frame(Palestine$Year),sum)
df2<-aggregate(as.data.frame(Palestine$`Minors Left Homeless`),by=as.data.frame(Palestine$Year),sum)
df3<-merge(df,df1,by=Palestine$Year)
df3<-cbind(df,df1)
df4<-cbind(df2,df3)
df5<-cbind(df3,df4)
df5
Df_main<-df5[!duplicated(df5$`Palestine$Year`),]
Df_main
main_df <- df5[!duplicated(as.list(df5))]
main_df
table(main_df)
ftable(main_df)
library(ggplot2)
Df_main
main_df
summary(main_df[,2:4])
class(main_df)
class(main_df[,2])
?barplot()
ggplot(main_df, aes(x=main_df[,1],y=main_df[,2])) +
geom_bar(stat='identity')+ labs(title = "Number of Demolitions in Year",x="Year",y="Number of Houses Demolished")+
geom_text(aes(label = main_df[,2]), vjust = -0.2)
ggplot(main_df, aes(x=main_df[,1],y=main_df[,3])) +
+     geom_bar(stat='identity')+ labs(title = "Number of People Left Homeless in Year",x="Year",y="Number of People Left Homeless")+
+     geom_text(aes(label = main_df[,3]), vjust = -0.2)
ggplot(main_df, aes(x=main_df[,1],y=main_df[,4])) + geom_bar(stat='identity')+ labs(title = "Number of Minors Left Homeless in Each Year",x="Year",y="Number of Minors Left Homeless")+geom_text(aes(label = main_df[,4]), vjust = -0.2)
df<-aggregate(as.data.frame(Palestine$`Housing Units`),by=as.data.frame(Palestine$Area),sum)
df
?pie
par(xpd=TRUE)
pie(df[,2],df[,2],main="Demolitions in Each Area")
legend("bottomleft",legend = df[,1],fil=c("white","lightblue","mistyrose"), bty="n")
df
df1<-aggregate(as.data.frame(Palestine$`Housing Units`),by=as.data.frame(Palestine$District),sum)
df1
df1<-aggregate(as.data.frame(Palestine$`Type of Structure`),by=as.data.frame(Palestine$`Housing Units`),sum,na.rm=T)
x1<-as.data.frame(Palestine$`Type of Structure`)
df1<-aggregate(x1,by=as.data.frame(Palestine$`Housing Units`),sum,na.rm=T)
Mode <- function(Palestine, na.rm = FALSE) {
if(na.rm){
x = x[!is.na(x)]
}
valx <- unique(Palestine)
return(valx[which.max(tabulate(match(Palestine, valx)))])
}
Mode(Palestine$`Type of Structure`)
Mode(Palestine$`Demolition carried out by`)
Mode(Palestine$`Demolish Scope`)
Mode(Palestine$`Demolition Reason`)
df1
df1<-aggregate(as.data.frame(Palestine$`Housing Units`),by=as.data.frame(Palestine$District),sum)
df1
plot(main_df[,2], main_df[,3], xlab="Number of Demolitions", ylab="People Left Homeless")
cor(main_df[,2],main_df[,3])
cor(main_df[,2],main_df[,4])
main_df
plot(main_df[,2], main_df[,4], xlab="Number of Demolitions", ylab="Minors Left Homeless")
source("~/.active-rstudio-document")
setwd("~/")
library(readxl)
Palestine <- read_excel("Palestine.xlsx",
col_types = c("numeric", "numeric", "numeric",
"numeric", "text", "text", "text",
"text", "text", "text"), na = "\"\"")
View(Palestine)
range(main_df[,2])
main-df
main_df
mean(main_df[,2],trim=.06)
mean(main_df[,2],trim=.1)
mean(main_df[,2],trim=.2)
mean(main_df[,3],trim=.1)
mean(main_df[,3],trim=.2)
mean(main_df[,4],trim=.1,na.rm=T)
mean(main_df[,4],trim=.2,na.rm=T)
1783-1609
dotchart(df1[,2], labels = df1[,1], main= "Demolitions in Each District",pch = 21, bg = "green", pt.cex = 1.5)
cor(main_df[,2],main_df[,4],use="pairwise.complete.obs")
