library("tidyverse")
library(readxl) 

#cargar referencias 
importacion<-"PadronNRO.txt"
exportacion<-"NROPROV.csv"

#cargar data y nombrar variables
padron<-read_delim(importacion, delim="|", col_names=FALSE) 
names(dat)<-c("distrito",
"matricula",
"clase",
"apellido",
"nombres",
"profesion",
"domicilio",
"tipdoc",
"secc",
"circu",
"mesa",
"sexo",
"padr_numreg",
"partido",
"padron",
"orden",
"id")

#calcular la cantidad de caracteres
padron<-padron%>%
select("distrito","secc","circu","mesa")%>%
filter(!is.na(distrito),!is.na(secc),!is.na(circu), !is.na(mesa))%>%
mutate(ncdistrito=nchar(distrito))%>%
mutate(ncseccion=nchar(secc))%>%
mutate(nccircu=nchar(circu))%>%
mutate(ncmesa=nchar(mesa))

#Construir ID
padron<-padron%>%mutate(mesa2=paste(mesa, "X", sep=""))%>%
mutate(p1=ifelse(ncdistrito==1, paste("0", distrito, sep=""),
ifelse(ncdistrito==2, distrito, "ERRORdistrito")))%>%
mutate(p2=ifelse(ncseccion==1, paste("00", secc, sep=""), 
ifelse(ncseccion==2, paste("0", secc, sep=""),
ifelse(ncseccion==3, secc, "ERRORsecc"))))%>%
mutate(p3=ifelse(nccircu==1, paste("0000", circu, sep=""), 
ifelse(nccircu==2, paste("000", circu, sep=""),
ifelse(nccircu==3, paste("00", circu, sep=""),
ifelse(nccircu==4, paste("0", circu, sep=""),
ifelse(nccircu==5, circu, "ERRORcircu"))))))%>%
mutate(p4=ifelse(ncmesa==1, paste("00000", mesa2, sep=""),
ifelse(ncmesa==2, paste("0000", mesa2, sep=""), 
ifelse(ncmesa==3, paste("000", mesa2, sep=""), 
ifelse(ncmesa==4, paste("00", mesa2, sep=""), 
ifelse(ncmesa==5, paste("0", mesa2, sep=""), 
ifelse(ncmesa==6, mesa2, "ERRORmesa"))))))) %>%
mutate(id=paste(p1,p2,p3,p4, sep=""))%>%
select(id, distrito, secc, circu, mesa)

#calcular electores por mesa, remover missing value y eliminar duplicados
padron<-padron%>%
group_by(id)%>%
mutate(electores=length(id))%>%
unique()

write_csv(dat,exportacion)
#repetir lo anterior para cada uno de los archivos, con excepción de PBA

#Para PBA, tenemos la tabla de establecimientos y no los padrones, que hay que homogeneizar
dat<-read_excel("Locales PBA Detalle por mesa.xlsx")

padron<-padron%>%
mutate(distrito=2)%>%
mutate(mesa=NUMERO_MESA)%>%
separate(SECCION, into=c("secc", "labelsecc"), sep="-")%>%
separate(CIRCUITO, into=c("circu", "labelcircu"), sep="-")%>%
mutate(mesa=str_replace_all(mesa, " ", ""))%>%
mutate(secc=str_replace_all(secc, " ", ""))%>%
mutate(circu=str_replace_all(circu, " ", ""))%>%
rename(electores=TOT_ELECTORES)

#calcular la cantidad de caracteres
padron<-padron%>%
select(distrito,secc,circu,mesa, electores)%>%
filter(!is.na(distrito),!is.na(secc),!is.na(circu), !is.na(mesa))%>%
mutate(ncdistrito=nchar(distrito))%>%
mutate(ncseccion=nchar(secc))%>%
mutate(nccircu=nchar(circu))%>%
mutate(ncmesa=nchar(mesa))

#Construir ID
padron<-padron%>%mutate(mesa2=paste(mesa, "X", sep=""))%>%
mutate(p1=ifelse(ncdistrito==1, paste("0", distrito, sep=""),
ifelse(ncdistrito==2, distrito, "ERRORdistrito")))%>%
mutate(p2=ifelse(ncseccion==1, paste("00", secc, sep=""), 
ifelse(ncseccion==2, paste("0", secc, sep=""),
ifelse(ncseccion==3, secc, "ERRORsecc"))))%>%
mutate(p3=ifelse(nccircu==1, paste("0000", circu, sep=""), 
ifelse(nccircu==2, paste("000", circu, sep=""),
ifelse(nccircu==3, paste("00", circu, sep=""),
ifelse(nccircu==4, paste("0", circu, sep=""),
ifelse(nccircu==5, circu, "ERRORcircu"))))))%>%
mutate(p4=ifelse(ncmesa==1, paste("00000", mesa2, sep=""),
ifelse(ncmesa==2, paste("0000", mesa2, sep=""), 
ifelse(ncmesa==3, paste("000", mesa2, sep=""), 
ifelse(ncmesa==4, paste("00", mesa2, sep=""), 
ifelse(ncmesa==5, paste("0", mesa2, sep=""), 
ifelse(ncmesa==6, mesa2, "ERRORmesa"))))))) %>%
mutate(id=paste(p1,p2,p3,p4, sep=""))%>%
select(id, distrito, secc, circu, mesa, electores)

write_csv(dat,"02pba.csv")

#unir data frames 
p01<-read_csv("01caba.csv")
p02<-read_csv("02pba.csv")
p03<-read_csv("03cat.csv")
p04<-read_csv("04cba.csv")
p05<-read_csv("05corr.csv")
p06<-read_csv("06cha.csv")
p07<-read_csv("07chu.csv")
p08<-read_csv("08er.csv")
p09<-read_csv("09for.csv")
p10<-read_csv("10juj.csv")
p11<-read_csv("11lp.csv")
p12<-read_csv("12lr.csv")
p13<-read_csv("13mza.csv")
p14<-read_csv("14mis.csv")
p15<-read_csv("15nqn.csv")
p16<-read_csv("16rn.csv")
p17<-read_csv("17sal.csv")
p18<-read_csv("18sj.csv")
p19<-read_csv("19sl.csv")
p20<-read_csv("20sc.csv")
p21<-read_csv("21sf.csv")
p22<-read_csv("22sde.csv")
p23<-read_csv("23tuc.csv")
p24<-read_csv("24tdf.csv")

electoresmesa<-merge(p01,p02, all=T)
electoresmesa<-merge(electoresmesa, p03, all=T)
electoresmesa<-merge(electoresmesa, p04, all=T)
electoresmesa<-merge(electoresmesa, p05, all=T)
electoresmesa<-merge(electoresmesa, p06, all=T)
electoresmesa<-merge(electoresmesa, p07, all=T)
electoresmesa<-merge(electoresmesa, p08, all=T)
electoresmesa<-merge(electoresmesa, p09, all=T)
electoresmesa<-merge(electoresmesa, p10, all=T)
electoresmesa<-merge(electoresmesa, p11, all=T)
electoresmesa<-merge(electoresmesa, p12, all=T)
electoresmesa<-merge(electoresmesa, p13, all=T)
electoresmesa<-merge(electoresmesa, p14, all=T)
electoresmesa<-merge(electoresmesa, p15, all=T)
electoresmesa<-merge(electoresmesa, p16, all=T)
electoresmesa<-merge(electoresmesa, p17, all=T)
electoresmesa<-merge(electoresmesa, p18, all=T)
electoresmesa<-merge(electoresmesa, p19, all=T)
electoresmesa<-merge(electoresmesa, p20, all=T)
electoresmesa<-merge(electoresmesa, p21, all=T)
electoresmesa<-merge(electoresmesa, p22, all=T)
electoresmesa<-merge(electoresmesa, p23, all=T)
electoresmesa<-merge(electoresmesa, p24, all=T)

write.csv(electoresmesa, "Electores por Mesa 2021.csv", row.names=F )






