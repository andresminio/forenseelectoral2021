#Cargar libraries 
library(tidyverse)
library(viridis)

#cargar referencias 
importacionresultados<-"MESAS_ESCRUTADAS_Cierre.csv"
importacionelectores<-"Electores por Mesa 2021.csv"
exportacion<-"generales21.csv"

#Cargar datos
resultados<-read_csv(importacionresultados)
attach(resultados)

#RESHAPE 
#Transformar datos 
resultados<-resultados%>%
mutate(missingap=is.na(Agrupacion))%>%
mutate(categoria=ifelse(missingap=="FALSE", toupper(Agrupacion), toupper(tipoVoto)))%>%
filter(Cargo=="DIPUTADOS NACIONALES")%>%
filter(!is.na(votos))%>%
select(Distrito, IdCircuito, IdDistrito, IdSeccion, Mesa, categoria, votos)%>%
rename(mesa=Mesa)%>%
rename(distrito=IdDistrito)%>%
rename(circu=IdCircuito)%>%
rename(secc=IdSeccion)%>%
arrange(mesa, -votos)

#Etiquetas y alianzas 
resultados<-resultados%>%
mutate(Alianza=ifelse(categoria=="CAMBIA JUJUY", "JXC",
ifelse(categoria=="CAMBIA MENDOZA", "JXC",
ifelse(categoria=="CAMBIA NEUQUÉN", "JXC",
ifelse(categoria=="CAMBIA SANTA CRUZ", "JXC",
ifelse(categoria=="CHACO CAMBIA + JUNTOS POR EL CAMBIO", "JXC",
ifelse(categoria=="ECO + VAMOS CORRIENTES", "JXC",
ifelse(categoria=="FRENTE JUNTOS POR EL CAMBIO", "JXC",
ifelse(categoria=="JUNTOS", "JXC",
ifelse(categoria=="JUNTOS POR EL CAMBIO", "JXC",
ifelse(categoria=="JUNTOS POR EL CAMBIO +", "JXC",
ifelse(categoria=="JUNTOS POR EL CAMBIO CHUBUT", "JXC",
ifelse(categoria=="JUNTOS POR EL CAMBIO JXC", "JXC",
ifelse(categoria=="JUNTOS POR EL CAMBIO TIERRA DEL FUEGO", "JXC",
ifelse(categoria=="JUNTOS POR ENTRE RÍOS", "JXC",
ifelse(categoria=="JUNTOS POR FORMOSA LIBRE", "JXC",
ifelse(categoria=="UNIDOS POR SAN LUIS", "JXC",
ifelse(categoria=="VAMOS LA RIOJA", "JXC",
ifelse(categoria=="FRENTE DE TODOS", "FDT",
ifelse(categoria=="FRENTE DE TODOS - TODOS SAN JUAN", "FDT",
ifelse(categoria=="FRENTE CIVICO POR SANTIAGO", "FDT",
ifelse(categoria=="FUERZA SAN LUIS", "FDT",
ifelse(categoria=="FRENTE DE IZQUIERDA Y DE TRABAJADORES - UNIDAD", "FIT",
ifelse(categoria=="NUEVA IZQUIERDA", "FIT",
ifelse(categoria=="AVANZA LIBERTAD", "Liberales",
ifelse(categoria=="LA LIBERTAD AVANZA", "Liberales",
ifelse(categoria=="UNION DEL CENTRO DEMOCRATICO", "Liberales",
ifelse(categoria=="UNITE POR LA LIBERTAD Y LA DIGNIDAD", "Liberales",
ifelse(categoria=="COALICIÓN CÍVICA - AFIRMACIÓN PARA UNA REPÚBLICA IGUALITARIA (ARI)", "CCARI",
ifelse(categoria=="VAMOS ! MENDOCINOS","CCARI",
ifelse(categoria=="FRENTE VAMOS CON VOS","VCV",
ifelse(categoria=="VAMOS CON VOS","VCV",
categoria))))))))))))))))))))))))))))))))

resultados<-resultados%>%
mutate(Alianza=ifelse(Alianza=="AUTODETERMINACIÓN Y LIBERTAD","AYL",
ifelse(Alianza=="CHUBUT SOMOS TODOS","CST",
ifelse(Alianza=="COMPROMISO FEDERAL","CF",
ifelse(Alianza=="CONSENSO ISCHIGUALASTO","CI",
ifelse(Alianza=="CONSERVADOR POPULAR","CP",
ifelse(Alianza=="CORRIENTE DE PENSAMIENTO BONAERENSE","CPB",
ifelse(Alianza=="DEL OBRERO","DO",
ifelse(Alianza=="ENCUENTRO VECINAL CÓRDOBA","EVC",
ifelse(Alianza=="FEDERAL","FED",
ifelse(Alianza=="FRENTE AMPLIO CATAMARQUEÑO","FAC",
ifelse(Alianza=="FRENTE AMPLIO POR TUCUMÁN","FAT",
ifelse(Alianza=="FRENTE AMPLIO PROGRESISTA","FAP",
ifelse(Alianza=="FRENTE INTEGRADOR","FI",
ifelse(Alianza=="FRENTE PATRIOTICO LABORISTA","FPL",
ifelse(Alianza=="FRENTE RENOVADOR","FR",
ifelse(Alianza=="FRENTE RENOVADOR DE LA CONCORDIA","FRCS",
ifelse(Alianza=="FRENTE SI + PRS","SIPRS",
ifelse(Alianza=="FUERZA REPUBLICANA","FREP",
ifelse(Alianza=="HACEMOS POR CÓRDOBA","HPC",
ifelse(Alianza=="INDEPENDIENTE DEL CHUBUT","IDC",
ifelse(Alianza=="JUNTOS SOMOS RIO NEGRO","JSRN",
ifelse(Alianza=="LIBERTAD, VALORES Y CAMBIO","LVC",
ifelse(Alianza=="MOVIMIENTO AL SOCIALISMO","MAS",
ifelse(Alianza=="MOVIMIENTO LIBRES DEL SUR","LIBSUR",
ifelse(Alianza=="MOVIMIENTO POPULAR FUEGUINO","MPF",
ifelse(Alianza=="MOVIMIENTO POPULAR NEUQUINO","MPN",
ifelse(Alianza=="POLITICA OBRERA","POLOB",
ifelse(Alianza=="POR UN SANTIAGO OBRERO","PUSO",
ifelse(Alianza=="PRIMERO SANTA FE","PSF",
ifelse(Alianza=="PRINCIPIOS Y CONVICCIÓN","PYC",
ifelse(Alianza=="REPUBLICANOS UNIDOS","RU",
ifelse(Alianza=="SOBERANIA POPULAR","SP",
ifelse(Alianza=="SOCIALISTA","SOC",
ifelse(Alianza=="SOMOS ENERGÍA PARA RENOVAR SANTA CRUZ -SER-","SER",
ifelse(Alianza=="SOMOS FUEGUINOS","SFUE",
ifelse(Alianza=="SOMOS FUTURO","SFUT",
ifelse(Alianza=="TODOS UNIDOS","TU",
ifelse(Alianza=="UNIDOS POR SALTA","UPS",
ifelse(Alianza=="UNIÓN CÍVICA RADICAL","UCR",
ifelse(Alianza=="UNIÓN POPULAR FEDERAL","UPF",
ifelse(Alianza=="+ VALORES","VALORES",
Alianza))))))))))))))))))))))))))))))))))))))))))

#Construir ID
resultados<-resultados%>%
filter(!is.na(distrito),!is.na(secc),!is.na(circu), !is.na(mesa))%>%
mutate(ncdistrito=nchar(distrito))%>%
mutate(ncseccion=nchar(secc))%>%
mutate(nccircu=nchar(circu))%>%
mutate(ncmesa=nchar(mesa))

resultados<-resultados%>%
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
mutate(p4=ifelse(ncmesa==1, paste("000000", mesa2, sep=""),
ifelse(ncmesa==2, paste("00000", mesa, sep=""), 
ifelse(ncmesa==3, paste("0000", mesa, sep=""), 
ifelse(ncmesa==4, paste("000", mesa, sep=""), 
ifelse(ncmesa==5, paste("00", mesa, sep=""), 
ifelse(ncmesa==6, paste("0", mesa, sep=""), 
ifelse(ncmesa==7, mesa, "ERRORmesa")))))))) %>%
mutate(id=paste(p1,p2,p3,p4, sep=""))

#Pivotar dataset 
(resultados<-resultados%>%
pivot_wider(id_cols=id, names_from=Alianza, values_from=votos))

#Merge resultados y electores
electoresmesa<-read_csv(importacionelectores)

output<-merge(electoresmesa, resultados, all=T)
output<-as.tibble(output)
output<-select(output, id, distrito, secc, circu, mesa, electores, 
BLANCOS, NULOS, RECURRIDOS, IMPUGNADOS, COMANDO,
JXC, FDT, FIT, Liberales, HPC, FAP, MPN, FRCS, JSRN,
everything())

output<-mutate(output, Nocargada=ifelse(is.na(BLANCOS),1,0))

write.csv(output, exportacion, row.names=F, na = "")




 