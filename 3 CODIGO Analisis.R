#Cargar libraries 
library(tidyverse)
library(ggpubr)
library(ggridges)
library(sf)
library(viridis)
library(wesanderson)

#carga de datos
dat<-read_csv("generales21.csv")
provincias<-st_read("provincia.json")

#Crear etiquetas de distrito
dat<-mutate(dat, idlabel=case_when(distrito==1~"CABA", 
distrito==2~"PBA",  
distrito==3~"Catamarca", 
distrito==4~"Córdoba", 
distrito==5~"Corrientes", 
distrito==6~"Chaco", 
distrito==7~"Chubut", 
distrito==8~"Entre Ríos", 
distrito==9~"Formosa", 
distrito==10~"Jujuy", 
distrito==11~"La Pampa", 
distrito==12~"La Rioja", 
distrito==13~"Mendoza", 
distrito==14~"Misiones", 
distrito==15~"Neuquén", 
distrito==16~"Río Negro", 
distrito==17~"Salta", 
distrito==18~"San Juan", 
distrito==19~"San Luis", 
distrito==20~"Santa Cruz", 
distrito==21~"Santa Fe", 
distrito==22~"S del Estero", 
distrito==23~"Tucumán", 
distrito==24~"T del Fuego"))

#calculo de votos afirmativos, validos, totales y participacion
dat<-mutate(dat, afirmativos=ifelse(Nocargada==0, 
rowSums(dat[,c(12:64)], na.rm = TRUE), NA))

dat<-dat%>%group_by(id)%>%
mutate(validos=sum(afirmativos, BLANCOS, na.rm = TRUE))

dat<-dat%>%group_by(id)%>%
mutate(totales=sum(validos, NULOS, RECURRIDOS, IMPUGNADOS, COMANDO, na.rm = TRUE))

dat<-dat%>%group_by(id)%>%
mutate(participacion=ifelse(totales!=is.na(totales), 
round((totales/electores)*100,2), NA))
dat<-as.tibble(dat)

#Seleccionar agrupaciones más relevantes 
dat2<-dat%>%
filter(Nocargada==0)%>%
filter(totales!=0)%>%
filter(afirmativos!=0)%>%
group_by(id)%>%
mutate(relevantes=sum(JXC, FDT, FIT, Liberales, HPC, FAP, MPN, FRCS, JSRN, na.rm=T))%>%
mutate(prelevantes=(relevantes/afirmativos)*100) 
 
relevantes<-colSums(dat[,c(12:64)], na.rm = TRUE)

#escribe una tabla con la cantidad de mesas en las que participa cada AP
write.csv(relevantes, "relevantes.csv")

#data set que incluye solo las variables necesarias
dat2<-dat2%>%
select(id,idlabel, distrito, secc, circu, mesa,electores, 
participacion, totales, validos, afirmativos,
JXC, FDT, FIT, Liberales, HPC, FAP, MPN, FRCS, JSRN,
BLANCOS, NULOS, RECURRIDOS, IMPUGNADOS, COMANDO)
(dat2<-as.tibble(dat2))

#calcular porcentaje electoral de cada AP relevante 
dat2<-dat2%>%group_by(id)%>%
mutate(pJXC=ifelse(JXC!=is.na(JXC), 
round((JXC/validos)*100,2), NA))

dat2<-dat2%>%group_by(id)%>%
mutate(pFDT=ifelse(FDT!=is.na(FDT), 
round((FDT/validos)*100,2), NA))

dat2<-dat2%>%group_by(id)%>%
mutate(pFIT=ifelse(FIT!=is.na(FIT), 
round((FIT/validos)*100,2), NA))

dat2<-dat2%>%group_by(id)%>%
mutate(pLib=ifelse(Liberales!=is.na(Liberales), 
round((Liberales/validos)*100,2), NA))

dat2<-dat2%>%group_by(id)%>%
mutate(pHPC=ifelse(HPC!=is.na(HPC), 
round((HPC/validos)*100,2), NA))

dat2<-dat2%>%group_by(id)%>%
mutate(pFAP=ifelse(FAP!=is.na(FAP), 
round((FAP/validos)*100,2), NA))

dat2<-dat2%>%group_by(id)%>%
mutate(pMPN=ifelse(MPN!=is.na(MPN), 
round((MPN/validos)*100,2), NA))

dat2<-dat2%>%group_by(id)%>%
mutate(pFRCS=ifelse(FRCS!=is.na(FRCS), 
round((FRCS/validos)*100,2), NA))

dat2<-dat2%>%group_by(id)%>%
mutate(pJSRN=ifelse(JSRN!=is.na(JSRN), 
round((JSRN/validos)*100,2), NA))

##ANALISIS
#Mesas no cargadas o incompletas
dat<-mutate(dat, incompletas=ifelse(totales==0, 1, 
ifelse(validos==0, 1, 
ifelse(afirmativos==0, 1,
Nocargada))))

coberturaplot<-ggplot(dat)+
geom_bar(aes(x=incompletas, y=..prop.., fill=as.factor(..x..)), alpha=0.6)+
theme_minimal()+
scale_y_continuous(labels=scales::percent)+
scale_x_discrete(breaks=c(0,1), labels = c("Cargadas", "No cargadas"))+
scale_fill_manual(values = wes_palette("Zissou1", 2, type = "continuous"))+
ggtitle("Mesas cargadas en el escrutinio provisorio")+
theme(plot.title=element_text(size=18, hjust=0.5, face="bold"),
text=element_text(size=16),
legend.position="none")+
labs(x="", y="", caption="@jandresmi")+
geom_text(aes(x=1, y=0.033, label="1329 (1.3%)"))+
geom_text(aes(x=0, y=1.007, label="100129 (98.7%)"))

ggsave("cobertura.tiff", coberturaplot, dpi=500)

coberturadistrito<-dat%>%
group_by(idlabel)%>%
mutate(pincompletas=sum(incompletas)/length(incompletas))%>%
summarise(cobpd=1-mean(pincompletas))%>%
ggplot(aes(x=reorder(idlabel, cobpd), y=cobpd, fill=as.factor(-cobpd)))+
geom_col()+
coord_flip()+
theme_minimal()+
scale_y_continuous(labels=scales::percent)+
scale_fill_manual(values = wes_palette("Zissou1", 55, type = "continuous"))+
theme(legend.position= "none")+
ggtitle("Mesas cargadas en el escrutinio provisorio por distrito")+
theme(plot.title=element_text(size=14, hjust=0.5, face="bold"),
text=element_text(size=16))+
labs(x="", y="", caption="@jandresmi")+
geom_hline(yintercept=0.95, linetype="dashed")+
geom_hline(yintercept=0.99, linetype="dashed")+
geom_text(aes(x=1, y=0.99, label="95%"))+
geom_text(aes(x=15, y=1.03, label="99%"))

ggsave("coberturadistrito.tiff", coberturadistrito, dpi=800)


#Participacion 

part<-dat2%>%
filter(participacion>100)%>%
select(id, idlabel, electores, participacion, 
totales, afirmativos, validos,JXC,FDT, Liberales, FIT)

participaciondist<-ggplot(dat2, aes(x=participacion,
y=reorder(idlabel, participacion), fill=factor(-..y..)))+
geom_density_ridges_gradient()+
theme_minimal()+
scale_fill_manual(values = wes_palette("Zissou1", 24, type = "continuous"))+
geom_vline(xintercept=71.88, linetype="dashed")+
geom_text(aes(x=72, y=24, label="72%", size=16), nudge_y=0.7, nudge_x=-4.5)+
labs(x="Participación", y="")+
ggtitle("Distribución de la participación por distrito")+
theme(plot.title=element_text(size=14, hjust=0.5, face="bold"), 
legend.position="none",
text=element_text(size=16))

ggsave("partdistrito.tiff", participaciondist, dpi=800)

#Participación y voto por AP
#JXC
partjxc<-dat2%>%
filter(participacion<100)%>%
ggplot(aes(participacion, pJXC))+
geom_hex(bins=90)+
theme_minimal()+
scale_fill_continuous(type = "viridis")+
ylab("Juntos por el Cambio")+xlab("Participación")+
labs(fill = "Mesas", caption="@jandresmi")+
ggtitle("Participación y porcentaje electoral (JXC)")+
theme(plot.title=element_text(hjust = 0.5, size=18, face="bold"),
legend.position="bottom")

ggsave("pyvjxc.tiff", partjxc, dpi=800)

#FDT
partfdt<-dat2%>%
filter(participacion<100)%>%
ggplot(aes(participacion, pFDT))+
geom_hex(bins=90)+
theme_minimal()+
scale_fill_continuous(type = "viridis")+
ylab("Frente de Todos")+xlab("Participación")+
labs(fill = "Mesas", caption="@jandresmi")+
ggtitle("Participación y porcentaje electoral (FDT)")+
theme(plot.title=element_text(hjust = 0.5, size=18, face="bold"),
legend.position="bottom")

ggsave("pyvfdt.tiff", partfdt, dpi=800)

histogramafdt<-dat2%>%
filter(participacion<100)%>%
ggplot()+
geom_histogram(bins=99.51, aes(pFDT, fill=-log(abs((..x..)-31.64))))+
theme_minimal()+
scale_fill_viridis()+
labs(x="%FDT", y="Mesas", fill="", caption="@jandresmi")+
ggtitle("Distribución del voto del Frente de Todos")+
theme(plot.title=element_text(hjust = 0.5, size=18, face="bold"),
legend.position="none")+
geom_vline(xintercept=75, linetype="dashed")

fdt75hist<-dat2%>%
filter(participacion<100)%>%
filter(pFDT>75)%>%
group_by(idlabel)%>%
summarize(cmesas=n())%>%
ggplot()+
geom_col(aes(reorder(idlabel, cmesas), cmesas, fill=-log10(cmesas)))+
theme_minimal()+
labs(x="", y="Mesas", fill="")+
theme(plot.title=element_text(hjust = 0.5, size=18, face="bold"),
legend.position="none",
text=element_text(size=14),
axis.text.x=element_text(angle=90))

#crea un id2 para hacer un merge con los poligonos de provincias.json para mapa
dat2<-mutate(dat2, id2=case_when(distrito==1~1, 
distrito==2~24,  
distrito==3~6, 
distrito==4~13, 
distrito==5~19, 
distrito==6~8, 
distrito==7~11, 
distrito==8~14, 
distrito==9~9, 
distrito==10~16, 
distrito==11~22, 
distrito==12~5, 
distrito==13~12, 
distrito==14~20, 
distrito==15~2, 
distrito==16~18, 
distrito==17~21, 
distrito==18~15, 
distrito==19~3, 
distrito==20~10, 
distrito==21~4, 
distrito==22~17, 
distrito==23~7, 
distrito==24~23))

fdt75b<-dat2%>%
filter(participacion<100)%>%
filter(pFDT>75)%>%
mutate(unos=1)%>%
group_by(id2)%>%
summarize(cmesas=sum(unos))

mergefdt<-merge(x=provincias, y=fdt75b, by.x="gid", by.y="id2", all.x=T)
mergefdt[is.na(mergefdt)]<-0

fdt75mapa<-ggplot(mergefdt)+
geom_sf(aes(fill=-log10(cmesas)), color = NA)+
theme_minimal()+
xlim(78,50)+ylim(60,20)+
theme(plot.title=element_text(hjust = 0.5, size=18, face="bold"),
legend.position="none", axis.text= element_blank())+
scale_fill_continuous(na.value="#C6E2FF")

fdt75<-ggarrange(fdt75hist,fdt75mapa, legend="none")
fdt75<-annotate_figure(fdt75, 
top=text_grob("Cantidad de Mesas con más de 75pp para el FDT", 
face = "bold", size = 18), 
bottom=text_grob("@jandresmi", size=12, hjust=-2.5))

ggsave("fdt75.tiff", fdt75, dpi=800, width = 9, height = 4.5)

#Todos los partidos relevantes
partjxc2<-dat2%>%
filter(participacion<100)%>%
ggplot(aes(participacion, pJXC))+
geom_hex(bins=90)+
theme_minimal()+
scale_fill_continuous(type = "viridis")+
ylab("JXC")+xlab("")+ labs(fill = "Mesas")

partfdt2<-dat2%>%
filter(participacion<100)%>%
ggplot(aes(participacion, pFDT))+
geom_hex(bins=90)+
theme_minimal()+
scale_fill_continuous(type = "viridis")+
ylab("FDT")+xlab("")

partlib<-dat2%>%
filter(participacion<100)%>%
ggplot(aes(participacion, pLib))+
geom_hex(bins=90)+
theme_minimal()+
scale_fill_continuous(type = "viridis")+
ylab("Libertarios")+xlab("")

partfit<-dat2%>%
filter(participacion<100)%>%
ggplot(aes(participacion, pFIT))+
geom_hex(bins=90)+
theme_minimal()+
scale_fill_continuous(type = "viridis")+
ylab("FIT")+xlab("")

parthpc<-dat2%>%
filter(participacion<100)%>%
ggplot(aes(participacion, pHPC))+
geom_hex(bins=90)+
theme_minimal()+
scale_fill_continuous(type = "viridis")+
ylab("Hacemos por Cba (CBA).")+xlab("")

partfap<-dat2%>%
filter(participacion<100)%>%
ggplot(aes(participacion, pFAP))+
geom_hex(bins=90)+
theme_minimal()+
scale_fill_continuous(type = "viridis")+
ylab("F. Amplio Progresista (SF)")+xlab("")

partmpn<-dat2%>%
filter(participacion<100)%>%
ggplot(aes(participacion, pMPN))+
geom_hex(bins=90)+
theme_minimal()+
scale_fill_continuous(type = "viridis")+
ylab("Mov. Popular Neuquino (NQN)")+xlab("")

partfrcs<-dat2%>%
filter(participacion<100)%>%
ggplot(aes(participacion, pFRCS))+
geom_hex(bins=90)+
theme_minimal()+
scale_fill_continuous(type = "viridis")+
ylab("F. Ren. de la Concordia (MIS)")+xlab("")

partjsrn<-dat2%>%
filter(participacion<100)%>%
ggplot(aes(participacion, pJSRN))+
geom_hex(bins=90)+
theme_minimal()+
scale_fill_continuous(type = "viridis")+
ylab("Juntos Somos RN (RN)")+xlab("")
 
todos<-ggarrange(partjxc2, partfdt2, partfit, partlib, parthpc, partfap, 
partmpn, partfrcs, partjsrn, 
common.legend = TRUE, legend="bottom"))
annotate_figure(todos, 
top=text_grob("Participación y voto por agrupaciones relevantes", 
face = "bold", size = 18), 
bottom=text_grob("@jandresmi", size=12, hjust=-2.5)

ggsave("pyvtodos.tiff", todos, dpi=800)

#Diferencia voto circuito 
#idcircuito
dat2$distrito<-as.character(dat2$distrito)
dat2$secc<-as.character(dat2$secc)
dat2$circu<-as.character(dat2$circu)

dat2<-dat2%>%
mutate(ncdistrito=nchar(distrito))%>%
mutate(ncseccion=nchar(secc))%>%
mutate(nccircu=nchar(circu))%>%
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
mutate(idcircu=paste(p1,p2,p3, sep=""))

#calcular voto y % ap por circuito
dat2<-dat2%>%
group_by(idcircu)%>%
mutate(validoscircu=sum(validos))%>%
mutate(jxccircu=sum(JXC,na.rm=T))%>%
mutate(fdtcircu=sum(FDT, na.rm=T))%>%
mutate(libcircu=sum(Liberales, na.rm=T))%>%
mutate(fitcircu=sum(FIT, na.rm=T))%>%
mutate(pjxccircu=round((jxccircu/validoscircu)*100,2))%>%
mutate(pfdtcircu=round((fdtcircu/validoscircu)*100,2))%>%
mutate(plibcircu=round((libcircu/validoscircu)*100,2))%>%
mutate(pfitcircu=round((fitcircu/validoscircu)*100,2))

#calcular diferencias Z y pp entre voto en mesa y circuito
dat2<-dat2%>%
mutate(difjxc=pJXC-pjxccircu)%>%
mutate(diffdt=pFDT-pfdtcircu)%>%
mutate(difliberales=pLib-plibcircu)%>%
mutate(diffit=pFIT-pfitcircu)%>%
mutate(zdifjxc=difjxc/sd(pJXC))%>%
mutate(zdiffdt=diffdt/sd(pFDT))%>%
mutate(zdiflib=difliberales/sd(pLib))%>%
mutate(zdiffit=diffit/sd(pFIT))

#graficando las diferencias
(circuitosjxc<-dat2%>%
filter(electores>100)%>%
ggplot(aes(zdifjxc))+
theme_minimal()+
geom_histogram(aes(y=..density.., bins=45),
fill="#eaca25", color="#d3dddc")+
geom_density(color="#ee3900")+facet_wrap(~idlabel, ncol=4)+
labs(x="Juntos por el Cambio", y="Densidad", caption="@jandresmi")+
ggtitle("Diferencia entre el voto de la agrupación política en la mesa y el promedio del 
circuito electoral (estandarizado)(JXC)")+
theme(plot.title=element_text(hjust = 0.5, face="bold")))
 
ggsave("difjxc.tiff", circuitosjxc, dpi=800)

circuitosfdt<-dat2%>%
filter(electores>100)%>%
ggplot(aes(zdiffdt))+
theme_minimal()+
geom_histogram(aes(y=..density.., bins=45),
fill="#046c9a", color="#d3dddc", alpha=0.8)+
geom_density(color="#ee3900")+facet_wrap(~idlabel, ncol=4)+
labs(x="Frente de Todos", y="Densidad", caption="@jandresmi")+
ggtitle("Diferencia entre el voto de la agrupación política en la mesa y el promedio del 
circuito electoral (estandarizado)(FDT)")+
theme(plot.title=element_text(hjust = 0.5, face="bold"))

ggsave("diffdt.tiff", circuitosfdt, dpi=800)

circuitoslib<-dat2%>%
filter(electores>100)%>%
ggplot(aes(zdiflib))+
theme_minimal()+
geom_histogram(aes(y=..density.., bins=45),
fill="#8968CD", color="#d3dddc", alpha=0.8)+
geom_density(color="#ee3900")+facet_wrap(~idlabel, ncol=4)+
labs(x="Liberales", y="Densidad", caption="@jandresmi")+
ggtitle("Diferencia entre el voto de la agrupación política en la mesa y el promedio del 
circuito electoral (estandarizado)(Lib.)")+
theme(plot.title=element_text(hjust = 0.5, face="bold"))

ggsave("diflib.tiff", circuitoslib, dpi=800)

circuitosfit<-dat2%>%
filter(electores>100)%>%
ggplot(aes(zdiffit), na.omit(zdiffit))+
theme_minimal()+
geom_histogram(aes(y=..density.., bins=45),
fill="#EE4000", color="#d3dddc", alpha=0.6)+
geom_density(color="#ee3900")+facet_wrap(~idlabel, ncol=4)+
labs(x="FIT-UNIDAD", y="Densidad", caption="@jandresmi")+
ggtitle("Diferencia entre el voto de la agrupación política en la mesa y el promedio del 
circuito electoral (estandarizado)(FIT)")+
theme(plot.title=element_text(hjust = 0.5, face="bold"))

ggsave("diffit.tiff", circuitosfit, dpi=800)

#identificación de mesas atipicas
dat2<-dat2%>%
group_by(idcircu)%>%
mutate(mesascircuito=n())

atipicas<-dat2%>%
filter(electores>99)%>%
mutate(atipicajxc=ifelse(abs(zdifjxc)>3&abs(difjxc)>5, 1, 0))%>%
mutate(atipicafdt=ifelse(abs(zdiffdt)>3&abs(diffdt)>5, 1, 0))%>%
mutate(atipicalib=ifelse(abs(zdiflib)>3&abs(difliberales)>5, 1, 0))%>%
mutate(atipicafit=ifelse(abs(zdiffit)>3&abs(diffit)>5, 1, 0))%>%
mutate(atipicas=ifelse(atipicajxc==1, 1,
ifelse(atipicafdt==1, 1, 
ifelse(atipicalib==1, 1,
ifelse(atipicafit==1, 1, 0)))))%>%
filter(atipicas==1)%>%
select(id,idcircu, idlabel,electores,participacion,mesascircuito, 
pJXC,	pFDT, pFIT, pLib, 
pjxccircu, pfdtcircu, plibcircu, pfitcircu,
zdifjxc, zdiffdt, zdiflib, zdiffit, atipicas,
atipicajxc,atipicafdt,atipicalib,atipicafit)

write.csv(atipicas, "atipicas.csv", row.names=F, na="")

