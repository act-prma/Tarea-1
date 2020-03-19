#Cargamos las librerias necesarias

#require(quantmod)
#install.packages("quantmod")
library(quantmod)
#require(data.table)
#install.packages("data.table")
library(data.table)
#require("PerformanceAnalytics")
#install.packages("PerformanceAnalytics")
library("PerformanceAnalytics")

#install.packages("Deriv")
library(Deriv)
#------------
#install.packages("dplyr")
library(dplyr)
con = gzcon(url('https://github.com/systematicinvestor/SIT/raw/master/sit.gz', 'rb'))
source(con)
close(con)
#---------------

#utilizamos la funcion de interpolación dada por el profesor

#FUNCIÓN DE INTERPOLACIÓN ALAMBRADA

talamb=function(nodos,curva,plazos) #función de interpolación de tasas por el método alamabrada
{
  n=max(ncol(plazos),1)
  m=max(ncol(nodos),1)
  TC=matrix(0,1,n)
  TL=matrix(0,1,n)
  TF=matrix(0,1,n)
  for (j in 1:n)
  {
    i=1
    repeat
    {
      if(nodos[i]<= plazos[j] && plazos[j] <=nodos[i+1])
      {
        TC[j]=curva[i]
        TL[j]=curva[i+1]
        TF[j]=((((1+TL[j]*nodos[i+1]/360)/(1+TC[j]*nodos[i]/360))^((plazos[j]-nodos[i])/(nodos[i+1]-nodos[i]))*(1+TC[j]*nodos[i]/360))-1)*360/plazos[j]
        break
      }
      else if (plazos[j]<nodos[1])
      {
        TC[j]=curva[1]
        TL[j]=curva[1]
        TF[j]=curva[1]
        break
      }
      else if (plazos[j]>nodos[m])
      {
        TC[j]=curva[m]
        TL[j]=curva[m]
        TF[j]=curva[m]
        break
      }
      else
      {i=i+1}
    }
  }
  as.matrix(t(as.numeric(rbind(TF))))
}

#otras funciones que utilizaremos

wquantile <- function(v,w=rep(1,length(v)),p=.5) 
{ 
  if ( !is.numeric(w) || length(v) != length(w) ) 
    stop("Los valores y los pesos tienen que tener misma longitud") 
  if ( !is.numeric(p) || any( p<0 | p>1) ) 
    stop("Percentil tiene que ser 0<=p<=1") 
  if ( min(w) < 0 ) stop("Los pesos tiene que ser mayores que 0") 
  ranking <- order(v) 
  sumw <- cumsum(w[ranking]) 
  plist <- sumw / sumw[ length(sumw) ] 
  v [ ranking [ which.max( plist >= p ) ] ]  
}


#CVaR con alisado
wcvar <- function(v,w=rep(1,length(v)),p=.5) 
{ 
  if ( !is.numeric(w) || length(v) != length(w) ) 
    stop("Los valores y los pesos tienen que tener misma longitud") 
  if ( !is.numeric(p) || any( p<0 | p>1) ) 
    stop("Percentil tiene que ser 0<=p<=1") 
  if ( min(w) < 0 ) stop("Los pesos tiene que ser mayores que 0") 
  ranking <- order(v) 
  sumw <- cumsum(w[ranking]) 
  plist <- sumw / sumw[ length(sumw) ] 
  loss= v [ ranking [ which( plist < p ) ] ]  
  esc=w [ ranking [ which( plist < p ) ] ]  
  sum(loss*esc)/(sum(esc))
}


#Parametros para la valoracion 
fval=as.Date("20200228",format="%Y%m%d") #Fecha de valoracion
itpl=0 #Usaremos la interpolacion lineal por eso ponemos el cero
alpha=0.98 #Nivel de confianza para obtener estimaciones de riesgo
nh=3660 #número de días de historia
yext=1 #si se usa la historia de internet o fija
#setwd(direc)	

#FORWARDS cargamos informacion para Tipo de Cambion


##---cambiar directorio **********ojo aqui
bext=read.table("C:/Users/tonaj/Desktop/2020-2/ADMI RIESGO FINANCIERO/tarea1/tasa_libor.txt")
bdom=read.table("C:/Users/tonaj/Desktop/2020-2/ADMI RIESGO FINANCIERO/tarea1/tasa_fwd.txt")
btsp=read.table("C:/Users/tonaj/Desktop/2020-2/ADMI RIESGO FINANCIERO/tarea1/tasa_spot.txt")
##---cambiar directorio **********ojo aqui


SymbolsFX_ftdc<-c("USDMXN=X", "GBPUSD=X" ) #tienen que ir en orden alfabético
plazos_fwd=cbind( 5)
contratos_fwd=cbind(100)
kst_fwd=cbind(19.83)
nominal_fwd=1 
yext=1 #si se carga información de yahoo en la fecha definida por fval o SymbolsFX, en caso contrario se utiliza información que se tendrá que cargar de tasas_spot.txt
trlib=1 #1 si la curva libor viene a 182 0 si no

#FORWARDS DE IPC
#Descontamos con gubernamental
base=read.table("C:/Users/tonaj/Desktop/2020-2/ADMI RIESGO FINANCIERO/tarea1/tasa_guber.txt")
SymbolsEQ_find<-c("^MXX", "GCARSOA1.MX" ) #tienen que ir en orden alfabético
plazos_fwd_ind=cbind( 53)#plazo
contratos_fwd_ind=cbind(50)#numero de
kst_fwd_ind=cbind(-49525)
nominal_fwd_ind=1






#-----
#2
#CARGA DE DATOS DE  FORWARDS DE TDC
#datas
#data<-read.table("tasa_tiie.txt")
data1<-bext
data2<-bdom

########minimos para parametrizar
n1=nrow(data1)
n2=nrow(data2)
m1_ftdc=ncol(data1)
m2_ftdc=ncol(data2)
n=min(n1,n2)-1
###NODOS###
nodos1_ftdc=data.frame(data1[1,2:m1_ftdc])
nodos2_ftdc=data.frame(data2[1,2:m2_ftdc])
####MATRICES DEL MISMO TAMAÑO MENOS DOLAR

x1_ftdc=as.data.table(mutate(data1[2:n,1:m1_ftdc],Date=as.Date(V1,format="%Y%m%d")))
x1_ftdc=x1_ftdc%>%select(-V1)
x2_ftdc=as.data.table(mutate(data2[2:n,1:m2_ftdc],Date=as.Date(V1,format="%Y%m%d")))
x2_ftdc=x2_ftdc%>%select(-V1)



#-----

#1
# CARGA DE DATOS DE  FORWARD DE IPC
data3<-btsp
print (data3)
print(head(data3))
n3<-nrow(data3)
m3<-ncol(data3)

X3=data.table(as.matrix(as.double(as.matrix(data3[2:(n+1),m3]))))
X3_find=as.data.table(mutate(data3[2:(n+1),1:m3],Date=as.Date(V1,format="%Y%m%d")))
print(head(X3_find))

#----
#3
###Para Dolar

if (yext==1)
{
  #Cargar los símbolos de yahoo finance para FX
  start_date=fval-3660 #fecha inicial
  
  #Creación del objeto para guardar los datos
  dataEnvFX<-new.env()
  
  #obtener los datos
  getSymbols.yahoo(SymbolsFX_ftdc,env=dataEnvFX,from=start_date, to=(fval))
  #limpiarlos, alinearnos y quedarnos con el precio de cierre 
  bt.prep(dataEnvFX,align='remove.na',fill.gaps=T)
  
  #muestra de datos
  head(dataEnvFX$prices[,2])
  
  #Nos quedamos con los precios
  X3_ftdc=data.table(Date=as.Date(index(dataEnvFX$prices[,2])),coredata(dataEnvFX$prices[,2]))
} else  
{ 
  data3<-read.table(btsp)
  print(head(data3))
  n3<-nrow(data3)
  m3<-ncol(data3)
  X3=data.table(as.matrix(as.double(as.matrix(data3[2:(n+1),m3]))))
  X3_find=as.data.table(mutate(data3[2:(n+1),1:m3],Date=as.Date(V1,format="%Y%m%d")))
}

# CARGA DE DATOS DE  FORWARD DE IPC

data<-base
n<-nrow(data)
m_gov=ncol(data)

#x_orig_gov=data.frame(data[2:n,1:m_gov])
x_orig_gov=as.data.table(mutate(data[2:n,1:m_gov],Date=as.Date(V1,format="%Y%m%d")))
x_orig_gov=x_orig_gov%>%select(-V1)
nodos_gov=data.frame(data[1,2:m_gov])

#Cargar los símbolos de yahoo finance para EQ
start_date=fval-nh #fecha inicial

#Creación del objeto para guardar los datos
dataEnvEQ<-new.env()

#obtener los datos
getSymbols.yahoo(SymbolsEQ_find,env=dataEnvEQ,from=start_date, to=(fval))
#limpiarlos, alinearnos y quedarnos con el precio de cierre 
bt.prep(dataEnvEQ,align='remove.na',fill.gaps=T)

#muestra de datos
#  head(dataEnvEQ$prices)

#Nos quedamos con los precios
X3_find=data.table(Date=as.Date(index(dataEnvEQ$prices[,2])),coredata(dataEnvEQ$prices[,2]))

#INTERSECCIÓN DE FECHAS DE TODOS LOS INSUMOS    

#head(x_orig_gov)  
lin_gub=data.table(Date=as.Date(X3_ftdc[x_orig_gov,on=.(Date),nomatch=0]$Date)) #Fechas acciones, equity y guber
lin_gub_bmybdst_flib=data.table(Date=as.Date(lin_gub[x1_ftdc,on=.(Date),nomatch=0]$Date)) #Fechas acciones, equity, guber, st (bonde), libor
lin_gub_bmybdst_flibfwd=data.table(Date=as.Date(lin_gub_bmybdst_flib[x2_ftdc,on=.(Date),nomatch=0]$Date)) #Fechas acciones, equity, guber, st (bonde), libor, fwd
lin_gub_bmybdst_flibfwdspind=data.table(Date=as.Date(lin_gub_bmybdst_flibfwd[X3_find,on=.(Date),nomatch=0]$Date)) #Fechas acciones, equity, guber, st (bonde), libor, fwd, spot, equity or index 

n=nrow(lin_gub_bmybdst_flibfwdspind) #Historia de todos


#historia de curva gubernamental
x_orig_gov=lin_gub_bmybdst_flibfwdspind[x_orig_gov,on=.(Date),nomatch=0][order(-Date)]
x_orig_gov=x_orig_gov%>%select(-Date)

#historia de curvas de forward tdc
x1_ftdc=lin_gub_bmybdst_flibfwdspind[x1_ftdc,on=.(Date),nomatch=0][order(-Date)]
x1_ftdc=x1_ftdc%>%select(-Date)/100
x2_ftdc=lin_gub_bmybdst_flibfwdspind[x2_ftdc,on=.(Date),nomatch=0][order(-Date)]
x2_ftdc=x2_ftdc%>%select(-Date)/100
X3_ftdc=lin_gub_bmybdst_flibfwdspind[X3_ftdc,on=.(Date),nomatch=0][order(-Date)]
X3_ftdc=X3_ftdc%>%select(-Date)

#historia de curvas de forward ind
#CONSIDERAR LA CURVA GUBERNAMENTAL X1_ORIG_GOV
X3_find=lin_gub_bmybdst_flibfwdspind[X3_find,on=.(Date),nomatch=0][order(-Date)]
X3_find=X3_find%>%select(-Date)


#---------
#FORWARDS Y/O FUTUROS DE TIPO DE CAMBIO CÁLCULO

################MATRICES DE INTERPOLACION LINEAL ####################

m=ncol(plazos_fwd)
X1_fwtdc=matrix(0,n,m)
X2_fwtdc=matrix(0,n,m)

for (j in 1:n)
{
  X1_fwtdc[j,]=if(itpl==0){approx(nodos1_ftdc,x1_ftdc[j,],plazos_fwd,rule=2)$y}else{talamb(nodos1_ftdc,x1_ftdc[j,],plazos_fwd)}
  X2_fwtdc[j,]=if(itpl==0){approx(nodos2_ftdc,x2_ftdc[j,],plazos_fwd,rule=2)$y}else{talamb(nodos2_ftdc,x2_ftdc[j,],plazos_fwd)}
  if(trlib==1){X1_fwtdc[j,]=((1+X1_fwtdc[j,])^(plazos_fwd/180)-1)*360/plazos_fwd} #transformación de act 180 a 360
}


futuroTC = function(t,tl,tn,s,k) #t=dias por vencer, tn=tasa nacional para tipo de cambio forward, tl= tasa extranjera pra tipo de cambio forward, S=spot
{
  f=s*((1+tn*t/360)/(1+tl*t/360)) #Se obtiene el tipo de cambio forward
  t(as.numeric((f-k)/(1+t*tn/360))) #Se obtiene el valor del payoff a valor presente con el valor z que define si es largo o corto
}

X3_ftdc=as.matrix(X3_ftdc)
X_futtdc=cbind(X1_fwtdc,X2_fwtdc,X3_ftdc)

V0_fwtdc=futuroTC(plazos_fwd,X1_fwtdc[1,],X2_fwtdc[1,],X3_ftdc[1,],kst_fwd)*contratos_fwd*nominal_fwd


#FORWARDS Y/O FUTUROS DE ÍNDICES CÁLCULO

################MATRICES DE INTERPOLACION LINEAL ####################

m_ind=ncol(plazos_fwd_ind)
X1_fwind=matrix(0,n,m_ind) #DIVIDENDOS
X2_fwind=matrix(0,n,m_ind)

for (j in 1:n)
{
  #X1_fwind[j,]=if(itpl==0){approx(nodos1_,x1_ftdc[j,],plazos_fwd)$y}else{talamb(nodos1_ftdc,x1_ftdc[j,],plazos_fwd)}
  X2_fwind[j,]=if(itpl==0){approx(nodos_gov,x_orig_gov[j,],plazos_fwd_ind,rule=2)$y}else{talamb(nodos_gov,x_orig_gov[j,],plazos_fwd_ind)}
  #if(trlib==1){X1_fwtdc[j,]=((1+X1_fwtdc[j,])^(plazos_fwd/180)-1)*360/plazos_fwd} #transformación de act 180 a 360
}

X3_find=as.matrix(X3_find)
X_futind=cbind(X1_fwind,X2_fwind,matrix(X3_find,n,ncol(X1_fwind)))

V0_fwind=futuroTC(plazos_fwd_ind,X1_fwind[1,],X2_fwind[1,],X3_find[1,],kst_fwd_ind)*contratos_fwd_ind*nominal_fwd_ind


#DIMENSION DE TODOS LOS INSTRUMENTOS
#Son 8 instrumentos financieros (9 si separamos acciones y divisas)
n_if=matrix(0,2,1)
n_if[1]=ncol(X_futtdc) #fwd tdc
n_if[2]=ncol(X_futind) #fwd ind

#valor del portafolios

V0_port=cbind(V0_fwtdc, V0_fwind)
V0T_port=sum(V0_port)



#INTEGRACIÓN DE TODOS LOS FACTORES DE RIESGO EN UNA MATRIZ
X_port=cbind(X_futtdc,X_futind) #Factores de riesgo del portafolios de 8(9) instrumentos financieros

#Cálculo de variaciones Delta_X DEL PORTAFOLIOS
DeltaX_port=as.matrix(log(X_port[1:(n-1),]/X_port[2:(n),]))
DeltaX_port[is.nan(DeltaX_port)] <- 0 #quitamos NaN
DeltaX_port[is.na(DeltaX_port)] <- 0 #quitamos Na
DeltaX_port[is.infinite(DeltaX_port)] <- 0 #quitamos Na


Ns=nrow(DeltaX_port) #Definimos número de simulaciones
alpha=0.98 #Nivel de Confianza para las medidas de riesgo

DeltaX_s=DeltaX_port
#print(head(DeltaX_s))


#Cálculo de matriz de pérdidas y ganancias FUTUROS TDC
#dimensión
m=ncol(plazos_fwd)  #PASO CLAVE
X_s_fwtdc=matrix(0,Ns,n_if[1]) #Factores de riesgo simulados con base en DeltaX_s x0*(1+Delta_Xs) #PASO CLAVE
V_fwtdc=matrix(0,Ns,m)
Vfr1_fwtdc=matrix(0,Ns,m)
Vfr2_fwtdc=matrix(0,Ns,m)
Vfr3_fwtdc=matrix(0,Ns,m)
PG_fwtdc=matrix(0,Ns,m) #Pèrdidas y ganancias
PGfr1_fwtdc=matrix(0,Ns,m)
PGfr2_fwtdc=matrix(0,Ns,m)
PGfr3_fwtdc=matrix(0,Ns,m)
PGT_fwtdc=matrix(0,Ns,1)
PGfr1T_fwtdc=matrix(0,Ns,1)
PGfr2T_fwtdc=matrix(0,Ns,1)
PGfr3T_fwtdc=matrix(0,Ns,1)

DeltaX_s_fwtdc=DeltaX_s[,1:(n_if[1])]  #PASO CLAVE
x0_fwtdc=X_futtdc[1,] #PASO CLAVE


for (i in 1:Ns)
{
  X_s_fwtdc[i,]=x0_fwtdc*(1+DeltaX_s_fwtdc[i,])
  #PASO CLAVE
  V_fwtdc[i,]=futuroTC(plazos_fwd,X_s_fwtdc[i,1:((n_if[1]-1)/2)],X_s_fwtdc[i,((n_if[1]-1)/2+1):(n_if[1]-1)],X_s_fwtdc[i,(n_if[1])],kst_fwd)*contratos_fwd*nominal_fwd     
  #PASO CLAVE
  Vfr1_fwtdc[i,]=futuroTC(plazos_fwd,X_s_fwtdc[i,1:((n_if[1]-1)/2)],x0_fwtdc[((n_if[1]-1)/2+1):(n_if[1]-1)],x0_fwtdc[(n_if[1])],kst_fwd)*contratos_fwd*nominal_fwd
  #PASO CLAVE
  Vfr2_fwtdc[i,]=futuroTC(plazos_fwd,x0_fwtdc[1:((n_if[1]-1)/2)],X_s_fwtdc[i,((n_if[1]-1)/2+1):(n_if[1]-1)],x0_fwtdc[(n_if[1])],kst_fwd)*contratos_fwd*nominal_fwd
  #PASO CLAVE
  Vfr3_fwtdc[i,]=futuroTC(plazos_fwd,x0_fwtdc[1:((n_if[1]-1)/2)],x0_fwtdc[((n_if[1]-1)/2+1):(n_if[1]-1)],X_s_fwtdc[i,(n_if[1])],kst_fwd)*contratos_fwd*nominal_fwd
  PG_fwtdc[i,]=V_fwtdc[i,]-V0_fwtdc
  PGfr1_fwtdc[i,]=Vfr1_fwtdc[i,]-V0_fwtdc
  PGfr2_fwtdc[i,]=Vfr2_fwtdc[i,]-V0_fwtdc
  PGfr3_fwtdc[i,]=Vfr3_fwtdc[i,]-V0_fwtdc
  PGT_fwtdc[i,]=sum(PG_fwtdc[i,])
  PGfr1T_fwtdc[i,]=sum(PGfr1_fwtdc[i,])
  PGfr2T_fwtdc[i,]=sum(PGfr2_fwtdc[i,])
  PGfr3T_fwtdc[i,]=sum(PGfr3_fwtdc[i,])
}

PG_fwtdc[1:5,]
PGfr1_fwtdc[1:5,]
PGfr2_fwtdc[1:5,]
PGT_fwtdc[1:5,]


#VaR por posición
VaRCont_fwtdc=matrix(0,1,m)#contrato
VaRfr1_fwtdc=matrix(0,1,m)#var factor riesgo 1
VaRfr2_fwtdc=matrix(0,1,m)#var factor riesgo 2
VaRfr3_fwtdc=matrix(0,1,m)#var factor de riesgo 3
CVaRCont_fwtdc=matrix(0,1,m)
CVaRfr1_fwtdc=matrix(0,1,m)
CVaRfr2_fwtdc=matrix(0,1,m)
CVaRfr3_fwtdc=matrix(0,1,m)


for (i in (1:m))
{
  VaRCont_fwtdc[i]=quantile(PG_fwtdc[,i],1-alpha,Ns)
  VaRfr1_fwtdc[i]=quantile(PGfr1_fwtdc[,i],1-alpha,Ns)
  VaRfr2_fwtdc[i]=quantile(PGfr2_fwtdc[,i],1-alpha,Ns)
  VaRfr3_fwtdc[i]=quantile(PGfr3_fwtdc[,i],1-alpha,Ns)
  CVaRfr1_fwtdc[i]= mean(merge(which(PGfr1_fwtdc[,i]<VaRfr1_fwtdc[i]),cbind(seq(1,Ns),PGfr1_fwtdc[,i]), by.x=1,by.y=1)[,2])
  CVaRfr2_fwtdc[i]= mean(merge(which(PGfr2_fwtdc[,i]<VaRfr2_fwtdc[i]),cbind(seq(1,Ns),PGfr2_fwtdc[,i]), by.x=1,by.y=1)[,2])
  CVaRfr3_fwtdc[i]= mean(merge(which(PGfr3_fwtdc[,i]<VaRfr3_fwtdc[i]),cbind(seq(1,Ns),PGfr3_fwtdc[,i]), by.x=1,by.y=1)[,2])
  CVaRCont_fwtdc[i]= mean(merge(which(PG_fwtdc[,i]<VaRCont_fwtdc[i]),cbind(seq(1,Ns),PG_fwtdc[,i]), by.x=1,by.y=1)[,2])
}

VaRCont_fwtdc
VaRfr1_fwtdc
VaRfr2_fwtdc
CVaRCont_fwtdc
CVaRfr1_fwtdc
CVaRfr2_fwtdc


#VaR Total
VaRTotal_fwtdc=quantile(PGT_fwtdc,1-alpha,Ns)
CVaRTotal_fwtdc= mean(merge(which(PGT_fwtdc<VaRTotal_fwtdc),cbind(seq(1,Ns),PGT_fwtdc), by.x=1,by.y=1)[,2])
VaRTotalfr1_fwtdc=quantile(PGfr1T_fwtdc,1-alpha,Ns)
CVaRTotalfr1_fwtdc= mean(PGfr1T_fwtdc[which(PGfr1T_fwtdc<VaRTotalfr1_fwtdc),])
VaRTotalfr2_fwtdc=quantile(PGfr2T_fwtdc,1-alpha,Ns)
CVaRTotalfr2_fwtdc= mean(PGfr2T_fwtdc[which(PGfr2T_fwtdc<VaRTotalfr2_fwtdc),])
VaRTotalfr3_fwtdc=quantile(PGfr3T_fwtdc,1-alpha,Ns)
CVaRTotalfr3_fwtdc= mean(PGfr3T_fwtdc[which(PGfr2T_fwtdc<VaRTotalfr2_fwtdc),])

#-----------------------------------RESULTADOS VAR Y CVAR tdc
cbind(VaRTotal_fwtdc,sum(V0_fwtdc), VaRCont_fwtdc, V0_fwtdc)
cbind(CVaRTotal_fwtdc,sum(V0_fwtdc), CVaRCont_fwtdc, V0_fwtdc)
print("VaR total, VaR libor, VaR tasa forward para tdc, VaR spot")
cbind(VaRTotal_fwtdc,VaRTotalfr1_fwtdc,VaRTotalfr2_fwtdc,VaRTotalfr3_fwtdc)
print("CVaR total, CVaR libor, CVaR tasa forward para tdc, CVaR spot")
cbind(CVaRTotal_fwtdc,CVaRTotalfr1_fwtdc,CVaRTotalfr2_fwtdc,CVaRTotalfr3_fwtdc)

###################VAR Y CVAR CON ALISADO PARA TDC

#VaR por posición
VaRCont_CA_fwtdc=matrix(0,1,m)#contrato
VaRfr1_CA_fwtdc=matrix(0,1,m)#var factor riesgo 1
VaRfr2_CA_fwtdc=matrix(0,1,m)#var factor riesgo 2
VaRfr3_CA_fwtdc=matrix(0,1,m)#var factor de riesgo 3
CVaRCont_CA_fwtdc=matrix(0,1,m)
CVaRfr1_CA_fwtdc=matrix(0,1,m)
CVaRfr2_CA_fwtdc=matrix(0,1,m)
CVaRfr3_CA_fwtdc=matrix(0,1,m)

for (i in (1:m))
{
  VaRCont_CA_fwtdc[i]=wquantile(PG_fwtdc[,i],w=rep(1,length(PG_fwtdc[,i])),1-alpha)
  VaRfr1_CA_fwtdc[i]=wquantile(PGfr1_fwtdc[,i],w=rep(1,length(PGfr1_fwtdc[,i])),1-alpha)
  VaRfr2_CA_fwtdc[i]=wquantile(PGfr2_fwtdc[,i],w=rep(1,length(PGfr2_fwtdc[,i])),1-alpha)
  VaRfr3_CA_fwtdc[i]=wquantile(PGfr3_fwtdc[,i],w=rep(1,length(PGfr3_fwtdc[,i])),1-alpha)
  CVaRfr1_CA_fwtdc[i]= wcvar(PGfr1_fwtdc[,i],w=rep(1,length(PGfr1_fwtdc[,i])),1-alpha)
  CVaRfr2_CA_fwtdc[i]= wcvar(PGfr2_fwtdc[,i],w=rep(1,length(PGfr2_fwtdc[,i])),1-alpha)
  CVaRfr3_CA_fwtdc[i]= wcvar(PGfr3_fwtdc[,i],w=rep(1,length(PGfr3_fwtdc[,i])),1-alpha)
  CVaRCont_CA_fwtdc[i]= wcvar(PG_fwtdc[,i],w=rep(1,length(PG_fwtdc[,i])),1-alpha)
}

VaRCont_CA_fwtdc
VaRfr1_CA_fwtdc
VaRfr2_CA_fwtdc
CVaRCont_CA_fwtdc
CVaRfr1_CA_fwtdc
CVaRfr2_CA_fwtdc

#VaR Total
VaRTotal_CA_fwtdc=wquantile(PGT_fwtdc,w=rep(1,length(PGT_fwtdc)),1-alpha)
CVaRTotal_CA_fwtdc= wcvar(PGT_fwtdc,w=rep(1,length(PGT_fwtdc)),1-alpha)
VaRTotalfr1_CA_fwtdc=wquantile(PGfr1T_fwtdc,w=rep(1,length(PGfr1T_fwtdc)),1-alpha)
CVaRTotalfr1_CA_fwtdc= wcvar(PGfr1T_fwtdc,w=rep(1,length(PGfr1T_fwtdc)),1-alpha)
VaRTotalfr2_CA_fwtdc=wquantile(PGfr2T_fwtdc,w=rep(1,length(PGfr2T_fwtdc)),1-alpha)
CVaRTotalfr2_CA_fwtdc= wcvar(PGfr2T_fwtdc,w=rep(1,length(PGfr2T_fwtdc)),1-alpha)
VaRTotalfr3_CA_fwtdc=wquantile(PGfr3T_fwtdc,w=rep(1,length(PGfr3T_fwtdc)),1-alpha)
CVaRTotalfr3_CA_fwtdc= wcvar(PGfr3T_fwtdc,w=rep(1,length(PGfr3T_fwtdc)),1-alpha)

cbind(VaRTotal_CA_fwtdc,sum(V0_fwtdc), VaRCont_CA_fwtdc, V0_fwtdc)
cbind(CVaRTotal_CA_fwtdc,sum(V0_fwtdc), CVaRCont_CA_fwtdc, V0_fwtdc)
print("VaR con alisado total, VaR libor, VaR tasa forward para tdc, Spot")
cbind(VaRTotal_CA_fwtdc,VaRTotalfr1_CA_fwtdc,VaRTotalfr2_CA_fwtdc,VaRTotalfr3_CA_fwtdc)
print("CVaR con alisado total, CVaR libor, CVaR tasa forward para tdc,Spot")
cbind(CVaRTotal_CA_fwtdc,CVaRTotalfr1_CA_fwtdc,CVaRTotalfr2_CA_fwtdc,CVaRTotalfr3_CA_fwtdc)


######################HASTA AQUI



#CALCULO IPC 

#Cálculo de matriz de pérdidas y ganancias FUTUROS IPC
m=ncol(plazos_fwd_ind)  #PASO CLAVE
X_s_fwind=matrix(0,Ns,n_if[2]) #Factores de riesgo simulados con base en DeltaX_s x0*(1+Delta_Xs) #PASO CLAVE
V_fwind=matrix(0,Ns,m)
Vfr1_fwind=matrix(0,Ns,m)
Vfr2_fwind=matrix(0,Ns,m)
Vfr3_fwind=matrix(0,Ns,m)
PG_fwind=matrix(0,Ns,m) #Pèrdidas y ganancias
PGfr1_fwind=matrix(0,Ns,m)
PGfr2_fwind=matrix(0,Ns,m)
PGfr3_fwind=matrix(0,Ns,m)
PGT_fwind=matrix(0,Ns,1)
PGfr1T_fwind=matrix(0,Ns,1)
PGfr2T_fwind=matrix(0,Ns,1)
PGfr3T_fwind=matrix(0,Ns,1)

DeltaX_s_fwind=DeltaX_s[,sum(n_if[1],1):sum(n_if[1:2])]  #PASO CLAVE
x0_fwind=X_futind[1,] #PASO CLAVE

for (i in 1:Ns)
{
  X_s_fwind[i,]=x0_fwind*(1+DeltaX_s_fwind[i,])
  #PASO CLAVE
  V_fwind[i,]=futuroTC(plazos_fwd_ind,X_s_fwind[i,1:(n_if[2]/3)],X_s_fwind[i,(n_if[2]/3+1):(n_if[2]*2/3)],X_s_fwind[i,(n_if[2]*2/3+1):n_if[2]],kst_fwd_ind)*contratos_fwd_ind*nominal_fwd_ind     
  Vfr1_fwind[i,]=futuroTC(plazos_fwd_ind,X_s_fwind[i,1:(n_if[2]/3)],x0_fwind[(n_if[2]/3+1):(n_if[2]*2/3)],x0_fwind[(n_if[2]*2/3+1):n_if[2]],kst_fwd_ind)*contratos_fwd_ind*nominal_fwd_ind
  #PASO CLAVE
  Vfr2_fwind[i,]=futuroTC(plazos_fwd_ind,x0_fwind[1:(n_if[2]/3)],X_s_fwind[i,(n_if[2]/3+1):(n_if[2]*2/3)],x0_fwind[(n_if[2]*2/3+1):n_if[2]],kst_fwd_ind)*contratos_fwd_ind*nominal_fwd_ind
  #PASO CLAVE
  Vfr3_fwind[i,]=futuroTC(plazos_fwd_ind,x0_fwind[1:(n_if[2]/3)],x0_fwind[(n_if[2]/3+1):(n_if[2]*2/3)],X_s_fwind[i,(n_if[2]*2/3+1):n_if[2]],kst_fwd_ind)*contratos_fwd_ind*nominal_fwd_ind
  PG_fwind[i,]=V_fwind[i,]-V0_fwind
  PGfr1_fwind[i,]=Vfr1_fwind[i,]-V0_fwind
  PGfr2_fwind[i,]=Vfr2_fwind[i,]-V0_fwind
  PGfr3_fwind[i,]=Vfr3_fwind[i,]-V0_fwind
  PGT_fwind[i,]=sum(PG_fwind[i,])
  PGfr1T_fwind[i,]=sum(PGfr1_fwind[i,])
  PGfr2T_fwind[i,]=sum(PGfr2_fwind[i,])
  PGfr3T_fwind[i,]=sum(PGfr3_fwind[i,])
}


PG_fwind[1:5,]
#PGfr1_fwind[1:5,]
#PGfr2_fwind[1:5,]
#PGT_fwind[1:5,]


#VaR por posición
VaRCont_fwind=matrix(0,1,m)
VaRfr1_fwind=matrix(0,1,m)
VaRfr2_fwind=matrix(0,1,m)
VaRfr3_fwind=matrix(0,1,m)
CVaRCont_fwind=matrix(0,1,m)
CVaRfr1_fwind=matrix(0,1,m)
CVaRfr2_fwind=matrix(0,1,m)
CVaRfr3_fwind=matrix(0,1,m)
for (i in (1:m))
{
  VaRCont_fwind[i]=quantile(PG_fwind[,i],1-alpha,Ns)
  VaRfr1_fwind[i]=quantile(PGfr1_fwind[,i],1-alpha,Ns)
  VaRfr2_fwind[i]=quantile(PGfr2_fwind[,i],1-alpha,Ns)
  VaRfr3_fwind[i]=quantile(PGfr3_fwind[,i],1-alpha,Ns)
  CVaRfr1_fwind[i]= mean(merge(which(PGfr1_fwind[,i]<VaRfr1_fwind[i]),cbind(seq(1,Ns),PGfr1_fwind[,i]), by.x=1,by.y=1)[,2])
  CVaRfr2_fwind[i]= mean(merge(which(PGfr2_fwind[,i]<VaRfr2_fwind[i]),cbind(seq(1,Ns),PGfr2_fwind[,i]), by.x=1,by.y=1)[,2])
  CVaRfr3_fwind[i]= mean(merge(which(PGfr3_fwind[,i]<VaRfr3_fwind[i]),cbind(seq(1,Ns),PGfr3_fwind[,i]), by.x=1,by.y=1)[,2])
  CVaRCont_fwind[i]= mean(merge(which(PG_fwind[,i]<VaRCont_fwind[i]),cbind(seq(1,Ns),PG_fwind[,i]), by.x=1,by.y=1)[,2])
}
VaRCont_fwind
VaRfr1_fwind
VaRfr2_fwind
CVaRCont_fwind
CVaRfr1_fwind
CVaRfr2_fwind


#VaR Total

VaRTotal_fwind=quantile(PGT_fwind,1-alpha,Ns)
CVaRTotal_fwind= mean(merge(which(PGT_fwind<VaRTotal_fwind),cbind(seq(1,Ns),PGT_fwind), by.x=1,by.y=1)[,2])
VaRTotalfr1_fwind=quantile(PGfr1T_fwind,1-alpha,Ns)
CVaRTotalfr1_fwind= mean(PGfr1T_fwind[which(PGfr1T_fwind<VaRTotalfr1_fwind),])
VaRTotalfr2_fwind=quantile(PGfr2T_fwind,1-alpha,Ns)
CVaRTotalfr2_fwind= mean(PGfr2T_fwind[which(PGfr2T_fwind<VaRTotalfr2_fwind),])
VaRTotalfr3_fwind=quantile(PGfr3T_fwind,1-alpha,Ns)
CVaRTotalfr3_fwind= mean(PGfr3T_fwind[which(PGfr3T_fwind<VaRTotalfr3_fwind),])


print(cbind(VaRTotal_fwind,sum(V0_fwind), VaRCont_fwind, V0_fwind))
print(cbind(CVaRTotal_fwind,sum(V0_fwind), CVaRCont_fwind, V0_fwind))
print ("VaR total,, VaR")
print(cbind(VaRTotal_fwind,VaRTotalfr1_fwind,VaRTotalfr2_fwind,VaRTotalfr3_fwind))
cbind(CVaRTotal_fwind,CVaRTotalfr1_fwind,CVaRTotalfr2_fwind,CVaRTotalfr3_fwind)

#########CVaR y VaR con alisado

VaRCont_CA_fwind=matrix(0,1,m)
VaRfr1_CA_fwind=matrix(0,1,m)
VaRfr2_CA_fwind=matrix(0,1,m)
VaRfr3_CA_fwind=matrix(0,1,m)
CVaRCont_CA_fwind=matrix(0,1,m)
CVaRfr1_CA_fwind=matrix(0,1,m)
CVaRfr2_CA_fwind=matrix(0,1,m)
CVaRfr3_CA_fwind=matrix(0,1,m)
for (i in (1:m))
{
  VaRCont_CA_fwind[i]=wquantile(PG_fwind[,i],w=rep(1,length(PG_fwind[,i])),1-alpha)
  VaRfr1_CA_fwind[i]=wquantile(PGfr1_fwind[,i],w=rep(1,length(PGfr1_fwind[,i])),1-alpha)
  VaRfr2_CA_fwind[i]=wquantile(PGfr2_fwind[,i],w=rep(1,length(PGfr2_fwind[,i])),1-alpha)
  VaRfr3_CA_fwind[i]=wquantile(PGfr3_fwind[,i],w=rep(1,length(PGfr3_fwind[,i])),1-alpha)
  CVaRfr1_CA_fwind[i]= wcvar(PGfr1_fwind[,i],w=rep(1,length(PGfr1_fwind[,i])),1-alpha)
  CVaRfr2_CA_fwind[i]=wcvar(PGfr2_fwind[,i],w=rep(1,length(PGfr2_fwind[,i])),1-alpha)
  CVaRfr3_CA_fwind[i]= wcvar(PGfr3_fwind[,i],w=rep(1,length(PGfr3_fwind[,i])),1-alpha)
  CVaRCont_CA_fwind[i]= wcvar(PG_fwind[,i],w=rep(1,length(PG_fwind[,i])),1-alpha)
}

VaRCont_CA_fwind
VaRfr1_CA_fwind
VaRfr2_CA_fwind
CVaRCont_CA_fwind
CVaRfr1_CA_fwind
CVaRfr2_CA_fwind


#VaR Total

VaRTotal_CA_fwind=wquantile(PGT_fwind,w=rep(1,length(PGT_fwind)),1-alpha)
CVaRTotal_CA_fwind= wcvar(PGT_fwind,w=rep(1,length(PGT_fwind)),1-alpha)
VaRTotalfr1_CA_fwind=wquantile(PGfr1T_fwind,w=rep(1,length(PGfr1T_fwind)),1-alpha)
CVaRTotalfr1_CA_fwind= wcvar(PGfr1T_fwind,w=rep(1,length(PGfr1T_fwind)),1-alpha)
VaRTotalfr2_CA_fwind=wquantile(PGfr2T_fwind,w=rep(1,length(PGfr2T_fwind)),1-alpha)
CVaRTotalfr2_CA_fwind= wcvar(PGfr2T_fwind,w=rep(1,length(PGfr2T_fwind)),1-alpha)
VaRTotalfr3_CA_fwind=wquantile(PGfr3T_fwind,w=rep(1,length(PGfr3T_fwind)),1-alpha)
CVaRTotalfr3_CA_fwind= wcvar(PGfr3T_fwind,w=rep(1,length(PGfr3T_fwind)),1-alpha)


print(cbind(VaRTotal_CA_fwind,sum(V0_fwind), VaRCont_CA_fwind, V0_fwind))
print(cbind(CVaRTotal_CA_fwind,sum(V0_fwind), CVaRCont_CA_fwind, V0_fwind))
print ("VaR total con alisado,, VaR ")
print(cbind(VaRTotal_CA_fwind,VaRTotalfr1_CA_fwind,VaRTotalfr2_CA_fwind,VaRTotalfr3_CA_fwind))
cbind(CVaRTotal_fwind,CVaRTotalfr1_CA_fwind,CVaRTotalfr2_CA_fwind,CVaRTotalfr3_CA_fwind)



########33

#Medición de riesgo por factor de riesgo de todo el portafolios
#Acciones
#1. Acciones 
#2. Forward de IPC
PGPort_ACC= PGfr3T_fwind #Pérdidas y ganancias
VaRPort_ACC=quantile(PGPort_ACC,1-alpha,Ns) #VaR
CVaRPort_ACC= mean(PGPort_ACC[which(PGPort_ACC<VaRPort_ACC)]) #CVaR


#Tasa de Interés
#1. Dado que swaps y bondes son de tasa de interés usaremos PGT_bd y PGT_sw
#2. Para futuros usaremos PGfr1T_fwtdc y PGfr2T_fwtdc
PGPort_TI=PGfr1T_fwtdc+PGfr2T_fwtdc +PGfr2T_fwind #Pérdidas y ganancias
VaRPort_TI=quantile(PGPort_TI,1-alpha,Ns) #VaR
CVaRPort_TI= mean(PGPort_TI[which(PGPort_TI<VaRPort_TI)]) #CVaR

#Tipo de cambio
#1. Dado que swaps y bondes son de tasa de interés no usamos nada
#2. Para futuros usamos sólo PGfr3T_fwtdc
PGPort_TDC=PGfr3T_fwtdc  #Pérdidas y ganancias
VaRPort_TDC=quantile(PGPort_TDC,1-alpha,Ns) #VaR
CVaRPort_TDC= mean(PGPort_TDC[which(PGPort_TDC<VaRPort_TDC)]) #CVaR


#Medición de riesgo de todo el portafolios
#Sumar todos los PGT de todos los instrumentos

PGT_Port=PGPort_ACC+PGPort_TI+PGPort_TDC
VaRTotal_Port=quantile(PGT_Port,1-alpha,Ns) #VaR
CVaRTotal_Port= mean(PGT_Port[which(PGT_Port<VaRTotal_Port)]) #CVaR
print(VaRTotal_Port)
print(CVaRTotal_Port)
print(V0T_port)

######ahora para con alisado

#Tasa de Interés
#1. Dado que swaps y bondes son de tasa de interés usaremos PGT_bd y PGT_sw
#2. Para futuros usaremos PGfr1T_fwtdc y PGfr2T_fwtdc

VaRPort_CA_TI=wquantile(PGPort_TI,w=rep(1,length(PGPort_TI)),1-alpha) #VaR
CVaRPort_CA_TI= wcvar(PGPort_TI,w=rep(1,length(PGPort_TI)),1-alpha) #CVaR

#Tipo de cambio
#1. Dado que swaps y bondes son de tasa de interés no usamos nada
#2. Para futuros usamos sólo PGfr3T_fwtdc

VaRPort_CA_TDC=wquantile(PGPort_TDC,w=rep(1,length(PGPort_TDC)),1-alpha)  #VaR
CVaRPort_CA_TDC= wcvar(PGPort_TDC,w=rep(1,length(PGPort_TDC)),1-alpha) #CVaR


#Medición de riesgo de todo el portafolios
#Sumar todos los PGT de todos los instrumentos


VaRTotal_CA_Port=wquantile(PGT_Port,w=rep(1,length(PGT_Port)),1-alpha) #VaR
CVaRTotal_CA_Port= wcvar(PGT_Port,w=rep(1,length(PGT_Port)),1-alpha) #CVaR
print(VaRTotal_CA_Port)
print(CVaRTotal_CA_Port)
print(V0T_port)
