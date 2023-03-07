library(caret)  #findCorrelation
library(stats) #cor
library(utils)  #choose.dir, read.csv2
library(base)  #pozostale funkcje niewymienione wyzej

workdir=choose.dir()
setwd(workdir)
path<-file.path(getwd())
sciezkiDoPliku <- list(plik1 ="2012.csv", plik2 = "2014.csv", plik3 = "2016.csv", plik4="2018.csv", plik5="2020.csv")
#Intr. to R - str. 26 - listy
daneZRoku <- list()
df<-list.files(path,pattern=".csv")

obliczWspolczynniki <- function(daneZPliku){
  #współczynnik zmienności
  srednia <- colMeans(daneZPliku)
  odch.stand. <- sapply(daneZPliku,sd)
  vs<-odch.stand./srednia
  
  #eliminacja zmiennych, dla których vs<10%
  daneZPliku<-subset(daneZPliku,,vs>0.1)
  
  #obliczanie macierzy korelacji
  korelacja<-cor(daneZPliku,method="pearson")
  
  #dyskryminacja zmiennych przy użyciu pakietu 'caret'
  dyskryminacja_korelacja<-findCorrelation(korelacja,cutoff=0.81,exact=TRUE)
  dyskryminacja_korelacja<-sort(dyskryminacja_korelacja)
  macierz_zredukowana<-daneZPliku[-c(dyskryminacja_korelacja)]
  
  #standaryzacja
  macierz_standaryzowana<-scale(macierz_zredukowana,center=TRUE,scale=TRUE)
  macierz_standaryzowana<-as.data.frame(macierz_standaryzowana) #musi być zmienione na data frame, bo potem nie da się użyć operatora $ w poniższych pętlach if
  
  #zamiana nominant i destymulant w stymulanty
  #nie mamy nominant, ale mamy destymulanty (x4, x6, x7, x8, x9, x10) - zamiana destymulant odbywa się przez pomnożenie każdej zmiennej przez -1
  #pusty wektor
  d<-c()
  # stworzenie wektora do zamiany destymulant w stymulanty, w zależności od tego, które zmienne pozostały w modelu po ich dyskryminacji wg kryterium współczynnika zmienności
  
  if ((is.null(macierz_standaryzowana$x1))==FALSE){
    d<-append(d,1)
    # append - dodaj do wektora d liczbę 1. Wektor d to wektor pusty
  } 
  
  if ((is.null(macierz_standaryzowana$x2))==FALSE){
    d<-append(d,1)
  }
  if ((is.null(macierz_standaryzowana$x3))==FALSE){
    d<-append(d,1)
  }
  if ((is.null(macierz_standaryzowana$x4))==FALSE){
    d<-append(d,-1)
  }
  if ((is.null(macierz_standaryzowana$x5))==FALSE){
    d<-append(d,1)
  }
  if ((is.null(macierz_standaryzowana$x6))==FALSE){
    d<-append(d,-1)
  }
  if ((is.null(macierz_standaryzowana$x7))==FALSE){
    d<-append(d,-1)
  }
  if ((is.null(macierz_standaryzowana$x8))==FALSE){
    d<-append(d,1)
  }
  if ((is.null(macierz_standaryzowana$x9))==FALSE){
    d<-append(d,-1)
  }
  if ((is.null(macierz_standaryzowana$x10))==FALSE){
    d<-append(d,-1)
  }
  
  macierz_zamiana<-sweep(macierz_standaryzowana,2,d,'*') 
  
    #wyznaczenie miary rozwoju 
  # 1. wyznaczenie min i max dla każdej zmiennej (kolumny)
  x<-apply(macierz_zamiana,2,min) #min
  y<-apply(macierz_zamiana,2,max) #max
  
  #2. pi = zsumowane wartości dla każdego wiersza i dla min i max
  z<-rowSums(macierz_zamiana,na.rm=FALSE)
  x<-sum(x)
  y<-sum(y)
  
  #stworzenie mi -> mi= (pi-x)/(y-x)
  mi<-(z-x)/(y-x)
  miary_rozwoju<-data.frame(mi)
  rownames(miary_rozwoju)<-(c("Węgry","Polska","Slowacja","Czechy","Francja","Holandia","Dania","Norwegia"))
  
  print(miary_rozwoju)


  
  #Intr. to R - str. 27 - data.frame, str. 48 - functions

}


for (i in 1:length(sciezkiDoPliku)) {
  rok<-read.csv2(sciezkiDoPliku[[i]],header = TRUE,sep=";",,dec=",") 
  daneZRoku[[length(daneZRoku) + 1]] <- obliczWspolczynniki(rok)
}




