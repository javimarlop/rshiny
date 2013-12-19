library(shiny)
require(diffEq)
library(rootSolve)
library(bvpSolve)
library(ReacTran)
library(shape)
library(diagram)
library(deTestSet)
library(scatterplot3d)
library(caTools)
#setwd("G:/R-cosas/R-12 - Fractals/DinamicSystems")


#------------------------------------------------
#------- Lorenz Attractor
a.lor <- -8/3; b.lor <- -10; c.lor <- 28
yini.lor <- c(X = 1, Y = 1, Z = 1)

Lorenz <- function (t, y, parms) {
  with(as.list(y), {
    dX <- a.lor * X + Y * Z
    dY <- b.lor * (Y - Z)
    dZ <- -X * Y + c.lor * Y - Z
    list(c(dX, dY, dZ)) })
}

times.lor <- seq(from = 0, to = 100, by = 0.01)
out.lor <- ode(y = yini.lor, times = times.lor, func = Lorenz, parms = NULL)  
out.lor.3d <- as.data.frame(out.lor)
out.lor.3d$lor3d <- "lor3d"

out.lor.2d <- as.data.frame(out.lor)
out.lor.2d$lor2d <- "lor2d" 

# Zeros...
x.zer.lor <- out.lor.3d[abs(out.lor.3d$X) < 1e-2,]; dim(x.zer.lor)
y.zer.lor <- out.lor.3d[abs(out.lor.3d$Y) < 1e-2,]; dim(y.zer.lor)
z.zer.lor <- out.lor.3d[abs(out.lor.3d$Z) < 1e-2,]; dim(z.zer.lor)


#------------------------------------------------
#------- Rossler Attractor
yini.ros  <- c(1, 1, 1)

rosslereq <- function(t, y, parms) {
  dy1 <- -1  * y[2] - y[3]
  dy2 <-  y[1] + 0.2* y[2]
  dy3 <- 0.2 + y[3] * (y[1] - 5)
  list(c(dy1, dy2, dy3))
}

times.ros <- seq(from = 0, to = 100, by = 0.01)
out.ros   <- ode (times = times.ros, y = yini.ros, func = rosslereq, parms = NULL, method = rkMethod("rk45ck")) 
out.ros.3d <- as.data.frame(out.ros)
out.ros.3d$ros3d <- "ros3d"

out.ros.2d <- as.data.frame(out.ros)
out.ros.2d$ros2d <- "ros2d"



#------------------------------------------------
#------- Pleaide
pleiade <- function (t, Y, pars) {
  x <- Y[1:7]
  y <- Y[8:14]
  u <- Y[15:21]
  v <- Y[22:28]
  
  distx <- outer(x, x, FUN = function(x, y) x - y)
  disty <- outer(y, y, FUN = function(x, y) x - y)
  
  rij3 <- (distx^2 + disty^2)^(3/2)
  
  fx <- starMass.plei * distx / rij3
  fy <- starMass.plei * disty / rij3
  
  list(c(dx = u,
         dy = v,
         du = colSums(fx, na.rm = TRUE),
         dv = colSums(fy, na.rm = TRUE)))
}

starMass.plei <- 1:7

yini.plei <- c(x1= 3, x2= 3, x3=-1, x4=-3,    x5= 2, x6=-2,   x7= 2,
               y1= 3, y2=-3, y3= 2, y4= 0,    y5= 0, y6=-4,   y7= 4,
               u1= 0, u2= 0, u3= 0, u4= 0,    u5= 0, u6=1.75, u7=-1.5,
               v1= 0, v2= 0, v3= 0, v4=-1.25, v5= 1, v6= 0,   v7= 0)

out.plei <- ode(func = pleiade, parms = NULL, y = yini.plei, method = "adams", times = seq(0, 3, 0.01))
out.plei <- as.data.frame(out.plei)

out.plei.all <- as.data.frame(out.plei[,c(2:8, 9:15)])


#-------------------------------------
# Mandelbrot 2D
# http://users.utu.fi/attenka/mandelbrot_set.R
#-------------------------------------
Limits=c(-2,0.8)
MaxIter=25
cl=colours()
Step=seq(Limits[1],Limits[2],by=0.005)
S=floor(length(cl)/MaxIter)
Dist=0
PointsMatrix=array(0,dim=c(length(Step)*length(Step),3))
t=0

for(a in Step)
{
  for(b in Step+0.6)
  {
    x=0;y=0;n=0;Dist=0
    while(n<MaxIter & Dist<4)
    {
      n=n+1
      newx=a+x^2-y^2
      newy=b+2*x*y
      Dist=newx^2+newy^2
      x=newx;y=newy
    }
    if(Dist<4) colour=24 # black colour
    else colour=n*S
    t=t+1
    PointsMatrix[t,]=c(a,b,colour)
  }
}

mandel.new.df <- as.data.frame(PointsMatrix)
names(mandel.new.df) <- c('X','Y','color')
#dim(mandel.new.df)
#314721 3

#-------------------------------------
# Julia
# http://users.utu.fi/attenka/julia_set.R
#-------------------------------------
a=-0.7;b=-0.4 # Complex parameter, connected to coordinate of the Mandelbrot set in a complex plane. Constants here.
Limits=c(-2,2)
MaxIter=60
cl=colours()
Step=seq(Limits[1],Limits[2],by=0.01)
JuliaMatrix=array(0,dim=c(length(Step)*length(Step),3))
a1=0
#plot(0,0,xlim=Limits,ylim=Limits,col="white")

for(x in Step)
{
  for(y in Step)
  {
    n=0
    DIST=0
    x1=x;y1=y # Original x and y are saved.
    while(n<MaxIter & DIST<4)
    {
      newx=x1^2-y1^2+a
      newy=2*x1*y1+b
      DIST=newx^2+newy^2
      x1=newx;y1=newy
      n=n+1
    }
    if(DIST<4) colour=24 else colour=n*10
    a1=a1+1
    JuliaMatrix[a1,]=c(x,y,colour)
  }
}

julia.new.df <- as.data.frame(JuliaMatrix)
names(julia.new.df) <- c('X','Y','color')

#------------------------------------------------
#------- Pleaide - 4 Stars
pleiade <- function (t, Y, pars) {
  x <- Y[1:4]
  y <- Y[5:8]
  u <- Y[9:12]
  v <- Y[13:16]
  
  distx <- outer(x, x, FUN = function(x, y) x - y)
  disty <- outer(y, y, FUN = function(x, y) x - y)
  
  rij3 <- (distx^2 + disty^2)^(3/2)
  
  fx <- starMass * distx / rij3
  fy <- starMass * disty / rij3
  
  list(c(dx = u,
         dy = v,
         du = colSums(fx, na.rm = TRUE),
         dv = colSums(fy, na.rm = TRUE)))
}

#starMass <- rep(1,4)
starMass <- c(1,1,1,1)

yini<- c(
  x1= 0, x2= 1, x3= 1, x4= 0,    
  y1= 0, y2= 0, y3= 1, y4= 1,  
  u1= 0.999, u2= 1, u3= -1.04, u4= -1,
  v1= -1, v2= 1.005, v3= 1, v4=-1.02
)
out.4st <- ode(func = pleiade, parms = NULL, y = yini, method = "adams", times = seq(0, 45, 0.01)) 
out.plei.4df <- as.data.frame(out.4st)
out.plei.4all <- out.plei.4df[,c(2:5,6:9)]




#------------------------------------------------
#------- Catalitic System

#Catalitic Network

#Tiempo de paso
dt<-0.001

#Concentraciones iniciales de las sustancias, moleculas, virus o cuya replicacion 
#se modelice en cada caso
x0<-100
y0<-75
z0<-110
s0<-115

#Concentraciones en una matriz
X<-matrix(c(x0,y0,z0,s0))
Xt<-X




#Constantes que controlan los procesos de replicacion de cada molécula.

#N es la concentración total de las cuatro moléculas que se mantiene fija en el sistema
N<-400

#Velocidad de replicacion no catalítica, en cada paso de tiempo cada molécula se replica
#con una velocidad A. Esta es una tasad de replicacion a nivel de molécula. Para todo el 
#sistema la concentracion cambia en funcion de A y de la cantidad de moléculas de una 
#especie que hay. Esto es XA. 
A<-1

#Ae representa la replicacion que da lugar a elementos que no se consideran en el sistema 
#o que salen
Ae<-0.8

#Tasa de acierto en la replicacion, 
#la replicacion fallida tiene una probabilidad de 1-Q
Q<-0.8



##PROCESOS CATALÍTICOS

#Replicacion autocatalítica correcta
#La velocidad de replicacion de cada molécula es proporcional al cuadrado de su
#concentracion. O lo que es lo mismo el producto de la concentracion Xi por si misma Xi*Xi.


#Al haber replicaciones fallidas, la molécula 1 aparece por la replicacion fallida
#de 2,3 y 4. Y a su vez, cuando se replica mal, da lugar esos mismos elementos. 
#Se considera que la replciacion fallida tambien tiene  comportamiento
#catalítico. De forma que la velocidad con que aparece 2 por replicacion fallida de 
#1 es mayor cuanto mayor es el producto de la concentracion de 1 y 2. 


#Las constantes de los procesos catlíticos (Relacionan productos de concentraciones
#y velocidades de replicación) se indican por Kij. Kij no tiene por que se igual que 
#Kji, ya que la velocidad de aparicion de i por la mala replicacion de j no tiene 
#porque ser igual a la velocidad e aparicion de j por la mala replicacion de i

mu <- 0
K11<-0.5+mu
K22<-1
K33<-0.6
K44<-0


K21<-1.6
K31<-0
K41<-2.2
K12<-1.5
K32<-2
K42<-0
K13<-0.5
K23<-0
K43<-0.4
K14<-0.1
K24<-0
K34<-0



#Matriz del proceso catalítico
K<-matrix(c(K11,K12,K13,K14,
            K21,K22,K23,K24,
            K31,K32,K33,K34,
            K41,K42,K43,K44),4,4,byrow=TRUE)


#Generacion de la trayectoria para 10000 pasos de tiempo
for(i in 1:10000){
  
  
  
  #	A partir de la ecuacion 7 del paper complex dynamics of catalytic network..
  
  A<-K%*%X
  
  B<--sum(X)*((1-Ae)/N)
  
  C<-as.vector(-(1/N)*(t(X)%*%K%*%X))
  
  DX<-X*A+X*B+X*C
  
  X<-X+DX*dt
  
  Xt<-cbind(Xt,X)
  
  #	if(ya*y[length(y)]<0){
  #		
  #		ya<-y[length(y)]
  #		tempo<-c(tempo,i*0.01)
  #		duration<-c(duration,0.05)
  #		
  #	}
  
  
}
catal.df <- as.data.frame(Xt)
catal.df.2d <- catal.df
catal.df.2d$cat2d <- rep(0,dim(catal.df.2d)[1])
