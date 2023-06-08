library (lattice)
library (Matrix) 
library (sp)
library (spgwr)
library (maptools) 
xx<-readShapePoints ("D:/kemiskinan1.shp"[1]} 
хх
col.b.adapt<-
gwr.sel (Y1~X1+X2, data=xx, coords=cbind(xx$X, xx$Y), adapt=TRUE) adapt.gauss<- 
gwr (Y1~ X1+X2, data=xx, coords=cbind(xx$X, xx$Y), adapt=col.b.adapt, hatmatrix=TRUE)
names (adapt.gauss) h=adapt.gauss$bandwidth
h<-as.matrix(x)
b=h
for (i in 1:37) (b<-cbind(b,h)} 
b
je<-read.table("D:/je.txt")

w=function (b, je, i) {
b<-b[, i]
je<-je [, i]
k=je/b
a=k^2
c=(-0.5) *a
w-exp (c)
}

W1<- w (b, je,1)
W2<- w (b, je, 2)
W3<- w (b, je, 3)
W4<- w (b, je, 4)
W5<- w (b, je, 5)
W6<- w (b, je, 6)
W7<- w (b, je, 7)
W8<- w (b, je, 8)
W9<- w (b, je, 9)
W10<- w (b, je, 10)
W11<- w (b, je, 11)
W12<- w (b, je, 12)
W13<- w (b, je, 13)
W14<- w (b, je, 14)
W15<- w (b, je, 15)
W16<- w (b, je, 16)
W17<- w (b, je, 17)
W18<- w (b, je, 18)
W19<- w (b, je, 19)
W20<- w(b, je, 20)
W21<- w (b, je, 21) 

W22<- w (b, je, 22)
W23<- w (b, je, 23)
W24<- w (b, je, 24)
W25<- w (b, je, 25)
W26<- w (b, je, 26)
W27<- w (b, je, 27)
W28<- w (b, je, 28)
W29<- w (b, je, 29)
W30<- w (b, je, 30)
W11<- w (b, je, 31)
W32<- w (b, je, 32)
W33<- w (b, je, 33)
W34<- w (b, je, 34)
W35<- w (b, je, 35)
W136<- w (b, je, 36)
W37<- w (b, je, 37)
W38<- w (b, je, 38) 


W<-

rbind (W1, W2, W3, W4, W5, W6, W7, W8, W9, W10, W11, W12, W13, W14, W15, W16, W17, W18, W19, W20, W21, W22, W23, W24, W25, W26, W27, W28, W29, W30, W31, W32, W33, W34, W35, W36, W37, W38)

X<- read.table("D:/datax.txt")
Y<-read.table("D:/datay.txt")
olahdata = function (data, jumlahpengamatan, jumlahvariabel) {
data<-an.matrix(data) 
jumlahrow-nrow (data)
batas = jumlahpengamatan*jumlaltrow
X0= matrix (1, batas,1) 
data<-as.vector (data)
X<-matrix (data, batas, jumlahvariabel)
X<-cbind(X0,x)
}

X <- olahdata (X, 5, 2)
Y <- olahdata (Y, 5, 1)
Y < Y(, -1)
Y <- as.matrix(Y)

beta=function (X, Y, W, i) {
W<- W [, i ]
W<-as.matrix (W) 
jumlahrow=nrow (W)
pembobot=W 
for (i in 1:4) (pembobot <-rbind (pembobot, W)} pembobot
p1<-diag(190) 
diag (p1) <-pembobot 
p1

,beta37,beta38) 
Beta
tbeta=t (beta) 
tbeta
betaintersep =tbeta[, 1]
betaLPE=tbeta[, 2]
betaTPT=tbeta[, 3] 
min (betaintersep)
min (betaLPE)
min (betaTPT) 
max (betaintersep)
max (betaLPE)
max (betaTPT) 
rata0=sum (betaintersep)/38
rata0
rata1=sum (betaLPE) / 38 
rata1
rata2 =sum (betaTPT) / 38 
rata2
akarc=function (X, Y, W, i) f
W<-W [, i]
W<-as.matrix (W)
jumlahrow=nrow (W)
pembobot=w
for (i in 1:4) {pembobot<-rbind{pembobot, W}
pembobot 
p1<-diag (190)
diag (p1)<-pembobot
p1
W<-as.matrix (p1)
a = t(X) . W
b = solve (a . X)
C = b . a

CCt= c . t (C )
CCt
c = diag(CCt)
akarc = t(sqrt (c))
akarc = aa.matrix (akarc)
akarc
}

akarc1<-akarc (X, Y, W, 1)
akarc2<-akarc (X,Y, W, 2)
akarc3<-akarc (X, Y, W, 3)
akarc4<-akarc (X, Y,W, 4)
akarc5<-akarc (X, Y, W, 5)
akarc6<-akare (X, Y, W, 6)
akare?<-akarc (X, Y, W, 7)
akarc8<-akarc (X, Y, W, 8)
akare9<-akare (X, Y, W, 9)
akarc10<-akarc (X, Y, W, 10)
akarcii<-akarc (X, Y, W, 11)
akarc12<-akarc (X, Y, W, 12)
akarc13<-akarc (X, Y, W, 13)
akarc14<-akarc (X, Y, W, 14)
akarc15<-akarc (X, Y, W, 15)
akarc16<-akarc (X, Y, W, 16)
akarc17<-akarc (X, Y, W, 17)
akare18<-akarc (X, Y, W, 18)
akarc19<-akarc (X, Y, W, 19)
akarc20<-akarc (X, Y, W, 20)
akarc21<-akarc (X, Y, W, 21)
akarc22<-akarc (X,Y, W, 22)
akarc23<-akarc (X, Y, W, 23)
akare24<-akarc (X, Y, W, 24)
akare25<-akarc (X, Y, W, 25)
akarc26<-akarc (X, Y, W, 26)
akare27<-akarc (X, Y,W,27)
akarc28<-akarc (X, Y,W,28)
akarc29<-akarc (X, Y, W, 29)
akarc30<-akarc (X, Y, W, 30)
akare31<-akarc (X, Y, W, 31)
akarc32<-akarc (X, Y, W, 32)
akarc33<-akarc (X, Y, W, 33)
akarc34<-akarc (X, Y, W, 34)
akarc35<-akarc (X, Y, W, 35)
akarc36<-akarc (X, Y, W, 36)
akarc37<-akarc (X, Y, W, 37)
akarc38<-akarc (X, Y, W, 38))

akarc<- आ rbind (akarc1, akarc2, akarc3, akarc4, akarc5, akarc6, akarc7, akarc8, akarc9, akare10, akarc11, akarc12, akarc13, akarc14, akarc15, akarc16, akarc17, akarc18, akarc19, akarc20, akarc21, akarc22, akarc23, akarc24, akarc25, akarc26, akarc27, akarc28, akarc29, akarc30, akarc31 ,akarc32, akarc33, akarc34, akare35, akarc36, akarc37, akarc38) akarc

m1=function (X, Y, W, 1, j). {
w<-w[,1]
W<-as.matrix (W)
jumlahrow=nrow (W)
pembobot=W 
for (i in 1:4) (pembobot<-rbind (pembobot, W) }
pembobot
pl<-diag (190)
diag (pl) <-pembobot
pl

W<-as.matrix (pl) 
a = t (X)  * W 
b = solve (a . X )
c = b . a
Xt=t (x)

L = Xt [, j] . C 
}

L1=ml (X, Y, W, 1, 1)
L2=ml (X, Y, W, 2, 2)
L3=ml (X, Y, W, 3,3)
L4=ml (X, Y, W, 4, 4)
L5=ml (X, Y, W, 5, 5)
L6=ml (X, Y, W, 6, 6)
L7=ml (X, Y, W, 7, 7)
L8=ml (X, Y, W, 8, 8)
L9=ml (X, Y, W, 9, 9)
L10=ml (X, Y, W, 10, 10)
L11=ml (X, Y, W, 11,11)
L12=mi (X,Y, W, 12, 12)
L13=ml (X, Y, W,13,13)
L14=ml (X, Y, W, 14, 14)
L15=ml (X,Y, W, 15, 15)
L16=ml (X, Y, W, 16,16)
L17=ml (X, Y, W,17,17)
L18=ml (X, Y, W,18,18)
L19=ml (X, Y, W,19,19)
L20=ml (X, Y, W, 20, 201
L21=ml (X, X, W, 21, 21)
L22=ml (X, Y, W, 22, 22)
L23=ml (X, Y, W, 23, 23)
L24=ml (X, Y, W, 24, 24)
L25=ml (X, Y, W, 25, 25)
L26=ml (X, Y, W, 26, 26)
L27=ml (X, Y, W, 27,27)
L28=ml (X, Y, W, 28,28)
L29=ml (X, Y, W, 29, 29)
L30=ml (X, Y, W, 30, 30)
L31=ml (X, Y, W, 31,31)
L32=ml (X, Y, W, 32, 32)
L33=ml (X, Y, W, 33, 33)
L34=ml (X, Y ,W, 34, 34)
L35=ml (X,Y, W, 35, 35)
L36=ml (X,Y, W, 36, 36)
L37=ml (X, Y, W, 37,37)
L38=ml (X, Y, W, 38, 38)
L39=ml (X, Y, W, 39, 39)
L40=ml (X, Y, W, 40,40)
L41=m1 (X, Y, W, 41, 41)
L42=ml (X, Y, W, 42,42)
L43=ml (X, Y, W, 43, 43)
L44=ml (X, Y, W, 44, 44)
L45=ml (X, Y ,W, 45, 45)
L46=ml (X, Y, W, 46, 46) 


L108, L109, L110, L111, L112, L113, L114, L115, L116, L117, L118, L119, L120, L121, L173, L174, L175, L176, L177, L178, L179, L180, L181, L182, L183, L184, L185, L186, L187
L

ci = array (1, dim = c (190,1)) 
I = diag (190)
diag (1) <-ci
I
f = I - L
g = t (f) %*% f
 g <-as.matrix(g)
si1 = sum (diag(g))
si1
j = g^2
si2 = sum (diag(j))
si2
u = t (x)%*%X
V = solve(u)
H = X%*%V%*%t(x)
V - I - H
JKGH1 - t (Y)%*%V%*%Y
JKGH1
sigmatopi = sqrt (JKGH1/si1) 
sigmatopi

sigmatopi - rbind (sigmatopi,sigmatopi,signatopi)
sigmatopi
tsigmatopi = t(sigmatopi)
ts = tsigmatopi
for (i in 1:37) (ts< - rbind(ts, tsigmatopi))
ts

salahbaku = ts akarc
salahbaku
t = tbeta/salahbaku
t

ratay = sum (Y)/190

tabeta = tbeta
for (i in 1:4) (tabeta<-rbind(tabeta, tbeta))

Ytopi = tabeta*X 
Ytopi
Y1< - Ytopi (, 1)
Y1< - as.matrix(Y1)

Y2<-Ytopi (, 2)
Y2<-as.matrix (Y2)
Y3<-Ytopi (,3)
13<-as.matrix (Y3)
Ytopi=Y1+Y2+Y3
Ytopi

11 = (Y- Ytopi)^2
111 = 11
for (i in 1:189) (111<- cbind(111, 111))
W = as.matrix (W)
p=w
for (i in 1:4) (p<-cbind(p, W))
w=p
for (i in 1:4) (<-rbind (w,p))
kl=sum (W*111)

jil = ( Y - ratay)^2
j i l = j l
for (1 in 1:189) (jil<-rbind(j i l, j l) }
hl=sum (W*jil)

Rkuadrat=kl/hl 
Rkuadrat 
