library(imager)
library(ggplot2)
library(reshape2)
bb<-grayscale(boats)
bb1<-as.data.frame(bb)
ggplot(bb1,aes(x,y))+geom_raster(aes(fill=value))+scale_fill_gradient(low="black",high="white")+scale_y_continuous(trans=scales::reverse_trans())+ggtitle("Original")+theme_classic()


bb2<-matrix(bb, nrow=256, ncol=384)
svbb1<-svd(bb2)
combb<- (svbb1$u[,1:10] %*% diag(svbb1$d[1:10]) %*% t(svbb1$v[,1:10]))
j10<-melt(combb, id.vars = c("x", "y"))
ggplot(j10,aes(X1,X2))+geom_raster(aes(fill=value))+ scale_fill_gradient(low="black",high="white")+scale_y_continuous(trans=scales::reverse_trans())+ggtitle("10 Columns")+theme_classic()

combb<- (svbb1$u[,1:50] %*% diag(svbb1$d[1:50]) %*% t(svbb1$v[,1:50]))
j50<-melt(combb, id.vars = c("x", "y"))
ggplot(j50,aes(X1,X2))+geom_raster(aes(fill=value))+ scale_fill_gradient(low="black",high="white")+scale_y_continuous(trans=scales::reverse_trans())+ggtitle("50 Columns")+theme_classic()

combb<- (svbb1$u[,1:100] %*% diag(svbb1$d[1:100]) %*% t(svbb1$v[,1:100]))
j100<-melt(combb, id.vars = c("x", "y"))
ggplot(j100,aes(X1,X2))+geom_raster(aes(fill=value))+ scale_fill_gradient(low="black",high="white")+scale_y_continuous(trans=scales::reverse_trans())+ggtitle("100 Columns")+theme_classic()

combb<- (svbb1$u[,1:200] %*% diag(svbb1$d[1:200]) %*% t(svbb1$v[,1:200]))
j200<-melt(combb, id.vars = c("x", "y"))
ggplot(j200,aes(X1,X2))+geom_raster(aes(fill=value))+ scale_fill_gradient(low="black",high="white")+scale_y_continuous(trans=scales::reverse_trans())+ggtitle("200 Columns")+theme_classic()



