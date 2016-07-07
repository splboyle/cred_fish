# Species Accumulation Script
# 7.6.2016 SB

library(vegan)
library(reshape)

# Subset species abundance matrix 
island_matrix <- wsd[c(3,6,37:684,1344)]
matrix.onlysp <- island_matrix[-c(1,2,651)]
site.info <- data.frame(wsd$ISLAND, wsd$SITE)
colnames(site.info) <- c("ISLAND", "SITE")

# Call island out for legend later
uIsl <- unique(island_matrix$ISLAND)

# Transform to presence absence matrix
islands_pa <- ifelse(matrix.onlysp>0, 1, 0)
matrix <- cbind(site.info, islands_pa)

# Subset by island 
Pal <- subset(matrix, ISLAND == "Palmyra")[,3:ncol(Pal)]
Kin <- subset(matrix, ISLAND == "Kingman")[,3:ncol(Pal)]
Jar <- subset(matrix, ISLAND == "Jarvis")[,3:ncol(Pal)]
Joh <- subset(matrix, ISLAND == "Johnston")[,3:ncol(Pal)]
Bak <- subset(matrix, ISLAND == "Baker")[,3:ncol(Pal)]
How <- subset(matrix, ISLAND == "Howland")[,3:ncol(Pal)]
Wak <- subset(matrix, ISLAND == "Wake")[,3:ncol(Pal)]

# Species accumulation
Pal.sp <- specaccum(Pal, method = "random")
Kin.sp <- specaccum(Kin, method = "random")
Jar.sp <- specaccum(Jar, method = "random")
Joh.sp <- specaccum(Joh, method = "random")
Bak.sp <- specaccum(Bak, method = "random")
How.sp <- specaccum(How, method = "random")
Wak.sp <- specaccum(Wak, method = "random")

# Plot curves
plot(Pal.sp, col = "purple", xlim=c(0,200), main="Randomized Accumulation Curves\nby Island", ylim=c(0,300), ylab="Number of Species",xlab="Number of Sites", lwd=0.1,ci.type='polygon', ci.lty=2,ci=1.96/max(sqrt(Pal.sp$sites)))
plot(Kin.sp,col="orange", add=TRUE,lwd=0.1,ci.type='polygon',ci.lty=2,ci=1.96/max(sqrt(Kin.sp$sites)))
plot(Jar.sp,col="green",add=TRUE,lwd=0.1,ci.type='polygon',ci.lty=2,ci=1.96/max(sqrt(Jar.sp$sites)))
plot(Joh.sp,col="blue",add=TRUE,lwd=0.1,ci.type='polygon',ci.lty=2,ci=1.96/max(sqrt(Joh.sp$sites)))
plot(Bak.sp,col="yellow",add=TRUE,lwd=0.1,ci.type='polygon',ci.lty=2,ci=1.96/max(sqrt(Bak.sp$sites)))
plot(How.sp,col="red",add=TRUE,lwd=0.1,ci.type='polygon',ci.lty=2,ci=1.96/max(sqrt(How.sp$sites)))
plot(Wak.sp,col="cyan",add=TRUE,lwd=0.1,ci.type='polygon',ci.lty=2,ci=1.96/max(sqrt(Wak.sp$sites)))
legsort=c(1,2,3,4,5,6,7)
legend(150,150,legend=(uIsl[legsort]),
       fill = (c("cyan","blue","red","yellow","green","purple", "orange")[legsort]),
       cex = 0.75)

