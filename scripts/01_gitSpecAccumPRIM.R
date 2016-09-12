# Species Accumulation Script
# 7.6.2016 SB

setwd("~/GitHub/cred_fish")
load("data/TMPwsd.Rdata")

library(vegan)
library(reshape)
library(ggplot2)

if (!require("RColorBrewer")) {
  install.packages("RColorBrewer")
  library(RColorBrewer)
}
display.brewer.all()

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
Pal <- subset(matrix, ISLAND == "Palmyra")[,3:ncol(matrix)]
Kin <- subset(matrix, ISLAND == "Kingman")[,3:ncol(matrix)]
Jar <- subset(matrix, ISLAND == "Jarvis")[,3:ncol(matrix)]
Joh <- subset(matrix, ISLAND == "Johnston")[,3:ncol(matrix)]
Bak <- subset(matrix, ISLAND == "Baker")[,3:ncol(matrix)]
How <- subset(matrix, ISLAND == "Howland")[,3:ncol(matrix)]
Wak <- subset(matrix, ISLAND == "Wake")[,3:ncol(matrix)]

# Species accumulation
Pal.sp <- specaccum(Pal, method = "random")
Kin.sp <- specaccum(Kin, method = "random")
Jar.sp <- specaccum(Jar, method = "random")
Joh.sp <- specaccum(Joh, method = "random")
Bak.sp <- specaccum(Bak, method = "random")
How.sp <- specaccum(How, method = "random")
Wak.sp <- specaccum(Wak, method = "random")

## Plot curves showing 95% confidence interval and save

jpeg(file = "graphs_tables/PRIMSpecAccum_CI.jpg")

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

dev.off()

## Plot with ggplot showing standard deviation and save

# Create data frames for all the results, including island 
sp.p <- data.frame(Sites=Pal.sp$sites, Richness=Pal.sp$richness, SD=Pal.sp$sd, ISLAND = "Palmyra")
sp.k <- data.frame(Sites=Kin.sp$sites, Richness=Kin.sp$richness, SD=Kin.sp$sd, ISLAND = "Kingman")
sp.j <- data.frame(Sites=Jar.sp$sites, Richness=Jar.sp$richness, SD=Jar.sp$sd, ISLAND = "Jarvis")
sp.jh <- data.frame(Sites=Joh.sp$sites, Richness=Joh.sp$richness, SD=Joh.sp$sd, ISLAND = "Johnston")
sp.b <- data.frame(Sites=Bak.sp$sites, Richness=Bak.sp$richness, SD=Bak.sp$sd, ISLAND = "Baker")
sp.h <- data.frame(Sites=How.sp$sites, Richness=How.sp$richness, SD=How.sp$sd, ISLAND = "Howland")
sp.w <- data.frame(Sites=Wak.sp$sites, Richness=Wak.sp$richness, SD=Wak.sp$sd, ISLAND = "Wake")

# Merge all data frames 
sp.all <- rbind(sp.p, sp.k, sp.j, sp.jh, sp.b, sp.h, sp.w)

#colorRampPalette(brewer.pal(4,"Dark2"))(7) #This will generate 4 colours based on the 4 from the ‘Blues’ palette
#cols = c("#1B9E77", "#7A7E3C", "#D95F02", "#A6675A", "#7570B3", "#AD4C9E", "#E7298A")

# Plot all islands showing standard deviation - colors are hard to read here - need to change them at some point
ggplot(data = sp.all, aes(x = Sites, y = Richness, fill = ISLAND)) +
  geom_line(aes(fill = ISLAND)) + 
  geom_ribbon(data = sp.all, aes(x = Sites, ymin=(Richness-2*SD),ymax=(Richness+2*SD)),alpha=0.3) + 
  theme_bw() + 
  ggtitle("Randomized Species Accumulation Curves By Island") + 
  scale_fill_discrete(name="Island") +
  #theme(legend.position="bottom") +
  ylab("Fish Species Richness") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.title=element_blank()) +
  theme(text = element_text(size = 18))+
  theme(legend.position=c(.85,0.25))

# Save plot to folder in GitHub
ggsave("graphs_tables/PRIMSpecAccum_SD_largetext.jpg")



