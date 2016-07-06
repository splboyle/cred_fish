# Species Accumulation Script

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
matrix <- cbind(islands_pa, site.info)

# Species accumulation 


#subsetting each region to create a species accum. curve
## remember to identify only the columns that are being assessed in the 
# example file, the species starts at column 3 and I'm telling it to go to the end
# of the dataframe columns

as=subset(cr,Region=="SAMOA")[,3:ncol(cr)]
spas=specaccum(as,method=spmethod)

# 
Pal.sp.random <- specaccum(Pal.mx.prab, method = "random")
King.sp.random <- specaccum(King.mx, method = "random")
Jar.sp.random <- specaccum(Jar.mx, method = "random")
Joh.sp.random <- specaccum(Joh.mx, method = "random")
Bak.sp.random <- specaccum(Bak.mx, method = "random")
How.sp.random <- specaccum(How.mx, method = "random")
Wak.sp.random <- specaccum(Wak.mx, method = "random")
