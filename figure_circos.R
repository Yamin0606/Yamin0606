#figure 4 and table S6 in the paper
#figure 4 Graphic representation of the number of included publications that consider each specific biomarker-socioenvironmental link 
#table S6. Number of studies investigating association between different socioenvironmental factors and biomarkers

library(tidyr);library(circlize);library(ggplot2);library(reshape2)
setwd("/Users/yaminzhang/Nutstore\ Files/文章筛选/documents\ for\ final\ review/analysis")
##input
sa<- xlsx::read.xlsx2("statistics.xlsx",sheetName = "sa")
biom <- xlsx::read.xlsx2("statistics.xlsx",sheetName = "biom")

df <- read.table("data_da-biom.txt",header = T)
df_table_eng <- table(df$SA,df$BIOM)
row.names(df_table_eng)[11] <- "k"

df <- read.table("data_da-biom_chi.txt",header = T)
df[grep("other",df$BIOM),"BIOM"] <- "o"
df_table_chi <- table(df$SA,df$BIOM)
df_table_chi <- rbind(df_table_chi[1:10,],apply(df_table_chi[c("l2","m"),],2,sum));
row.names(df_table_chi)[11] <- "k"

df_table <- df_table_chi + df_table_eng  #11 socio-environmental factors x 14 biomarker categories
data <- data.frame(from = rep(biom$biom_abbr, each = nrow(sa)),
                   to = rep(sa$sa_abbr[1:11], times = nrow(biom)),
                   value = c(df_table),
                   stringsAsFactors = FALSE)
#conbine other molecule, epigenetics, and GxE
omol_n <- data[data$from=="other","value"]+data[data$from=="Omol","value"]
num <- length(unique(data$to))
aa0 <- data[c(1:(7*num),(11*num+1):(12*num),(7*num+1):(11*num),(12*num+1):(13*num)),]#adjust the position (1:7,12,8:11,13)
aa0[aa0$from=="Omol","value"] <- omol_n
aa0[aa0$from=="Oorg","from"] <- "microbe"
data<- aa0[,c(2,1,3)] #switch the direction
names(data) <- names(aa0)
##
set.seed(13)
grid.col = rand_color(length(unique(data[[1]]))+length(unique(data[[2]])))
grid.col[12:24] <- rep("#00000000",13) #remove the color for 
circos.clear()
circos.par(gap.after = c(rep(5, length(unique(data[[1]]))-1), 10, 
                         rep(5, length(unique(data[[2]]))-1), 10))
#png("chord_all.png", width = 700,height = 700) #resolution is bad
chordDiagram(data,grid.col=grid.col)

#highlight
col <- c()
for (i in 1:11){
  col <- c(col,rep(grid.col[i],13))
}
col1=col
for (i in 1:11){
  col1=col
  col1[data$from!=unique(data[[1]])[i]] <- "#00000000"
  sa=unique(data$from)[i]
  png(paste(sa,"_chord_highlighted.png"), width = 700,height = 700) 
  chordDiagram(data, grid.col = grid.col, col=col1)
  dev.off()
}

#table s6
df_table <- df_table_chi + df_table_eng 
df_table[,12] <- df_table[,12]+df_table[,14]
df_table
df_table <- df_table[,1:13] #11 socio-environmental factors x 13 biomarker categories
