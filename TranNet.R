## Smart city Exam

rm(list=ls())
gc()

library(data.table)
library(igraph)

# Import the data

library(readxl)
datamap <- as.data.table(read_excel("Transportmodeling.xlsx", 
                           sheet = "DATAMAP"))
View(datamap)
network <- as.data.table(read_excel("Transportmodeling.xlsx", 
                      sheet = "NETWORK"))
View(network)
coord <- as.data.table(read_excel("Transportmodeling.xlsx", 
                      sheet = "COORD"))
View(coord)
speedFlow <- as.data.table(read_excel("Transportmodeling.xlsx", 
                    sheet = "SPEED-FLOW"))
View(speedFlow)

matOD <- as.data.table(read_excel("Transportmodeling.xlsx", 
                                      sheet = "MAT_OD"))
View(matOD)
#drop vertices without coordinates

#real zones

realz<- network$B_NODE[network$A_NODE %in% 1:17]
names<-coord$NAME[1:17]
coord$NAME[1:17]<-NA
for (i in 1:17){
coord$NAME[coord$NODE==realz[i]]<-names[i]}

tr_net<-graph_from_data_frame(as.matrix(network), directed = TRUE, vertices=coord)

tr_net<-graph.data.frame(as.matrix(network[, .(A_NODE, B_NODE)]), directed = FALSE)

plot.igraph(tr_net,layout=as.matrix(coord[,2:3]),
            vertex.color=adjustcolor("gray",alpha.f=0.5), vertex.size=3, vertex.label=NA,
            link=adjustcolor("red",alpha.f=0.5), edge.width=3,
            edge.arrow.size=4)


