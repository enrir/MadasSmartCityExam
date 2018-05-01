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

names(network) <- gsub(x = names(network),
                        pattern = "-",
                        replacement = "_")

#manage edge list according to directions

#subset two way same ch

network_2way = network[DIR_CODE==0, .(A_NODE=B_NODE, B_NODE=A_NODE, DIST, LINK_TYPE_A_B=LINK_TYPE_B_A, 
                       CAP_INDEX_A_B=CAP_INDEX_B_A, 
                       SPEED_A_B=SPEED_B_A, CAPACITY_A_B=CAPACITY_B_A, DIR_CODE)]

network[, c('LINK_TYPE_B_A', 'CAP_INDEX_B_A', 'SPEED_B_A', 'CAPACITY_B_A'):=NULL]

network_complete = rbind(network, network_2way)
#drop vertices without coordinates


#vertex without links

net_edgelist = c(network_complete$A_NODE,network_complete$B_NODE)

coord_complete = coord[(NODE %in% net_edgelist),]

# Load the network

# node 375 is missing from coord table
# node 375 has links with only two other real point
cord375E = colMeans(coord[NODE %in% c(643, 2680), .(COORD_X, COORD_Y)])

coord_complete = rbind(coord_complete,
                data.frame("NODE" = 375, "COORD_X" = cord375E[1], "COORD_Y" = cord375E[2], "NAME" = NA))

#network_complete = merge(network_complete, )

tr_net<-graph_from_data_frame(vertices = coord_complete, d= network_complete, directed = TRUE)

V(tr_net)$color <- ifelse(names(V(tr_net)) < 18, "lightblue", "gray")

plot.igraph(tr_net,layout=as.matrix(coord_complete[,2:3]),
            vertex.color=adjustcolor("gray",alpha.f=0.5), vertex.size=3, vertex.label=NA,
            link=adjustcolor("red",alpha.f=0.5), edge.width=1,
            edge.arrow.size=.1)

#real zones

realz<- network$B_NODE[network$A_NODE %in% 1:17]
names<-coord$NAME[1:17]
coord$NAME[1:17]<-NA
for (i in 1:18){
  coord$NAME[coord$NODE==realz[i]]<-names[i]}

E(tr_net)$TIME_min <-  E(tr_net)$DIST / E(tr_net)$SPEED_A_B / 60
E(tr_net)$LOAD <- 0

#prova con zone originali

short_path <- shortest_paths(tr_net, matOD[VEH_H>0,]$ZONE_O, matOD[VEH_H>0,]$ZONE_D, mode = 'out',
               weights = E(tr_net)$TIME_min, output = "both",
               predecessors = FALSE, inbound.edges = FALSE)

#fare loop sulle shortpath aumentanto E attr load con il numero di veicoli assegnati
for (i in short_path$epath){print(i)}

#ricalcolare cost/speed ?
# capire cap index
