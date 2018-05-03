## Smart city Exam

rm(list=ls())
gc()

library(data.table)
library(igraph)
library(stringr)
# Import the data

library(readxl)
datamap <- as.data.table(read_excel("Transportmodeling.xlsx", 
                           sheet = "DATAMAP"))
network <- as.data.table(read_excel("Transportmodeling.xlsx", 
                      sheet = "NETWORK"))
coord <- as.data.table(read_excel("Transportmodeling.xlsx", 
                      sheet = "COORD"))
speedFlow <- as.data.table(read_excel("Transportmodeling.xlsx", 
                    sheet = "SPEED-FLOW"))

speedFlow[, SPEED_0:=0]
#melt speedflow
speedFlow<- melt(speedFlow, id=1, measure.vars = c(2:7), 
                 variable.name = 'load_left', value.name = 'speed_red')[,
            load_left := as.numeric(str_extract(load_left, "([0-9])"))/10] 

setorder(speedFlow, CAP_INDEX, load_left)

speedFlow[, load_right:=shift(load_left,1, type = 'lead'), by=CAP_INDEX]
speedFlow[is.na(speedFlow)]<- 10

#tail(speedFlow[(CAP_INDEX==4 & variable<=0.6),.(value)],1)


matOD <- as.data.table(read_excel("Transportmodeling.xlsx", 
                                      sheet = "MAT_OD"))

matOD[, c('VEH_50', 'VEH_30', 'VEH_20'):= .(ceiling(VEH_H*0.5), round(VEH_H*0.3), VEH_H - round(VEH_H*0.3) - ceiling(VEH_H*0.5))]

names(network) <- gsub(x = names(network),
                        pattern = "-",
                        replacement = "_")

#manage edge list according to directions

#subset two way link

network_2way = network[DIR_CODE==0, .(A_NODE=B_NODE, B_NODE=A_NODE, DIST, LINK_TYPE=LINK_TYPE_B_A, 
                       CAP_INDEX=CAP_INDEX_B_A, 
                       SPEED=SPEED_B_A, CAPACITY=CAPACITY_B_A, DIR_CODE)]

network[, c('LINK_TYPE_B_A', 'CAP_INDEX_B_A', 'SPEED_B_A', 'CAPACITY_B_A'):=NULL]
names(network)<- c("A_NODE", "B_NODE", "DIST", "LINK_TYPE", "CAP_INDEX",
                   "SPEED",     "CAPACITY",  "DIR_CODE")
network_complete = rbind(network, network_2way)

#drop vertex without links

net_vertexlist = c(network_complete$A_NODE,network_complete$B_NODE)

coord_complete = coord[(NODE %in% net_vertexlist),]

# node 375 is missing from coord table
# node 375 has links with only two other real point
cord375E = colMeans(coord[NODE %in% c(643, 2680), .(COORD_X, COORD_Y)])



coord_complete = rbind(coord_complete,
                data.frame("NODE" = 375, "COORD_X" = cord375E[1], "COORD_Y" = cord375E[2], "NAME" = NA))


rm(coord, network, network_2way)
gc()
# Load the network

names(coord_complete)<- c("NODE", "x", "y", "NAME")
network_complete[, c('LOAD', 'VLOAD', 'TIME_min') := list(0, 0, (DIST/SPEED)*60) ]

# the are two edges with 0 capacity (they should have capacity 999999)
network_complete[CAPACITY==0, CAPACITY:=999999]
#Load in igraph

tr_net<-graph_from_data_frame(vertices = coord_complete, d= network_complete, directed = TRUE)


plot.igraph(tr_net,
            vertex.color=adjustcolor("gray",alpha.f=0.5), vertex.size=3, vertex.label=NA,
            link=adjustcolor("red",alpha.f=0.5), edge.width=1,
            edge.arrow.size=.1)



# First try with original zoning


#fare loop sulle shortpath aumentanto E attr load con il numero di veicoli assegnati

E(tr_net)$VLOAD <- 0

# Assign the vehicles to the network
for (cap in 4:6){
#cap<- 4  
print(cap)
short_path <- shortest_paths(tr_net, matOD[matOD[[cap]]>0,]$ZONE_O, matOD[matOD[[cap]]>0,]$ZONE_D, mode = 'out',
                               weights = E(tr_net)$TIME_min, output = "both",
                               predecessors = FALSE, inbound.edges = FALSE)

for (i in 1:length(short_path$epath)){
  E(tr_net, path=unlist(short_path$vpath[[i]]))$VLOAD <- 
    (E(tr_net, path=unlist(short_path$vpath[[i]]))$VLOAD + matOD[i, (cap)])
  }

#Retrive the vehicles, calculate the load and update speed
network_complete$VLOAD<- E(tr_net)$VLOAD
network_complete[, LOAD:= VLOAD / CAPACITY]

# i join the network with the speedflow table to update the speed
network_complete <- network_complete[speedFlow, .(A_NODE, B_NODE, DIST, LINK_TYPE, CAP_INDEX,
                              CAPACITY, DIR_CODE, LOAD, VLOAD, TIME_min, SPEED,
                              SPEED_updated = SPEED *(1 - speed_red/100)),
                 on=.(CAP_INDEX, LOAD>=load_left, LOAD<=load_right),  nomatch=0]
# update the speed in the network
network_complete[, TIME_min:=(DIST/SPEED_updated)*60]
E(tr_net)$SPEED <- network_complete$SPEED_updated
print('next step')
}


sum(network_complete$VLOAD)

sum(matOD$VEH_H)

E(tr_net)$width<- (3 + network_complete$LOAD*10)
E(tr_net)$color<-  as.factor((1 + network_complete$LOAD))

library(RColorBrewer)

#PLOT

pal = brewer.pal( n=6 , 'YlOrBr')[2:6]

V(tr_net)$size = 1
V(tr_net)[1:17]$size = 3
V(tr_net)[1:17]$color = 'red'

plot.igraph(tr_net, vertex.label=NA,
            link=adjustcolor("red",alpha=0.5), edge.alpha = .5,
            edge.arrow.size=.1, palette=pal)

