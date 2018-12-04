topn_countries_attries<-function(n,bet,clo,deg,gdp){
  all<-union(union(union(bet, clo),deg),gdp)
  len<-length(all)
  bet_stat<-rep.int(0,len)
  for(each in bet){
    bet_stat[all==each]=1
  }
  clo_stat<-rep.int(0,len)
  for(each in clo){
    clo_stat[all==each]=1
  }
  deg_stat<-rep.int(0,len)
  for(each in deg){
    deg_stat[all==each]=1
  }
  gdp_stat<-rep.int(0,len)
  for(each in gdp){
    gdp_stat[all==each]=1
  }
  df<-cbind(c(all,all,all,all),c(rep("BET",len),rep("CLO", len),rep("DEG",len),rep("GDP",len)),
            c(bet_stat,clo_stat,deg_stat,gdp_stat))
  df[is.na(df)]<-0
  df
}

getAttributes<-function(name){
  GDP<-vertex_attr(trade_graph,"gdp")[countries$X[countries$iso==name]]
  OutDeg<-vertex_attr(trade_graph,"OutDeg")[countries$X[countries$iso==name]]
  InDeg<-vertex_attr(trade_graph,"InDeg")[countries$X[countries$iso==name]]
  Export<-vertex_attr(trade_graph,"Export")[countries$X[countries$iso==name]]
  Import<-vertex_attr(trade_graph,"Import")[countries$X[countries$iso==name]]
  bet_cent<-vertex_attr(trade_graph,"BetCent")[[1]][countries$X[countries$iso==name]]
  clo_cent<-vertex_attr(trade_graph,"CloCent")[[1]][countries$X[countries$iso==name]]
  deg_cent<-vertex_attr(trade_graph,"DegCent")[[1]][countries$X[countries$iso==name]]
  list(GDP,OutDeg,InDeg,Export,Import,bet_cent,clo_cent,deg_cent)
}


getID<-function(name){
  countries$X[countries$iso==name]
}

ExploreEgo<-function(iso, mymode="all"){
  id<-getID(iso)
  ego_graph<-make_ego_graph(trade_graph,mode = mymode, nodes=V(trade_graph)[id])[[1]]
  write.graph(ego_graph,paste(iso,"ego_graph.graphml"),"graphml")
  
  density<-graph.density(ego_graph)
  size<-length(V(ego_graph))
  bet<-centr_betw(ego_graph)
  bet_ego<-bet[[1]][id]
  bet_cent<-bet$centralization
  const<-constraint(ego_graph)[id]
  res<-c(iso,size,density,bet_ego,bet_cent,const)
  names(res)<-c("ISO","Size","Density","Bet Ego","Bet Cent","Constraint")
  res
}
