# based off egonet.R
# using the network and statnet SNA libraries, generate networks and ego nets, and calculate statistics of those
# adapted from previous version that pulls data from sqlite database, and assumes that database has specific structure or this project
# becuase this is SNA, remove igraph package if it's loaded from previous work
if("package:igraph" %in% search()) { detach("package:igraph") }
library("network")
library("sna")
library("plyr")

##### necessary tables  ### still need to link them in
lifeperiods
lifperiods_clanlists
egos
lifeperiods_ais_el
lifeperiods_aggressions_el
lifeperiods_ll_el

####### basic functions

is.vertex  = function(net,n) {
  # look up vertex by name in network 
  # convenience function to remember this syntax
  return(n %in% network.vertex.names(net))
}


get.vertex.id <- function(net,name){
  return(
    match(name,get.vertex.attribute(net, "vertex.names"))
  )
}



#### data functions: adapted from extract edgelists from SQL


one_period <- function(gdf,ego,period){
  # Given a data frame with an edge list partitioned on an ego and period, return a subset
  edgelist.df = gdf[gdf$ego ==ego & gdf$period == period,c("H1","H2","weight")]
  return(edgelist.df)  
}


get.clanlist <- function(ego,period) {
  clanlists.df = lifeperiods_clanlists
  clanlist = clanlists.df[clanlists.df$ego ==ego & clanlists.df$period == period,c("hyena","hyenarank","sex","hyenaAgeAtStart")]
  names(clanlist) <- c("name","rank","sex","age")
  row.names(clanlist) <- clanlist$name
  return(clanlist)
}

one_clanlist <- function(ego,period){
  return(get.clanlist(ego,period))
}


### graph functions
# used to subset edgelist by ego and period assumes 

one_graph <- function(gdf,ego,period,directedgraph=FALSE){
  # Given edge list data frame gdf  partitioned on an ego and period, subset and get network
  
  edgelist.df   = one_period(gdf,ego,period)
  # check if no data for this period/individual 
  if (nrow(edgelist.df) == 0) { return(NA)}
  
  h_attrs       = one_clanlist(ego,period) # needs to be a list
  net = network(edgelist.df,vertex.attr=h_attrs, matrix.type="edgelist", directed=directedgraph,ignore.eval = FALSE, names.eval = "weight" )
  # network.edgelist(x, g, ignore.eval = TRUE, names.eval = NULL, ...)
  set.network.attribute(net, "ego", ego)
  set.network.attribute(net, "period", period)
  
  return(net)
}


# I renamed these functions for consistency
# saving alias to not break downstream code
onePeriod <- one_period 
one.graph <- one_graph 
is_vertex <- is.vertex

analysis_that_used_longevity <- function(df){
  newdf = df[is.na(df$longevity),]
}

rank_category <-function(sr){
  # if out of range, eg. invalid number return blank 
  # should return NA
  if( sr < -1    ) ''
  else if( sr < -0.33 ) 'low' 
  else if( sr <  0.33 ) 'mid'
  else if( sr <= 1.0  ) 'high'
  else ''
  
}

# converts the above formula to use on a whole vector or column in a data frame
rank_categories <- Vectorize(rank_category, vectorize.args="sr")
# use  like this:  df$newrankcolumn = rank_categories(df$ranknumbers)


#####################
# graph statistics 
# these function return a list of stats of interest
# and a means to systematicaly get an associated function or column name
# it means there needst to exist functions for each stat


calc_net_stat = function(statname,net,ego,directed=FALSE) {
  # calculate one of several network statistics
  # if ego not in network, return 0
  if (!is.vertex(net,ego)) {return(0)}
  # then do some setup
  netu<-as.sociomatrix(net)
  netw<-as.sociomatrix(net, 'weight')
  gmode_code = if (directed) "digraph" else "graph"
  nodeid = get.vertex.id(net,ego)
  # make the calculation, default answer is 0
  s = 0
  s = switch(statname,
             "degree"     = degree(dat=netu,nodes=nodeid, gmode=gmode_code, cmode="freeman"),
             "indegree"   = degree(dat=netu,nodes=nodeid, gmode=gmode_code, cmode="indegree"),
             "outdegree"  = degree(dat=netu,nodes=nodeid, gmode=gmode_code, cmode="outdegree"),
             "strength"     = degree(dat=netw,nodes=nodeid, gmode=gmode_code, cmode="freeman"),
             "instrength"   = degree(dat=netw,nodes=nodeid, gmode=gmode_code, cmode="indegree"),
             "outstrength"  = degree(dat=netw,nodes=nodeid, gmode=gmode_code, cmode="outdegree"),
             "betweenness"= betweenness(dat=net, g=1, nodes=nodeid, gmode=gmode_code,cmode="directed")
  )
}




####################
#  stat accumulator

statnames <- function(){
  return (c("degree", "indegree", "outdegree", "strength","instrength","outstrength","betweenness"))
}
statcolname   <- function(statname,prefix="") {
  if (is.na(prefix) && length(prefix) == 0) return(statname)
  else return(paste(prefix, statname, sep="_"))
}

# this is the convention we used to name the database tables with edgelists
# e.g. lifeperiods_aggressions_el
standard_edgelist_table_name = function(behavstr) {
  paste("lifeperiods", behavstr, "el", sep="_")
}

gStats <- function(behavstr= "greetings",
                   lifeperiods.df = lifeperiods, 
                   behav.df =  NA,
                   directed = FALSE,
                   add_ego_edata    = FALSE) {
  # add new columns to the list of egos + periods to store graph statistics
  # for each kind of behavior.  this scheme depends on consistent naming of tables in the database
  # and the behavstr is in the name of the table, it's then used to create the graph statistic columns in the ego data frame
  
  
  # function to calculate all stats, and fill up the row
  # implicit parameters: behav.df, 
  get_net_stats <- function(dfrow){
    ego = as.character(dfrow$ego)
    period = dfrow$period
    # debug, cheezy progress bar
    cat(".")
    # print(ego)
    # print(period)
    
    # fill with zeros first
    dfrow[statcolname(statnames(),behavstr)] <- 0 
    # get network for this behavior
    behav.net = one_graph(behav.df, ego, period, directedgraph = directed) # directed global
    # check that a network was made, e.g. ego actualy has obs for this period
    if( class(behav.net)=="network" && is.vertex(behav.net,ego)) {
      for (s in statnames()){ 
        dfrow[statcolname(s,behavstr)] <- calc_net_stat(s, behav.net, ego, directed)
      }
    }
    return( data.frame(dfrow))
  }
  
  if (is.na(behav.df)) { 
    behav.df = standard_edgelist_table_name(behavstr)
    # check that it exists in the database! 
  }
  
  # remove all NAs from behavior dataframe to avoid crashes 
  behav.df = behav.df[complete.cases(behav.df),]
  
  # use plyr to run stats for each row
  stats.df = adply(lifeperiods.df,1,get_net_stats)
  
  # add in egos data if asked for
  if ( add_ego_edata ) {
    egos.df = egos 
    stats.df = merge(stats.df,egos.df,by="ego")
  }
  
  return(stats.df)
  
}

#######removed "liftlegboth", and ,"dyadicaggressions" 
# when all are in the behavs:
#directed=c("aggressions"=TRUE, "dyadicaggressions"=TRUE, "greetings"=FALSE,
#"liftlegboth"=FALSE,"ll"=TRUE,"ais"=FALSE) 

gStatsCombine <- function(periodstable = lifeperiods, behavs = c("ais","aggressions","ll")) {
  
  directed=c("ais"=FALSE, "aggressions"=TRUE, "ll"=TRUE) 
  
  # build a list of all the stats data frames that will be merged later
  # note this is inefficient and doubles memory requirement, but only ~ 100,000 datapoints right now
  netstats = list()
  
  # select a type of period group to use 
  # periodstable = periodstable[periodstable$seq < 4, ]
  
  for (b in behavs) {
    # to do: all for different lifeperiods table, currently uses the default
    netstats[[b]] = gStats(lifeperiods.df=periodstable, behavstr= b,   directed = directed[b], add_ego_edata = FALSE )
  }
  
  # currently, first 12 fields in stats df are not statistics fields, so don't merge them

  commonfields = names(netstats[[1]])[1:12]
  # start with first item in list of dfs of stats
  netstats.df = netstats[[1]]
  # for remaining dfs in the list, merge in, one at a time 
  # note the merge command takes just two dfs as a time
  for ( i in 2:length(netstats)) { 
    netstats.df = merge(netstats.df, netstats[[i]],  by=commonfields)
  }
  
  # add other common data from egos table
  egos.df = egos
  netstats.df = merge(netstats.df,egos.df,by="ego")
  
  return( netstats.df)
}
