# data
cdc.month = read.csv("data/cdcmonth.csv")
cdc.month$date <- as.yearmon(cdc.month$date, "%Y-%m")
# state
all.states = unique(cdc.month$state)
# functions

top_pop_states <- function(data, n=5) {
  d = data %>% group_by(state) %>% summarize(tot_case = sum(new_case, na.rm = TRUE))
  return (top_n(d, n=n, wt=tot_case)[[1]])                          
}

# plot covid cases for top population states
plot_states <- function(data, states, feature, title, x=1, y=1) {
  # top_states = top_pop_states(covid, n)
  par(mfrow=c(x,y), mar=c(4,4,2,0.5)) # Setup grid, margins
  d = data %>% filter(state %in% states)
  ggplot(d, aes(x=date, y=new_case, group = state, color=state)) + geom_line() + theme(axis.text.x = element_text(angle=90)) + ggtitle(title)
  # plot(d$date, d[,feature], xlab = "Time", ylab = feature, main = paste("COVID cases in", state), pch = 20, cex = 0.5)
}

# returns basis of the spline to be modelled
spline.basis <- function(data, n.knots=15, degree=3) {
  X <- as.matrix(data)
  x=seq(0,1,length.out=nrow(X))
  knots = seq(0,1,length.out = n.knots-2)
  return (bs(x, knots = knots, degree = 3)[,1:(n.knots)])
}

# shows heatmap of the basis
B.heatmap <- function(B) {
  return (B%>%
            as.data.frame()%>%
            mutate(rw=row_number())%>%
            gather(key='key',value='value','1':'6')%>%
            ggplot(aes(x=key,y=rw,fill=value))+geom_tile()+
            labs(title='B-Spline Basis with 6 knots and 3rd-Degree Polynomials',x='Knot',y='Date'))
}

# shows multiple curves based on knots/partitions of the basis
B.basis.plot <- function(B, n.knots=10) {
  return(B%>%
           as.data.frame()%>%
           mutate(rw=row_number())%>%
           gather(key='key',value='value',`1`:paste(n.knots))%>%
           ggplot(aes(x=rw,y=value,col=key))+geom_line(aes(group=key))+
           labs(title=paste('B-Spline Basis with ', n.knots, ' knots and 3rd-Degree Polynomials', sep='')))
}

# get spline model and return knots and spline predictions of training data
bspline.model <- function(data, dates, n.knots=6, degree=3) {
  X <- as.matrix(data)
  x=seq(0,1,length.out=nrow(X))
  knots = seq(0,1,length.out = n.knots-2)
  B = bs(x, knots = knots, degree = 3)[,1:(n.knots)]
  
  Bcoef = matrix(0,1,n.knots)
  Bcoef[1,] = solve(t(B)%*%B)%*%t(B)%*%as.matrix(X[,2])
  
  preds = (Bcoef%*%t(B))[1,]
  l = list()
  l[[1]] = Bcoef[1,]
  l[[2]] = preds
  return (l)
}

# get spline model and return knots and spline predictions of training data
bspline.basis <- function(data, dates, n.knots=6, degree=3) {
  X <- as.matrix(data)
  x=seq(0,1,length.out=nrow(X))
  knots = seq(0,1,length.out = n.knots-2)
  B = bs(x, knots = knots, degree = 3)[,1:(n.knots)]
  
  Bcoef = matrix(0,1,n.knots)
  Bcoef[1,] = solve(t(B)%*%B)%*%t(B)%*%as.matrix(X[,2])
  
  preds = (Bcoef%*%t(B))[1,]
  l = list()
  l[[1]] = Bcoef[1,]
  l[[2]] = B
  l[[3]] = preds
  return (l)
}

plot.basis.com <- function(data, states, feature, n.knots=6, degree=3, x=1, y=1) {
  # top_states = top_pop_states(covid, n)
  # par(mfrow=c(x,y), mar=c(4,4,2,0.5)) # Setup grid, margins
  data = data %>% subset(!is.na(data[, feature]))
  n = length(states)
  colors = c("#BBE7FE","#D3B5E5", "#FFD4DB", "#EFF1DB", "#3cacae", "#fe9c52")
  Bcoef.mat = matrix(0,n,n.knots)
  
  for (i in 1:n) {
    plot(c(min(data$date), max(data$date)), c(min(data[,feature]), max(data[,feature])), xlab = "Time", ylab = feature, main = paste(states[i], "COVID basis demonstration"), type = "n", xaxt='n', pch = 20, cex = 0.5)
    d = data %>% filter(state == states[i]) %>% select("date", contains(feature)) %>% arrange(date)
    dates = d$date
    d$date = as.numeric(d$date)
    l = bspline.basis(d, dates, n.knots, degree)
    lines(dates, l[[3]], pch = 20, cex = 1, col="black")
    for (j in 1:n.knots) {
      l[[2]][,j] = l[[1]][j] * l[[2]][,j]
      lines(dates, l[[2]][,j], pch = 20, cex = 1, col=colors[j])
    }
    # print(paste(c("Basis", n.knots), 1:n.knots, as.character(l[[1]])))
    legend("topleft", legend=c("Estimated Spline", paste(rep("Basis", n.knots), 1:n.knots, rep("coef:", n.knots) , as.character(round(l[[1]], digits = 3)))), col=c("black", colors), lty=1, cex=0.6)
  }
  ix <- seq(1, length(dates), by=60)
  fmt <- "%b-%Y" # format for axis labels
  labs <- format(dates[ix], fmt)
  axis(side = 1, at = dates[ix], labels = labs,  cex.axis = 0.8)
}

# This function plots the bspline curves given the target states and parameters and returns knots for each state. 
plot.bspline <- function(data, states, feature, n.knots=6, degree=3, x=1, y=1) {
  # top_states = top_pop_states(covid, n)
  # par(mfrow=c(x,y), mar=c(4,4,2,0.5)) # Setup grid, margins
  data = data %>% subset(!is.na(data[, feature]))
  n = length(states)
  colors = rainbow(n)
  Bcoef.mat = matrix(0,n,n.knots)
  
  date = as.numeric(data$date, na.rm=TRUE)
  # plot(c(min(date, na.rm=TRUE), max(date, na.rm=TRUE)), c(min(data[,feature], na.rm=TRUE), max(data[,feature], na.rm=TRUE)), xlab = "Time", ylab = feature, main = "COVID cases across states", type = "n", xaxt='n', pch = 20, cex = 0.5)
   for (i in 1:n) {
    d = data %>% filter(state == states[i]) %>% select(date, contains(feature)) %>% arrange(date)
    dates = d$date
    d$date = as.numeric(d$date)
    # points(d$date, d[,feature], pch = 20, cex = 0.5)
    
    l = bspline.model(d, dates, n.knots, degree)
    # lines(dates, l[[2]], pch = 20, cex = 1, col = colors[i])
    Bcoef.mat[i,] = l[[1]]
    # legend("topleft", legend=states, col=colors, lty=1, cex=0.6)
    ix <- seq(1, length(dates), by=60)
    fmt <- "%b-%Y" # format for axis labels
    # labs <- format(dates[ix], fmt)
    # axis(side = 1, at = dates[ix], labels = labs,  cex.axis = 0.8)
  }
  
  return (Bcoef.mat)
  
}

state.name2abb <- function(states) {
  return (state.abb[match(states, state.name)])
}

Bcoef.mat = plot.bspline(cdc.month, all.states, 'new_case', n.knots=10, degree=3)

mid <- function(x) {
  return ((max(x)+min(x))/2)
}

wss <- function(k) {
  kmeans(as.data.frame(Bcoef.mat), k, nstart = 10)$tot.withinss
}

sort.state.clusters <- function(clusters, sort.cluster.list) {
  clen = length(clusters)
  sort.clusters = 1:clen
  color.counter = 1
  for (c in 1:clen) {
    if (sort.cluster.list[[clusters[c]]] < 0) {
      sort.cluster.list[[clusters[c]]] = color.counter
      color.counter = color.counter + 1
    }
    sort.clusters[c] = sort.cluster.list[[clusters[c]]]
  }
  return (sort.clusters)
}
########################################
# 6
Northeast = state.name2abb(c("Connecticut", "Massachusetts", "Maine", 
                             "New Hampshire", "Rhode Island", "Vermont"))

# 8
Mid.Atlantic = c(state.name2abb(c("Delaware", "Maryland", 
                                  "New Jersey", "New York", "Pennsylvania",
                                  "Virginia", "West Virginia")),"DC")

# 9
Southeast = state.name2abb(c("Alabama", "Arkansas", "Florida",
                             "Georgia", "Louisiana", "Mississippi",
                             "North Carolina", "South Carolina", "Tennessee"))

# 13
Midwest = state.name2abb(c("Indiana", "Illinois", "Iowa", 
                           "Kansas", "Kentucky", "Michigan",
                           "Minnesota", "Missouri", "Nebraska",
                           "North Dakota", "Ohio", "South Dakota",
                           "Wisconsin"))
# 4
Southwest = state.name2abb(c("Arizona", "New Mexico", "Oklahoma", "Texas"))

# 11
West = state.name2abb(c("Alaska", "California", "Colorado", "Hawaii",
                        "Idaho", "Montana", "Nevada", "Oregon",
                        "Utah", "Washington", "Wyoming"))

region.list <- list(Northeast, Mid.Atlantic, Southeast, Midwest, Southwest, West)
names(region.list) <- c('Northeast', 'Mid.Atlantic', 'Southeast', 'Midwest', 'Southwest', 'West')

# Compute and plot wss for k = 2 to k = 15
k.values <- 2:10

# extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)

#plot(wss_values)

### line 449
map.df = map_data("state")
map.df$state = state.name2abb(str_to_title(map.df$region))
map.df$region = ifelse (map.df$state %in% Northeast, "Northeast",
                        ifelse (map.df$state %in% Mid.Atlantic, "Mid.Atlantic",
                                ifelse (map.df$state %in% Southeast, "Southeast",
                                        ifelse (map.df$state %in% Midwest, "Midwest",
                                                ifelse (map.df$state %in% Southwest, "Southwest",
                                                        ifelse (map.df$state %in% West, "West", "Other"
                                                        ))))))
state.centers.df = map.df %>% group_by(state) %>% summarise(long.mid = mid(long), lat.mid = mid(lat), group=mean(group))

map.state.names = geom_text(data=state.centers.df, aes(x=long.mid, y=lat.mid, group=group, label=state),size=3, hjust=0, vjust=0, colour = 1)
map.state.points = geom_point(data=state.centers.df, aes(x=long.mid, y=lat.mid, group=group), size = 1, alpha = 0.6, colour = 1)

### Ploting 