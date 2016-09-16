currdir <- getSrcDirectory(function(x) {x})
maindir <- paste(currdir, "/Datasets", sep="")
subdir <- list.files(maindir)

fit.weibull.eclipse = function(dat) {
  n = sum(dat)
  nls(y ~ n*dweibull(t, scale = l, shape = k), 
      data = data.frame(t = 1:length(dat), y = dat), 
      start = list(l = 2.4 , k = 7.1))
}

fit.weibull.jetspeed2 = function(dat) {
  n = sum(dat)
  nls(y ~ n*dweibull(t, scale = l, shape = k), 
      data = data.frame(t = 1:length(dat), y = dat), 
      start = list(l= 1.3 , k = 4.1))
}

fit.weibull = function(dat) {
  n = sum(dat)
  nls(y ~ n*dweibull(t, scale = l, shape = k), 
      data = data.frame(t = 1:length(dat), y = dat), 
      start = list( l = 1.5, k = 4))
}

fit.gamma.eclipse = function(dat) {
  n = sum(dat)
  nls(y~n*dgamma(t,scale = l, shape = k), 
      data = data.frame(t = 1:length(dat), y = dat), 
      start = list(l = 0.5  , k = 2))
}

fit.gamma.jetspeed2 = function(dat) {
  n = sum(dat)
  nls(y ~ n*dgamma(t, scale = l, shape = k ), 
      data = data.frame(t = 1:length(dat), y = dat), 
      start = list(l= 0.8, k = 5))
}

fit.gamma.tomcat = function(dat) {
  n = sum(dat)
  nls(y~n*dgamma(t,scale = l, shape = k), 
      data = data.frame(t = 1:length(dat), y = dat), 
      start = list(l = 3.5  , k = 1))
}

r2 = function(d1,d2){
  r=1 - var(d2-d1)/var(d2)
  r
}

list1 = list2 = list3 = list4 = list()
setwd(file.path(maindir,paste(subdir[1])))
files = dir(getwd(),pattern=".txt",recursive=TRUE)
if(subdir[1]=="Eclipse")
{
	data.list<-list()
	for(k in 1:6)
	{
		filename = paste('E',k,'.txt', sep='')
		data.list[[k]] = scan(filename)
	}
	for(i in 1:6)
	{
		wei = fit.weibull.eclipse(data.list[[i]])
		gam = fit.gamma.eclipse(data.list[[i]])
		list1[i] = AIC(wei)
		list2[i] = AIC(gam)
		list3[i] = r2(fitted.values(wei),data.list[[i]])
		list4[i] = r2(fitted.values(gam),data.list[[i]])
	}
}

list11 = list12 = list13 = list14 = list()
setwd(file.path(maindir,paste(subdir[2])))
files = dir(getwd(),pattern=".txt",recursive=TRUE)
if(subdir[2]=="JetSpeed-2")
{
	data.list1<-list()
	for(k in 1:4)
	{
		filename = paste('J',k,'.txt', sep='')
		data.list1[[k]] = scan(filename)
	}
	for ( i in 1:4)
	{
		wei = fit.weibull.jetspeed2(data.list1[[i]])
		gam = fit.gamma.jetspeed2(data.list1[[i]])
		list11[i] = AIC(wei)
		list12[i] = AIC(gam)
		list13[i] = r2(fitted.values(wei),data.list1[[i]])
		list14[i] = r2(fitted.values(gam),data.list1[[i]])
	}  
}

list21 = list22 = list23 = list24 = list()
setwd(file.path(maindir,paste(subdir[3])))
files = dir(getwd(),pattern=".txt",recursive=TRUE)
if(subdir[3]=="Tomcat")
{
	data.list2<-list()
	for(k in 1:4){
		filename = paste('T',k,'.txt', sep='')
		data.list2[[k]] = scan(filename)
	}
	for ( i in 1:4){
		wei = fit.weibull(data.list2[[i]])
		gam = fit.gamma.tomcat(data.list2[[i]])
		list21[i] = AIC(wei)
		list22[i] = AIC(gam)
		list23[i] = r2(fitted.values(wei),data.list2[[i]])
		list24[i] = r2(fitted.values(gam),data.list2[[i]])
	}  
}   

Versions <- c()
for(i in 1:sum(length(list1),length(list11),length(list21))){
  if(i <= length(list1))
  {Versions[i] = paste("E",i)} 
  else if(i > length(list1) && i<= sum(length(list1),length(list11))) 
  {Versions[i] = paste("J",i-length(list1))} 
  else
  {Versions[i] = paste("T",i-sum(length(list1),length(list11)))}
}


AICwei <- c()
for(i in 1:sum(length(list1),length(list11),length(list21))){
  if(i <= length(list1))
  {AICwei[i] = list1[[i]]} 
  else if(i > length(list1) && i<= sum(length(list1),length(list11))) 
  {AICwei[i] = list11[[i-length(list1)]]} 
  else
  {AICwei[i] = list21[[i-sum(length(list1),length(list11))]]}
}

AICgam <- c()
for(i in 1:sum(length(list2),length(list12),length(list22))){
  if(i <= length(list2))
  {AICgam[i] = list2[[i]]} 
  else if(i > length(list2) && i<= sum(length(list2),length(list12))) 
  {AICgam[i] = list12[[i-length(list2)]]} 
  else
  {AICgam[i] = list22[[i-sum(length(list2),length(list12))]]}
}

R2wei <- c()
for(i in 1:sum(length(list3),length(list13),length(list23))){
  if(i <= length(list3))
  {R2wei[i] = list3[[i]]} 
  else if(i > length(list3) && i<= sum(length(list3),length(list13))) 
  {R2wei[i] = list13[[i-length(list3)]]} 
  else
  {R2wei[i] = list23[[i-sum(length(list3),length(list13))]]}
}

R2gam <- c()
for(i in 1:sum(length(list4),length(list14),length(list24))){
  if(i <= length(list4))
  {R2gam[i] = list4[[i]]} 
  else if(i > length(list4) && i<= sum(length(list4),length(list14))) 
  {R2gam[i] = list14[[i-length(list4)]]} 
  else
  {R2gam[i] = list24[[i-sum(length(list4),length(list14))]]}
}

fit.weibull.eclipse = function(dat) {
  n = sum(dat)
  nls(y ~ n*dweibull(t, scale = l, shape = k), 
      data = data.frame(t = 1:length(dat), y = dat), 
      start = list(l = 2.4 , k = 7.1))
}

fit.weibull.jetspeed2 = function(dat) {
  n = sum(dat)
  nls(y ~ n*dweibull(t, scale = l, shape = k), 
      data = data.frame(t = 1:length(dat), y = dat), 
      start = list(l= 1.3 , k = 4.1))
}

fit.weibull = function(dat) {
  n = sum(dat)
  nls(y ~ n*dweibull(t, scale = l, shape = k), 
      data = data.frame(t = 1:length(dat), y = dat), 
      start = list( l = 1.5, k = 4))
}

fit.gamma.eclipse = function(dat) {
  n = sum(dat)
  nls(y~n*dgamma(t,scale = l, shape = k), 
      data = data.frame(t = 1:length(dat), y = dat), 
      start = list(l = 0.5  , k = 2))
}

fit.gamma.jetspeed2 = function(dat) {
  n = sum(dat)
  nls(y ~ n*dgamma(t, scale = l, shape = k ), 
      data = data.frame(t = 1:length(dat), y = dat), 
      start = list(l= 0.8, k = 5))
}

fit.gamma.tomcat = function(dat) {
  n = sum(dat)
  nls(y~n*dgamma(t,scale = l, shape = k), 
      data = data.frame(t = 1:length(dat), y = dat), 
      start = list(l = 3.5  , k = 1))
}

r2 = function(d1,d2){
  r=1 - var(d2-d1)/var(d2)
  r
}

list1 = list2 = list3 = list4 = list()
setwd(file.path(maindir,paste(subdir[1])))
files = dir(getwd(),pattern=".txt",recursive=TRUE)
if(subdir[1]=="Eclipse")
{
	data.list<-list()
	for(k in 1:6)
	{
		filename = paste('E',k,'.txt', sep='')
		data.list[[k]] = scan(filename)
	}
	for(i in 1:6)
	{
		wei = fit.weibull.eclipse(data.list[[i]])
		gam = fit.gamma.eclipse(data.list[[i]])
		list1[i] = AIC(wei)
		list2[i] = AIC(gam)
		list3[i] = r2(fitted.values(wei),data.list[[i]])
		list4[i] = r2(fitted.values(gam),data.list[[i]])
	}
}

list11 = list12 = list13 = list14 = list()
setwd(file.path(maindir,paste(subdir[2])))
files = dir(getwd(),pattern=".txt",recursive=TRUE)
if(subdir[2]=="JetSpeed-2")
{
	data.list1<-list()
	for(k in 1:4)
	{
		filename = paste('J',k,'.txt', sep='')
		data.list1[[k]] = scan(filename)
	}
	for ( i in 1:4)
	{
		wei = fit.weibull.jetspeed2(data.list1[[i]])
		gam = fit.gamma.jetspeed2(data.list1[[i]])
		list11[i] = AIC(wei)
		list12[i] = AIC(gam)
		list13[i] = r2(fitted.values(wei),data.list1[[i]])
		list14[i] = r2(fitted.values(gam),data.list1[[i]])
	}  
}

list21 = list22 = list23 = list24 = list()
setwd(file.path(maindir,paste(subdir[3])))
files = dir(getwd(),pattern=".txt",recursive=TRUE)
if(subdir[3]=="Tomcat")
{
	data.list2<-list()
	for(k in 1:4){
		filename = paste('T',k,'.txt', sep='')
		data.list2[[k]] = scan(filename)
	}
	for ( i in 1:4){
		wei = fit.weibull(data.list2[[i]])
		gam = fit.gamma.tomcat(data.list2[[i]])
		list21[i] = AIC(wei)
		list22[i] = AIC(gam)
		list23[i] = r2(fitted.values(wei),data.list2[[i]])
		list24[i] = r2(fitted.values(gam),data.list2[[i]])
	}  
}   

Versions <- c()
for(i in 1:sum(length(list1),length(list11),length(list21))){
  if(i <= length(list1))
  {Versions[i] = paste("E",i)} 
  else if(i > length(list1) && i<= sum(length(list1),length(list11))) 
  {Versions[i] = paste("J",i-length(list1))} 
  else
  {Versions[i] = paste("T",i-sum(length(list1),length(list11)))}
}


AICwei <- c()
for(i in 1:sum(length(list1),length(list11),length(list21))){
  if(i <= length(list1))
  {AICwei[i] = list1[[i]]} 
  else if(i > length(list1) && i<= sum(length(list1),length(list11))) 
  {AICwei[i] = list11[[i-length(list1)]]} 
  else
  {AICwei[i] = list21[[i-sum(length(list1),length(list11))]]}
}

AICgam <- c()
for(i in 1:sum(length(list2),length(list12),length(list22))){
  if(i <= length(list2))
  {AICgam[i] = list2[[i]]} 
  else if(i > length(list2) && i<= sum(length(list2),length(list12))) 
  {AICgam[i] = list12[[i-length(list2)]]} 
  else
  {AICgam[i] = list22[[i-sum(length(list2),length(list12))]]}
}

R2wei <- c()
for(i in 1:sum(length(list3),length(list13),length(list23))){
  if(i <= length(list3))
  {R2wei[i] = list3[[i]]} 
  else if(i > length(list3) && i<= sum(length(list3),length(list13))) 
  {R2wei[i] = list13[[i-length(list3)]]} 
  else
  {R2wei[i] = list23[[i-sum(length(list3),length(list13))]]}
}

R2gam <- c()
for(i in 1:sum(length(list4),length(list14),length(list24))){
  if(i <= length(list4))
  {R2gam[i] = list4[[i]]} 
  else if(i > length(list4) && i<= sum(length(list4),length(list14))) 
  {R2gam[i] = list14[[i-length(list4)]]} 
  else
  {R2gam[i] = list24[[i-sum(length(list4),length(list14))]]}
}

df1 = data.frame(Versions, AICwei, AICgam, R2wei, R2gam)   
df1

setwd(file.path(maindir,paste(subdir[1])))
if(subdir[1]=="Eclipse"){
	files = dir(getwd(),pattern=".txt",recursive=TRUE)
	# Eclipse Graph
	# Define 3 vectors
	data.list<-list()
	for(k in 1:4){
		filename = paste('E',k,'.txt', sep='')
		data.list[[k]] = scan(filename)
	}
	ecp1 <- data.list[[1]][1:5]
	ecp2 <- data.list[[2]][1:5]
	ecp3 <- data.list[[3]][1:5]
	ecp4 <- data.list[[4]][1:5]
	
	g_range <- range(0, ecp1, ecp2, ecp3, ecp4)

	plot(ecp1, type="o", col="blue", ylim=g_range, 
	   axes=FALSE, ann=FALSE)

	axis(1, at=1:5, lab=c("Q1","Q2","Q3","Q4","Q5"))

	axis(2, las=1, at=50*0:10)

	# Create box around plot
	box()

	lines(ecp2, type="o", col="red")
	lines(ecp3, type="o", col="green")
	lines(ecp4, type="o", col="yellow")

	# Create a title with a green, bold/italic font
	title(main="Eclipse", col.main="green", font.main=4)

	# Label the x and y axes with dark green text
	title(xlab="Versions", col.lab=rgb(0,0.5,0))
	title(ylab="Score", col.lab=rgb(0,0.5,0))

	# Create a legend at (1, g_range[2]) that is slightly smaller 
	# (cex) and uses the same line colors and points used by 
	# the actual plots 
	legend(1, g_range[2], c("E1","E2","E3","E4"), cex=0.5, 
	   col=c("blue","red","green","yellow"), pch=21:22, lty=1:1);
}
setwd(file.path(maindir,paste(subdir[2])))
if(subdir[2]=="JetSpeed-2"){
	# Jetspeed Graph   
	# Define 3 vectors
	files = dir(getwd(),pattern=".txt",recursive=TRUE)
	data.list<-list()
	for(k in 1:4){
		filename = paste('J',k,'.txt', sep='')
		data.list[[k]] = scan(filename)
	}
	js1 <- data.list[[1]][1:5]
	js2 <- data.list[[2]][1:5]
	js3 <- data.list[[3]][1:5]
	js4 <- data.list[[4]][1:5]

	g_range <- range(0, js1, js2, js3, js4)

	plot(js1, type="o", col="blue", ylim=g_range, 
	   axes=FALSE, ann=FALSE)

	axis(1, at=1:5, lab=c("Q1","Q2","Q3","Q4","Q5"))

	axis(2, las=1, at=4*0:g_range[2])

	# Create box around plot
	box()

	lines(js2, type="o", col="red")
	lines(js3, type="o", col="green")
	lines(js4, type="o", col="yellow")

	# Create a title with a green, bold/italic font
	title(main="JetSpeed2", col.main="green", font.main=4)

	# Label the x and y axes with dark green text
	title(xlab="Versions", col.lab=rgb(0,0.5,0))
	title(ylab="Score", col.lab=rgb(0,0.5,0))

	# Create a legend at (1, g_range[2]) that is slightly smaller 
	# (cex) and uses the same line colors and points used by 
	# the actual plots 
	legend(1, g_range[2], c("J1","J2","J3","J4"), cex=0.5, 
	   col=c("blue","red","green","yellow"), pch=21:22, lty=1:1);
}
setwd(file.path(maindir,paste(subdir[3])))
if(subdir[3]=="Tomcat"){	   
	# Tomcat Graph
	# Define 3 vectors
	files = dir(getwd(),pattern=".txt",recursive=TRUE)
	data.list<-list()
	for(k in 1:4){
		filename = paste('T',k,'.txt', sep='')
		data.list[[k]] = scan(filename)
	}
	tc1 <- data.list[[1]][1:5]
	tc2 <- data.list[[2]][1:5]
	tc3 <- data.list[[3]][1:5]
	tc4 <- data.list[[4]][1:5]

	#g_range <- range(0, tc1, tc2, tc3, tc4)
	g_range <- range(0, tc1, tc2, tc3, tc4)

	plot(tc1, type="o", col="blue", ylim=g_range, 
	   axes=FALSE, ann=FALSE)

	axis(1, at=1:5, lab=c("Q1","Q2","Q3","Q4","Q5"))

	axis(2, las=1, at=50*0:10)

	# Create box around plot
	box()

	lines(tc2, type="o", col="red")
	lines(tc3, type="o", col="green")
	lines(tc4, type="o", col="yellow")

	# Create a title with a green, bold/italic font
	title(main="Tomcat", col.main="green", font.main=4)

	# Label the x and y axes with dark green text
	title(xlab="Versions", col.lab=rgb(0,0.5,0))
	title(ylab="Score", col.lab=rgb(0,0.5,0))

	# Create a legend at (1, g_range[2]) that is slightly smaller 
	# (cex) and uses the same line colors and points used by 
	# the actual plots 
	legend(1, g_range[2], c("T1","T2","T3","T4"), cex=0.5, 
	   col=c("blue","red","green","yellow"), pch=21:22, lty=1:1);   
}	   

df1 = data.frame(Versions, AICwei, AICgam, R2wei, R2gam)   
print(df1)

