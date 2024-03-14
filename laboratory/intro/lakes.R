name <- c("Ness","Lomond","Morar","Tay","Awe","Maree","Ericht","Lochy","Rannoch","Shiel","Katrine","Arkaig","Shin")
volume <- c(7.45,2.6,2.3,1.6,1.2,1.09,1.08,1.07,0.97,0.79,0.77,0.75,0.35)
area <- c(56,71,27,26.4,39,28.6,18.6,16,19,19.5,12.4,16,22.5)
lenght <- c(39,36,18.8,23,41,20,23,16,15.7,28,12.9,19.3,27.8)
maxdepth <- c(230,190,310,150,94,114,156,162,134,128,151,109,49)
meandepth <- c(132,37,87,60.6,32,38,57.6,70,51,40,43.4,46.5,15.5)

scottish.lakes <- data.frame(name, volume, area, lenght, maxdepth, meandepth) 		# data frame

print(paste("The highest volume lake is: ", name[which.max(scottish.lakes$volume)]))
print(paste("The lowest volume lake is: ", name[which.min(scottish.lakes$volume)]))
print(paste("The highest area lake is: ", name[which.max(scottish.lakes$area)]))
print(paste("The lowest volume lake is: ", name[which.min(scottish.lakes$area)]))

scottish.lakes2 <- scottish.lakes[order(scottish.lakes$area),]						# new data frame ordered by areas

print(paste("The two largest area lakes are: ", scottish.lakes2[length(name)-1,1]," and ",scottish.lakes2[length(name),1]))

sum <- sum(scottish.lakes$area)

print(paste("The area of Scotland covered by water is: ",sum," km^2"))


