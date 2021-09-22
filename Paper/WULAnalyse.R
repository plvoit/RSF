unique(WULZone2$FINALIDADE)

WUL <- WULZone2[,c(10,12,18)]
WUL <- WUL[,c(2,3)]

WUL <- aggregate(WUL$VOLUME_ANU,by=list(WUL$FINALIDADE), sum)

WUL <- WUL[!(is.na(WUL$x)),]
WUL <- WUL[!(WUL$x == 0),]

WUL$Percent <- WUL$x / sum(WUL$x) * 100
