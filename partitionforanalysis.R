flagdf <- data.frame(flag.data)
cvars <- c("cont", "hemi", "area", "pop", "lang", "rel")
fvars <- c("nbars", "nstripes", "ncolors", "red", "green", "blue", "gold", "white", "black", 
           "orange", "mainhue", "ncircles", "nX", "ndiagX", "n4ed", "nsunstars", "crescent",
           "tri", "icon", "animate", "text", "topleft", "botright")
names(flagdf)[1] <- "name"
names(flagdf)[2:7] <- cvars
names(flagdf)[8:30] <- fvars

#just colors
flagcols <- flagdf[ ,fvars[3:11]]
mainhue.inwords <- flagdf$mainhue
flagcols$mainhue <- match(flagcols$mainhue, rev(names(sort(table(flagcols$mainhue)))))
flagcols[, 2:9] <- apply(flagcols[, 2:9], 2, as.factor)

#just country characteristics
cc <- flagdf[ ,cvars]
cc[, c(1,2,5,6)] <- apply(cc[, c(1,2,5,6)], 2, as.factor)

