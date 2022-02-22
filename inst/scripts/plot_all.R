library(fvcom)
library(sf)
places <- c("GOM", "MassBay", "Hampton", "Boston", "Scituate", "SacoBay")
funList <- as.list(paste0(places, "Physics"))

XX <- lapply(funList,
             function(fun){
               do.call(fun, list())
             })
cols <- rev(cols <- grDevices::palette.colors())
XX[[1]]$plot(border = cols[1], axes = TRUE)
for (i in seq_along(XX)[-1]) XX[[i]]$plot(add = TRUE, border = cols[i])
