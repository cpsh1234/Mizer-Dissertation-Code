library(mizer)
params_knife <- set_community_model(z0 = 0.1, recruitment = 4e7, alpha = 0.2, f0 = 0.7, knife_edge_size = 1000, sigma = 2, h=10, beta = 100, zeta = 2, n=0.88)

simNoFishing <- project(params_knife, effort = 0, t_max = 300, dt=0.01)
simFishing<- project(params_knife, effort = 1, t_max = 300, dt=0.01)


plankton_no_fishing <- simNoFishing@n_pp
plankton_fishing <- simFishing@n_pp




for (i in 1:length(plankton_no_fishing[1,])) {
    for (j in 1:length(plankton_no_fishing[,1])) {
        if(plankton_no_fishing[j,i] == 0){
            plankton_no_fishing[j,i] <- NaN
        }
        plankton_no_fishing[j,i] <- plankton_no_fishing[j,i]*simNoFishing@params@w_full[i]
    }
}


for (i in 1:length(plankton_fishing[1,])) {
    for (j in 1:length(plankton_fishing[,1])) {
        if(plankton_fishing[j,i] == 0){
            plankton_fishing[j,i] <- NaN
        }
        plankton_fishing[j,i] <- plankton_fishing[j,i]*simNoFishing@params@w_full[i]
    }
}


fish_no_fishing <- simNoFishing@n[,1,]
fish_fishing <- simFishing@n[,1,]


for (i in 1:length(fish_no_fishing[1,])) {
    for (j in 1:length(fish_no_fishing[,1])) {
        if(fish_no_fishing[j,i] == 0){
            fish_no_fishing[j,i] <- NaN
        }
        fish_no_fishing[j,i] <- fish_no_fishing[j,i]*simNoFishing@params@w[i]
    }
}


for (i in 1:length(fish_fishing[1,])) {
    for (j in 1:length(fish_fishing[,1])) {
        if(fish_fishing[j,i] == 0){
            fish_fishing[j,i] <- NaN
        }
        fish_fishing[j,i] <- fish_fishing[j,i]*simNoFishing@params@w[i]
    }
}

library(plotly)

relative_abundance <- ((fish_fishing-fish_no_fishing) / (fish_no_fishing+fish_fishing))

plankton_relative_abundance <- (plankton_fishing-plankton_no_fishing) / (plankton_no_fishing+plankton_fishing)

p <- plot_ly(x = log10(simNoFishing@params@w), y = 0:(length(relative_abundance[,1])-1),z = (relative_abundance),colorscale='Portland') %>% 
    layout(
    title = "Relative Abundance",
    scene = list(
        xaxis = list(title = "log(weight) [log(g)]"),
        yaxis = list(title = "time [years]"),
        zaxis = list(title = "relative abundance")
    )) %>% add_surface() %>% 
    add_surface(x = log10(simNoFishing@params@w_full), y = 0:(length(plankton_relative_abundance[,1])-1),z = (plankton_relative_abundance),colorscale='Greens')
p
