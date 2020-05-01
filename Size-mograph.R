library(mizer)
params_knife <- set_community_model(z0 = 0.1, recruitment = 4e7, alpha = 0.2, f0 = 0.7, knife_edge_size = 1000, sigma = 2, h=10, beta = 100, zeta = 2, n=0.72)


simNoFishing <- project(params_knife, effort = 1, t_max = 300, dt=0.01)


plankton_no_fishing <- simNoFishing@n_pp




for (i in 1:length(plankton_no_fishing[1,])) {
    for (j in 1:length(plankton_no_fishing[,1])) {
        if(plankton_no_fishing[j,i] == 0){
            plankton_no_fishing[j,i] <- NaN
        }
        plankton_no_fishing[j,i] <- plankton_no_fishing[j,i]*simNoFishing@params@w_full[i]
    }
}


fish_no_fishing <- simNoFishing@n[,1,]


for (i in 1:length(fish_no_fishing[1,])) {
    for (j in 1:length(fish_no_fishing[,1])) {
        if(fish_no_fishing[j,i] == 0){
            fish_no_fishing[j,i] <- NaN
        }
        fish_no_fishing[j,i] <- fish_no_fishing[j,i]*simNoFishing@params@w[i]
    }
}

library(plotly)


pb <- plot_ly(x = log10(simNoFishing@params@w), y = 0:(length(fish_no_fishing[,1])-1),z = log10(fish_no_fishing),colorscale='Blues') %>% 
    layout(
        title = "Biomass Density Against Time",
        scene = list(
            xaxis = list(title = "log(weight) [log(g)]"),
            yaxis = list(title = "time [years]"),
            zaxis = list(title = "log(biomass density) [log(g/m^(-3))]")
        )) %>% add_surface() %>%
    add_surface(x = log10(simNoFishing@params@w_full), y = 0:(length(plankton_no_fishing[,1])-1),z = log10(plankton_no_fishing),colorscale='Greens')
pb


# This loop selects the same weight of plankton each time. 
for(i in 1:length(plankton_no_fishing[1,])){
    if(is.nan(plankton_no_fishing[1,i])){
        planton_index = i-3
        break
    }
}


time = c(49:(length(plankton_no_fishing[,1])-1))
y1 = ((log(plankton_no_fishing[,planton_index]) - log(plankton_no_fishing[77,planton_index])))[-1:-49]

y2 = ((log(fish_no_fishing[,3]) - log(fish_no_fishing[50,3])))[-1:-49]
y3 = ((log(fish_no_fishing[,50]) - log(fish_no_fishing[50,50])))[-1:-49]
y4 = ((log(fish_no_fishing[,90]) - log(fish_no_fishing[50,90])))[-1:-49]


f <- list(
    family = "Courier New, monospace",
    size = 18,
    color = "#7f7f7f"
)
x <- list(
    title = "time",
    titlefont = f
)
y <- list(
    title = "log(biomass density change)",
    titlefont = f
)

data <- data.frame(time, y1, y2, y3, y4)

plot2 <- plot_ly(data, x = ~time)  %>%
    add_trace(y = ~y1, name = 'planton w = 0.66mg', type = 'scatter',mode = 'lines') %>%
    add_trace(y = ~y2, name = 'fish w = 1.52mg', type = 'scatter',mode = 'lines') %>%
    add_trace(y = ~y3, name = 'fish w = 28.5g', type = 'scatter',mode = 'lines') %>%
    add_trace(y = ~y4, name = 'fish w = 123kg', type = 'scatter',mode = 'lines')

plot2 <- plot2 %>% layout(xaxis = x, yaxis = y)

plot2
