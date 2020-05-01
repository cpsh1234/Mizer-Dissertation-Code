library(mizer)
params_knife <- set_community_model(z0 = 0.1, recruitment = 4e7, alpha = 0.2, f0 = 0.7, knife_edge_size = 1000, sigma = 2, h=10, beta = 235, zeta = 2, n=2/3)


simNoFishing <- project(params_knife, effort = 0, t_max = 1500, dt=0.01, t_save = 1)


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


# Poincare plots


library(plotly)

#Set trial wave length
Trial_wave = log(plankton_no_fishing[,3])

#Trial wave is given by the Fourier transform
for (i in 1:length(Trial_wave)) {
    Trial_wave[i] = 14.99355*cos(-55/119*pi*i)+9.73306*sin(-55/119*pi*i)
}



Large_Plankton = log(plankton_no_fishing[,40])
Small_Fish = log(fish_no_fishing[,3])
Medium_Fish = log(fish_no_fishing[,50])
Large_Fish = log(fish_no_fishing[,90])


Trial_wave <- Trial_wave[-1:-299]
Large_Plankton = Large_Plankton[-1:-299]
Small_Fish = Small_Fish[-1:-299]
Medium_Fish = Medium_Fish[-1:-299]
Large_Fish = Large_Fish[-1:-299]



Poincare_data_Large_Plankton <- data.frame(Trial_wave, Large_Plankton)
Poincare_data_Small_Fish <- data.frame(Trial_wave, Small_Fish)
Poincare_data_Medium_Fish <- data.frame(Trial_wave, Medium_Fish)
Poincare_data_Large_Fish <- data.frame(Trial_wave, Large_Fish)


Poincare_Large_Plankton <- plot_ly(Poincare_data_Large_Plankton, x = ~Trial_wave, y = ~Large_Plankton, name = 'trace 0', type = 'scatter', mode = 'markers')
Poincare_Small_Fish <- plot_ly(Poincare_data_Small_Fish, x = ~Trial_wave, y = ~Small_Fish, name = 'trace 0', type = 'scatter', mode = 'markers')
Poincare_Medium_Fish <- plot_ly(Poincare_data_Medium_Fish, x = ~Trial_wave, y = ~Medium_Fish, name = 'trace 0', type = 'scatter', mode = 'markers')
Poincare_Large_Fish <- plot_ly(Poincare_data_Large_Fish, x = ~Trial_wave, y = ~Large_Fish, name = 'trace 0', type = 'scatter', mode = 'markers')


Poincare_Large_Plankton
Poincare_Small_Fish
Poincare_Medium_Fish
Poincare_Large_Fish

strobed_Trial_wave     <- 0
strobed_Large_Plankton <- 0
strobed_Small_Fish     <- 0
strobed_Medium_Fish    <- 0
strobed_Large_Fish     <- 0

#Change 238 to become the value that represents the periodicity of the wave. For example, if t_save = 1/3 and the frequency is 1/14 years^(-1), 238 would be changed to 42.

for (i in 1:length(Trial_wave)) {
    if(i%%238 == 0){
        strobed_Trial_wave[i/238] = Trial_wave[i]
        strobed_Large_Plankton[i/238] = Large_Plankton[i]
        strobed_Small_Fish[i/238] = Small_Fish[i]
        strobed_Medium_Fish[i/238] = Medium_Fish[i]
        strobed_Large_Fish[i/238] = Large_Fish[i]
    }
}

strobed_Poincare_data_Large_Plankton <- data.frame(strobed_Trial_wave, strobed_Large_Plankton)
strobed_Poincare_data_Small_Fish <- data.frame(strobed_Trial_wave, strobed_Small_Fish)
strobed_Poincare_data_Medium_Fish <- data.frame(strobed_Trial_wave, strobed_Medium_Fish)
strobed_Poincare_data_Large_Fish <- data.frame(strobed_Trial_wave, strobed_Large_Fish)


strobed_Poincare_Large_Plankton <- plot_ly(strobed_Poincare_data_Large_Plankton, x = ~strobed_Trial_wave, y = ~strobed_Large_Plankton, name = 'trace 0', type = 'scatter', mode = 'markers')
strobed_Poincare_Small_Fish <- plot_ly(strobed_Poincare_data_Small_Fish, x = ~strobed_Trial_wave, y = ~strobed_Small_Fish, name = 'trace 0', type = 'scatter', mode = 'markers')
strobed_Poincare_Medium_Fish <- plot_ly(strobed_Poincare_data_Medium_Fish, x = ~strobed_Trial_wave, y = ~strobed_Medium_Fish, name = 'trace 0', type = 'scatter', mode = 'markers')
strobed_Poincare_Large_Fish <- plot_ly(strobed_Poincare_data_Large_Fish, x = ~strobed_Trial_wave, y = ~strobed_Large_Fish, name = 'trace 0', type = 'scatter', mode = 'markers')


strobed_Poincare_Large_Plankton
strobed_Poincare_Small_Fish
strobed_Poincare_Medium_Fish
strobed_Poincare_Large_Fish

