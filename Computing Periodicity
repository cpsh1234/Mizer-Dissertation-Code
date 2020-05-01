library(mizer)
params_knife <- set_community_model(z0 = 0.1, recruitment = 4e7, alpha = 0.2, f0 = 0.7, knife_edge_size = 1000, sigma = 1.69, h=10, beta = 100, zeta = 2, n=2/3)


simNoFishing <- project(params_knife, effort = 0, t_max = 1000, dt=0.01)


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



#DFT

fish_of_interest <- ((log(fish_no_fishing[,4]) - log(fish_no_fishing[50,4])))[-1:-49]

for (i in 1:(length(fish_of_interest))) {
    hann_window = (sin(pi*(i-1)/length(fish_of_interest)))^2
    fish_of_interest[i] <- fish_of_interest[i]*(hann_window)
}

#Matrix builder
imaginary = sqrt(as.complex(-1))

W = exp(-imaginary*2*pi/length(fish_of_interest))


fourier_Matrix <- matrix(1:length(fish_of_interest)^2, nrow = length(fish_of_interest), ncol = length(fish_of_interest))


for (i in 1:(length(fish_of_interest))) {
    for (j in 1:(length(fish_of_interest))){
        fourier_Matrix[i,j] = W^((i-1)*(j-1))
    }
}

Discrete_Forier_Transform = fourier_Matrix %*% fish_of_interest


plot(Discrete_Forier_Transform)

library(plotly)

Wave_Numbers = c(0:(length(Discrete_Forier_Transform)-1))
Real_Forier = Re(Discrete_Forier_Transform)
imaginary_Forier = Im(Discrete_Forier_Transform)
abs_Fourier = abs(Discrete_Forier_Transform)

Forier_data_frame = data.frame(Wave_Numbers, Real_Forier, imaginary_Forier, abs_Fourier)


plot_Forier <- plot_ly(Forier_data_frame, x = ~Wave_Numbers) %>%
    add_trace(y = ~Real_Forier, name = 'Real Forier', type = 'scatter',mode = 'markers') %>%
    add_trace(y = ~imaginary_Forier, name = 'Imaginary Forier', type = 'scatter',mode = 'markers') %>%
    add_trace(y = ~abs_Fourier, name = 'absolute Forier', type = 'scatter',mode = 'line')

plot_Forier
