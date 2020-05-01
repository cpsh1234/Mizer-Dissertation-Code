library(mizer)
params_knife <- set_community_model(z0 = 0.1, recruitment = 4e7, alpha = 0.2, f0 = 0.7, knife_edge_size = 1000, sigma = 2, h=10, beta = 100, zeta = 2, n=2/3)

simNoFishing_a <- project(params_knife, effort = 0, t_max = 200, dt=0.01)
simFishing_a   <- project(params_knife, effort = 1, t_max = 200, dt=0.01)

params_knife_b <- set_community_model(z0 = 0.1, recruitment = 4e7, alpha = 0.2, f0 = 0.7, knife_edge_size = 1000, sigma = 2, h=10, beta = 10, zeta = 2, n=2/3)

simNoFishing_b <- project(params_knife_b, effort = 0, t_max = 200, dt=0.01)
simFishing_b   <- project(params_knife_b, effort = 1, t_max = 200, dt=0.01)

params_knife_c <- set_community_model(z0 = 0.1, recruitment = 4e7, alpha = 0.2, f0 = 0.7, knife_edge_size = 1000, sigma = 3, h=10, beta = 100, zeta = 2, n=2/3)

simNoFishing_c <- project(params_knife_c, effort = 0, t_max = 200, dt=0.01)
simFishing_c   <- project(params_knife_c, effort = 1, t_max = 200, dt=0.01)

params_knife_d <- set_community_model(z0 = 0.1, recruitment = 4e7, alpha = 0.2, f0 = 0.7, knife_edge_size = 1000, sigma = 2, h=5, beta = 100, zeta = 2, n=2/3)

simNoFishing_d <- project(params_knife_d, effort = 0, t_max = 200, dt=0.01)
simFishing_d   <- project(params_knife_d, effort = 1, t_max = 200, dt=0.01)

params_knife_e <- set_community_model(z0 = 0.1, recruitment = 4e7, alpha = 0.2, f0 = 0.7, knife_edge_size = 1000, sigma = 2, h=20, beta = 100, zeta = 2, n=2/3)

simNoFishing_e <- project(params_knife_e, effort = 0, t_max = 200, dt=0.01)
simFishing_e   <- project(params_knife_e, effort = 1, t_max = 200, dt=0.01)

params_knife_f <- set_community_model(z0 = 0.1, recruitment = 4e7, alpha = 0.2, f0 = 0.7, knife_edge_size = 1000, sigma = 2, h=10, beta = 100, zeta = 0.01, n=2/3)

simNoFishing_f <- project(params_knife_f, effort = 0, t_max = 200, dt=0.01)
simFishing_f   <- project(params_knife_f, effort = 1, t_max = 200, dt=0.01)

params_knife_g <- set_community_model(z0 = 0.1, recruitment = 4e7, alpha = 0.2, f0 = 0.7, knife_edge_size = 1000, sigma = 2, h=10, beta = 100, zeta = 400, n=2/3)

simNoFishing_g <- project(params_knife_g, effort = 0, t_max = 200, dt=0.01)
simFishing_g   <- project(params_knife_g, effort = 1, t_max = 200, dt=0.01)

simNoFishing_h <- project(params_knife, effort = 0, t_max = 200, dt=0.01)
simFishing_h   <- project(params_knife, effort = 0.5, t_max = 200, dt=0.01)


library(ggplot2)
library(scales)
relative_abundance_a <- ((simFishing_a@n[,1,]-simNoFishing_a@n[,1,]) / (simNoFishing_a@n[,1,]+simFishing_a@n[,1,]))[201,]
relative_abundance_b <- ((simFishing_b@n[,1,]-simNoFishing_b@n[,1,]) / (simNoFishing_b@n[,1,]+simFishing_b@n[,1,]))[201,]
relative_abundance_c <- ((simFishing_c@n[,1,]-simNoFishing_c@n[,1,]) / (simNoFishing_c@n[,1,]+simFishing_c@n[,1,]))[201,]
relative_abundance_d <- ((simFishing_d@n[,1,]-simNoFishing_d@n[,1,]) / (simNoFishing_d@n[,1,]+simFishing_d@n[,1,]))[201,]
relative_abundance_e <- ((simFishing_e@n[,1,]-simNoFishing_e@n[,1,]) / (simNoFishing_e@n[,1,]+simFishing_e@n[,1,]))[201,]
relative_abundance_f <- ((simFishing_f@n[,1,]-simNoFishing_f@n[,1,]) / (simNoFishing_f@n[,1,]+simFishing_f@n[,1,]))[201,]
relative_abundance_g <- ((simFishing_a@n[,1,]-simNoFishing_g@n[,1,]) / (simNoFishing_g@n[,1,]+simFishing_g@n[,1,]))[201,]
relative_abundance_h <- ((simFishing_h@n[,1,]-simNoFishing_h@n[,1,]) / (simNoFishing_h@n[,1,]+simFishing_h@n[,1,]))[201,]





rp = ggplot() +
    geom_line(aes(x = simNoFishing_a@params@w, y = relative_abundance_a), color = "grey", linetype = 1) +
    geom_line(aes(x = simNoFishing_a@params@w, y = relative_abundance_b), color = "blue", linetype = 1) +
    geom_line(aes(x = simNoFishing_a@params@w, y = relative_abundance_c), color = "green", linetype = 1) +
    geom_line(aes(x = simNoFishing_a@params@w, y = relative_abundance_d), color = "purple", linetype = 1) +
    geom_line(aes(x = simNoFishing_a@params@w, y = relative_abundance_e), color = "magenta", linetype = 1) +
    geom_line(aes(x = simNoFishing_a@params@w, y = relative_abundance_f), color = "pink", linetype = 1) +
    geom_line(aes(x = simNoFishing_a@params@w, y = relative_abundance_g), color = "black", linetype = 1) +
    geom_line(aes(x = simNoFishing_a@params@w, y = relative_abundance_h), color = "red", linetype = 1) +
    xlab('Size (g)') +
    ylab('Relative abundance') + 
    scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x))) +
    scale_colour_manual(values =c('red')) +
    guides(color=guide_legend("Time"))

print(rp)
