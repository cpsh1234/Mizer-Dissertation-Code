getMort <- function(params, n = params@initial_n, 
                    n_pp = params@initial_n_pp,
                    B = params@initial_B,
                    effort = params@initial_effort, 
                    m2 = getPredMort(params, n = n, n_pp = n_pp, B = B)) {
    if (!all(dim(m2) == c(nrow(params@species_params), length(params@w)))) {
        stop("m2 argument must have dimensions: no. species (",
             nrow(params@species_params), ") x no. size bins (",
             length(params@w), ")")
    }
    
    encounter = getEncounter(params, n = n,
                             n_pp = n_pp, B = B)
    feeding_level = getFeedingLevel(params, n = n,
                                    n_pp = n_pp, B = B,
                                    encounter = encounter)
    
    stavation_param <- params@species_params$zeta
    feeding_param <- params@species_params$h
    assim_param <- params@species_params$alpha
    
    combined_param <- feeding_param*assim_param/stavation_param
    
    feeding_deficit <- sweep(feeding_level, 1,
                             0.2, "-", check.margin = FALSE)
    e <- params@w^(params@n-1)
    hunger_mort <- 1:100
    
    for(i in 1:length(feeding_deficit)){
        if(feeding_level[i] < 0.2){
            hunger_mort[i] <- -feeding_deficit[1,i]*e[i]*combined_param
        }
        else{
            hunger_mort[i] <- 0
        }
    }
    
    return(m2 + params@mu_b + getFMort(params, effort = effort) + hunger_mort)
}
