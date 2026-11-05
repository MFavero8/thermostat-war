# ============================================================
# Prior specification: Model 04
#
# Reference category:
#   y4 (comfortable)
#
# ============================================================


prior_mod_04 <-   
  #prior intercepts (for K-1 categories with cat4 reference category)
  #index variable approach: sexfemale
  c(prior(normal(-2.69, 2), class = b, coef = sexfemale, dpar = muy1),
    prior(normal(-1.84, 2), class = b, coef = sexfemale, dpar = muy2),
    prior(normal(-0.89, 2), class = b, coef = sexfemale, dpar = muy3),
    prior(normal(-0.89, 2), class = b, coef = sexfemale, dpar = muy5),
    prior(normal(-1.84, 2), class = b, coef = sexfemale, dpar = muy6),
    prior(normal(-2.69, 2), class = b, coef = sexfemale, dpar = muy7),
    #index variable approach: sexmale
    prior(normal(-2.69, 2), class = b, coef = sexmale, dpar = muy1),
    prior(normal(-1.84, 2), class = b, coef = sexmale, dpar = muy2),
    prior(normal(-0.89, 2), class = b, coef = sexmale, dpar = muy3),
    prior(normal(-0.89, 2), class = b, coef = sexmale, dpar = muy5),
    prior(normal(-1.84, 2), class = b, coef = sexmale, dpar = muy6),
    prior(normal(-2.69, 2), class = b, coef = sexmale, dpar = muy7),
    #prior time.day
    prior(normal(0, 0.25), class = b, coef = time.dayevening, dpar = muy1),
    prior(normal(0, 0.25), class = b, coef = time.dayevening, dpar = muy2),
    prior(normal(0, 0.25), class = b, coef = time.dayevening, dpar = muy3),
    prior(normal(0, 0.25), class = b, coef = time.dayevening, dpar = muy5),
    prior(normal(0, 0.25), class = b, coef = time.dayevening, dpar = muy6),
    prior(normal(0, 0.25), class = b, coef = time.dayevening, dpar = muy7),
    #prior observation
    prior(normal(0, 0.25), class = b, coef = moc_time.onset.h, dpar = muy1),
    prior(normal(0, 0.25), class = b, coef = moc_time.onset.h, dpar = muy2),
    prior(normal(0, 0.25), class = b, coef = moc_time.onset.h, dpar = muy3),
    prior(normal(0, 0.25), class = b, coef = moc_time.onset.h, dpar = muy5),
    prior(normal(0, 0.25), class = b, coef = moc_time.onset.h, dpar = muy6),
    prior(normal(0, 0.25), class = b, coef = moc_time.onset.h, dpar = muy7),
    prior(dirichlet(c(2, 2, 2, 2)), class = simo, coef = moc_time.onset.h1, dpar = muy1),
    prior(dirichlet(c(2, 2, 2, 2)), class = simo, coef = moc_time.onset.h1, dpar = muy2),
    prior(dirichlet(c(2, 2, 2, 2)), class = simo, coef = moc_time.onset.h1, dpar = muy3),
    prior(dirichlet(c(2, 2, 2, 2)), class = simo, coef = moc_time.onset.h1, dpar = muy5),
    prior(dirichlet(c(2, 2, 2, 2)), class = simo, coef = moc_time.onset.h1, dpar = muy6),
    prior(dirichlet(c(2, 2, 2, 2)), class = simo, coef = moc_time.onset.h1, dpar = muy7),  
    #prior temperature
    #index variable approach: sexmale:c_air.temp
    prior(normal(-1, 0.5), class = b, coef = sexmale:c_air.temp, dpar = muy1),
    prior(normal(-1, 0.5), class = b, coef = sexmale:c_air.temp, dpar = muy2),
    prior(normal(-1, 0.5), class = b, coef = sexmale:c_air.temp, dpar = muy3),
    prior(normal(1, 0.5), class = b, coef = sexmale:c_air.temp, dpar = muy5),
    prior(normal(1, 0.5), class = b, coef = sexmale:c_air.temp, dpar = muy6),
    prior(normal(1, 0.5), class = b, coef = sexmale:c_air.temp, dpar = muy7),  
    #index variable approach: sexfemale:c_air.temp
    prior(normal(-1, 0.5), class = b, coef = sexfemale:c_air.temp, dpar = muy1),
    prior(normal(-1, 0.5), class = b, coef = sexfemale:c_air.temp, dpar = muy2),
    prior(normal(-1, 0.5), class = b, coef = sexfemale:c_air.temp, dpar = muy3),
    prior(normal(1, 0.5), class = b, coef = sexfemale:c_air.temp, dpar = muy5),
    prior(normal(1, 0.5), class = b, coef = sexfemale:c_air.temp, dpar = muy6),
    prior(normal(1, 0.5), class = b, coef = sexfemale:c_air.temp, dpar = muy7), 
    #prior relative humidity
    prior(normal(-0.5, 0.25), class = b, coef = c_rel.hum, dpar = muy1),
    prior(normal(-0.5, 0.25), class = b, coef = c_rel.hum, dpar = muy2),
    prior(normal(-0.5, 0.25), class = b, coef = c_rel.hum, dpar = muy3),
    prior(normal(0.5, 0.25), class = b, coef = c_rel.hum, dpar = muy5),
    prior(normal(0.5, 0.25), class = b, coef = c_rel.hum, dpar = muy6),
    prior(normal(0.5, 0.25), class = b, coef = c_rel.hum, dpar = muy7),
    #prior temperature : relative humidity
    prior(normal(0, 0.2), class = b, coef = c_rel.hum:c_air.temp, dpar = muy1),
    prior(normal(0, 0.2), class = b, coef = c_rel.hum:c_air.temp, dpar = muy2),
    prior(normal(0, 0.2), class = b, coef = c_rel.hum:c_air.temp, dpar = muy3),
    prior(normal(0, 0.2), class = b, coef = c_rel.hum:c_air.temp, dpar = muy5),
    prior(normal(0, 0.2), class = b, coef = c_rel.hum:c_air.temp, dpar = muy6),
    prior(normal(0, 0.2), class = b, coef = c_rel.hum:c_air.temp, dpar = muy7),
    #prior varying slopes for sex (for K-1 categories with cat4 reference category)  
    prior(exponential(1), class = sd, coef = sexmale, group = condition_id, dpar = muy1),
    prior(exponential(1), class = sd, coef = sexmale, group = condition_id, dpar = muy2),
    prior(exponential(1), class = sd, coef = sexmale, group = condition_id, dpar = muy3),
    prior(exponential(1), class = sd, coef = sexmale, group = condition_id, dpar = muy5),
    prior(exponential(1), class = sd, coef = sexmale, group = condition_id, dpar = muy6),
    prior(exponential(1), class = sd, coef = sexmale, group = condition_id, dpar = muy7),
    prior(exponential(1), class = sd, coef = sexfemale, group = condition_id, dpar = muy1),
    prior(exponential(1), class = sd, coef = sexfemale, group = condition_id, dpar = muy2),
    prior(exponential(1), class = sd, coef = sexfemale, group = condition_id, dpar = muy3),
    prior(exponential(1), class = sd, coef = sexfemale, group = condition_id, dpar = muy5),
    prior(exponential(1), class = sd, coef = sexfemale, group = condition_id, dpar = muy6),
    prior(exponential(1), class = sd, coef = sexfemale, group = condition_id, dpar = muy7),
    #prior varying slopes for observation (for K-1 categories with cat4 reference category)  
    prior(exponential(1), class = sd, coef = moc_time.onset.h, group = condition_id, dpar = muy1),
    prior(exponential(1), class = sd, coef = moc_time.onset.h, group = condition_id, dpar = muy2),
    prior(exponential(1), class = sd, coef = moc_time.onset.h, group = condition_id, dpar = muy3),
    prior(exponential(1), class = sd, coef = moc_time.onset.h, group = condition_id, dpar = muy5),
    prior(exponential(1), class = sd, coef = moc_time.onset.h, group = condition_id, dpar = muy6),
    prior(exponential(1), class = sd, coef = moc_time.onset.h, group = condition_id, dpar = muy7),
    #prior correlation
    prior(lkj(2), class = cor)
  )
