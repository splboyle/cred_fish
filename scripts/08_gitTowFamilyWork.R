tow_bar <- do.call(data.frame, aggregate(Barracuda~ISLAND+YEAR, data = forereefonly_tow_families, FUN = function(x){c(Mean = mean(x), SE = st.err(x))}))

tow_emp <- do.call(data.frame, aggregate(Emperor~ISLAND+YEAR, data = forereefonly_tow_families, FUN = function(x){c(Mean = mean(x), SE = st.err(x))}))

tow_grou <- do.call(data.frame, aggregate(Grouper~ISLAND+YEAR, data = forereefonly_tow_families, FUN = function(x){c(Mean = mean(x), SE = st.err(x))}))

tow_jack <- do.call(data.frame, aggregate(Jack~ISLAND+YEAR, data = forereefonly_tow_families, FUN = function(x){c(Mean = mean(x), SE = st.err(x))}))

tow_milk <- do.call(data.frame, aggregate(Milkfish~ISLAND+YEAR, data = forereefonly_tow_families, FUN = function(x){c(Mean = mean(x), SE = st.err(x))}))

tow_other <- do.call(data.frame, aggregate(OTHER~ISLAND+YEAR, data = forereefonly_tow_families, FUN = function(x){c(Mean = mean(x), SE = st.err(x))}))

tow_par <- do.call(data.frame, aggregate(Parrotfish~ISLAND+YEAR, data = forereefonly_tow_families, FUN = function(x){c(Mean = mean(x), SE = st.err(x))}))

tow_shar <- do.call(data.frame, aggregate(Shark~ISLAND+YEAR, data = forereefonly_tow_families, FUN = function(x){c(Mean = mean(x), SE = st.err(x))}))

tow_snap <- do.call(data.frame, aggregate(Snapper~ISLAND+YEAR, data = forereefonly_tow_families, FUN = function(x){c(Mean = mean(x), SE = st.err(x))}))

tow_sur <- do.call(data.frame, aggregate(Surgeonfish~ISLAND+YEAR, data = forereefonly_tow_families, FUN = function(x){c(Mean = mean(x), SE = st.err(x))}))

tow_tot2 <- do.call(data.frame, aggregate(TotFish~ISLAND+YEAR, data = forereefonly_tow_families, FUN = function(x){c(Mean = mean(x), SE = st.err(x))}))

tow_tri <- do.call(data.frame, aggregate(Triggerfish~ISLAND+YEAR, data = forereefonly_tow_families, FUN = function(x){c(Mean = mean(x), SE = st.err(x))}))

tow_tuna <- do.call(data.frame, aggregate(Tuna~ISLAND+YEAR, data = forereefonly_tow_families, FUN = function(x){c(Mean = mean(x), SE = st.err(x))}))

tow_wras <- do.call(data.frame, aggregate(Wrasse~ISLAND+YEAR, data = forereefonly_tow_families, FUN = function(x){c(Mean = mean(x), SE = st.err(x))}))

tow_ray <- do.call(data.frame, aggregate(Rays~ISLAND+YEAR, data = forereefonly_tow_families, FUN = function(x){c(Mean = mean(x), SE = st.err(x))}))

tot_forplot <- cbind(tow_tot2, tow_ray, tow_wras, tow_tuna, tow_tri, tow_sur, tow_snap, tow_shar, tow_par, tow_other, tow_milk, tow_jack, tow_grou, tow_emp, tow_bar)
tot_forplot <- tot_forplot[-c(5,6,9,10,13,14,17,18,21,22,25,26,29,30,33,34,37,38,41,42,45,46,49,50,53,54,57,58)]
write.csv(tot_forplot, "data/clean_data/tow_means.csv")






tot <- subset(TotFish_tow_formean, STRATA == "Forereef" & DEPTH_M >= 10)
tow_tot <- do.call(data.frame, aggregate(TotFish~ISLAND+YEAR, data = tot, FUN = function(x){c(Mean = mean(x), SE = st.err(x))}))


