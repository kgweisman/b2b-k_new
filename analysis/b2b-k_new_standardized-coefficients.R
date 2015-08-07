# --- STUDY 1 -----------------------------------------------------------------

# orthogonal contrasts
contrasts(d$pair, how.many = 11) <- contrastOrthogonal
r1std.orth <- lmer(scale(response) ~ pair + (1 | subid), 
                subset(d, phase == "test" & study == "1"))
summary(r1std.orth)
round(summary(r1std.orth)$coefficients, 2)

# --- STUDY 2 -----------------------------------------------------------------

# orthogonal contrasts
contrasts(d$pair, how.many = 11) <- contrastOrthogonal
r2std.orth <- lmer(scale(response) ~ pair + gender + scale(age, scale = T) + (1 | subid), 
                subset(d, phase == "test" & study == "2"))
summary(r2std.orth)
round(summary(r2std.orth)$coefficients, 2)

# adult/child comparison: orthogonal contrasts
contrasts(d$pair, how.many = 11) <- contrastOrthogonal
r2std.orthAgeGrpSimp <- lmer(scale(response) ~ pair + (1 | subid), 
                          subset(d, phase == "test" & 
                                   (study == "1" | study == "2")))
r2std.orthAgeGrpAdd <- lmer(scale(response) ~ pair + ageGroup + (1 | subid), 
                         subset(d, phase == "test" & 
                                  (study == "1" | study == "2")))
r2std.orthAgeGrpInt <- lmer(scale(response) ~ pair * ageGroup + (1 | subid), 
                         subset(d, phase == "test" & 
                                  (study == "1" | study == "2")))
anova(r2std.orthAgeGrpSimp, r2std.orthAgeGrpAdd, r2std.orthAgeGrpInt)
summary(r2std.orthAgeGrpInt)
round(summary(r2std.orthAgeGrpInt)$coefficients,2)

# race/ethnicity comparison: orthogonal contrasts
contrasts(d$pair, how.many = 11) <- contrastOrthogonal
r2std.orthREsimp <- lmer(scale(response) ~ pair + gender + scale(age, scale = T) + (1 | subid), 
                      data = subset(d, phase == "test" & 
                                      study == "2" & 
                                      raceEthn2 != "NA"))
r2std.orthREadd <- lmer(scale(response) ~ pair + raceEthn2 + gender + scale(age, scale = T) + (1 | subid), 
                     data = subset(d, phase == "test" & 
                                     study == "2" & 
                                     raceEthn2 != "NA"))
r2std.orthREint <- lmer(scale(response) ~ pair * raceEthn2 + gender + scale(age, scale = T) + (1 | subid), 
                     data = subset(d, phase == "test" & 
                                     study == "2" & 
                                     raceEthn2 != "NA"))
anova(r2std.orthREsimp, r2std.orthREadd, r2std.orthREint)
summary(r2std.orthREint)
round(summary(r2std.orthREint)$coefficients,2)

# more interactions!
# three-way interactions with gender and age
contrasts(d$pair, how.many = 11) <- contrastOrthogonal
r2std.orth2way <- lmer(scale(response) ~ pair * gender * scale(age, scale = T) 
                    + (1 | subid), 
                    subset(d, phase == "test" & study == "2"))
summary(r2std.orth2way)
round(summary(r2std.orth2way)$coefficients, 2)

# four-way interactions with gender, age, and raceEthn2
contrasts(d$pair, how.many = 11) <- contrastOrthogonal
r2std.orth3way <- lmer(scale(response) ~ pair * gender * scale(age, scale = T) 
                    * raceEthn2 
                    + (1 | subid), 
                    subset(d, phase == "test" & study == "2"))
summary(r2std.orth3way)
round(summary(r2std.orth3way)$coefficients, 2)

# --- STUDY 3 -----------------------------------------------------------------

# orthogonal contrasts
contrasts(d$pair, how.many = 11) <- contrastOrthogonal
r3std.orth <- lmer(scale(response) ~ pair + (1 | subid), 
                subset(d, phase == "test" & study == "3"))
summary(r3std.orth)
round(summary(r3std.orth)$coefficients, 2)

# us/india comparison: orthogonal contrasts
contrasts(d$pair, how.many = 11) <- contrastOrthogonal
r3std.orthCountrySimp <- lmer(scale(response) ~ pair + (1 | subid), 
                           subset(d, (phase == "test") & 
                                    (study == "1" | study == "3")))
r3std.orthCountryAdd <- lmer(scale(response) ~ pair + country + (1 | subid), 
                          subset(d, (phase == "test") & 
                                   (study == "1" | study == "3")))
r3std.orthCountryInt <- lmer(scale(response) ~ pair * country + (1 | subid), 
                          subset(d, (phase == "test") & 
                                   (study == "1" | study == "3")))
anova(r3std.orthCountrySimp, r3std.orthCountryAdd, r3std.orthCountryInt)
summary(r3std.orthCountryInt)
round(summary(r3std.orthCountryInt)$coefficients, 2)

# --- STUDY 4 -----------------------------------------------------------------

# us/india comparison: orthogonal contrasts
contrasts(d$pair, how.many = 11) <- contrastOrthogonal
r4std.orthCountrySimp <- lmer(scale(response) ~ pair + (1 | subid), 
                           subset(d, phase == "test" & study == "4"))
r4std.orthCountryAdd <- lmer(scale(response) ~ pair + country + (1 | subid), 
                          subset(d, phase == "test" & study == "4"))
r4std.orthCountryInt <- lmer(scale(response) ~ pair * country + (1 | subid), 
                          subset(d, phase == "test" & study == "4"))
anova(r4std.orthCountrySimp, r4std.orthCountryAdd, r4std.orthCountryInt)
summary(r4std.orthCountryInt)
round(summary(r4std.orthCountryInt)$coefficients, 2)

# us/india comparison, framing comparison: orthogonal contrasts
contrasts(d$pair, how.many = 11) <- contrastOrthogonal
r4std.orthCntryFrmSimp <- lmer(scale(response) ~ pair * country + (1 | subid), 
                            subset(d, phase == "test" & 
                                     study != "2" & study != "1prime"))
r4std.orthCntryFrmAdd <- lmer(scale(response) ~ pair * country + framing + (1 | subid), 
                           subset(d, phase == "test" & 
                                    study != "2" & study != "1prime"))
r4std.orthCntryFrmInt1 <- lmer(scale(response) ~ pair * country + 
                              framing + country:framing + (1 | subid), 
                            subset(d, phase == "test" & 
                                     study != "2" & study != "1prime"))
r4std.orthCntryFrmInt2 <- lmer(scale(response) ~ pair * country * framing + (1 | subid), 
                            subset(d, phase == "test" & 
                                     study != "2" & study != "1prime"))
anova(r4std.orthCntryFrmSimp, r4std.orthCntryFrmAdd, 
      r4std.orthCntryFrmInt1, r4std.orthCntryFrmInt2)
summary(r4std.orthCntryFrmInt2)
round(summary(r4std.orthCntryFrmInt2)$coefficients, 2)

# --- lmerTest script ---------------------------------------------------------

# for checking significance of t-values, if desired
# library(lmerTest)
# temp <- lmer(response ~ pair * country + (1 | subid), 
#              subset(d, (phase == "test") & 
#                       (study == "1" | study == "3")))
# summary(temp)
# 
# round(summary(temp)$coefficients, 2)
# 
# detach("package:lmerTest", unload=TRUE)
