using CairoMakie # plotting backend needed for the other plotting stuff
using AlgebraOfGraphics # ggplot2 type interface
using MixedModelsMakie  # mixed models specials for plotting
using DataFrames
using Effects # like effects or emmeans
using MixedModels # you know this one
using MixedModelsDatasets # for the data
using Random # for the random number generator

kb07 = dataset(:kb07)
insteval = dataset(:insteval)
ml1m = dataset(:ml1m)

mkb07 = lmm(@formula(rt_trunc ~ 1 + spkr * prec * load 
                             + (1 + spkr + prec + load | subj)
                             + (1 + spkr + prec + load | item)),
            kb07;
            contrasts=Dict(:spkr => EffectsCoding(),
                           :prec => EffectsCoding(base="maintain"),
                           :load => EffectsCoding()))

mi = lmm(@formula(y ~ 1 + service + (1|s) + (1|d) + (1|dept)), dataset(:insteval))
mm = lmm(@formula(Y ~ 1 + (1|G) + (1|H)), dataset(:ml1m))

nestingplot(mkb07)
nestingtable(mkb07)
filter(:count => iszero, nestingtable(mkb07))
nestingstructure(mkb07)
nestingplot(mi)
nestingplot(mm)

# BUG HERE. I WILL FIX
upsetplot(kb07; cols=Not([:subj, :item]))
upsetplot(mkb07, :subj)
upsetplot(mkb07, :item)

mkb07_small = lmm(@formula(rt_trunc ~ 1 + spkr * prec * load 
                             + (1 + spkr | subj)
                             + (1 + load | item)),
            kb07;
            contrasts=Dict(:spkr => EffectsCoding(),
                           :prec => EffectsCoding(base="maintain"),
                           :load => EffectsCoding()))

bkb07 = parametricbootstrap(MersenneTwister(2708), 500, mkb07_small)

coefplot(mkb07)

coefplot(mkb07, mkb07_small; 
         show_intercept=false,
         labels=["big", "small"])

coefplot(mkb07_small, bkb07; 
         show_intercept=false,
         labels=["wald", "boot"])

ridgeplot(bkb07; show_intercept=false)

ridgeplot(bkb07; ptype=:σ)

ridgeplot(bkb07; 
          ptype=:sigma, 
          group=:subj)
