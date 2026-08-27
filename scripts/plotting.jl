using CairoMakie # plotting backend needed for the other plotting stuff
using AlgebraOfGraphics # ggplot2 type interface
using MixedModelsMakie  # mixed models specials for plotting
using DataFrames
using Effects # like effects or emmeans
using MixedModels # you know this one
using MixedModelsDatasets # for the data

kb07 = dataset(:kb07)
insteval = dataset(:insteval)
ml1m = dataset(:ml1m)

mkb07 = lmm(@formula(rt_trunc ~ 1 + spkr * prec * load 
                             + (1 + spkr + prec + load | subj)
                             + (1 + spkr + prec + load | item)),
            kb07;
            contrasts=Dict(:spkr => EffectsCoding(),
                           :prec => EffectsCoding(),
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
