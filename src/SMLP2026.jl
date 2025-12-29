module SMLP2026

using Arrow
using CSV
using DataFrames
using Dates
using Downloads
using Markdown
using MixedModels
using MixedModelsDatasets
using PooledArrays
using Scratch
using SHA
using TypedTables
using ZipFile

const CACHE = Ref("")
const MMDS = String[]

function __init__()
    CACHE[] = @get_scratch!("data")
    mkpath(CACHE[])
    append!(MMDS, MixedModelsDatasets.datasets())
end

include("datasets.jl")
include("tagpad.jl")
include("movielens.jl")

"""
    age_at_event(edate::Dates.TimeType, dob::Dates.TimeType)

Return the age in years at `edate` for a person born on `dob`.
"""
function age_at_event(edate::TimeType, dob::TimeType)
    (ey, em, ed) = yearmonthday(edate)
    (by, bm, bd) = yearmonthday(dob)
    return (ey - by) - (em < bm | (em == bm & ed < bd))
end

export GENRES,
    age_at_event,
    tagpad,
    fit_or_restore,
    fit_or_restore!

function _normalize_path(path)
    dir, file = splitdir(path)
    if isempty(dir) 
        return joinpath(dirname(@__DIR__), "fits", file)
    else 
        return path
    end
end

function fit_or_restore(fname, args...;
                         force_fit=false, fit_kwargs=(;), restore_kwargs=(;))
    model = MixedModel(args...)
    return fit_or_restore!(model, fname; force_fit, fit_kwargs, restore_kwargs)
end
    
function fit_or_restore!(model::MixedModel, fname;
                         force_fit=false, fit_kwargs=(;), restore_kwargs=(;))
    fname = _normalize_path(fname)
    if !isfile(fname) || force_fit
        fit!(model; fitlog=true, fit_kwargs...)
        saveoptsum(fname, model)
    else
        @info "Restoring from cache"
        restoreoptsum!(model, fname; restore_kwargs...)
    end

    return model
end

    
end # module EmbraceUncertainty
