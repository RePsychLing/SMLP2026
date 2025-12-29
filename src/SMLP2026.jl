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

function _normalize_model_cache_path(path)
    dir, file = splitdir(path)
    if isempty(dir)
        path = joinpath(dirname(@__DIR__), "fits", file)
    end

    _, ext = splitext(path)
    if ext != ".zip"
        path = path * ".zip"
    end

    return path
end

function fit_or_restore(fname, ::Type{<:MixedModel}, args...; kwargs...)
    return fit_or_restore(fname, args...; kwargs...)
end

function fit_or_restore(fname, args...; contrasts=Dict{Symbol}(), kwargs...)
    model = MixedModel(args...; contrasts)
    return fit_or_restore!(model, fname; kwargs...)
end

function fit_or_restore!(model::MixedModel, fname;
                         force_fit=false, restore_kwargs=(; atol=1e-8), fit_kwargs...)
    fname = _normalize_model_cache_path(fname)
    @debug "cache path: $(fname)"
    if !isfile(fname) || force_fit
        @debug "fitting model"
        fit!(model; fit_kwargs...)
        zip = ZipFile.Writer(fname)
        try
            mktempdir() do dir
                f = ZipFile.addfile(zip, "model.json"; method=ZipFile.Deflate)
                saveoptsum(f, model)
                return nothing
            end
        catch
            error("Something went wrong in saving the model cache")
        finally
            close(zip)
        end
    else
        @debug "restoring from cache"
        zip = ZipFile.Reader(fname)
        try
            restoreoptsum!(model, only(zip.files); restore_kwargs...)
        catch
            error("Something went wrong in reading the model cache")
        finally
            close(zip)
        end
    end

    return model
end

# TODO: wrapper function for bootstrap, which crosschecks the PRNG

end # module EmbraceUncertainty
