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
using Random
using Scratch
using Suppressor
using SHA
using TypedTables
using ZipFile

const CACHE = Ref("")
const MMDS = String[]

const DATADIR = joinpath(dirname(@__DIR__), "data")
const FITSDIR = joinpath(dirname(@__DIR__), "fits")
export DATADIR, FITSDIR

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

function _normalize_cache_path(path)
    dir, file = splitdir(path)
    if isempty(dir)
        path = joinpath(FITSDIR, file)
    end

    return path
end

function _normalize_model_cache_path(path)
    path = _normalize_cache_path(path)
    _, ext = splitext(path)
    if ext != ".zip"
        path = path * ".zip"
    end

    return path
end

function fit_or_restore(fname, ::Type{<:MixedModel}, args...; kwargs...)
    return fit_or_restore(fname, args...; kwargs...)
end

function fit_or_restore(fname, args...; contrasts=Dict{Symbol,Any}(), kwargs...)
    model = MixedModel(args...; contrasts)
    return fit_or_restore!(model, fname; kwargs...)
end

function fit_or_restore!(model::MixedModel, fname;
                         force=false, restore_kwargs=(; atol=1e-8), 
                         fallback_to_new_fit=true, fit_kwargs...)
    fname = _normalize_model_cache_path(fname)
    @debug "cache path: $(fname)"
    if isfile(fname) && !force
        @debug "restoring from cache"
        zip = ZipFile.Reader(fname)
        try
            @suppress restoreoptsum!(model, only(zip.files); restore_kwargs...)
            return model
        catch ex
            @error "Something went wrong in reading the model cache from $(fname)"
            fallback_to_new_fit || rethrow(ex)
            @error "Trying a new fit..."
            MixedModels.unfit!(model)
        finally
            close(zip)
        end
    end

    @debug "fitting model"
    fit!(model; fit_kwargs...)
    zip = ZipFile.Writer(fname)
    try
        f = ZipFile.addfile(zip, "model.json"; method=ZipFile.Deflate)
        saveoptsum(f, model)
    catch ex
        @error "Something went wrong in saving the model cache to $(fname)"
        @error string(ex)
    finally
        close(zip)
    end

    return model
end

# TODO: cache invalidation if PRNG / replicates don't match
function bootstrap_or_restore(fname,  args...; kwargs...)
    return bootstrap_or_restore(fname, Random.default_rng(), args...; kwargs...)
end
function bootstrap_or_restore(fname, rng::AbstractRNG, n::Integer, model::MixedModel, args...;
                              force=false, bootstrap_kwargs...)
    fname = _normalize_cache_path(fname)
    @debug "cache path: $(fname)"
    if !isfile(fname) || force
        @debug "performing bootstrap"
        boot = parametricbootstrap(rng, n, model, args...; bootstrap_kwargs...)
        savereplicates(fname, boot)
    else
        @debug "restoring from cache"
        boot = restorereplicates(fname, model)
    end

    return boot
end

end # module EmbraceUncertainty
