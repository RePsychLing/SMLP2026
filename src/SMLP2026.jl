module SMLP2026

# using Arrow            # only needed by dataset/movielens machinery
using CategoricalArrays
# using CSV              # only needed by dataset/movielens machinery
using DataFrames
using Dates
# using Downloads        # only needed by dataset/movielens machinery
# using Markdown         # only needed by movielens machinery
using MixedModels
using MixedModelsDatasets: MixedModelsDatasets
using PooledArrays
using Random
# using Scratch          # only needed by dataset machinery
using StableRNGs
using StatsBase
using Suppressor
# using SHA              # only needed by dataset machinery
# using TypedTables      # only needed by dataset machinery
using ZipFile

# const CACHE = Ref("")
# const MMDS = String[]

const DATADIR = joinpath(dirname(@__DIR__), "data")
const FITSDIR = joinpath(dirname(@__DIR__), "fits")
export DATADIR, FITSDIR

# function __init__()
#     CACHE[] = @get_scratch!("data")
#     mkpath(CACHE[])
#     append!(MMDS, MixedModelsDatasets.datasets())
# end

# include("datasets.jl")
include("tagpad.jl")
# include("movielens.jl")

"""
    age_at_event(edate::Dates.TimeType, dob::Dates.TimeType)

Return the age in years at `edate` for a person born on `dob`.
"""
function age_at_event(edate::TimeType, dob::TimeType)
    (ey, em, ed) = yearmonthday(edate)
    (by, bm, bd) = yearmonthday(dob)
    return (ey - by) - (em < bm | (em == bm & ed < bd))
end

"""
    fggk21_preprocessed()

Return the full `fggk21` (Emotikon) data as a `DataFrame`, preprocessed as in
the original publication (Fühner et al., 2021): `age` centered at 8.5 years
(`a1`), `Sex` recoded to "Boys"/"Girls", `Test` recoded to descriptive labels
(Endurance, Coordination, Speed, PowerLOW, PowerUP), and `score` replaced by
its z-score within each `Test` (`zScore`), computed on the full data.
"""
function fggk21_preprocessed()
    df = DataFrame(MixedModelsDatasets.dataset(:fggk21))
    transform!(df,
        :age => (x -> x .- 8.5) => :a1,
        :Sex => categorical => :Sex,
        :Test => categorical => :Test,
    )
    levels!(df.Sex, ["male", "female"])
    recode!(df.Sex, "male" => "Boys", "female" => "Girls")
    levels!(df.Test, ["Run", "Star_r", "S20_r", "SLJ", "BPT"])
    recode!(
        df.Test,
        "Run" => "Endurance",
        "Star_r" => "Coordination",
        "S20_r" => "Speed",
        "SLJ" => "PowerLOW",
        "BPT" => "PowerUP",
    )
    select!(groupby(df, :Test), Not(:score), :score => zscore => :zScore)
    return df
end

"""
    fggk21_teaching_sample(; nboys=1000, ngirls=1000, seed=42)
    fggk21_teaching_sample(df::AbstractDataFrame; nboys=1000, ngirls=1000, seed=42)

Return the canonical teaching subsample of the `fggk21` (Emotikon) data used
throughout the course materials: a stratified sample of whole children
(`nboys` boys and `ngirls` girls, keeping all of a sampled child's test
scores), drawn without replacement using a `StableRNG` seeded with `seed` so
that the subsample is reproducible across Julia versions.

Sampling whole children (rather than individual scores) preserves the
within-child structure needed for by-`Child` random slopes. The z-scores are
computed on the full data *before* sampling (see [`fggk21_preprocessed`](@ref),
which supplies the input when no `DataFrame` is given).
"""
function fggk21_teaching_sample(; kwargs...)
    return fggk21_teaching_sample(fggk21_preprocessed(); kwargs...)
end

function fggk21_teaching_sample(df::AbstractDataFrame;
                                nboys=1000, ngirls=1000, seed=42)
    child = unique(select(df, :Cohort, :School, :Child, :Sex, :age))
    rng = StableRNG(seed)
    samp = combine(groupby(child, :Sex)) do sdf
        n = first(sdf.Sex) == "Boys" ? nboys : ngirls
        return sdf[sample(rng, 1:nrow(sdf), n; replace=false), :]
    end
    sampled = Set(samp.Child)
    return subset(df, :Child => ByRow(in(sampled)))
end

export # GENRES,
    age_at_event,
    fggk21_preprocessed,
    fggk21_teaching_sample,
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
