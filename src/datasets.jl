# Dataset loading machinery — commented out because all datasets used by the
# course are now in MixedModelsDatasets v0.2.  Kept for easy restoration.
#
# _file(x) = joinpath(CACHE[], string(x, ".arrow"))
#
# clear_scratchspaces!() = Scratch.clear_scratchspaces!(@__MODULE__)
#
# const DATASETS =
#     CSV.read(
#         IOBuffer(
# """
# dsname,filename,version,sha2
# exp_2x2x3,za9gs,1,cb09684b7373492e849c83f20a071b97f986123677134ac2ddb9ec0dcb32e503
# """
#         ),
#         Table;
#         downcast=true,
#         pool=false,
#     )
#
# if @isdefined(_cacheddatasets)
#     empty!(_cacheddatasets)    # start from an empty cache in case DATASETS has changed
# else
#     const _cacheddatasets = Dict{Symbol, Arrow.Table}()
# end
#
# """
#     datasets()
#
# Return a vector of the names of datasets available for use in [`dataset`](@ref).
# """
# function datasets()
#     return sort!(vcat(SMLP2026.DATASETS.dsname, MixedModelsDatasets.datasets()))
# end
#
# """
#     dataset(name::Union(Symbol, AbstractString))
#
# Return as an `Arrow.Table` the dataset named `name`.
#
# Available dataset names, their versions, the filenames on the osf.io site and an SHA2 checksum of their contents
# are in the table `DATASETS`.
#
# The files are cached in the scratchspace for this package.  The name of this directory is the value of `CACHE[]`.
# """
# function dataset(nm::AbstractString)
#     return get!(_cacheddatasets, Symbol(nm)) do  # retrieve from cache if available, otherwise
#         # check for nm in DATASETS table first so MMDS can be overridden
#         rows = filter(==(nm) ∘ getproperty(:dsname), DATASETS)
#         if isempty(rows)
#             nm in MMDS || error("Dataset '$nm' is not available")
#             MixedModelsDatasets.dataset(nm)
#         else
#             row = only(rows)       # check that there is only one matching row and extract it
#             fnm = _file(nm)
#             if !isfile(fnm) || row.sha2 ≠ bytes2hex(open(sha2_256, fnm))
#                 if ismissing(row.filename)
#                     load_quiver()  # special-case `ratings` and `movies`
#                 else
#                     @info "Downloading dataset..."
#                     Downloads.download(
#                         string("https://osf.io/", row.filename, "/download?version=", row.version),
#                         fnm,
#                     )
#                     @info "done"
#                 end
#             end
#             if row.sha2 ≠ bytes2hex(open(sha2_256, fnm))
#                 throw(error("Invalid checksum on downloaded $nm dataset, version $(row.version)"))
#             end
#             Arrow.Table(fnm)
#         end
#     end
# end
# dataset(nm::Symbol) = dataset(string(nm))
