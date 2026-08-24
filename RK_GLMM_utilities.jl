module RKUtils

using DataFrames
using MixedModels
using StatsModels
using StatsBase
using PrettyTables
using Printf
using CSV

export create_word_table, get_fe_table, get_gof_summary, 
       extract_vccp, collate_models, stack_models, get_terms,
       generate_term_sheet, read_term_sheet,
       varcorr_to_df,
       @compare_models, @collate_coeftables, @collate_vccptables,
       @collate_coef, @collate_z, @collate_se, @collate_vccp, @stack_vccp,
       @view_vc

# ------------------------------------------------------------------
# 1. Term Sheet Management
# ------------------------------------------------------------------

function generate_term_sheet(df::DataFrame, filename::String)
    target_col = nothing
    if "Term" in names(df); target_col = :Term
    elseif "Parameter" in names(df); target_col = :Parameter
    elseif eltype(df[!, 1]) <: AbstractString; target_col = names(df)[1]
    end

    if isnothing(target_col); error("Could not find a Term/Parameter column."); end
    
    terms = df[!, target_col]
    n = length(terms)

    sheet = DataFrame(
        Old_Row = 1:n,
        Old_Term = terms,
        New_Term = Vector{Union{String, Missing}}(missing, n), 
        Order_Key = collect(1:n),
        New_Term_Order = Vector{Union{String, Missing}}(missing, n)
    )

    fname = endswith(filename, ".csv") ? filename : "$filename.csv"
    CSV.write(fname, sheet)
    println("Template saved to: $fname")
    return sheet
end

"""
    read_term_sheet(filename, output_dir)
    -> returns (sorted_names, sort_keys, replacement_dict)
"""
function read_term_sheet(filename::String, output_dir::String=".")
    fname = endswith(filename, ".csv") ? filename : "$filename.csv"
    if !isfile(fname); error("File not found: $fname"); end
    
    df = CSV.read(fname, DataFrame)
    
    # 1. Parse Replacements
    replacements = String[]
    replacement_dict = Dict{String, String}()
    
    for row in eachrow(df)
        old = row.Old_Term
        new_val = ismissing(row.New_Term) || row.New_Term == "" ? old : string(row.New_Term)
        push!(replacements, new_val)
        replacement_dict[old] = new_val
    end
    
    # 2. Parse Ranks (FIXED: Uses Rank Logic 1, 2, 3...)
    ranked_indices = []
    default_rank = nrow(df) + 1000
    
    try
        # Collect tuples of (original_index, rank_value)
        for (i, r) in enumerate(df.Order_Key)
            rank_val = ismissing(r) ? default_rank : Int(r)
            push!(ranked_indices, (index=i, rank=rank_val))
        end
    catch
        error("Order_Key column must contain integers only.")
    end
    
    # Sort by the rank provided by user
    sort!(ranked_indices, by = x -> x.rank)
    
    # Extract the original indices in their new sorted order
    sort_keys = [x.index for x in ranked_indices]

    # 3. Generate Validation Column
    doc_df = copy(df)
    doc_df.New_Term_Order = Vector{Union{String, Missing}}(missing, nrow(doc_df))
    
    # This vector holds the actual names in the user's desired order
    sorted_names = String[]
    
    # Populate sorted_names and validation column
    for (i, key) in enumerate(sort_keys)
        if key <= length(replacements)
            val = replacements[key]
            push!(sorted_names, val)
            if i <= nrow(doc_df)
                doc_df.New_Term_Order[i] = val
            end
        end
    end

    doc_filename = joinpath(output_dir, "documented_terms.csv")
    CSV.write(doc_filename, doc_df)
    println("Documentation saved to: $doc_filename")

    return sorted_names, sort_keys, replacement_dict
end

# ------------------------------------------------------------------
# 2. Output Generation
# ------------------------------------------------------------------

function create_word_table(df::DataFrame, filename::String; 
                           decimals::Int=3, 
                           landscape::Bool=false,
                           replacements::Union{Dict, Vector{String}, Nothing}=nothing,
                           sort_by::Union{Vector{String}, Vector{Int}, Nothing}=nothing,
                           header_mode::Symbol=:clean)
    
    df_fmt = copy(df)
    
    is_vccp = "Group" in names(df_fmt) && "Parameter" in names(df_fmt) && "Type" in names(df_fmt)
    target_col = is_vccp ? :Parameter : ( "Term" in names(df_fmt) ? :Term : names(df_fmt)[1] )

    # --- VCCP Logic ---
    if is_vccp
        if !isnothing(replacements) && isa(replacements, Dict)
            function smart_replace(s)
                if haskey(replacements, s); return replacements[s]; end
                if occursin(" & ", s)
                    parts = split(s, " & ")
                    new_parts = [get(replacements, p, p) for p in parts]
                    return join(new_parts, " & ")
                end
                return s
            end
            df_fmt[!, target_col] = map(smart_replace, df_fmt[!, target_col])
        end

        if !isnothing(sort_by) && isa(sort_by, Vector{String})
            rank_dict = Dict(val => i for (i, val) in enumerate(sort_by))
            default_rank = length(sort_by) + 100
            
            function get_vccp_rank(row)
                grp = row[:Group]
                grp_rank = grp == "Residual" ? 2 : 1
                type_rank = row[:Type] == "SD" ? 1 : 2
                param = row[:Parameter]
                p_rank = default_rank
                
                if type_rank == 1 # SD
                    p_rank = get(rank_dict, param, default_rank)
                else # Corr
                    if occursin(" & ", param)
                        parts = split(param, " & ")
                        r1 = get(rank_dict, parts[1], default_rank)
                        r2 = get(rank_dict, parts[2], default_rank)
                        p_rank = min(r1, r2) * 1000 + max(r1, r2) 
                        if r1 > r2
                             row[:Parameter] = "$(parts[2]) & $(parts[1])"
                        end
                    end
                end
                return (grp_rank, grp, type_rank, p_rank)
            end
            
            df_fmt[!, :_temp_sort_key_] = [get_vccp_rank(r) for r in eachrow(df_fmt)]
            sort!(df_fmt, :_temp_sort_key_)
            select!(df_fmt, Not(:_temp_sort_key_))
        end
    
    # --- FE Logic ---
    else
        if !isnothing(replacements)
            if isa(replacements, Dict)
                replace!(df_fmt[!, target_col], [k => v for (k,v) in replacements]...)
            elseif isa(replacements, Vector{String})
                if length(replacements) == nrow(df_fmt)
                    df_fmt[!, target_col] = replacements
                else
                    @warn "Length mismatch. Renaming skipped."
                end
            end
        end

        if !isnothing(sort_by)
            if isa(sort_by, Vector{String})
                rank_dict = Dict(val => i for (i, val) in enumerate(sort_by))
                default_rank = length(sort_by) + 1
                get_rank(x) = get(rank_dict, x, default_rank)
                sort!(df_fmt, target_col, by=get_rank)
            elseif isa(sort_by, Vector{Int})
                valid_indices = filter(x -> 1 <= x <= nrow(df_fmt), sort_by)
                remaining = setdiff(1:nrow(df_fmt), valid_indices)
                final_order = vcat(valid_indices, remaining)
                df_fmt = df_fmt[final_order, :]
            end
        end
    end

    clean_str(x) = replace(replace(replace(string(x), "\"" => ""), "“" => ""), "”" => "")
    df_fmt[!, target_col] = map(clean_str, df_fmt[!, target_col])

    new_names = [replace(n, "String" => "") for n in names(df_fmt)]
    rename!(df_fmt, new_names)

    for col in names(df_fmt)
        if Base.nonmissingtype(eltype(df_fmt[!, col])) <: AbstractFloat
            df_fmt[!, col] = map(x -> ismissing(x) ? "" : @sprintf("%.*f", decimals, x), df_fmt[!, col])
        end
    end

    base_path = abspath(filename)
    md_filename = "$(base_path).md"
    docx_filename = "$(base_path).docx"
    
    open(md_filename, "w") do io
        pretty_table(io, df_fmt; backend=:markdown)
    end
    
    _process_md_header(md_filename, header_mode)
    
    out_dir = dirname(base_path)
    ref_name = landscape ? "reference_landscape.docx" : "reference.docx"
    ref_doc_path = joinpath(out_dir, ref_name)

    try
        if isfile(ref_doc_path)
            run(`/usr/local/bin/pandoc $md_filename --reference-doc=$ref_doc_path -o $docx_filename`)
            println("Saved (with $ref_name): $docx_filename")
        else
            run(`/usr/local/bin/pandoc $md_filename -o $docx_filename`)
            println("Saved (default style): $docx_filename")
        end
    catch e
        println("Error running Pandoc: $e")
    end
    
    return docx_filename
end

function _process_md_header(filename::String, mode::Symbol)
    if mode == :raw; return; end
    lines = readlines(filename)
    sep_row_idx = findfirst(l -> occursin(r"\|.*-.*\|", l), lines)
    if isnothing(sep_row_idx) || sep_row_idx < 2; return; end
    
    header_idx = sep_row_idx - 1
    header_line = lines[header_idx]
    cells = split(header_line, "|")
    
    new_header_cells = String[]
    subheader_cells = String[]
    has_subheader = false
    
    for cell in cells
        if occursin("<br>", cell)
            parts = split(cell, "<br>")
            push!(new_header_cells, parts[1])
            push!(subheader_cells, join(parts[2:end], " "))
            has_subheader = true
        else
            push!(new_header_cells, cell)
            push!(subheader_cells, "")
        end
    end
    
    lines[header_idx] = join(new_header_cells, "|")
    if mode == :split && has_subheader
        insert!(lines, sep_row_idx + 1, join(subheader_cells, "|"))
    end
    
    open(filename, "w") do io
        for line in lines; println(io, line); end
    end
end

# ... (Keep get_fe_table, get_gof_summary unchanged) ...

function get_fe_table(model; stats=[:Coef, :SE, :z, :p])
    ct = coeftable(model); df = DataFrame(ct)
    rename_dict = Dict(names(df)[1]=>:Term, names(df)[2]=>:Coef, names(df)[3]=>:SE, names(df)[4]=>:z, names(df)[5]=>:p)
    if ncol(df)>=6; rename_dict[names(df)[6]]=:Lower; end; if ncol(df)>=7; rename_dict[names(df)[7]]=:Upper; end
    rename!(df, rename_dict)
    cols = [:Term]; append!(cols, stats); return df[:, intersect(cols, propertynames(df))]
end

function get_gof_summary(models_list)
    DataFrame(Model=[x.name for x in models_list], dof=dof.([x.model for x in models_list]), 
              deviance=round.(deviance.([x.model for x in models_list]), digits=0),
              AIC=round.(aic.([x.model for x in models_list]), digits=0), 
              BIC=round.(bic.([x.model for x in models_list]), digits=0))
end

function extract_vccp(model)
    vc = VarCorr(model); groups_vec=String[]; params_vec=String[]; types_vec=String[]; val_vec=Union{Float64, Missing}[]
    for grp in Base.keys(model.σs)
        term_names = string.(Base.keys(model.σs[grp]))
        for name in term_names
            push!(groups_vec, string(grp)); push!(params_vec, name); push!(types_vec, "SD"); push!(val_vec, model.σs[grp][Symbol(name)])
        end
        rho_obj = vc.σρ[grp].ρ; is_matrix = isa(rho_obj, AbstractMatrix); counter = 1
        for i in 2:length(term_names), j in 1:(i-1)
            val = is_matrix ? rho_obj[i, j] : rho_obj[counter]
            if !is_matrix; counter += 1; end
            # Relaxed zero check
            final_val = abs(val) < 1e-8 ? missing : val
            push!(groups_vec, string(grp)); push!(params_vec, "$(term_names[i]) & $(term_names[j])"); push!(types_vec, "Corr"); push!(val_vec, final_val)
        end
    end
    if !ismissing(model.σ); push!(groups_vec, "Residual"); push!(params_vec, "Residual"); push!(types_vec, "SD"); push!(val_vec, model.σ); end
    return DataFrame(Group=groups_vec, Type=types_vec, Parameter=params_vec, Value=val_vec)
end

function collate_models(model_dict, model_keys; type=:FE, stats=[:Coef])
    if isa(stats, Symbol); stats = [stats]; end
    first_mod = model_dict[model_keys[1]]
    if type == :FE; df_temp = get_fe_table(first_mod; stats=stats); master = df_temp[:, [:Term]]
    else; df_temp = extract_vccp(first_mod); master = df_temp[:, [:Group, :Parameter, :Type]]; end
    for key in model_keys
        mod = model_dict[key]
        if type == :FE
            current_df = get_fe_table(mod; stats=stats)
            for s in stats; rename!(current_df, s => (length(stats)==1 ? Symbol(key) : Symbol("$(key)_$(s)"))); end
            master = outerjoin(master, current_df, on=[:Term])
        else
            current_df = extract_vccp(mod); rename!(current_df, :Value => Symbol(key))
            master = outerjoin(master, current_df, on=[:Group, :Parameter, :Type])
        end
    end
    if type != :FE
        function default_sort_key(r)
            g_rank = r[:Group] == "Residual" ? 2 : 1
            t_rank = r[:Type] == "SD" ? 1 : 2
            return (g_rank, r[:Group], t_rank, r[:Parameter])
        end
        # Use temp column for safe sorting
        master[!, :_tmp_sort] = [default_sort_key(r) for r in eachrow(master)]
        sort!(master, :_tmp_sort)
        select!(master, Not(:_tmp_sort))
    end
    return master
end

function stack_models(model_dict, model_keys)
    df_list = DataFrame[]
    for key in model_keys
        mod = model_dict[key]
        df = extract_vccp(mod)
        insertcols!(df, 1, :Model => key)
        push!(df_list, df)
    end
    return vcat(df_list...)
end

function varcorr_to_df(model)
    vc = VarCorr(model)
    groups = String[]
    terms = String[]
    stds = Float64[]
    corrs = String[] 
    
    for grp in Base.keys(model.σs)
        σ_grp = model.σs[grp]
        term_names = string.(Base.keys(σ_grp))
        ρ_grp = vc.σρ[grp].ρ
        is_matrix = isa(ρ_grp, AbstractMatrix)
        counter = 1
        
        for (i, name) in enumerate(term_names)
            push!(groups, string(grp))
            push!(terms, name)
            push!(stds, σ_grp[Symbol(name)])
            
            if i == 1
                push!(corrs, "")
            else
                row_corrs = Float64[]
                for j in 1:(i-1)
                    val = is_matrix ? ρ_grp[i, j] : ρ_grp[counter]
                    if !is_matrix; counter += 1; end
                    if abs(val) > 1e-8
                        push!(row_corrs, val)
                    else
                         push!(row_corrs, 0.0) 
                    end
                end
                
                corr_str = join([@sprintf("%.2f", c) for c in row_corrs], ", ")
                if all(x -> abs(x) < 1e-8, row_corrs)
                     push!(corrs, "")
                else
                     push!(corrs, corr_str)
                end
            end
        end
    end
    
    if !ismissing(model.σ)
        push!(groups, "Residual")
        push!(terms, "Residual")
        push!(stds, model.σ)
        push!(corrs, "")
    end
    
    return DataFrame(Group=groups, Term=terms, StdDev=stds, Corr=corrs)
end

# Macros
macro compare_models(args...)
    list_expr = Expr(:vect); for arg in args; push!(list_expr.args, :((name=$(string(arg)), model=$(esc(arg))))); end
    return :(get_gof_summary($list_expr))
end

macro collate_coef(args...)
    names_expr = Expr(:vect); models_expr = Expr(:vect)
    for arg in args; push!(names_expr.args, string(arg)); push!(models_expr.args, esc(arg)); end
    return quote; local d=Dict{String,Any}(); for (n,m) in zip($names_expr, $models_expr); d[n]=m; end; collate_models(d, $names_expr, type=:FE, stats=:Coef); end
end

macro collate_z(args...)
    names_expr = Expr(:vect); models_expr = Expr(:vect)
    for arg in args; push!(names_expr.args, string(arg)); push!(models_expr.args, esc(arg)); end
    return quote; local d=Dict{String,Any}(); for (n,m) in zip($names_expr, $models_expr); d[n]=m; end; collate_models(d, $names_expr, type=:FE, stats=:z); end
end

macro collate_se(args...)
    names_expr = Expr(:vect); models_expr = Expr(:vect)
    for arg in args; push!(names_expr.args, string(arg)); push!(models_expr.args, esc(arg)); end
    return quote; local d=Dict{String,Any}(); for (n,m) in zip($names_expr, $models_expr); d[n]=m; end; collate_models(d, $names_expr, type=:FE, stats=:SE); end
end

macro collate_vccp(models...)
    keys_expr = [string(m) for m in models]; pairs = [Expr(:call, :(=>), k, esc(m)) for (k,m) in zip(keys_expr, models)]
    return quote; collate_models(Dict($(pairs...)), $keys_expr, type=:VC); end
end

macro stack_vccp(models...)
    keys_expr = [string(m) for m in models]
    pairs = [Expr(:call, :(=>), k, esc(m)) for (k,m) in zip(keys_expr, models)]
    return quote
        Base.invokelatest($stack_models, Dict($(pairs...)), $keys_expr)
    end
end

macro view_vc(models...)
    exprs = []
    for arg in models
        name = string(arg)
        push!(exprs, quote
            println("\n--- Variance Components: ", $name, " ---")
            display(varcorr_to_df($(esc(arg))))
        end)
    end
    return Expr(:block, exprs...)
end

macro collate_vccptables(models...); return :(@collate_vccp($(models...))); end
macro collate_coeftables(args...); return :(@collate_coef($(args...))); end

function get_terms(df::DataFrame)
    target_col = "Term" in names(df) ? :Term : ("Parameter" in names(df) ? :Parameter : names(df)[1])
    return df[!, target_col]
end

end # module