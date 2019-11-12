
module RecipesBase

export
    @recipe,
    @series,
    @userplot,
    @shorthands,
    RecipeData,
    AbstractBackend,
    AbstractPlot,
    AbstractLayout

# Common abstract types for the Plots ecosystem
abstract type AbstractBackend end
abstract type AbstractPlot{T<:AbstractBackend} end
abstract type AbstractLayout end

# a placeholder to establish the name so that other packages (Plots.jl for example)
# can add their own definition of RecipesBase.plot since RecipesBase is the common
# dependency of the Plots ecosystem
function plot end
function plot! end

# a placeholder to establish the name so that other packages (Plots.jl for example)
# can add their own definition of RecipesBase.animate since RecipesBase is the common
# dependency of the Plots ecosystem. Plots.jl will handle the basic cases, while
# other packages can now extend for their types
function animate end

# a placeholder to establish the name so that other packages (Plots.jl for example)
# can add their own definition of RecipesBase.is_key_supported(k::Symbol)
function is_key_supported end

# a placeholder to establish the name so that other packages (Plots.jl for example)
# can add their own definition of RecipesBase.group_as_matrix(t)
group_as_matrix(t) = false

# This holds the recipe definitions to be dispatched on
# the function takes in an attribute dict `d` and a list of args.
# This default definition specifies the "no-arg" case.
apply_recipe(plotattributes::Dict{Symbol,Any}) = ()

const _debug_recipes = Bool[false]
function debug(v::Bool = true)
    _debug_recipes[1] = v
end

# --------------------------------------------------------------------------

# this holds the data and attributes of one series, and is returned from apply_recipe
struct RecipeData
    plotattributes::Dict{Symbol,Any}
    args::Tuple
end

# --------------------------------------------------------------------------

@inline to_symbol(s::Symbol) = s
@inline to_symbol(qn::QuoteNode) = qn.value

@inline wrap_tuple(tup::Tuple) = tup
@inline wrap_tuple(v) = (v,)

# check for flags as part of the `-->` expression
function _is_arrow_tuple(expr::Expr)
    expr.head == :tuple && !isempty(expr.args) &&
        isa(expr.args[1], Expr) &&
        expr.args[1].head == :(-->)
end

function _equals_symbol(arg::Symbol, sym::Symbol)
    arg == sym
end
function _equals_symbol(arg::Expr, sym::Symbol) #not sure this method is necessary anymore on 0.7
    arg.head == :quote && arg.args[1] == sym
end
function _equals_symbol(arg::QuoteNode, sym::Symbol)
    arg.value == sym
end
_equals_symbol(x, sym::Symbol) = false

# build an apply_recipe function header from the recipe function header
function get_function_def(func_signature::Expr, args::Vector)
    front = func_signature.args[1]
    if func_signature.head == :where
        Expr(:where, get_function_def(front, args), esc.(func_signature.args[2:end])...)
    elseif func_signature.head == :call
        func = Expr(:call, :(RecipesBase.apply_recipe), esc.([:(plotattributes::Dict{Symbol, Any}); args])...)
        if isa(front, Expr) && front.head == :curly
            Expr(:where, func, esc.(front.args[2:end])...)
        else
            func
        end
    else
        error("Expected `func_signature = ...` with func_signature as a call or where Expr... got: $func_signature")
    end
end

function create_kw_body(func_signature::Expr)
    # get the arg list, stripping out any keyword parameters into a
    # bunch of get!(kw, key, value) lines
    func_signature.head == :where && return create_kw_body(func_signature.args[1])
    args = func_signature.args[2:end]
    kw_body = Expr(:block)
    cleanup_body = Expr(:block)
    if isa(args[1], Expr) && args[1].head == :parameters
        for kwpair in args[1].args
            k, v = kwpair.args
            if isa(k, Expr) && k.head == :(::)
                k = k.args[1]
                @warn("Type annotations on keyword arguments not currently supported in recipes. Type information has been discarded")
            end
            push!(kw_body.args, :($k = get!(plotattributes, $(QuoteNode(k)), $v)))
            push!(cleanup_body.args, :(RecipesBase.is_key_supported($(QuoteNode(k))) || delete!(plotattributes, $(QuoteNode(k)))))
        end
        args = args[2:end]
    end
    args, kw_body, cleanup_body
end

# process the body of the recipe recursively.
# when we see the series macro, we split that block off:
    # let
    #   d2 = copy(d)
    #   <process_recipe_body on d2>
    #   RecipeData(d2, block_return)
    # end
# and we push this block onto the series_blocks list.
# then at the end we push the main body onto the series list
function process_recipe_body!(expr::Expr)
    for (i,e) in enumerate(expr.args)
        if isa(e,Expr)

            # process trailing flags, like:
            #   a --> b, :quiet, :force
            quiet, require, force = false, false, false
            if _is_arrow_tuple(e)
                for flag in e.args
                    if _equals_symbol(flag, :quiet)
                        quiet = true
                    elseif _equals_symbol(flag, :require)
                        require = true
                    elseif _equals_symbol(flag, :force)
                        force = true
                    end
                end
                e = e.args[1]
            end

            # the unused operator `:=` will mean force: `x := 5` is equivalent to `x --> 5, force`
            # note: this means "x is defined as 5"
            if e.head == :(:=)
                force = true
                e.head = :(-->)
            end

            # we are going to recursively swap out `a --> b, flags...` commands
            # note: this means "x may become 5"
            if e.head == :(-->)
                k, v = e.args
                if isa(k, Symbol)
                    k = QuoteNode(k)
                end

                set_expr = if force
                    # forced override user settings
                    :(plotattributes[$k] = $v)
                else
                    # if the user has set this keyword, use theirs
                    :(get!(plotattributes, $k, $v))
                end

                expr.args[i] = if quiet
                    # quietly ignore keywords which are not supported
                    :(RecipesBase.is_key_supported($k) ? $set_expr : nothing)
                elseif require
                    # error when not supported by the backend
                    :(RecipesBase.is_key_supported($k) ? $set_expr : error("In recipe: required keyword ", $k, " is not supported by backend $(backend_name())"))
                else
                    set_expr
                end

            # TODO elseif it's a @series macrocall, add a series block and push to the `series` list

            elseif e.head != :call
                # we want to recursively replace the arrows, but not inside function calls
                # as this might include things like Dict(1=>2)
                process_recipe_body!(e)
            end
        end
    end
end

# --------------------------------------------------------------------------

"""
This handy macro will process a function definition, replace `-->` commands, and
then add a new version of `RecipesBase.apply_recipe` for dispatching on the arguments.

This functionality is primarily geared to turning user types and settings into the
data and attributes that describe a Plots.jl visualization.

Set attributes using the `-->` command, and return a comma separated list of arguments that
should replace the current arguments.

An example:

```
using RecipesBase

# Our custom type that we want to display
type T end

@recipe function plot{N<:Integer}(t::T, n::N = 1; customcolor = :green)
    markershape --> :auto, :require
    markercolor --> customcolor, :force
    xrotation --> 5
    zrotation --> 6, :quiet
    rand(10,n)
end

# ---------------------

# Plots will be the ultimate consumer of our recipe in this example
using Plots; gr()

# This call will implicitly call `RecipesBase.apply_recipe` as part of the Plots
# processing pipeline (see the Pipeline section of the Plots documentation).
# It will plot 5 line plots, all with black circles for markers.
# The markershape argument must be supported, and the zrotation argument's warning
# will be suppressed.  The user can override all arguments except markercolor.
plot(T(), 5; customcolor = :black, shape=:c)
```

In this example, we see lots of the machinery in action.  We create a new type `T` which
we will use for dispatch, and an optional argument `n`, which will be used to determine the
number of series to display.  User-defined keyword arguments are passed through, and the
`-->` command can be trailed by flags:

- quiet:   Suppress unsupported keyword warnings
- require: Error if keyword is unsupported
- force:   Don't allow user override for this keyword
"""
macro recipe(funcexpr::Expr)
    func_signature, func_body = funcexpr.args

    if !(funcexpr.head in (:(=), :function))
        error("Must wrap a valid function call!")
    end
    if !(isa(func_signature, Expr) && func_signature.head in (:call, :where))
        error("Expected `func_signature = ...` with func_signature as a call or where Expr... got: $func_signature")
    end
    if length(func_signature.args) < 2
        error("Missing function arguments... need something to dispatch on!")
    end

    args, kw_body, cleanup_body = create_kw_body(func_signature)
    func = get_function_def(func_signature, args)

    # this is where the receipe func_body is processed
    # replace all the key => value lines with argument setting logic
    # and break up by series.
    process_recipe_body!(func_body)

    # now build a function definition for apply_recipe, wrapping the return value in a tuple if needed.
    # we are creating a vector of RecipeData objects, one per series.
    funcdef = Expr(:function, func, esc(quote
        if RecipesBase._debug_recipes[1]
            println("apply_recipe args: ", $args)
        end
        $kw_body
        $cleanup_body
        series_list = RecipesBase.RecipeData[]
        func_return = $func_body
        if func_return != nothing
            push!(series_list, RecipesBase.RecipeData(plotattributes, RecipesBase.wrap_tuple(func_return)))
        end
        series_list
    end))
    funcdef
end


# --------------------------------------------------------------------------

"""
Meant to be used inside a recipe to add additional RecipeData objects to the list:

```
@recipe function f(::T)
    # everything get this setting
    linecolor --> :red

    @series begin
        # this setting is only for this series
        fillcolor := :green

        # return the args, just like in recipes
        rand(10)
    end

    # this is the main series... though it can be skipped by returning nothing.
    # note: a @series block returns nothing
    rand(100)
end
```
"""
macro series(expr::Expr)
    esc(quote
        let plotattributes = copy(plotattributes)
            args = $expr
            push!(series_list, RecipesBase.RecipeData(plotattributes, RecipesBase.wrap_tuple(args)))
            nothing
        end
    end)
end

# --------------------------------------------------------------------------

"""
You can easily define your own plotting recipes with convenience methods:
```
@userplot GroupHist

@recipe function f(gh::GroupHist)
    # set some attributes, add some series, using gh.args as input
end
# now you can plot like:
grouphist(rand(1000,4))
```
"""
macro userplot(expr)
    _userplot(expr)
end

function _userplot(expr::Expr)
    if expr.head != :struct
        error("Must call userplot on a [mutable] struct expression.  Got: $expr")
    end

    typename = expr.args[2]
    funcname = Symbol(lowercase(string(typename)))
    funcname2 = Symbol(funcname, "!")

    # return a code block with the type definition and convenience plotting methods
    esc(quote
        $expr
        export $funcname, $funcname2
        Core.@__doc__ $funcname(args...; kw...) = RecipesBase.plot($typename(args); kw...)
        Core.@__doc__ $funcname2(args...; kw...) = RecipesBase.plot!($typename(args); kw...)
        Core.@__doc__ $funcname2(plt::RecipesBase.AbstractPlot, args...; kw...) = RecipesBase.plot!(plt, $typename(args); kw...)
    end)
end

function _userplot(sym::Symbol)
    _userplot(:(mutable struct $sym
            args
    end))
end

#----------------------------------------------------------------------------

"""
    @shorthands(funcname::Symbol)

Defines and exports shorthand plotting method definitions (`\$funcname` and `\$funcname!`).
Pass the series type (as a symbol) to the macro.

## Examples

```julia
# define some series type
@recipe function f(::Type{Val{:myseriestype}}, x, y)
    # some implementation here
end
# docstrings are forwarded
\"\"\"
    myseriestype(x, y)
Plot my series type!
\"\"\"
@shorthands myseriestype
```
"""
macro shorthands(funcname::Symbol)
    funcname2 = Symbol(funcname, "!")
    esc(quote
        export $funcname, $funcname2
        Core.@__doc__ $funcname(args...; kw...) = RecipesBase.plot(args...; kw..., seriestype = $(Meta.quot(funcname)))
        Core.@__doc__ $funcname2(args...; kw...) = RecipesBase.plot!(args...; kw..., seriestype = $(Meta.quot(funcname)))
    end)
end

#----------------------------------------------------------------------------

# allow usage of type recipes without depending on StatsPlots

"""
`recipetype(s, args...)`

Use this function to refer to type recipes by their symbol, without taking a dependency.

# Example

```julia
import RecipesBase: recipetype
recipetype(:groupedbar, 1:10, rand(10, 2))
```

instead of

```julia
import StatsPlots: GroupedBar
GroupedBar((1:10, rand(10, 2)))
```
"""
recipetype(s, args...) = recipetype(Val(s), args...)

function recipetype(s::Val{T}, args...) where T
    error("No type recipe defined for type $T. You may need to load StatsPlots")
end

function _precompile_()
    ccall(:jl_generating_output, Cint, ()) == 1 || return nothing
    precompile(Tuple{typeof(RecipesBase.apply_recipe), Base.Dict{Symbol, Any}, Type{Base.Val{:sticks}}, Array{Float64, 1}, Array{Float64, 1}, Nothing})
#     precompile(Tuple{typeof(RecipesBase.apply_recipe), Base.Dict{Symbol, Any}, Type{Plots.SliceIt}, Array{String, 1}, Array{String, 1}, Plots.Surface{Array{Float64, 2}}})
#     precompile(Tuple{typeof(RecipesBase.apply_recipe), Base.Dict{Symbol, Any}, Type{Plots.SliceIt}, Base.StepRangeLen{Float64, Base.TwicePrecision{Float64}, Base.TwicePrecision{Float64}}, Base.StepRangeLen{Float64, Base.TwicePrecision{Float64}, Base.TwicePrecision{Float64}}, Plots.Surface{Array{Float64, 2}}})
#     precompile(Tuple{typeof(RecipesBase.apply_recipe), Base.Dict{Symbol, Any}, Type{Plots.SliceIt}, Base.StepRangeLen{Float64, Base.TwicePrecision{Float64}, Base.TwicePrecision{Float64}}, Array{Float64, 1}, Nothing})
#     precompile(Tuple{typeof(RecipesBase.apply_recipe), Base.Dict{Symbol, Any}, Type{Base.Val{:spy}}, Array{Float64, 1}, Array{Float64, 1}, Plots.Surface{SparseArrays.SparseMatrixCSC{Float64, Int64}}})
#     precompile(Tuple{typeof(RecipesBase.apply_recipe), Base.Dict{Symbol, Any}, Type{Plots.SliceIt}, Array{Float64, 1}, Array{Float64, 1}, Base.UnitRange{Int64}})
#     precompile(Tuple{typeof(RecipesBase.apply_recipe), Base.Dict{Symbol, Any}, Type{Plots.SliceIt}, Nothing, Array{Float64, 1}, Nothing})
#     precompile(Tuple{typeof(RecipesBase.apply_recipe), Base.Dict{Symbol, Any}, Type{Plots.SliceIt}, Array{Float64, 1}, Array{Float64, 1}, Nothing})
#     precompile(Tuple{typeof(RecipesBase.apply_recipe), Base.Dict{Symbol, Any}, Type{Plots.SliceIt}, Nothing, Array{Array{T, 1} where T, 1}, Nothing})
    precompile(Tuple{typeof(RecipesBase.apply_recipe), Base.Dict{Symbol, Any}, Type{Base.Val{:bar}}, Base.UnitRange{Int64}, Array{Float64, 1}, Nothing})
#     precompile(Tuple{typeof(RecipesBase.apply_recipe), Base.Dict{Symbol, Any}, Type{Plots.SliceIt}, Array{Array{T, 1} where T, 1}, Array{Float64, 2}, Nothing})
    precompile(Tuple{typeof(RecipesBase.apply_recipe), Base.Dict{Symbol, Any}, Type{Base.Val{:bar}}, Array{Float64, 1}, Array{Float64, 1}, Nothing})
    precompile(Tuple{typeof(RecipesBase.apply_recipe), Base.Dict{Symbol, Any}, Type{String}, String})
    precompile(Tuple{typeof(RecipesBase.apply_recipe), Base.Dict{Symbol, Any}, Type{Base.Val{:vline}}, Base.UnitRange{Int64}, Array{Float64, 1}, Nothing})
#     precompile(Tuple{typeof(RecipesBase.apply_recipe), Base.Dict{Symbol, Any}, Type{Plots.SliceIt}, Array{String, 1}, Array{Float64, 1}, Nothing})
    precompile(Tuple{typeof(RecipesBase.apply_recipe), Base.Dict{Symbol, Any}, Type{Base.Val{:barhist}}, Base.UnitRange{Int64}, Array{Float64, 1}, Nothing})
#     precompile(Tuple{typeof(RecipesBase.apply_recipe), Base.Dict{Symbol, Any}, Type{Plots.SliceIt}, Base.StepRangeLen{Float64, Base.TwicePrecision{Float64}, Base.TwicePrecision{Float64}}, Array{Float64, 2}, Nothing})
#     precompile(Tuple{typeof(RecipesBase.apply_recipe), Base.Dict{Symbol, Any}, Type{Plots.SliceIt}, Nothing, Array{Int64, 1}, Nothing})
#     precompile(Tuple{typeof(RecipesBase.apply_recipe), Base.Dict{Symbol, Any}, Plots.PortfolioComposition})
    precompile(Tuple{typeof(RecipesBase.apply_recipe), Base.Dict{Symbol, Any}, Type{Base.Val{:histogram2d}}, Array{Float64, 1}, Array{Float64, 1}, Nothing})
# #     precompile(Tuple{typeof(RecipesBase.apply_recipe), Base.Dict{Symbol, Any}, Type{Plots.SliceIt}, Base.UnitRange{Int64}, Base.UnitRange{Int64}, Plots.Surface{Array{ColorTypes.RGBA{FixedPointNumbers.Normed{UInt8, 8}}, 2}}})
#     precompile(Tuple{typeof(RecipesBase.apply_recipe), Base.Dict{Symbol, Any}, Plots.Spy})
#     precompile(Tuple{typeof(RecipesBase.apply_recipe), Base.Dict{Symbol, Any}, Type{Plots.SliceIt}, Base.UnitRange{Int64}, Base.UnitRange{Int64}, Plots.Surface{SparseArrays.SparseMatrixCSC{Int64, Int64}}})
#     precompile(Tuple{typeof(RecipesBase.apply_recipe), Base.Dict{Symbol, Any}, Type{Plots.SliceIt}, Base.UnitRange{Int64}, Base.UnitRange{Int64}, Plots.Surface{SparseArrays.SparseMatrixCSC{Float64, Int64}}})
#     precompile(Tuple{typeof(RecipesBase.apply_recipe), Base.Dict{Symbol, Any}, Type{Plots.SliceIt}, Base.StepRange{Int64, Int64}, Array{Float64, 2}, Nothing})
#     precompile(Tuple{typeof(RecipesBase.apply_recipe), Base.Dict{Symbol, Any}, Type{Plots.SliceIt}, Array{Float64, 1}, Array{Function, 1}, Nothing})
#     precompile(Tuple{typeof(RecipesBase.apply_recipe), Base.Dict{Symbol, Any}, Type{Plots.SliceIt}, Array{Array{Float64, 1}, 1}, Array{Array{Float64, 1}, 1}, Nothing})
    precompile(Tuple{typeof(RecipesBase.apply_recipe), Base.Dict{Symbol, Any}, Type{Base.Val{:steppost}}, Array{Float64, 1}, Array{Float64, 1}, Nothing})
#     precompile(Tuple{typeof(RecipesBase.apply_recipe), Base.Dict{Symbol, Any}, typeof(Base.sin), getfield(Main, Symbol("#3#4")), Int64, Float64, Int64})
    precompile(Tuple{typeof(RecipesBase.apply_recipe), Base.Dict{Symbol, Any}, Array{Float64, 1}, Array{Float64, 1}})
    precompile(Tuple{typeof(RecipesBase.apply_recipe), Base.Dict{Symbol, Any}, Base.StepRangeLen{Float64, Base.TwicePrecision{Float64}, Base.TwicePrecision{Float64}}, Base.StepRangeLen{Float64, Base.TwicePrecision{Float64}, Base.TwicePrecision{Float64}}, Array{Float64, 2}})
    precompile(Tuple{typeof(RecipesBase.apply_recipe), Base.Dict{Symbol, Any}, Base.StepRangeLen{Float64, Base.TwicePrecision{Float64}, Base.TwicePrecision{Float64}}, Array{Float64, 2}})
    precompile(Tuple{typeof(RecipesBase.apply_recipe), Base.Dict{Symbol, Any}, Base.StepRangeLen{Float64, Base.TwicePrecision{Float64}, Base.TwicePrecision{Float64}}, Array{Float64, 1}})
    precompile(Tuple{typeof(RecipesBase.apply_recipe), Base.Dict{Symbol, Any}, Array{Array{T, 1} where T, 1}})
    precompile(Tuple{typeof(RecipesBase.apply_recipe), Base.Dict{Symbol, Any}, Array{String, 1}, Array{String, 1}, Array{Float64, 2}})
    precompile(Tuple{typeof(RecipesBase.apply_recipe), Base.Dict{Symbol, Any}, Base.StepRange{Int64, Int64}, Array{Float64, 2}})
#     precompile(Tuple{typeof(RecipesBase.apply_recipe), Base.Dict{Symbol, Any}, Type{Base.Val{:bins2d}}, Base.StepRangeLen{Float64, Base.TwicePrecision{Float64}, Base.TwicePrecision{Float64}}, Base.StepRangeLen{Float64, Base.TwicePrecision{Float64}, Base.TwicePrecision{Float64}}, Plots.Surface{Array{Float64, 2}}})
    precompile(Tuple{typeof(RecipesBase.apply_recipe), Base.Dict{Symbol, Any}, Base.StepRangeLen{Float64, Base.TwicePrecision{Float64}, Base.TwicePrecision{Float64}}, Base.StepRangeLen{Float64, Base.TwicePrecision{Float64}, Base.TwicePrecision{Float64}}, typeof(identity)})
    precompile(Tuple{typeof(RecipesBase.apply_recipe), Base.Dict{Symbol, Any}, Array{Float64, 1}, Array{Function, 1}})
    precompile(Tuple{typeof(RecipesBase.apply_recipe), Base.Dict{Symbol, Any}, Array{Float64, 1}})
    precompile(Tuple{typeof(RecipesBase.apply_recipe), Base.Dict{Symbol, Any}, Array{Int64, 1}})
#     precompile(Tuple{typeof(RecipesBase.apply_recipe), Base.Dict{Symbol, Any}, Type{Plots.SliceIt}, Nothing, Array{Float64, 2}, Nothing})
    precompile(Tuple{typeof(RecipesBase.apply_recipe), Base.Dict{Symbol, Any}, Type{Base.Val{:histogram}}, Base.UnitRange{Int64}, Array{Float64, 1}, Nothing})
    precompile(Tuple{typeof(RecipesBase.apply_recipe), Base.Dict{Symbol, Any}, Array{Array{T, 1} where T, 1}, Array{Float64, 2}})
    precompile(Tuple{typeof(RecipesBase.apply_recipe), Base.Dict{Symbol, Any}, Array{Float64, 1}, Array{Float64, 1}, Base.UnitRange{Int64}})
    precompile(Tuple{typeof(RecipesBase.apply_recipe), Base.Dict{Symbol, Any}, Array{Function, 1}, Array{Float64, 1}})
    precompile(Tuple{typeof(RecipesBase.apply_recipe), Base.Dict{Symbol, Any}, Array{Float64, 2}})
    precompile(Tuple{typeof(RecipesBase.apply_recipe), Base.Dict{Symbol, Any}, Type{Base.Val{:steppre}}, Array{Float64, 1}, Array{Float64, 1}, Nothing})
    precompile(Tuple{typeof(RecipesBase.apply_recipe), Base.Dict{Symbol, Any}, Type{typeof(Base.sin)}, typeof(Base.sin)})
    precompile(Tuple{typeof(RecipesBase.apply_recipe), Base.Dict{Symbol, Any}, Type{Array{Float64, 1}}, Array{Float64, 1}})
    precompile(Tuple{typeof(RecipesBase.apply_recipe), Base.Dict{Symbol, Any}, Type{Base.Val{:hline}}, Base.UnitRange{Int64}, Array{Float64, 1}, Nothing})
#     precompile(Tuple{getfield(RecipesBase, Symbol("#plot!##kw")), NamedTuple{(:title,), Tuple{String}}, typeof(RecipesBase.plot!)})
#     precompile(Tuple{getfield(RecipesBase, Symbol("#plot##kw")), NamedTuple{(:lab, :w, :palette, :fill, :α), Tuple{String, Int64, Symbol, Int64, Float64}}, typeof(RecipesBase.plot), Base.StepRange{Int64, Int64}, Array{Float64, 2}})
#     precompile(Tuple{typeof(RecipesBase.apply_recipe), Base.Dict{Symbol, Any}, Type{Base.Val{:bar}}, Plots.Plot{Plots.GRBackend}})
#     precompile(Tuple{getfield(RecipesBase, Symbol("#plot!##kw")), NamedTuple{(:marker, :series_annotations, :seriestype), Tuple{Tuple{Int64, Float64, Symbol}, Array{Any, 1}, Symbol}}, typeof(RecipesBase.plot!), Base.StepRangeLen{Float64, Base.TwicePrecision{Float64}, Base.TwicePrecision{Float64}}, Array{Float64, 1}})
#     precompile(Tuple{getfield(RecipesBase, Symbol("#plot##kw")), NamedTuple{(:proj, :m), Tuple{Symbol, Int64}}, typeof(RecipesBase.plot), Base.StepRangeLen{Float64, Base.TwicePrecision{Float64}, Base.TwicePrecision{Float64}}, Array{Float64, 1}})
# # #     precompile(Tuple{getfield(RecipesBase, Symbol("#plot##kw")), NamedTuple{(:line, :marker, :bg, :fg, :xlim, :ylim, :leg), Tuple{Tuple{Int64, Symbol, Symbol}, Tuple{Plots.Shape, Int64, ColorTypes.RGBA{Float64}}, Symbol, Symbol, Tuple{Int64, Int64}, Tuple{Int64, Int64}, Bool}}, typeof(RecipesBase.plot), Base.StepRangeLen{Float64, Base.TwicePrecision{Float64}, Base.TwicePrecision{Float64}}, Array{Float64, 1}})
#     precompile(Tuple{typeof(RecipesBase.plot!), Plots.Plot{Plots.GRBackend}, Array{Float64, 2}})
#     precompile(Tuple{getfield(RecipesBase, Symbol("#plot##kw")), NamedTuple{(:leg,), Tuple{Bool}}, typeof(RecipesBase.plot), Array{Function, 1}, Array{Float64, 1}})
# #     precompile(Tuple{getfield(RecipesBase, Symbol("#plot##kw")), NamedTuple{(:layout, :label, :fillrange, :fillalpha), Tuple{Tuple{Int64, Int64}, String, Int64, Float64}}, typeof(RecipesBase.plot), Plots.Plot{Plots.GRBackend}, Plots.Plot{Plots.GRBackend}, Plots.Plot{Plots.GRBackend}})
#     precompile(Tuple{getfield(RecipesBase, Symbol("#plot##kw")), NamedTuple{(:line_z, :linewidth, :legend), Tuple{Base.StepRangeLen{Float64, Base.TwicePrecision{Float64}, Base.TwicePrecision{Float64}}, Int64, Bool}}, typeof(RecipesBase.plot), Array{Float64, 1}, Array{Float64, 1}})
#     precompile(Tuple{getfield(RecipesBase, Symbol("#plot!##kw")), NamedTuple{(:markersize, :c, :seriestype), Tuple{Int64, Symbol, Symbol}}, typeof(RecipesBase.plot!), Array{Float64, 1}})
#     precompile(Tuple{getfield(RecipesBase, Symbol("#plot##kw")), NamedTuple{(:line, :lab, :ms), Tuple{Tuple{Array{Symbol, 2}, Int64}, Array{String, 2}, Int64}}, typeof(RecipesBase.plot), Array{Array{T, 1} where T, 1}, Array{Float64, 2}})
#     precompile(Tuple{getfield(RecipesBase, Symbol("#plot##kw")), NamedTuple{(:fill, :seriestype), Tuple{Bool, Symbol}}, typeof(RecipesBase.plot), Base.StepRangeLen{Float64, Base.TwicePrecision{Float64}, Base.TwicePrecision{Float64}}, Base.StepRangeLen{Float64, Base.TwicePrecision{Float64}, Base.TwicePrecision{Float64}}, Int})
# #     precompile(Tuple{getfield(RecipesBase, Symbol("#plot!##kw")), NamedTuple{(:markersize, :c, :seriestype), Tuple{Int64, Symbol, Symbol}}, typeof(RecipesBase.plot!), Plots.Plot{Plots.GRBackend}, Array{Float64, 1}})
# #     precompile(Tuple{getfield(RecipesBase, Symbol("#plot##kw")), NamedTuple{(:zcolor, :m, :leg, :cbar, :w), Tuple{Base.StepRange{Int64, Int64}, Tuple{Int64, Float64, Symbol, Plots.Stroke}, Bool, Bool, Int64}}, typeof(RecipesBase.plot), Array{Float64, 1}, Array{Float64, 1}, Int})
#     precompile(Tuple{getfield(RecipesBase, Symbol("#plot!##kw")), NamedTuple{(:w,), Tuple{Int64}}, typeof(RecipesBase.plot!), Array{Float64, 1}, Array{Float64, 1}, Int})
#     precompile(Tuple{getfield(RecipesBase, Symbol("#plot!##kw")), NamedTuple{(:seriestype,), Tuple{Symbol}}, typeof(RecipesBase.plot!), Array{Int64, 1}})
#     precompile(Tuple{getfield(RecipesBase, Symbol("#plot##kw")), NamedTuple{(:line, :leg, :fill), Tuple{Int64, Bool, Tuple{Int64, Symbol}}}, typeof(RecipesBase.plot), typeof(identity), typeof(identity), Int})
# #     precompile(Tuple{getfield(RecipesBase, Symbol("#plot!##kw")), NamedTuple{(:w,), Tuple{Int64}}, typeof(RecipesBase.plot!), Plots.Plot{Plots.GRBackend}, Array{Float64, 1}, Int})
#     precompile(Tuple{getfield(RecipesBase, Symbol("#plot##kw")), NamedTuple{(:aspect_ratio, :seriestype), Tuple{Int64, Symbol}}, typeof(RecipesBase.plot), Array{String, 1}, Array{String, 1}, Int})
# #     precompile(Tuple{getfield(RecipesBase, Symbol("#plot!##kw")), NamedTuple{(:line, :seriestype), Tuple{Tuple{Int64, Symbol, Float64, Array{Symbol, 2}}, Symbol}}, typeof(RecipesBase.plot!), Plots.Plot{Plots.GRBackend}, Array{Float64, 2}})
# #     precompile(Tuple{getfield(RecipesBase, Symbol("#plot!##kw")), NamedTuple{(:marker, :series_annotations, :seriestype), Tuple{Tuple{Int64, Float64, Symbol}, Array{Any, 1}, Symbol}}, typeof(RecipesBase.plot!), Plots.Plot{Plots.GRBackend}, Base.StepRangeLen{Float64, Base.TwicePrecision{Float64}, Base.TwicePrecision{Float64}}, Int})
#     precompile(Tuple{getfield(RecipesBase, Symbol("#plot!##kw")), NamedTuple{(:yaxis,), Tuple{Tuple{String, Symbol}}}, typeof(RecipesBase.plot!)})
#     precompile(Tuple{getfield(RecipesBase, Symbol("#plot!##kw")), NamedTuple{(:line, :seriestype), Tuple{Tuple{Int64, Symbol, Float64, Array{Symbol, 2}}, Symbol}}, typeof(RecipesBase.plot!), Array{Float64, 2}})
    precompile(Tuple{typeof(RecipesBase.apply_recipe), Base.Dict{Symbol, Any}, Array{Array{Float64, 1}, 1}, Array{Array{Float64, 1}, 1}})
# #     precompile(Tuple{getfield(RecipesBase, Symbol("#plot!##kw")), NamedTuple{(:annotation,), Tuple{Array{Tuple{Int64, Float64, Plots.PlotText}, 1}}}, typeof(RecipesBase.plot!)})
#     precompile(Tuple{typeof(RecipesBase.apply_recipe), Base.Dict{Symbol, Any}, Type{Base.Val{:histogram}}, Plots.Plot{Plots.GRBackend}})
#     precompile(Tuple{typeof(RecipesBase.apply_recipe), Base.Dict{Symbol, Any}, Type{Base.Val{:histogram2d}}, Plots.Plot{Plots.GRBackend}})
#     precompile(Tuple{typeof(RecipesBase.apply_recipe), Base.Dict{Symbol, Any}, Type{Base.Val{:shape}}, Plots.Plot{Plots.GRBackend}})
#     precompile(Tuple{typeof(RecipesBase.apply_recipe), Base.Dict{Symbol, Any}, Type{Base.Val{:scatter}}, Plots.Plot{Plots.GRBackend}})
    precompile(Tuple{typeof(RecipesBase.plot!), Array{Float64, 2}})
# #     precompile(Tuple{getfield(RecipesBase, Symbol("#plot!##kw")), NamedTuple{(:zcolor, :m, :ms, :lab, :seriestype), Tuple{Array{Float64, 1}, Tuple{Symbol, Float64, Plots.Stroke}, Array{Float64, 1}, String, Symbol}}, typeof(RecipesBase.plot!), Array{Float64, 1}})
#     precompile(Tuple{typeof(RecipesBase.apply_recipe), Base.Dict{Symbol, Any}, Type{Base.Val{:hline}}, Plots.Plot{Plots.GRBackend}})
#     precompile(Tuple{typeof(RecipesBase.apply_recipe), Base.Dict{Symbol, Any}, Type{Base.Val{:path}}, Plots.Plot{Plots.GRBackend}})
#     precompile(Tuple{typeof(RecipesBase.apply_recipe), Base.Dict{Symbol, Any}, Type{Base.Val{:spy}}, Plots.Plot{Plots.GRBackend}})
#     precompile(Tuple{typeof(RecipesBase.apply_recipe), Base.Dict{Symbol, Any}, Type{Base.Val{:pie}}, Plots.Plot{Plots.GRBackend}})
#     precompile(Tuple{typeof(RecipesBase.apply_recipe), Base.Dict{Symbol, Any}, Type{Base.Val{:heatmap}}, Plots.Plot{Plots.GRBackend}})
#     precompile(Tuple{typeof(RecipesBase.apply_recipe), Base.Dict{Symbol, Any}, Type{Base.Val{:vline}}, Plots.Plot{Plots.GRBackend}})
#     precompile(Tuple{typeof(RecipesBase.apply_recipe), Base.Dict{Symbol, Any}, Type{Base.Val{:contour}}, Plots.Plot{Plots.GRBackend}})
#     precompile(Tuple{typeof(RecipesBase.apply_recipe), Base.Dict{Symbol, Any}, Type{Base.Val{:image}}, Plots.Plot{Plots.GRBackend}})
    precompile(Tuple{typeof(RecipesBase.apply_recipe), Base.Dict{Symbol, Any}, Array{String, 1}, Array{Float64, 1}})
#     precompile(Tuple{getfield(RecipesBase, Symbol("#plot##kw")), NamedTuple{(:seriestype,), Tuple{Symbol}}, typeof(RecipesBase.plot), Base.StepRangeLen{Float64, Base.TwicePrecision{Float64}, Base.TwicePrecision{Float64}}, Base.StepRangeLen{Float64, Base.TwicePrecision{Float64}, Base.TwicePrecision{Float64}}, Int})
#     precompile(Tuple{typeof(RecipesBase.apply_recipe), Base.Dict{Symbol, Any}, typeof(Base.sin), getfield(Main, Symbol("#3#4")), Base.StepRangeLen{Float64, Base.TwicePrecision{Float64}, Base.TwicePrecision{Float64}}})
    precompile(Tuple{typeof(RecipesBase.apply_recipe), Base.Dict{Symbol, Any}, Type{Base.Val{:barbins}}, Base.StepRangeLen{Float64, Base.TwicePrecision{Float64}, Base.TwicePrecision{Float64}}, Array{Float64, 1}, Nothing})
#     precompile(Tuple{typeof(RecipesBase.apply_recipe), Base.Dict{Symbol, Any}, Array{Plots.OHLC{T} where T<:Real, 1}})
#     precompile(Tuple{typeof(RecipesBase.apply_recipe), Base.Dict{Symbol, Any}, Array{ColorTypes.RGBA{FixedPointNumbers.Normed{UInt8, 8}}, 2}})
#     precompile(Tuple{getfield(RecipesBase, Symbol("#plot##kw")), NamedTuple{(:m, :lab, :bg, :xlim, :ylim, :seriestype), Tuple{Tuple{Int64, Symbol}, Array{String, 2}, Symbol, Tuple{Int64, Int64}, Tuple{Int64, Int64}, Symbol}}, typeof(RecipesBase.plot), Base.StepRangeLen{Float64, Base.TwicePrecision{Float64}, Base.TwicePrecision{Float64}}, Array{Float64, 2}})
# #     precompile(Tuple{getfield(RecipesBase, Symbol("#plot##kw")), NamedTuple{(:xaxis, :background_color, :leg), Tuple{Tuple{String, Tuple{Int64, Int64}, Base.StepRange{Int64, Int64}, Symbol}, ColorTypes.RGB{Float64}, Bool}}, typeof(RecipesBase.plot), Array{Float64, 2}})
#     precompile(Tuple{getfield(RecipesBase, Symbol("#plot##kw")), NamedTuple{(:framestyle, :title, :color, :layout, :label, :markerstrokewidth, :ticks, :seriestype), Tuple{Array{Symbol, 2}, Array{String, 2}, Base.ReshapedArray{Int64, 2, Base.UnitRange{Int64}, Tuple{}}, Int64, String, Int64, Base.UnitRange{Int64}, Symbol}}, typeof(RecipesBase.plot), Array{Array{Float64, 1}, 1}, Array{Array{Float64, 1}, 1}})
# #     precompile(Tuple{getfield(RecipesBase, Symbol("#plot##kw")), NamedTuple{(:title,), Tuple{Array{String, 2}}}, typeof(RecipesBase.plot), Plots.Plot{Plots.GRBackend}, Plots.Plot{Plots.GRBackend}})
# #     precompile(Tuple{getfield(RecipesBase, Symbol("#plot##kw")), NamedTuple{(:color, :line, :marker), Tuple{Array{Symbol, 2}, Tuple{Symbol, Int64}, Tuple{Array{Symbol, 2}, Int64, Float64, Plots.Stroke}}}, typeof(RecipesBase.plot), Array{Array{T, 1} where T, 1}})
# #     precompile(Tuple{getfield(RecipesBase, Symbol("#plot##kw")), NamedTuple{(:title,), Tuple{String}}, typeof(RecipesBase.plot), Plots.Plot{Plots.GRBackend}})
#     precompile(Tuple{typeof(RecipesBase.plot), Array{ColorTypes.RGBA{FixedPointNumbers.Normed{UInt8, 8}}, 2}})
#     precompile(Tuple{getfield(RecipesBase, Symbol("#plot##kw")), NamedTuple{(:title, :l, :seriestype, :aspect_ratio, :grid, :xticks, :yticks), Tuple{String, Float64, Symbol, Symbol, Bool, Nothing, Nothing}}, typeof(RecipesBase.plot), Array{String, 1}, Array{Float64, 1}})
#     precompile(Tuple{getfield(RecipesBase, Symbol("#plot##kw")), NamedTuple{(:title,), Tuple{String}}, typeof(RecipesBase.plot), Array{Float64, 1}})
# #     precompile(Tuple{getfield(RecipesBase, Symbol("#plot!##kw")), NamedTuple{(:yaxis,), Tuple{Tuple{String, Symbol}}}, typeof(RecipesBase.plot!), Plots.Plot{Plots.GRBackend}})
#     precompile(Tuple{typeof(RecipesBase.plot), Plots.Plot{Plots.GRBackend}, Plots.Plot{Plots.GRBackend}})
# #     precompile(Tuple{getfield(RecipesBase, Symbol("#plot!##kw")), NamedTuple{(:annotation,), Tuple{Array{Tuple{Int64, Float64, Plots.PlotText}, 1}}}, typeof(RecipesBase.plot!), Plots.Plot{Plots.GRBackend}})
#     precompile(Tuple{getfield(RecipesBase, Symbol("#plot##kw")), NamedTuple{(:w,), Tuple{Int64}}, typeof(RecipesBase.plot), Array{Float64, 2}})
#     precompile(Tuple{getfield(RecipesBase, Symbol("#plot##kw")), NamedTuple{(:marker_z, :color, :legend, :seriestype), Tuple{getfield(Main, Symbol("#13#14")), Symbol, Bool, Symbol}}, typeof(RecipesBase.plot), Array{Float64, 1}, Array{Float64, 1}})
#     precompile(Tuple{getfield(RecipesBase, Symbol("#plot##kw")), NamedTuple{(:line, :label, :legendtitle), Tuple{Tuple{Int64, Array{Symbol, 2}}, Array{String, 2}, String}}, typeof(RecipesBase.plot), Array{Float64, 2}})
# #     precompile(Tuple{getfield(RecipesBase, Symbol("#plot##kw")), NamedTuple{(:annotations, :leg), Tuple{Tuple{Int64, Float64, Plots.PlotText}, Bool}}, typeof(RecipesBase.plot), Array{Float64, 1}})
# #     precompile(Tuple{getfield(RecipesBase, Symbol("#plot!##kw")), NamedTuple{(:xgrid,), Tuple{Tuple{Symbol, Symbol, Int64, Symbol, Float64}}}, typeof(RecipesBase.plot!), Plots.Plot{Plots.GRBackend}})
# #     precompile(Tuple{getfield(RecipesBase, Symbol("#plot!##kw")), NamedTuple{(:seriestype,), Tuple{Symbol}}, typeof(RecipesBase.plot!), Plots.Plot{Plots.GRBackend}, Array{Int64, 1}})
# #     precompile(Tuple{getfield(RecipesBase, Symbol("#plot!##kw")), NamedTuple{(:zcolor, :m, :ms, :lab, :seriestype), Tuple{Array{Float64, 1}, Tuple{Symbol, Float64, Plots.Stroke}, Array{Float64, 1}, String, Symbol}}, typeof(RecipesBase.plot!), Plots.Plot{Plots.GRBackend}, Array{Float64, 1}})
#     precompile(Tuple{getfield(RecipesBase, Symbol("#plot##kw")), NamedTuple{(:nbins, :seriestype), Tuple{Int64, Symbol}}, typeof(RecipesBase.plot), Array{Float64, 1}, Array{Float64, 1}})
#     precompile(Tuple{getfield(RecipesBase, Symbol("#plot##kw")), NamedTuple{(:reg, :fill), Tuple{Bool, Tuple{Int64, Symbol}}}, typeof(RecipesBase.plot), Array{Float64, 1}})
# #     precompile(Tuple{getfield(RecipesBase, Symbol("#plot!##kw")), NamedTuple{(:title,), Tuple{String}}, typeof(RecipesBase.plot!), Plots.Plot{Plots.GRBackend}})
#     precompile(Tuple{getfield(RecipesBase, Symbol("#plot##kw")), NamedTuple{(:layout, :palette, :bg_inside), Tuple{Int64, Array{Symbol, 2}, Array{Symbol, 2}}}, typeof(RecipesBase.plot), Array{Float64, 2}})
#     precompile(Tuple{getfield(RecipesBase, Symbol("#plot##kw")), NamedTuple{(:grid, :title), Tuple{Tuple{Symbol, Symbol, Symbol, Int64, Float64}, String}}, typeof(RecipesBase.plot), Array{Float64, 1}})
#     precompile(Tuple{typeof(RecipesBase.apply_recipe), Base.Dict{Symbol, Any}, typeof(Base.sin), getfield(Main, Symbol("#3#4")), Int64, Float64})
end
_precompile_()

end # module
