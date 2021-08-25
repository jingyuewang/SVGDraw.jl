# A simple style processor
#using PrettyPrint  # for debug
using OrderedCollections
# To find the position and size of an element
#
# 1. get its x, y, w, h properties.
# 2. check element `position` property in the style context to see if it is `relative`.
#    If it is, create relative lengths, otherwise, get the unit from the global
#    setting and create absolute length.
# 3. check its alignment in the style context, if there is alignment property,
#    create an attribute.
# 4. will do margin and padding later...
abstract type VGElement end

PVDict= OrderedDict{String, String}  # Dict of (property=>value)
SPVDict = OrderedDict{String, OrderedDict{String, String}} # selector => PVDict
CssRules = OrderedDict{String, String}  # selector => one CSS rule in string

#TODO: complete this dictionary
const default_pvd = OrderedDict([
    ("fill", "none"),
    ("stroke", "black"),
    ("stroke-width", "1"),
    ("--vg-unit", "px"),
    ("--vg-showlabel", "true"),
    ("--vg-suppress", "false"),
    ("--vg-shape", "bracket"),
    ("--vg-offset", "0.2"),
    ("--vg-distance", "5"),
    ("--vg-side", "above"),
    ("--vg-brace-height", "5"),
    ("--vg-brace-stroke", "blue"),
    ("--vg-brace-stroke-width", "1"),
    ("--vg-label-side", "above"),
    ("--vg-label-anchor", "middle"),
    ("--vg-label-distance", "8"),
    ("--vg-label-font-size", "18"),
    ("--vg-inner-center-vshift", "-2")            # hack
   ])

# minimum set of properties needed for drawing rect, circle, polyline, etc
const supp_properties = OrderedDict([
    ("fill", default_pvd["fill"]),
    ("stroke", default_pvd["stroke"]),
    ("stroke-width", default_pvd["stroke-width"])
    ])

# we strip leading and trailing whitespaces for a property name and values, but does not
# consolidate inner whitespaces...
function cssrules_to_spvdict(css::CssRules)
    styles = SPVDict()

    for k in keys(css)
        rule_list = split(strip(css[k]), ';')
        # NOTE: empty string could appear in rule_list, e.g., the splitting
        # result of "color:red;"
        pvd = OrderedDict{String, String}()
        for r in rule_list
            isempty(r) && continue
            keyval_pair = split(r, ':')
            length(keyval_pair) == 1 && error("Invalid CSS format")
            #if property keyval_pair[1] already exists in rdict, the last one will be the
            #finally accepted one
            pvd[strip(keyval_pair[1])] = strip(keyval_pair[2])
            # keyval_pair[2] is a string containing space-separated values
            # for example "solid black" (for a property such as `border`)
            # that can be further splitted by `split(str, r" +")`
        end
        # FIXME: what if k already exits in styles? we should merge instead of
        # replace
        styles[k] = pvd
    end
    return styles
end

# TODO: support combinator selectors
function calculate_specificity(e::VGElement, selector::String)
    sels = split(strip(selector), r"\s+")
    if length(sels) > 1
        error("We can only process simple selectors so far:), ", @__FILE__, " ", @__LINE__)
    end
  
    # we only compare simple selectors with `sels[1]` for now
    #(string(typeof(e)) == string(@__MODULE__)*"."*sels[1]) && return 1
    (string(typeof(e)) == sels[1]) && return 1
    e.class != "" && "."*e.class == sels[1] && return 10
    e.id != "" && "#"*e.id == sels[1] && return 100
    return 0
end

# Not too ambitious at this time. Just extract all **custom** CSS rules for `el`
# and collect them in a property-value dictionary
function get_style_PVD(e::VGElement, spv_dict::SPVDict)
    fin_dict = PVDict()
    specificity = OrderedDict{String, Int}()
    for selector in keys(spv_dict)
        s = calculate_specificity(e, selector)
        s == 0 && continue
        pv_dict = spv_dict[selector]
        for p in keys(pv_dict)
            if !(p in keys(fin_dict)) || specificity[p] <= s
                fin_dict[p] = pv_dict[p]
                specificity[p] = s
            end
        end
    end
    return fin_dict
end

function get_property(pvd::PVDict, p::String, v::String="")
    global default_pvd

    haskey(pvd, p) && return pvd[p]
    haskey(default_pvd, p) && return default_pvd[p]
    !isempty(v) && return v
    return ""
end

function isnumber(s::String)
    dots, others= 0, 0, 0
    for c in s
        if isdigit(c); continue; end
        if c == '.'; dots += 1; continue; end
        others += 1
        return false
    end
    return dots == 1
end

# return a numerical value, with unit converted to px
# + a pure number input is converted to a Float64 and returned
# + a number with unit `px` is stripped off of the `px`, converted to a Float64, and
#   returned
# TODO: actually units other than `px` are not supported for now.
function get_numeric_px(val::String)
    val = strip(val)
    length(val) <= 2 && error("invalid numerical format")
    # a pure number is converted to a Float64 and returned
    isnumber(val) && return parse(Float64, val)
    unit = val[end-1:end]
    num = val[1:end-2]
    !isnumber(num) && error("invalid numerical format")
    unit == "px" && return parse(Float64, num)
    error("length units other than `px` are not supported!") 
end

const tag_of = Dict([
    (:Box, "rect"),
    (:Circ, "circle"),
    (:Polyline, "polyline")
    ])

# convert a single rule dictionary of (propert=>value)s into a CSS rule string
function stringify(pvd::PVDict)
    css = ""
    for property in keys(pvd)
        css *= property*':'*pvd[property]*';'
    end
    return css 
end

# Remove all **custom** properties and convert one PVDict `pvd` into a CSS rule string.
function nstringify(pvd::PVDict)
    css = ""
    for property in keys(pvd)
        (length(property) > 5 && property[1:5] == "--vg-") && continue
        css *= property*':'*pvd[property]*';'
    end
    return css
end

# Remove all custom properties and convert a set of PVDicts into a CSS string
function nstringify_css(spvd::SPVDict)
    css = ""
    for selector in keys(spvd)
        css *= selector*'{'*nstringify(spvd[selector])*'}'
    end
    return css 
end


