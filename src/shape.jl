# Difference between SVGDraw and Luxor:
#   1. svgdraw does not depend on cairo or any other rendering library
#   2. svgdraw can do incremental drawing (in a standalone figure window)
#   3. svgdraw is more oriented toward math drawing, less toward artistic drawing
#   4. svgdraw provides a MetaPost-inspired interface
#
# Warning! we use canvas-coordinate system which flips the y-axis of
# the canonical cartesion-coordinate system
#using PrettyPrint        # for debug
import Base: convert, +, -
import Base: setproperty!, getproperty

# settings
const allow_outer_stylesheet = false

mutable struct R²
    x::Float64
    y::Float64
end

mutable struct R³
    x::Float64
    y::Float64
    z::Float64
end

mutable struct R⁴
    x::Float64
    y::Float64
    w::Float64
    h::Float64
end
BoundingBox = R⁴

convert(::Type{R²}, x::Tuple{Real, Real}) = R²(Float64(x[1]), Float64(x[2]))
convert(::Type{R³}, x::Tuple{Real, Real, Real}) = R³(Float64.(x)...)

+(a::R², b::R²) = R²(a.x + b.x, a.y + b.y)
+(a::R², b::Tuple{Real, Real}) = a + convert(R², b)
+(a::R³, b::R³) = R³(a.x + b.x, a.y + b.y, a.z + b.z)

-(a::R², b::R²) = R²(a.x - b.x, a.y - b.y)
-(a::R³, b::R³) = R³(a.x - b.x, a.y - b.y)

mutable struct Attribute
    id::String
    class::String
    label::String
    isUpdated::Bool     # if the position is up-to-date
end
Attribute(id::String, class::String, label::String) = Attribute(id, class, label, true)

mutable struct Rect <: VGElement
    x::Float64
    y::Float64
    w::Float64
    h::Float64
    a::Union{Attribute, Nothing}
end

function Rect(x::Real, y::Real, w::Real, h::Real; id="", class="", label="")
    x, y, w, h = Float64.((x, y, w, h))
    return Rect(x, y, w, h, Attribute(id, class, label))
end

mutable struct Circ <: VGElement
    x::Float64
    y::Float64
    r::Float64
    a::Union{Attribute, Nothing}
end

function Circ(x::Real, y::Real, r::Real; id="", class="", label="")
    x, y, r = Float64.((x, y, r))
    return Circ(x, y, r, Attribute(id, class, label))
end

function Circ(c::R², r::Number; id="", class="", label="")
    x, y = Float64.((c.x, c.y))
    return Circ(x, y, r, Attribute(id, class, label))
end

# TODO
mutable struct Point <: VGElement
    x::Float64
    y::Float64
    a::Union{Attribute, Nothing}
end

mutable struct Polyline <: VGElement
    points::Vector{R²}
    a::Union{Attribute, Nothing}
end

function Polyline(pt1::R², points::R²...; id="", class="", label="")
    pts = [pt1]
    append!(pts, points...)
    return Polyine(pts, Attribute(id, class, label))
end

function Polyline(pt1::Tuple{Real, Real}, points::Tuple{Real,Real}...;
                  id="", class="", label="")
    pts = [convert(R², pt1)]
    append!(pts, convert.(R², points))
    return Polyline(pts, Attribute(id, class, label))
end

# TODO
mutable struct Text <: VGElement
    x::Float64
    y::Float64
    a::Union{Attribute, Nothing}
end

function Text(x::Real, y::Real, w::Real, h::Real;
              id="", class="", label="")
    x, y = Float64.((x, y))
    return Text(x, y, Attribute(id, class, label))
end

mutable struct Brace <: VGElement
    x::Float64
    y::Float64
    w::Float64
    h::Float64
    se::Union{VGElement, Nothing}      # starting element
    ee::Union{VGElement, Nothing}      # ending element
    a::Union{Attribute, Nothing}
end

function Brace(x::Real, y::Real, w::Real, h::Real;
               id="", class="", label="")
    x, y, w, h = Float64.((x, y, w, h))
    return Brace(x, y, w, h, nothing, nothing, Attribute(id, class, label))
end

function Brace(se::VGElement; id="", class="", label="")
    return Brace(NaN, NaN, NaN, NaN, se, nothing, Attribute(id, class, label, false))
end

function Brace(se::VGElement, ee::VGElement;id="", class="", label="")
    return Brace(NaN, NaN, NaN, NaN, se, ee, Attribute(id, class, label, false))
end

function setproperty!(e::VGElement, name::Symbol, x)
    name === :id && (e.a.id = x; return)
    name === :class && (e.a.class = x; return)
    name === :label && (e.a.label = x; return)
    name === :isUpdated && (e.a.isUpdated = x; return)
    return Base.setfield!(e, name, x)
end

function getproperty(e::VGElement, name::Symbol)
    name === :id && return e.a.id
    name === :class && return e.a.class
    name === :label && return e.a.label
    name === :isUpdated && return e.a.isUpdated
    return Base.getfield(e, name)
end

__BoxShaped = Union{BoundingBox, Rect, Brace}
# Helper functions
boundingbox(e::__BoxShaped) = R⁴(e.x, e.y, e.w, e.h)
boundingbox(e::Circ) = R⁴(e.x - e.r, e.y - e.r, 2*e.r, 2*e.r)

# In the future, Rect object could also have their positions depending on other objects. 
midtop(e::__BoxShaped) = R²(e.x + e.w/2, e.y)
midbottom(e::__BoxShaped) = R²(e.x + e.w/2, e.y + e.h)
midleft(e::__BoxShaped) = R²(e.x, e.y + e.h/2)
midright(e::__BoxShaped) = R²(e.x + e.w, e.y + e.h/2)
lefttop(e::__BoxShaped) = R²(e.x, e.y)
righttop(e::__BoxShaped) = R²(e.x + e.w, e.y)
leftbottom(e::__BoxShaped) = R²(e.x, e.y + e.h)
rightbottom(e::__BoxShaped) = R²(e.x + e.w, e.y + e.h)

update_box(::Nothing, pvd::PVDict) = nothing
update_box(e::Rect, pvd::PVDict) = nothing
update_box(e::Circ, pvd::PVDict) = nothing

function update_box(e::Brace, pvd::PVDict)
    e.isUpdated && return
    update_box(e.se)
    update_box(e.ee)
    bb = calc_brace_bbox(e, pvd)
    e.x, e.y, e.w, e.h = bb.x, bb.y, bb.w, bb.h
    e.isUpdated = true
end

#TODO: more helper functions for Circ, ...
#TODO: add midtop_xy, midbottom_xy, ...

# SVG primitives
# assume rule strings in style have been normalized (no leading and traling white
# spaces), and have been cleaned (no user-defined properties)
function make_rect(x::Real, y::Real, w::Real, h::Real, pvd::PVDict)
    return "<rect x=\"$x\" y=\"$y\" width=\"$w\" height=\"$h\" style=$(nstringify(pvd)) />"
end

function make_rect(b::Rect, pvd::PVDict)
    x, y, w, h = b.x, b.y, b.w, b.h
    return "<rect x=\"$x\" y=\"$y\" width=\"$w\" height=\"$h\" style=\"$(nstringify(pvd))\" />"
end

function make_circle(x::Real, y::Real, r::Real, pvd::PVDict)
    return "<circle cx=\"$x\" cy=\"$y\" r=\"$r\" style=\"$(nstringify(pvd))\" />"
end

function make_circle(c::Circ, pvd::PVDict)
    x, y, r = c.x, c.y, c.r
    return "<circle cx=\"$x\" cy=\"$y\" r=\"$r\" style=\"$(nstringify(pvd))\" />"
end

function make_label(el::VGElement, pvd::PVDict)
    anchor = get_property(pvd, "--vg-label-anchor")
    m = eachmatch(r"\n", el.label)
    lines = length(collect(m)) + 1
    pt = calc_label_pos(el, pvd, lines)
    textattr = "x=\"$(pt.x)\" y=\"$(pt.y)\" text-anchor=\"$(anchor)\""
    return "<text $(textattr) >"*el.label*"</text>"
end

const anchor_ratio = Dict([
    ("start", 0),
    ("middle", 0.5),
    ("end", 1)
   ])

# TODO: add support for vertical text
function calc_label_pos(el::VGElement, st::PVDict, lines::Int)
    side = get_property(st, "--vg-label-side")
    dist = parse(Float64, get_property(st, "--vg-label-distance"))
    anchor = get_property(st, "--vg-label-anchor")
    xheight = parse(Float64, get_property(st, "--vg-label-font-size"))
    xvshift = parse(Float64, get_property(st, "--vg-inner-center-vshift"))

    # calculate text position based on the `--vg-label-side` and
    #    `--vg-offset` properties.
    # NOTE: the `x`, `y` properties of a <text> element are the coordinates of
    # the bottom-left of the text box.
    pt = R²(0,0)
    bb = boundingbox(el)
    if side == "above"
        offset = anchor_ratio[anchor]*bb.w
        pt = lefttop(bb) + (offset, -dist - (lines-1)*xheight)
    elseif side == "below"
        offset = anchor_ratio[anchor]*bb.w
        pt = leftbottom(bb) + (offset, dist + xheight)
    elseif side == "left"
        pt = lefttop(bb) + (-dist, (bb.h - xheight)/2)
    elseif side == "right"
        pt = righttop(bb) + (dist, (bb.h - xheight)/2)
    else # side == "center"
        #FIXME: can not get the size of the text area, we have to guess...
        # the --vg-label-anchor must be `middle`
        pt = midtop(bb) + (0, (bb.h + xheight)/2 + xvshift)
    end
    return pt
end

calc_brace_bbox(se::VGElement, pvd::PVDict) = calc_brace_bbox(se, nothing, pvd)

function calc_brace_bbox(se::VGElement, ee::Union{VGElement, Nothing}, pvd::PVDict)
    side = get_property(pvd, "--vg-side")
    d = parse(Float64, get_property(pvd, "--vg-distance"))
    off = parse(Float64, get_property(pvd, "--vg-offset")) # relative val given in percentage
    bh = parse(Float64, get_property(pvd, "--vg-brace-height", "5"))

    # ew: the width of the bounding box if --vg-side == above|below
    # eh: the height of the bounding box if --vg-side == above|below
    # ew2: the width of the bounding box if --vg-side == left| right
    # eh2: the height of the bounding box if --vg-side == left|right
    bb = R⁴(0,0,0,0)
    if ee == nothing
        if off >= 0.5
            # degenerated brace (only a line segment)
            ew = 0
            eh2 = 0
        else
            ew = se.w*(1-2*off)
            eh2 = se.h*(1-2*off)
        end
    else
        ew = ee.x + ee.w*(1-off) - se.x - se.w*off
        eh2 = ee.y + ee.h*(1-off) - se.y - se.h*off
    end
    eh, ew2 = bh, bh

    if side == "above"
        bb = R⁴(se.x + off*se.w, se.y - d - eh, ew, eh)
    elseif side == "below"
        bb = R⁴(se.x + off*se.w, ee.y + ee.h + d, ew, eh)
    elseif side == "left"
        bb = R⁴(se.x - d - ew2, se.y + off*se.h, ew2, eh2)
    else # side == "right"
        bb = R⁴(ee.x + ee.w + d, se.y + off*se.h, ew2, eh2)
    end

    return bb
end

# Note: there can be a chain-reaction: the position of an overbrace label depends
# on the position of the overbrace, which in turn depends on the position of
# the boxes that the overbrace encloses. So the overbrace position and size
# should be calculated before calculating those of its label. But the
# quesiton is, how to distinguish a standalone overbrace and an pos-size updated
# overbrace?
function make_svg_root(eles...; css=CssRules())
    spvd = cssrules_to_spvdict(css)
    content = ""
    leftxs, rightxs, topys, bottomys = [], [], [], []
    for el in eles
        if el isa Vector
            for e in el
                st = get_style_PVD(e, spvd)
                content *= make_svg_ele(e, st)
                bb = boundingbox(e)
                push!(leftxs, bb.x)
                push!(rightxs, bb.x + bb.w)
                push!(topys, bb.y)
                push!(bottomys, bb.y + bb.h)
                if !isempty(e.label)
                    # FIXME we do not know yet how to get the pixel length of a text
                    # area, thus we can not update maxxs for it
                    content *= make_label(e, st)
                end
            end
        elseif el isa VGElement
            st = get_style_PVD(el, spvd)
            content *= make_svg_ele(el, st)
            bb = boundingbox(el)
            push!(leftxs, bb.x)
            push!(rightxs, bb.x + bb.w)
            push!(topys, bb.y)
            push!(bottomys, bb.y + bb.h)
            if !isempty(e.label)
                content *= make_label(el, st)
            end
        else
            println(typeof(el))
            error("Unknown type in writing SVG elements")
        end
    end

    # calculate viewBox
    lmargin, rmargin, tmargin, bmargin = 40, 10, 60, 30
    vx = string(minimum(leftxs) - lmargin)
    vy = string(minimum(topys) - tmargin)
    vw = string(maximum(rightxs) - minimum(leftxs) + lmargin + rmargin)
    vh = string(maximum(bottomys) - minimum(topys) + tmargin + bmargin)

    return "<svg width=\"$(vw)\" height=\"$(vh)\" viewBox=\"$(vx) $(vy) $(vw) $(vh)\">"*content*"</svg>"
end

function write_svg_ele(el, pvd::PVDict)
    return "NOT implemented yet" 
end

function make_svg_ele(el::Rect, pvd::PVDict)
    if !allow_outer_stylesheet
        pvd = merge(supp_properties, pvd) 
    end
    make_rect(el, pvd)
end

function make_svg_ele(el::Circ, pvd::PVDict)
    if !allow_outer_stylesheet
        pvd = merge(supp_properties, pvd) 
    end
    make_circle(el, pvd)
end

function make_svg_ele(el::Polyline, pvd::PVDict)
    if !allow_outer_stylesheet
        pvd = merge(supp_properties, pvd) 
    end
    pts= ""
    for pt in el.points
        pts *= "$(pt.x),$(pt.y) "
    end
    st = nstringify(pvd)
    return "<polyline fill=\"none\" style=\"$st\" points=\"$(pts)\" />"
end

# four cases to deal with:
#   1. lsd == nothing, rsd == nothing: a standalone overbrace
#   2. lsd != nothing, rsd == nothing: an overbrace over one box, degenerated
#   3. lsd != nothing, rsd == nothing: an overbrace over one box, non-degenerated
#   4. lsd != nothing, rsd != nothing: an overbrace over two boxes
#
# use properties: --vg-offset, --vg-distance, --vg-shape, --vg-suppress
function make_svg_ele(el::Brace, pvd::PVDict)
    if !allow_outer_stylesheet
        pvd = merge(supp_properties, pvd) 
    end
    bb = BoundingBox(0, 0, 0, 0)
    if  el.se === nothing && el.ee === nothing
        # user-defined position-dimension
        bb = BoundingBox(el.x, el.y, el.w, el.h)
    elseif el.ee === nothing
        bb = calc_brace_bbox(el.se, pvd)
    else
        bb = calc_brace_bbox(el.se, el.ee, pvd)
    end
    el.x, el.y, el.w, el.h = bb.x, bb.y, bb.w, bb.h
    el.isUpdated = true

    side = get_property(pvd, "--vg-side")
    s = ""
    # how about making curly braces?
    if side == "above"
        pline = Polyline((bb.x, bb.y+bb.h), (bb.x, bb.y),
                         (bb.x+bb.w, bb.y), (bb.x+bb.w, bb.y+bb.h))
        s = make_svg_ele(pline, pvd) # get path color, path stroke, ... from pvd
    elseif side == "below"
        pline = Polyline((bb.x, bb.y), (bb.x, bb.y+bb.h),
                         (bb.x+bb.w, bb.y+bb.h), (bb.x+bb.w, bb.y))
        s = make_svg_ele(pline, pvd) 
    elseif side == "left"
        pline = Polyline((bb.x+bb.w, bb.y), (bb.x, bb.y),
                         (bb.x, bb.y+bb.h), (bb.x+bb.w, bb.y+bb.h))
        s = make_svg_ele(pline, pvd) 
    else # side == "right"
        s = pline = Polyline((bb.x, bb.y), (bb.x+bb.w, bb.y),
                         (bb.x+bb.w, bb.y+bb.h), (bb.x, bb.y+bb.h))
        make_svg_ele(pline, pvd) 
    end
    return s
end
