module SVGDraw

include("style.jl")
include("shape.jl")

export R², R³, R⁴, BoundingBox, setproperty!, getproperty,
       VGElement, Rect, Circ, Polyline, Brace,
       lefttop, leftbottom, righttop, rightbottom,
       midtop, midbottom, midleft, midright,
       CssRules,
       make_svg_root, make_svg_ele
end
