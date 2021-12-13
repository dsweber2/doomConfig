using Plots
using Colors
cgrad(:viridis)[1]
function getColorRange(colorList, nColors = 26)
    c = ["#$(x)" for x in hex.(cgrad(colorList, range(0, 1, length = 300), categorical = nothing), :RRGGBB)]
    println((c[round.(Int64, range(1, length(c), length = nColors))]))
    letters = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    return ["""(?$(letters[ii]) :foreground "$x)\n""" for (ii, x) in enumerate(c[round.(Int64, range(1, length(c), length = nColors))])]
end
c = getColorRange([colorant"#FF6c6b", colorant"#da8548", colorant"#c678dd", colorant"#a9a1e1", colorant"#5B6268"]);
print(string(c...))
