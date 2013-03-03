
Pkg.add("DataFrames", "Distributions", "GLM", "MCMC", "Optim", 
        "NHST", "Clustering")

Pkg.add("RDatasets")

rand(2,3)

2 + 3
print("hello")
sqrt(5)

2 + 3;
print("hello");
sqrt(5);

Pkg.add("Winston")

using Winston
x = linspace(0, 3pi, 100)
c = cos(x)
s = sin(x)
p = FramedPlot();
setattr(p, "title", "title!")
setattr(p, "xlabel", L"\Sigma x^2_i")
setattr(p, "ylabel", L"\Theta_i")
add(p, FillBetween(x, c, x, s) )
add(p, Curve(x, c, "color", "red") )
add(p, Curve(x, s, "color", "blue") )

Winston.tk(p)

file(p, "example1.png")
