require(Rcpp)

m <- Module("Maxima", dyn.load("Rmaxima.so"))

mx.start <- function()
{
mxm <<- new(m$RMaxima)
}

mx.exec <- function(x)
{
mxm$execute(as.character(x))
}

mx.start()

mx.exec("9+7")
mx.exec("% - 10")
mx.exec("%o1 * 2")
mx.exec("float(1/3)")
mx.exec("sin(%pi/2)+cos(%pi/3)")
mx.exec("plot2d(x^2-x+3,[x,-10,10])")
mx.exec("f(x,y):= sin(x)+cos(y)")
mx.exec("plot3d(f(x,y),[x,-5,5],[y,-5,5])")
result = mx.exec("diff(sin(x),x)")
result
mx.exec("2+2;;")
mx.exec("asd+-*;")