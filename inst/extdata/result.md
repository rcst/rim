---
title: "knitr engine test page"
output: html_document
---

library(OpenImageR)maxima.options(engine.format = "latex", 
	       engine.label = TRUE,
	       inline.format = "inline", 
	       inline.label = FALSE)

(%i1) L: sqrt(1 - 1/R^2);$$\mathtt{(\textit{\%o}_{1})}\quad \sqrt{1-\frac{1}{R^2}}$$
(%i2) assume(R > 0);$$\mathtt{(\textit{\%o}_{2})}\quad \left[ R>0 \right] $$
(%i3) 'integrate(x, x, 0, L) = integrate(x, x, 0, L);$$\mathtt{(\textit{\%o}_{3})}\quad \int_{0}^{\sqrt{1-\frac{1}{R^2}}}{x\;dx}=\frac{R^2-1}{2\,R^2}$$


(%i4) 'L = L;$$\mathtt{(\textit{\%o}_{4})}\quad L=\sqrt{1-\frac{1}{R^2}}$$
(%i5) 'integrate(x, x, 0, 'L) = integrate(x, x, 0, L);$$\mathtt{(\textit{\%o}_{5})}\quad \int_{0}^{L}{x\;dx}=\frac{R^2-1}{2\,R^2}$$


This is an inline test: $L=\sqrt{1-\frac{1}{R^2}}$
.


(%i7) sqrt(3/4);$$\mathtt{(\textit{\%o}_{7})}\quad \frac{\sqrt{3}}{2}$$


(%i8) f(x) := e^(x^2)$(%i9) diff(f(x), x);$$\mathtt{(\textit{\%o}_{9})}\quad 2\,e^{x^2}\,\log e\,x$$


(%i10) %;$$\mathtt{(\textit{\%o}_{10})}\quad 2\,e^{x^2}\,\log e\,x$$


(%i11) log(%o1);$$\mathtt{(\textit{\%o}_{11})}\quad \frac{\log \left(1-\frac{1}{R^2}\right)}{2}$$



# Plots

(%i12) r: (exp(cos(t))-2*cos(4*t)-sin(t/12)^5)$(%i13) plot2d([parametric, r*sin(t), r*cos(t), [t,-8*%pi,8*%pi]]);

(%i14) plot3d(log (x^2*y^2), [x, -2, 2], [y, -2, 2],[grid, 29, 29],
       [palette, [gradient, red, orange, yellow, green]],
       color_bar, [xtics, 1], [ytics, 1], [ztics, 4],
       [color_bar_tics, 4]);

(%i15) example1:
  gr3d (title          = "Controlling color range",
        enhanced3d     = true,
        color          = green,
        cbrange        = [-3,10],
        explicit(x^2+y^2, x,-2,2,y,-2,2)) $(%i16) example2:
  gr3d (title          = "Playing with tics in colorbox",
        enhanced3d     = true,
        color          = green,
        cbtics         = {["High",10],["Medium",05],["Low",0]},
        cbrange = [0, 10],
        explicit(x^2+y^2, x,-2,2,y,-2,2))$(%i17) example3:
  gr3d (title      = "Logarithmic scale to colors",
        enhanced3d = true,
        color      = green,
        logcb      = true,
        logz       = true,
        palette    = [-15,24,-9],
        explicit(exp(x^2-y^2), x,-2,2,y,-2,2))$(%i18) draw(
  dimensions = [500,1500],
  example1, example2, example3);

(%i19) draw2d(
  dimensions = [1000, 1000],
  proportional_axes = xy,
  fill_color        = sea_green,
  color             = aquamarine,
  line_width        = 6,
  ellipse(7,6,2,3,0,360));

(%i20) draw3d(
   dimensions = [1000, 1000],
   surface_hide      = true,
   axis_3d           = false,
   proportional_axes = xyz,
 
   color             = blue,
   cylindrical(z,z,-2,2,a,0,2*%pi), 
 
   color            = brown,
   cylindrical(3,z,-2,2,az,0,%pi),
 
   color            = green,
   cylindrical(sqrt(25-z^2),z,-5,5,a,0,%pi));


pft <- list.files(pattern = "(?:plot|draw)(2d|3d)?-[[:print:]]{6}\\.png", full.names = TRUE)pref <- system.file("extdata", 
		    c("draw-ref.png",
		      "draw2d-ref.png", 
		      "draw3d-ref.png", 
		      "plot2d-ref.png", 
		      "plot3d-ref.png"), 
		    package = "rim", 
		    mustWork = TRUE) r1 <- readImage(pref[1])r2 <- readImage(pref[2])r3 <- readImage(pref[3])r4 <- readImage(pref[4])p1 <- readImage(pft[1])p2 <- readImage(pft[2])p3 <- readImage(pft[3])p4 <- readImage(pft[4])p1 <- rgb_2gray(RGB_image = p1)p2 <- rgb_2gray(RGB_image = p2)p3 <- rgb_2gray(RGB_image = p3)p4 <- rgb_2gray(RGB_image = p4)r1 <- rgb_2gray(RGB_image = r1)r2 <- rgb_2gray(RGB_image = r2)r3 <- rgb_2gray(RGB_image = r3)r4 <- rgb_2gray(RGB_image = r4)hs1 <- average_hash(p1, hash_size = 32, MODE = "binary")hs2 <- average_hash(p2, hash_size = 32, MODE = "binary")hs3 <- average_hash(p3, hash_size = 32, MODE = "binary")hs4 <- average_hash(p4, hash_size = 32, MODE = "binary")rs1 <- average_hash(r1, hash_size = 32, MODE = "binary")rs2 <- average_hash(r2, hash_size = 32, MODE = "binary")rs3 <- average_hash(r3, hash_size = 32, MODE = "binary")rs4 <- average_hash(r4, hash_size = 32, MODE = "binary")if((d <- sum(abs(hs1 - rs1))) < 100) {
  paste0("OK")
} else {
  paste0("Not OK: ", d)
}## [1] "OK"
if((d <- sum(abs(hs2 - rs2))) < 100) {
  paste0("OK")
} else {
  paste0("Not OK: ", d)
}## [1] "OK"
if((d <- sum(abs(hs3 - rs3))) < 100) {
  paste0("OK")
} else {
  paste0("Not OK: ", d)
}## [1] "OK"
if((d <- sum(abs(hs4 - rs4))) < 180) {
  paste0("OK")
} else {
  paste0("Not OK: ", d)
}## [1] "OK"


# Normal Distribution

(%i21) area(dist) := integrate(dist, x, minf, inf)$(%i22) mean(dist) := area(dist*x)$(%i23) EX2(dist) := area(dist*x^2)$(%i24) variance(dist) := EX2(dist) - mean(dist)^2$(%i25) mgf(dist) := area(dist*%e^(x*t))$

(%i26) normal(x) := 
      (2*%pi*sigma^2)^(-1/2) * 
      exp(-(x-mu)^2/(2*sigma^2));$$\mathtt{(\textit{\%o}_{26})}\quad \textit{normal}\left(x\right):=\left(2\,\pi\,\sigma^2\right)^{\frac{-1}{2}}\,\exp \left(\frac{-\left(x-\mu\right)^2}{2\,\sigma^2}\right)$$
(%i27) assume(sigma > 0)$(%i28) area(normal(x));$$\mathtt{(\textit{\%o}_{28})}\quad 1$$
(%i29) mean(normal(x));$$\mathtt{(\textit{\%o}_{29})}\quad \mu$$
(%i30) variance(normal(x));$$\mathtt{(\textit{\%o}_{30})}\quad \frac{2^{\frac{3}{2}}\,\sqrt{\pi}\,\sigma^3+2^{\frac{3}{2}}\,\sqrt{\pi}\,\mu^2\,\sigma}{2^{\frac{3}{2}}\,\sqrt{\pi}\,\sigma}-\mu^2$$
(%i31) mgf(normal(x));$$\mathtt{(\textit{\%o}_{31})}\quad e^{\frac{\sigma^2\,t^2+2\,\mu\,t}{2}}$$


# Laplace Distribution

(%i32) laplace(x) := (2*b)^-1 * exp(-abs(x - mu)/b);$$\mathtt{(\textit{\%o}_{32})}\quad \textit{laplace}\left(x\right):=\left(2\,b\right)^ {- 1 }\,\exp \left(\frac{-\left| x-\mu\right| }{b}\right)$$
(%i33) load("abs_integrate")$(%i34) assume(b > 0)$(%i35) area(laplace(x))$(%i36) mean(laplace(x))$(%i37) variance(laplace(x))$

# Exponential Distribution

(%i38) expo(x) := unit_step(x) * lambda * exp(-lambda * x);$$\mathtt{(\textit{\%o}_{38})}\quad \textit{expo}\left(x\right):=\textit{unit\_step}\left(x\right)\,\lambda\,\exp \left(\left(-\lambda\right)\,x\right)$$
(%i39) assume(lambda > 0)$(%i40) area(expo(x));$$\mathtt{(\textit{\%o}_{40})}\quad 1$$
(%i41) mean(expo(x));$$\mathtt{(\textit{\%o}_{41})}\quad \frac{1}{\lambda}$$
(%i42) variance(expo(x));$$\mathtt{(\textit{\%o}_{42})}\quad \frac{1}{\lambda^2}$$


# Matrices

(%i43) m: matrix([0, 1, a], [1, 0, 1], [1, 1, 0]);$$\mathtt{(\textit{\%o}_{43})}\quad \begin{pmatrix}0 & 1 & a \\ 1 & 0 & 1 \\ 1 & 1 & 0 \\ \end{pmatrix}$$
(%i44) transpose(m);$$\mathtt{(\textit{\%o}_{44})}\quad \begin{pmatrix}0 & 1 & 1 \\ 1 & 0 & 1 \\ a & 1 & 0 \\ \end{pmatrix}$$
(%i45) determinant(m);$$\mathtt{(\textit{\%o}_{45})}\quad a+1$$
(%i46) f: invert(m), detout;$$\mathtt{(\textit{\%o}_{46})}\quad \frac{\begin{pmatrix}-1 & a & 1 \\ 1 & -a & a \\ 1 & 1 & -1 \\ \end{pmatrix}}{a+1}$$
(%i47) m . f;$$\mathtt{(\textit{\%o}_{47})}\quad \begin{pmatrix}0 & 1 & a \\ 1 & 0 & 1 \\ 1 & 1 & 0 \\ \end{pmatrix}\cdot \left(\frac{\begin{pmatrix}-1 & a & 1 \\ 1 & -a & a \\ 1 & 1 & -1 \\ \end{pmatrix}}{a+1}\right)$$
(%i48) expand(%);$$\mathtt{(\textit{\%o}_{48})}\quad \begin{pmatrix}\frac{a}{a+1}+\frac{1}{a+1} & 0 & 0 \\ 0 & \frac{a}{a+1}+\frac{1}{a+1} & 0 \\ 0 & 0 & \frac{a}{a+1}+\frac{1}{a+1} \\ \end{pmatrix}$$
(%i49) factor(%);$$\mathtt{(\textit{\%o}_{49})}\quad \begin{pmatrix}1 & 0 & 0 \\ 0 & 1 & 0 \\ 0 & 0 & 1 \\ \end{pmatrix}$$


# If-then-else

(%i50) x: 1234;$$\mathtt{(\textit{\%o}_{50})}\quad 1234$$
(%i51) y: 2345;$$\mathtt{(\textit{\%o}_{51})}\quad 2345$$


(%i52) if x > y
  then x
  else y;$$\mathtt{(\textit{\%o}_{52})}\quad 2345$$

