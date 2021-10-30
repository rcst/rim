---
title: "knitr engine test page"
output: html_document
---

library(OpenImageR)maxima.options(engine.format = "latex", 
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


(%i12) area(dist) := integrate(dist, x, minf, inf)$(%i13) mean(dist) := area(dist*x)$(%i14) EX2(dist) := area(dist*x^2)$(%i15) variance(dist) := EX2(dist) - mean(dist)^2$(%i16) mgf(dist) := area(dist*%e^(x*t))$

# Plots

(%i17) r: (exp(cos(t))-2*cos(4*t)-sin(t/12)^5)$(%i18) plot2d([parametric, r*sin(t), r*cos(t), [t,-8*%pi,8*%pi]]);

(%i19) plot3d(log (x^2*y^2), [x, -2, 2], [y, -2, 2],[grid, 29, 29],
       [palette, [gradient, red, orange, yellow, green]],
       color_bar, [xtics, 1], [ytics, 1], [ztics, 4],
       [color_bar_tics, 4]);

pft <- list.files(pattern = "plot(2|3)d-[[:print:]]{6}\\.png", full.names = TRUE)pref <- system.file("extdata", c("plot2d-ref.png", "plot3d-ref.png"),
		    package = "rim", mustWork = TRUE)r1 <- readImage(pref[1])r2 <- readImage(pref[2])p1 <- readImage(pft[1])## Error in readImage(pft[1]): the path to an image is invalid or the image does not exist
p2 <- readImage(pft[2])## Error in readImage(pft[2]): the path to an image is invalid or the image does not exist
p1 <- rgb_2gray(RGB_image = p1)## Error in rgb_2gray(RGB_image = p1): The 'RGB_image' parameter must be a 3-dimensional array!
p2 <- rgb_2gray(RGB_image = p2)## Error in rgb_2gray(RGB_image = p2): The 'RGB_image' parameter must be a 3-dimensional array!
r1 <- rgb_2gray(RGB_image = r1)r2 <- rgb_2gray(RGB_image = r2)hs1 <- average_hash(p1, hash_size = 32, MODE = "binary")hs2 <- average_hash(p2, hash_size = 32, MODE = "binary")rs1 <- average_hash(r1, hash_size = 32, MODE = "binary")rs2 <- average_hash(r2, hash_size = 32, MODE = "binary")if((d <- sum(abs(hs1 - rs1))) < 50) {
  paste0("OK")

} else {
  paste0("Not OK")
}## [1] "OK"
if((d <- sum(abs(hs2 - rs2))) < 50) {
  paste0("OK")
} else {
  paste0("Not OK")
}## [1] "OK"



# Normal Distribution

(%i20) normal(x) := 
      (2*%pi*sigma^2)^(-1/2) * 
      exp(-(x-mu)^2/(2*sigma^2));$$\mathtt{(\textit{\%o}_{20})}\quad \textit{normal}\left(x\right):=\left(2\,\pi\,\sigma^2\right)^{\frac{-1}{2}}\,\exp \left(\frac{-\left(x-\mu\right)^2}{2\,\sigma^2}\right)$$
(%i21) assume(sigma > 0)$(%i22) area(normal(x));$$\mathtt{(\textit{\%o}_{22})}\quad 1$$
(%i23) mean(normal(x));$$\mathtt{(\textit{\%o}_{23})}\quad \mu$$
(%i24) variance(normal(x));$$\mathtt{(\textit{\%o}_{24})}\quad \frac{2^{\frac{3}{2}}\,\sqrt{\pi}\,\sigma^3+2^{\frac{3}{2}}\,\sqrt{\pi}\,\mu^2\,\sigma}{2^{\frac{3}{2}}\,\sqrt{\pi}\,\sigma}-\mu^2$$
(%i25) mgf(normal(x));$$\mathtt{(\textit{\%o}_{25})}\quad e^{\frac{\sigma^2\,t^2+2\,\mu\,t}{2}}$$


# Laplace Distribution

(%i26) laplace(x) := (2*b)^-1 * exp(-abs(x - mu)/b);$$\mathtt{(\textit{\%o}_{26})}\quad \textit{laplace}\left(x\right):=\left(2\,b\right)^ {- 1 }\,\exp \left(\frac{-\left| x-\mu\right| }{b}\right)$$
(%i27) load("abs_integrate")$(%i28) assume(b > 0)$(%i29) area(laplace(x));$$\mathtt{(\textit{\%o}_{29})}\quad 1$$
(%i30) mean(laplace(x));$$\mathtt{(\textit{\%o}_{30})}\quad \mu$$
(%i31) variance(laplace(x));$$\mathtt{(\textit{\%o}_{31})}\quad \frac{2\,b\,\mu^2+4\,b^3}{2\,b}-\mu^2$$


# Exponential Distribution

(%i32) expo(x) := unit_step(x) * lambda * exp(-lambda * x);$$\mathtt{(\textit{\%o}_{32})}\quad \textit{expo}\left(x\right):=\textit{unit\_step}\left(x\right)\,\lambda\,\exp \left(\left(-\lambda\right)\,x\right)$$
(%i33) assume(lambda > 0)$(%i34) area(expo(x));$$\mathtt{(\textit{\%o}_{34})}\quad 1$$
(%i35) mean(expo(x));$$\mathtt{(\textit{\%o}_{35})}\quad \frac{1}{\lambda}$$
(%i36) variance(expo(x));$$\mathtt{(\textit{\%o}_{36})}\quad \frac{1}{\lambda^2}$$


# Matrices

(%i37) m: matrix([0, 1, a], [1, 0, 1], [1, 1, 0]);$$\mathtt{(\textit{\%o}_{37})}\quad \begin{pmatrix}0 & 1 & a \\ 1 & 0 & 1 \\ 1 & 1 & 0 \\ \end{pmatrix}$$
(%i38) transpose(m);$$\mathtt{(\textit{\%o}_{38})}\quad \begin{pmatrix}0 & 1 & 1 \\ 1 & 0 & 1 \\ a & 1 & 0 \\ \end{pmatrix}$$
(%i39) determinant(m);$$\mathtt{(\textit{\%o}_{39})}\quad a+1$$
(%i40) f: invert(m), detout;$$\mathtt{(\textit{\%o}_{40})}\quad \frac{\begin{pmatrix}-1 & a & 1 \\ 1 & -a & a \\ 1 & 1 & -1 \\ \end{pmatrix}}{a+1}$$
(%i41) m . f;$$\mathtt{(\textit{\%o}_{41})}\quad \begin{pmatrix}0 & 1 & a \\ 1 & 0 & 1 \\ 1 & 1 & 0 \\ \end{pmatrix}\cdot \left(\frac{\begin{pmatrix}-1 & a & 1 \\ 1 & -a & a \\ 1 & 1 & -1 \\ \end{pmatrix}}{a+1}\right)$$
(%i42) expand(%);$$\mathtt{(\textit{\%o}_{42})}\quad \begin{pmatrix}\frac{a}{a+1}+\frac{1}{a+1} & 0 & 0 \\ 0 & \frac{a}{a+1}+\frac{1}{a+1} & 0 \\ 0 & 0 & \frac{a}{a+1}+\frac{1}{a+1} \\ \end{pmatrix}$$
(%i43) factor(%);$$\mathtt{(\textit{\%o}_{43})}\quad \begin{pmatrix}1 & 0 & 0 \\ 0 & 1 & 0 \\ 0 & 0 & 1 \\ \end{pmatrix}$$


# If-then-else

(%i44) x: 1234;$$\mathtt{(\textit{\%o}_{44})}\quad 1234$$
(%i45) y: 2345;$$\mathtt{(\textit{\%o}_{45})}\quad 2345$$


(%i46) if x > y
  then x
  else y;$$\mathtt{(\textit{\%o}_{46})}\quad 2345$$

