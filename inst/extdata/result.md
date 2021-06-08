---
title: "knitr engine test page"
---

(%i1) L: sqrt(1 - 1/R^2);## sqrt(1-1/R^2)
(%i2) assume(R > 0);## [R > 0]
(%i3) 'integrate(x, x, 0, L) = integrate(x, x, 0, L);## 'integrate(x,x,0,sqrt(1-1/R^2)) = (R^2-1)/(2*R^2)


(%i5) 'L = L;$$L=\sqrt{1-\frac{1}{R^2}}$$(%i6) 'integrate(x, x, 0, 'L) = integrate(x, x, 0, L);$$\int_{0}^{L}{x\;dx}=\frac{R^2-1}{2\,R^2}$$

knitr::opts_chunk$set(maxima.format = "latex", results = 'asis')

(%i9) sqrt(3/4);<math xmlns="http://www.w3.org/1998/Math/MathML"> <mfrac><mrow>
 <msqrt><mn>3</mn> </msqrt></mrow> <mrow><mn>2</mn> </mrow></mfrac> </math>

(%i12) f(x) := e^(x^2)$(%i13) diff(f(x), x);$$2\,e^{x^2}\,\log e\,x$$

(%i16) %;<math xmlns="http://www.w3.org/1998/Math/MathML"> <mn>2</mn> 
 <mspace width="thinmathspace"/><msup><mrow><mi>e</mi> </mrow> <mrow>
 <msup><mrow><mi>x</mi> </mrow> <mn>2</mn> </msup> </mrow></msup> 
 <mspace width="thinmathspace"/><mi>log</mi> <mi>e</mi> 
 <mspace width="thinmathspace"/><mi>x</mi> </math>

(%i19) log(%o1);$$\frac{\log \left(1-\frac{1}{R^2}\right)}{2}$$

(%i22) area(dist) := integrate(dist, x, minf, inf)$(%i23) mean(dist) := area(dist*x)$(%i24) EX2(dist) := area(dist*x^2)$(%i25) variance(dist) := EX2(dist) - mean(dist)^2$(%i26) mgf(dist) := area(dist*%e^(x*t))$

# Normal Distribution

(%i29) normal(x) :=(2*%pi*sigma^2)^(-1/2) *exp(-(x-mu)^2/(2*sigma^2));$${\it normal}\left(x\right):=\left(2\,\pi\,\sigma^2\right)^{\frac{-1}{2}}\,\exp \left(\frac{-\left(x-\mu\right)^2}{2\,\sigma^2}\right)$$(%i30) assume(sigma > 0)$(%i31) area(normal(x));$$1$$(%i32) mean(normal(x));$$\mu$$(%i33) variance(normal(x));$$\frac{2^{\frac{3}{2}}\,\sqrt{\pi}\,\sigma^3+2^{\frac{3}{2}}\,\sqrt{\pi}\,\mu^2\,\sigma}{2^{\frac{3}{2}}\,\sqrt{\pi}\,\sigma}-\mu^2$$(%i34) mgf(normal(x));$$e^{\frac{\sigma^2\,t^2+2\,\mu\,t}{2}}$$

# Laplace Distribution

(%i37) laplace(x) := (2*b)^-1 * exp(-abs(x - mu)/b);$${\it laplace}\left(x\right):=\left(2\,b\right)^ {- 1 }\,\exp \left(\frac{-\left| x-\mu\right| }{b}\right)$$(%i38) load("abs_integrate")$(%i39) assume(b > 0)$(%i40) area(laplace(x));$$1$$(%i41) mean(laplace(x));$$\mu$$(%i42) variance(laplace(x));$$\frac{2\,b\,\mu^2+4\,b^3}{2\,b}-\mu^2$$

# Exponential Distribution

(%i45) expo(x) := unit_step(x) * lambda * exp(-lambda * x);$${\it expo}\left(x\right):={\it unit\_step}\left(x\right)\,\lambda\,\exp \left(\left(-\lambda\right)\,x\right)$$(%i46) assume(lambda > 0)$(%i47) area(expo(x));$$1$$(%i48) mean(expo(x));$$\frac{1}{\lambda}$$(%i49) variance(expo(x));$$\frac{1}{\lambda^2}$$

# Matrices

(%i52) m: matrix([0, 1, a], [1, 0, 1], [1, 1, 0]);$$\begin{pmatrix}0 & 1 & a \\ 1 & 0 & 1 \\ 1 & 1 & 0 \\ \end{pmatrix}$$(%i53) transpose(m);$$\begin{pmatrix}0 & 1 & 1 \\ 1 & 0 & 1 \\ a & 1 & 0 \\ \end{pmatrix}$$(%i54) determinant(m);$$a+1$$(%i55) f: invert(m), detout;$$\frac{\begin{pmatrix}-1 & a & 1 \\ 1 & -a & a \\ 1 & 1 & -1 \\ \end{pmatrix}}{a+1}$$(%i56) m . f;$$\begin{pmatrix}0 & 1 & a \\ 1 & 0 & 1 \\ 1 & 1 & 0 \\ \end{pmatrix}\cdot \left(\frac{\begin{pmatrix}-1 & a & 1 \\ 1 & -a & a \\ 1 & 1 & -1 \\ \end{pmatrix}}{a+1}\right)$$(%i57) expand(%);$$\begin{pmatrix}\frac{a}{a+1}+\frac{1}{a+1} & 0 & 0 \\ 0 & \frac{a}{a+1}+\frac{1}{a+1} & 0 \\ 0 & 0 & \frac{a}{a+1}+\frac{1}{a+1} \\ \end{pmatrix}$$(%i58) factor(%);$$\begin{pmatrix}1 & 0 & 0 \\ 0 & 1 & 0 \\ 0 & 0 & 1 \\ \end{pmatrix}$$
