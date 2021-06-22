---
title: "knitr engine test page"
---

maxima.engine.format("latex")## [1] "latex"


(%i1) L: sqrt(1 - 1/R^2);$$\sqrt{1-\frac{1}{R^2}}$$(%i2) assume(R > 0);$$\left[ R>0 \right] $$(%i3) 'integrate(x, x, 0, L) = integrate(x, x, 0, L);$$\int_{0}^{\sqrt{1-\frac{1}{R^2}}}{x\;dx}=\frac{R^2-1}{2\,R^2}$$

(%i4) 'L = L;$$L=\sqrt{1-\frac{1}{R^2}}$$(%i5) 'integrate(x, x, 0, 'L) = integrate(x, x, 0, L);$$\int_{0}^{L}{x\;dx}=\frac{R^2-1}{2\,R^2}$$

(%i6) sqrt(3/4);$$\frac{\sqrt{3}}{2}$$

(%i7) f(x) := e^(x^2)$(%i8) diff(f(x), x);$$2\,e^{x^2}\,\log e\,x$$

(%i9) %;$$2\,e^{x^2}\,\log e\,x$$

(%i10) log(%o1);$$\frac{\log \left(1-\frac{1}{R^2}\right)}{2}$$

(%i11) area(dist) := integrate(dist, x, minf, inf)$(%i12) mean(dist) := area(dist*x)$(%i13) EX2(dist) := area(dist*x^2)$(%i14) variance(dist) := EX2(dist) - mean(dist)^2$(%i15) mgf(dist) := area(dist*%e^(x*t))$

# Normal Distribution

(%i16) normal(x) :=(2*%pi*sigma^2)^(-1/2) *exp(-(x-mu)^2/(2*sigma^2));$${\it normal}\left(x\right):=\left(2\,\pi\,\sigma^2\right)^{\frac{-1}{2}}\,\exp \left(\frac{-\left(x-\mu\right)^2}{2\,\sigma^2}\right)$$(%i17) assume(sigma > 0)$(%i18) area(normal(x));$$1$$(%i19) mean(normal(x));$$\mu$$(%i20) variance(normal(x));$$\frac{2^{\frac{3}{2}}\,\sqrt{\pi}\,\sigma^3+2^{\frac{3}{2}}\,\sqrt{\pi}\,\mu^2\,\sigma}{2^{\frac{3}{2}}\,\sqrt{\pi}\,\sigma}-\mu^2$$(%i21) mgf(normal(x));$$e^{\frac{\sigma^2\,t^2+2\,\mu\,t}{2}}$$

# Laplace Distribution

(%i22) laplace(x) := (2*b)^-1 * exp(-abs(x - mu)/b);$${\it laplace}\left(x\right):=\left(2\,b\right)^ {- 1 }\,\exp \left(\frac{-\left| x-\mu\right| }{b}\right)$$(%i23) load("abs_integrate")$(%i24) assume(b > 0)$(%i25) area(laplace(x));$$1$$(%i26) mean(laplace(x));$$\mu$$(%i27) variance(laplace(x));$$\frac{2\,b\,\mu^2+4\,b^3}{2\,b}-\mu^2$$

# Exponential Distribution

(%i28) expo(x) := unit_step(x) * lambda * exp(-lambda * x);$${\it expo}\left(x\right):={\it unit\_step}\left(x\right)\,\lambda\,\exp \left(\left(-\lambda\right)\,x\right)$$(%i29) assume(lambda > 0)$(%i30) area(expo(x));$$1$$(%i31) mean(expo(x));$$\frac{1}{\lambda}$$(%i32) variance(expo(x));$$\frac{1}{\lambda^2}$$

# Matrices

(%i33) m: matrix([0, 1, a], [1, 0, 1], [1, 1, 0]);$$\begin{pmatrix}0 & 1 & a \\ 1 & 0 & 1 \\ 1 & 1 & 0 \\ \end{pmatrix}$$(%i34) transpose(m);$$\begin{pmatrix}0 & 1 & 1 \\ 1 & 0 & 1 \\ a & 1 & 0 \\ \end{pmatrix}$$(%i35) determinant(m);$$a+1$$(%i36) f: invert(m), detout;$$\frac{\begin{pmatrix}-1 & a & 1 \\ 1 & -a & a \\ 1 & 1 & -1 \\ \end{pmatrix}}{a+1}$$(%i37) m . f;$$\begin{pmatrix}0 & 1 & a \\ 1 & 0 & 1 \\ 1 & 1 & 0 \\ \end{pmatrix}\cdot \left(\frac{\begin{pmatrix}-1 & a & 1 \\ 1 & -a & a \\ 1 & 1 & -1 \\ \end{pmatrix}}{a+1}\right)$$(%i38) expand(%);$$\begin{pmatrix}\frac{a}{a+1}+\frac{1}{a+1} & 0 & 0 \\ 0 & \frac{a}{a+1}+\frac{1}{a+1} & 0 \\ 0 & 0 & \frac{a}{a+1}+\frac{1}{a+1} \\ \end{pmatrix}$$(%i39) factor(%);$$\begin{pmatrix}1 & 0 & 0 \\ 0 & 1 & 0 \\ 0 & 0 & 1 \\ \end{pmatrix}$$
