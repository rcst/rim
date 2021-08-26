---
title: "knitr engine test page"
output: 
   pdf_document:
     keep_tex: true
---

maxima.options(engine.format = "latex", 
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

# Normal Distribution

(%i17) normal(x) := 
      (2*%pi*sigma^2)^(-1/2) * 
      exp(-(x-mu)^2/(2*sigma^2));$$\mathtt{(\textit{\%o}_{17})}\quad \textit{normal}\left(x\right):=\left(2\,\pi\,\sigma^2\right)^{\frac{-1}{2}}\,\exp \left(\frac{-\left(x-\mu\right)^2}{2\,\sigma^2}\right)$$
(%i18) assume(sigma > 0)$(%i19) area(normal(x));$$\mathtt{(\textit{\%o}_{19})}\quad 1$$
(%i20) mean(normal(x));$$\mathtt{(\textit{\%o}_{20})}\quad \mu$$
(%i21) variance(normal(x));$$\mathtt{(\textit{\%o}_{21})}\quad \frac{2^{\frac{3}{2}}\,\sqrt{\pi}\,\sigma^3+2^{\frac{3}{2}}\,\sqrt{\pi}\,\mu^2\,\sigma}{2^{\frac{3}{2}}\,\sqrt{\pi}\,\sigma}-\mu^2$$
(%i22) mgf(normal(x));$$\mathtt{(\textit{\%o}_{22})}\quad e^{\frac{\sigma^2\,t^2+2\,\mu\,t}{2}}$$


# Laplace Distribution

(%i23) laplace(x) := (2*b)^-1 * exp(-abs(x - mu)/b);$$\mathtt{(\textit{\%o}_{23})}\quad \textit{laplace}\left(x\right):=\left(2\,b\right)^ {- 1 }\,\exp \left(\frac{-\left| x-\mu\right| }{b}\right)$$
(%i24) load("abs_integrate")$(%i25) assume(b > 0)$(%i26) area(laplace(x));$$\mathtt{(\textit{\%o}_{26})}\quad 1$$
(%i27) mean(laplace(x));$$\mathtt{(\textit{\%o}_{27})}\quad \mu$$
(%i28) variance(laplace(x));$$\mathtt{(\textit{\%o}_{28})}\quad \frac{2\,b\,\mu^2+4\,b^3}{2\,b}-\mu^2$$


# Exponential Distribution

(%i29) expo(x) := unit_step(x) * lambda * exp(-lambda * x);$$\mathtt{(\textit{\%o}_{29})}\quad \textit{expo}\left(x\right):=\textit{unit\_step}\left(x\right)\,\lambda\,\exp \left(\left(-\lambda\right)\,x\right)$$
(%i30) assume(lambda > 0)$(%i31) area(expo(x));$$\mathtt{(\textit{\%o}_{31})}\quad 1$$
(%i32) mean(expo(x));$$\mathtt{(\textit{\%o}_{32})}\quad \frac{1}{\lambda}$$
(%i33) variance(expo(x));$$\mathtt{(\textit{\%o}_{33})}\quad \frac{1}{\lambda^2}$$


# Matrices

(%i34) m: matrix([0, 1, a], [1, 0, 1], [1, 1, 0]);$$\mathtt{(\textit{\%o}_{34})}\quad \begin{pmatrix}0 & 1 & a \\ 1 & 0 & 1 \\ 1 & 1 & 0 \\ \end{pmatrix}$$
(%i35) transpose(m);$$\mathtt{(\textit{\%o}_{35})}\quad \begin{pmatrix}0 & 1 & 1 \\ 1 & 0 & 1 \\ a & 1 & 0 \\ \end{pmatrix}$$
(%i36) determinant(m);$$\mathtt{(\textit{\%o}_{36})}\quad a+1$$
(%i37) f: invert(m), detout;$$\mathtt{(\textit{\%o}_{37})}\quad \frac{\begin{pmatrix}-1 & a & 1 \\ 1 & -a & a \\ 1 & 1 & -1 \\ \end{pmatrix}}{a+1}$$
(%i38) m . f;$$\mathtt{(\textit{\%o}_{38})}\quad \begin{pmatrix}0 & 1 & a \\ 1 & 0 & 1 \\ 1 & 1 & 0 \\ \end{pmatrix}\cdot \left(\frac{\begin{pmatrix}-1 & a & 1 \\ 1 & -a & a \\ 1 & 1 & -1 \\ \end{pmatrix}}{a+1}\right)$$
(%i39) expand(%);$$\mathtt{(\textit{\%o}_{39})}\quad \begin{pmatrix}\frac{a}{a+1}+\frac{1}{a+1} & 0 & 0 \\ 0 & \frac{a}{a+1}+\frac{1}{a+1} & 0 \\ 0 & 0 & \frac{a}{a+1}+\frac{1}{a+1} \\ \end{pmatrix}$$
(%i40) factor(%);$$\mathtt{(\textit{\%o}_{40})}\quad \begin{pmatrix}1 & 0 & 0 \\ 0 & 1 & 0 \\ 0 & 0 & 1 \\ \end{pmatrix}$$


# If-then-else

(%i41) x: 1234;$$\mathtt{(\textit{\%o}_{41})}\quad 1234$$
(%i42) y: 2345;$$\mathtt{(\textit{\%o}_{42})}\quad 2345$$


(%i43) if x > y
  then x
  else y;$$\mathtt{(\textit{\%o}_{43})}\quad 2345$$

