---
title: "knitr engine test page"
---

L: sqrt(1 - 1/R^2);
assume(R > 0);
'integrate(x, x, 0, L) = integrate(x, x, 0, L);
## sqrt(1-1/R^2)
## [redundant]
## 'integrate(x,x,0,sqrt(1-1/R^2)) = (R^2-1)/(2*R^2)


'L = L;
'integrate(x, x, 0, 'L) = integrate(x, x, 0, L);
$$L=\sqrt{1-\frac{1}{R^2}}$$
$$\int_{0}^{L}{x\;dx}=\frac{R^2-1}{2\,R^2}$$


knitr::opts_chunk$set(maxima.format = "latex", results = 'asis')

sqrt(3/4);
<math xmlns="http://www.w3.org/1998/Math/MathML"> <mfrac><mrow>
 <msqrt><mn>3</mn> </msqrt></mrow> <mrow><mn>2</mn> </mrow></mfrac> </math>


f(x) := e^(x^2)$
diff(f(x), x);

$$2\,e^{x^2}\,\log e\,x$$


%;
<math xmlns="http://www.w3.org/1998/Math/MathML"> <mn>2</mn> 
 <mspace width="thinmathspace"/><msup><mrow><mi>e</mi> </mrow> <mrow>
 <msup><mrow><mi>x</mi> </mrow> <mn>2</mn> </msup> </mrow></msup> 
 <mspace width="thinmathspace"/><mi>log</mi> <mi>e</mi> 
 <mspace width="thinmathspace"/><mi>x</mi> </math>


log(%o1);
$$\frac{\log \left(1-\frac{1}{R^2}\right)}{2}$$


area(dist) := integrate(dist, x, minf, inf)$
mean(dist) := area(dist*x)$
EX2(dist) := area(dist*x^2)$
variance(dist) := EX2(dist) - mean(dist)^2$
mgf(dist) := area(dist*%e^(x*t))$

# Normal Distribution

normal(x) := 
      (2*%pi*sigma^2)^(-1/2) * 
      exp(-(x-mu)^2/(2*sigma^2));

assume(sigma > 0)$

area(normal(x));
mean(normal(x));
variance(normal(x));
mgf(normal(x));
$${\it normal}\left(x\right):=\left(2\,\pi\,\sigma^2\right)^{\frac{-1}{2}}\,\exp \left(\frac{-\left(x-\mu\right)^2}{2\,\sigma^2}\right)$$

$$1$$
$$\mu$$
$$\frac{2^{\frac{3}{2}}\,\sqrt{\pi}\,\sigma^3+2^{\frac{3}{2}}\,\sqrt{\pi}\,\mu^2\,\sigma}{2^{\frac{3}{2}}\,\sqrt{\pi}\,\sigma}-\mu^2$$
$$e^{\frac{\sigma^2\,t^2+2\,\mu\,t}{2}}$$


# Laplace Distribution

laplace(x) := (2*b)^-1 * exp(-abs(x - mu)/b);

load("abs_integrate")$

assume(b > 0)$

area(laplace(x));
mean(laplace(x));
variance(laplace(x));
$${\it laplace}\left(x\right):=\left(2\,b\right)^ {- 1 }\,\exp \left(\frac{-\left| x-\mu\right| }{b}\right)$$


$$1$$
$$\mu$$
$$\frac{2\,b\,\mu^2+4\,b^3}{2\,b}-\mu^2$$


# Exponential Distribution

expo(x) := unit_step(x) * lambda * exp(-lambda * x);

assume(lambda > 0)$

area(expo(x));
mean(expo(x));
variance(expo(x));
$${\it expo}\left(x\right):={\it unit\_step}\left(x\right)\,\lambda\,\exp \left(\left(-\lambda\right)\,x\right)$$

$$1$$
$$\frac{1}{\lambda}$$
$$\frac{1}{\lambda^2}$$

