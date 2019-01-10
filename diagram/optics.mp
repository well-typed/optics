prologues      := 3;

% "Grid" dimensions

dimX=2.5cm;
dimY=-1.5cm;

% Path clipping

vardef clippathp(expr p, a, b) =
	save s, t, r;

	numeric s;
	numeric t;
	path r;

	(whatever, t1) = a intersectiontimes p;
	(whatever, t2) = b intersectiontimes (reverse p);

	subpath (t1, length(p) - t2) of p
enddef;

vardef clippath(expr p, a, b) =
	clippathp(p, bbox(a), bbox(b))
enddef;

% Latex prelude

verbatimtex
%&latex
\documentclass{article}
\usepackage{mathpazo}
\usepackage{helvet}
\usepackage{inconsolata}
\begin{document}
etex

% Colors

color blackColor, darkGreenColor, greenColor, blueColor, redColor, orangeColor, greyColor;

blackColor := (0, 0, 0);
greenColor := (0, 0.5, 0);
darkGreenColor := (0, 0.3, 0.3);
blueColor := (0, 0, 0.7);
redColor := (0.5, 0, 0);
orangeColor := (1, 0.5, 0);
greyColor := (0.5, 0.5, 0.5);

% Figure

beginfig(1);
picture q[];
path p[];
picture diag;

numeric t[];
path w[];

diag:=image(

%          1 
%          |
%          2
% 3    4       5    6

z1=(2dimX, 0dimY);
z2=(2dimX, 1dimY);

z3=(0dimX, 2dimY);
z4=(1dimX, 2dimY);
z5=(3dimX, 2dimY);
z6=(4dimX, 2dimY);

% z7=(0dimX, 3dimY);
z8=(3dimX, 4dimY); % (1dimX, 4dimY);
z9=(2dimX, 3dimY);

z10=(1dimX, 3dimY);
% z11=(0dimX, 4dimY);
z12=(3dimX, 5dimY); % (1dimX, 5dimY);
z13=(2dimX, 4dimY);

z14=(4dimX, 5dimY); % (2dimX, 5dimY);
z15=(3dimX, 3dimY);

% domain labels
q1 = thelabel(btex $\mathit{Equality}$ etex, z1);
q2 = thelabel(btex $\mathit{Iso}$ etex, z2);

q3 = thelabel(btex $\mathit{PrismyGetter}$ etex, z3);
q4 = thelabel(btex $\mathit{Lens}$ etex, z4);
q5 = thelabel(btex $\mathit{Prism}$ etex, z5);
q6 = thelabel(btex $\mathit{LensyReview}$ etex, z6);

% q7 = thelabel(btex $\mathit{Traversal1}$ etex, z7);
q8 = thelabel(btex $\mathit{Traversal}$ etex, z8);
q9 = thelabel(btex $\mathit{AffineTraversal}$ etex, z9);

q10 = thelabel(btex $\mathit{Getter}$ etex, z10);
% q11 = thelabel(btex $\mathit{Fold1}$ etex, z11);
q12 = thelabel(btex $\mathit{Fold}$ etex, z12);
q13 = thelabel(btex $\mathit{AffineFold}$ etex, z13);

q14 = thelabel(btex $\mathit{Setter}$ etex, z14);
q15 = thelabel(btex $\mathit{Review}$ etex, z15);

draw q1;
draw q2;
draw q3;
draw q4;
draw q5;
draw q6;
% draw q7;
draw q8;
draw q9;
draw q10;
% draw q11;
draw q12;
draw q13;
draw q14;
draw q15;

% arrows
p12 = z1..z2;

p23 = z2{dir 181}..z3;
p24 = z2..z4;
p25 = z2..z5;
p26 = z2{dir 359}..z6;


drawarrow clippath(p12, q1, q2); % withcolor blueColor;

drawarrow clippath(p23, q2, q3); % withcolor blueColor;
drawarrow clippath(p26, q2, q6); % withcolor blueColor;

% Iso -> Lens, Iso -> Prism
drawarrow clippath(p24, q2, q4); % withcolor blueColor;
drawarrow clippath(p25, q2, q5); % withcolor blueColor;

% LensyReview + Prism
p59 = z5 .. z9;
drawarrow clippath(z5 .. z15, q5, q15);
drawarrow clippath(p59, q5, q9);
drawarrow clippath(z6 .. z15, q6, q15);

vardef crossing(expr a, b, c, d, x, y, z, w, m, n) =
    save p, t, s;
    path p[];
    numeric t;
    path s;

    p0 = a..b;
    p1 = c..d;

    (whatever, t) = p0 intersectiontimes p1;
    s = fullcircle scaled 1bp shifted (point t of p1);

    drawarrow clippath(p0, x, y) withcolor m;
    draw clippath(subpath (0, t) of p1, z, s) withcolor n;
    drawarrow clippath(subpath (t, length p1) of p1, s, w) withcolor n;
enddef;


% To Folds
drawarrow clippath(z4 .. z10, q4, q10) withcolor greenColor;
% drawarrow clippath(z7 .. z11, q7, q11) withcolor greenColor;
drawarrow clippath(z8 .. z12, q8, q12) withcolor greenColor;
drawarrow clippath(z9 .. z13, q9, q13) withcolor greenColor;

% Lens diamond
p49 = z4 .. z9;
drawarrow clippath(p49, q4, q9) withcolor blueColor;

% Getter diamond
% drawarrow clippath(z11 .. z12, q11, q12) withcolor darkGreenColor;

%%% Crossing

% Lens -> Traveral1; PrismyGetter -> Getter
% crossing(z4, z7, z3, z10, q4, q7, q3, q10, blueColor, blackColor);
drawarrow clippath(z3..z10, q3, q10);

% Traveral1 -> Traversal; Getter -> Fold1
% crossing(z7, z8, z10, z11, q7, q8, q10, q11, blueColor, darkGreenColor);

% AffineTraversal -> Traversal; Getter -> AffineFold
% crossing(z9, z8, z10, z13, q9, q8, q10, q13, blueColor, darkGreenColor);
drawarrow clippath(z9 .. z8, q9, q8) withcolor blueColor;
drawarrow clippath(z10 .. z13, q10, q13) withcolor darkGreenColor;

% Traversal -> Setter; AffineFold -> Fold
% crossing(z8, z14, z13, z12, q8, q14, q13, q12, blueColor, darkGreenColor);
drawarrow clippath(z8..z14, q8, q14) withcolor blueColor;
drawarrow clippath(z13..z12, q13, q12) withcolor darkGreenColor;

%%% Red arrows

% Equality
z20=urcorner q1;
drawarrow (z20 + (-1mm,0)){dir 100}..(z20 + (5mm,5mm)){dir 315}..(z20 + (1mm,-1mm)){dir 170} withcolor redColor;

% Iso
z21=urcorner q2;
drawarrow (z21 + (-1mm,0)){dir 100}..(z21 + (5mm,5mm)){dir 315}..(z21 + (1mm,-1mm)){dir 170} withcolor redColor;

% PrismyGetter ^ LensyReview
p35 = z3{dir 20}..z5;
p46 = z4{dir 20}..z6;

(whatever, t3) = p24 intersectiontimes p35;
w3 = fullcircle scaled 1bp shifted (point t3 of p35);

(whatever, t4) = p25 intersectiontimes p46;
w4 = fullcircle scaled 1bp shifted (point t4 of p46);

(t5, t6) = p35 intersectiontimes p46;
w6 = fullcircle scaled 2bp shifted (point t6 of p46);

drawarrow reverse(clippath(subpath (0, t3) of p35, q3, w3)) withcolor redColor;
drawarrow clippath(subpath(t3, length p35) of p35, w3, q5) withcolor redColor;

drawarrow reverse(clippath(subpath (0, t6) of p46, q4, w6)) withcolor redColor;
draw clippath(subpath (t6, t4) of p46, w6, w4) withcolor redColor;
drawarrow clippath(subpath (t4, length p46) of p46, w4, q6) withcolor redColor;

% Getter -> Review
p1015 = z10{dir 30}..z15;

(whatever, t1) = p49 intersectiontimes p1015;
w1 = fullcircle scaled 1bp shifted (point t1 of p1015);
(whatever, t2) = p59 intersectiontimes p1015;
w2 = fullcircle scaled 1bp shifted (point t2 of p1015);

drawarrow clippath(reverse (subpath (0, t1) of p1015), w1, q10) withcolor redColor;
drawarrow clippath(subpath (t2, length p1015) of p1015, w2, q15) withcolor redColor;
draw clippath(subpath (t1, t2) of p1015, w1, w2) withcolor redColor;

);

unfill bbox(diag scaled 2);
draw diag scaled 2;

endfig;

verbatimtex
\end{document}
etex
end
