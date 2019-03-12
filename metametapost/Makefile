SVGIMAGES=optics reoptics indexedoptics

all : $(PNGIMAGES:%=%.png) $(SVGIMAGES:%=%.svg) $(SVGIMAGES:%=%.png) $(SVGIMAGES:%=%.mps)

%.png : %.dot
	dot -Tpng $< -o $@

MPOST=mpost
SVGPARAMS=-s defaultscale:=8 -s outputformat='"svg"' -s outputtemplate='"%j.svg"'
MPSPARAMS=-s outputformat='"mps"' -s outputtemplate='"%j.mps"'

%.mp : %.hs
	runhaskell $< > $@

%.mps : %.mp
	$(MPOST) $(MPSPARAMS) $<

%.svg : %.mp
	$(MPOST) $(SVGPARAMS) $<

%.png : %.svg
	inkscape --export-png=$@ --export-dpi=108 --export-background-opacity=0 --without-gui $<
