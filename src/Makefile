BINDIR = ../bin
APP = spirograph_shiny.R

all:

install:
	install --mode 0444 -D runRShiny.R ${BINDIR}/runRShiny.R
	install --mode 0444 -D ${APP} ${BINDIR}/${APP}

clean:

distclean: clean
	rm -rf ${BINDIR}/runRShiny.R
	rm -rf ${BINDIR}/${APP}

.PHONY: all install clean distclean

