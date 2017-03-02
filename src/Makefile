BINDIR = ../bin
APP = spirograph_shiny.R

all:

install:
	install --mode 0444 -D ${APP} ${BINDIR}/${APP}

clean:

distclean: clean
	rm -rf ${BINDIR}/${APP}

.PHONY: all install clean distclean

