#
#  steering for event_monitor build
#
#  ejw, 6-may-98
#

PROG = level3


all: depends $(PROG)

depends:
	mkdepend *.f

$(PROG): depend.mk
	$(MAKE) -f Makefile.prog $(PROG)

clean:
	rm -f core *.o depend.mk

distclean:
	rm -f core *.o depend.mk $(PROG) 


install: exports
exports:
	cp $(PROG) $(CLON_DEVBIN)
