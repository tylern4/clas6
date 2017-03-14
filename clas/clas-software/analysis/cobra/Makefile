#! gnumake

all: ptypes_dir parray_dir clasruns_dir kinfit_dir event_dir clasevent_dir

ptypes_dir: ptypes/*.h ptypes/*.C
	@cd ptypes && $(MAKE)

parray_dir: parray/*.h parray/*.C
	@cd parray && $(MAKE)

clasruns_dir: clasruns/*.h clasruns/*.C
	@cd clasruns && $(MAKE)

kinfit_dir: kinfit/*.h kinfit/*.C
	@cd kinfit && $(MAKE)

event_dir: event/*.h event/*.C
	@cd event && $(MAKE)

clasevent_dir: clasevent/*.h clasevent/*.C
	@cd clasevent && $(MAKE)

clean:;
	@cd ptypes && $(MAKE) clean
	@cd parray && $(MAKE) clean
	@cd clasruns && $(MAKE) clean
	@cd kinfit && $(MAKE) clean
	@cd event && $(MAKE) clean
	@cd clasevent && $(MAKE) clean
	@rm -f lib/*.so
