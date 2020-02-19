
include ./makefile.in

ifeq ($(MPP_BUILD), YES)
	SUBDIRS := share mpp phys ldas run
else
	SUBDIRS := share phys ldas run
endif

define submake
	for d in $(SUBDIRS); \
	do \
		$(MAKE) $(1) --directory=$$d; \
	done
endef

all: makefile.in
	$(call submake, $@)

clean: makefile.in
	$(call submake, $@)

.PHONY: all clean
