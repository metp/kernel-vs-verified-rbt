KDIR ?= /home/mete/perf/linux

.PHONY: all clean install uninstall

all:
	$(MAKE) -C $(KDIR) M=$$PWD

clean:
	$(MAKE) -C $(KDIR) M=$$PWD clean

install:
	insmod rbt_interface.ko

uninstall:
	rmmod rbt_interface
