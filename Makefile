# Makefile for boot_format

INSTALL=install
PREFIX=/usr

all: boot_format

boot_format: boot_format.o
	$(CC) $< -o $@

install: boot_format
	$(INSTALL) -d $(DESTDIR)$(PREFIX)/bin
	$(INSTALL) boot_format $(DESTDIR)$(PREFIX)/bin/
	$(INSTALL) -d $(DESTDIR)$(PREFIX)/share
	$(INSTALL) config*.dat $(DESTDIR)$(PREFIX)/share/

clean:
	rm -rf *.o
	rm boot_format

