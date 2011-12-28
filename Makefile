# Makefile for boot_format

INSTALL=install
PREFIX=/usr

all: boot_format

boot_format: boot_format.o
	$(CC) $(CFLAGS) $(LDFLAGS) $< -o $@

install: boot_format
	$(INSTALL) -d $(DESTDIR)$(PREFIX)/bin
	$(INSTALL) boot_format $(DESTDIR)$(PREFIX)/bin/
	$(INSTALL) -d $(DESTDIR)$(PREFIX)/share/boot_format
	$(INSTALL) config*.dat $(DESTDIR)$(PREFIX)/share/boot_format

clean:
	rm -f *.o boot_format
