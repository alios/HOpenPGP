PROJ=httpd
OBJS= httpd.o
INCLUDE=

CC=gcc
LD=gcc

CFLAGS=-ansi -g -pedantic -Wall -Wcast-align -Wcast-qual \
    -Wchar-subscripts -Winline -Wpointer-arith -Wredundant-decls -Wshadow \
    -Wwrite-strings -ffunction-sections -pthread $(INCLUDE)

LDFLAGS=-Wl,-Map,$(PROJ).map,--gc-sections

GENDEPFLAGS=-MD -MP -MF .deps/$(@F).d

all: $(PROJ)

clean:
	rm -rf .deps $(OBJS) $(PROJ) $(PROJ).map

$(PROJ): $(OBJS)
	$(LD) $(LDFLAGS) $(OBJS) -o $@

%.o : %.c
	$(CC) $(CFLAGS) $(GENDEPFLAGS) -o $@ -c $<

-include $(shell mkdir -p .deps 2>/dev/null) $(wildcard .deps/*)

.PHONY : all clean
