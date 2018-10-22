.PHONY: all clean

CC ?= gcc
CFLAGS += -w

all: NuSMV evrostos

evrostos:
	$(CC) $(CFLAGS) evrostosSource.c -o evrostos

NuSMV:
	mkdir ./NuSMV-2.6.0/NuSMV/build
	cd ./NuSMV-2.6.0/NuSMV/build && cmake ..
	make -C ./NuSMV-2.6.0/NuSMV/build

clean:
	-rm -rf ./NuSMV-2.6.0/NuSMV/build
	-rm -f evrostos