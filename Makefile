.PHONY: all clean

CXX ?= g++
CFLAGS += -w

all: evrostos NuSMV SPIN

evrostos:
	$(CXX) $(CFLAGS) ./helpers/evrostosSource.cpp -o evrostos

NuSMV:
	mkdir ./helpers/NuSMV-2.6.0/NuSMV/build
	cd ./helpers/NuSMV-2.6.0/NuSMV/build && cmake ..
	make -C ./helpers/NuSMV-2.6.0/NuSMV/build

SPIN:
	cd ./helpers/Spin && make

clean:
	-rm -rf ./helpers/NuSMV-2.6.0/NuSMV/build
	-rm -f evrostos