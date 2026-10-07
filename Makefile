.PHONY: all clean

CXX ?= g++
CFLAGS += -w

all: evrostos rLTL2LTL NuSMV SPIN

evrostos:
	$(CXX) $(CFLAGS) ./modules/evrostosSource.cpp -o evrostos

rLTL2LTL:
	cd ./modules/rltl2ltl && ant
	cd ./modules/rltl2ltl && ant jar

NuSMV:
	mkdir ./modules/NuSMV-2.6.0/NuSMV/build
	cd ./modules/NuSMV-2.6.0/NuSMV/build && cmake ..
	make -C ./modules/NuSMV-2.6.0/NuSMV/build

SPIN:
	cd ./modules/Spin && make

clean:
	-rm -rf ./modules/NuSMV-2.6.0/NuSMV/build
	-rm -f evrostos