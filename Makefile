.PHONY: all clean

CXX ?= g++
CFLAGS += -w

all: evrostos NuSMV SPIN

evrostos:
	$(CXX) $(CFLAGS) ./helpers/evrostosSource.cpp -o evrostos

NuSMV:
	mkdir ./helpers/NuSMV/NuSMV/build
	cd ./helpers/NuSMV/NuSMV/build && cmake ..
	make -C ./helpers/NuSMV/NuSMV/build

SPIN:
	cd ./helpers/Spin && make

clean:
	-rm -f evrostos
	-rm -rf ./helpers/NuSMV/NuSMV/build