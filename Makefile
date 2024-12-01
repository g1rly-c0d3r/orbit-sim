PLPLOTMODPATH=${HOME}/.local/lib/fortran/modules/plplot/
PLPLOTPATH=/usr/lib/
F95=gfortran
F95FLAGS= -g -Og -I${HOME}/.local/include/ -I${PLPLOTMODPATH} -L ${PLPLOTPATH}

OBJS = body.o plot.o main.o 

all: $(OBJS)
	${F95} ${F95FLAGS} -o orbit_sim $^ -lplplotfortran

%.o: %.f95
	${F95} ${F95FLAGS} -c -o $@ $^ -lplplotfortran

run: all
	./orbit_sim -dev pngcairo
clean:
	rm -f ./target/data/* *.o *.mod orbit_sim
