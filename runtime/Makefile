.PHONE: all clean

all: runtime.o

%.o: %.c
	gcc -m32 -c $^ -o $@

clean:
	rm -f runtime.o *~

