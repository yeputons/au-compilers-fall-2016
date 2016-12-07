.PHONE: all clean

all: runtime.o

%.o: %.c
	gcc -m32 -c $^ -o $@ -std=c99 -Wall -Wextra -Werror

clean:
	rm -f runtime.o *~

