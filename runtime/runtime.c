# include <assert.h>
# include <stdio.h>

typedef enum BoxType {
  STR = 0 // arbitrary length string
} BoxType;

typedef struct Box {
  BoxType type;
  union {
    // length of the string, data is implicitly terminated
    // with NULL (not included in length)
    int len;
  } str;
  char data[];
} Box;

extern int read () {
  int d;
  printf ("> ");
  scanf ("%d", &d);
  return d;
}

extern void write (int x) {
  printf ("%d\n", x);
}

extern void writeb (Box *v) {
  assert(v->type == STR);
  fwrite (v->data, 1, v->str.len, stdout);
  printf ("\n");
}
