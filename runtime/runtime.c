# include <assert.h>
# include <stdbool.h>
# include <stdio.h>
# include <stdlib.h>
# include <memory.h>

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

extern int bi_read () {
  int d;
  printf ("> ");
  scanf ("%d", &d);
  return d;
}

extern void bi_write (int x) {
  printf ("%d\n", x);
}

extern void bi_writeb (Box *v) {
  assert(v->type == STR);
  fwrite (v->data, 1, v->str.len, stdout);
  printf ("\n");
}

Box* newbox(BoxType type, int data_len) {
  Box* res = malloc(sizeof(Box) + data_len);
  res->type = type;
  return res;
}

extern Box* bi_strmake(int n, int c) {
  Box *v = newbox(STR, n + 1);
  v->str.len = n;
  for (int i = 0; i < n; i++)
    v->data[i] = c;
  v->data[n] = 0;
  return v;
}

extern Box* bi_strset(Box *v, int i, int c) {
  assert(v->type == STR);
  assert(0 <= i && i < v->str.len);
  v->data[i] = c;
  return v;
}

extern int bi_strget(Box *v, int i) {
  assert(v->type == STR);
  assert(0 <= i && i < v->str.len);
  return v->data[i];
}

extern Box* bi_strdup(Box *v) {
  assert(v->type == STR);
  Box *v2 = newbox(STR, v->str.len + 1);
  v2->str.len = v->str.len;
  memcpy(v2->data, v->data, v->str.len + 1);
  return v2;
}

extern Box* bi_strcat(Box *v1, Box *v2) {
  assert(v1->type == STR);
  assert(v2->type == STR);
  Box *v = newbox(STR, v1->str.len + v2->str.len + 1);
  v->str.len = v1->str.len + v2->str.len;
  memcpy(v->data, v1->data, v1->str.len);
  memcpy(v->data + v1->str.len, v2->data, v2->str.len + 1);
  return v;
}

extern int bi_strlen(Box *v) {
  assert(v->type == STR);
  return v->str.len;
}

extern int bi_strcmp(Box *v1, Box *v2) {
  assert(v1->type == STR);
  assert(v2->type == STR);
  int l1 = v1->str.len;
  int l2 = v2->str.len;
  int minl = l1 < l2 ? l1 : l2;
  // TODO: memcmp compares unsigned bytes. Is it what we want?
  int res = memcmp(v1->data, v2->data, minl);
  if (res) {
      return res < 0 ? -1 : 1;
  } else {
      // One string is a prefix of another
      if (l1 == l2) {
          return 0;
      } else {
          return l1 < l2 ? -1 : 1;
      }
  }
}

extern Box* bi_strsub(Box *v, int i, int l) {
  assert(v->type == STR);
  assert(0 <= i);
  assert(i + l <= v->str.len);
  Box *nv = newbox(STR, l + 1);
  nv->str.len = l;
  memcpy(nv->data, v->data + i, l + 1);
  return nv;
}
