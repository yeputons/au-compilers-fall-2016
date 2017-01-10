# include <assert.h>
# include <stdbool.h>
# include <stdio.h>
# include <stdlib.h>
# include <stdarg.h>
# include <memory.h>

typedef enum BoxType {
  STR = 0, // arbitrary length string
  UARR = 1, // unboxed array
  BARR = 2  // boxed array
} BoxType;

typedef struct Box {
  BoxType type;
  union {
    struct {
      // length of the string, data is implicitly terminated
      // with NULL (not included in length)
      int len;
    } str;
    union {
      // number of dimensions in the array.
      // their sizes are stored in first ints of data[],
      // followed by real array data.
      int dims;
    } arr;
  };
  char data[];
} Box;

static int* get_arr_dims(Box *v) {
  return (int*)v->data;
}

static void* get_arr_start(Box *v) {
  return (int*)v->data + v->arr.dims;
}

static int get_arr_n(Box *v) {
  int n = 1;
  int *dims = get_arr_dims(v);
  for (int i = 0; i < v->arr.dims; i++)
    n *= dims[i];
  return n;
}

extern int bi_read () {
  int d;
  printf ("> ");
  scanf ("%d", &d);
  return d;
}

extern void bi_write (int x) {
  printf ("%d\n", x);
}

static void writeb(Box *v);

static void writea(int tag, int d, int *dims, int n, char* start) {
  if (d == 0) {
    if (tag == UARR) {
      assert(n == sizeof(int));
      printf("%d", *(int*)start);
    } else {
      assert(n == sizeof(Box*));
      writeb(*(Box**)start);
    }
    return;
  }
  printf(tag == UARR ? "[" : "{");
  int step = dims[0] ? n / dims[0] : 0;
  for (int i = 0; i < dims[0]; i++) {
    if (i > 0) {
      printf(", ");
    }
    writea(tag, d - 1, dims + 1, step, start);
    start += step;
  }
  printf(tag == UARR ? "]" : "}");
}

static void writeb(Box *v) {
  switch (v->type) {
  case STR:
    fwrite (v->data, 1, v->str.len, stdout);
    break;
  case UARR:
  case BARR: {
    int el_size = v->type == UARR ? sizeof(int) : sizeof(Box*);
    writea(
      v->type,
      v->arr.dims,
      get_arr_dims(v),
      el_size * get_arr_n(v),
      get_arr_start(v)
    );
  } break;
  default:
    assert(false && "Invalid Box type");
  }
}

extern void bi_writeb (Box *v) {
  writeb(v);
  printf("\n");
}

static Box* newbox(BoxType type, int data_len) {
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

extern Box* bi_reads() {
  static char buf[1024 * 1024];
  assert(fgets(buf, sizeof buf, stdin));
  int len = strlen(buf);
  while (len && strchr("\n\r", buf[len - 1])) {
    buf[--len] = 0;
  }
  Box *v = newbox(STR, len + 1);
  v->str.len = len;
  memcpy(v->data, buf, len + 1);
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

extern int bi_arrlen(Box *v) {
  assert(v->type == UARR || v->type == BARR);
  assert(v->arr.dims == 1);
  return get_arr_dims(v)[0];
}

extern int bi_arrlenm(Box *v, int i) {
  assert(v->type == UARR || v->type == BARR);
  assert(i < v->arr.dims);
  return get_arr_dims(v)[i];
}

static Box* prepare_arr(int tag, int d, va_list dims, int *n) {
  va_list dims2;
  va_copy(dims2, dims);
  *n = 1;
  for (int i = 0; i < d; i++) {
    *n *= va_arg(dims2, int);
  }
  va_end(dims2);

  Box *nv = newbox(tag, d * sizeof(int) + *n * sizeof(int));
  nv->arr.dims = d;
  int *nv_dims = get_arr_dims(nv);
  for (int i = 0; i < d; i++) {
    nv_dims[i] = va_arg(dims, int);
  }
  return nv;
}

extern Box* bi_arrmakem(int d, int v, ...) {
  int n;

  va_list dims;
  va_start(dims, v);
  Box *nv = prepare_arr(UARR, d, dims, &n);
  va_end(dims);

  int *arr = get_arr_start(nv);
  for (int i = 0; i < n; i++) {
    arr[i] = v;
  }
  return nv;
}

extern Box* bi_Arrmakem(int d, Box *v, ...) {
  int n;

  va_list dims;
  va_start(dims, v);
  Box *nv = prepare_arr(BARR, d,  dims, &n);
  va_end(dims);

  Box **arr = get_arr_start(nv);
  for (int i = 0; i < n; i++) {
    arr[i] = v;
  }
  return nv;
}

extern Box* bi_arrmake(int n, int v) {
  return bi_arrmakem(1, v, n);
}

extern Box* bi_Arrmake(int n, Box *v) {
  return bi_Arrmakem(1, v, n);
}
