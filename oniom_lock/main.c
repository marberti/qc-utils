#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define LIST_LEN 1000

int compare_int(const void *a, const void *b);

int main(int argc, char *argv[]) {
  int list_i;
  int list[LIST_LEN];
  int rscanf, val;
  char *fname_in;
  char *fname_out = "locked.xyz";
  FILE *fd_in;
  FILE *fd_out;
  int an;
  char line_mod[200+1];
  const size_t line_mod_len = 200;
  char *line;
  size_t line_n;
  int flag_lock;
  int flag_name;

  if (argc != 2) {
    printf("Usage: %s file_xyz\n",argv[0]);
    return 1;
  }
  fname_in = argv[1];

  for (int i = 0; i < LIST_LEN; i++) {
    list[i] = -1;
  }

  for (int i = 0; i < LIST_LEN; i++) {
    rscanf = fscanf(stdin,"%d",&val);
    if ((rscanf == 0)||(rscanf == EOF)) {
      printf("To lock: %d nuclei\n",i);
      break;
    }
    list[i] = val;
  }

  qsort(list,LIST_LEN,sizeof(int),compare_int);

  for (list_i = 0; list_i < LIST_LEN; list_i++) {
    if (list[list_i] != -1) break;
  }

  /*
  for (int i = list_i; i < LIST_LEN; i++) {
    printf("%d\n",list[i]);
  }
  */

  fd_in = fopen(fname_in,"r");
  if (fd_in == NULL) {
    printf("Error: open input file\n");
    return 1;
  }
  fd_out = fopen(fname_out,"w");
  if (fd_out == NULL) {
    printf("Error: open output file\n");
    return 1;
  }

  line = NULL;
  line_n = 0;
  getline(&line,&line_n,fd_in);
  sscanf(line,"%d",&an);
  fprintf(fd_out,"%s",line);
  line = NULL;
  line_n = 0;
  getline(&line,&line_n,fd_in);
  fprintf(fd_out,"%s",line);

  for (int i = 1; i <= an; i++) {
    line = NULL;
    line_n = 0;
    getline(&line,&line_n,fd_in);

    if (i == list[list_i]) {
      flag_lock = 1;
      list_i++;
    } else {
      flag_lock = 0;
    }

    if (flag_lock) {
      strncpy(line_mod,line,line_mod_len);
      flag_name = 1;
      for (int j = 0; j < strlen(line_mod); j++) {
        if (flag_name) {
          if (line_mod[j] == ' ') {
            flag_name = 0;
          }
        } else {
          if (line_mod[j] == '0') {
            line_mod[j-1] = '-';
            line_mod[j] = '1';
            break;
          } else if (line_mod[j] == '-') {
            break;
          }
        }
      }
      fprintf(fd_out,"%s",line_mod);
    } else {
      fprintf(fd_out,"%s",line);
    }
  }

  fclose(fd_in);
  fclose(fd_out);

  return 0;
}

int compare_int(const void *aa, const void *bb) {
  int a, b;
  a = *(int *) aa;
  b = *(int *) bb;

  if (a < b) {
    return -1;
  } else if (a == b) {
    return 0;
  } else {
    return 1;
  }
}
