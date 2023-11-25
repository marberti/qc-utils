#include <stdio.h>
#include <error.h>
#include <string.h>
#include <ctype.h>

#define XYZ_N 8
#define E_LEN 120
#define O_LEN 4002

struct xyz_dat_s {
  int an;
  int en;
  int e[E_LEN];
  char o[O_LEN+1];
};

void init_xyz_struct(struct xyz_dat_s *x);
void analyze_xyz(char *fname, struct xyz_dat_s *x);
void write_analysis_results(struct xyz_dat_s *x, int x_n);
int get_atomic_number(char *atom);
void get_atom_name(int an, char *name);

int main(int argc, char **argv)
{
  int xyz_n;
  int i;
  struct xyz_dat_s xyz[XYZ_N];

  if (argc == 1) {
    error(1,0,"At least one argument needed");
  }

  xyz_n = argc - 1;
  if (xyz_n > XYZ_N) {
    error(1,0,"Max %d xyz files can be specified",XYZ_N);
  }
  printf("Total xyz files to analyze: %d\n",xyz_n);

  for (i = 0; i < xyz_n; i++) {
    init_xyz_struct(&xyz[i]);
  }

  for (i = 0; i < xyz_n; i++) {
    analyze_xyz(argv[i+1],&xyz[i]);
  }

  write_analysis_results(xyz,xyz_n);

  return 0;
}

void init_xyz_struct(struct xyz_dat_s *x) {
  int i;

  x->an = 0;
  x->en = 0;
  for (i = 0; i < E_LEN; i++) {
    x->e[i] = 0;
  }
  x->o[0] = '\0';
}

void analyze_xyz(char *fname, struct xyz_dat_s *x) {
  FILE *fd;
  int an;
  int en;
  int buff_len = 256;
  char buff[buff_len];
  char aname[3];
  int i, j, oi, n;

  if ((fd = fopen(fname,"r")) == NULL) {
    error(1,0,"Cannot open %s",fname);
  }

  oi = 0;
  en = 0;
  fgets(buff,buff_len,fd);
  sscanf(buff,"%d",&an);
  fgets(buff,buff_len,fd);

  if (an > O_LEN/2 - 1) {
    error(1,0,"Supported xyz files of max %d atoms",O_LEN/2 - 1);
  }

  for (i = 0; i < an; i++) {
    fgets(buff,buff_len,fd);

    for (j = 0; (j < strlen(buff)) && (buff[j] == ' '); j++);
    if (j+1 >= strlen(buff)) error(1,0,"Bad formatting in %s",fname);

    aname[0] = buff[j];
    aname[1] = buff[j+1];
    aname[2] = '\0';

    x->o[oi]   = aname[0];
    x->o[oi+1] = aname[1];
    x->o[oi+2] = '\0';
    oi += 2;

    if (aname[1] == ' ') aname[1] = '\0';

    n = get_atomic_number(aname);
    en += n;
    x->e[n] += 1;
  }

  x->an = an;
  x->en = en;

  if (fclose(fd) != 0) {
    error(1,0,"Cannot close %s",fname);
  }
}

void write_analysis_results(struct xyz_dat_s *x, int x_n) {
  int i, j, eq;
  char found_e[E_LEN];
  char el[3];

  for (i = 0; i < E_LEN; i++) {
    found_e[i] = 0;
  }

  for (i = 0; i < E_LEN; i++) {
    for (j = 0; (j < x_n) && (found_e[i] == 0); j++) {
      if (x[j].e[i] != 0) found_e[i] = 1;
    }
  }

  printf("File");
  for (i = 0; i < x_n; i++) {
    printf("\t\t   F%d",i+1);
  }
  printf("\n");

  printf("Nuclei");
  for (i = 0; i < x_n; i++) {
    printf("\t\t%5d",x[i].an);
  }
  printf("\n");

  for (i = 0; i < E_LEN; i++) {
    if (found_e[i] == 1) {
      get_atom_name(i,el);
      printf("  %-2s",el);
      for (j = 0; j < x_n; j++) {
        printf("\t\t%5d",x[j].e[i]);
      }
      printf("\n");
    }
  }

  printf("Electr");
  for (i = 0; i < x_n; i++) {
    printf("\t\t%5d",x[i].en);
  }
  printf("\n");

  if (x_n > 1) {
    printf("\nSimilarity Matrix (first N nuclei equal)\n");
    for (i = 1; i < x_n; i++) {
      printf("\t\t   F%d",i+1);
    }
    printf("\n");
    for (i = 0; i < x_n - 1; i++) {
      printf("F%d",i+1);
      for (j = 1; j < x_n; j++) {
        if (j <= i) printf("\t\t");
        else {
          for (eq = 0; (eq < O_LEN) && (x[i].o[eq] == x[j].o[eq]); eq++);
          printf("\t\t%5d",eq / 2);
        }
      }
      printf("\n");
    }
  }
}

int get_atomic_number(char *atom) {
  char a[3];
  int an;

  strncpy(a,atom,2);
  a[0] = toupper(a[0]);
  a[1] = tolower(a[1]);
  a[2] = '\0';

  an = 0;

  if (strcmp(a,"H")   == 0) an =   1;
  if (strcmp(a,"He")  == 0) an =   2;
  if (strcmp(a,"Li")  == 0) an =   3;
  if (strcmp(a,"Be")  == 0) an =   4;
  if (strcmp(a,"B")   == 0) an =   5;
  if (strcmp(a,"C")   == 0) an =   6;
  if (strcmp(a,"N")   == 0) an =   7;
  if (strcmp(a,"O")   == 0) an =   8;
  if (strcmp(a,"F")   == 0) an =   9;
  if (strcmp(a,"Ne")  == 0) an =  10;
  if (strcmp(a,"Na")  == 0) an =  11;
  if (strcmp(a,"Mg")  == 0) an =  12;
  if (strcmp(a,"Al")  == 0) an =  13;
  if (strcmp(a,"Si")  == 0) an =  14;
  if (strcmp(a,"P")   == 0) an =  15;
  if (strcmp(a,"S")   == 0) an =  16;
  if (strcmp(a,"Cl")  == 0) an =  17;
  if (strcmp(a,"Ar")  == 0) an =  18;
  if (strcmp(a,"K")   == 0) an =  19;
  if (strcmp(a,"Ca")  == 0) an =  20;
  if (strcmp(a,"Sc")  == 0) an =  21;
  if (strcmp(a,"Ti")  == 0) an =  22;
  if (strcmp(a,"V")   == 0) an =  23;
  if (strcmp(a,"Cr")  == 0) an =  24;
  if (strcmp(a,"Mn")  == 0) an =  25;
  if (strcmp(a,"Fe")  == 0) an =  26;
  if (strcmp(a,"Co")  == 0) an =  27;
  if (strcmp(a,"Ni")  == 0) an =  28;
  if (strcmp(a,"Cu")  == 0) an =  29;
  if (strcmp(a,"Zn")  == 0) an =  30;
  if (strcmp(a,"Ga")  == 0) an =  31;
  if (strcmp(a,"Ge")  == 0) an =  32;
  if (strcmp(a,"As")  == 0) an =  33;
  if (strcmp(a,"Se")  == 0) an =  34;
  if (strcmp(a,"Br")  == 0) an =  35;
  if (strcmp(a,"Kr")  == 0) an =  36;
  if (strcmp(a,"Rb")  == 0) an =  37;
  if (strcmp(a,"Sr")  == 0) an =  38;
  if (strcmp(a,"Y")   == 0) an =  39;
  if (strcmp(a,"Zr")  == 0) an =  40;
  if (strcmp(a,"Nb")  == 0) an =  41;
  if (strcmp(a,"Mo")  == 0) an =  42;
  if (strcmp(a,"Tc")  == 0) an =  43;
  if (strcmp(a,"Ru")  == 0) an =  44;
  if (strcmp(a,"Rh")  == 0) an =  45;
  if (strcmp(a,"Pd")  == 0) an =  46;
  if (strcmp(a,"Ag")  == 0) an =  47;
  if (strcmp(a,"Cd")  == 0) an =  48;
  if (strcmp(a,"In")  == 0) an =  49;
  if (strcmp(a,"Sn")  == 0) an =  50;
  if (strcmp(a,"Sb")  == 0) an =  51;
  if (strcmp(a,"Te")  == 0) an =  52;
  if (strcmp(a,"I")   == 0) an =  53;
  if (strcmp(a,"Xe")  == 0) an =  54;
  if (strcmp(a,"Cs")  == 0) an =  55;
  if (strcmp(a,"Ba")  == 0) an =  56;
  if (strcmp(a,"La")  == 0) an =  57;
  if (strcmp(a,"Ce")  == 0) an =  58;
  if (strcmp(a,"Pr")  == 0) an =  59;
  if (strcmp(a,"Nd")  == 0) an =  60;
  if (strcmp(a,"Pm")  == 0) an =  61;
  if (strcmp(a,"Sm")  == 0) an =  62;
  if (strcmp(a,"Eu")  == 0) an =  63;
  if (strcmp(a,"Gd")  == 0) an =  64;
  if (strcmp(a,"Tb")  == 0) an =  65;
  if (strcmp(a,"Dy")  == 0) an =  66;
  if (strcmp(a,"Ho")  == 0) an =  67;
  if (strcmp(a,"Er")  == 0) an =  68;
  if (strcmp(a,"Tm")  == 0) an =  69;
  if (strcmp(a,"Yb")  == 0) an =  70;
  if (strcmp(a,"Lu")  == 0) an =  71;
  if (strcmp(a,"Hf")  == 0) an =  72;
  if (strcmp(a,"Ta")  == 0) an =  73;
  if (strcmp(a,"W")   == 0) an =  74;
  if (strcmp(a,"Re")  == 0) an =  75;
  if (strcmp(a,"Os")  == 0) an =  76;
  if (strcmp(a,"Ir")  == 0) an =  77;
  if (strcmp(a,"Pt")  == 0) an =  78;
  if (strcmp(a,"Au")  == 0) an =  79;
  if (strcmp(a,"Hg")  == 0) an =  80;
  if (strcmp(a,"Tl")  == 0) an =  81;
  if (strcmp(a,"Pb")  == 0) an =  82;
  if (strcmp(a,"Bi")  == 0) an =  83;
  if (strcmp(a,"Po")  == 0) an =  84;
  if (strcmp(a,"At")  == 0) an =  85;
  if (strcmp(a,"Rn")  == 0) an =  86;
  if (strcmp(a,"Fr")  == 0) an =  87;
  if (strcmp(a,"Ra")  == 0) an =  88;
  if (strcmp(a,"Ac")  == 0) an =  89;
  if (strcmp(a,"Th")  == 0) an =  90;
  if (strcmp(a,"Pa")  == 0) an =  91;
  if (strcmp(a,"U")   == 0) an =  92;
  if (strcmp(a,"Np")  == 0) an =  93;
  if (strcmp(a,"Pu")  == 0) an =  94;
  if (strcmp(a,"Am")  == 0) an =  95;
  if (strcmp(a,"Cm")  == 0) an =  96;
  if (strcmp(a,"Bk")  == 0) an =  97;
  if (strcmp(a,"Cf")  == 0) an =  98;
  if (strcmp(a,"Es")  == 0) an =  99;
  if (strcmp(a,"Fm")  == 0) an = 100;
  if (strcmp(a,"Md")  == 0) an = 101;
  if (strcmp(a,"No")  == 0) an = 102;
  if (strcmp(a,"Lr")  == 0) an = 103;
  if (strcmp(a,"Rf")  == 0) an = 104;
  if (strcmp(a,"Db")  == 0) an = 105;
  if (strcmp(a,"Sg")  == 0) an = 106;
  if (strcmp(a,"Bh")  == 0) an = 107;
  if (strcmp(a,"Hs")  == 0) an = 108;
  if (strcmp(a,"Mt")  == 0) an = 109;
  if (strcmp(a,"Ds")  == 0) an = 110;
  if (strcmp(a,"Rg")  == 0) an = 111;
  if (strcmp(a,"Cn")  == 0) an = 112;
  if (strcmp(a,"Nh")  == 0) an = 113;
  if (strcmp(a,"Fl")  == 0) an = 114;
  if (strcmp(a,"Mc")  == 0) an = 115;
  if (strcmp(a,"Lv")  == 0) an = 116;
  if (strcmp(a,"Ts")  == 0) an = 117;
  if (strcmp(a,"Og")  == 0) an = 118;

  return an;
}

void get_atom_name(int an, char *name) {
  if (an <=   0) strcpy(name,"--");
  if (an ==   1) strcpy(name,"H" );
  if (an ==   2) strcpy(name,"He");
  if (an ==   3) strcpy(name,"Li");
  if (an ==   4) strcpy(name,"Be");
  if (an ==   5) strcpy(name,"B" );
  if (an ==   6) strcpy(name,"C" );
  if (an ==   7) strcpy(name,"N" );
  if (an ==   8) strcpy(name,"O" );
  if (an ==   9) strcpy(name,"F" );
  if (an ==  10) strcpy(name,"Ne");
  if (an ==  11) strcpy(name,"Na");
  if (an ==  12) strcpy(name,"Mg");
  if (an ==  13) strcpy(name,"Al");
  if (an ==  14) strcpy(name,"Si");
  if (an ==  15) strcpy(name,"P" );
  if (an ==  16) strcpy(name,"S" );
  if (an ==  17) strcpy(name,"Cl");
  if (an ==  18) strcpy(name,"Ar");
  if (an ==  19) strcpy(name,"K" );
  if (an ==  20) strcpy(name,"Ca");
  if (an ==  21) strcpy(name,"Sc");
  if (an ==  22) strcpy(name,"Ti");
  if (an ==  23) strcpy(name,"V" );
  if (an ==  24) strcpy(name,"Cr");
  if (an ==  25) strcpy(name,"Mn");
  if (an ==  26) strcpy(name,"Fe");
  if (an ==  27) strcpy(name,"Co");
  if (an ==  28) strcpy(name,"Ni");
  if (an ==  29) strcpy(name,"Cu");
  if (an ==  30) strcpy(name,"Zn");
  if (an ==  31) strcpy(name,"Ga");
  if (an ==  32) strcpy(name,"Ge");
  if (an ==  33) strcpy(name,"As");
  if (an ==  34) strcpy(name,"Se");
  if (an ==  35) strcpy(name,"Br");
  if (an ==  36) strcpy(name,"Kr");
  if (an ==  37) strcpy(name,"Rb");
  if (an ==  38) strcpy(name,"Sr");
  if (an ==  39) strcpy(name,"Y" );
  if (an ==  40) strcpy(name,"Zr");
  if (an ==  41) strcpy(name,"Nb");
  if (an ==  42) strcpy(name,"Mo");
  if (an ==  43) strcpy(name,"Tc");
  if (an ==  44) strcpy(name,"Ru");
  if (an ==  45) strcpy(name,"Rh");
  if (an ==  46) strcpy(name,"Pd");
  if (an ==  47) strcpy(name,"Ag");
  if (an ==  48) strcpy(name,"Cd");
  if (an ==  49) strcpy(name,"In");
  if (an ==  50) strcpy(name,"Sn");
  if (an ==  51) strcpy(name,"Sb");
  if (an ==  52) strcpy(name,"Te");
  if (an ==  53) strcpy(name,"I" );
  if (an ==  54) strcpy(name,"Xe");
  if (an ==  55) strcpy(name,"Cs");
  if (an ==  56) strcpy(name,"Ba");
  if (an ==  57) strcpy(name,"La");
  if (an ==  58) strcpy(name,"Ce");
  if (an ==  59) strcpy(name,"Pr");
  if (an ==  60) strcpy(name,"Nd");
  if (an ==  61) strcpy(name,"Pm");
  if (an ==  62) strcpy(name,"Sm");
  if (an ==  63) strcpy(name,"Eu");
  if (an ==  64) strcpy(name,"Gd");
  if (an ==  65) strcpy(name,"Tb");
  if (an ==  66) strcpy(name,"Dy");
  if (an ==  67) strcpy(name,"Ho");
  if (an ==  68) strcpy(name,"Er");
  if (an ==  69) strcpy(name,"Tm");
  if (an ==  70) strcpy(name,"Yb");
  if (an ==  71) strcpy(name,"Lu");
  if (an ==  72) strcpy(name,"Hf");
  if (an ==  73) strcpy(name,"Ta");
  if (an ==  74) strcpy(name,"W" );
  if (an ==  75) strcpy(name,"Re");
  if (an ==  76) strcpy(name,"Os");
  if (an ==  77) strcpy(name,"Ir");
  if (an ==  78) strcpy(name,"Pt");
  if (an ==  79) strcpy(name,"Au");
  if (an ==  80) strcpy(name,"Hg");
  if (an ==  81) strcpy(name,"Tl");
  if (an ==  82) strcpy(name,"Pb");
  if (an ==  83) strcpy(name,"Bi");
  if (an ==  84) strcpy(name,"Po");
  if (an ==  85) strcpy(name,"At");
  if (an ==  86) strcpy(name,"Rn");
  if (an ==  87) strcpy(name,"Fr");
  if (an ==  88) strcpy(name,"Ra");
  if (an ==  89) strcpy(name,"Ac");
  if (an ==  90) strcpy(name,"Th");
  if (an ==  91) strcpy(name,"Pa");
  if (an ==  92) strcpy(name,"U" );
  if (an ==  93) strcpy(name,"Np");
  if (an ==  94) strcpy(name,"Pu");
  if (an ==  95) strcpy(name,"Am");
  if (an ==  96) strcpy(name,"Cm");
  if (an ==  97) strcpy(name,"Bk");
  if (an ==  98) strcpy(name,"Cf");
  if (an ==  99) strcpy(name,"Es");
  if (an == 100) strcpy(name,"Fm");
  if (an == 101) strcpy(name,"Md");
  if (an == 102) strcpy(name,"No");
  if (an == 103) strcpy(name,"Lr");
  if (an == 104) strcpy(name,"Rf");
  if (an == 105) strcpy(name,"Db");
  if (an == 106) strcpy(name,"Sg");
  if (an == 107) strcpy(name,"Bh");
  if (an == 108) strcpy(name,"Hs");
  if (an == 109) strcpy(name,"Mt");
  if (an == 110) strcpy(name,"Ds");
  if (an == 111) strcpy(name,"Rg");
  if (an == 112) strcpy(name,"Cn");
  if (an == 113) strcpy(name,"Nh");
  if (an == 114) strcpy(name,"Fl");
  if (an == 115) strcpy(name,"Mc");
  if (an == 116) strcpy(name,"Lv");
  if (an == 117) strcpy(name,"Ts");
  if (an == 118) strcpy(name,"Og");
  if (an >= 119) strcpy(name,"--");
}
