module mod_periodictable

  use iso_fortran_env

  implicit none
  save
  public

  integer, private, parameter :: dbl = REAL64

  type :: pt_entry_t
    character(2) :: e
    character(2) :: ef
    integer :: z
    real(dbl) :: m
  end type

  type(pt_entry_t), dimension(118), parameter :: ptable = (/ &
    pt_entry_t(e="H" ,ef="H ",z=  1,m=  1.0080_dbl),&
    pt_entry_t(e="He",ef="He",z=  2,m=  4.0026_dbl),&
    pt_entry_t(e="Li",ef="Li",z=  3,m=  6.9400_dbl),&
    pt_entry_t(e="Be",ef="Be",z=  4,m=  9.0122_dbl),&
    pt_entry_t(e="B" ,ef="B ",z=  5,m= 10.8100_dbl),&
    pt_entry_t(e="C" ,ef="C ",z=  6,m= 12.0110_dbl),&
    pt_entry_t(e="N" ,ef="N ",z=  7,m= 14.0070_dbl),&
    pt_entry_t(e="O" ,ef="O ",z=  8,m= 15.9990_dbl),&
    pt_entry_t(e="F" ,ef="F ",z=  9,m= 18.9980_dbl),&
    pt_entry_t(e="Ne",ef="Ne",z= 10,m= 20.1800_dbl),&
    pt_entry_t(e="Na",ef="Na",z= 11,m= 22.9900_dbl),&
    pt_entry_t(e="Mg",ef="Mg",z= 12,m= 24.3050_dbl),&
    pt_entry_t(e="Al",ef="Al",z= 13,m= 26.9820_dbl),&
    pt_entry_t(e="Si",ef="Si",z= 14,m= 28.0850_dbl),&
    pt_entry_t(e="P" ,ef="P ",z= 15,m= 30.9740_dbl),&
    pt_entry_t(e="S" ,ef="S ",z= 16,m= 32.0600_dbl),&
    pt_entry_t(e="Cl",ef="Cl",z= 17,m= 35.4500_dbl),&
    pt_entry_t(e="Ar",ef="Ar",z= 18,m= 39.9480_dbl),&
    pt_entry_t(e="K" ,ef="K ",z= 19,m= 39.0980_dbl),&
    pt_entry_t(e="Ca",ef="Ca",z= 20,m= 40.0780_dbl),&
    pt_entry_t(e="Sc",ef="Sc",z= 21,m= 44.9560_dbl),&
    pt_entry_t(e="Ti",ef="Ti",z= 22,m= 47.8670_dbl),&
    pt_entry_t(e="V" ,ef="V ",z= 23,m= 50.9420_dbl),&
    pt_entry_t(e="Cr",ef="Cr",z= 24,m= 51.9960_dbl),&
    pt_entry_t(e="Mn",ef="Mn",z= 25,m= 54.9380_dbl),&
    pt_entry_t(e="Fe",ef="Fe",z= 26,m= 55.8450_dbl),&
    pt_entry_t(e="Co",ef="Co",z= 27,m= 58.9330_dbl),&
    pt_entry_t(e="Ni",ef="Ni",z= 28,m= 58.6930_dbl),&
    pt_entry_t(e="Cu",ef="Cu",z= 29,m= 63.5460_dbl),&
    pt_entry_t(e="Zn",ef="Zn",z= 30,m= 65.3800_dbl),&
    pt_entry_t(e="Ga",ef="Ga",z= 31,m= 69.7230_dbl),&
    pt_entry_t(e="Ge",ef="Ge",z= 32,m= 72.6300_dbl),&
    pt_entry_t(e="As",ef="As",z= 33,m= 74.9220_dbl),&
    pt_entry_t(e="Se",ef="Se",z= 34,m= 78.9710_dbl),&
    pt_entry_t(e="Br",ef="Br",z= 35,m= 79.9040_dbl),&
    pt_entry_t(e="Kr",ef="Kr",z= 36,m= 83.7980_dbl),&
    pt_entry_t(e="Rb",ef="Rb",z= 37,m= 85.4680_dbl),&
    pt_entry_t(e="Sr",ef="Sr",z= 38,m= 87.6200_dbl),&
    pt_entry_t(e="Y" ,ef="Y ",z= 39,m= 88.9060_dbl),&
    pt_entry_t(e="Zr",ef="Zr",z= 40,m= 91.2240_dbl),&
    pt_entry_t(e="Nb",ef="Nb",z= 41,m= 92.9060_dbl),&
    pt_entry_t(e="Mo",ef="Mo",z= 42,m= 95.9500_dbl),&
    pt_entry_t(e="Tc",ef="Tc",z= 43,m= 97.0000_dbl),&
    pt_entry_t(e="Ru",ef="Ru",z= 44,m=101.0700_dbl),&
    pt_entry_t(e="Rh",ef="Rh",z= 45,m=102.9100_dbl),&
    pt_entry_t(e="Pd",ef="Pd",z= 46,m=106.4200_dbl),&
    pt_entry_t(e="Ag",ef="Ag",z= 47,m=107.8700_dbl),&
    pt_entry_t(e="Cd",ef="Cd",z= 48,m=112.4100_dbl),&
    pt_entry_t(e="In",ef="In",z= 49,m=114.8200_dbl),&
    pt_entry_t(e="Sn",ef="Sn",z= 50,m=118.7100_dbl),&
    pt_entry_t(e="Sb",ef="Sb",z= 51,m=121.7600_dbl),&
    pt_entry_t(e="Te",ef="Te",z= 52,m=127.6000_dbl),&
    pt_entry_t(e="I" ,ef="I ",z= 53,m=126.9000_dbl),&
    pt_entry_t(e="Xe",ef="Xe",z= 54,m=131.2900_dbl),&
    pt_entry_t(e="Cs",ef="Cs",z= 55,m=132.9100_dbl),&
    pt_entry_t(e="Ba",ef="Ba",z= 56,m=137.3300_dbl),&
    pt_entry_t(e="La",ef="La",z= 57,m=138.9100_dbl),&
    pt_entry_t(e="Ce",ef="Ce",z= 58,m=140.1200_dbl),&
    pt_entry_t(e="Pr",ef="Pr",z= 59,m=140.9100_dbl),&
    pt_entry_t(e="Nd",ef="Nd",z= 60,m=144.2400_dbl),&
    pt_entry_t(e="Pm",ef="Pm",z= 61,m=145.0000_dbl),&
    pt_entry_t(e="Sm",ef="Sm",z= 62,m=150.3600_dbl),&
    pt_entry_t(e="Eu",ef="Eu",z= 63,m=151.9600_dbl),&
    pt_entry_t(e="Gd",ef="Gd",z= 64,m=157.2500_dbl),&
    pt_entry_t(e="Tb",ef="Tb",z= 65,m=158.9300_dbl),&
    pt_entry_t(e="Dy",ef="Dy",z= 66,m=162.5000_dbl),&
    pt_entry_t(e="Ho",ef="Ho",z= 67,m=164.9300_dbl),&
    pt_entry_t(e="Er",ef="Er",z= 68,m=167.2600_dbl),&
    pt_entry_t(e="Tm",ef="Tm",z= 69,m=168.9300_dbl),&
    pt_entry_t(e="Yb",ef="Yb",z= 70,m=173.0500_dbl),&
    pt_entry_t(e="Lu",ef="Lu",z= 71,m=174.9700_dbl),&
    pt_entry_t(e="Hf",ef="Hf",z= 72,m=178.4900_dbl),&
    pt_entry_t(e="Ta",ef="Ta",z= 73,m=180.9500_dbl),&
    pt_entry_t(e="W" ,ef="W ",z= 74,m=183.8400_dbl),&
    pt_entry_t(e="Re",ef="Re",z= 75,m=186.2100_dbl),&
    pt_entry_t(e="Os",ef="Os",z= 76,m=190.2300_dbl),&
    pt_entry_t(e="Ir",ef="Ir",z= 77,m=192.2200_dbl),&
    pt_entry_t(e="Pt",ef="Pt",z= 78,m=195.0800_dbl),&
    pt_entry_t(e="Au",ef="Au",z= 79,m=196.9700_dbl),&
    pt_entry_t(e="Hg",ef="Hg",z= 80,m=200.5900_dbl),&
    pt_entry_t(e="Tl",ef="Tl",z= 81,m=204.3800_dbl),&
    pt_entry_t(e="Pb",ef="Pb",z= 82,m=207.2000_dbl),&
    pt_entry_t(e="Bi",ef="Bi",z= 83,m=208.9800_dbl),&
    pt_entry_t(e="Po",ef="Po",z= 84,m=209.0000_dbl),&
    pt_entry_t(e="At",ef="At",z= 85,m=210.0000_dbl),&
    pt_entry_t(e="Rn",ef="Rn",z= 86,m=222.0000_dbl),&
    pt_entry_t(e="Fr",ef="Fr",z= 87,m=223.0000_dbl),&
    pt_entry_t(e="Ra",ef="Ra",z= 88,m=226.0000_dbl),&
    pt_entry_t(e="Ac",ef="Ac",z= 89,m=227.0000_dbl),&
    pt_entry_t(e="Th",ef="Th",z= 90,m=232.0400_dbl),&
    pt_entry_t(e="Pa",ef="Pa",z= 91,m=231.0400_dbl),&
    pt_entry_t(e="U" ,ef="U ",z= 92,m=238.0300_dbl),&
    pt_entry_t(e="Np",ef="Np",z= 93,m=237.0000_dbl),&
    pt_entry_t(e="Pu",ef="Pu",z= 94,m=244.0000_dbl),&
    pt_entry_t(e="Am",ef="Am",z= 95,m=243.0000_dbl),&
    pt_entry_t(e="Cm",ef="Cm",z= 96,m=247.0000_dbl),&
    pt_entry_t(e="Bk",ef="Bk",z= 97,m=247.0000_dbl),&
    pt_entry_t(e="Cf",ef="Cf",z= 98,m=251.0000_dbl),&
    pt_entry_t(e="Es",ef="Es",z= 99,m=252.0000_dbl),&
    pt_entry_t(e="Fm",ef="Fm",z=100,m=257.0000_dbl),&
    pt_entry_t(e="Md",ef="Md",z=101,m=258.0000_dbl),&
    pt_entry_t(e="No",ef="No",z=102,m=259.0000_dbl),&
    pt_entry_t(e="Lr",ef="Lr",z=103,m=266.0000_dbl),&
    pt_entry_t(e="Rf",ef="Rf",z=104,m=267.0000_dbl),&
    pt_entry_t(e="Db",ef="Db",z=105,m=268.0000_dbl),&
    pt_entry_t(e="Sg",ef="Sg",z=106,m=269.0000_dbl),&
    pt_entry_t(e="Bh",ef="Bh",z=107,m=270.0000_dbl),&
    pt_entry_t(e="Hs",ef="Hs",z=108,m=269.0000_dbl),&
    pt_entry_t(e="Mt",ef="Mt",z=109,m=278.0000_dbl),&
    pt_entry_t(e="Ds",ef="Ds",z=110,m=281.0000_dbl),&
    pt_entry_t(e="Rg",ef="Rg",z=111,m=282.0000_dbl),&
    pt_entry_t(e="Cn",ef="Cn",z=112,m=285.0000_dbl),&
    pt_entry_t(e="Nh",ef="Nh",z=113,m=286.0000_dbl),&
    pt_entry_t(e="Fl",ef="Fl",z=114,m=289.0000_dbl),&
    pt_entry_t(e="Mc",ef="Mc",z=115,m=289.0000_dbl),&
    pt_entry_t(e="Lv",ef="Lv",z=116,m=293.0000_dbl),&
    pt_entry_t(e="Ts",ef="Ts",z=117,m=294.0000_dbl),&
    pt_entry_t(e="Og",ef="Og",z=118,m=294.0000_dbl) &
  /)

end module mod_periodictable
