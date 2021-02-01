/* Actions (message types) */
mtype = { nop, a28, a6, a22, a5, a16, a18, a25, a21, a1, a17, a32, a8, a11, a2, a10, a27, a12, a20, a23, a13, a29, a15, a7, a4, a30, a19, a24, a14, a31, a3, a9, a26 };

/* Inter-process channels */
chan p6_5 = [0] of {mtype};
chan p7_3 = [0] of {mtype};
chan p7_6 = [0] of {mtype};
chan p2_1 = [0] of {mtype};
chan p5_1 = [0] of {mtype};
chan p7_4 = [0] of {mtype};
chan p6_0 = [0] of {mtype};

/* Environement<->process channels */
chan p7 = [0] of {mtype};
chan p5 = [0] of {mtype};
chan p4 = [0] of {mtype};
chan p1 = [0] of {mtype};
chan p6 = [0] of {mtype};
chan p3 = [0] of {mtype};
chan p2 = [0] of {mtype};
chan p0 = [0] of {mtype};

/* Environment process */
active proctype Environment()
{
  do
  :: p7 ! a28
  :: p5 ! a6
  :: p5 ! a22
  :: p5 ! a5
  :: p5 ! a16
  :: p5 ! a18
  :: p5 ! a25
  :: p5 ! a1
  :: p5 ! a17
  :: p4 ! a32
  :: p1 ! a15
  :: p4 ! a30
  :: p5 ! a19
  :: p4 ! a31
  :: p5 ! a3
  :: p7 ! nop
  :: p6 ! nop
  :: p5 ! nop
  :: p4 ! nop
  :: p3 ! nop
  :: p2 ! nop
  :: p1 ! nop
  :: p0 ! nop
  od
}

/* Action channel */
chan act = [0] of {mtype};

/* Most recent message event */
mtype lastAction = nop;

/* Action listener */
active proctype Listener()
{
  atomic {
    do
    :: act ? lastAction ->
  step: skip
    od
  }
}

/* Process 0 */
active proctype Proc0()
{
  int state = 5;
  do
  :: state == 5 ->
    atomic {
      if
      :: p0 ? nop ->

        state = 4;
      fi
    }
  :: state == 4 ->
    atomic {
      if
      :: p6_0 ? a9 ->
        act ! a9;
        state = 3;
      fi
    }
  :: state == 3 ->
    atomic {
      if
      :: p6_0 ? a9 ->
        act ! a9;
        state = 2;
      :: p6_0 ? a14 ->
        act ! a14;
        state = 1;
      fi
    }
  :: state == 2 ->
    atomic {
      if
      :: p6_0 ? a9 ->
        act ! a9;
        state = 2;
      fi
    }
  :: state == 1 ->
    atomic {
      if
      :: p6_0 ? a9 ->
        act ! a9;
        state = 3;
      fi
    }
  od
}

/* Process 1 */
active proctype Proc1()
{
  int state = 16;
  do
  :: state == 16 ->
    atomic {
      if
      :: p1 ? nop ->

        state = 15;
      fi
    }
  :: state == 15 ->
    atomic {
      if
      :: p5_1 ? a24 ->
        act ! a24;
        state = 12;
      :: p5_1 ? a4 ->
        act ! a4;
        state = 11;
      fi
    }
  :: state == 12 ->
    atomic {
      if
      :: p1 ? a15 ->
        act ! a15;
        state = 13;
      fi
    }
  :: state == 11 ->
    atomic {
      if
      :: p1 ? a15 ->
        act ! a15;
        state = 10;
      fi
    }
  :: state == 13 ->
    atomic {
      if
      :: p2_1 ? a12 ->
        act ! a12;
        state = 9;
      fi
    }
  :: state == 10 ->
    atomic {
      if
      :: p2_1 ? a12 ->
        act ! a12;
        state = 8;
      fi
    }
  :: state == 9 ->
    atomic {
      if
      :: p2_1 ? a13 ->
        act ! a13;
        state = 7;
      fi
    }
  :: state == 8 ->
    atomic {
      if
      :: p2_1 ? a13 ->
        act ! a13;
        state = 6;
      fi
    }
  :: state == 7 ->
    atomic {
      if
      :: p2_1 ? a7 ->
        act ! a7;
        state = 14;
      fi
    }
  :: state == 6 ->
    atomic {
      if
      :: p2_1 ? a7 ->
        act ! a7;
        state = 15;
      fi
    }
  :: state == 14 ->
    atomic {
      if
      :: p5_1 ? a23 ->
        act ! a23;
        state = 11;
      fi
    }
  od
}

/* Process 2 */
active proctype Proc2()
{
  int state = 21;
  do
  :: state == 21 ->
    atomic {
      if
      :: p2 ? nop ->

        state = 20;
      fi
    }
  :: state == 20 ->
    atomic {
      if
      :: p2_1 ! a12 ->

        state = 18;
      fi
    }
  :: state == 18 ->
    atomic {
      if
      :: p2_1 ! a12 ->

        state = 19;
      :: p2_1 ! a13 ->

        state = 17;
      fi
    }
  :: state == 19 ->
    atomic {
      if
      :: p2_1 ! a12 ->

        state = 19;
      fi
    }
  :: state == 17 ->
    atomic {
      if
      :: p2_1 ! a13 ->

        state = 19;
      :: p2_1 ! a7 ->

        state = 20;
      fi
    }
  od
}

/* Process 3 */
active proctype Proc3()
{
  int state = 28;
  do
  :: state == 28 ->
    atomic {
      if
      :: p3 ? nop ->

        state = 25;
      fi
    }
  :: state == 25 ->
    atomic {
      if
      :: p7_3 ? a8 ->
        act ! a8;
        state = 24;
      :: p7_3 ? a27 ->
        act ! a27;
        state = 23;
      fi
    }
  :: state == 24 ->
    atomic {
      if
      :: p7_3 ? a27 ->
        act ! a27;
        state = 22;
      fi
    }
  :: state == 22 ->
    atomic {
      if
      :: p7_3 ? a8 ->
        act ! a8;
        state = 27;
      :: p7_3 ? a27 ->
        act ! a27;
        state = 23;
      fi
    }
  :: state == 27 ->
    atomic {
      if
      :: p7_3 ? a27 ->
        act ! a27;
        state = 26;
      :: p7_3 ? a8 ->
        act ! a8;
        state = 23;
      fi
    }
  :: state == 26 ->
    atomic {
      if
      :: p7_3 ? a8 ->
        act ! a8;
        state = 24;
      fi
    }
  od
}

/* Process 4 */
active proctype Proc4()
{
  int state = 41;
  do
  :: state == 41 ->
    atomic {
      if
      :: p4 ? nop ->

        state = 39;
      fi
    }
  :: state == 39 ->
    atomic {
      if
      :: p7_4 ? a26 ->
        act ! a26;
        state = 40;
      :: p7_4 ? a29 ->
        act ! a29;
        state = 32;
      fi
    }
  :: state == 40 ->
    atomic {
      if
      :: p4 ? a30 ->
        act ! a30;
        state = 31;
      fi
    }
  :: state == 32 ->
    atomic {
      if
      :: p4 ? a30 ->
        act ! a30;
        state = 38;
      fi
    }
  :: state == 31 ->
    atomic {
      if
      :: p4 ? a31 ->
        act ! a31;
        state = 37;
      fi
    }
  :: state == 38 ->
    atomic {
      if
      :: p4 ? a31 ->
        act ! a31;
        state = 36;
      fi
    }
  :: state == 37 ->
    atomic {
      if
      :: p4 ? a32 ->
        act ! a32;
        state = 35;
      fi
    }
  :: state == 36 ->
    atomic {
      if
      :: p4 ? a32 ->
        act ! a32;
        state = 30;
      fi
    }
  :: state == 35 ->
    atomic {
      if
      :: p7_4 ? a29 ->
        act ! a29;
        state = 29;
      fi
    }
  :: state == 30 ->
    atomic {
      if
      :: p7_4 ? a26 ->
        act ! a26;
        state = 32;
      fi
    }
  :: state == 29 ->
    atomic {
      if
      :: p4 ? a30 ->
        act ! a30;
        state = 34;
      fi
    }
  :: state == 34 ->
    atomic {
      if
      :: p4 ? a31 ->
        act ! a31;
        state = 33;
      fi
    }
  :: state == 33 ->
    atomic {
      if
      :: p4 ? a32 ->
        act ! a32;
        state = 39;
      fi
    }
  od
}

/* Process 5 */
active proctype Proc5()
{
  int state = 65;
  do
  :: state == 65 ->
    atomic {
      if
      :: p5 ? nop ->

        state = 47;
      fi
    }
  :: state == 47 ->
    atomic {
      if
      :: p5 ? a25 ->
        act ! a25;
        state = 45;
      :: p5 ? a1 ->
        act ! a1;
        state = 46;
      :: p5 ? a16 ->
        act ! a16;
        state = 44;
      fi
    }
  :: state == 45 ->
    atomic {
      if
      :: p5_1 ! a24 ->

        state = 49;
      :: p5 ? a6 ->
        act ! a6;
        state = 50;
      :: p5 ? a17 ->
        act ! a17;
        state = 47;
      fi
    }
  :: state == 46 ->
    atomic {
      if
      :: p5_1 ! a4 ->

        state = 51;
      fi
    }
  :: state == 44 ->
    atomic {
      if
      :: p5 ? a18 ->
        act ! a18;
        state = 48;
      fi
    }
  :: state == 49 ->
    atomic {
      if
      :: p5 ? a19 ->
        act ! a19;
        state = 42;
      :: p5_1 ! a24 ->

        state = 57;
      fi
    }
  :: state == 50 ->
    atomic {
      if
      :: p5 ? a5 ->
        act ! a5;
        state = 43;
      fi
    }
  :: state == 51 ->
    atomic {
      if
      :: p5 ? a3 ->
        act ! a3;
        state = 45;
      fi
    }
  :: state == 48 ->
    atomic {
      if
      :: p6_5 ? a21 ->
        act ! a21;
        state = 56;
      :: p5 ? a22 ->
        act ! a22;
        state = 47;
      fi
    }
  :: state == 42 ->
    atomic {
      if
      :: p5_1 ! a23 ->

        state = 48;
      :: p6_5 ? a20 ->
        act ! a20;
        state = 54;
      fi
    }
  :: state == 57 ->
    atomic {
      if
      :: p5 ? a18 ->
        act ! a18;
        state = 57;
      :: p6_5 ? a2 ->
        act ! a2;
        state = 57;
      :: p5 ? a22 ->
        act ! a22;
        state = 57;
      :: p5_1 ! a24 ->

        state = 57;
      :: p5 ? a19 ->
        act ! a19;
        state = 57;
      :: p5 ? a25 ->
        act ! a25;
        state = 57;
      :: p5 ? a5 ->
        act ! a5;
        state = 57;
      fi
    }
  :: state == 43 ->
    atomic {
      if
      :: p6_5 ? a2 ->
        act ! a2;
        state = 55;
      fi
    }
  :: state == 56 ->
    atomic {
      if
      :: p6_5 ? a20 ->
        act ! a20;
        state = 45;
      fi
    }
  :: state == 54 ->
    atomic {
      if
      :: p6_5 ? a20 ->
        act ! a20;
        state = 54;
      :: p6_5 ? a21 ->
        act ! a21;
        state = 54;
      :: p6_5 ? a2 ->
        act ! a2;
        state = 54;
      :: p5 ? a22 ->
        act ! a22;
        state = 54;
      :: p5 ? a3 ->
        act ! a3;
        state = 61;
      fi
    }
  :: state == 55 ->
    atomic {
      if
      :: p5 ? a1 ->
        act ! a1;
        state = 46;
      :: p5 ? a16 ->
        act ! a16;
        state = 53;
      :: p5 ? a25 ->
        act ! a25;
        state = 58;
      fi
    }
  :: state == 61 ->
    atomic {
      if
      :: p6_5 ? a2 ->
        act ! a2;
        state = 61;
      :: p5_1 ! a24 ->

        state = 61;
      :: p5 ? a22 ->
        act ! a22;
        state = 61;
      :: p6_5 ? a20 ->
        act ! a20;
        state = 61;
      :: p5 ? a16 ->
        act ! a16;
        state = 61;
      :: p5 ? a19 ->
        act ! a19;
        state = 61;
      :: p5 ? a5 ->
        act ! a5;
        state = 61;
      :: p5_1 ! a23 ->

        state = 61;
      :: p5 ? a1 ->
        act ! a1;
        state = 61;
      :: p5 ? a18 ->
        act ! a18;
        state = 61;
      :: p6_5 ? a21 ->
        act ! a21;
        state = 61;
      fi
    }
  :: state == 53 ->
    atomic {
      if
      :: p5 ? a18 ->
        act ! a18;
        state = 60;
      fi
    }
  :: state == 58 ->
    atomic {
      if
      :: p5 ? a17 ->
        act ! a17;
        state = 55;
      :: p5 ? a6 ->
        act ! a6;
        state = 50;
      :: p5_1 ! a24 ->

        state = 59;
      fi
    }
  :: state == 60 ->
    atomic {
      if
      :: p5 ? a22 ->
        act ! a22;
        state = 55;
      :: p6_5 ? a21 ->
        act ! a21;
        state = 64;
      fi
    }
  :: state == 59 ->
    atomic {
      if
      :: p5 ? a19 ->
        act ! a19;
        state = 63;
      fi
    }
  :: state == 64 ->
    atomic {
      if
      :: p6_5 ? a20 ->
        act ! a20;
        state = 58;
      fi
    }
  :: state == 63 ->
    atomic {
      if
      :: p5_1 ! a23 ->

        state = 60;
      :: p6_5 ? a20 ->
        act ! a20;
        state = 52;
      fi
    }
  :: state == 52 ->
    atomic {
      if
      :: p5 ? a5 ->
        act ! a5;
        state = 52;
      :: p5 ? a22 ->
        act ! a22;
        state = 52;
      :: p5 ? a1 ->
        act ! a1;
        state = 62;
      :: p5 ? a19 ->
        act ! a19;
        state = 52;
      :: p5 ? a25 ->
        act ! a25;
        state = 52;
      :: p5_1 ! a23 ->

        state = 52;
      :: p5 ? a18 ->
        act ! a18;
        state = 52;
      :: p5_1 ! a4 ->

        state = 52;
      fi
    }
  :: state == 62 ->
    atomic {
      if
      :: p5 ? a1 ->
        act ! a1;
        state = 61;
      :: p6_5 ? a21 ->
        act ! a21;
        state = 62;
      :: p6_5 ? a20 ->
        act ! a20;
        state = 62;
      :: p5 ? a16 ->
        act ! a16;
        state = 62;
      fi
    }
  od
}

/* Process 6 */
active proctype Proc6()
{
  int state = 83;
  do
  :: state == 83 ->
    atomic {
      if
      :: p6 ? nop ->

        state = 68;
      fi
    }
  :: state == 68 ->
    atomic {
      if
      :: p6_5 ! a21 ->

        state = 74;
      :: p6_5 ! a2 ->

        state = 80;
      fi
    }
  :: state == 74 ->
    atomic {
      if
      :: p7_6 ? a11 ->
        act ! a11;
        state = 67;
      fi
    }
  :: state == 80 ->
    atomic {
      if
      :: p7_6 ? a11 ->
        act ! a11;
        state = 78;
      fi
    }
  :: state == 67 ->
    atomic {
      if
      :: p6_0 ! a9 ->

        state = 77;
      fi
    }
  :: state == 78 ->
    atomic {
      if
      :: p6_0 ! a9 ->

        state = 66;
      fi
    }
  :: state == 77 ->
    atomic {
      if
      :: p7_6 ? a10 ->
        act ! a10;
        state = 82;
      fi
    }
  :: state == 66 ->
    atomic {
      if
      :: p7_6 ? a10 ->
        act ! a10;
        state = 81;
      fi
    }
  :: state == 82 ->
    atomic {
      if
      :: p6_0 ! a14 ->

        state = 72;
      :: p7_6 ? a10 ->
        act ! a10;
        state = 73;
      fi
    }
  :: state == 81 ->
    atomic {
      if
      :: p6_0 ! a14 ->

        state = 68;
      fi
    }
  :: state == 72 ->
    atomic {
      if
      :: p6_5 ! a21 ->

        state = 69;
      :: p6_5 ! a20 ->

        state = 80;
      fi
    }
  :: state == 73 ->
    atomic {
      if
      :: p6_5 ! a2 ->

        state = 73;
      :: p7_6 ? a10 ->
        act ! a10;
        state = 73;
      :: p6_5 ! a21 ->

        state = 73;
      fi
    }
  :: state == 69 ->
    atomic {
      if
      :: p7_6 ? a11 ->
        act ! a11;
        state = 71;
      fi
    }
  :: state == 71 ->
    atomic {
      if
      :: p6_0 ! a9 ->

        state = 79;
      :: p6_0 ! a14 ->

        state = 76;
      fi
    }
  :: state == 79 ->
    atomic {
      if
      :: p7_6 ? a10 ->
        act ! a10;
        state = 75;
      fi
    }
  :: state == 76 ->
    atomic {
      if
      :: p7_6 ? a10 ->
        act ! a10;
        state = 76;
      :: p6_0 ! a14 ->

        state = 76;
      :: p6_5 ! a21 ->

        state = 76;
      :: p6_0 ! a9 ->

        state = 76;
      :: p7_6 ? a11 ->
        act ! a11;
        state = 76;
      fi
    }
  :: state == 75 ->
    atomic {
      if
      :: p6_0 ! a14 ->

        state = 70;
      fi
    }
  :: state == 70 ->
    atomic {
      if
      :: p6_5 ! a20 ->

        state = 69;
      :: p6_5 ! a21 ->

        state = 69;
      :: p6_5 ! a2 ->

        state = 69;
      fi
    }
  od
}

/* Process 7 */
active proctype Proc7()
{
  int state = 103;
  do
  :: state == 103 ->
    atomic {
      if
      :: p7 ? nop ->

        state = 93;
      fi
    }
  :: state == 93 ->
    atomic {
      if
      :: p7_6 ! a10 ->

        state = 100;
      :: p7_6 ! a11 ->

        state = 90;
      fi
    }
  :: state == 100 ->
    atomic {
      if
      :: p7_3 ! a8 ->

        state = 91;
      fi
    }
  :: state == 90 ->
    atomic {
      if
      :: p7_3 ! a8 ->

        state = 95;
      fi
    }
  :: state == 91 ->
    atomic {
      if
      :: p7_4 ! a26 ->

        state = 85;
      fi
    }
  :: state == 95 ->
    atomic {
      if
      :: p7_4 ! a26 ->

        state = 88;
      fi
    }
  :: state == 85 ->
    atomic {
      if
      :: p7_3 ! a27 ->

        state = 102;
      fi
    }
  :: state == 88 ->
    atomic {
      if
      :: p7_3 ! a27 ->

        state = 84;
      fi
    }
  :: state == 102 ->
    atomic {
      if
      :: p7 ? a28 ->
        act ! a28;
        state = 87;
      fi
    }
  :: state == 84 ->
    atomic {
      if
      :: p7 ? a28 ->
        act ! a28;
        state = 99;
      fi
    }
  :: state == 87 ->
    atomic {
      if
      :: p7_4 ! a29 ->

        state = 101;
      fi
    }
  :: state == 99 ->
    atomic {
      if
      :: p7_4 ! a29 ->

        state = 96;
      fi
    }
  :: state == 96 ->
    atomic {
      if
      :: p7_6 ! a11 ->

        state = 100;
      :: p7_6 ! a10 ->

        state = 86;
      fi
    }
  :: state == 86 ->
    atomic {
      if
      :: p7_3 ! a8 ->

        state = 92;
      fi
    }
  :: state == 92 ->
    atomic {
      if
      :: p7_4 ! a26 ->

        state = 97;
      fi
    }
  :: state == 97 ->
    atomic {
      if
      :: p7_3 ! a27 ->

        state = 98;
      fi
    }
  :: state == 98 ->
    atomic {
      if
      :: p7 ? a28 ->
        act ! a28;
        state = 94;
      fi
    }
  :: state == 94 ->
    atomic {
      if
      :: p7_4 ! a29 ->

        state = 89;
      fi
    }
  :: state == 89 ->
    atomic {
      if
      :: p7_6 ! a11 ->

        state = 90;
      fi
    }
  od
}

#define pa28	 (lastAction == a28)
#define pa6	 (lastAction == a6)
#define pa22	 (lastAction == a22)
#define pa5	 (lastAction == a5)
#define pa16	 (lastAction == a16)
#define pa18	 (lastAction == a18)
#define pa25	 (lastAction == a25)
#define pa21	 (lastAction == a21)
#define pa1	 (lastAction == a1)
#define pa17	 (lastAction == a17)
#define pa32	 (lastAction == a32)
#define pa8	 (lastAction == a8)
#define pa11	 (lastAction == a11)
#define pa2	 (lastAction == a2)
#define pa10	 (lastAction == a10)
#define pa27	 (lastAction == a27)
#define pa12	 (lastAction == a12)
#define pa20	 (lastAction == a20)
#define pa23	 (lastAction == a23)
#define pa13	 (lastAction == a13)
#define pa29	 (lastAction == a29)
#define pa15	 (lastAction == a15)
#define pa7	 (lastAction == a7)
#define pa4	 (lastAction == a4)
#define pa30	 (lastAction == a30)
#define pa19	 (lastAction == a19)
#define pa24	 (lastAction == a24)
#define pa14	 (lastAction == a14)
#define pa31	 (lastAction == a31)
#define pa3	 (lastAction == a3)
#define pa9	 (lastAction == a9)
#define pa26	 (lastAction == a26)
