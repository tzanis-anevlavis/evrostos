/* Actions (message types) */
mtype = { nop, c5_t17, c5_t16, c5_t15, c5_t13, c5_t12, c5_t8, c5_t7, c5_t6, c5_t4, c5_t10, c5_t0, c4_t6, c4_t2, c5_t18, c4_t1, c4_t0, c3_t12__c4_t7, c4_t5__c3_t3, c3_t14, c3_t13, c3_t11, c3_t8, c3_t5, c3_t4, c3_t2, c5_t11, c2_t3__c5_t19, c2_t7__c3_t10, c2_t5, c4_t3, c5_t5, c1_t6__c3_t9, c1_t7, c3_t6__c2_t2, c1_t5, c1_t8, c1_t4, c1_t2, c1_t0, c4_t4__c0_t13, c2_t0__c0_t1, c0_t10, c0_t8, c0_t7, c2_t9, c0_t6, c5_t14, c2_t1, c0_t15, c0_t11, c0_t9, c3_t1, c0_t5, c0_t4, c1_t3__c2_t8, c1_t1__c3_t0, c0_t3, c0_t12__c4_t9, c5_t3, c2_t6__c1_t9, c0_t14, c5_t2, c5_t9, c0_t2, c4_t8, c3_t7, c5_t1, c2_t4, c0_t0 };

/* Inter-process channels */
chan p4_3 = [0] of {mtype};
chan p5_2 = [0] of {mtype};
chan p3_2 = [0] of {mtype};
chan p3_1 = [0] of {mtype};
chan p4_0 = [0] of {mtype};
chan p2_0 = [0] of {mtype};
chan p2_1 = [0] of {mtype};

/* Environement<->process channels */
chan p5 = [0] of {mtype};
chan p4 = [0] of {mtype};
chan p3 = [0] of {mtype};
chan p2 = [0] of {mtype};
chan p1 = [0] of {mtype};
chan p0 = [0] of {mtype};

/* Environment process */
active proctype Environment()
{
  do
  :: p5 ! c5_t17
  :: p5 ! c5_t16
  :: p5 ! c5_t15
  :: p5 ! c5_t13
  :: p5 ! c5_t12
  :: p5 ! c5_t8
  :: p5 ! c5_t7
  :: p5 ! c5_t6
  :: p5 ! c5_t4
  :: p5 ! c5_t10
  :: p5 ! c5_t0
  :: p4 ! c4_t6
  :: p4 ! c4_t2
  :: p5 ! c5_t18
  :: p4 ! c4_t1
  :: p4 ! c4_t0
  :: p3 ! c3_t14
  :: p3 ! c3_t13
  :: p3 ! c3_t11
  :: p3 ! c3_t8
  :: p3 ! c3_t5
  :: p3 ! c3_t4
  :: p3 ! c3_t2
  :: p5 ! c5_t11
  :: p2 ! c2_t5
  :: p4 ! c4_t3
  :: p5 ! c5_t5
  :: p1 ! c1_t7
  :: p1 ! c1_t5
  :: p1 ! c1_t8
  :: p1 ! c1_t4
  :: p1 ! c1_t2
  :: p1 ! c1_t0
  :: p0 ! c0_t10
  :: p0 ! c0_t8
  :: p0 ! c0_t7
  :: p2 ! c2_t9
  :: p0 ! c0_t6
  :: p5 ! c5_t14
  :: p2 ! c2_t1
  :: p0 ! c0_t15
  :: p0 ! c0_t11
  :: p0 ! c0_t9
  :: p3 ! c3_t1
  :: p0 ! c0_t5
  :: p0 ! c0_t4
  :: p0 ! c0_t3
  :: p5 ! c5_t3
  :: p0 ! c0_t14
  :: p5 ! nop
  :: p4 ! nop
  :: p3 ! nop
  :: p2 ! nop
  :: p1 ! nop
  :: p0 ! nop
  :: p5 ! c5_t2
  :: p5 ! c5_t9
  :: p0 ! c0_t2
  :: p4 ! c4_t8
  :: p3 ! c3_t7
  :: p5 ! c5_t1
  :: p2 ! c2_t4
  :: p0 ! c0_t0
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
  int state = 0;
  do
  :: state == 0 ->
    atomic {
      if
      :: p0 ? nop ->

        state = 4;
      fi
    }
  :: state == 4 ->
    atomic {
      if
      :: p0 ? c0_t4 ->
        act ! c0_t4;
        state = 5;
      fi
    }
  :: state == 5 ->
    atomic {
      if
      :: p0 ? c0_t5 ->
        act ! c0_t5;
        state = 6;
      fi
    }
  :: state == 6 ->
    atomic {
      if
      :: p0 ? c0_t6 ->
        act ! c0_t6;
        state = 1;
      :: p4_0 ? c0_t12__c4_t9 ->
        act ! c0_t12__c4_t9;
        state = 11;
      fi
    }
  :: state == 1 ->
    atomic {
      if
      :: p0 ? c0_t0 ->
        act ! c0_t0;
        state = 2;
      :: p0 ? c0_t3 ->
        act ! c0_t3;
        state = 4;
      fi
    }
  :: state == 11 ->
    atomic {
      if
      :: p4_0 ? c4_t4__c0_t13 ->
        act ! c4_t4__c0_t13;
        state = 12;
      fi
    }
  :: state == 2 ->
    atomic {
      if
      :: p2_0 ? c2_t0__c0_t1 ->
        act ! c2_t0__c0_t1;
        state = 3;
      fi
    }
  :: state == 12 ->
    atomic {
      if
      :: p0 ? c0_t14 ->
        act ! c0_t14;
        state = 13;
      fi
    }
  :: state == 3 ->
    atomic {
      if
      :: p0 ? c0_t2 ->
        act ! c0_t2;
        state = 1;
      :: p0 ? c0_t7 ->
        act ! c0_t7;
        state = 7;
      fi
    }
  :: state == 13 ->
    atomic {
      if
      :: p0 ? c0_t15 ->
        act ! c0_t15;
        state = 9;
      fi
    }
  :: state == 7 ->
    atomic {
      if
      :: p0 ? c0_t8 ->
        act ! c0_t8;
        state = 8;
      fi
    }
  :: state == 9 ->
    atomic {
      if
      :: p0 ? c0_t10 ->
        act ! c0_t10;
        state = 10;
      fi
    }
  :: state == 8 ->
    atomic {
      if
      :: p0 ? c0_t9 ->
        act ! c0_t9;
        state = 3;
      fi
    }
  :: state == 10 ->
    atomic {
      if
      :: p0 ? c0_t11 ->
        act ! c0_t11;
        state = 6;
      fi
    }
  od
}

/* Process 1 */
active proctype Proc1()
{
  int state = 14;
  do
  :: state == 14 ->
    atomic {
      if
      :: p1 ? nop ->

        state = 16;
      fi
    }
  :: state == 16 ->
    atomic {
      if
      :: p1 ? c1_t8 ->
        act ! c1_t8;
        state = 23;
      :: p3_1 ? c1_t1__c3_t0 ->
        act ! c1_t1__c3_t0;
        state = 17;
      fi
    }
  :: state == 23 ->
    atomic {
      if
      :: p2_1 ? c2_t6__c1_t9 ->
        act ! c2_t6__c1_t9;
        state = 21;
      fi
    }
  :: state == 17 ->
    atomic {
      if
      :: p1 ? c1_t2 ->
        act ! c1_t2;
        state = 18;
      fi
    }
  :: state == 21 ->
    atomic {
      if
      :: p3_1 ? c1_t6__c3_t9 ->
        act ! c1_t6__c3_t9;
        state = 22;
      fi
    }
  :: state == 18 ->
    atomic {
      if
      :: p2_1 ? c1_t3__c2_t8 ->
        act ! c1_t3__c2_t8;
        state = 19;
      fi
    }
  :: state == 22 ->
    atomic {
      if
      :: p1 ? c1_t7 ->
        act ! c1_t7;
        state = 16;
      fi
    }
  :: state == 19 ->
    atomic {
      if
      :: p1 ? c1_t4 ->
        act ! c1_t4;
        state = 20;
      fi
    }
  :: state == 20 ->
    atomic {
      if
      :: p1 ? c1_t5 ->
        act ! c1_t5;
        state = 15;
      fi
    }
  :: state == 15 ->
    atomic {
      if
      :: p1 ? c1_t0 ->
        act ! c1_t0;
        state = 16;
      fi
    }
  od
}

/* Process 2 */
active proctype Proc2()
{
  int state = 24;
  do
  :: state == 24 ->
    atomic {
      if
      :: p2 ? nop ->

        state = 33;
      fi
    }
  :: state == 33 ->
    atomic {
      if
      :: p2 ? c2_t9 ->
        act ! c2_t9;
        state = 29;
      fi
    }
  :: state == 29 ->
    atomic {
      if
      :: p2 ? c2_t4 ->
        act ! c2_t4;
        state = 27;
      fi
    }
  :: state == 27 ->
    atomic {
      if
      :: p2 ? c2_t5 ->
        act ! c2_t5;
        state = 30;
      :: p3_2 ? c3_t6__c2_t2 ->
        act ! c3_t6__c2_t2;
        state = 28;
      fi
    }
  :: state == 30 ->
    atomic {
      if
      :: p2_1 ! c2_t6__c1_t9 ->

        state = 31;
      fi
    }
  :: state == 28 ->
    atomic {
      if
      :: p5_2 ? c2_t3__c5_t19 ->
        act ! c2_t3__c5_t19;
        state = 25;
      fi
    }
  :: state == 31 ->
    atomic {
      if
      :: p3_2 ? c2_t7__c3_t10 ->
        act ! c2_t7__c3_t10;
        state = 32;
      fi
    }
  :: state == 25 ->
    atomic {
      if
      :: p2_0 ! c2_t0__c0_t1 ->

        state = 26;
      fi
    }
  :: state == 32 ->
    atomic {
      if
      :: p2_1 ! c1_t3__c2_t8 ->

        state = 33;
      fi
    }
  :: state == 26 ->
    atomic {
      if
      :: p2 ? c2_t1 ->
        act ! c2_t1;
        state = 27;
      fi
    }
  od
}

/* Process 3 */
active proctype Proc3()
{
  int state = 34;
  do
  :: state == 34 ->
    atomic {
      if
      :: p3 ? nop ->

        state = 46;
      fi
    }
  :: state == 46 ->
    atomic {
      if
      :: p3 ? c3_t14 ->
        act ! c3_t14;
        state = 42;
      fi
    }
  :: state == 42 ->
    atomic {
      if
      :: p3 ? c3_t8 ->
        act ! c3_t8;
        state = 39;
      :: p3_1 ! c1_t6__c3_t9 ->

        state = 43;
      :: p4_3 ? c3_t12__c4_t7 ->
        act ! c3_t12__c4_t7;
        state = 45;
      fi
    }
  :: state == 39 ->
    atomic {
      if
      :: p3 ? c3_t4 ->
        act ! c3_t4;
        state = 40;
      fi
    }
  :: state == 43 ->
    atomic {
      if
      :: p3_2 ! c2_t7__c3_t10 ->

        state = 44;
      fi
    }
  :: state == 45 ->
    atomic {
      if
      :: p3 ? c3_t13 ->
        act ! c3_t13;
        state = 46;
      fi
    }
  :: state == 40 ->
    atomic {
      if
      :: p3 ? c3_t5 ->
        act ! c3_t5;
        state = 41;
      fi
    }
  :: state == 44 ->
    atomic {
      if
      :: p3 ? c3_t11 ->
        act ! c3_t11;
        state = 42;
      fi
    }
  :: state == 41 ->
    atomic {
      if
      :: p3_2 ! c3_t6__c2_t2 ->

        state = 37;
      fi
    }
  :: state == 37 ->
    atomic {
      if
      :: p3 ? c3_t2 ->
        act ! c3_t2;
        state = 38;
      :: p3 ? c3_t7 ->
        act ! c3_t7;
        state = 42;
      fi
    }
  :: state == 38 ->
    atomic {
      if
      :: p4_3 ? c4_t5__c3_t3 ->
        act ! c4_t5__c3_t3;
        state = 35;
      fi
    }
  :: state == 35 ->
    atomic {
      if
      :: p3_1 ! c1_t1__c3_t0 ->

        state = 36;
      fi
    }
  :: state == 36 ->
    atomic {
      if
      :: p3 ? c3_t1 ->
        act ! c3_t1;
        state = 37;
      fi
    }
  od
}

/* Process 4 */
active proctype Proc4()
{
  int state = 47;
  do
  :: state == 47 ->
    atomic {
      if
      :: p4 ? nop ->

        state = 48;
      fi
    }
  :: state == 48 ->
    atomic {
      if
      :: p4 ? c4_t0 ->
        act ! c4_t0;
        state = 49;
      :: p4 ? c4_t6 ->
        act ! c4_t6;
        state = 53;
      :: p4_0 ! c0_t12__c4_t9 ->

        state = 48;
      fi
    }
  :: state == 49 ->
    atomic {
      if
      :: p4 ? c4_t1 ->
        act ! c4_t1;
        state = 50;
      fi
    }
  :: state == 53 ->
    atomic {
      if
      :: p4_3 ! c3_t12__c4_t7 ->

        state = 54;
      fi
    }
  :: state == 50 ->
    atomic {
      if
      :: p4 ? c4_t2 ->
        act ! c4_t2;
        state = 48;
      :: p4_3 ! c4_t5__c3_t3 ->

        state = 51;
      fi
    }
  :: state == 54 ->
    atomic {
      if
      :: p4 ? c4_t8 ->
        act ! c4_t8;
        state = 48;
      fi
    }
  :: state == 51 ->
    atomic {
      if
      :: p4 ? c4_t3 ->
        act ! c4_t3;
        state = 52;
      fi
    }
  :: state == 52 ->
    atomic {
      if
      :: p4_0 ! c4_t4__c0_t13 ->

        state = 50;
      fi
    }
  od
}

/* Process 5 */
active proctype Proc5()
{
  int state = 55;
  do
  :: state == 55 ->
    atomic {
      if
      :: p5 ? nop ->

        state = 71;
      fi
    }
  :: state == 71 ->
    atomic {
      if
      :: p5 ? c5_t17 ->
        act ! c5_t17;
        state = 66;
      fi
    }
  :: state == 66 ->
    atomic {
      if
      :: p5 ? c5_t11 ->
        act ! c5_t11;
        state = 62;
      :: p5 ? c5_t18 ->
        act ! c5_t18;
        state = 72;
      fi
    }
  :: state == 62 ->
    atomic {
      if
      :: p5 ? c5_t6 ->
        act ! c5_t6;
        state = 63;
      fi
    }
  :: state == 72 ->
    atomic {
      if
      :: p5_2 ! c2_t3__c5_t19 ->

        state = 70;
      fi
    }
  :: state == 63 ->
    atomic {
      if
      :: p5 ? c5_t7 ->
        act ! c5_t7;
        state = 64;
      :: p5 ? c5_t12 ->
        act ! c5_t12;
        state = 67;
      fi
    }
  :: state == 70 ->
    atomic {
      if
      :: p5 ? c5_t16 ->
        act ! c5_t16;
        state = 71;
      fi
    }
  :: state == 64 ->
    atomic {
      if
      :: p5 ? c5_t8 ->
        act ! c5_t8;
        state = 56;
      fi
    }
  :: state == 67 ->
    atomic {
      if
      :: p5 ? c5_t13 ->
        act ! c5_t13;
        state = 68;
      fi
    }
  :: state == 56 ->
    atomic {
      if
      :: p5 ? c5_t0 ->
        act ! c5_t0;
        state = 57;
      :: p5 ? c5_t9 ->
        act ! c5_t9;
        state = 65;
      fi
    }
  :: state == 68 ->
    atomic {
      if
      :: p5 ? c5_t14 ->
        act ! c5_t14;
        state = 69;
      fi
    }
  :: state == 57 ->
    atomic {
      if
      :: p5 ? c5_t1 ->
        act ! c5_t1;
        state = 58;
      fi
    }
  :: state == 65 ->
    atomic {
      if
      :: p5 ? c5_t10 ->
        act ! c5_t10;
        state = 66;
      fi
    }
  :: state == 69 ->
    atomic {
      if
      :: p5 ? c5_t15 ->
        act ! c5_t15;
        state = 63;
      fi
    }
  :: state == 58 ->
    atomic {
      if
      :: p5 ? c5_t2 ->
        act ! c5_t2;
        state = 59;
      fi
    }
  :: state == 59 ->
    atomic {
      if
      :: p5 ? c5_t3 ->
        act ! c5_t3;
        state = 60;
      fi
    }
  :: state == 60 ->
    atomic {
      if
      :: p5 ? c5_t4 ->
        act ! c5_t4;
        state = 61;
      fi
    }
  :: state == 61 ->
    atomic {
      if
      :: p5 ? c5_t5 ->
        act ! c5_t5;
        state = 56;
      fi
    }
  od
}

#define pc5t17	 (lastAction == c5_t17)
#define pc5t16	 (lastAction == c5_t16)
#define pc5t15	 (lastAction == c5_t15)
#define pc5t13	 (lastAction == c5_t13)
#define pc5t12	 (lastAction == c5_t12)
#define pc5t8	 (lastAction == c5_t8)
#define pc5t7	 (lastAction == c5_t7)
#define pc5t6	 (lastAction == c5_t6)
#define pc5t4	 (lastAction == c5_t4)
#define pc5t10	 (lastAction == c5_t10)
#define pc5t0	 (lastAction == c5_t0)
#define pc4t6	 (lastAction == c4_t6)
#define pc4t2	 (lastAction == c4_t2)
#define pc5t18	 (lastAction == c5_t18)
#define pc4t1	 (lastAction == c4_t1)
#define pc4t0	 (lastAction == c4_t0)
#define pc3t12c4t7	 (lastAction == c3_t12__c4_t7)
#define pc4t5c3t3	 (lastAction == c4_t5__c3_t3)
#define pc3t14	 (lastAction == c3_t14)
#define pc3t13	 (lastAction == c3_t13)
#define pc3t11	 (lastAction == c3_t11)
#define pc3t8	 (lastAction == c3_t8)
#define pc3t5	 (lastAction == c3_t5)
#define pc3t4	 (lastAction == c3_t4)
#define pc3t2	 (lastAction == c3_t2)
#define pc5t11	 (lastAction == c5_t11)
#define pc2t3c5t19	 (lastAction == c2_t3__c5_t19)
#define pc2t7c3t10	 (lastAction == c2_t7__c3_t10)
#define pc2t5	 (lastAction == c2_t5)
#define pc4t3	 (lastAction == c4_t3)
#define pc5t5	 (lastAction == c5_t5)
#define pc1t6c3t9	 (lastAction == c1_t6__c3_t9)
#define pc1t7	 (lastAction == c1_t7)
#define pc3t6c2t2	 (lastAction == c3_t6__c2_t2)
#define pc1t5	 (lastAction == c1_t5)
#define pc1t8	 (lastAction == c1_t8)
#define pc1t4	 (lastAction == c1_t4)
#define pc1t2	 (lastAction == c1_t2)
#define pc1t0	 (lastAction == c1_t0)
#define pc4t4c0t13	 (lastAction == c4_t4__c0_t13)
#define pc2t0c0t1	 (lastAction == c2_t0__c0_t1)
#define pc0t10	 (lastAction == c0_t10)
#define pc0t8	 (lastAction == c0_t8)
#define pc0t7	 (lastAction == c0_t7)
#define pc2t9	 (lastAction == c2_t9)
#define pc0t6	 (lastAction == c0_t6)
#define pc5t14	 (lastAction == c5_t14)
#define pc2t1	 (lastAction == c2_t1)
#define pc0t15	 (lastAction == c0_t15)
#define pc0t11	 (lastAction == c0_t11)
#define pc0t9	 (lastAction == c0_t9)
#define pc3t1	 (lastAction == c3_t1)
#define pc0t5	 (lastAction == c0_t5)
#define pc0t4	 (lastAction == c0_t4)
#define pc1t3c2t8	 (lastAction == c1_t3__c2_t8)
#define pc1t1c3t0	 (lastAction == c1_t1__c3_t0)
#define pc0t3	 (lastAction == c0_t3)
#define pc0t12c4t9	 (lastAction == c0_t12__c4_t9)
#define pc5t3	 (lastAction == c5_t3)
#define pc2t6c1t9	 (lastAction == c2_t6__c1_t9)
#define pc0t14	 (lastAction == c0_t14)
#define pc5t2	 (lastAction == c5_t2)
#define pc5t9	 (lastAction == c5_t9)
#define pc0t2	 (lastAction == c0_t2)
#define pc4t8	 (lastAction == c4_t8)
#define pc3t7	 (lastAction == c3_t7)
#define pc5t1	 (lastAction == c5_t1)
#define pc2t4	 (lastAction == c2_t4)
#define pc0t0	 (lastAction == c0_t0)
