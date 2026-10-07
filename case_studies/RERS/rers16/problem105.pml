/* Actions (message types) */
mtype = { nop, c4_t18, c4_t17, c4_t14, c4_t13, c4_t15, c4_t12, c4_t7, c4_t6, c4_t5, c4_t4, c4_t2, c3_t11, c3_t10, c4_t11, c3_t2, c3_t0, c3_t8, c3_t12__c2_t11, c2_t13, c2_t12, c3_t6, c2_t7, c4_t19__c2_t10, c2_t6, c4_t3, c2_t5, c2_t2, c2_t0, c4_t9, c2_t8__c1_t2, c4_t1__c1_t3, c2_t3__c1_t6, c1_t0, c1_t7, c3_t4, c0_t8__c4_t0, c3_t3, c1_t1, c2_t14__c1_t4, c0_t1__c3_t5, c4_t16, c4_t10, c0_t7, c3_t9, c1_t9, c1_t5, c0_t9, c4_t8__c1_t8, c0_t6, c2_t9, c2_t1, c3_t1, c0_t5, c0_t4, c0_t3, c0_t2, c3_t7, c0_t0, c2_t4 };

/* Inter-process channels */
chan p3_2 = [0] of {mtype};
chan p4_2 = [0] of {mtype};
chan p2_1 = [0] of {mtype};
chan p4_1 = [0] of {mtype};
chan p4_0 = [0] of {mtype};
chan p3_0 = [0] of {mtype};

/* Environement<->process channels */
chan p4 = [0] of {mtype};
chan p3 = [0] of {mtype};
chan p2 = [0] of {mtype};
chan p1 = [0] of {mtype};
chan p0 = [0] of {mtype};

/* Environment process */
active proctype Environment()
{
  do
  :: p4 ! c4_t18
  :: p4 ! c4_t17
  :: p4 ! c4_t14
  :: p4 ! c4_t13
  :: p4 ! c4_t15
  :: p4 ! c4_t12
  :: p4 ! c4_t7
  :: p4 ! c4_t6
  :: p4 ! c4_t5
  :: p4 ! c4_t4
  :: p4 ! c4_t2
  :: p3 ! c3_t11
  :: p3 ! c3_t10
  :: p4 ! c4_t11
  :: p3 ! c3_t2
  :: p3 ! c3_t0
  :: p3 ! c3_t8
  :: p2 ! c2_t13
  :: p2 ! c2_t12
  :: p3 ! c3_t6
  :: p2 ! c2_t7
  :: p2 ! c2_t6
  :: p4 ! c4_t3
  :: p2 ! c2_t5
  :: p2 ! c2_t2
  :: p2 ! c2_t0
  :: p4 ! c4_t9
  :: p1 ! c1_t0
  :: p1 ! c1_t7
  :: p3 ! c3_t4
  :: p3 ! c3_t3
  :: p1 ! c1_t1
  :: p4 ! c4_t16
  :: p4 ! c4_t10
  :: p0 ! c0_t7
  :: p3 ! c3_t9
  :: p1 ! c1_t9
  :: p1 ! c1_t5
  :: p0 ! c0_t9
  :: p0 ! c0_t6
  :: p2 ! c2_t9
  :: p2 ! c2_t1
  :: p3 ! c3_t1
  :: p0 ! c0_t5
  :: p0 ! c0_t4
  :: p0 ! c0_t3
  :: p0 ! c0_t2
  :: p4 ! nop
  :: p3 ! nop
  :: p2 ! nop
  :: p1 ! nop
  :: p0 ! nop
  :: p3 ! c3_t7
  :: p0 ! c0_t0
  :: p2 ! c2_t4
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

        state = 6;
      fi
    }
  :: state == 6 ->
    atomic {
      if
      :: p0 ? c0_t5 ->
        act ! c0_t5;
        state = 1;
      fi
    }
  :: state == 1 ->
    atomic {
      if
      :: p0 ? c0_t0 ->
        act ! c0_t0;
        state = 2;
      fi
    }
  :: state == 2 ->
    atomic {
      if
      :: p3_0 ? c0_t1__c3_t5 ->
        act ! c0_t1__c3_t5;
        state = 3;
      fi
    }
  :: state == 3 ->
    atomic {
      if
      :: p0 ? c0_t2 ->
        act ! c0_t2;
        state = 4;
      fi
    }
  :: state == 4 ->
    atomic {
      if
      :: p0 ? c0_t3 ->
        act ! c0_t3;
        state = 5;
      :: p0 ? c0_t9 ->
        act ! c0_t9;
        state = 7;
      fi
    }
  :: state == 5 ->
    atomic {
      if
      :: p0 ? c0_t4 ->
        act ! c0_t4;
        state = 6;
      fi
    }
  :: state == 7 ->
    atomic {
      if
      :: p0 ? c0_t6 ->
        act ! c0_t6;
        state = 8;
      fi
    }
  :: state == 8 ->
    atomic {
      if
      :: p0 ? c0_t7 ->
        act ! c0_t7;
        state = 9;
      fi
    }
  :: state == 9 ->
    atomic {
      if
      :: p4_0 ? c0_t8__c4_t0 ->
        act ! c0_t8__c4_t0;
        state = 4;
      fi
    }
  od
}

/* Process 1 */
active proctype Proc1()
{
  int state = 10;
  do
  :: state == 10 ->
    atomic {
      if
      :: p1 ? nop ->

        state = 11;
      fi
    }
  :: state == 11 ->
    atomic {
      if
      :: p1 ? c1_t0 ->
        act ! c1_t0;
        state = 12;
      fi
    }
  :: state == 12 ->
    atomic {
      if
      :: p1 ? c1_t1 ->
        act ! c1_t1;
        state = 13;
      fi
    }
  :: state == 13 ->
    atomic {
      if
      :: p1 ? c1_t5 ->
        act ! c1_t5;
        state = 16;
      :: p4_1 ? c4_t8__c1_t8 ->
        act ! c4_t8__c1_t8;
        state = 18;
      :: p2_1 ? c2_t8__c1_t2 ->
        act ! c2_t8__c1_t2;
        state = 14;
      fi
    }
  :: state == 16 ->
    atomic {
      if
      :: p2_1 ? c2_t3__c1_t6 ->
        act ! c2_t3__c1_t6;
        state = 15;
      fi
    }
  :: state == 18 ->
    atomic {
      if
      :: p1 ? c1_t9 ->
        act ! c1_t9;
        state = 17;
      fi
    }
  :: state == 14 ->
    atomic {
      if
      :: p4_1 ? c4_t1__c1_t3 ->
        act ! c4_t1__c1_t3;
        state = 11;
      fi
    }
  :: state == 15 ->
    atomic {
      if
      :: p2_1 ? c2_t14__c1_t4 ->
        act ! c2_t14__c1_t4;
        state = 13;
      fi
    }
  :: state == 17 ->
    atomic {
      if
      :: p1 ? c1_t7 ->
        act ! c1_t7;
        state = 13;
      fi
    }
  od
}

/* Process 2 */
active proctype Proc2()
{
  int state = 19;
  do
  :: state == 19 ->
    atomic {
      if
      :: p2 ? nop ->

        state = 26;
      fi
    }
  :: state == 26 ->
    atomic {
      if
      :: p2 ? c2_t6 ->
        act ! c2_t6;
        state = 27;
      fi
    }
  :: state == 27 ->
    atomic {
      if
      :: p2 ? c2_t7 ->
        act ! c2_t7;
        state = 24;
      fi
    }
  :: state == 24 ->
    atomic {
      if
      :: p2 ? c2_t4 ->
        act ! c2_t4;
        state = 20;
      :: p2_1 ! c2_t8__c1_t2 ->

        state = 25;
      fi
    }
  :: state == 20 ->
    atomic {
      if
      :: p2 ? c2_t0 ->
        act ! c2_t0;
        state = 21;
      fi
    }
  :: state == 25 ->
    atomic {
      if
      :: p2 ? c2_t5 ->
        act ! c2_t5;
        state = 26;
      fi
    }
  :: state == 21 ->
    atomic {
      if
      :: p2 ? c2_t1 ->
        act ! c2_t1;
        state = 22;
      fi
    }
  :: state == 22 ->
    atomic {
      if
      :: p2 ? c2_t2 ->
        act ! c2_t2;
        state = 23;
      fi
    }
  :: state == 23 ->
    atomic {
      if
      :: p2 ? c2_t12 ->
        act ! c2_t12;
        state = 30;
      :: p2_1 ! c2_t3__c1_t6 ->

        state = 24;
      :: p4_2 ? c4_t19__c2_t10 ->
        act ! c4_t19__c2_t10;
        state = 29;
      fi
    }
  :: state == 30 ->
    atomic {
      if
      :: p2 ? c2_t13 ->
        act ! c2_t13;
        state = 31;
      fi
    }
  :: state == 29 ->
    atomic {
      if
      :: p3_2 ? c3_t12__c2_t11 ->
        act ! c3_t12__c2_t11;
        state = 23;
      fi
    }
  :: state == 31 ->
    atomic {
      if
      :: p2_1 ! c2_t14__c1_t4 ->

        state = 28;
      fi
    }
  :: state == 28 ->
    atomic {
      if
      :: p2 ? c2_t9 ->
        act ! c2_t9;
        state = 23;
      fi
    }
  od
}

/* Process 3 */
active proctype Proc3()
{
  int state = 32;
  do
  :: state == 32 ->
    atomic {
      if
      :: p3 ? nop ->

        state = 39;
      fi
    }
  :: state == 39 ->
    atomic {
      if
      :: p3 ? c3_t7 ->
        act ! c3_t7;
        state = 37;
      :: p3 ? c3_t9 ->
        act ! c3_t9;
        state = 40;
      fi
    }
  :: state == 37 ->
    atomic {
      if
      :: p3 ? c3_t4 ->
        act ! c3_t4;
        state = 33;
      :: p3 ? c3_t8 ->
        act ! c3_t8;
        state = 37;
      :: p3_0 ! c0_t1__c3_t5 ->

        state = 38;
      fi
    }
  :: state == 40 ->
    atomic {
      if
      :: p3 ? c3_t10 ->
        act ! c3_t10;
        state = 41;
      fi
    }
  :: state == 33 ->
    atomic {
      if
      :: p3 ? c3_t0 ->
        act ! c3_t0;
        state = 34;
      fi
    }
  :: state == 38 ->
    atomic {
      if
      :: p3 ? c3_t6 ->
        act ! c3_t6;
        state = 39;
      fi
    }
  :: state == 41 ->
    atomic {
      if
      :: p3 ? c3_t11 ->
        act ! c3_t11;
        state = 42;
      fi
    }
  :: state == 34 ->
    atomic {
      if
      :: p3 ? c3_t1 ->
        act ! c3_t1;
        state = 35;
      fi
    }
  :: state == 42 ->
    atomic {
      if
      :: p3_2 ! c3_t12__c2_t11 ->

        state = 39;
      fi
    }
  :: state == 35 ->
    atomic {
      if
      :: p3 ? c3_t2 ->
        act ! c3_t2;
        state = 36;
      fi
    }
  :: state == 36 ->
    atomic {
      if
      :: p3 ? c3_t3 ->
        act ! c3_t3;
        state = 37;
      fi
    }
  od
}

/* Process 4 */
active proctype Proc4()
{
  int state = 43;
  do
  :: state == 43 ->
    atomic {
      if
      :: p4 ? nop ->

        state = 47;
      fi
    }
  :: state == 47 ->
    atomic {
      if
      :: p4 ? c4_t3 ->
        act ! c4_t3;
        state = 48;
      :: p4 ? c4_t16 ->
        act ! c4_t16;
        state = 57;
      fi
    }
  :: state == 48 ->
    atomic {
      if
      :: p4 ? c4_t4 ->
        act ! c4_t4;
        state = 44;
      :: p4_1 ! c4_t8__c1_t8 ->

        state = 49;
      fi
    }
  :: state == 57 ->
    atomic {
      if
      :: p4 ? c4_t17 ->
        act ! c4_t17;
        state = 58;
      fi
    }
  :: state == 44 ->
    atomic {
      if
      :: p4_0 ! c0_t8__c4_t0 ->

        state = 45;
      fi
    }
  :: state == 49 ->
    atomic {
      if
      :: p4 ? c4_t5 ->
        act ! c4_t5;
        state = 50;
      fi
    }
  :: state == 58 ->
    atomic {
      if
      :: p4 ? c4_t18 ->
        act ! c4_t18;
        state = 59;
      fi
    }
  :: state == 45 ->
    atomic {
      if
      :: p4_1 ! c4_t1__c1_t3 ->

        state = 46;
      fi
    }
  :: state == 50 ->
    atomic {
      if
      :: p4 ? c4_t6 ->
        act ! c4_t6;
        state = 51;
      :: p4 ? c4_t11 ->
        act ! c4_t11;
        state = 54;
      fi
    }
  :: state == 59 ->
    atomic {
      if
      :: p4_2 ! c4_t19__c2_t10 ->

        state = 56;
      fi
    }
  :: state == 46 ->
    atomic {
      if
      :: p4 ? c4_t2 ->
        act ! c4_t2;
        state = 47;
      fi
    }
  :: state == 51 ->
    atomic {
      if
      :: p4 ? c4_t7 ->
        act ! c4_t7;
        state = 48;
      fi
    }
  :: state == 54 ->
    atomic {
      if
      :: p4 ? c4_t12 ->
        act ! c4_t12;
        state = 55;
      :: p4 ? c4_t15 ->
        act ! c4_t15;
        state = 47;
      fi
    }
  :: state == 56 ->
    atomic {
      if
      :: p4 ? c4_t14 ->
        act ! c4_t14;
        state = 54;
      fi
    }
  :: state == 55 ->
    atomic {
      if
      :: p4 ? c4_t13 ->
        act ! c4_t13;
        state = 52;
      fi
    }
  :: state == 52 ->
    atomic {
      if
      :: p4 ? c4_t9 ->
        act ! c4_t9;
        state = 53;
      fi
    }
  :: state == 53 ->
    atomic {
      if
      :: p4 ? c4_t10 ->
        act ! c4_t10;
        state = 50;
      fi
    }
  od
}

#define pc4t18	 (lastAction == c4_t18)
#define pc4t17	 (lastAction == c4_t17)
#define pc4t14	 (lastAction == c4_t14)
#define pc4t13	 (lastAction == c4_t13)
#define pc4t15	 (lastAction == c4_t15)
#define pc4t12	 (lastAction == c4_t12)
#define pc4t7	 (lastAction == c4_t7)
#define pc4t6	 (lastAction == c4_t6)
#define pc4t5	 (lastAction == c4_t5)
#define pc4t4	 (lastAction == c4_t4)
#define pc4t2	 (lastAction == c4_t2)
#define pc3t11	 (lastAction == c3_t11)
#define pc3t10	 (lastAction == c3_t10)
#define pc4t11	 (lastAction == c4_t11)
#define pc3t2	 (lastAction == c3_t2)
#define pc3t0	 (lastAction == c3_t0)
#define pc3t8	 (lastAction == c3_t8)
#define pc3t12c2t11	 (lastAction == c3_t12__c2_t11)
#define pc2t13	 (lastAction == c2_t13)
#define pc2t12	 (lastAction == c2_t12)
#define pc3t6	 (lastAction == c3_t6)
#define pc2t7	 (lastAction == c2_t7)
#define pc4t19c2t10	 (lastAction == c4_t19__c2_t10)
#define pc2t6	 (lastAction == c2_t6)
#define pc4t3	 (lastAction == c4_t3)
#define pc2t5	 (lastAction == c2_t5)
#define pc2t2	 (lastAction == c2_t2)
#define pc2t0	 (lastAction == c2_t0)
#define pc4t9	 (lastAction == c4_t9)
#define pc2t8c1t2	 (lastAction == c2_t8__c1_t2)
#define pc4t1c1t3	 (lastAction == c4_t1__c1_t3)
#define pc2t3c1t6	 (lastAction == c2_t3__c1_t6)
#define pc1t0	 (lastAction == c1_t0)
#define pc1t7	 (lastAction == c1_t7)
#define pc3t4	 (lastAction == c3_t4)
#define pc0t8c4t0	 (lastAction == c0_t8__c4_t0)
#define pc3t3	 (lastAction == c3_t3)
#define pc1t1	 (lastAction == c1_t1)
#define pc2t14c1t4	 (lastAction == c2_t14__c1_t4)
#define pc0t1c3t5	 (lastAction == c0_t1__c3_t5)
#define pc4t16	 (lastAction == c4_t16)
#define pc4t10	 (lastAction == c4_t10)
#define pc0t7	 (lastAction == c0_t7)
#define pc3t9	 (lastAction == c3_t9)
#define pc1t9	 (lastAction == c1_t9)
#define pc1t5	 (lastAction == c1_t5)
#define pc0t9	 (lastAction == c0_t9)
#define pc4t8c1t8	 (lastAction == c4_t8__c1_t8)
#define pc0t6	 (lastAction == c0_t6)
#define pc2t9	 (lastAction == c2_t9)
#define pc2t1	 (lastAction == c2_t1)
#define pc3t1	 (lastAction == c3_t1)
#define pc0t5	 (lastAction == c0_t5)
#define pc0t4	 (lastAction == c0_t4)
#define pc0t3	 (lastAction == c0_t3)
#define pc0t2	 (lastAction == c0_t2)
#define pc3t7	 (lastAction == c3_t7)
#define pc0t0	 (lastAction == c0_t0)
#define pc2t4	 (lastAction == c2_t4)
