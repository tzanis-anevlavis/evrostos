/* Actions (message types) */
mtype = { nop, c3_t14, c3_t12, c3_t10, c3_t8, c3_t5, c3_t6, c3_t0__c2_t0, c3_t13, c2_t6, c2_t5, c1_t8__c3_t11, c3_t9__c1_t2, c1_t5, c1_t4, c2_t8, c1_t3, c3_t2, c2_t3, c1_t0__c0_t8, c2_t2, c3_t4__c0_t1, c1_t6, c0_t12__c3_t1, c3_t3, c1_t1, c0_t10, c1_t9, c0_t7, c0_t15, c0_t11, c1_t7__c2_t7, c2_t1, c2_t9, c0_t6, c0_t9, c0_t5, c0_t4, c0_t3, c0_t2, c0_t14, c0_t13, c3_t7, c0_t0, c2_t4 };

/* Inter-process channels */
chan p3_2 = [0] of {mtype};
chan p3_1 = [0] of {mtype};
chan p1_0 = [0] of {mtype};
chan p3_0 = [0] of {mtype};
chan p2_1 = [0] of {mtype};

/* Environement<->process channels */
chan p3 = [0] of {mtype};
chan p2 = [0] of {mtype};
chan p1 = [0] of {mtype};
chan p0 = [0] of {mtype};

/* Environment process */
active proctype Environment()
{
  do
  :: p3 ! c3_t14
  :: p3 ! c3_t12
  :: p3 ! c3_t10
  :: p3 ! c3_t8
  :: p3 ! c3_t5
  :: p3 ! c3_t6
  :: p3 ! c3_t13
  :: p2 ! c2_t6
  :: p2 ! c2_t5
  :: p1 ! c1_t5
  :: p1 ! c1_t4
  :: p2 ! c2_t8
  :: p1 ! c1_t3
  :: p3 ! c3_t2
  :: p2 ! c2_t3
  :: p2 ! c2_t2
  :: p1 ! c1_t6
  :: p3 ! c3_t3
  :: p1 ! c1_t1
  :: p0 ! c0_t10
  :: p1 ! c1_t9
  :: p0 ! c0_t7
  :: p0 ! c0_t15
  :: p0 ! c0_t11
  :: p2 ! c2_t1
  :: p2 ! c2_t9
  :: p0 ! c0_t6
  :: p0 ! c0_t9
  :: p0 ! c0_t5
  :: p0 ! c0_t4
  :: p0 ! c0_t3
  :: p0 ! c0_t2
  :: p3 ! nop
  :: p2 ! nop
  :: p1 ! nop
  :: p0 ! nop
  :: p0 ! c0_t14
  :: p0 ! c0_t13
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
      :: p3_0 ? c0_t12__c3_t1 ->
        act ! c0_t12__c3_t1;
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
      :: p0 ? c0_t13 ->
        act ! c0_t13;
        state = 12;
      fi
    }
  :: state == 2 ->
    atomic {
      if
      :: p3_0 ? c3_t4__c0_t1 ->
        act ! c3_t4__c0_t1;
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
      :: p1_0 ? c1_t0__c0_t8 ->
        act ! c1_t0__c0_t8;
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
      :: p1 ? c1_t1 ->
        act ! c1_t1;
        state = 17;
      :: p3_1 ? c1_t8__c3_t11 ->
        act ! c1_t8__c3_t11;
        state = 23;
      fi
    }
  :: state == 17 ->
    atomic {
      if
      :: p3_1 ? c3_t9__c1_t2 ->
        act ! c3_t9__c1_t2;
        state = 18;
      fi
    }
  :: state == 23 ->
    atomic {
      if
      :: p1 ? c1_t9 ->
        act ! c1_t9;
        state = 21;
      fi
    }
  :: state == 18 ->
    atomic {
      if
      :: p1 ? c1_t3 ->
        act ! c1_t3;
        state = 19;
      fi
    }
  :: state == 21 ->
    atomic {
      if
      :: p1 ? c1_t6 ->
        act ! c1_t6;
        state = 22;
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
  :: state == 22 ->
    atomic {
      if
      :: p2_1 ? c1_t7__c2_t7 ->
        act ! c1_t7__c2_t7;
        state = 16;
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
      :: p1_0 ! c1_t0__c0_t8 ->

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
      :: p2 ? c2_t2 ->
        act ! c2_t2;
        state = 28;
      :: p2 ? c2_t5 ->
        act ! c2_t5;
        state = 30;
      fi
    }
  :: state == 28 ->
    atomic {
      if
      :: p2 ? c2_t3 ->
        act ! c2_t3;
        state = 25;
      fi
    }
  :: state == 30 ->
    atomic {
      if
      :: p2 ? c2_t6 ->
        act ! c2_t6;
        state = 31;
      fi
    }
  :: state == 25 ->
    atomic {
      if
      :: p3_2 ? c3_t0__c2_t0 ->
        act ! c3_t0__c2_t0;
        state = 26;
      fi
    }
  :: state == 31 ->
    atomic {
      if
      :: p2_1 ! c1_t7__c2_t7 ->

        state = 32;
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
  :: state == 32 ->
    atomic {
      if
      :: p2 ? c2_t8 ->
        act ! c2_t8;
        state = 33;
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
      :: p3 ? c3_t12 ->
        act ! c3_t12;
        state = 45;
      :: p3_1 ! c3_t9__c1_t2 ->

        state = 43;
      fi
    }
  :: state == 39 ->
    atomic {
      if
      :: p3_0 ! c3_t4__c0_t1 ->

        state = 40;
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
  :: state == 43 ->
    atomic {
      if
      :: p3 ? c3_t10 ->
        act ! c3_t10;
        state = 44;
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
      :: p3_1 ! c1_t8__c3_t11 ->

        state = 42;
      fi
    }
  :: state == 41 ->
    atomic {
      if
      :: p3 ? c3_t6 ->
        act ! c3_t6;
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
      :: p3 ? c3_t3 ->
        act ! c3_t3;
        state = 35;
      fi
    }
  :: state == 35 ->
    atomic {
      if
      :: p3_2 ! c3_t0__c2_t0 ->

        state = 36;
      fi
    }
  :: state == 36 ->
    atomic {
      if
      :: p3_0 ! c0_t12__c3_t1 ->

        state = 37;
      fi
    }
  od
}

#define pc3t14	 (lastAction == c3_t14)
#define pc3t12	 (lastAction == c3_t12)
#define pc3t10	 (lastAction == c3_t10)
#define pc3t8	 (lastAction == c3_t8)
#define pc3t5	 (lastAction == c3_t5)
#define pc3t6	 (lastAction == c3_t6)
#define pc3t0c2t0	 (lastAction == c3_t0__c2_t0)
#define pc3t13	 (lastAction == c3_t13)
#define pc2t6	 (lastAction == c2_t6)
#define pc2t5	 (lastAction == c2_t5)
#define pc1t8c3t11	 (lastAction == c1_t8__c3_t11)
#define pc3t9c1t2	 (lastAction == c3_t9__c1_t2)
#define pc1t5	 (lastAction == c1_t5)
#define pc1t4	 (lastAction == c1_t4)
#define pc2t8	 (lastAction == c2_t8)
#define pc1t3	 (lastAction == c1_t3)
#define pc3t2	 (lastAction == c3_t2)
#define pc2t3	 (lastAction == c2_t3)
#define pc1t0c0t8	 (lastAction == c1_t0__c0_t8)
#define pc2t2	 (lastAction == c2_t2)
#define pc3t4c0t1	 (lastAction == c3_t4__c0_t1)
#define pc1t6	 (lastAction == c1_t6)
#define pc0t12c3t1	 (lastAction == c0_t12__c3_t1)
#define pc3t3	 (lastAction == c3_t3)
#define pc1t1	 (lastAction == c1_t1)
#define pc0t10	 (lastAction == c0_t10)
#define pc1t9	 (lastAction == c1_t9)
#define pc0t7	 (lastAction == c0_t7)
#define pc0t15	 (lastAction == c0_t15)
#define pc0t11	 (lastAction == c0_t11)
#define pc1t7c2t7	 (lastAction == c1_t7__c2_t7)
#define pc2t1	 (lastAction == c2_t1)
#define pc2t9	 (lastAction == c2_t9)
#define pc0t6	 (lastAction == c0_t6)
#define pc0t9	 (lastAction == c0_t9)
#define pc0t5	 (lastAction == c0_t5)
#define pc0t4	 (lastAction == c0_t4)
#define pc0t3	 (lastAction == c0_t3)
#define pc0t2	 (lastAction == c0_t2)
#define pc0t14	 (lastAction == c0_t14)
#define pc0t13	 (lastAction == c0_t13)
#define pc3t7	 (lastAction == c3_t7)
#define pc0t0	 (lastAction == c0_t0)
#define pc2t4	 (lastAction == c2_t4)
