/* Actions (message types) */
mtype = { nop, c2_t13, c2_t12, c2_t10, c2_t8, c2_t6, c2_t4, c2_t0, c2_t5__c1_t0, c1_t7, c2_t2, c1_t6, c1_t2, c2_t14, c1_t1, c2_t7, c1_t3__c0_t0, c1_t5, c0_t7, c0_t9, c2_t9, c0_t6, c2_t1, c1_t8, c1_t4, c0_t4, c0_t3, c0_t5__c2_t3, c0_t2, c1_t9__c2_t11, c0_t1, c0_t8 };

/* Inter-process channels */
chan p2_1 = [0] of {mtype};
chan p1_0 = [0] of {mtype};
chan p2_0 = [0] of {mtype};

/* Environement<->process channels */
chan p2 = [0] of {mtype};
chan p1 = [0] of {mtype};
chan p0 = [0] of {mtype};

/* Environment process */
active proctype Environment()
{
  do
  :: p2 ! c2_t13
  :: p2 ! c2_t12
  :: p2 ! c2_t10
  :: p2 ! c2_t8
  :: p2 ! c2_t6
  :: p2 ! c2_t4
  :: p2 ! c2_t0
  :: p1 ! c1_t7
  :: p2 ! c2_t2
  :: p1 ! c1_t6
  :: p1 ! c1_t2
  :: p2 ! c2_t14
  :: p1 ! c1_t1
  :: p2 ! c2_t7
  :: p1 ! c1_t5
  :: p0 ! c0_t7
  :: p0 ! c0_t9
  :: p2 ! c2_t9
  :: p0 ! c0_t6
  :: p2 ! c2_t1
  :: p1 ! c1_t8
  :: p1 ! c1_t4
  :: p0 ! c0_t4
  :: p0 ! c0_t3
  :: p0 ! c0_t2
  :: p2 ! nop
  :: p1 ! nop
  :: p0 ! nop
  :: p0 ! c0_t1
  :: p0 ! c0_t8
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
      :: p2_0 ? c0_t5__c2_t3 ->
        act ! c0_t5__c2_t3;
        state = 1;
      fi
    }
  :: state == 1 ->
    atomic {
      if
      :: p1_0 ? c1_t3__c0_t0 ->
        act ! c1_t3__c0_t0;
        state = 2;
      fi
    }
  :: state == 2 ->
    atomic {
      if
      :: p0 ? c0_t1 ->
        act ! c0_t1;
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
      :: p0 ? c0_t8 ->
        act ! c0_t8;
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
      :: p2_1 ? c2_t5__c1_t0 ->
        act ! c2_t5__c1_t0;
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
      :: p1 ? c1_t2 ->
        act ! c1_t2;
        state = 14;
      :: p1 ? c1_t5 ->
        act ! c1_t5;
        state = 16;
      :: p1 ? c1_t8 ->
        act ! c1_t8;
        state = 18;
      fi
    }
  :: state == 14 ->
    atomic {
      if
      :: p1_0 ! c1_t3__c0_t0 ->

        state = 11;
      fi
    }
  :: state == 16 ->
    atomic {
      if
      :: p1 ? c1_t6 ->
        act ! c1_t6;
        state = 15;
      fi
    }
  :: state == 18 ->
    atomic {
      if
      :: p2_1 ? c1_t9__c2_t11 ->
        act ! c1_t9__c2_t11;
        state = 17;
      fi
    }
  :: state == 15 ->
    atomic {
      if
      :: p1 ? c1_t4 ->
        act ! c1_t4;
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
      :: p2 ? c2_t8 ->
        act ! c2_t8;
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
      :: p2_1 ! c2_t5__c1_t0 ->

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
      :: p2 ? c2_t10 ->
        act ! c2_t10;
        state = 29;
      :: p2 ? c2_t12 ->
        act ! c2_t12;
        state = 30;
      :: p2_0 ! c0_t5__c2_t3 ->

        state = 24;
      fi
    }
  :: state == 29 ->
    atomic {
      if
      :: p2_1 ! c1_t9__c2_t11 ->

        state = 23;
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
  :: state == 31 ->
    atomic {
      if
      :: p2 ? c2_t14 ->
        act ! c2_t14;
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

#define pc2t13	 (lastAction == c2_t13)
#define pc2t12	 (lastAction == c2_t12)
#define pc2t10	 (lastAction == c2_t10)
#define pc2t8	 (lastAction == c2_t8)
#define pc2t6	 (lastAction == c2_t6)
#define pc2t4	 (lastAction == c2_t4)
#define pc2t0	 (lastAction == c2_t0)
#define pc2t5c1t0	 (lastAction == c2_t5__c1_t0)
#define pc1t7	 (lastAction == c1_t7)
#define pc2t2	 (lastAction == c2_t2)
#define pc1t6	 (lastAction == c1_t6)
#define pc1t2	 (lastAction == c1_t2)
#define pc2t14	 (lastAction == c2_t14)
#define pc1t1	 (lastAction == c1_t1)
#define pc2t7	 (lastAction == c2_t7)
#define pc1t3c0t0	 (lastAction == c1_t3__c0_t0)
#define pc1t5	 (lastAction == c1_t5)
#define pc0t7	 (lastAction == c0_t7)
#define pc0t9	 (lastAction == c0_t9)
#define pc2t9	 (lastAction == c2_t9)
#define pc0t6	 (lastAction == c0_t6)
#define pc2t1	 (lastAction == c2_t1)
#define pc1t8	 (lastAction == c1_t8)
#define pc1t4	 (lastAction == c1_t4)
#define pc0t4	 (lastAction == c0_t4)
#define pc0t3	 (lastAction == c0_t3)
#define pc0t5c2t3	 (lastAction == c0_t5__c2_t3)
#define pc0t2	 (lastAction == c0_t2)
#define pc1t9c2t11	 (lastAction == c1_t9__c2_t11)
#define pc0t1	 (lastAction == c0_t1)
#define pc0t8	 (lastAction == c0_t8)
