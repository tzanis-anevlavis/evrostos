/* Actions (message types) */
mtype = { nop, c1_t7, c1_t6, c1_t3, c1_t1, c1_t0, c1_t5, c1_t9, c0_t7, c0_t0__c1_t2, c0_t9, c0_t6, c0_t5, c1_t8, c1_t4, c0_t4, c0_t3, c0_t2, c0_t1, c0_t8 };

/* Inter-process channels */
chan p1_0 = [0] of {mtype};

/* Environement<->process channels */
chan p1 = [0] of {mtype};
chan p0 = [0] of {mtype};

/* Environment process */
active proctype Environment()
{
  do
  :: p1 ! c1_t7
  :: p1 ! c1_t6
  :: p1 ! c1_t3
  :: p1 ! c1_t1
  :: p1 ! c1_t0
  :: p1 ! c1_t5
  :: p1 ! c1_t9
  :: p0 ! c0_t7
  :: p0 ! c0_t9
  :: p0 ! c0_t6
  :: p0 ! c0_t5
  :: p1 ! c1_t8
  :: p1 ! c1_t4
  :: p0 ! c0_t4
  :: p0 ! c0_t3
  :: p0 ! c0_t2
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
      :: p0 ? c0_t5 ->
        act ! c0_t5;
        state = 1;
      fi
    }
  :: state == 1 ->
    atomic {
      if
      :: p1_0 ? c0_t0__c1_t2 ->
        act ! c0_t0__c1_t2;
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
      :: p1 ? c1_t8 ->
        act ! c1_t8;
        state = 18;
      :: p1_0 ! c0_t0__c1_t2 ->

        state = 14;
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
      :: p1 ? c1_t9 ->
        act ! c1_t9;
        state = 17;
      fi
    }
  :: state == 14 ->
    atomic {
      if
      :: p1 ? c1_t3 ->
        act ! c1_t3;
        state = 11;
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

#define pc1t7	 (lastAction == c1_t7)
#define pc1t6	 (lastAction == c1_t6)
#define pc1t3	 (lastAction == c1_t3)
#define pc1t1	 (lastAction == c1_t1)
#define pc1t0	 (lastAction == c1_t0)
#define pc1t5	 (lastAction == c1_t5)
#define pc1t9	 (lastAction == c1_t9)
#define pc0t7	 (lastAction == c0_t7)
#define pc0t0c1t2	 (lastAction == c0_t0__c1_t2)
#define pc0t9	 (lastAction == c0_t9)
#define pc0t6	 (lastAction == c0_t6)
#define pc0t5	 (lastAction == c0_t5)
#define pc1t8	 (lastAction == c1_t8)
#define pc1t4	 (lastAction == c1_t4)
#define pc0t4	 (lastAction == c0_t4)
#define pc0t3	 (lastAction == c0_t3)
#define pc0t2	 (lastAction == c0_t2)
#define pc0t1	 (lastAction == c0_t1)
#define pc0t8	 (lastAction == c0_t8)
