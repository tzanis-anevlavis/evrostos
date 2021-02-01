/* Actions (message types) */
mtype = { nop, c0_t7, c0_t9, c0_t6, c0_t5, c0_t4, c0_t3, c0_t2, c0_t8, c0_t1, c0_t0 };

/* Inter-process channels */

/* Environement<->process channels */
chan p0 = [0] of {mtype};

/* Environment process */
active proctype Environment()
{
  do
  :: p0 ! c0_t7
  :: p0 ! c0_t9
  :: p0 ! c0_t6
  :: p0 ! c0_t5
  :: p0 ! c0_t4
  :: p0 ! c0_t3
  :: p0 ! nop
  :: p0 ! c0_t2
  :: p0 ! c0_t8
  :: p0 ! c0_t1
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

#define pc0t7	 (lastAction == c0_t7)
#define pc0t9	 (lastAction == c0_t9)
#define pc0t6	 (lastAction == c0_t6)
#define pc0t5	 (lastAction == c0_t5)
#define pc0t4	 (lastAction == c0_t4)
#define pc0t3	 (lastAction == c0_t3)
#define pc0t2	 (lastAction == c0_t2)
#define pc0t8	 (lastAction == c0_t8)
#define pc0t1	 (lastAction == c0_t1)
#define pc0t0	 (lastAction == c0_t0)
