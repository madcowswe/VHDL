LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.std_logic_arith.ALL;
USE work.vdp_pack.ALL;
USE work.project_pack.ALL;


ENTITY db_behav IS
   PORT(
      clk: IN std_logic;
      reset: IN std_logic; -- not used!
      -- bus from host
      hdb      : IN  STD_LOGIC_VECTOR( 15 DOWNTO 0);
      dav      : IN  STD_LOGIC;
      hdb_busy : OUT STD_LOGIC;

      -- bus to VRAM
      dbb                 : OUT db_2_rcd;
      dbb_delaycmd        : IN std_logic;

      -- to testbench
      db_finish : OUT std_logic
      );
END db_behav;


ARCHITECTURE behav OF db_behav IS

   SIGNAL dbb_i: db_2_rcd;

   SIGNAL cycle_init, cycle_done, hdb_busy_i : STD_LOGIC;

BEGIN
    dbb <= dbb_i;
    hdb_busy <= hdb_busy_i;
    db_finish <= not dbb_i.startcmd and not hdb_busy_i and not dav;

   command : PROCESS

      VARIABLE x, y : INTEGER;
      VARIABLE cmd  : cmd_type;
      VARIABLE pen  : pen_type;

   BEGIN
      hdb_busy_i <= '1';
      x := 0;
      y := 0;
      WAIT UNTIL reset = '1';
      WAIT UNTIL reset = '0';
      WHILE TRUE LOOP
      wait until clk'EVENT and clk = '1' and dav = '1' ;     
      decode_paras( hdb, cmd, x, y, pen);
      hdb_busy_i <= '1';
      do_vdp_command( cmd, x, y, pen, cycle_init, cycle_done, rcb);
      hdb_busy_i <= '0';
    END LOOP;
   END PROCESS command;

   rcb_timing: PROCESS
   BEGIN
      WAIT UNTIL clk'EVENT and clk='1';
      if dbb_i.startcmd = '1' and dbb_delaycmd = '0' THEN
         cycle_done <= '0';
         WAIT FOR 0 ns;
         cycle_done <= '1';
      END IF;
   END PROCESS rcb_timing;

   rcbio : PROCESS
      -- this process implements RCB interface cycles, using 0->1 transitions on
      -- cycle_init (start a new cycle)
      -- and cycle_done (cycle is complete)
      -- to handshake the cycle
      -- all cycle I/O and R/W control, are specified via shared variables
      -- which must be written before cycle_init 0->1, or read after
      -- cycle_done 0->1
   BEGIN
      WAIT UNTIL cycle_init'EVENT AND cycle_init = '1';
      -- at this time x_rcb_v, y_rcb_v, pen_rcb_v specify what RCB cycle is
      -- required
      dbb_i.x <= conv_std_logic_vector( x_rcb_v, dbb.x'LENGTH);
      dbb_i.y <= conv_std_logic_vector( y_rcb_v, dbb.y'LENGTH);
      CASE pen_rcb_v IS
			WHEN white => dbb_i.rcb_cmd <= "001";
			WHEN black => dbb_i.rcb_cmd <= "010";
			WHEN invert => dbb_i.rcb_cmd <= "011";
            WHEN OTHERS => ASSERT FALSE REPORT "Wrong pen type in behav rcb output" SEVERITY error;
	  END CASE;
      dbb_i.startcmd <= '1';
      wait until cycle_done'EVENT and cycle_done = '1';
      dbb_i.startcmd <= '0';
   END PROCESS;

END behav;      


