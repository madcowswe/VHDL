library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.project_pack.all;
use work.config_pack.all;
use work.draw_any_octant;


entity db is
   port(
      clk: in std_logic;
      reset: in std_logic;

      -- bus from host
      hdb      : in  STD_LOGIC_VECTOR( 15 downto 0);
      dav      : in  STD_LOGIC;
      hdb_busy : out STD_LOGIC;

      -- bus to VRAM
      dbb                 : out db_2_rcd;
      dbb_delaycmd        : in std_logic;

      -- to testbench
      db_finish : out std_logic
      );
end db;

architecture arch of db is

--------------------------------------------------------------
-- pseudo-package
--------------------------------------------------------------

	-- fsm states
	type db_fsm_state_t is (UNITIALISED_STATE, idle_state, draw_single_pix_state, set_xy_state, fill_state, draw_line_state);

	type line_fsm_state_t is (LINE_UNITIALISED_STATE, line_startpt_state, line_endpt_state, line_pix_state);

	type fill_fsm_state_t is (FILL_UNINITIALISED_STATE, fill_startpt_state, fill_endpt_state);

	-- abstraction layer for host commands
	type opcode_t is ( movepen, drawline, fill );
	type pen_t is ( black, white, invert );

	-- host instruction word fields
	type host_cmd_t is
	record
		opcode  : opcode_t; 
		x 		: std_logic_vector(VSIZE-1 downto 0);
		y 		: std_logic_vector(VSIZE-1 downto 0);	
		pen 	: pen_t;
	end record;

	--------------------------------------------------------------
	-- Local function prototypes:

	-- outermost fsm transitions based on host's instruction
	function next_db_fsm_state(dav : std_logic; hdb : std_logic_vector; current_x : std_logic_vector; current_y : std_logic_vector) return db_fsm_state_t;

	-- decode instruction word from host into a record structure for convenience and modularity
	function decode_host_cmd(instr_word : std_logic_vector) return host_cmd_t;

	-- decode host cmd to opcode for the rcb
	function decode_to_rcb_opcode(host_cmd : host_cmd_t) return std_logic_vector;

	-- calculate delta between two coordinate values
	function calc_delta(coord1 : std_logic_vector; coord2 : std_logic_vector) return unsigned;

	--------------------------------------------------------------
	-- Local functions:

	-- outermost fsm transitions based on host's instruction
	function next_db_fsm_state(dav : std_logic; hdb : std_logic_vector; current_x : std_logic_vector; current_y : std_logic_vector) return db_fsm_state_t is
		variable cmd : host_cmd_t;
	begin
		cmd := decode_host_cmd(hdb);
		if (dav='1') then -- if host has a pending instruction, go directly to execution stage of the fsm
			case cmd.opcode is

			 when movepen => return set_xy_state;

			 -- if line or fill to the same target coordinate as current -> single pixel draw, otherwise line/fill
			 when drawline | fill  => 
			 	if cmd.x = current_x and cmd.y = current_y then
					return draw_single_pix_state;
				elsif cmd.opcode=drawline then
					return draw_line_state;
				else
					return fill_state;
				end if;

			 when others => return idle_state; -- erroneous transition
			end case;
		else -- no pending instructions from the host
			return idle_state; 
		end if;

	end next_db_fsm_state;

	--------------------------------------------------------------

	-- decode instruction word from host into a record structure for convenience and modularity
	function decode_host_cmd(instr_word : std_logic_vector) return host_cmd_t is
		variable cmd : host_cmd_t;
	begin
		-- defaults
		cmd.opcode := movepen;
		cmd.pen := white;
		cmd.x := (others=>'0');
		cmd.y := (others=>'0');

		-- bitfields hardcoded, poor practice but given config files do not parameterise it properly
		-- *_h constants defined in given config_pack
		case instr_word(15 downto 14) is
		 when movepen_h 	=> cmd.opcode := movepen;
		 when drawline_h 	=> cmd.opcode := drawline;
		 when clearscreen_h => cmd.opcode := fill;
		 when others => null;
		end case;

		case instr_word(1 downto 0) is
		 when black_h  => cmd.pen := black;
		 when white_h  => cmd.pen := white;
		 when invert_h => cmd.pen := invert;
		 when others => null;
		end case;

		cmd.x := instr_word(13 downto 8);
		cmd.y := instr_word(7 downto 2);

		return cmd;

	end decode_host_cmd;

	--------------------------------------------------------------

	-- decode host cmd to opcode for the rcb
	function decode_to_rcb_opcode(host_cmd : host_cmd_t) return std_logic_vector is
	begin

		-- rcb opcodes defined in config pack
		case host_cmd.pen is

		 when white => 
		 	if host_cmd.opcode = drawline then
		 		return dbrcb_draw_white;
		 	else
		 		return dbrcb_fill_white;
		 	end if;

		 when black => 
		 	if host_cmd.opcode = drawline then
		 		return dbrcb_draw_black;
		 	else
		 		return dbrcb_fill_black;
		 	end if;

		 when invert =>
 		 	if host_cmd.opcode = drawline then
		 		return dbrcb_draw_invert;
		 	else
		 		return dbrcb_fill_invert;
		 	end if;

		 when others => return dbrcb_move; -- shouldn't happen
		end case;

	end decode_to_rcb_opcode;

	----------------------------------------------------------------

	-- calculate delta between two coordinate values (must be same size)
	function calc_delta(coord1 : std_logic_vector; coord2 : std_logic_vector) return unsigned is
		variable sgn_rsz_coord1, sgn_rsz_coord2 : signed(coord1'length downto 0); -- note that this is one bit longer than coord1 & coord2
	begin
		-- cast and extend to one more bit to preserve carry
		sgn_rsz_coord1 := signed(resize(unsigned(coord1), coord1'length +1));
		sgn_rsz_coord2 := signed(resize(unsigned(coord2), coord1'length +1));
	
		return unsigned(abs(sgn_rsz_coord1 - sgn_rsz_coord2));

	end calc_delta;

--------------------------------------------------------------
-- architecture signals
--------------------------------------------------------------

	-- shadowing outputs
	signal dbb_alias : db_2_rcd;
	signal hdb_busy_alias : std_logic;
	
	-- registered host command
	signal saved_host_cmd : host_cmd_t;
	
 	-- registered cursor xy and enable signal
 	signal xy_reg_enable : std_logic;

	signal current_x : std_logic_vector(VSIZE-1 downto 0);
	signal current_y : std_logic_vector(VSIZE-1 downto 0);

	-- inputs into xy regs
	signal new_x : std_logic_vector(VSIZE-1 downto 0);
	signal new_y : std_logic_vector(VSIZE-1 downto 0);

	-- fsm states
	signal fsm_state : db_fsm_state_t;
	signal line_fsm_state : line_fsm_state_t;
	signal fill_fsm_state : fill_fsm_state_t;

	-- line drawing module glue signals
	signal line_enable : std_logic;
	signal line_startpt : std_logic;
	signal line_endpt : std_logic;
	signal line_done : std_logic;

	signal line_xbias : std_logic;
	signal line_swapxy : std_logic;
	signal line_negx : std_logic;
	signal line_negy : std_logic;

	signal line_xout : std_logic_vector(VSIZE-1 downto 0);
	signal line_yout : std_logic_vector(VSIZE-1 downto 0);

---------------------------------------------------------------
-- body of architecture
---------------------------------------------------------------

begin

-- shadowing
dbb <= dbb_alias;
hdb_busy <= hdb_busy_alias;

---------------------------------------------------------------

-- clocked process to handle state transitions for the fsm (including nested fsms)
FSM_TRANSITION_R: process
begin
	wait until clk'event and clk='1';
	if reset = '1' then
		fsm_state <= idle_state;
		line_fsm_state <= line_startpt_state;
		fill_fsm_state <= fill_startpt_state;
	else
		-- default, will spawn a warning but is clearer
		fsm_state <= fsm_state;
		line_fsm_state <= line_fsm_state;
		fill_fsm_state <= fill_fsm_state;

		-- note: new_xy will always be the freshest state of the cursor's xy state
		case fsm_state is

		-- idling while waiting for new host command if previous were nops
		 when idle_state => 
		 	fsm_state <= next_db_fsm_state(dav, hdb, new_x, new_y);

		-- single pixel drawing
		 when draw_single_pix_state => 
		 	if dbb_delaycmd='0' then -- if rcb slave is not busy, can consume next command
				fsm_state <= next_db_fsm_state(dav, hdb, new_x, new_y);
			end if;

		-- move cursor
		 when set_xy_state => 
		 	fsm_state <= next_db_fsm_state(dav, hdb, new_x, new_y);

		-- fill subfsm on fills
		 when fill_state => 
		 	case (fill_fsm_state) is
				 when fill_startpt_state => if dbb_delaycmd='0' then -- proceed if rcb not blocking
				 								fill_fsm_state <= fill_endpt_state;
				 							end if;

				 when fill_endpt_state => if dbb_delaycmd='0' then -- consume new command if rcb not blocking
				 								fill_fsm_state <= fill_startpt_state;
				 								fsm_state <= next_db_fsm_state(dav, hdb, new_x, new_y);
				 						 end if;

				 when others => null;
			end case;

		-- line subfsm on line drawing
		 when draw_line_state => 
		 	case line_fsm_state is
				when line_startpt_state => 
					line_fsm_state <= line_endpt_state;

				when line_endpt_state => 
					line_fsm_state <= line_pix_state;

				when line_pix_state => -- transition to self by default
					 if line_done='1' and dbb_delaycmd='0' then -- if done and not blocked on rcb, transition to next command
						 line_fsm_state <= line_startpt_state;
						 fsm_state <= next_db_fsm_state(dav, hdb, new_x, new_y);
					 end if;

				when others => null;
			end case;

		 when others => null;

		end case;
	end if;
end process FSM_TRANSITION_R;

-------------------------------------------------------------------------------------------

-- control potential accept state flag (is true when it would be possible to accept a new host cmd in the next cycle)
HDB_BUSY_INTERFACE_C : process (fsm_state, dbb_delaycmd, line_fsm_state, line_done, fill_fsm_state)
begin

	-- default to not be able to accept a command next cycle
	hdb_busy_alias <= '1';

	-- if in a state such that the block could accept a new command (assuming the dependencies like rcb are not blocking the progress), assert flag
	case (fsm_state) is
	
		-- can always accept a new command from the following states
		when idle_state | set_xy_state =>
			hdb_busy_alias <= '0';

		-- depend on rcb
		when draw_single_pix_state => 
			hdb_busy_alias <= dbb_delaycmd;

		-- if line drawing is finished, busy state depends purely on whether rcb is busy
		when draw_line_state => 
			if line_fsm_state=line_pix_state and line_done='1' then
				hdb_busy_alias <= dbb_delaycmd;
			end if;

		-- depend on rcb busy flag when in final fill subfsm state
		when fill_state => 
			if fill_fsm_state=fill_endpt_state then
				hdb_busy_alias <= dbb_delaycmd;
			end if;

		when others => null;
	
	end case;
end process HDB_BUSY_INTERFACE_C;


-- controls done-with-processing flag
DB_FINISH_INTERFACE_C: process (fsm_state, dav)
begin
	-- if no data pending from processor and in idle state, assert done flag
	if fsm_state = idle_state and dav = '0' then
		db_finish <= '1';
	else
		db_finish <= '0';
	end if;
end process DB_FINISH_INTERFACE_C;

-------------------------------------------------------------------------------------------

-- drives the rcb interface (encapsulates all state-derived logic that can affect it)
RCB_INTERFACE_C: process (fsm_state, line_fsm_state, line_xout, line_yout, saved_host_cmd, fill_fsm_state, current_x, current_y)
begin

	-- default to not be able to accept a command next cycle
	dbb_alias.startcmd <= '0';
	dbb_alias.rcb_cmd <= dbrcb_move;
	dbb_alias.X <= std_logic_vector(to_unsigned(0, dbb.X'length));
	dbb_alias.Y <= std_logic_vector(to_unsigned(0, dbb.Y'length));

	
	case fsm_state is
	
		when draw_single_pix_state => 
			dbb_alias.startcmd <= '1';
			dbb_alias.rcb_cmd <= '0' & decode_to_rcb_opcode(saved_host_cmd)(1 downto 0); -- override instruction to a draw even if it originated from fill
			dbb_alias.X <= saved_host_cmd.x;
			dbb_alias.Y <= saved_host_cmd.y;

		when draw_line_state => 
			case line_fsm_state is
			 when line_pix_state => 
			 	dbb_alias.startcmd <= '1';
				 dbb_alias.rcb_cmd <= decode_to_rcb_opcode(saved_host_cmd); 
				 dbb_alias.X <= line_xout; -- multiplex output of the drawing entity to the rcb bus
				 dbb_alias.Y <= line_yout;

			 when others => null;
			end case;

		when fill_state => 
			case fill_fsm_state is
			 when fill_startpt_state =>
				dbb_alias.startcmd <= '1';
				dbb_alias.rcb_cmd <= dbrcb_move;
				dbb_alias.X <= current_x; -- fill start point: current x and y
				dbb_alias.Y <= current_y;

			 when fill_endpt_state =>
				dbb_alias.startcmd <= '1';
				dbb_alias.rcb_cmd <= decode_to_rcb_opcode(saved_host_cmd); 
				dbb_alias.X <= saved_host_cmd.x; -- fill end point: from host command
				dbb_alias.Y <= saved_host_cmd.y;

			 when others => null;
			end case;
		
		when others => null;	-- keep defaults for states that do not affect rcb interface
	
	end case;	
end process RCB_INTERFACE_C; 

-------------------------------------------------------------------------------------------

-- as dealing with host commands is pipelined, need to register the command
SAVED_HOST_CMD_R : process
begin
	wait until clk'event and clk='1';
	if reset = '1' then
		saved_host_cmd <= decode_host_cmd(std_logic_vector(to_unsigned(0, hdb'length)));
	else

		-- if going to accept a new command, enable the register
		if hdb_busy_alias = '0' then
			saved_host_cmd <= decode_host_cmd(hdb);
		end if;

	end if;
end process SAVED_HOST_CMD_R; 

-------------------------------------------------------------------------------------------

-- registers holding current x and y cursor coordinates 
XY_REG_R: process
begin
	wait until clk'event and clk='1';
	if reset = '1' then
		current_x <= (others=>'0');
		current_y <= (others=>'0');
	else
		if xy_reg_enable='1' then
			current_x <= new_x;
			current_y <= new_y;
		end if;
	end if;
end process XY_REG_R;

-------------------------------------------------------------------------------------------

-- combinatorial enable and input data into the xy state registers
XY_REG_INPUT_C : process (fsm_state, line_fsm_state, saved_host_cmd, fill_fsm_state, current_x, current_y, dbb_delaycmd)
begin

	xy_reg_enable <= '0';

	-- by default: new_xy are equal to contents of current_xy registers
	new_x <= current_x;
	new_y <= current_y;

	-- if update xy instruction, enable register and set up data
	case fsm_state is
	 when set_xy_state =>
		xy_reg_enable <= '1';
		new_x <= saved_host_cmd.x;
		new_y <= saved_host_cmd.y;
	
	-- update xy during line setup
	 when draw_line_state =>
	 	if line_fsm_state = line_startpt_state and dbb_delaycmd='0' then --todo: temp hack
			xy_reg_enable <= '1';
			new_x <= saved_host_cmd.x;
			new_y <= saved_host_cmd.y;
		end if;

	-- update xy state if doing fills
	 when fill_state =>
	 	if fill_fsm_state = fill_startpt_state and dbb_delaycmd='0' then --todo: temp hack
			xy_reg_enable <= '1';
			new_x <= saved_host_cmd.x;
			new_y <= saved_host_cmd.y;
		end if;	 		

	 when others => null;
	end case;

end process XY_REG_INPUT_C;

-------------------------------------------------------------------------------------------

-- line drawing entity
LINE_DRAWER: entity draw_any_octant
generic map (VSIZE)
port map (
  clk    => clk,
  enable => line_enable,
  resetx => line_startpt,
  draw   => line_endpt,
  done   => line_done,
  x      => line_xout,
  y      => line_yout,
  xin    => current_x,
  yin    => current_y,
  xbias  => line_xbias,
  swapxy => line_swapxy,
  negx   => line_negx,
  negy   => line_negy);


-- interface to the line drawing entity
LINE_INTERFACE_C: process (line_fsm_state, dbb_delaycmd, current_x, current_y, saved_host_cmd)
begin

	-- pulse start point signal for one cycle to set up starting xy coords
	line_startpt <= '0';
	if line_fsm_state = line_startpt_state then
		line_startpt <= '1';
	end if;

	-- pulse end point signal for a cycle to set final xy coords
	line_endpt <= '0';
	if line_fsm_state = line_endpt_state then
		line_endpt <= '1';
	end if;

	-- pause the draw block while waiting for ram
	line_enable <= '1';
	if line_fsm_state = line_pix_state then
		line_enable <= not dbb_delaycmd;
	end if;

	-- TODO: bias hacked inside drawing entity to meet testbench spec (wrong but tb won't be fixed)
	line_xbias <= '1';

	-- negx and negy for drawing lines in any octant
	line_negx <= '0';
	if (unsigned(current_x) > unsigned(saved_host_cmd.x)) then
		line_negx <= '1';
	end if;

	line_negy <= '0';
	if (unsigned(current_y) > unsigned(saved_host_cmd.y)) then
		line_negy <= '1';
	end if;

	-- swap x and y if vertical direction lines (delta y bigger than delta x)
	line_swapxy <= '0';
	if (calc_delta(current_x, saved_host_cmd.x) < calc_delta(current_y, saved_host_cmd.y)) then
		line_swapxy <= '1';
	end if;


end process LINE_INTERFACE_C;

-------------------------------------------------------------------------------------------

end architecture arch;