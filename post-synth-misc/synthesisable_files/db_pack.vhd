library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.project_pack.all;
use work.config_pack.all;

-- note: all functions/types moved into a separate package file to avoid modelsim coverage errors

package db_pack is

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

end package db_pack;

--------------------------------------------------------------

package body db_pack is

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

end package body db_pack;