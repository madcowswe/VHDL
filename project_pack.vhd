USE WORK.config_pack.ALL;
LIBRARY IEEE;
USE IEEE.std_logic_1164.ALL;
USE IEEE.numeric_std.ALL;
use IEEE.MATH_REAL.ALL; --for log2 for auto width

PACKAGE project_pack IS

CONSTANT VSIZE: INTEGER         := 6;  -- length of pixel coordinates.
                                       -- for this project will always be 6
							           -- could be changed for other applications
									   
CONSTANT RAM_WORD_SIZE: INTEGER := 16; -- fixed for this project could be changed by other applications

CONSTANT FLUSH_LATENCY: integer := 5;
CONSTANT FLUSH_LATENCY_COUNTER_SIZE: integer := integer(ceil(log2(real(FLUSH_LATENCY))));

TYPE db_2_rcd IS
RECORD -- possible type for interface from DB to RCD. Change as required
   X,Y     : std_logic_vector(VSIZE-1 DOWNTO 0);
   rcb_cmd : std_logic_vector(2 DOWNTO 0);
   startcmd: std_logic;
END RECORD;

TYPE db_rcd_data_all IS
RECORD -- possible type for complete interface. Change as required
   iout  : db_2_rcd; -- from DB to RCD
   delay : std_logic; -- from RCD to DB
END RECORD;

procedure swap_arrays (a1, a2 : inout std_logic_vector);
procedure swap_arrays (a1, a2 : inout unsigned);
procedure swap_arrays (a1, a2 : inout signed);

--------------------------------------------------------------------------

constant dbrcb_move : std_logic_vector(2 downto 0) := "000";
constant dbrcb_draw_white : std_logic_vector(2 downto 0) := "001";
constant dbrcb_draw_black : std_logic_vector(2 downto 0) := "010";
constant dbrcb_draw_invert : std_logic_vector(2 downto 0) := "011";
constant dbrcb_notused : std_logic_vector(2 downto 0) := "100";
constant dbrcb_fill_white : std_logic_vector(2 downto 0) := "101";
constant dbrcb_fill_black : std_logic_vector(2 downto 0) := "110";
constant dbrcb_fill_invert : std_logic_vector(2 downto 0) := "111";

END PACKAGE project_pack;


package body project_pack IS

	procedure swap_arrays (a1, a2 : inout std_logic_vector) is
		variable temp : std_logic_vector(a1'range);
	begin
		assert a1'length = a2'length;
		temp := a1;
		a1 := a2;
		a2 := temp;
	end procedure swap_arrays;

	procedure swap_arrays (a1, a2 : inout unsigned) is
		variable temp : unsigned(a1'range);
	begin
		assert a1'length = a2'length;
		temp := a1;
		a1 := a2;
		a2 := temp;
	end procedure swap_arrays;

	procedure swap_arrays (a1, a2 : inout signed) is
		variable temp : signed(a1'range);
	begin
		assert a1'length = a2'length;
		temp := a1;
		a1 := a2;
		a2 := temp;
	end procedure swap_arrays;

end package body project_pack;
