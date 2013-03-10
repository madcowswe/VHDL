library ieee ;
	use ieee.std_logic_1164.all ;
	use ieee.numeric_std.all ;
library work;
	use work.project_pack.all;
	use work.pix_cache_pak.all;
	

entity read_modify_write is
  	port (
		clk, reset, start 	: IN std_logic;
		delay, vwrite 		: OUT std_logic;
		store 				: IN store_t;
		storetag			: IN unsigned; -- open port, exact size depends on VSIZE
		vdin   : OUT STD_LOGIC_VECTOR(RAM_WORD_SIZE-1 DOWNTO 0);
    	vdout  : IN  STD_LOGIC_VECTOR(RAM_WORD_SIZE-1 DOWNTO 0);
   		vaddr  : OUT STD_LOGIC_VECTOR -- open port, exact size depends on VSIZE

  	) ;
end entity read_modify_write;

architecture arch of read_modify_write is
	signal delay1 : std_logic;
	signal localstore : store_t;
begin

	-- FSM provides control signals for driving the vram properly
	RAMFSM: entity work.ram_fsm port map(
		clk => clk,
		reset => reset,
		start => start,
		delay => delay1,
		vwrite => vwrite
	);

	delay <= delay1;

	R1 : process
	begin
		WAIT UNTIL clk'event AND clk = '1';

		--sample and hold store tag = vaddr on start of operation
		if start = '1' and delay1 = '0' then
			localstore <= store;
			vaddr <= std_logic_vector(storetag);
		end if ;
		
	end process R1;

	C1 : process (vdout, localstore)
	begin

		combinationloop : for i in vdin'range loop
			
			case( localstore(i) ) is
				when same => vdin(i) <= vdout(i);
				when black => vdin(i) <= '1';
				when white => vdin(i) <= '0';
				when invert => vdin(i) <= not vdout(i);
				when others => NULL;
			end case ;

		end loop ; -- combinationloop

	end process C1;

end architecture arch;