LIBRARY IEEE;
LIBRARY WORK;
USE IEEE.std_logic_1164.ALL;
USE IEEE.numeric_std.ALL;

USE WORK.config_pack.ALL;        
USE WORK.vdp_pack.ALL;
USE WORK.project_pack.ALL;


ENTITY rcb_behav IS
  GENERIC( speed : integer := 0);

  PORT(
    clk, reset          : IN  std_logic;

    -- bus to DB
    dbb                 : IN db_2_rcd;
    dbb_delaycmd        : OUT std_logic;

    -- signal to testbench
    rcb_finish             : OUT std_logic;
    -- bus to VRAM
    vdin   : OUT STD_LOGIC_VECTOR(RAM_WORD_SIZE-1 DOWNTO 0);
    vdout  : IN  STD_LOGIC_VECTOR(RAM_WORD_SIZE-1 DOWNTO 0);
    vaddr  : OUT STD_LOGIC_VECTOR; -- open port, exact size depends on VSIZE
    vwrite : OUT STD_LOGIC
    );

END rcb_behav;


ARCHITECTURE behav OF rcb_behav IS

  SIGNAL ready : std_logic;



  SUBTYPE  xyutype IS unsigned(dbb.x'range);

  FUNCTION an(x : xyutype; y : xyutype) RETURN integer IS
  BEGIN
    RETURN (to_integer(x)/ 4) + 16 * (to_integer(y)/4);
  END;

  FUNCTION pn(x : xyutype; y : xyutype) RETURN integer IS
  BEGIN
    RETURN (to_integer(x) MOD 4) + 4 * (to_integer(y) MOD 4);
  END;
  
  FUNCTION an(x : integer; y : integer) RETURN integer IS
  BEGIN
    RETURN (x/ 4) + 16 * (y/4);
  END;

  FUNCTION pn(x : integer; y : integer) RETURN integer IS
  BEGIN
    RETURN (x MOD 4) + 4 * (y MOD 4);
  END;


BEGIN

  vdin <= (OTHERS=>'0');
  vaddr <= slv(to_unsigned(0, vaddr'LENGTH));
  vwrite <= '0';

  dbb_delaycmd <= dbb.startcmd AND NOT ready;
  rcb_finish <= ready and not dbb.startcmd and not reset;

  ASSERT speed>= 0 AND
    speed <= 3
    REPORT "Bad value for readysel"
    SEVERITY failure;

  
  R1 : PROCESS
    VARIABLE oldx, oldy, newx,
      newy, xu, yu : xyutype;
    VARIABLE n,a,p  : integer;
    VARIABLE un : unsigned(3 DOWNTO 0);
    VARIABLE b: std_logic;
  BEGIN
    WAIT UNTIL clk'event AND clk = '1';
    xu := unsigned(dbb.x);
    yu := unsigned(dbb.y);
    a        := an(xu, yu);
    p        := pn(xu, yu);

    IF reset = '1' THEN
      n := 0; ready <= '0';
    END IF;
    n     := (n + 1) MOD 16;
    un    := to_unsigned(n, 4);
    ready <= '1';
    CASE speed IS
        WHEN 0 => NULL;
        WHEN OTHERS => ready <= std_logic(un(speed));
    END CASE;
    IF dbb.startcmd = '1' and ready = '1' THEN
      CASE dbb.rcb_cmd IS
        WHEN "100"             => NULL;
        WHEN "001"|"010"       => ram_data(a)(p) := dbb.rcb_cmd(1);
        WHEN "011"             => ram_data(a)(p) := NOT ram_data(a)(p);
        WHEN "000"             => oldx           := xu; oldy := yu;
        WHEN "101"|"110"|"111" =>
          newx := xu; newy := yu;
          IF xu < oldx THEN
            newx:= oldx;
            oldx:= xu;
          END IF;
          IF yu < oldy THEN
            newy := oldy;
            oldy := yu;
          END IF;
          FOR j IN to_integer(newy) DOWNTO to_integer(oldy) LOOP
            FOR i IN to_integer(newx) DOWNTO to_integer(oldx) LOOP
              IF dbb.rcb_cmd(1 DOWNTO 0) = "11"
              THEN
                b := NOT ram_data(an(i, j))(pn(i, j));
              ELSE
                b := dbb.rcb_cmd(1);
              END IF;
              ram_data(an(i, j))(pn(i, j)) := b;
            END LOOP;
          END LOOP;
        WHEN OTHERS => NULL;
      END CASE;
    END IF;
  END PROCESS R1;
  
END ARCHITECTURE behav;








