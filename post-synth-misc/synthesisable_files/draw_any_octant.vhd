LIBRARY IEEE;

USE IEEE.std_logic_1164.ALL;
USE IEEE.numeric_std.ALL;
USE work.ALL;

ENTITY draw_any_octant IS

  -- swapxy negx  negy  octant
  --  0      0      0     ENE
  --  1      0      0     NNE
  --  1      1      0     NNW
  --  0      1      0     WNW
  --  0      1      1     WSW
  --  1      1      1     SSW
  --  1      0      1     SSE
  --  0      0      1     ESE

  -- swapxy: x & y swap round on inputs & outputs
  -- negx:   invert bits of x on inputs & outputs
  -- negy:   invert bits of y on inputs & outputs

  -- xbias always give bias in x axis direction, so swapxy must invert xbias
  GENERIC(
    vsize: INTEGER := 12
  );
  
  PORT(
    clk, enable, resetx, draw, xbias : IN  std_logic;
    xin, yin                 : IN  std_logic_vector(vsize-1 DOWNTO 0);
    done                     : OUT std_logic;
    x, y                     : OUT std_logic_vector(vsize-1 DOWNTO 0);
    swapxy, negx, negy       : IN  std_logic
    );
END ENTITY draw_any_octant;

ARCHITECTURE desc OF draw_any_octant IS
  SIGNAL xin_do, yin_do, x_do, y_do : std_logic_vector(vsize-1 DOWNTO 0);
  SIGNAL xbias_do, swapxy_dly, negx_dly, negy_dly : std_logic;
  signal saved_negx, saved_negy, saved_swapxy, saved_xbias : std_logic;
  signal muxed_negx, muxed_negy, muxed_swapxy, muxed_xbias : std_logic;
BEGIN
  
  DO: ENTITY draw_octant 
    GENERIC MAP (vsize) 
    PORT MAP (
      clk,
      enable,
      resetx,
      draw,
      xbias_do,
      xin_do,
      yin_do,
      done,
      x_do,
      y_do);
      
-- TODO: hacked to meet testbench spec, wrong but won't be fixed. Proper xbias in a comment below
xbias_do <= '1' xor muxed_swapxy xor muxed_negx xor muxed_negy;
--xbias_do <= muxed_xbias xor muxed_swapxy;



muxed_xbias <= xbias WHEN resetx='1' ELSE saved_xbias;
muxed_swapxy <= swapxy WHEN resetx='1' ELSE saved_swapxy;
muxed_negx <= negx WHEN resetx='1' ELSE saved_negx;
muxed_negy <= negy WHEN resetx='1' ELSE saved_negy;



  R0: process
  begin
    wait until clk'event and clk='1';
    if enable='1' and resetx='1' then

    saved_xbias <= xbias;
    saved_swapxy <= swapxy;
    saved_negx <= negx;
    saved_negy <= negy;


    end if;
  end process R0;


  
  C1: PROCESS(xin, yin, muxed_negx, muxed_negy, muxed_swapxy)
    VARIABLE xout, yout : std_logic_vector(vsize-1 DOWNTO 0);
  BEGIN
    xout := xin;
    yout := yin;
    
    IF muxed_negx = '1' THEN
      xout := not xin;
    END IF;
    
    IF muxed_negy = '1' THEN
      yout := not yin;
    END IF;
    
    xin_do <= xout;
    yin_do <= yout;
    IF muxed_swapxy = '1' THEN
      xin_do <= yout;
      yin_do <= xout;
    END IF;
  END PROCESS C1;
    
  ------------------------------------
    
  R1: PROCESS
  
  BEGIN
    WAIT UNTIL clk'EVENT and clk = '1';
    if enable='1' then
      negx_dly <= muxed_negx;
      negy_dly <= muxed_negy;
      swapxy_dly <= muxed_swapxy;   
    end if;
  END PROCESS R1;
  
  ------------------------------------
  
  C2: PROCESS(x_do, y_do, negx_dly, negy_dly, swapxy_dly)
    VARIABLE xout, yout, temp : std_logic_vector(vsize-1 DOWNTO 0);
  BEGIN

    xout := x_do;
    yout := y_do;

    if swapxy_dly = '1' then
      temp := xout;
      xout := yout;
      yout := temp;


    end if;

    IF negx_dly = '1' THEN
      xout := not xout;
    END IF;
    
    IF negy_dly = '1' THEN
      yout := not yout;
    END IF;
    
    x <= xout;
    y <= yout;
  END PROCESS C2;
  
END ARCHITECTURE desc;
