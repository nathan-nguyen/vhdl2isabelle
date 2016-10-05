architecture PRE of PRE is

-----------------------------------------------------------------------------
-- BASIC
-----------------------------------------------------------------------------

type boolean is (false,true);

-----------------------------------------------------------------------------
-- File: arith.vhd
-- div32_in_type ; div32_out_type
-----------------------------------------------------------------------------

type div32_in_type is record
  y                : std_logic_vector(32 downto 0); -- Y (MSB divident)
  op1              : std_logic_vector(32 downto 0); -- operand 1 (LSB divident)
  op2              : std_logic_vector(32 downto 0); -- operand 2 (divisor)
  flush            : std_logic;
  signed           : std_logic;
  start            : std_logic;
end record;

type div32_out_type is record
  ready           : std_logic;
  nready          : std_logic;
  icc             : std_logic_vector(3 downto 0);  -- ICC
  result          : std_logic_vector(31 downto 0); -- div result
end record;

begin
end;