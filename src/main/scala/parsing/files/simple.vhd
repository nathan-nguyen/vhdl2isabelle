library ieee;
use ieee.std_logic_1164.all;
library grlib;
use grlib.config_types.all;
use grlib.config.all;
use grlib.stdlib.all;
library gaisler;
use gaisler.arith.all;

entity div32 is
generic (scantest  : integer := 0);
port (
    rst     : in  std_ulogic;
    clk     : in  std_ulogic;
    holdn   : in  std_ulogic;
    divi    : in  div32_in_type;
    divo    : out div32_out_type;
    testen  : in  std_ulogic := '0';
    testrst : in  std_ulogic := '1'
);
end;
