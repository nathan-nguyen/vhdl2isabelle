entity iu3 is
  generic (
    nwin    : integer range 2 to 32 := 8;
    isets   : integer range 1 to 4 := 1;
    dsets   : integer range 1 to 4 := 1;
    fpu     : integer range 0 to 15 := 0;
    v8      : integer range 0 to 63 := 0;
    cp, mac : integer range 0 to 1 := 0;
    dsu     : integer range 0 to 1 := 0;
    nwp     : integer range 0 to 4 := 0;
    pclow   : integer range 0 to 2 := 2;
    notag   : integer range 0 to 1 := 0;
    index   : integer range 0 to 15:= 0;
    lddel   : integer range 1 to 2 := 2;
    irfwt   : integer range 0 to 1 := 0;
    disas   : integer range 0 to 2 := 0;
    tbuf    : integer range 0 to 128 := 0;      -- trace buf size in kB (0 - no trace buffer)
    pwd     : integer range 0 to 2 := 0;        -- power-down
    svt     : integer range 0 to 1 := 0;        -- single-vector trapping
    --rstaddr  : integer := 16#00000#;            -- reset vector MSB address
    smp     : integer range 0 to 15 := 0;       -- support SMP systems
    fabtech : integer range 0 to NTECH := 0;
    clk2x   : integer := 0;
    bp      : integer range 0 to 2 := 1;
    npasi   : integer range 0 to 1 := 0;
    pwrpsr  : integer range 0 to 1  := 0
  );
  port (
    clk     : in  std_ulogic;
    rstn    : in  std_ulogic;
    holdn   : in  std_ulogic;
    ici     : out icache_in_type;
    ico     : in  icache_out_type
  );
end;

architecture rtl of iu3 is
  constant CONST_CST : integer := IDBITS;
  --constant CONST_IDBITS : integer := CONST_CST - 1;
  constant CONST_CST_BOOLEAN : boolean := (IDBITS - 1) = 31;

  --constant IDBITS : integer := 32;
  --subtype cword is std_logic_vector(IDBITS-1 downto 0);
  --type cdatatype is array (0 to 3) of cword;

  --type abc is record
  --  set : std_logic_vector(IDBITS downto 0);
  --end record;

begin
end;