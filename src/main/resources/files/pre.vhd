architecture PRE of PRE is

-----------------------------------------------------------------------------
-- BASIC
-- boolean
-----------------------------------------------------------------------------

type boolean is (false,true);

-----------------------------------------------------------------------------
-- Built-in funcion
-- rising_edge
-----------------------------------------------------------------------------

function rising_edge(clk : std_ulogic) return boolean is
  begin
    return clk = '1';
  end function rising_edge;

function to_unsigned(i : integer; w : integer) return integer is
  begin
    return 0;
  end function to_unsigned;

function std_logic_vector(i : integer) return std_logic_vector is
variable tmp : std_logic_vector(i downto 0);
  begin
    return tmp;
  end function std_logic_vector;

function is_x ( s : std_logic_vector ) return boolean is
  begin
    return false;
  end;

function to_integer( v : std_logic_vector) return integer is
  begin
    return 0;
  end;

function unsigned( v: std_logic_vector) return std_logic_vector is
  begin
    return v;
  end;

-----------------------------------------------------------------------------
-- File: leon3.vhd
-- l3_cstat_type
-----------------------------------------------------------------------------

type l3_irq_in_type is record
  irl         : std_logic_vector(3 downto 0);
  rst         : std_ulogic;
  run         : std_ulogic;
  rstvec      : std_logic_vector(31 downto 12);
  iact        : std_ulogic;
  index       : std_logic_vector(3 downto 0);
  hrdrst      : std_ulogic;
end record;

type l3_irq_out_type is record
  intack      : std_ulogic;
  irl         : std_logic_vector(3 downto 0);
  pwd         : std_ulogic;
  fpen        : std_ulogic;
  idle        : std_ulogic;
end record;

type l3_debug_in_type is record
  dsuen   : std_ulogic;  -- DSU enable
  denable : std_ulogic;  -- diagnostic register access enable
  dbreak  : std_ulogic;  -- debug break-in
  step    : std_ulogic;  -- single step
  halt    : std_ulogic;  -- halt processor
  reset   : std_ulogic;  -- reset processor
  dwrite  : std_ulogic;  -- read/write
  daddr   : std_logic_vector(23 downto 2); -- diagnostic address
  ddata   : std_logic_vector(31 downto 0); -- diagnostic data
  btrapa  : std_ulogic;          -- break on IU trap
  btrape  : std_ulogic;       -- break on IU trap
  berror  : std_ulogic;       -- break on IU error mode
  bwatch  : std_ulogic;       -- break on IU watchpoint
  bsoft   : std_ulogic;       -- break on software breakpoint (TA 1)
  tenable : std_ulogic;
  timer   : std_logic_vector(30 downto 0);                                                --
end record;

type l3_cstat_type is record
  cmiss   : std_ulogic;                       -- cache miss
  tmiss   : std_ulogic;                       -- TLB miss
  chold   : std_ulogic;                       -- cache hold
  mhold   : std_ulogic;                       -- cache mmu hold
end record;

type l3_debug_out_type is record
  data    : std_logic_vector(31 downto 0);
  crdy    : std_ulogic;
  dsu     : std_ulogic;
  dsumode : std_ulogic;
  error   : std_ulogic;
  halt    : std_ulogic;
  pwd     : std_ulogic;
  idle    : std_ulogic;
  ipend   : std_ulogic;
  icnt    : std_ulogic;
  fcnt    : std_ulogic;
  optype  : std_logic_vector(5 downto 0);     -- instruction type
  bpmiss  : std_ulogic;                       -- branch predict miss
  istat   : l3_cstat_type;
  dstat   : l3_cstat_type;
  wbhold  : std_ulogic;                       -- write buffer hold
  su      : std_ulogic;                       -- supervisor state
end record;

type tracebuf_in_type is record
  addr             : std_logic_vector(11 downto 0);
  data             : std_logic_vector(255 downto 0);
  enable           : std_logic;
  write            : std_logic_vector(7 downto 0);
end record;

type tracebuf_out_type is record
  data             : std_logic_vector(255 downto 0);
end record;

type tracebuf_2p_in_type is record
  renable          : std_logic;
  raddr            : std_logic_vector(11 downto 0);
  write            : std_logic_vector(7 downto 0);
  waddr            : std_logic_vector(11 downto 0);
  data             : std_logic_vector(255 downto 0);
end record;

type tracebuf_2p_out_type is record
  data             : std_logic_vector(255 downto 0);
end record;

-----------------------------------------------------------------------------
-- File: mmuconfig.vhd
-- VA_I_U ; VA_I_D
-----------------------------------------------------------------------------

constant VA_I1_SZ : integer := 8;
constant VA_I2_SZ : integer := 6;
constant VA_I3_SZ : integer := 6;
constant VA_I_SZ  : integer := VA_I1_SZ+VA_I2_SZ+VA_I3_SZ;

constant VA_I_U   : integer := 31;
constant VA_I_D   : integer := 32-VA_I_SZ;

------------------------------------------------------------------------------------------------------------------------
-- File: libiu.vhd
--
------------------------------------------------------------------------------------------------------------------------

constant RDBITS : integer := 32;
constant IDBITS : integer := 32;

subtype cword is std_logic_vector(IDBITS-1 downto 0);
type cdatatype is array (0 to 3) of cword;

type iregfile_in_type is record
   raddr1        : std_logic_vector(9 downto 0); -- read address 1
   raddr2        : std_logic_vector(9 downto 0); -- read address 2
   waddr         : std_logic_vector(9 downto 0); -- write address
   wdata         : std_logic_vector(31 downto 0); -- write data
   ren1          : std_ulogic;                    -- read 1 enable
   ren2          : std_ulogic;                    -- read 2 enable
   wren          : std_ulogic;                    -- write enable
end record;

type iregfile_out_type is record
   data1         : std_logic_vector(RDBITS-1 downto 0); -- read data 1
   data2         : std_logic_vector(RDBITS-1 downto 0); -- read data 2
end record;

type cctrltype is record
   burst  : std_ulogic;                          -- icache burst enable
   dfrz   : std_ulogic;                          -- dcache freeze enable
   ifrz   : std_ulogic;                          -- icache freeze enable
   dsnoop : std_ulogic;                          -- data cache snooping
   dcs    : std_logic_vector(1 downto 0);        -- dcache state
   ics    : std_logic_vector(1 downto 0);        -- icache state
end record;

type icache_in_type is record
  rpc              : std_logic_vector(31 downto 0);  -- raw address (npc)
  fpc              : std_logic_vector(31 downto 0);  -- latched address (fpc)
  dpc              : std_logic_vector(31 downto 0);  -- latched address (dpc)
  rbranch          : std_ulogic;                     -- Instruction branch
  fbranch          : std_ulogic;                     -- Instruction branch
  inull            : std_ulogic;                     -- instruction nullify
  su               : std_ulogic;                     -- super-user
  flush            : std_ulogic;                     -- flush icache
  fline            : std_logic_vector(31 downto 3);  -- flush line offset
  nobpmiss         : std_ulogic;                     -- Predicted instruction, block hold
end record;

type icache_out_type is record
  data             : cdatatype;
  set              : std_logic_vector(1 downto 0);
  mexc             : std_ulogic;
  hold             : std_ulogic;
  flush            : std_ulogic;                             -- flush in progress
  diagrdy          : std_ulogic;                             -- diagnostic access ready
  diagdata         : std_logic_vector(IDBITS-1 downto 0);    -- diagnostic data
  mds              : std_ulogic;                             -- memory data strobe
  cfg              : std_logic_vector(31 downto 0);
  idle             : std_ulogic;                             -- idle mode
  cstat            : l3_cstat_type;
  bpmiss           : std_ulogic;
  eocl             : std_ulogic;
end record;

type icdiag_in_type is record
   addr             : std_logic_vector(31 downto 0); -- memory stage address
   enable           : std_ulogic;
   read             : std_ulogic;
   tag              : std_ulogic;
   ctx              : std_ulogic;
   flush            : std_ulogic;
   ilramen          : std_ulogic;
   cctrl            : cctrltype;
   pflush           : std_ulogic;
   pflushaddr       : std_logic_vector(VA_I_U downto VA_I_D);
   pflushtyp        : std_ulogic;
end record;

type dcache_in_type is record
   asi              : std_logic_vector(7 downto 0);
   maddress         : std_logic_vector(31 downto 0);
   eaddress         : std_logic_vector(31 downto 0);
   edata            : std_logic_vector(31 downto 0);
   size             : std_logic_vector(1 downto 0);
   enaddr           : std_ulogic;
   eenaddr          : std_ulogic;
   nullify          : std_ulogic;
   lock             : std_ulogic;
   read             : std_ulogic;
   write            : std_ulogic;
   flush            : std_ulogic;
   flushl           : std_ulogic;                        -- flush line
   dsuen            : std_ulogic;
   msu              : std_ulogic;                   -- memory stage supervisor
   esu              : std_ulogic;                   -- execution stage supervisor
   intack           : std_ulogic;
end record;

type dcache_out_type is record
   data             : cdatatype;
   set              : std_logic_vector(1 downto 0);
   mexc             : std_ulogic;
   hold             : std_ulogic;
   mds              : std_ulogic;
   werr             : std_ulogic;
   icdiag           : icdiag_in_type;
   cache            : std_ulogic;
   idle             : std_ulogic;                        -- idle mode
   hit              : std_ulogic;
   cstat            : l3_cstat_type;
   wbhold           : std_ulogic;
end record;

-----------------------------------------------------------------------------
-- File: arith.vhd
-- div32_in_type ; div32_out_type ; mul32_in_type ; mul32_out_type
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

type mul32_in_type is record
  op1              : std_logic_vector(32 downto 0); -- operand 1
  op2              : std_logic_vector(32 downto 0); -- operand 2
  flush            : std_logic;
  signed           : std_logic;
  start            : std_logic;
  mac              : std_logic;
  acc              : std_logic_vector(39 downto 0);
  --y                : std_logic_vector(7 downto 0); -- Y (MSB MAC register)
  --asr18           : std_logic_vector(31 downto 0); -- LSB MAC register
end record;

type mul32_out_type is record
  ready           : std_logic;
  nready          : std_logic;
  icc             : std_logic_vector(3 downto 0); -- ICC
  result          : std_logic_vector(63 downto 0); -- mul result
end record;

-----------------------------------------------------------------------------
-- File: libfpu.vhd
--
-----------------------------------------------------------------------------

type fpc_pipeline_control_type is record
  pc    : std_logic_vector(31 downto 0);
  inst  : std_logic_vector(31 downto 0);
  cnt   : std_logic_vector(1 downto 0);
  trap  : std_ulogic;
  annul : std_ulogic;
  pv    : std_ulogic;
end record;

type fpc_debug_in_type is record
  enable : std_ulogic;
  write  : std_ulogic;
  fsr    : std_ulogic;                            -- FSR access
  addr   : std_logic_vector(4 downto 0);
  data   : std_logic_vector(31 downto 0);
end record;

type fpc_debug_out_type is record
  data   : std_logic_vector(31 downto 0);
end record;

type fpc_in_type is record
  flush       : std_ulogic;                     -- pipeline flush
  exack       : std_ulogic;                     -- FP exception acknowledge
  a_rs1       : std_logic_vector(4 downto 0);
  d             : fpc_pipeline_control_type;
  a             : fpc_pipeline_control_type;
  e             : fpc_pipeline_control_type;
  m             : fpc_pipeline_control_type;
  x             : fpc_pipeline_control_type;
  lddata        : std_logic_vector(31 downto 0);     -- load data
  dbg           : fpc_debug_in_type;               -- debug signals
end record;

type fpc_out_type is record
  data          : std_logic_vector(31 downto 0); -- store data
  exc                 : std_logic;                     -- FP exception
  cc            : std_logic_vector(1 downto 0);  -- FP condition codes
  ccv                 : std_ulogic;                    -- FP condition codes valid
  ldlock        : std_logic;                   -- FP pipeline hold
  holdn          : std_ulogic;
  dbg           : fpc_debug_out_type;             -- FP debug signals
end record;

------------------------------------------------------------------------------------------------------------------------
-- File: stdlib.vhd
--
------------------------------------------------------------------------------------------------------------------------

constant zero32 : std_logic_vector(31 downto 0) := (others => '0');

type log2arr is array(0 to 512) of integer;

constant log2   : log2arr := (
0,0,1,2,2,3,3,3,3,4,4,4,4,4,4,4,4,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,
  6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
  7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
  7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
  8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
  8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
  8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
  8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
  others => 9);

constant log2x  : log2arr := (
0,1,1,2,2,3,3,3,3,4,4,4,4,4,4,4,4,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,
  6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
  7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
  7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
  8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
  8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
  8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
  8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
  others => 9);

function notx(d : std_logic_vector) return boolean is
variable res : boolean;
begin
  res := true;
-- pragma translate_off
  res := not is_x(d);
-- pragma translate_on
  return (res);
end;

function conv_integer(v : std_logic_vector) return integer is
begin
  if notx(v) then return(to_integer(unsigned(v)));
  else return(0); end if;
  return 0;
end;

function conv_std_logic_vector(i : integer; w : integer) return std_logic_vector is
variable tmp : std_logic_vector(w-1 downto 0);
begin
  tmp := std_logic_vector(to_unsigned(i, w));
  return(tmp);
end;

------------------------------------------------------------------------------------------------------------------------
-- File: gencomp.vhd
--
------------------------------------------------------------------------------------------------------------------------

constant NTECH : integer := 56;
type tech_ability_type is array (0 to NTECH) of integer;

constant is_fpga : tech_ability_type :=
	(inferred => 1, virtex => 1, virtex2 => 1, axcel => 1,
	 proasic => 1, altera => 1, apa3 => 1, spartan3 => 1,
         virtex4 => 1, lattice => 1, spartan3e => 1, virtex5 => 1,
	 stratix1 => 1, stratix2 => 1, eclipse => 1,
	 stratix3 => 1, cyclone3 => 1, axdsp => 1,
	 spartan6 => 1, virtex6 => 1, actfus => 1,
	 stratix4 => 1, apa3e => 1, apa3l => 1, virtex7 => 1, kintex7 => 1,
	 artix7 => 1, zynq7000 => 1, igloo2 => 1,
	 others => 0);

------------------------------------------------------------------------------------------------------------------------
-- File: sparc.vhd
--
------------------------------------------------------------------------------------------------------------------------

subtype trap_type is std_logic_vector(5 downto 0);

constant TT_IAEX   : trap_type := "000001";
constant TT_IINST  : trap_type := "000010";
constant TT_PRIV   : trap_type := "000011";
constant TT_FPDIS  : trap_type := "000100";
constant TT_WINOF  : trap_type := "000101";
constant TT_WINUF  : trap_type := "000110";
constant TT_UNALA  : trap_type := "000111";
constant TT_FPEXC  : trap_type := "001000";
constant TT_DAEX   : trap_type := "001001";
constant TT_TAG    : trap_type := "001010";
constant TT_WATCH  : trap_type := "001011";

-----------------------------------------------------------------------------
-- END
-----------------------------------------------------------------------------

begin
end;