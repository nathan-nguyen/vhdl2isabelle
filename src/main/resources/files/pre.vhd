architecture PRE of PRE is

-----------------------------------------------------------------------------
-- BASIC
-----------------------------------------------------------------------------

type boolean is (false,true);

-----------------------------------------------------------------------------
-- File: libiu.vhd
-- IDBITS ; cword ; cdatatype ; icache_in_type ; icache_out_type
-----------------------------------------------------------------------------

constant IDBITS : integer := 32;

--subtype cword is std_logic_vector(IDBITS-1 downto 0);
--type cdatatype is array (0 to 3) of cword;

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
     --data             : cdatatype;
     set              : std_logic_vector(1 downto 0);
     mexc             : std_ulogic;
     hold             : std_ulogic;
     flush            : std_ulogic;                             -- flush in progress
     diagrdy          : std_ulogic;                             -- diagnostic access ready
     diagdata         : std_logic_vector(IDBITS-1 downto 0);    -- diagnostic data
     mds              : std_ulogic;                             -- memory data strobe
     cfg              : std_logic_vector(31 downto 0);
     idle             : std_ulogic;                             -- idle mode
     --cstat            : l3_cstat_type;
     bpmiss           : std_ulogic;
     eocl             : std_ulogic;
  end record;

  type type1 is record
    r1 : std_logic_vector(31 downto 0);
    r2 : std_logic_vector(0 to 10);
  end record;

  type type2 is record
    r3 : type1;
    r4 : std_logic_vector(10 downto 1);
  end record;
-----------------------------------------------------------------------------
-- END
-----------------------------------------------------------------------------

begin
end;