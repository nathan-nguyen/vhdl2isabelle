package parsing

import org.antlr.v4.runtime.{ANTLRInputStream, CommonTokenStream}
import sg.edu.ntu.hchen.{VHDLLexer, VHDLParser}

class ParserSpec extends BaseSpec {

  def pFromStr(s: String): VHDLParser = {
    val lexer = new VHDLLexer(new ANTLRInputStream(s))
    val tokens = new CommonTokenStream(lexer)
    val parser = new VHDLParser(tokens)
    parser.removeErrorListeners()
    parser.addErrorListener(new PErrorListener)
    parser
  }

  "type-declaration" should "parse" in {
    val s1 =
      """
        |type Weight is range 0 to 10_000
        |units
        |gr;
        |kg = 1000 gr;
        |ton = 1000 kg;
        |end units;
      """.stripMargin
    pFromStr(s1).type_declaration()
    val s2 =
      """
        |TYPE StateMachine IS (start,count,steady);
      """.stripMargin
    pFromStr(s2).type_declaration()

    val s4 =
      """
        |TYPE Matrix IS ARRAY (0 TO 1, 0 TO 1) OF BIT;
      """.stripMargin
    pFromStr(s4).type_declaration()

  }

  "record-type-declaration" should "parse" in {
    val s1 =
      """
        |TYPE Clock IS RECORD
        |Hour : INTEGER RANGE 0 TO 23;
        |Min : INTEGER RANGE 0 TO 59;
        |Sec : INTEGER RANGE 0 TO 59;
        |END RECORD Clock;
      """.stripMargin
    pFromStr(s1).type_declaration()
  }

  "access-type-declaration" should "parse" in {
    val s1 =
      """
        |TYPE ListElement; -- Incomplete type declaration
        |TYPE ListPointer IS ACCESS ListElement;
        |TYPE ListElement IS RECORD
        |Data : INTEGER RANGE 0 TO 31;
        |NextPoint : ListPointer;
        |END RECORD ListElement;
      """.stripMargin
    pFromStr(s1).type_declaration()
  }

  "group-template-declaration" should "parse" in {
    val s1 =
      """
        |ENTITY Mux IS
        |PORT(a, b, c : IN STD_ULOGIC;
        |choose : IN STD_ULOGIC_VECTOR(1 DOWNTO 0);
        |q : OUT STD_ULOGIC);
        |END ENTITY Mux;
        |ARCHITECTURE Behave OF Mux IS
        |GROUP Ports IS (SIGNAL <>); -- Create a group template
        |GROUP InPorts : Ports (a,b,c);-- Create a group of the template
        |GROUP OutPort : Ports (q); -- Create another group
        |GROUP InToOut IS (GROUP,GROUP); -- A 2-dim group template
        |GROUP Timing : InToOut (InPorts,OutPort); -- The final group
        |ATTRIBUTE synthesis_maxdelay : TIME; -- Use the groups
        |ATTRIBUTE synthesis_maxdelay OF Timing : GROUP IS 9 ns;
        |BEGIN
        |PROCESS(a,b,c,choose)
        |BEGIN
        |CASE choose IS
        |WHEN ”00” => q <= a;
        |WHEN ”01” => q <= b;
        |WHEN ”10” => q <= c;
        |WHEN OTHERS => NULL;
        |END CASE;
        |END PROCESS;
        |END ARCHITECTURE Behave;
      """.stripMargin
    pFromStr(s1).entity_declaration()
  }

  "attribute-declaration attribute-specification" should "parse" in {
    val s1 =
      """
        |ATTRIBUTE syn_encoding : STRING;
      """.stripMargin
    pFromStr(s1).attribute_declaration()
  }

  "constant-declaration" should "parse" in {
    val s1 =
      """
        |CONSTANT bits : BIT_VECTOR := x"0FA3";
      """.stripMargin
    pFromStr(s1).constant_declaration()

    val s2 =
      """
        |CONSTANT zero : STD_LOGIC_VECTOR(0 TO 3) := (OTHERS => '0');
      """.stripMargin

    pFromStr(s2).constant_declaration()

    val s4 =
      """
        |constant RESET_ALL : boolean := GRLIB_CONFIG_ARRAY(grlib_sync_reset_enable_all) = 1;
      """.stripMargin
    pFromStr(s4).constant_declaration()
  }

  "variable-declaration" should "parse" in {
    val s1 =
      """
        |VARIABLE internal : REAL := 0.0;
      """.stripMargin
    pFromStr(s1).variable_declaration()
  }

  "signal-declaration" should "parse" in {
    val s1 =
      """
        |SIGNAL data : STD_LOGIC_VECTOR(3 DOWNTO 0);
      """.stripMargin
    pFromStr(s1).signal_declaration()
  }

  "attribute-declaration" should "parse" in {
    val s1 =
      """
        |	attribute foreign : string;
      """.stripMargin

    pFromStr(s1).attribute_declaration()
  }

}
