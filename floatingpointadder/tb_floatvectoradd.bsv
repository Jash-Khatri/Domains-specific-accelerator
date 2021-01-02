// Test-bench for Domain specific accelerator for Floating Point Vector Addition with dual ported memory
package tb_floatvectoradd;

//import the library for supporting Floating point operations
import FloatingPoint :: *;
// Import the Package for building the FSMs
import StmtFSM :: *;
// import the tunable parameter from the file
import parameters :: *;
// Import the Accelerator for Floating point Vector Addition
import floatvectorAddition:: *;

	// Test-bench Module
	(* synthesize *)
	module mkTest(Empty);
	
	// instantiate the accelerator for Floating Point Vector Addition
	VectorAdder_ifc v <- mkVectorAdder;

	Reg#(int) step <- mkReg(0);

	// Registers used to store the Floating Point value of the source1 Vector 
	Reg#(Float) f11 <- mkReg(20.05);
	Reg#(Float) f12 <- mkReg(0.20);
	Reg#(Float) f13 <- mkReg(-8.5);
	
	// Registers used to store the Floating Point value of the source2 Vector 
	Reg#(Float) f21 <- mkReg(-12.55);
	Reg#(Float) f22 <- mkReg(0.05);
	Reg#(Float) f23 <- mkReg(1.75);

	// Registers used to store the Floating Point value of the destination Vector
	Reg#(Float) f31 <- mkReg(0);
	Reg#(Float) f32 <- mkReg(0);
	Reg#(Float) f33 <- mkReg(0);

	// Register used to store the exponent field in form of 8-bit Integer for display purpose 
	Reg#(Int#(8)) e1 <- mkReg(0);
	Reg#(Int#(8)) e2 <- mkReg(0);
	Reg#(Int#(8)) e3 <- mkReg(0);
	
	Stmt test_seq = seq

		// Memory initialization begin
		// dual ported memory so we can store 2 values at same time in parallel
		par
		v.mem_store(50, zeroExtend( pack(f11) ) );	// initialize the memory location from 50 to 52 with values of Vector1.
		v.mem_store1(51, zeroExtend( pack(f12) ) );
		endpar
	
		par		
		v.mem_store(52, zeroExtend( pack(f13) ) );
		v.mem_store1(60, zeroExtend( pack(f21) ) );	// initialize the memory location from 60 to 62 with values of Vector2.
		endpar

		par		
		v.mem_store(61, zeroExtend( pack(f22) ) );
		v.mem_store1(62, zeroExtend( pack(f23) ) );
		endpar		
		// Memory initialization end here
      
		// Initialize the CPU begin.
		par
		v.put_src1 (50);		// give src1 to CPU
		v.put_src2 (60);		// give src2 to CPU
		v.put_dst (110);		// give dst to CPU
		v.put_bsize (3);		// give block-size to CPU
		step <= 1;	
		endpar
		// Initialize the CPU end. 
	endseq;	

	Stmt test_seq1 = seq

		// loading results into the registers begin
		// dual ported memory so we can load 2 values at the same time	
		action
		f31 <= unpack( truncate(v.mem_load( 110 )) ) ;
		f32 <= unpack( truncate(v.mem_load1( 111 )) ) ;	
		endaction
		
		f33 <= unpack( truncate(v.mem_load( 112 )) ) ;
		step <= 2;
		
	endseq;	

	// Generate to FSMs one for initialization part and other for fetching results
	FSM testfsm <- mkFSM (test_seq);
	FSM testfsm1 <- mkFSM (test_seq1);

	// Run the FSM which performs the initialization
	rule run1 ( True );
		testfsm.start ;
	endrule

	// Run the FSM which fetches the results
	rule run2 (v.iscomplete() && step == 1  );								
		testfsm1.start ;
	endrule

	// Rule to calcuate the true exponent from the biased exponent for the result display purpose..
	rule calc_exp(step == 2);
		e1 <= unpack(f31.exp-127);
		e2 <= unpack(f32.exp-127);
		e3 <= unpack(f33.exp-127);
		step <= 3;
	endrule

	// Rule to display the fetched results as the final Vector obtained after addition.
	rule rl_print(step == 3);
	$display ("The final vector:\n ");

	$display ("[ (%s1.%b * (2^%d))  ",f31.sign ? "-":"+",f31.sfd, e1);			// print the results	
	$display (" (%s1.%b * (2^%d))  ",f32.sign ? "-":"+",f32.sfd, e2);			// print the results
	$display (" (%s1.%b * (2^%d))  ]",f33.sign ? "-":"+",f33.sfd, e3);			// print the results

	$display("\n");
	$display("\nTotal Time taken is: %d units", $time);			// display the total time taken
	$display("\n");
	$finish();
	endrule

	endmodule

endpackage

//<- mkReg(sign: True; exp: 1; sfd:32; );
