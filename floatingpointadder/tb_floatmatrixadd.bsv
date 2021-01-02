// Test-bench for Domain specific accelerator for Floating Point Matrix Addition with dual ported memory
package tb_floatmatrixadd;

// Import the Accelerator for floating-point Matrix Addition
import floatmatrixAddition:: *;
//import the library for supporting Floating point operations
import FloatingPoint :: *;
// Import the Package for building the FSMs
import StmtFSM :: *;
// import the tunable parameter from the file
import parameters :: *; 

	// Test-bench Module
	(* synthesize *)
	module mkTest(Empty);
	
	// instantiate the accelerator for Floating Point Matrix Addition
	MatrixAdder_ifc v <- mkMatrixAdder;

	Reg#(int) step <- mkReg(0);

	// Registers used to store the Floating Point value of the source1 Matrix 
	Reg#(Float) f11 <- mkReg(20.05);
	Reg#(Float) f12 <- mkReg(0.20);
	Reg#(Float) f13 <- mkReg(-8.5);
	Reg#(Float) f14 <- mkReg(-1.5);

	// Registers used to store the Floating Point value of the source2 Matrix 	
	Reg#(Float) f21 <- mkReg(1.70);
	Reg#(Float) f22 <- mkReg(0.30);
	Reg#(Float) f23 <- mkReg(1.75);
	Reg#(Float) f24 <- mkReg(-6.5);

	// Registers used to store the Floating Point value of the destination Matrix
	Reg#(Float) f31 <- mkReg(0);
	Reg#(Float) f32 <- mkReg(0);
	Reg#(Float) f33 <- mkReg(0);
	Reg#(Float) f34 <- mkReg(0);

	// Register used to store the exponent field in form of 8-bit Integer for display purpose 
	Reg#(Int#(8)) e1 <- mkReg(0);
	Reg#(Int#(8)) e2 <- mkReg(0);
	Reg#(Int#(8)) e3 <- mkReg(0);
	Reg#(Int#(8)) e4 <- mkReg(0);
	
	Stmt test_seq = seq
	
		// Memory initialization begin
		// dual ported memory so we can store 2 values at same time in parallel	
		par
		v.mem_store(50, zeroExtend( pack(f11) ) );	// initialize the memory location from 50 to 53 with values of Matrix1.
		v.mem_store1(51, zeroExtend( pack(f12) ) );
		endpar
	
		par		
		v.mem_store(52, zeroExtend( pack(f13) ) );
		v.mem_store1(53, zeroExtend( pack(f14) ) );
		endpar

		par
		v.mem_store(60, zeroExtend( pack(f21) ) );	// initialize the memory location from 60 to 63 with values of Matrix2.
		v.mem_store1(61, zeroExtend( pack(f22) ) );
		endpar
		par		
		v.mem_store(62, zeroExtend( pack(f23) ) );
		v.mem_store1(63, zeroExtend( pack(f24) ) );
		endpar		
		// Memory initialization end here
      
		// Initialize the CPU begin.
		par
		v.put_src1 (50);		// give src1 to CPU
		v.put_s1rows(2);		// give num-of-rows for matrix1
		v.put_s1cols(2);		// give num-of-columns for matrix1

		v.put_src2 (60);		// give src2 to CPU
		v.put_s2rows(2);		// give num-of-rows for matrix2
		v.put_s2cols(2);		// give num-of-columns for matrix2

		v.put_dst (110);		// give destination to CPU
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

		action		
		f33 <= unpack( truncate(v.mem_load( 112 )) ) ;	
		f34 <= unpack( truncate(v.mem_load1( 113 )) ) ;
		endaction
		// loading results into the registers end
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
		e4 <= unpack(f34.exp-127);
		step <= 3;
	endrule

	// Rule to display the fetched results as the final Matrix obtained after addition.
	rule rl_print(step == 3);
	
	if(!v.sizemismatch())
			begin
		$display ("\n\nRETURN SUCCESS: Operation successful\n");
		$display ("The final Matrix:\n ");

		$display ("[ (%s1.%b * (2^%d))    (%s1.%b * (2^%d))",f31.sign ? "-":"+",f31.sfd, e1,f32.sign ? "-":"+",f32.sfd, e2);			// print the results
		
		$display ("  (%s1.%b * (2^%d))    (%s1.%b * (2^%d))  ]",f33.sign ? "-":"+",f33.sfd, e3,f34.sign ? "-":"+",f34.sfd, e4);			// print the results
			end
	
	else
			begin
		$display ("\n\nRETURN FAIL: Matrix size check failed\n");		// Display FAIL in case of Matrix size mismatch
			end

	$display("\n");
	$display("\nTotal Time taken is: %d units", $time);			// display the total time taken
	$display("\n");
	$finish();
	endrule

	endmodule

endpackage

//<- mkReg(sign: True; exp: 1; sfd:32; );
