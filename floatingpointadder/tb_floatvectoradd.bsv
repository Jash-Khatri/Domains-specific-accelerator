package simplefloatadd;

import FloatingPoint :: *;
import StmtFSM :: *;
// Import the Accelerator for Vector Addition
import vectorAddition:: *;

	(* synthesize *)
	module mkTest(Empty);
	
	VectorAdder_ifc v <- mkVectorAdder;
	Reg#(int) step <- mkReg(0);

	Reg#(Float) f11 <- mkReg(20.05);
	Reg#(Float) f12 <- mkReg(0.20);
	Reg#(Float) f13 <- mkReg(-8.5);
	
	Reg#(Float) f21 <- mkReg(1.70);
	Reg#(Float) f22 <- mkReg(0.05);
	Reg#(Float) f23 <- mkReg(1.75);

	Reg#(Float) f31 <- mkReg(0);
	Reg#(Float) f32 <- mkReg(0);
	Reg#(Float) f33 <- mkReg(0);

	Reg#(Int#(8)) e1 <- mkReg(0);
	Reg#(Int#(8)) e2 <- mkReg(0);
	Reg#(Int#(8)) e3 <- mkReg(0);
	
	Stmt test_seq = seq
	
		// specifying the floating point addition as the type of addition
		v.spectype(1);

		// Memory initialization begin
		par
		v.mem_store(50, zeroExtend( pack(f11) ) );
		v.mem_store1(51, zeroExtend( pack(f12) ) );
		endpar
	
		par		
		v.mem_store(52, zeroExtend( pack(f13) ) );
		v.mem_store1(60, zeroExtend( pack(f21) ) );
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
		action
		f31 <= unpack( truncate(v.mem_load( 110 )) ) ;
		f32 <= unpack( truncate(v.mem_load1( 111 )) ) ;	
		endaction

		action		
		f33 <= unpack( truncate(v.mem_load( 112 )) ) ;
		step <= 2;
		endaction
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

	// Rule to calcuate the ture exponent from the biased exponent for the result display purpose..
	rule calc_exp(step == 2);
		e1 <= unpack(f31.exp-127);
		e2 <= unpack(f32.exp-127);
		e3 <= unpack(f33.exp-127);
		step <= 3;
	endrule

	// Rule to display the fetched results as the final Vector obtained after addition.
	rule rl_print(step == 3);
	$display ("The final vector:\n ");

	if(f31.sign == False)
	$display ("[ +1.%b * 2^%d ",f31.sfd, e1);			// print the results
	else	
	$display("[ -1.%b * 2^%d ",f31.sfd,e1);	

	if(f32.sign == False)
	$display (" +1.%b * 2^%d ",f32.sfd, e2);			// print the results
	else	
	$display(" -1.%b * 2^%d ",f32.sfd, e2);	

	if(f33.sign == False)
	$display (" +1.%b * 2^%d ] ",f33.sfd, e3);			// print the results
	else	
	$display(" -1.%b * 2^%d ] ",f33.sfd, e3);	



	$display("\n");
	$display("\nTotal Time taken is: %d units", $time);			// display the total time taken
	$display("\n");
	$finish();
	endrule

	endmodule

endpackage

//<- mkReg(sign: True; exp: 1; sfd:32; );
