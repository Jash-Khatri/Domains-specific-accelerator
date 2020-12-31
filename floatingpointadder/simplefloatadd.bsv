package simplefloatadd;

import FloatingPoint :: *;
import StmtFSM :: *;
import FloatVectorAdd :: *;

	(* synthesize *)
	module mkadd(Empty);
	
	VectorAdder_ifc v <- mkVectorAdder;
	Reg#(int) step <- mkReg(0);

	Float f11;
	f11.sign = False;
	f11.exp = 131;
	f11.sfd = 2883584;

	Float f12;
	f12.sign = False;
	f12.exp = 1;
	f12.sfd = 2;

	Float f21;
	f21.sign = False;
	f21.exp = 131;
	f21.sfd = 2883584;

	Float f22;
	f22.sign = False;
	f22.exp = 1;
	f22.sfd = 12;
	
	Reg#(Float) f31 <- mkReg(0);
	Reg#(Float) f32 <- mkReg(0);
	
	Stmt test_seq = seq

		// Memory initialization begin
		v.mem_store(50, zeroExtend( pack(f11) ) );
		v.mem_store(51, zeroExtend( pack(f12) ) );
		
		v.mem_store(60, zeroExtend( pack(f21) ) );
		v.mem_store(61, zeroExtend( pack(f22) ) );
		// Memory initialization end here
      
		// Initialize the CPU begin.
		par
		v.put_src1 (50);		// give src1 to CPU
		v.put_src2 (60);		// give src2 to CPU
		v.put_dst (110);		// give dst to CPU
		v.put_bsize (2);		// give block-size to CPU
		step <= 1;	
		endpar
		// Initialize the CPU end. 
	endseq;	

	Stmt test_seq1 = seq

		// loading results into the registers begin
		f31 <= unpack( truncate(v.mem_load( 110 )) ) ;
		f32 <= unpack( truncate(v.mem_load( 111 )) ) ;
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

	// Rule to display the fetched results as the final Vector obtained after addition.
	rule rl_print(step == 2);
	$display ("The final vector:\n ");

	if(f31.sign == False)
	$display ("[ +%d*2^%d ",f31.sfd,f31.exp);			// print the results
	else	
	$display("[ -%d*2^%d ",f31.sfd,f31.exp);	

	if(f32.sign == False)
	$display (" +%d*2^%d ] ",f32.sfd,f32.exp);			// print the results
	else	
	$display(" -%d*2^%d ] ",f32.sfd,f32.exp);	


	$display("\n");
	$display("\nTotal Time taken is: %d units", $time);			// display the total time taken
	$display("\n");
	$finish();
	endrule

	endmodule

endpackage

//<- mkReg(sign: True; exp: 1; sfd:32; );
