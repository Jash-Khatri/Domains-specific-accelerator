// Test-bench for parameterized Domain specific accelerator for Vector Addition with dual ported memory
package tb_vectoradd;

// Import the Accelerator for Vector Addition
import vectorAddition:: *;
// Import the Package for building the FSMs
import StmtFSM :: *;

import FloatingPoint :: *;

// Test-bench Module
(* synthesize *)
module mkTest (Empty);
	
	// instantiate the accelerator for for Vector Addition
	VectorAdder_ifc v <- mkVectorAdder;

	Reg#(int) step <- mkReg(0);

	// Below registers are used to store the result obtained from memory load operation.
   	Reg#(Int#(CBDATASIZE)) x <- mkReg(0);
	Reg#(Int#(CBDATASIZE)) y <- mkReg(0);
	Reg#(Int#(CBDATASIZE)) z <- mkReg(0);
	Reg#(Int#(CBDATASIZE)) w <- mkReg(0);
	Reg#(Int#(CBDATASIZE)) q <- mkReg(0);
	
		
	// statements to initialize the memory and CPU.
	Stmt test_seq = seq

		// Memory initialization begin
		// dual ported memory so we can store 2 values at same time in parallel
		par
	   	v.mem_store(50,-100);		// initialize the memory location from 50 to 54 with values of Vector1.
	   	v.mem_store1(51,3);
		endpar    
		
		par	   	
		v.mem_store(52,2);
		v.mem_store1(53,-2000);
		endpar

		par
		v.mem_store(54,100);
	   	v.mem_store1(60,-210);		// initialize the memory location from 60 to 64 with values of Vector2.
		endpar

		par	   	
		v.mem_store(61, 340);
	   	v.mem_store1(62,-1);
		endpar

		par
	   	v.mem_store(63,800);
		v.mem_store1(64,2000);
	   	endpar
		// Memory initialization end here
      
		// Initialize the CPU begin.
		par
		v.put_src1 (50);		// give src1 to CPU
		v.put_src2 (60);		// give src2 to CPU
		v.put_dst (110);		// give dst to CPU
		v.put_bsize (5);		// give block-size to CPU
		step <= 1;	
		endpar
		// Initialize the CPU end. 
	endseq;	

	//statements to obtain the results from Dst memory location.
	Stmt test_seq1 = seq

		// loading results into the registers begin
		// dual ported memory so we can load 2 values at the same time	
		action
	   	x <= unpack(v.mem_load( 110 ));		// get the values stored from the Memory location 110-114 which contains the Vector addition result
		y <= unpack(v.mem_load1( 111 ));
		endaction		
		
		action
		z <= unpack(v.mem_load( 112 ));
		w <= unpack(v.mem_load1( 113 ));	
		endaction 
	
		q <= unpack(v.mem_load( 114 ));
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
	
	// Rule to display the fetched results as the final Vector obtained after addition.
	rule rl_print(step == 2);
	$display ("The final vector:\n [ %d %d %d %d %d]\n",x,y,z,w,q);		// print the results
	$display("\nTotal Time taken is: %d units", $time);			// display the total time taken
	$display("\n");
	$finish();
	endrule
	
endmodule

endpackage
