// Test-bench for parameterized Domain specific accelerator for Integer Matrix Addition with dual ported memory
package tb_matrixadd;

// Import the Accelerator for Integer Matrix Addition
import matrixAddition:: *;
// Import the Package for building the FSMs
import StmtFSM :: *;
// import the tunable parameter from the file
import parameters :: *; 

// Test-bench Module
(* synthesize *)
module mkTest (Empty);
	
	// instantiate the accelerator for Integer Matrix Addition
	MatrixAdder_ifc v <- mkMatrixAdder;
	Reg#(int) step <- mkReg(0);

	// Below registers are used to store the result obtained from memory load operation.
   	Reg#(Int#(CBDATASIZE)) x <- mkReg(0);
	Reg#(Int#(CBDATASIZE)) y <- mkReg(0);
	Reg#(Int#(CBDATASIZE)) z <- mkReg(0);
	Reg#(Int#(CBDATASIZE)) w <- mkReg(0);
		
	// statements to initialize the memory and CPU.
	Stmt test_seq = seq

		// Memory initialization begin
		// dual ported memory so we can store 2 values at same time
		par
	   	v.mem_store(80,200);		// initialize the memory location from 80 to 83 where matrix1 is present
	   	v.mem_store1(81,300);
		endpar

		par    	   	
		v.mem_store(82,-50);
		v.mem_store1(83,-200);
		endpar

		par
	   	v.mem_store(90, -510);		// initialize the memory location from 90 to 93 where matrix2 is present
	   	v.mem_store1(91, 110);
		endpar

		par
	   	v.mem_store(92,-1);
	   	v.mem_store1(93,100);
	   	endpar
		// Memory initialization end here
      
		// Initialize the CPU begin.
		par
		v.put_src1 (80);		// give src1 to CPU
		v.put_s1rows(2);		// give num-of-rows for matrix1
		v.put_s1cols(2);		// give num-of-columns for matrix1

		v.put_src2 (90);		// give src2 to CPU
		v.put_s2rows(2);		// give num-of-rows for matrix2
		v.put_s2cols(2);		// give num-of-columns for matrix2

		v.put_dst (100);		// give destination to CPU
		step <= 1;	
		endpar
		// Initialize the CPU end. 
	endseq;	
		
	Stmt test_seq1 = seq
		// loading results into the registers begin
		// dual ported memory so we can load 2 values at the same time	
		action
	   	x <= unpack(v.mem_load( 100 ));		// get the values stored from the Memory location 100-103 which contains the Matrix addition result in Row Major Order
		y <= unpack(v.mem_load1( 101 ));
		endaction

		action		
		z <= unpack(v.mem_load( 102 ));
		w <= unpack(v.mem_load1( 103 ));
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
	
	// Rule to display the fetched results as the final Matrix obtained after addition. In case of Matrix-size mismatch print FAIL.
	rule rl_print(step == 2);
		
		if(!v.sizemismatch())
			begin
			$display ("\n\nRETURN SUCCESS: Operation successful\n");	// print the results
			$display ("The final matrix:\n [ %d %d \n",x,y);
			$display (" %d %d ]\n",z,w);
			end		
		else
			begin
			$display ("\n\nRETURN FAIL: Matrix size check failed\n");	// Display FAIL in case of Matrix size mismatch
			end
	
		$display("\nTotal Time taken is: %d units", $time);		// display the total time taken
		$display("\n");
		$finish();
	endrule

endmodule

endpackage

