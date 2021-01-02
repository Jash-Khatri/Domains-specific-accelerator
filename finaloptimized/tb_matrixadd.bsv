package tb_matrixadd;

import matrixAddition:: *;
import StmtFSM :: *;
import parameters :: *; // import the tunable parameter from the file

(* synthesize *)
module mkTest (Empty);
	
	
	MatrixAdder_ifc v <- mkMatrixAdder;
	Reg#(int) step <- mkReg(0);

   	Reg#(Int#(CBDATASIZE)) x <- mkReg(0);
	Reg#(Int#(CBDATASIZE)) y <- mkReg(0);
	Reg#(Int#(CBDATASIZE)) z <- mkReg(0);
	Reg#(Int#(CBDATASIZE)) w <- mkReg(0);
		
	// rule to initailze the memory

	// rule to initailze the memory
	Stmt test_seq = seq

		// dual ported memory so we can store 2 values at same time
		par
	   	v.mem_store(80,200);		// initialize the mem where matrix1 is present
	   	v.mem_store1(81,300);
		endpar

		par    	   	
		v.mem_store(82,-50);
		v.mem_store1(83,-200);
		endpar

		par
	   	v.mem_store(90, -510);		// initialize the mem where matrix2 is present
	   	v.mem_store1(91, 110);
		endpar

		par
	   	v.mem_store(92,-1);
	   	v.mem_store1(93,100);
	   	endpar
      
		// run the accelerator..
		par
		v.put_src1 (80);		// init src1
		v.put_s1rows(2);
		v.put_s1cols(2);

		v.put_src2 (90);		// init src2
		v.put_s2rows(2);
		v.put_s2cols(2);

		v.put_dst (100);
		step <= 1;	
		endpar
	endseq;	
		
	Stmt test_seq1 = seq
		// dual ported memory so we can load 2 values at the same time	
		action
	   	x <= unpack(v.mem_load( 100 ));		// get the values stored from the dst 
		y <= unpack(v.mem_load1( 101 ));
		endaction

		action		
		z <= unpack(v.mem_load( 102 ));
		w <= unpack(v.mem_load1( 103 ));
		endaction		
		step <= 2;

	endseq;	


	FSM testfsm <- mkFSM (test_seq);
	FSM testfsm1 <- mkFSM (test_seq1);

	rule run1 ( True );
		testfsm.start ;
	endrule

	rule run2 (v.iscomplete() && step == 1  );								
		testfsm1.start ;
	endrule
	
	rule rl_print(step == 2);
		
		if(!v.sizemismatch())
			begin
			$display ("\n\nRETURN SUCCESS: Operation successful\n");
			$display ("The final matrix:\n [ %d %d \n",x,y);
			$display (" %d %d ]\n",z,w);
			end		
		else
			begin
			$display ("\n\nRETURN FAIL: Matrix size check failed\n");
			end
	
		$display("\nTotal Time taken is: %d units", $time);
		$display("\n");
		$finish();
	endrule

endmodule

endpackage

