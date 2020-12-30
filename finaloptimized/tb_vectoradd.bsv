package tb_vectoradd;

import vectorAddition:: *;
import StmtFSM :: *;
import FloatingPoint :: *;

(* synthesize *)
module mkTest (Empty);
	
	
	VectorAdder_ifc v <- mkVectorAdder;
	Reg#(int) step <- mkReg(0);

   	Reg#(Int#(CBDATASIZE)) x <- mkReg(0);
	Reg#(Int#(CBDATASIZE)) y <- mkReg(0);
	Reg#(Int#(CBDATASIZE)) z <- mkReg(0);
	Reg#(Int#(CBDATASIZE)) w <- mkReg(0);
	Reg#(Int#(CBDATASIZE)) q <- mkReg(0);
	
		
	// rule to initailze the memory
	Stmt test_seq = seq

		// dual ported memory so we can store 2 values at same time
		par
	   	v.mem_store(50,-100);		// initialize the mem where vector1 is present
	   	v.mem_store1(51,3);
		endpar    
		
		par	   	
		v.mem_store(52,2);
		v.mem_store1(53,-2000);
		endpar

		par
		v.mem_store(54,100);
	   	v.mem_store1(60,-210);		// initialize the mem where vector2 is present
		endpar

		par	   	
		v.mem_store(61, 340);
	   	v.mem_store1(62,-1);
		endpar

		par
	   	v.mem_store(63,800);
		v.mem_store1(64,2000);
	   	endpar
      
		// run the accelerator..
		par
		v.put_src1 (50);		// give src1
		v.put_src2 (60);		// give src2
		v.put_dst (110);		// give dst
		v.put_bsize (5);		// give block-size
		step <= 1;	
		endpar
	endseq;	

	Stmt test_seq1 = seq

		// dual ported memory so we can load 2 values at the same time	
		action
	   	x <= unpack(v.mem_load( 110 ));		// get the values stored from the dst 
		y <= unpack(v.mem_load1( 111 ));
		endaction		
		
		action
		z <= unpack(v.mem_load( 112 ));
		w <= unpack(v.mem_load1( 113 ));	
		endaction 
	
		q <= unpack(v.mem_load( 114 ));

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
	$display ("The final vector:\n [ %d %d %d %d %d]\n",x,y,z,w,q);		// print the results
	$display("\nTotal Time taken is: %d units", $time);
	$display("\n");
	$finish();
	endrule
	
endmodule

endpackage
