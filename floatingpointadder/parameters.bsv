package parameters;

typedef 8 CBADDRSIZE;  //size of configuration address bus to decode
// Ensure that CBDATASIZE have an value greater then or equal to 32. This is because floating point value are of 32-bits as per the IEEE-754 standards
typedef 32 CBDATASIZE;	//size of configuration data bus and the register used in the Accelerator

endpackage
