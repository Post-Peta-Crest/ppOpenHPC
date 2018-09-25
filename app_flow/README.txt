This application program calculate two dimensional fluid dynamics 
around a circular cylinder by Finite Element Method.

The original serial program was developed by Dr. HATANAKA Katsumori,
and published in the book "Fluid Flow Simulation by the Finite Element Method",
The Japan Society of Fluid Mechanics Ed., Springer-Verlag, Tokyo (1998)
( in Japanese ).  This program was parallelized by using ppohFEM.


The following are used as input files other than ppohFEM input files. 

input.dat : Define calculation conditions. 
First row is omitted. 
Second row shows delta T and Reynolds number, respectively. 
Third row shows initial step, final step, output interval for display and output interval for UCD files. 

init.dat : Define initial conditions. 
First row is omitted. 
Following rows show, 
NodeID, initial value of flow velocity (U, V) and pressure (P) at each node, respectively. 

Boundary conditions are defined in a file "mesh.msh" using node groups. 
Nodes in node group "NGENTU" have inflow (U=1.0). 
Nodes in node group "NGNSU"  have zero flow velocity for U. 
Nodes in node group "NGNSV"  have zero flow velocity for V. 
Nodes in node group "NGNSP"  have zero pressure (P) and outflow. 


Calculation result is given as AVS UCD format. Example is stored in result directory. 
