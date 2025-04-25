*----------------------------------------------------------
* AIRPORT SCHEDULING PROBLEM
* Justin Evangelista
*----------------------------------------------------------
*

Sets

    r          Runways         / R1*R4 /
    g          Gates           / G1*G16 /
    l          Taxi Lane       / L1*L2 / 
    t          Tasks           / landing, taxi_in, unload,taxi_out, takeoff /
    s          Size            / small, medium, large / 
    ;



Set a "Airplanes";
PARAMETERS
PT(a,t)      "Processing time for task t for airplane a"
DD(a,t)      "Due date for task t for airplane a"
PlaneSize(a, s) "1 if airplane a is of size s"
Arrival(a)      "Due date for task t for airplane a"
AllowR(s, r) "1 if a plane of size s can use runway r"
AllowG(s, g) "1 if a plane of size s can use gate g"
AllowL(s, l) "1 if a plane of size s can use taxi lane l"
;



$gdxin "GDXFiles\Sets.gdx"
$LOAD a

$gdxin "GDXFiles\Parameters.gdx"
$LOAD PT, DD, PlaneSize, Arrival, AllowR,AllowG,AllowL
Display a,PT,DD,PlaneSize,Arrival,AllowR,AllowG,AllowL;



;
Alias(a,a_prime);

Scalar 
   M            Big-M constant   / 10000 /
   TimeLimit    Time limit for circling  / 30 /
   numA         Number of airplanes      /    3 /
;


Variables
   k(a,t)    "start time of airplane a on task t "
   phi     "objective: average tardiness"
;

Positive Variable
   z(a,t)    "tardiness for airplane a";

Binary Variable
   xg(a,g)  "1 if airplane a is assigned to gate g"
   xr_in(a,r)  "1 if airplane a is assigned to runway r for landing"
   xl_in(a,l)  "1 if airplane a is assigned to taxi lane l after landing"
   xr_out(a,r)  "1 if airplane a is assigned to runway r for takeoff"
   xl_out(a,l)  "1 if airplane a is assigned to taxi lane l before takeoff"
   
   yg(a,a_prime,g) "1 if airplane a precedes a_prime in gate g"
   yr_in(a,a_prime,r) "1 if airplane a precedes a_prime on runway r for landing"
   yl_in(a,a_prime,l) "1 if airplane a is assigned to taxi lane l after landing"
   
   yr_out(a,a_prime,r) "1 if airplane a precedes a_prime on runway r for takeoff"
   yl_out(a,a_prime,l) "1 if airplane a is assigned to taxi lane l before takeoff"
   
   yr_io(a,a_prime,r) if plane a’s landing precedes airplane a′ s takeoff on runway r (1) or not (0)
   yl_io(a,a_prime,l) if plane a’s taxi in precedes airplane a′ s taxi out on taxi lane l (1) or not (0)
   
;

*----------------------------------------------------------
* Equations
*----------------------------------------------------------
* 
Equation
    Objective
    Tardiness(a,t)
   
    ArrivalTime(a)
   
    CirclingTimeLimit(a)
    
    AssignRunwayIn(a)
    AssignRunwayOut(a)
    AssignGate(a)
    AssignTaxiLaneIn(a)
    AssignTaxiLaneOut(a)
   
    RestrictGate(a, g)
    RestrictRunwayIn(a, r)
    RestrictLaneIn(a, l)
    RestrictRunwayOut(a, r)
    RestrictLaneOut(a, l)
   
    DisjunctiveRW1In(a,a_prime,r)
    DisjunctiveRW2In(a,a_prime,r)
    
    DisjunctiveRW1Out(a,a_prime,r)
    DisjunctiveRW2Out(a,a_prime,r)
    
    DisjunctiveRW1InOut(a,a_prime,r)
    DisjunctiveRW2InOut(a,a_prime,r)
   
    DisjunctiveTL1In(a,a_prime,l)
    DisjunctiveTL2In(a,a_prime,l)
    
    DisjunctiveTL1Out(a,a_prime,l)
    DisjunctiveTL2Out(a,a_prime,l)
    
    DisjunctiveTL1InOut(a,a_prime,l)
    DisjunctiveTL2InOut(a,a_prime,l)

    DisjunctiveG1(a,a_prime,g)
    DisjunctiveG2(a,a_prime,g)
   
    LinkXRYR1In(a,a_prime,r)
    LinkXRYR2In(a,a_prime,r)

    LinkXRYR1InOut(a,a_prime,r)
    LinkXRYR2InOut(a,a_prime,r)
    
    LinkXRYR1Out(a,a_prime,r)
    LinkXRYR2Out(a,a_prime,r)

    LinkXLYL1In(a,a_prime,l)
    LinkXLYL2In(a,a_prime,l)
   
    LinkXLYL1Out(a,a_prime,l)
    LinkXLYL2Out(a,a_prime,l)
    
    LinkXLYL1InOut(a,a_prime,l)
    LinkXLYL2InOut(a,a_prime,l)

    LinkXGYG1(a,a_prime,g)
    LinkXGYG2(a,a_prime,g)
   
    TaskOrder
;


Objective.. phi =E= sum(a, (k(a,'takeoff') + PT(a,'takeoff') - DD(a,'takeoff')));
Tardiness(a,t).. z(a,t) =E= k(a,t) + PT(a,t) - DD(a,t);

* Disjunctive constraints to prevent overla_prime on the same runway
*    - They only "activate" if both planes use the same runway (xr_in(a,r) = xr_in(a_prime,r) = 1).
*    - yr_in(a,a_prime,r) = 1 means "a" precedes "a_prime" on runway r.

* -----------------------   ARRIVAL  ----------------------------------------------

ArrivalTime(a).. k(a,'landing') =G= Arrival(a);
CirclingTimeLimit(a).. z(a,'landing') =L= TimeLimit;


* -----------------------  SIZE RESTRICTIONS  ----------------------------------------------

RestrictGate(a, g).. xg(a,g) =L= sum(s, PlaneSize(a,s)*AllowG(s, g));

* ---- IN -----
RestrictRunwayIn(a, r).. xr_in(a,r) =L= sum(s, PlaneSize(a,s)*AllowR(s, r));
RestrictLaneIn(a, l).. xl_in(a,l) =L= sum(s, PlaneSize(a,s)*AllowL(s, l));
* ---- OUT -----
RestrictRunwayOut(a, r).. xr_out(a,r) =L= sum(s, PlaneSize(a,s)*AllowR(s, r));
RestrictLaneOut(a, l).. xl_out(a,l) =L= sum(s, PlaneSize(a,s)*AllowL(s, l));

* -----------------------  REASOURCE  ----------------------------------------------

*Each airplane must be assigned to exactly one runway
AssignRunwayIn(a).. sum(r, xr_in(a,r)) =E= 1;
AssignRunwayOut(a).. sum(r, xr_out(a,r)) =E= 1;

*Each airplane must be assigned to exactly one gate
AssignGate(a).. sum(g, xg(a,g)) =E= 1;
*Each airplane must be assigned to exactly one taxi lane
AssignTaxiLaneIn(a).. sum(l, xl_in(a,l)) =E= 1;
AssignTaxiLaneOut(a).. sum(l, xl_out(a,l)) =E= 1;

* -----------------------   RUNWAY  ----------------------------------------------

* ---- IN -----
DisjunctiveRW1In(a,a_prime,r)$(ord(a) <> ord(a_prime))..
    k(a,'landing') + PT(a,'landing') =L= k(a_prime,'landing') + M*(1 - yr_in(a,a_prime,r)) + M*(1 - xr_in(a,r)) + M*(1 - xr_in(a_prime,r));

DisjunctiveRW2In(a,a_prime,r)$(ord(a) <> ord(a_prime))..
    k(a_prime,'landing') + PT(a_prime,'landing') =L= k(a,'landing') + M*(yr_in(a,a_prime,r)) + M*(1 - xr_in(a,r)) + M*(1 - xr_in(a_prime,r));

LinkXRYR1In(a,a_prime,r).. yr_in(a,a_prime,r) =L= xr_in(a,r);
LinkXRYR2In(a,a_prime,r).. yr_in(a,a_prime,r) =L= xr_in(a_prime,r);

* ---- OUT -----
DisjunctiveRW1Out(a,a_prime,r)$(ord(a) <> ord(a_prime))..
    k(a,'takeoff') + PT(a,'takeoff') =L= k(a_prime,'takeoff') + M*(1 - yr_out(a,a_prime,r)) + M*(1 - xr_out(a,r)) + M*(1 - xr_out(a_prime,r));

DisjunctiveRW2Out(a,a_prime,r)$(ord(a) <> ord(a_prime))..
    k(a_prime,'takeoff') + PT(a_prime,'takeoff') =L= k(a,'takeoff') + M*(yr_out(a,a_prime,r)) + M*(1 - xr_out(a,r)) + M*(1 - xr_out(a_prime,r));

LinkXRYR1Out(a,a_prime,r).. yr_out(a,a_prime,r) =L= xr_out(a,r);
LinkXRYR2Out(a,a_prime,r).. yr_out(a,a_prime,r) =L= xr_out(a_prime,r);

* ---- IN OUT -----
DisjunctiveRW1InOut(a,a_prime,r)$(ord(a) <> ord(a_prime))..
    k(a,'landing') + PT(a,'landing') =L= k(a_prime,'takeoff') + M*(1 - yr_io(a,a_prime,r)) + M*(1 - xr_in(a,r)) + M*(1 - xr_out(a_prime,r));

DisjunctiveRW2InOut(a,a_prime,r)$(ord(a) <> ord(a_prime))..
    k(a_prime,'takeoff') + PT(a_prime,'takeoff') =L= k(a,'landing') + M*(yr_io(a,a_prime,r)) + M*(1 - xr_in(a,r)) + M*(1 - xr_out(a_prime,r));

LinkXRYR1InOut(a,a_prime,r).. yr_io(a,a_prime,r) =L= xr_in(a,r);
LinkXRYR2InOut(a,a_prime,r).. yr_io(a,a_prime,r) =L= xr_out(a_prime,r);
    
* -----------------------   TAXI LANE ----------------------------------------------

* ---- IN -----
DisjunctiveTL1In(a,a_prime,l)$(ord(a) <> ord(a_prime))..
    k(a,'taxi_in') + PT(a,'taxi_in') =L= k(a_prime,'taxi_in') + M*(1 - yl_in(a,a_prime,l)) + M*(1 - xl_in(a,l)) + M*(1 - xl_in(a_prime,l));

DisjunctiveTL2In(a,a_prime,l)$(ord(a) <> ord(a_prime))..
    k(a_prime,'taxi_in') + PT(a_prime,'taxi_in') =L= k(a,'taxi_in') + M*(yl_in(a,a_prime,l)) + M*(1 - xl_in(a,l)) + M*(1 - xl_in(a_prime,l));

LinkXLYL1In(a,a_prime,l).. yl_in(a,a_prime,l) =L= xl_in(a,l);
LinkXLYL2In(a,a_prime,l).. yl_in(a,a_prime,l) =L= xl_in(a_prime,l);

* ---- OUT -----
DisjunctiveTL1Out(a,a_prime,l)$(ord(a) <> ord(a_prime))..
    k(a,'taxi_out') + PT(a,'taxi_out') =L= k(a_prime,'taxi_out') + M*(1 - yl_out(a,a_prime,l)) + M*(1 - xl_out(a,l)) + M*(1 - xl_out(a_prime,l));

DisjunctiveTL2Out(a,a_prime,l)$(ord(a) <> ord(a_prime))..
    k(a_prime,'taxi_out') + PT(a_prime,'taxi_out') =L= k(a,'taxi_out') + M*(yl_out(a,a_prime,l)) + M*(1 - xl_out(a,l)) + M*(1 - xl_out(a_prime,l));

LinkXLYL1Out(a,a_prime,l).. yl_out(a,a_prime,l) =L= xl_out(a,l);
LinkXLYL2Out(a,a_prime,l).. yl_out(a,a_prime,l) =L= xl_out(a_prime,l);

* ---- IN OUT -----
DisjunctiveTL1InOut(a,a_prime,l)$(ord(a) <> ord(a_prime))..
    k(a,'taxi_in') + PT(a,'taxi_in') =L= k(a_prime,'taxi_out') + M*(1 - yl_io(a,a_prime,l)) + M*(1 - xl_in(a,l)) + M*(1 - xl_out(a_prime,l));

DisjunctiveTL2InOut(a,a_prime,l)$(ord(a) <> ord(a_prime))..
    k(a_prime,'taxi_out') + PT(a_prime,'taxi_out') =L= k(a,'taxi_in') + M*(yl_io(a,a_prime,l)) + M*(1 - xl_in(a,l)) + M*(1 - xl_out(a_prime,l));

LinkXLYL1InOut(a,a_prime,l).. yl_io(a,a_prime,l) =L= xl_in(a,l);
LinkXLYL2InOut(a,a_prime,l).. yl_io(a,a_prime,l) =L= xl_out(a_prime,l);
* ----------------------   GATE  ---------------------------------------------------

DisjunctiveG1(a,a_prime,g)$(ord(a) <> ord(a_prime))..
    k(a,'unload') + PT(a,'unload') =L= k(a_prime,'unload') + M*(1 - yg(a,a_prime,g)) + M*(1 - xg(a,g)) + M*(1 - xg(a_prime,g));

DisjunctiveG2(a,a_prime,g)$(ord(a) <> ord(a_prime))..
    k(a_prime,'unload') + PT(a_prime,'unload') =L= k(a,'unload') + M*(yg(a,a_prime,g)) + M*(1 - xg(a,g)) + M*(1 - xg(a_prime,g));
    
LinkXGYG1(a,a_prime,g).. yg(a,a_prime,g) =L= xg(a,g);
LinkXGYG2(a,a_prime,g).. yg(a,a_prime,g) =L= xg(a_prime,g);

* ----------------------   ORDER  ---------------------------------------------------
TaskOrder(a,t)$(ord(t) < card(t))..
    k(a,t+1) =G= k(a,t) + PT(a,t);

*----------------------------------------------------------
* Variable bounds
*----------------------------------------------------------
k.up(a,t) = 500;
phi.lo = 0;
*----------------------------------------------------------
* Model declaration and solve
*----------------------------------------------------------
Model landingSchedule /all/;
option MIP = cplex;
Solve landingSchedule using MIP minimizing phi;


*----------------------------------------------------------
* Display results
*----------------------------------------------------------
Display t;

Display a,PT,DD,PlaneSize,Arrival,AllowR,AllowG,AllowL;
Display z.l, phi.l, k.l;
Display yr_in.l,yl_in.l, yg.l,yl_out.l, yr_out.l;
Display xr_in.l,xl_in.l, xg.l,xl_out.l,xr_out.l
Display yr_io.l, yl_io.l;

