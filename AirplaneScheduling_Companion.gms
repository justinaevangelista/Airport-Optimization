*----------------------------------------------------------
* AIRPORT SCHEDULING PROBLEM
* Justin Evangelista
*----------------------------------------------------------
*

execute '=gdxxrw  input=AirplaneData.xlsx  output=GDXFiles\Sets.gdx          @CompileGDX_AirplaneSets.txt'       ;
execute '=gdxxrw  input=AirplaneData.xlsx  output=GDXFiles\Parameters.gdx    @CompileGDX_AirplaneParameters.txt' ;
