'---------------------------------------------------------------------------------------------------------------
'This is the custom code written in VB.Net and used in the MB_HydroSim. 
'However, any sensitive data that requires authorization from Manitoba Hydro or the Government of Manitoba 
'has been replaced with placeholders ('Num'). This includes confidential numerical values such as rule curve 
'parameters and regression equations developed for various control points.'
'---------------------------------------------------------------------------------------------------------------
'86.4 is the conversion coefficient from cms to 1000m3/day
'35.3147 is the conversion coefficient from cfs to cms
'0.02831 is the conversion coefficient from cms to cfs
'0.3048 is the conversion coefficient from ft to m
'Since two decimal precision is used for simulation, all data read from MODSIM is divided by 100, 
'and all data written back to MODSIM is multiplied by 100 to maintain consistency in numerical scaling of MODSIM.
'---------------------------------------------------------------------------------------------------------------
Imports Csu.Modsim.ModsimIO
Imports Csu.Modsim.ModsimModel
Imports System
Imports System.Diagnostics
Imports System.IO


Module CustomMODSIM
	Dim myModel As New Model
    Dim JamesElevArray(14, 24) As Double 'Jamse Stage-Discharge Table
    Dim Nat_JamesElevArray(10, 29) As Double 'Natural James Stage-Discharge Table

	Sub Main(ByVal CmdArgs() As String)
        InitializeArrays()
		Dim FileName As String = cmdargs(0)
		AddHandler myModel.Init, AddressOf OnInitialize
		AddHandler myModel.IterBottom, AddressOf OnIterationBottom
		AddHandler myModel.IterTop, AddressOf OnIterationTop
		AddHandler myModel.Converged, AddressOf OnIterationConverge
		AddHandler myModel.End, AddressOf OnFinished
		AddHandler myModel.OnMessage, AddressOf OnMessage
		AddHandler myModel.OnModsimError, AddressOf OnMessage

    Using writer As New StreamWriter("output.txt") 'This is used to write what shown in command on a text file named output'
        Console.SetOut(writer)
		XYFileReader.Read(myModel, FileName)
        myModel.hydro.IterativeTechnique = IterativeSolutionTechnique.SuccApprox 'Run Based on Hydropower Target
        myModel.hydro.SetHydroLinkCosts(1000) 'Hydropower Wieght=1000
		Modsim.RunSolver(myModel)
	End Using
    End Sub

 '----------------------------------------------------------------------------------------------------------------------
 '---------------------------------------- Defining nodes, links, and variables ----------------------------------------
 '----------------------------------------------------------------------------------------------------------------------

 '---------------Poratage_Floodway-----------
    Dim Poratge_Diversion, Local_PortagetoWinnipeg, Local_Winnipeg, Sturgeon, Laselle, Floodway, Floodway_Inlet, Southport, RedRiver  As Node 'Inflow Nodes'
    Dim Southport_Pr, Southport_2Pr As Double 'Southport Flow in 2 Previous Time Steps
    Dim Poratge_Inflow, Actual_Southport, RedRiver_Discharge, Actual_JamesLevel As Double 'Variables for Calculations
    Dim JamesTarget_Level, JamesTarget_Level1, JamesTarget_Level2, JamesTarget_Level3, Assiniboine_MaxDischarge, Assiniboine_MaxDischarge1, Assiniboine_MaxDischarge2 As Double 'Regulation Variables 
    Dim Assiniboine_MinDischarge, PortageDiversion_MaxDischarge, Delta, FloodWay_Efficiency, JamesShift, IceShift As Double 'Regulation Variables 
    Dim Delta1, Actual_JamesLevel1 As Node 'For Printing Result
    Dim RedRiver_Outflow_Link, RedRiver_DSInflow_Link As Link 'Ouflow Link'
    Dim RedRiver_DSInflow_Node As Node

 '---------------Lake Manitoba System-------------
    'Lake Winnipegosis
    Dim LakeWinnipegosis, LakeWinnipegosis_LF, LakeWinnipegosis_LD, LakeWinnipegosis_Inflow1, LakeWinnipegosis_Inflow2, LakeWinnipegosis_Inflow3 As Node 'Reservoir, Localflow, Localdemand, and Inflow Nodes'
    Dim LakeWinnipegosis_Outflow As Link 'Ouflow Links'
    Dim LakeWinnipegosis_Storage, LakeWinnipegosis_Level  As Double 'Storage and Level
    'LakeManitoba
    Dim LakeManitoba, LakeManitoba_LF, LakeManitoba_LD, LakeManitoba_Inflow1, LakeManitoba_OutCh, LakeManitoba_Fairford As Node 'Reservoir, Localflow, Localdemand, Inflow, Outlet Channel Nodes'
    Dim LakeManitoba_Outflow_Fairford, LakeManitoba_Outflow_OutletChannel As Link 'Ouflow Links'
    Dim LakeManitoba_Storage, LakeManitoba_Level, LakeManitoba_Inflow2  As Double 'Storage and level
    Dim OP As Integer 'Operating Condition
    'LakeStMartin'
    Dim LakeStMartin, LakeStMartin_LF, LakeStMartin_LD, LakeStMartin_OutCh, LakeStMartin_Dauphin As Node 'Reservoir, Localflow, Localdemand, Outlet Channel Nodes'
    Dim LakeStMartin_Outflow_Dauphin, LakeStMartin_Outflow_OutletChannel As Link 'Ouflow Links'
    Dim LakeStMartin_Storage, LakeStMartin_Level As Double 'Storage and level
    'OutletChannels
    Dim OutletChannels As Integer 'Including outlet channels or not

 '---------------Shellmouth-------------------
    Dim Shellmouth, Shellmouth_LF, Shellmouth_LF1,Shellmouth_LF2,Shellmouth_LF3, Shellmouth_LD, Shellmouth_Outflow, Shellmouth_Spill, ConduitOpening_Save As Node 'Reservoir, Localflow, Localdemand, Flowthru Demands for Conduit and Spill, and  ConduitOpening Saving Nodes'
    Dim Shellmouth_Outflow_Link, Shellmouth_Spill_Link As Link 'Ouflow Links'
    Dim Brandon_Outflow_Link, Holland_Outflow_Link As Link 'Brandon and Holland Links'
    Dim Russell_Inflow1, Russell_Inflow2, Russell_Inflow3, Miniota_Inflow1, Miniota_Inflow2, Miniota_Inflow3, Brandon_Inflow1, Brandon_Inflow2, Brandon_Inflow3 As Node 'Downstream Inflow Nodes 
    Dim Shellmouth_Storage, Shellmouth_Inflow, ConduitOpening, Storage_Save, Shellmouth_Storage_Peak_1, Shellmouth_Level  As Double 'Storage, Inflow, Conduit, Storage Saving
    Dim Shellmouth_Outflow_1 As Double 'Conduit Outflow (Used for Calculation)
    Dim LB_Q1,LB_Q2 As Double  'lower bound for DD and SRE discharge, 1000m3/day
    Dim Min_Brandon,Min_Holland As Double 'lower bound for Brandon and Holland dischagre, 1000m3/day
    Dim Brandon_Outflow,Holland_Outflow As Double'Initial values Brandon and Holland dischagre, 1000m3/day
    Dim DD_HighestDesired,DD_LowestDesired As Double 'DD desired: highest and lowest amount for DD
    Dim StartDOY As Integer 'Start Date (DOY) of simulation
    Dim DD_StartDOY As Integer  '1st NOV is the date considered for starting the drawdown
    Dim Shellmouth_Level_Pr As Double 'No need to update
    Dim DD_DOY As Integer '31 March 'Drawdown Day of Year, Just before spring runoff start (increase in inflow)
    Dim SRE_DOY As Integer '7 June 'DOY of spring runoff end (decrease to low amount), Max=165
    Dim SPE_DOY As Integer '15 June 'DOY of spring end
    Dim Shellmout_Outfolw_tm1 As Double 'Previous TimeSteps Conduit Outflow (cms)
    Dim overtopped As Integer 'Check if the spill is overtopped during summer
    Dim Q1 As Double 'Shellmouth outflow from 1st November to DD date
    Dim Q2 As DOuble 'Shellmouth outflow from DD to SRE date
    
 '----------------Winnipeg River Basin---------------------  
    'StJoseph
    Dim StJoseph, CatRiver, StJoseph_LF, StJoseph_LD, RatRapid_De, RootRiver_De As Node 'Reservoir, Localflow, Localdemand Nodes'
    Dim StJoseph_Root, StJoseph_Rat As Link 'Ouflow Links'
    Dim StJoseph_Storage, StJoseph_Outflow_PrvStep As Double 'Storage(t) and Outflow(t-1) or previous time step
    'LacSeul'
    Dim LacSeul, LacSeul_LF, LacSeul_LD, LacSeul_Inflow1, LacSeul_Inflow2 As Node 'Reservoir, Localflow, Localdemand, Inflow Nodes'
    Dim LacSeul_Outflow As Link 'Ouflow and Link'
    Dim LacSeul_Storage, LacSeul_Outflow_PrvStep, LacSeul_Storage_PrvStep As Double 'Storage(t) and Outflow(t-1) or previous time step
    'Pakwash'
    Dim Pakwash, Pakwash_Inflow1, Pakwash_Inflow2, Pakwash_Inflow3 As Node 'Reservoir and Inflow Nodes'
    Dim Pakwash_Outflow As Link 'Ouflow Link'
    Dim Pakwash_Storage, Pakwash_Outflow_PrvStep As Double 'Storage(t) and Outflow(t-1) or previous time step
    'Ball'
    Dim Ball, Ball_Inflow1, Ball_Inflow2, Ball_Inflow3 As Node 'Reservoir and Inflow Nodes'
    Dim Ball_Outflow As Link 'Ouflow Link'
    Dim Ball_Storage, Ball_Outflow_PrvStep As Double 'Storage(t) and Outflow(t-1) or previous time step
    'Separation'
    Dim Separation, Separation_Inflow1 As Node 'Reservoir and Inflow Nodes'
    Dim Separation_Outflow As Link 'Ouflow Link'
    Dim Separation_Storage, Separation_Outflow_PrvStep As Double 'Storage(t) and Outflow(t-1) or previous time step
    'Umfreville'
    Dim Umfreville, Umfreville_Inflow1 As Node 'Reservoir and Inflow Nodes'
    Dim Umfreville_Outflow As Link 'Ouflow Link'
    Dim Umfreville_Storage, Umfreville_Outflow_PrvStep As Double 'Storage(t) and Outflow(t-1) or previous time step
    'LacLaCroix'
    Dim LacLaCroix, LacLaCroix_Inflow1 As Node 'Reservoir and Inflow'
    Dim LacLaCroix_Outflow As Link 'Ouflow'
    Dim LacLaCroix_Storage, LacLaCroix_Outflow_PrvStep As Double 'Storage(t) and Outflow(t-1) or previous time step
    'Namakan'
    Dim Namakan, Namakan_LF, Namakan_LD As Node 'Reservoir, Localflow, Localdemand Nodes'
    Dim Namakan_Outflow As Link 'Ouflow Link'
    Dim Namakan_Storage, Namakan_Outflow_PrvStep As Double 'Storage(t) and Outflow(t-1) or previous time step
    'Rainy'
    Dim Rainy, Rainy_LF, Rainy_LD, Rainy_Inflow1 As Node 'Reservoir, Localflow, Localdemand, and Inflow Nodes'
    Dim Rainy_Outflow As Link 'Ouflow Link'
    Dim Rainy_Storage, Rainy_Outflow_PrvStep As Double 'Storage(t) and Outflow(t-1) or previous time step
    'Woods'
    Dim Woods, Woods_LF, Woods_LD, Woods_Inflow1, Woods_Inflow2 As Node 'Reservoir, Localflow, Localdemand, and Inflow Nodes'
    Dim Woods_Outflow As Link 'Ouflow Link'
    Dim Woods_Storage, Woods_Outflow_PrvStep As Double 'Storage(t) and Outflow(t-1) or previous time step
    'Minaki'
    Dim Minaki, Minaki_Inflow1 As Node 'Reservoir and Inflow Nodes'
    Dim Minaki_Outflow As Link 'Ouflow Link'
    Dim Minaki_Storage As Double 'Storage(t)
    'PineFalls'
    Dim PineFalls, PineFalls_DSInflow_Node As Node
    Dim PineFalls_Outflow_Link1, PineFalls_Outflow_Link2, PineFalls_DSInflow_Link As Link


 '----------------------Neslon and Churcill River Basins-------------------------
    'Southern_Indian_Lake
    Dim Southern_Indian_Lake, SIL_Median_Stor As Node
    Dim Southern_Indian_Lake_Inflow, Southern_Indian_Lake_Discharge, Churchill_Inflow As link
    Dim Southern_Indian_Lake_Storage, Southern_Indian_Lake_Forebay As Integer
    Dim SIL_ub_condition, Ch_Cumulative_Inflow, Ch_Cumulative_Inflow_Prev as double
    
    'SouthBay Channel
    Dim SouthBay_Inflow, SouthBay_Discharge As link
    Dim SouthBay_Coef As node
    'Missi
    Dim Missi_Demand, Missi_Min_Flow as node
    Dim Missi_Discharge as link
    Dim Missi_Flow_PrvStep as double
    'Notigi
    Dim Notigi As Node
    Dim Notigi_Inflow, Notigi_Discharge As link
    Dim Notigi_Storage, Notigi_Forebay, Notigi_Flow_PrvStep As double
    'Wapisu
    Dim Wapisu As Node
    Dim Wapisu_Inflow, Wapisu_Discharge As link
    Dim Wapisu_Storage, Wapisu_Forebay As double
    'Footprint
    Dim Footprint As Node
    Dim Footprint_Inflow, Footprint_Discharge As link
    Dim Footprint_Storage, Footprint_Forebay As double 
    'Wuskwatim
    Dim Wuskwatim As Node
    Dim Wuskwatim_Inflow, Wuskwatim_Discharge As link
    Dim Wuskwatim_Storage As double
    'Opegano
    Dim Opegano As Node
    Dim Opegano_Inflow, Opegano_Discharge As link
    Dim Opegano_Storage, Opegano_Forebay As double
    'Birchtree
    Dim Birchtree As Node
    Dim Birchtree_Inflow, Birchtree_Discharge As link
    Dim Birchtree_Storage, Birchtree_Forebay As double
    'GrandRapids
    Dim GrandRapids, GR_HydroTarget As Node
    Dim GrandRapids_Inflow, GR_Hydro_Discharge, GR_Spill_Discharge  As link
    Dim GrandRapids_Storage As double
    'Lake Winnipeg
    Dim LakeWinnipeg As Node
    Dim LakeWinnipeg_Inflow, LakeWinnipeg_Discharge As link
    Dim LakeWinnipeg_Storage, LW_Outflow_PrvStep As Double
    Dim LakeWinnipeg_Forebay As Double
    'Jenpeg
    Dim Jenpeg As Node
    Dim Jenpeg_Inflow, Jenpeg_Discharge_Hydro, Jenpeg_Discharge_Spill, Jenpeg_HydroTarget, Jenpeg_MedStor As link
    Dim Jenpeg_Storage, Jenpeg_Head As double
    Dim Jenpeg_Forebay As Double
    Dim Jenpeg_Storage_PrvStep, Jenpeg_Outflow_PrvStep as double
    'Nelson West Channel
    Dim Nelson_West_Inflow, Nelson_West_Discharge As link
    Dim Nelson_West_Coef As node
    'Nelson East Channel
    Dim Nelson_East_Inflow, Nelson_East_Discharge As link
    Dim Nelson_East_Coef As node
    'Cross Lake
    Dim Crosslake As Node
    Dim Crosslake_Inflow, Crosslake_Discharge As link
    Dim Crosslake_Storage, Crosslake_Forebay As double
    'Sipiwesk
    Dim Sipiwesk As Node
    Dim Sipiwesk_Inflow, Sipiwesk_Discharge As link
    Dim Sipiwesk_Storage, Sipiwesk_Forebay As double
    'Kelsey
    Dim Kelsey As Node
    Dim Kelsey_Inflow, Kelsey_Discharge As link
    Dim Kelsey_Storage As double
    'Splitlake
    Dim Splitlake As Node
    Dim Splitlake_Inflow, Splitlake_Discharge As link
    Dim Splitlake_Storage, Splitlake_Forebay As double
    'Kettle
    Dim Kettle As Node
    Dim Kettle_Inflow, Kettle_Discharge As link
    Dim Kettle_Storage As double
    'LongSpruce
    Dim LongSpruce As Node
    Dim LongSpruce_Inflow, LongSpruce_Discharge As link
    Dim LongSpruce_Storage As double
    'Limestone
    Dim Limestone As Node
    Dim Limestone_Inflow, Limestone_Discharge As link
    Dim Limestone_Storage As double

 '----------------------------------------------------------------------------------------------------------------------
 '---------------------------------------- End of defining nodes, links, and variables ---------------------------------
 '----------------------------------------------------------------------------------------------------------------------
 
 '-----------------------------Shellmouth: Storage for different levels-------------------------
    Dim Storage_1386ft As Double=Num
    Dim Storage_1389ft As Double=Num
    Dim Storage_1391ft As Double=Num
    Dim Storage_1395ft As Double=Num
    Dim Storage_1398ft As Double=Num
    Dim Storage_1400ft As Double=Num
    Dim Storage_1402ft As Double=Num
    Dim Storage_1404ft As Double=Num
    Dim Storage_1405ft As Double=Num
    Dim Storage_14055ft As Double=Num
    Dim Storage_1406ft As Double=Num
    Dim Storage_14065ft As Double=Num
    Dim Storage_1407ft As Double=Num
    Dim Storage_Spill As Double=Num
    Dim Storage_1410ft As Double=Num 
    
 '-------------------------------James Discharge and Level Tables---------------------------------
    Dim JamesQArray() As Double= {Num, Num, ....}
    Dim RRFQArray() As Double = {Num, Num, ....}
    Dim Nat_JamesQArray() As Double = {Num, Num, ....}
    Dim Nat_AssinQArray() As Double = {Num, Num, ....}
    
   Private Sub InitializeArrays()
   JamesElevArray(0,0) = Num
   JamesElevArray(0,1) = Num
   '...
   JamesElevArray(14,24) = Num

   Nat_JamesElevArray(0,0) = Num
   Nat_JamesElevArray(0,1) = Num
   '...
   Nat_JamesElevArray(10,29) = Num
   End Sub

 '--------------------------------------------------------------------------------------------------
 '---------------------------------------- Regression Coefficients ---------------------------------
 '--------------------------------------------------------------------------------------------------
 '--------------------Winnipegr River Basin--------------
    'StJoseph (ST) from month 1 to 12
    Dim a_ST_1 As Double = Num
    Dim b_ST_1 As Double = Num
    Dim c_ST_1 As Double = Num
    Dim d_ST_1 As Double = Num
    Dim e_ST_1 As Double = Num
    '...
    Dim a_ST_12 As Double = Num
    Dim b_ST_12 As Double = Num
    Dim c_ST_12 As Double = Num
    Dim d_ST_12 As Double = Num
    Dim e_ST_12 As Double = Num

    'LacSeul, March is divided into to segments, that is why we have 13 set of coefficients
    Dim a_LS_1 As Double = Num
    Dim b_LS_1 As Double = Num
    Dim c_LS_1 As Double = Num
    Dim d_LS_1 As Double = Num
    Dim e_LS_1 As Double = Num
    '...
    Dim a_LS_13 As Double = Num
    Dim b_LS_13 As Double = Num
    Dim c_LS_13 As Double = Num
    Dim d_LS_13 As Double = Num
    Dim e_LS_13 As Double = Num

    'Pakwash'
    Dim a_PW As Double=Num
    Dim b_PW As Double=Num
    Dim c_PW As Double=Num
    Dim d_PW As Double=Num
    Dim e_PW As Double=Num

    'Ball'
    Dim a_Ba As Double=Num
    Dim b_Ba As Double=Num
    Dim c_Ba As Double=Num
    Dim d_Ba As Double=Num
    Dim e_Ba As Double=Num

    'Separation'
    Dim a_SP As Double=Num
    Dim b_SP As Double=Num
    Dim c_SP As Double=Num
    Dim d_SP As Double=Num
    Dim e_SP As Double=Num

    'Umfreville'
    Dim a_UF As Double=Num
    Dim b_UF As Double=Num
    Dim c_UF As Double=Num
    Dim d_UF As Double=Num
    Dim e_UF As Double=Num

    'LaLaCroix'
    Dim a_LC As Double=Num
    Dim b_LC As Double=Num
    Dim c_LC As Double=Num
    Dim d_LC As Double=Num
    Dim e_LC As Double=Num

    'Namakan'
    Dim a_NK_1 As Double=Num
    Dim b_NK_1 As Double=Num
    Dim c_NK_1 As Double=Num
    Dim d_NK_1 As Double=Num
    Dim e_NK_1 As Double=Num
    '...
    Dim a_NK_12 As Double=Num
    Dim b_NK_12 As Double=Num
    Dim c_NK_12 As Double=Num
    Dim d_NK_12 As Double=Num
    Dim e_NK_12 As Double=Num

    'Rainy'
    Dim a_RL_1 As Double=Num
    Dim b_RL_1 As Double=Num
    Dim c_RL_1 As Double=Num
    Dim d_RL_1 As Double=Num
    Dim e_RL_1 As Double=Num
    '...
    Dim a_RL_12 As Double=Num
    Dim b_RL_12 As Double=Num
    Dim c_RL_12 As Double=Num
    Dim d_RL_12 As Double=Num
    Dim e_RL_12 As Double=Num


    'Lake of the Woods'
    Dim a_LW_1 As Double=Num
    Dim b_LW_1 As Double=Num
    Dim c_LW_1 As Double=Num
    Dim d_LW_1 As Double=Num
    Dim e_LW_1 As Double=Num
    '...
    Dim a_LW_12 As Double=Num
    Dim b_LW_12 As Double=Num
    Dim c_LW_12 As Double=Num
    Dim d_LW_12 As Double=Num
    Dim e_LW_12 As Double=Num


    'Minaki'
    Dim a_MN As Double=Num
    Dim b_MN As Double=Num

 '-------------Nelson and Churchill River Basins---------------
    'Grand Rapids
    Dim GR_int_hydro as double = Num 
    Dim GR_a_hydro as double = Num
    Dim GR_int_spill as double = Num
    Dim GR_a_spill as double = Num
    
    'Footprint
    Dim FP_int as double = Num
    Dim FP_a as double = Num
    Dim FP_b as double = Num
    
    'Split Lake
    Dim SL_int as double = Num
    Dim SL_a as double = Num
    Dim SL_b as double = Num
    
    'Cross Lake
    Dim CL_int_1 as double = Num
    Dim CL_a_1 as double = Num
    Dim CL_b_1 as double = Num 
    
    Dim CL_int_2 as double = Num
    Dim CL_a_2 as double = Num
    
    'Sipiwesk
    Dim SW_int as double = Num
    Dim SW_a as double = Num
    
    'Missi
    Dim Missi_a_1 as double = Num
    Dim Missi_b_1 as double = Num
    Dim Missi_int_1 as double = Num
    
    Dim Missi_a_2 as double = Num
    Dim Missi_int_2 as double = -Num
    
    'Notigi
    'Storage relationship
    Dim Not_a_1 as double = Num
    Dim Not_int_1 as double = Num
    
    'Storage Regression with SIL Storage
    Dim Not_m_1 as double = Num
    Dim Not_b_1 as double = Num
    Dim Not_m_2 as double = Num
    Dim Not_b_2 as double = Num
    Dim Not_a_3 as double = Num
    Dim Not_b_3 as double = Num
    Dim Not_c_3 as double = Num
    '...
    Dim Not_m_12 as double = Num
    Dim Not_b_12 as double = Num
    
    'Notigi (Regression)
    Dim Not_max_m_1 as double = Num
    Dim Not_max_b_1 as double = Num
    Dim Not_max_m_2 as double = Num
    Dim Not_max_b_2 as double = Num
    
    'Notigi minimum discharge
    '(regression)
    Dim Not_min_a as double = Num
    Dim Not_min_b as double = Num
    Dim Not_min_c as double = Num
    Dim Not_min_d as double = Num
    
    Dim Not_Stor_1 as double 
    Dim Not_Stor_2 as double
    Dim Not_Stor_3 as double
    Dim Not_Stor_4 as double
    Dim Not_Stor_5 as double
    Dim Not_Stor_6 as double
    Dim Not_Stor_7 as double
    Dim Not_Stor_8 as double
    Dim Not_Stor_9 as double
    Dim Not_Stor_10 as double
    
    'Jenpeg
    'Upper Transition zone, monthly coefficients
    Dim Jenpeg_int_1 as double = Num
    Dim Jenpeg_a_1 as double = Num
    Dim Jenpeg_b_1 as double = Num
    '...
    Dim Jenpeg_int_12 as double = Num 
    Dim Jenpeg_a_12 as double = Num
    Dim Jenpeg_b_12 as double = Num
    
    'Lower storage range 3D surface (x = DOW, y = LW Stor)
    Dim Jenpeg_int as double = Num
    Dim Jenpeg_x as double = Num
    Dim Jenpeg_y as double = Num
    Dim Jenpeg_x2 as double = Num
    Dim Jenpeg_xy as double = Num
    
    'SIL monthly regression coefficients
    Dim SIL_m_1 as double =Num
    Dim SIL_b_1 as double = Num
    Dim SIL_m_2 as double = Num
    Dim SIL_b_2 as double = Num
    Dim SIL_a_3 as double = Num
    Dim SIL_b_3 as double = Num
    Dim SIL_c_3 as double = Num
    '...
    Dim SIL_m_12 as double = Num
    Dim SIL_b_12 as double = Num
 '--------------------------------------------------------------------------------------------------
 '---------------------------------------- End of Regression Coefficients --------------------------
 '--------------------------------------------------------------------------------------------------

 '--------------------------------------------------------------------------------------------------
 '---------------- Functions Needed for Portage Diversion and Red River Floodway -------------------
 '--------------------------------------------------------------------------------------------------

 'GIVEN A NATURAL Q AT PORTAGE, WHAT IS THE NATURAL Q AT HEADINGLEY.  AS PER TABLE 2 OF 1984 RRF MANUAL.
  Function NatlHeadQGivenNatPort(NatlPortQ As Double) As Double
    Dim CoeffA As Double
    Dim CoeffB As Double
    Dim CoeffC As Double

    If NatlPortQ < Num Then
        CoeffA = Num
        CoeffB = Num
        CoeffC = Num
    ElseIf NatlPortQ < Num Then
        CoeffA = Num
        CoeffB = Num
        CoeffC = Num
    ElseIf NatlPortQ < Num Then
        CoeffA = Num
        CoeffB = Num
        CoeffC = Num
    Else
        CoeffA = Num
        CoeffB = Num
        CoeffC = Num
    End If

    NatlHeadQGivenNatPort = (CoeffA * (NatlPortQ / 35.3147) ^ 2 + CoeffB * (NatlPortQ / 35.3147) + CoeffC) * 35.3147
  End Function

 'GIVEN A NATURAL Q AT PORTAGE AND RESULT OF NatlHeadQGivenNatPort FUNCTION, WHAT IS THE NATURAL Q AT HEADINGLEY.  AS PER TABLE 2 OF 1984 RRF MANUAL.
  Function NatARCAtHeadGivenNatPort(NatlHeadQ As Double, NatlPortQ As Double) As Double
    
    Dim CoeffA As Double
    Dim CoeffB As Double
    Dim CoeffC As Double

    If NatlHeadQ < Num Then
        CoeffA = Num
        CoeffB = Num
        CoeffC = Num
    Else
        CoeffA = Num
        CoeffB = Num
        CoeffC = Num
    End If

    NatARCAtHeadGivenNatPort = (CoeffA * (NatlHeadQ / 35.3147) + CoeffB * (NatlPortQ / 35.3147) + CoeffC) * 35.3147
  End Function


 'Interpolation Function
  Function InterpValue(InputValue As Double, CalcWhat As Integer, ElevArray() As Double, StorArray() As Double) As Double
    Dim elev_size As Integer = ElevArray.Length
    Dim stor_size As Integer = StorArray.Length

    If elev_size <> stor_size Then
        Return -1 ' Return an error code or message as needed
    End If

    Dim lookup_array(elev_size - 1) As Double

    For i As Integer = 0 To stor_size - 1
        If CalcWhat = 1 Then
            lookup_array(i) = StorArray(i)
        ElseIf CalcWhat = 2 Then
            lookup_array(i) = ElevArray(i)
        Else
            Return -1 ' Return an error code or message as needed
        End If
    Next

    Dim interp_value As Double = 0.0

    For i As Integer = 0 To stor_size - 1
        If i = stor_size - 1 Then
            Dim x2 As Double = ElevArray(i)
            Dim x1 As Double = ElevArray(i - 1)
            Dim y2 As Double = StorArray(i)
            Dim y1 As Double = StorArray(i - 1)

            If CalcWhat = 1 Then
                interp_value = (InputValue - y2) * ((x2 - x1) / (y2 - y1)) + x2
            ElseIf CalcWhat = 2 Then
                interp_value = (InputValue - x2) * ((y2 - y1) / (x2 - x1)) + y2
            End If
            Exit For
        End If

        If lookup_array(i) > InputValue Then
            Dim x2 As Double = ElevArray(i)
            Dim x1 As Double = ElevArray(i - 1)
            Dim y2 As Double = StorArray(i)
            Dim y1 As Double = StorArray(i - 1)

            If CalcWhat = 1 Then
                interp_value = (InputValue - y1) * ((x2 - x1) / (y2 - y1)) + x1
            ElseIf CalcWhat = 2 Then
                interp_value = (InputValue - x1) * ((y2 - y1) / (x2 - x1)) + y1
            End If
            Exit For
        End If
    Next

    Return interp_value
  End Function

 'Interpolation for natural level at james
  Function NatWL_Sep282004 (Nat_JamesQArray As Double(), Nat_AssinQArray As Double(), Nat_JamesElevArray(,) As Double, Nat_JamesQ As Double, Nat_AssinQ As Double) As Double
    
    Dim Elow, Ehigh, GLow, GHigh, Q1, Q2, Q3, Q4, Qint1, Qint2 As Double
    Dim E1, E2, G1, G2 As Integer

    For i As Integer = 0 To Nat_JamesQArray.Length - 2
        Elow = Nat_JamesQArray(i)
        If Nat_JamesQArray(i + 1) > Nat_JamesQ Then
            Ehigh = Nat_JamesQArray(i + 1)
            E1 = i
            E2 = i + 1
            Exit For
        End If
    Next

    For i As Integer = 0 To Nat_AssinQArray.Length - 2
        GLow = Nat_AssinQArray(i)
        If Nat_AssinQArray(i + 1) > Nat_AssinQ Then
            GHigh = Nat_AssinQArray(i + 1)
            G1 = i
            G2 = i + 1
            Exit For
        End If
    Next

    Q1 = Nat_JamesElevArray(G1, E1)
    Q2 = Nat_JamesElevArray(G1, E2)
    Q3 = Nat_JamesElevArray(G2, E1)
    Q4 = Nat_JamesElevArray(G2, E2)

    Qint1 = (Q2 - Q1) / (Ehigh - Elow) * (Nat_JamesQ - Elow) + Q1
    Qint2 = (Q4 - Q3) / (Ehigh - Elow) * (Nat_JamesQ - Elow) + Q3

    Dim NatWL_Sep282004_2 As Double = (Qint2 - Qint1) / (GHigh - GLow) * (Nat_AssinQ - GLow) + Qint1

    Return NatWL_Sep282004_2
  End Function

 'This curve uses inlet elevations (05OC026) to determine Floodway Q. Curve is based on 2018 recorded water levels and St Marys meterings and WSC estimate of flows.
  Function RRFInletCurve_2019(InputValue As Double, CalcWhat As Double) As Double
    Dim ElevArray() As Double = {Num, Num, ....}
    Dim StorArray() As Double = {Num, Num, ....}
    Dim VAnswer As Double = InterpValue(InputValue, CalcWhat, ElevArray, StorArray)
    Return VAnswer
  End Function

 'This curve uses inlet elevations (05OC026) to determine Floodway Q. Curve is based on 2022 recorded water levels and St Marys meterings and WSC estimate of flows.
  Function RRFInletCurve_2022(InputValue As Double, CalcWhat As Double) As Double
    Dim ElevArray() As Double = {Num, Num, ....}
    Dim StorArray() As Double = {Num, Num, ....}
    Dim VAnswer As Double = InterpValue(InputValue, CalcWhat, ElevArray, StorArray)
    Return VAnswer
  End Function

 'Perform linear interpolation for james level
  Function JamesLevel (JamesQArray As Double(), RRFQArray As Double(), JamesElevArray(,) As Double, JamesQ As Double, RRFQ As Double) As Double
    
    Dim Elow, Ehigh, GLow, GHigh, Q1, Q2, Q3, Q4, Qint1, Qint2 As Double
    Dim E1, E2, G1, G2 As Integer

    For i As Integer = 0 To JamesQArray.Length - 2
        Elow = JamesQArray(i)
        If JamesQArray(i + 1) > JamesQ Then
            Ehigh = JamesQArray(i + 1)
            E1 = i
            E2 = i + 1
            Exit For
        End If
    Next

    For i As Integer = 0 To RRFQArray.Length - 2
        GLow = RRFQArray(i)
        If RRFQArray(i + 1) > RRFQ Then
            GHigh = RRFQArray(i + 1)
            G1 = i
            G2 = i + 1
            Exit For
        End If
    Next

    Q1 = JamesElevArray(G1, E1)
    Q2 = JamesElevArray(G1, E2)
    Q3 = JamesElevArray(G2, E1)
    Q4 = JamesElevArray(G2, E2)

    Qint1 = (Q2 - Q1) / (Ehigh - Elow) * (JamesQ - Elow) + Q1
    Qint2 = (Q4 - Q3) / (Ehigh - Elow) * (JamesQ - Elow) + Q3

    Dim JamesLevel2 As Double = (Qint2 - Qint1) / (GHigh - GLow) * (RRFQ - GLow) + Qint1

    Return JamesLevel2
  End Function
 '--------------------------------------------------------------------------------------------------
 '-------------- End of Functions Needed for Portage Diversion and Red River Floodway --------------
 '--------------------------------------------------------------------------------------------------

  Private Sub OnInitialize()

 '--------------------------------------------------------------------------------------------------
 '--------------------------------Assigning nodes and links-----------------------------------------
 '--------------------------------------------------------------------------------------------------
    
    'Shellmouth
    Shellmouth= myModel.FindNode("Shellmouth")
    Shellmouth_LF=myModel.FindNode("Shellmouth_LF")
    Shellmouth_LF1=myModel.FindNode("ShellRiver") 
    Shellmouth_LF2=myModel.FindNode("StonyCreek") 
    Shellmouth_LF3=myModel.FindNode("Assiniboine")  
    Shellmouth_LD=myModel.FindNode("Shellmouth_LD")
    Shellmouth_Outflow=myModel.FindNode("Shellmouth_Outflow")
    Shellmouth_Outflow_Link=myModel.FindLink("Shellmouth_Shellmouth_Outflow")
    Shellmouth_Spill_Link=myModel.FindLink("Shellmouth_Shellmouth_Spill")
    Shellmouth_Spill=myModel.FindNode("Shellmouth_Spill")
    ConduitOpening_Save=myModel.FindNode("ConduitOpening")
    Brandon_Outflow_Link=myModel.FindLink("Brandon_US_Brandon_05MH001")
    Holland_Outflow_Link=myModel.FindLink("Holland_US_Holland_05MH005")
    Russell_Inflow1=myModel.FindNode("BirdtailCreek_05ME003")
    Russell_Inflow2=myModel.FindNode("QUAppelleRiver_05JM001")
    Russell_Inflow3=myModel.FindNode("RussellRecordedM923")
    Miniota_Inflow1=myModel.FindNode("ArrowRiver_05MG001")
    Miniota_Inflow2=myModel.FindNode("LittleSASKRiver_05MF018")
    Miniota_Inflow3=myModel.FindNode("MiniotaRecordedM1562")
    Brandon_Inflow1=myModel.FindNode("SourisRiver_05NG001")
    Brandon_Inflow2=myModel.FindNode("CypressRiver_05MH008")
    Brandon_Inflow3=myModel.FindNode("HollandRecordedM1467")

    'Poratage_Floodway
    Poratge_Diversion=myModel.FindNode("PoratgeDiversion_05LL019") 
    Local_PortagetoWinnipeg=myModel.FindNode("LocalPortagetoWinnipeg")
    Local_Winnipeg=myModel.FindNode("LocalWinnipeg")
    Sturgeon=myModel.FindNode("Sturgeon_05MJ004")
    Laselle=myModel.FindNode("Laselle_05OG001")
    Floodway=myModel.FindNode("Floodway_05DC029")
    Floodway_Inlet=myModel.FindNode("FloodwayInlet_05OC012")
    Southport=myModel.FindNode("Southport")
    RedRiver=myModel.FindNode("RedRiver")
    Delta1=myModel.FindNode("Delta")
    Actual_JamesLevel1=myModel.FindNode("JamesLevel")
    RedRiver_Outflow_Link=myModel.FindLink("Lake_Winnipeg_US_LakeWinnipeg")
    RedRiver_DSInflow_Link = myModel.FindLink("RedRiver_Inflow_LW_US")
    RedRiver_DSInflow_Node = myModel.FindNode("RedRiver_Inflow")

    'Lake Winnipegosis
    LakeWinnipegosis= myModel.FindNode("LakeWinnipegosis_05LH001")
    LakeWinnipegosis_LF=myModel.FindNode("LakeWinnipegosis_LF") 
    LakeWinnipegosis_LD=myModel.FindNode("LakeWinnipegosis_LD")
    LakeWinnipegosis_Inflow1=myModel.FindNode("Inflow_OverflowingRiver_05LD001")
    LakeWinnipegosis_Inflow2=myModel.FindNode("Inflow_OverflowingRiver_05LC004")
    LakeWinnipegosis_Inflow3=myModel.FindNode("Inflow_DauphinLake_05LJ025")
    LakeWinnipegosis_Outflow=myModel.FindLink("LakeWinnipegosis_05LH001_Waterhern_05LH005")
    'LakeManitoba'
    LakeManitoba= myModel.FindNode("LakeManitoba_05LK002")
    LakeManitoba_LF=myModel.FindNode("LakeManitoba_LF") 
    LakeManitoba_LD=myModel.FindNode("LakeManitoba_LD")
    LakeManitoba_Inflow1=myModel.FindNode("Inflow_Westbourne_05LL002")
    LakeManitoba_Outflow_Fairford=myModel.FindLink("LakeManitoba_05LK002_Fairford_05LM001")
    LakeManitoba_Outflow_OutletChannel=myModel.FindLink("LakeManitoba_05LK002_LM_OutletChannel")
    LakeManitoba_OutCh=myModel.FindNode("LM_OutletChannel")
    LakeManitoba_Fairford=myModel.FindNode("Fairford_05LM001")
    'LakeStMartin'
    LakeStMartin= myModel.FindNode("LakeStMartin_05LM005")
    LakeStMartin_LF=myModel.FindNode("LakeStMartin_LF") 
    LakeStMartin_LD=myModel.FindNode("LakeStMartin_LD")
    LakeStMartin_Outflow_Dauphin=myModel.FindLink("LakeStMartin_05LM005_DauphinRiver_05LM006")
    LakeStMartin_Outflow_OutletChannel=myModel.FindLink("LakeStMartin_05LM005_LSM_OutletChannel")
    LakeStMartin_OutCh=myModel.FindNode("LSM_OutletChannel")
    LakeStMartin_Dauphin=myModel.FindNode("DauphinRiver_05LM006")
    
    'StJoseph
    StJoseph_LF=myModel.FindNode("LakeSTJoseph_LF")
    CatRiver=myModel.FindNode("CatRiver")
    StJoseph=myModel.FindNode("LakeSTJoseph")
    StJoseph_LD=myModel.FindNode("LakeSTJoseph_LD")
    RatRapid_De=myModel.FindNode("RatRapidDam")
    RootRiver_De=myModel.FindNode("RootRiverDam")
    StJoseph_Root=myModel.FindLink("LakeSTJoseph_RootRiverDam")
    StJoseph_Rat=myModel.FindLink("LakeSTJoseph_RatRapidDam")
    'LacSeul'
    LacSeul= myModel.FindNode("LacSeul")
    LacSeul_LF=myModel.FindNode("LacSeul_LF") 
    LacSeul_LD=myModel.FindNode("LacSeul_LD")
    LacSeul_Inflow1=myModel.FindNode("EnglishRiver")
    LacSeul_Inflow2=myModel.FindNode("SturgeonR_atMcDougall")
    LacSeul_Outflow=myModel.FindLink("LacSeul_LacSeul_DS_EarFalls")
    'Pakwash'
    Pakwash= myModel.FindNode("PakwashLake")
    Pakwash_Inflow1= myModel.FindNode("ChukuniRiver")
    Pakwash_Inflow2= myModel.FindNode("TroutlakeRiver")
    Pakwash_Inflow3= myModel.FindNode("CedarRiver_M177")
    Pakwash_Outflow=myModel.FindLink("PakwashLake_PakwashLake_DS_ManitouFalls")
    'Ball'
    Ball= myModel.FindNode("BallLake")
    Ball_Inflow1= myModel.FindNode("CedarRiver_M262")
    Ball_Inflow2= myModel.FindNode("WabigoonRiver")
    Ball_Inflow3= myModel.FindNode("LongLeggedRiver")
    Ball_Outflow=myModel.FindLink("BallLake_BallLake_DS")
    'Separation'
    Separation= myModel.FindNode("SeparationLake")
    Separation_Inflow1= myModel.FindNode("SturgeonR_atSalveson_M064")
    Separation_Outflow=myModel.FindLink("SeparationLake_SeparationLake_DS")
    'Umfreville'
    Umfreville= myModel.FindNode("UmfrevilleLake")
    Umfreville_Inflow1= myModel.FindNode("SturgeonR_atSalveson_M163")
    Umfreville_Outflow=myModel.FindLink("UmfrevilleLake_UmfrevilleLake_DS_CaribouFalls")
    'LacLaCroix'
    LacLaCroix= myModel.FindNode("LacLaCroix")
    LacLaCroix_Inflow1=myModel.FindNode("LacLaCroix_US")
    LacLaCroix_Outflow=myModel.FindLink("LacLaCroix_LacLaCroix_DS_NamkanRiver")
    'Namakan'
    Namakan= myModel.FindNode("NamakanLake")
    Namakan_LF=myModel.FindNode("NamakanLake_LF") 
    Namakan_LD=myModel.FindNode("NamakanLake_LD")
    Namakan_Outflow=myModel.FindLink("NamakanLake_NamakanLake_DS_KettleFalls")
    'Rainy'
    Rainy= myModel.FindNode("RainyLake")
    Rainy_LF=myModel.FindNode("RainyLake_LF") 
    Rainy_LD=myModel.FindNode("RainyLake_LD")
    Rainy_Inflow1=myModel.FindNode("TurtleRiver")
    Rainy_Outflow=myModel.FindLink("RainyLake_RainyLake_DS_Fortfrances")
    'Woods'
    Woods= myModel.FindNode("LakeOfTheWoods")
    Woods_LF=myModel.FindNode("LakeOfTheWoods_LF") 
    Woods_LD=myModel.FindNode("LakeOfTheWoods_LD")
    Woods_Inflow1=myModel.FindNode("BigForkRiver_M1399_LF")
    Woods_Inflow2=myModel.FindNode("LittleForkRiver_M1095")
    Woods_Outflow=myModel.FindLink("LakeOfTheWoods_LakeOfTheWoods_DS_NormanDam")
    'Minaki'
    Minaki= myModel.FindNode("Minaki")
    Minaki_Inflow1= myModel.FindNode("SturgeonR_atSalveson_M096")
    Minaki_Outflow=myModel.FindLink("Minaki_Minaki_DS_WhitedogFalls")
    'PineFalls'
    PineFalls=myModel.FindNode("PineFalls")
    PineFalls_Outflow_Link1 = myModel.FindLink("PineFalls_PineFalls_DS")
    PineFalls_Outflow_Link2=myModel.FindLink("PineFalls_PineFalls_DS_1")
    PineFalls_DSInflow_Node = myModel.FindNode("PineFalls_Inflow")
    PineFalls_DSInflow_Link = myModel.FindLink("PineFalls_Inflow_LW_US")
    
    'Southern_Indian_Lake
    Southern_Indian_Lake = myModel.FindNode("Southern_Indian_Lake")
    Southern_Indian_Lake_Inflow = myModel.FindLink("SIL_US_Southern_Indian_Lake")
    Churchill_Inflow = myModel.FindLink("Churchill_Inflow_SIL_US")
    Southern_Indian_Lake_Discharge = myModel.FindLink("Southern_Indian_Lake_SIL_DS")
    SIL_Median_Stor = myModel.FindNode("SIL_Median_Storage_Inflow")
    
    'SouthBay Channel
    SouthBay_Inflow = myModel.FindLink("Southern_Indian_Lake_SouthBay_Channel")
    SouthBay_Discharge = myModel.FindLink("SouthBay_Channel_Notigi_US")
    SouthBay_Coef = myModel.FindNode("SouthBayChannel_IceCoefInflow")
     
    'Missi
    Missi_Demand = myModel.FindNode("ToHudsonBay")
    Missi_Min_Flow = myModel.FindNode("Missi_Min_Flow_Inflow")
    Missi_Discharge = myModel.FindLink("Missi_ToHudsonBay")
    
    'Notigi
    Notigi = myModel.FindNode("Notigi")
    Notigi_Inflow = myModel.FindLink("Notigi_US_Notigi")
    Notigi_Discharge = myModel.FindLink("Notigi_Notigi_DS")
     
     'Wapisu
    Wapisu = myModel.FindNode("Wapisu")
    Wapisu_Inflow = myModel.FindLink("Wapisu_US_Wapisu")
    Wapisu_Discharge = myModel.FindLink("Wapisu_Wapisu_DS")
    
    'Footprint
    Footprint = myModel.FindNode("Footprint")
    Footprint_Inflow = myModel.FindLink("Footprint_US_Footprint")
    Footprint_Discharge = myModel.FindLink("Footprint_Footprint_DS")
    
    'Wuskwatim
    Wuskwatim = myModel.FindNode("Wuskwatim")
    Wuskwatim_Inflow = myModel.FindLink("Wuskwatim_US_Wuskwatim")
    Wuskwatim_Discharge = myModel.FindLink("Wuskwatim_Wuskwatim_DS")
    
    'Opegano
    Opegano = myModel.FindNode("Opegano")
    Opegano_Inflow = myModel.FindLink("Opegano_US_Opegano")
    Opegano_Discharge = myModel.FindLink("Opegano_Opegano_DS")
  
    'Birchtree
    Birchtree = myModel.FindNode("Birchtree")
    Birchtree_Inflow = myModel.FindLink("Birchtree_US_Birchtree")
    Birchtree_Discharge = myModel.FindLink("Birchtree_Birchtree_DS")
    
    'GrandRapids
    GrandRapids = myModel.FindNode("GrandRapids")
    GrandRapids_Inflow = myModel.FindLink("GrandRapids_US_GrandRapids")
    GR_Hydro_Discharge = myModel.FindLink("GrandRapids_GrandRapids_DS")
    GR_Spill_Discharge = myModel.FindLink("GrandRapids_GrandRapids_DS_1")
    GR_HydroTarget = myModel.FindNode("GrandRapids_HydroTarget_Inflow")
       
    'Lake_Winnipeg
    LakeWinnipeg = myModel.FindNode("Lake_Winnipeg")
    LakeWinnipeg_Inflow = myModel.FindLink("LW_US_Lake_Winnipeg")
    LakeWinnipeg_Discharge = myModel.FindLink("Lake_Winnipeg_LW_DS")
                 
    'Nelson West Channel
    Nelson_West_Inflow = myModel.FindLink("LW_DS_Nelson_West")
    Nelson_West_Discharge = myModel.FindLink("Nelson_West_Jenpeg_US")
    Nelson_West_Coef = myModel.FindNode("WestChannel_IceCoefInflow")
         
    'Nelson East Channel
    Nelson_East_Inflow = myModel.FindLink("LW_DS_Nelson_East")
    Nelson_East_Discharge = myModel.FindLink("Nelson_East_Cross_US")
    Nelson_East_Coef = myModel.FindNode("EastChannel_IceCoefInflow")
     
    'Jenpeg
    Jenpeg = myModel.FindNode("Jenpeg")
    Jenpeg_Inflow = myModel.FindLink("Jenpeg_US_Jenpeg")
    Jenpeg_Discharge_Hydro = myModel.FindLink("Jenpeg_Jenpeg_DS")
    Jenpeg_Discharge_Spill = myModel.FindLink("Jenpeg_Jenpeg_DS_1")
    Jenpeg_HydroTarget = myModel.FindLink("Jenpeg_HydroTarget_Inflow_Jenpeg_HydroTarget")
    Jenpeg_MedStor = myModel.FindLink("Jenpeg_MedStor_Inflow_Jenpeg_MedStor")
    
    'Cross Lake
    Crosslake = myModel.FindNode("Crosslake")
    Crosslake_Inflow = myModel.FindLink("Cross_US_Crosslake")
    Crosslake_Discharge = myModel.FindLink("Crosslake_Cross_DS")

    'Sipiwesk
    Sipiwesk = myModel.FindNode("Sipiwesk")
    Sipiwesk_Inflow = myModel.FindLink("Sipiwesk_US_Sipiwesk")
    Sipiwesk_Discharge = myModel.FindLink("Sipiwesk_Sipiwesk_DS")
    
    'Kelsey
    Kelsey = myModel.FindNode("Kelsey")
    Kelsey_Inflow = myModel.FindLink("Kelsey_US_Kelsey")
    Kelsey_Discharge = myModel.FindLink("Kelsey_Kelsey_DS")
    
    'Splitlake
    Splitlake = myModel.FindNode("Splitlake")
    Splitlake_Inflow = myModel.FindLink("Split_US_Splitlake")
    Splitlake_Discharge = myModel.FindLink("Splitlake_Split_DS")
    
    'Kettle
    Kettle = myModel.FindNode("Kettle")
    Kettle_Inflow = myModel.FindLink("Kettle_US_Kettle")
    Kettle_Discharge = myModel.FindLink("Kettle_Kettle_DS")

    'LongSpruce
    LongSpruce = myModel.FindNode("LongSpruce")
    LongSpruce_Inflow = myModel.FindLink("LongSpruce_US_LongSpruce")
    LongSpruce_Discharge = myModel.FindLink("LongSpruce_LongSpruce_DS")
    
    'Limestone
    Limestone = myModel.FindNode("Limestone")
    Limestone_Inflow = myModel.FindLink("Limestone_US_Limestone")
    Limestone_Discharge = myModel.FindLink("Limestone_Limestone_DS")
 '--------------------------------------------------------------------------------------------------
 '--------------------------------End of Assigning nodes and links----------------------------------
 '--------------------------------------------------------------------------------------------------

 '--------------------------------------------------------------------------------------------------
 '-----------------------------------------Initial Values-------------------------------------------
 '--------------------------------------------------------------------------------------------------

    'Shellmouth
    Shellmouth_Level = 425.925
    LB_Q1 =(100*0.02831*86.4)
    LB_Q2 =(25*0.02831*86.4)
    Min_Brandon =(100*0.02831*86.4)
    Min_Holland =(200*0.02831*86.4)
    Brandon_Outflow =(101*0.02831*86.4)
    Holland_Outflow =(201*0.02831*86.4)
    DD_HighestDesired =Storage_1398ft
    DD_LowestDesired =Storage_1386ft
    StartDOY=1
    DD_StartDOY=305 
    Shellmouth_Level_Pr=0 'No need to update
    DD_DOY = 90 '31 March
    SRE_DOY = 158 '7 June
    SPE_DOY = 166 '15 June
    Shellmout_Outfolw_tm1=8 
    overtopped=0

    'LakeWinnipegosis
    LakeWinnipegosis_Level=253.373

    'LakeManitoba'
    OutletChannels=0
    LakeManitoba_Level=247.499
    if LakeManitoba_Level>=Num Then
    OP=1
    elseif LakeManitoba_Level<Num and LakeManitoba_Level>=Num Then
    OP=2
    elseif LakeManitoba_Level<Num and LakeManitoba_Level>=Num Then
    OP=3
    elseif LakeManitoba_Level<Num Then
    OP=4
    End if

    'LakeStMartin'
    LakeStMartin_Level=243.747
    
    'Diversion and Floodway
    Southport_2Pr=21.6 'cms 
    Southport_Pr=20.9  'cms
    Delta=Num 'ft
    FloodWay_Efficiency=1
    JamesShift=0 'ft
    IceShift=0 'ft
    PortageDiversion_MaxDischarge=Num 'cfs
    JamesTarget_Level1=Num 'ft
    JamesTarget_Level2=Num 'ft
    JamesTarget_Level3=Num 'ft
    if LakeManitoba_Level<=(Num*0.3048)
    JamesTarget_Level=JamesTarget_Level1
    elseif LakeManitoba_Level>=(Num*0.3048)
    JamesTarget_Level=JamesTarget_Level2
    elseif LakeManitoba_Level>=(Num*0.3048) and PortageDiversion_MaxDischarge>Num
    JamesTarget_Level=JamesTarget_Level3
    End if
    Assiniboine_MaxDischarge1=Num  'cfs
    Assiniboine_MaxDischarge2=Num  'cfs
    if LakeManitoba_Level<=(Num*0.3048)
    Assiniboine_MaxDischarge=Assiniboine_MaxDischarge1
    elseif LakeManitoba_Level>=(Num*0.3048)
    Assiniboine_MaxDischarge=Assiniboine_MaxDischarge2
    elseif LakeManitoba_Level>=(Num*0.3048) and PortageDiversion_MaxDischarge>Num
    Assiniboine_MaxDischarge=Num
    End if
    Assiniboine_MinDischarge=Num 'cfs
    Actual_JamesLevel=Num
    
    'Nelson River Basin
    StJoseph_Outflow_PrvStep=146
    LacSeul_Storage_PrvStep=4833072.13
    LacSeul_Outflow_PrvStep=381
    Pakwash_Outflow_PrvStep=446.61
    Ball_Outflow_PrvStep=500
    Separation_Outflow_PrvStep=525
    Umfreville_Outflow_PrvStep=550
    LacLaCroix_Outflow_PrvStep=42.7
    Namakan_Outflow_PrvStep=57.48
    Rainy_Outflow_PrvStep=229
    Woods_Outflow_PrvStep=292
    Footprint_Forebay = 243.42*100
    Splitlake_Forebay = 167.762*100
    Crosslake_Forebay = 207.420*100
    Sipiwesk_Forebay = 186.351*100
    Missi_Flow_PrvStep = 143.0371372
    Notigi_Flow_PrvStep = 880.54
    Jenpeg_Outflow_PrvStep = 1892.556 'cms
    LakeWinnipeg_Forebay = 217.4092*100
    Jenpeg_Forebay = 214.8935*100 + (0.1584*100)
    Notigi_Forebay = 256.83*100
    Notigi_Storage = 1620564.55077428 '1000 m3
    Southern_Indian_Lake_Forebay = 258.03*100
    Southern_Indian_Lake_Storage = 4601736.2 ' 1000m3
    SIL_ub_condition = 0
    Ch_Cumulative_Inflow_Prev = 0
    Ch_Cumulative_Inflow = 0
    Not_Stor_1 = 1620564.55077428
    Not_Stor_2 = 1620564.55077428
    Not_Stor_3 = 1620564.55077428
    Not_Stor_4 = 1620564.55077428
    Not_Stor_5 = 1620564.55077428
    Not_Stor_6 = 1620564.55077428
    Not_Stor_7 = 1620564.55077428
    Not_Stor_8 = 1620564.55077428
    Not_Stor_9 = 1620564.55077428
    Not_Stor_10 = 1620564.55077428
 '--------------------------------------------------------------------------------------------------
 '-----------------------------------------End of Initial Values------------------------------------
 '--------------------------------------------------------------------------------------------------
	End Sub

	Private Sub OnIterationTop()

	End Sub

	Private Sub OnMessage(ByVal message As String)
	
    End Sub

	Private Sub OnIterationBottom()

    Dim currentIndex As Integer=myModel.mInfo.CurrentModelTimeStepIndex 'Current Time Step Index
    Dim CurrDate As Date=myModel.TimeStepManager.Index2Date(currentIndex,TypeIndexes.ModelIndex)'Finding the Current Date: Starting Date of Time Step
    Dim CurrEndDate As Date=myModel.TimeStepManager.Index2EndDate(currentIndex,TypeIndexes.ModelIndex)'Finding the Current Date: Ending Date of Time Step
    Dim DOY AS Integer=CurrDate.DayOfYear 'Finding the DOY of the Current Date
    Dim CurrMonth As Integer=CurrDate.Month 'Finding the Month of the Current Date
    Dim CurrYear As Integer=CurrDate.Year 'Finding the Year of the Current Date
    Dim CurrDay As Integer=CurrDate.Day 'Finding the Day of the Current Date

    if myModel.mInfo.Iteration=0 Then 'Defining Target Storages at the Beginning Iteration to Avoid Calculating Again

    Console.WriteLine("TimeStep: " & CurrDate & " To " & CurrEndDate) 'To Check The TimeStep: in output text file'
    Console.WriteLine("DOY : " & DOY) 'To Check The TimeStep: in output text file'

 '--------------------------------------------------------------------------------------------------
 '---------------------------------------- Defined Operations --------------------------------------
 '--------------------------------------------------------------------------------------------------

 '------------------------Shellmouth Operation----------------------------------
    'Shellmouth Spill
    Dim Shellmouth_Spill1 As Double=0.01
    if Shellmouth_Level>=1408.5*0.3048 and Shellmouth_Level<=432.4
    Shellmouth_Spill1=(Num*(Shellmouth_Level^3) + Num*(Shellmouth_Level^2) - Num*(Shellmouth_Level) + Num)*86.4
    elseif Shellmouth_Level>=432.4
    Shellmouth_Spill1=(Num*(Shellmouth_Level^3) - Num*(Shellmouth_Level^2) + Num*(Shellmouth_Level) - Num)*86.4
    End if
    if Shellmouth_Spill1<=0
    Shellmouth_Spill1=0.01*86.4
    End if
    Shellmouth_Spill.mnInfo.nodedemand(currentIndex,0)=Shellmouth_Spill1*100 'Defining the outflow for conduit
    
    'Shellmouth Conduit Outflow
    'Varibales used in caculation
    Shellmouth_Storage=Shellmouth.mnInfo.start/100
    Shellmouth_Inflow=(Shellmouth_LF.mnInfo.inflow(currentIndex,0)/100)+(Shellmouth_LF1.mnInfo.inflow(currentIndex,0)/100)+(Shellmouth_LF2.mnInfo.inflow(currentIndex,0)/100)+(Shellmouth_LF3.mnInfo.inflow(currentIndex,0)/100)-(Shellmouth_LD.mnInfo.nodedemand(currentIndex,0)/100)
    
    'DD=Drawdown, SRE=Spring Runoff End
    
    '1) DD and Peak Storage check to calculate outflows
    if currentIndex=0 or DOY=DD_StartDOY Then
    Console.WriteLine("Recalculate Dischagre for DD and SRE")
    Q1= LB_Q1
    Q2= LB_Q2

    '1-1) Start Date is Before DD 
    if DOY>=DD_StartDOY or DOY<DD_DOY Then
        
    Dim b As Integer 'defining index for "for loops" (start to DD)
    if DOY>=DD_StartDOY Then
    b=366-DOY+DD_DOY
    else
    b=DD_DOY-DOY
    End if
    Dim c As Integer=SRE_DOY-DD_DOY 'defining index for "for loops" (DD to SRE)

    While Q1<= (1500*0.02831*86.4) and Q2<= (1601*0.02831*86.4) 'Upper bound for DD and SRE dischagre

    Dim Shellmouth_Storage_Next As Double=Shellmouth_Storage
    Dim Shellmouth_Storage_Next1 As Double
    Dim Shellmouth_Storage_Peak As Double
    Dim i As Integer
    
    for i=currentIndex To currentIndex+b 'calculate DD_storage based on Q1
    Shellmouth_Storage_Next=Shellmouth_Storage_Next+(Shellmouth_LF.mnInfo.inflow(i,0)/100)+(Shellmouth_LF1.mnInfo.inflow(i,0)/100)+(Shellmouth_LF2.mnInfo.inflow(i,0)/100)+(Shellmouth_LF3.mnInfo.inflow(i,0)/100)-(Shellmouth_LD.mnInfo.nodedemand(i,0)/100)-Q1
    next
    
    Shellmouth_Storage_Next1=Shellmouth_Storage_Next
    Shellmouth_Storage_Peak=Shellmouth_Storage_Next1
    Dim j As Integer
    for j=currentIndex+b+1 To currentIndex+b+c 'calculate SRE_storage based on Q2
    Shellmouth_Storage_Next1=Shellmouth_Storage_Next1+(Shellmouth_LF.mnInfo.inflow(j,0)/100)+(Shellmouth_LF1.mnInfo.inflow(j,0)/100)+(Shellmouth_LF2.mnInfo.inflow(j,0)/100)+(Shellmouth_LF3.mnInfo.inflow(j,0)/100)-(Shellmouth_LD.mnInfo.nodedemand(j,0)/100)-Q2
    if Shellmouth_Storage_Next1>=Shellmouth_Storage_Peak 'updating peak level
    Shellmouth_Storage_Peak=Shellmouth_Storage_Next1
    Shellmouth_Storage_Peak_1=Shellmouth_Storage_Peak
    End if
    next
    'stop cirteria for outflow calculation 
    if Shellmouth_Storage_Peak<Storage_1404ft and Q2<= (500*0.02831*86.4) and Shellmouth_Storage_Next<DD_HighestDesired Then 
    Exit While
    elseif Shellmouth_Storage_Peak>Storage_1404ft and Shellmouth_Storage_Peak<Storage_1405ft and Q2<= (1200*0.02831*86.4) and Q2> (500*0.02831*86.4) and Shellmouth_Storage_Next<DD_HighestDesired Then
    Exit While
    elseif Shellmouth_Storage_Peak>Storage_1405ft and Shellmouth_Storage_Peak<Storage_14055ft and Q2<= (1300*0.02831*86.4) and Q2> (1200*0.02831*86.4) and Shellmouth_Storage_Next<DD_HighestDesired Then
    Exit While
    elseif Shellmouth_Storage_Peak>Storage_14055ft and Shellmouth_Storage_Peak<Storage_1406ft and Q2<= (1400*0.02831*86.4) and Q2> (1300*0.02831*86.4) and Shellmouth_Storage_Next<DD_HighestDesired Then
    Exit While
    elseif Shellmouth_Storage_Peak>Storage_1406ft and Shellmouth_Storage_Peak<Storage_14065ft and Q2<= (1500*0.02831*86.4) and Q2> (1400*0.02831*86.4) and Shellmouth_Storage_Next<DD_HighestDesired Then
    Exit While
    elseif Shellmouth_Storage_Peak>Storage_14065ft and Shellmouth_Storage_Peak<Storage_1407ft and Q2<= (1600*0.02831*86.4) and Q2> (1500*0.02831*86.4) and Shellmouth_Storage_Next<DD_HighestDesired Then
    Exit While
    elseif Shellmouth_Storage_Next<DD_LowestDesired 
    Q2=(1600*0.02831*86.4) 'Max Q2 (1600cfs) when DD is less than 1386
    Exit While
    elseif Q1>= (1490*0.02831*86.4) and Shellmouth_Storage_Peak>Storage_1407ft Then
    Q1=(1500*0.02831*86.4) 'Max Q1 (1500cfs)    
    Q2=(1600*0.02831*86.4) 'Max Q2 (1600cfs)
    Exit While
    End if

    Q2+=10
    Q1+=10
    
    End While 'while for changing Q for SRE

    End if  'Start date before DD

    '1-2) Start Date is Before SRE but after DD
    if DOY>=DD_DOY and DOY<SRE_DOY
    Console.WriteLine("Recalculate Dischagre for SRE")
    Dim c As Integer=SRE_DOY-DD_DOY 'defining index for for loops (DD to SRE)

    While Q2<= (1600*0.02831*86.4) 'Upper bound for SRE dischagre
    
    Dim Shellmouth_Storage_Next As Double=Shellmouth_Storage
    Dim Shellmouth_Storage_Peak As Double=Shellmouth_Storage_Next

    Dim j As Integer
    for j=currentIndex To currentIndex+c 'calculate SRE_storage
    Shellmouth_Storage_Next=Shellmouth_Storage_Next+(Shellmouth_LF.mnInfo.inflow(j,0)/100)+(Shellmouth_LF1.mnInfo.inflow(j,0)/100)+(Shellmouth_LF2.mnInfo.inflow(j,0)/100)+(Shellmouth_LF3.mnInfo.inflow(j,0)/100)-(Shellmouth_LD.mnInfo.nodedemand(j,0)/100)-Q2
    if Shellmouth_Storage_Next>=Shellmouth_Storage_Peak 'updating peak level
    Shellmouth_Storage_Peak=Shellmouth_Storage_Next
    Shellmouth_Storage_Peak_1=Shellmouth_Storage_Peak
    End if
    next
    if Shellmouth_Storage_Peak<Storage_1404ft and Q2<= (500*0.02831*86.4) Then
    Exit While
    elseif Shellmouth_Storage_Peak>Storage_1404ft and Shellmouth_Storage_Peak<Storage_1405ft and Q2<= (1200*0.02831*86.4) and Q2> (500*0.02831*86.4)Then
    Exit While
    elseif Shellmouth_Storage_Peak>Storage_1405ft and Shellmouth_Storage_Peak<Storage_14055ft and Q2<= (1300*0.02831*86.4) and Q2> (1200*0.02831*86.4) Then
    Exit While
    elseif Shellmouth_Storage_Peak>Storage_14055ft and Shellmouth_Storage_Peak<Storage_1406ft and Q2<= (1400*0.02831*86.4) and Q2> (1300*0.02831*86.4) Then
    Exit While
    elseif Shellmouth_Storage_Peak>Storage_1406ft and Shellmouth_Storage_Peak<Storage_14065ft and Q2<= (1500*0.02831*86.4) and Q2> (1400*0.02831*86.4) Then
    Exit While
    elseif Shellmouth_Storage_Peak>Storage_14065ft and Shellmouth_Storage_Peak<Storage_1407ft and Q2<= (1600*0.02831*86.4) and Q2> (1500*0.02831*86.4) Then
    Exit While
    End if

    Q2+=10
    End While 'while for changing Q for SRE
    
    End if  'Start date before SRE but after DD

    End if 'currIndex=0
    
    Console.WriteLine("Q1 (cfs): " & Q1/(0.02831*86.4))
    Console.WriteLine("Q2 (cfs): " & Q2/(0.02831*86.4))

    '2) When the simulation date is before DD
    if DOY>=DD_StartDOY or DOY<DD_DOY
    
    if Shellmouth_Storage<=DD_LowestDesired 'when lower than 1386 release minimum flow
    Shellmouth_Outflow_1=LB_Q1
    else
    Shellmouth_Outflow_1=Q1
    End if

    ConduitOpening=Shellmouth_Outflow_1/(0.028316847*86.4*NUM*((NUM*((Shellmouth_Level/0.3048)-1368.6))^0.5)) 'Calculated Conduit Opening
    ConduitOpening_Save.mnInfo.nodedemand(currentIndex,0)=ConduitOpening*100 'Reporting Conduit opening
    Shellmouth_Outflow.mnInfo.nodedemand(currentIndex,0)=Shellmouth_Outflow_1*100 'Defining the outflow for conduit
    Shellmouth.mnInfo.targetcontent(currentIndex,0)=(Shellmouth_Storage+Shellmouth_Inflow-Shellmouth_Outflow_1-Shellmouth_Spill1)*100
    
    End if

    '3) When the simulation date is after DD day but before SRE
    if DOY>=DD_DOY and DOY<SRE_DOY

    if Shellmouth_Storage_Peak_1>=Storage_Spill and Shellmouth_Storage>=Storage_1404ft and CurrMonth=4 'If peak storage more than spill and we are in April, set outflow as high to bring the level to 1407
    Shellmouth_Outflow_1=(5000*0.028316847*86.4)
    elseif Shellmouth_Storage_Peak_1>=Storage_Spill and (CurrMonth=5 or CurrMonth=6) 'Allow overtop with peak shaving during May and June
    Shellmouth_Outflow_1=(2000*0.028316847*86.4)-Shellmouth_Spill1
    if Shellmouth_Outflow_1<0
    Shellmouth_Outflow_1 = 0
    End if
    elseif Shellmouth_Storage<=DD_LowestDesired 'when lower than 1386 release minimum flow
    Shellmouth_Outflow_1=LB_Q2
    else
    Shellmouth_Outflow_1=Q2
    End if
    
    ConduitOpening=Shellmouth_Outflow_1/(0.028316847*86.4*Num*((Num*((Shellmouth_Level/0.3048)-1368.6))^0.5)) 'Calculated Conduit Opening
    ConduitOpening_Save.mnInfo.nodedemand(currentIndex,0)=ConduitOpening*100 'Reporting Conduit opening
    Shellmouth_Outflow.mnInfo.nodedemand(currentIndex,0)=Shellmouth_Outflow_1*100 'Defining the outflow for conduit
    Shellmouth.mnInfo.targetcontent(currentIndex,0)=(Shellmouth_Storage+Shellmouth_Inflow-Shellmouth_Outflow_1-Shellmouth_Spill1)*100

    End if
    ' SPE=Spring Peak End
    '4) When the simulation date is after SRE day but before SPE
    if DOY>=SRE_DOY and DOY<SPE_DOY

    if Shellmouth_Storage>=Storage_Spill 'Allow overtop with peak shaving during June
    Shellmouth_Outflow_1=(2000*0.028316847*86.4)-Shellmouth_Spill1
    if Shellmouth_Outflow_1<0
    Shellmouth_Outflow_1 = 0
    End if
    elseif Shellmouth_Storage>=Storage_1402ft and Shellmouth_Storage<=Storage_1404ft 'keep Q2 when in range
    Shellmouth_Outflow_1=Q2
    elseif Shellmouth_Storage>Storage_1404ft '1600 cfs when it is more than 1404
    Shellmouth_Outflow_1=1600*0.028316847*86.4
    elseif Shellmouth_Storage<=DD_LowestDesired or Shellmouth_Storage<Storage_1402ft'when lower than 1386 or 1402 release minimum flow
    Shellmouth_Outflow_1=LB_Q2
    End if
    
    ConduitOpening=Shellmouth_Outflow_1/(0.028316847*86.4*Num*((Num*((Shellmouth_Level/0.3048)-1368.6))^0.5)) 'Calculated Conduit Opening
    ConduitOpening_Save.mnInfo.nodedemand(currentIndex,0)=ConduitOpening*100 'Reporting Conduit opening
    Shellmouth_Outflow.mnInfo.nodedemand(currentIndex,0)=Shellmouth_Outflow_1*100 'Defining the outflow for conduit
    Shellmouth.mnInfo.targetcontent(currentIndex,0)=(Shellmouth_Storage+Shellmouth_Inflow-Shellmouth_Outflow_1-Shellmouth_Spill1)*100

    End if

    '5) After SPE to DD Start Day
    if DOY>=SPE_DOY and DOY<DD_StartDOY

    '5-1) Defining Outflow: Target Reservoir Level is 1400-1404
    if Shellmouth_Level<1400*0.3048
    Shellmouth_Outflow_1=25*0.028316847*86.4

    elseif Shellmouth_Level>=1400*0.3048 and Shellmouth_Level<1404*0.3048
    Shellmouth_Outflow_1=Shellmout_Outfolw_tm1
    if Shellmouth_Outflow_1<=50*0.028316847*86.4 
    Shellmouth_Outflow_1=50*0.028316847*86.4 
    End if
    if Shellmouth_Outflow_1>=500*0.028316847*86.4 
    Shellmouth_Outflow_1=500*0.028316847*86.4 
    End if

    elseif Shellmouth_Level>=1404*0.3048 and Shellmouth_Level<1405*0.3048
    Shellmouth_Outflow_1=1000*0.028316847*86.4

    elseif Shellmouth_Level>=1405*0.3048 and Shellmouth_Level<1408.5*0.3048
    Shellmouth_Outflow_1=1600*0.028316847*86.4

    elseif Shellmouth_Level>=1408.5*0.3048 and Shellmouth_Level<1410.6*0.3048
    Shellmouth_Outflow_1=(2000*0.028316847*86.4)-Shellmouth_Spill1 'Peak shaving
    if Shellmouth_Outflow_1<0
    Shellmouth_Outflow_1 = 0
    End if

    elseif Shellmouth_Level>=1410.6*0.3048
    Shellmouth_Outflow_1=Shellmouth_Storage+Shellmouth_Inflow-Storage_1410ft
    if Shellmouth_Outflow_1>=150*86.4
    Shellmouth_Outflow_1 = 150*86.4
    End if

    End if

    'if spillway overtopped and in falling limb, 1400 cfs until level>1407 and 1000 cfs until level>1405
    if Shellmouth_Level>=1408.5*0.3048 and Shellmouth_Level_Pr>Shellmouth_Level 'Check if the spill is overtopped and in falling limb
    overtopped=1
    End if
 
    if overtopped=1 and Shellmouth_Level>=1407*0.3048 and Shellmouth_Level<1408.5*0.3048
    Shellmouth_Outflow_1=1400*0.028316847*86.4
    elseif overtopped=1 and Shellmouth_Level>=1405*0.3048 and Shellmouth_Level<1407*0.3048
    Shellmouth_Outflow_1=1000*0.028316847*86.4
    End if
    
    'Min of 100 in Brandon and 200 in Holland
    if Brandon_Outflow<Min_Brandon
    Shellmouth_Outflow_1=Min_Brandon-Brandon_Outflow
    elseif Holland_Outflow<Min_Holland
    Shellmouth_Outflow_1=Min_Holland-Holland_Outflow
    End if

    ConduitOpening=Shellmouth_Outflow_1/(0.028316847*86.4*Num*((Num*((Shellmouth_Level/0.3048)-1368.6))^0.5)) 'Calculated Conduit Opening
    ConduitOpening_Save.mnInfo.nodedemand(currentIndex,0)=ConduitOpening*100 'Reporting Conduit opening
    Shellmouth_Outflow.mnInfo.nodedemand(currentIndex,0)=Shellmouth_Outflow_1*100 'Defining the outflow for conduit
    Shellmouth.mnInfo.targetcontent(currentIndex,0)=(Shellmouth_Storage+Shellmouth_Inflow-Shellmouth_Outflow_1-Shellmouth_Spill1)*100
    End if
    
    'Giving outflow from shellmouth to portage
    Poratge_Inflow=Shellmouth_Outflow.mnInfo.nodedemand(currentIndex,0)+Shellmouth_Spill.mnInfo.nodedemand(currentIndex,0)+ _
    Russell_Inflow1.mnInfo.inflow(currentIndex,0)+Russell_Inflow2.mnInfo.inflow(currentIndex,0)+ _
    Miniota_Inflow1.mnInfo.inflow(currentIndex,0)+Miniota_Inflow2.mnInfo.inflow(currentIndex,0)+ _
    Brandon_Inflow1.mnInfo.inflow(currentIndex,0)+Brandon_Inflow2.mnInfo.inflow(currentIndex,0)+ _
    Russell_Inflow3.mnInfo.inflow(currentIndex,0)+Miniota_Inflow3.mnInfo.inflow(currentIndex,0)+ _
    Brandon_Inflow3.mnInfo.inflow(currentIndex,0)
    
 '--------------------------------PortageDiversion and Floodway----------------------------------
    '1) Finding Natural Discharge At Forks and James
    Dim Natural_PortageReservoirInflow As Double=Poratge_Inflow/(100*86.4) 'cms
    Dim Natural_Southport As Double=Natural_PortageReservoirInflow+(Local_PortagetoWinnipeg.mnInfo.inflow(currentIndex,0)/(100*86.4)) 'cms
    Dim Natural_Headingly1 As Double= (NatlHeadQGivenNatPort(Natural_Southport*35.3147))/35.3147 'cms
    Dim Natural_Headingly2 As Double= (NatARCAtHeadGivenNatPort((Natural_Headingly1*35.3147),(Natural_Southport*35.3147)))/35.3147 'cms
    Dim Natural_JamesAve As Double=Natural_Headingly2+(Floodway_Inlet.mnInfo.inflow(currentIndex,0)/(100*86.4))+(Laselle.mnInfo.inflow(currentIndex,0)/(100*86.4)) _
    +(Sturgeon.mnInfo.inflow(currentIndex,0)/(100*86.4))+(Local_Winnipeg.mnInfo.inflow(currentIndex,0)/(100*86.4)) 'cms
    Dim Natural_AssiniboineAtFroks As Double=Natural_Headingly2+(Sturgeon.mnInfo.inflow(currentIndex,0)/(100*86.4))+(Local_Winnipeg.mnInfo.inflow(currentIndex,0)/(100*86.4)) 'cms
    
    if Natural_JamesAve<(10000/35.3147)   'Assuming that when Natural at James is less than 10000, there is no need for diversion and floodway!
    Poratge_Diversion.mnInfo.nodedemand(currentIndex,0)= 0.01*100
    Southport.mnInfo.nodedemand(currentIndex,0)=Poratge_Inflow
    Floodway.mnInfo.nodedemand(currentIndex,0)= 0.01*100
    RedRiver.mnInfo.nodedemand(currentIndex,0)= Floodway_Inlet.mnInfo.inflow(currentIndex,0)-Floodway.mnInfo.nodedemand(currentIndex,0)
    Console.WriteLine("No need for diversion and floodway: Natural Dischagre At Jamess < 10000 cfs")
    else
    '2) Finding The Diversion Discharge
    Dim RedRiverFloodway_Dischagre As Double

    Dim Q As Double=0
    While Q<=PortageDiversion_MaxDischarge
    
    '2-1)Finding Natural and Actual Water Level Upstream of Floodway Inlet
    Dim OC026WLn As Double = NatWL_Sep282004(Nat_JamesQArray, Nat_AssinQArray, Nat_JamesElevArray,(Natural_JamesAve*35.3147), (Natural_AssiniboineAtFroks*35.3147)) + IceShift 'ft
    Dim OC026WLa As Double = OC026WLn - Delta 'ft
    
    '2-2)Finding Floodway and Red River Discharge
    
    If OC026WLa < 750 Then
    Console.WriteLine("No need for floodway: Actual Water Level U/S of Floodway < 750 ft")
    RedRiverFloodway_Dischagre = 0.01/86.4
    Exit While
    End if
    
    RedRiverFloodway_Dischagre = (RRFInletCurve_2019(OC026WLa, 2))/35.3147 'cms
    RedRiver_Discharge=(Floodway_Inlet.mnInfo.inflow(currentIndex,0)/(100*86.4))-RedRiverFloodway_Dischagre 'cms

    '2-3)Finding Actual Discharges At Southport and Headingly 
    Actual_Southport=(Poratge_Inflow/(100*86.4))+(Local_PortagetoWinnipeg.mnInfo.inflow(currentIndex,0)/(100*86.4))-(Q/35.3147) 'cms
    Dim Actual_Headingly As Double=(Num*Southport_2Pr)+(Num*Southport_Pr)+(Num*Actual_Southport) 'cms

    '2-4)Finding James Actual Discharge and Water Level
    Dim Actual_JamesDischagre As Double= RedRiver_Discharge + (Laselle.mnInfo.inflow(currentIndex,0)/(100*86.4)) + (Sturgeon.mnInfo.inflow(currentIndex,0)/(100*86.4)) _
    + (Local_Winnipeg.mnInfo.inflow(currentIndex,0)/(100*86.4)) + Actual_Headingly 'cms

    if Actual_JamesDischagre < (20000/35.3147) 'In this condition James Level is less than 8.7 ft
    Console.WriteLine("No need for floodway: Actual Dischagre At Jamess < 20000 cfs")
    Exit While
    End if
    Actual_JamesLevel = JamesLevel(JamesQArray, RRFQArray, JamesElevArray, (Actual_JamesDischagre*35.3147),(RedRiverFloodway_Dischagre*35.3147)) - JamesShift 'ft

    '2-4)Stop Criteria
    If Actual_JamesLevel < JamesTarget_Level 

    If Actual_Southport < (Assiniboine_MinDischarge/35.3147) Then
    Console.WriteLine("Stop Criteria 1: Southport Dischagre < Assiniboine_MinDischarge")
    Exit While
    ElseIf Actual_Southport < (Assiniboine_MaxDischarge/35.3147) Then
    Console.WriteLine("Stop Criteria 2: Southport Dischagre < Assiniboine_MaxDischarge ")
    Exit While
    End if
    
    Else if Actual_JamesLevel > JamesTarget_Level and Q>=PortageDiversion_MaxDischarge Then
    
    Delta=Delta-0.1
    Console.WriteLine("Decreasing Delta")
    if Delta<-6.2
    Console.WriteLine("Highest Delta and Portage Diversion Outflow")
    Exit While
    End if
    Q=0

    End If

    Q+=10
    End WHile
    
    'Control Max Assiniboine (Actual_Soutport vs. Assiniboine_MaxDischarge). 
    'This is checked here again for the cases that there is no need for floodway
    
    if ((Poratge_Inflow/100)+(Local_PortagetoWinnipeg.mnInfo.inflow(currentIndex,0)/100))>=((Q/35.3147)*86.4)
    
    if ((Poratge_Inflow/100)+(Local_PortagetoWinnipeg.mnInfo.inflow(currentIndex,0)/100)-((Q/35.3147)*86.4))<=((Assiniboine_MaxDischarge/35.3147)*86.4)
    Poratge_Diversion.mnInfo.nodedemand(currentIndex,0)= (Q/35.3147)*86.4*100
    Southport.mnInfo.nodedemand(currentIndex,0)=((Poratge_Inflow/100)-((Q/35.3147)*86.4))*100
    else
    Poratge_Diversion.mnInfo.nodedemand(currentIndex,0)= ((Poratge_Inflow/100)+(Local_PortagetoWinnipeg.mnInfo.inflow(currentIndex,0)/100)-((Assiniboine_MaxDischarge/35.3147)*86.4))*100
    Southport.mnInfo.nodedemand(currentIndex,0)=(((Assiniboine_MaxDischarge/35.3147)*86.4)-(Local_PortagetoWinnipeg.mnInfo.inflow(currentIndex,0)/100))*100
    End if
    
    else
    Poratge_Diversion.mnInfo.nodedemand(currentIndex,0)= Poratge_Inflow
    Southport.mnInfo.nodedemand(currentIndex,0)=0.01*100
    End if

    Floodway.mnInfo.nodedemand(currentIndex,0)= (RedRiverFloodway_Dischagre)*86.4*100
    RedRiver.mnInfo.nodedemand(currentIndex,0)= Floodway_Inlet.mnInfo.inflow(currentIndex,0)-Floodway.mnInfo.nodedemand(currentIndex,0)
    End if 'if natural discharge at james is less than 10000
    
    Delta1.mnInfo.nodedemand(currentIndex,0)= Delta*100
    Actual_JamesLevel1.mnInfo.nodedemand(currentIndex,0)= Actual_JamesLevel*100
    LakeManitoba_Inflow2=Poratge_Diversion.mnInfo.nodedemand(currentIndex,0)

 '--------------------------------Lake Manitoba System--------------------------------
    'LakeWinnipegosis Target Storage
    'Step 1: Calculating Outflow(t) based on rule curves
    LakeWinnipegosis_Storage=LakeWinnipegosis.mnInfo.start/100
    Dim LakeWinnipegosis_Inflow As Double=(LakeWinnipegosis_LF.mnInfo.inflow(currentIndex,0)/100)+(LakeWinnipegosis_Inflow1.mnInfo.inflow(currentIndex,0)/100)+(LakeWinnipegosis_Inflow2.mnInfo.inflow(currentIndex,0)/100)+(LakeWinnipegosis_Inflow3.mnInfo.inflow(currentIndex,0)/100)
    Dim LakeWinnipegosis_Outflow_1 As Double
    if CurrMonth>=11 or CurrMonth<=4 Then
    LakeWinnipegosis_Outflow_1=Num*(LakeWinnipegosis_Level^3) - Num*(LakeWinnipegosis_Level^2) + Num*(LakeWinnipegosis_Level) - Num
    elseif CurrMonth>4 Then
    LakeWinnipegosis_Outflow_1=Num*(LakeWinnipegosis_Level^3) - Num*(LakeWinnipegosis_Level^2)+ Num*(LakeWinnipegosis_Level) - Num
    End if
    'Step 2: Checking Outflow(t) to be more than 0 
    Dim LakeWinnipegosis_Outflow_2 As Double
    if LakeWinnipegosis_Outflow_1>=1 Then
    LakeWinnipegosis_Outflow_2=LakeWinnipegosis_Outflow_1
    else
    LakeWinnipegosis_Outflow_2=1
    End if 
    'Step 3: Finding & Allocating The Target Storage=S(t+1)=S(t)+Inflow(t)-Outflow(t)-LocalDemand(t)
    Dim LakeWinnipegosis_Target_Storage As Double=LakeWinnipegosis_Storage+LakeWinnipegosis_Inflow-(LakeWinnipegosis_Outflow_2*86.4)-(LakeWinnipegosis_LD.mnInfo.nodedemand(currentIndex,0)/100)
    LakeWinnipegosis.mnInfo.targetcontent(currentIndex,0)=LakeWinnipegosis_Target_Storage*100
    'LakeWinnipegosis Outflow
    Dim LakeWinnipegosis_Outflow_3 As Double=LakeWinnipegosis_Storage+LakeWinnipegosis_Inflow-(LakeWinnipegosis_LD.mnInfo.nodedemand(currentIndex,0)/100)-(LakeWinnipegosis.mnInfo.targetcontent(currentIndex,0)/100)
    
    'LakeManitoba Target Storage
    'Step 1: Calculating Fairford Outflow(t) based operating rules
    LakeManitoba_Storage=LakeManitoba.mnInfo.start/100
    Dim LakeManitoba_Inflow As Double=LakeWinnipegosis_Outflow_3+(LakeManitoba_LF.mnInfo.inflow(currentIndex,0)/100)+(LakeManitoba_Inflow1.mnInfo.inflow(currentIndex,0)/100)+(LakeManitoba_Inflow2/100)
    Dim LakeManitoba_Outflow_1 As Double
    if OP=1 Then
    LakeManitoba_Outflow_1=Num*(LakeManitoba_Level^5) + Num*(LakeManitoba_Level^4) - Num*(LakeManitoba_Level^3) + Num*(LakeManitoba_Level^2) - Num*(LakeManitoba_Level) + Num
    elseif OP=2 Then
    LakeManitoba_Outflow_1=Num*(LakeManitoba_Level^5) + Num*(LakeManitoba_Level^4) - Num*(LakeManitoba_Level^3) + Num*(LakeManitoba_Level^2) - Num*(LakeManitoba_Level) + Num
    elseif OP=3 Then
    LakeManitoba_Outflow_1=Num*(LakeManitoba_Level^5) + Num*(LakeManitoba_Level^4) - Num*(LakeManitoba_Level^3) + Num*(LakeManitoba_Level^2) - Num*(LakeManitoba_Level) + Num
    elseif OP=4 Then
    LakeManitoba_Outflow_1= Num
    End if
    'Step 2: Checking Outflow(t) to be between Min and Max
    Dim LakeManitoba_Outflow_2 As Double
    if LakeManitoba_Outflow_1<=Num and LakeManitoba_Outflow_1>=Num Then 
    LakeManitoba_Outflow_2=LakeManitoba_Outflow_1
    elseif LakeManitoba_Outflow_1>Num Then
    LakeManitoba_Outflow_2=Num
    else
    LakeManitoba_Outflow_2=Num
    End if
    LakeManitoba_Fairford.mnInfo.nodedemand(currentIndex,0)=LakeManitoba_Outflow_2*86.4*100
    'Step 3: Calculating Outlet Channel Outflow(t) based operating rules
    if OutletChannels=0 Then 'include or exclude outlet channels
    LakeManitoba_OutCh.mnInfo.nodedemand(currentIndex,0)=0.001*100
    else

    if OP=1 Then
    if CurrMonth>=11 or CurrMonth<=3 Then 'For Winter
    if LakeManitoba_Level>=246.888 and LakeManitoba_Level<246.9
    LakeManitoba_OutCh.mnInfo.nodedemand(currentIndex,0)=(Num*(LakeManitoba_Level) - Num)*86.4*100
    elseif LakeManitoba_Level>=246.9 and LakeManitoba_Level<247.01
    LakeManitoba_OutCh.mnInfo.nodedemand(currentIndex,0)=(Num*(LakeManitoba_Level^2) - Num*(LakeManitoba_Level) + Num)*86.4*100
    elseif LakeManitoba_Level>=247.01 and LakeManitoba_Level<248.412
    LakeManitoba_OutCh.mnInfo.nodedemand(currentIndex,0)=(Num*(LakeManitoba_Level^3) - Num*(LakeManitoba_Level^2) + Num*(LakeManitoba_Level) - Num)*86.4*100
    elseif LakeManitoba_Level>=248.412
    LakeManitoba_OutCh.mnInfo.nodedemand(currentIndex,0)=Num*86.4*100
    End if
    elseif CurrMonth>3 Then 'For Summer
    if LakeManitoba_Level>=245.974 and LakeManitoba_Level<246.035
    LakeManitoba_OutCh.mnInfo.nodedemand(currentIndex,0)=(Num*(LakeManitoba_Level) - Num)*86.4*100
    elseif LakeManitoba_Level>=246.035 and LakeManitoba_Level<246.217
    LakeManitoba_OutCh.mnInfo.nodedemand(currentIndex,0)=(Num*(LakeManitoba_Level^3) + Num*(LakeManitoba_Level^2) - Num*(LakeManitoba_Level) + Num)*86.4*100
    elseif LakeManitoba_Level>=246.217
    LakeManitoba_OutCh.mnInfo.nodedemand(currentIndex,0)=(Num*(LakeManitoba_Level^3) - Num*(LakeManitoba_Level^2) + Num*(LakeManitoba_Level) - Num)*86.4*100
    End if
    End if
    End if

    End if
    'Step 4: Finding & Allocating The Target Storage=S(t+1)=S(t)+Inflow(t)-Outflow(t)-LocalDemand(t)
    Dim LakeManitoba_Target_Storage As Double=LakeManitoba_Storage+LakeManitoba_Inflow-(LakeManitoba_Fairford.mnInfo.nodedemand(currentIndex,0)/100)-(LakeManitoba_LD.mnInfo.nodedemand(currentIndex,0)/100)-(LakeManitoba_OutCh.mnInfo.nodedemand(currentIndex,0)/100)
    'Step 5: Checking Target Storage to be between Min and Max
    if LakeManitoba_Target_Storage<=Num and LakeManitoba_Target_Storage>=Num Then 
    LakeManitoba.mnInfo.targetcontent(currentIndex,0)=LakeManitoba_Target_Storage*100
    elseif LakeManitoba_Target_Storage>Num Then
    LakeManitoba.mnInfo.targetcontent(currentIndex,0)=Num*100
    else
    LakeManitoba.mnInfo.targetcontent(currentIndex,0)=Num*100
    End if

    'LakeManitoba Outflow
    Dim LakeManitoba_Outflow3 As Double=LakeManitoba_Storage+LakeManitoba_Inflow-(LakeManitoba_LD.mnInfo.nodedemand(currentIndex,0)/100)-(LakeManitoba.mnInfo.targetcontent(currentIndex,0)/100)

    'LakeStMartin Target Storage
    'Step 1: Calculating Outflow(t) based on operating curves
    LakeStMartin_Storage=LakeStMartin.mnInfo.start/100
    Dim LakeStMartin_Inflow As Double=LakeManitoba_Outflow3+(LakeStMartin_LF.mnInfo.inflow(currentIndex,0)/100)
    Dim LakeStMartin_Outflow_1 As Double
    if CurrMonth>=11 or CurrMonth<=3 Then 'For Winter
    LakeStMartin_Outflow_1=Num*(LakeStMartin_Level^4) + Num*(LakeStMartin_Level^3) - Num*(LakeStMartin_Level^2) + Num*(LakeStMartin_Level) - Num
    elseif CurrMonth>3 Then 'For Summer
    LakeStMartin_Outflow_1=Num*(LakeStMartin_Level^4) + Num*(LakeStMartin_Level^3) - Num*(LakeStMartin_Level^2)+ Num*(LakeStMartin_Level) - Num
    End if
    'Step 2: Checking Outflow(t) to be between Min and Max
    Dim LakeStMartin_Outflow_2 As Double
    if LakeStMartin_Outflow_1<=Num and LakeStMartin_Outflow_1>=1 Then 
    LakeStMartin_Outflow_2=LakeStMartin_Outflow_1
    elseif LakeStMartin_Outflow_1>Num Then
    LakeStMartin_Outflow_2=Num
    else
    LakeStMartin_Outflow_2=1
    End if
    LakeStMartin_Dauphin.mnInfo.nodedemand(currentIndex,0)=LakeStMartin_Outflow_2*86.4*100
    'Step 3: Calculating Outlet Channel Outflow(t) based operating rules
    if OutletChannels=0 Then 'include or exclude outlet channels
    LakeStMartin_OutCh.mnInfo.nodedemand(currentIndex,0)=0.001*100
    else

    if (OP=1 and LakeStMartin_Level>=242.926) or LakeStMartin_Level>=243.84  Then
    if CurrMonth>=11 or CurrMonth<=3 Then 'For Winter
    if LakeStMartin_Level>=241.706 and LakeStMartin_Level<242.316
    LakeStMartin_OutCh.mnInfo.nodedemand(currentIndex,0)=(Num*(LakeStMartin_Level) - Num)*86.4*100
    elseif LakeStMartin_Level>=242.316 and LakeStMartin_Level<242.621
    LakeStMartin_OutCh.mnInfo.nodedemand(currentIndex,0)=(Num*(LakeStMartin_Level) - Num)*86.4*100
    elseif LakeStMartin_Level>=242.621 and LakeStMartin_Level<243.230
    LakeStMartin_OutCh.mnInfo.nodedemand(currentIndex,0)=(Num*(LakeStMartin_Level^2) + Num*(LakeStMartin_Level) - Num)*86.4*100
    elseif LakeStMartin_Level>=243.230 and LakeStMartin_Level<244.130
    LakeStMartin_OutCh.mnInfo.nodedemand(currentIndex,0)=(Num*(LakeStMartin_Level) - Num)*86.4*100
    elseif LakeStMartin_Level>=244.130 
    LakeStMartin_OutCh.mnInfo.nodedemand(currentIndex,0)=(Num)*86.4*100
    End if
    elseif CurrMonth>3 Then 'For Summer
    if LakeStMartin_Level>=241.706 and LakeStMartin_Level<242.011
    LakeStMartin_OutCh.mnInfo.nodedemand(currentIndex,0)=(Num*(LakeStMartin_Level) - Num)*86.4*100
    elseif LakeStMartin_Level>=242.011 and LakeStMartin_Level<242.316
    LakeStMartin_OutCh.mnInfo.nodedemand(currentIndex,0)=(Num*(LakeStMartin_Level) - Num)*86.4*100
    elseif LakeStMartin_Level>=242.316 and LakeStMartin_Level<242.926
    LakeStMartin_OutCh.mnInfo.nodedemand(currentIndex,0)=(Num*(LakeStMartin_Level^2) + Num*(LakeStMartin_Level) - Num)*86.4*100
    elseif LakeStMartin_Level>=242.926 and LakeStMartin_Level<245.059
    LakeStMartin_OutCh.mnInfo.nodedemand(currentIndex,0)=(Num*(LakeStMartin_Level) - Num)*86.4*100
    elseif LakeStMartin_Level>=245.059
    LakeStMartin_OutCh.mnInfo.nodedemand(currentIndex,0)=(601.04)*86.4*100
    End if
    End if
    End if

    End if
    'Step 4: Finding & Allocating The Target Storage=S(t+1)=S(t)+Inflow(t)-Outflow(t)-LocalDemand(t)
    Dim LakeStMartin_Target_Storage As Double=LakeStMartin_Storage+LakeStMartin_Inflow-(LakeStMartin_Dauphin.mnInfo.nodedemand(currentIndex,0)/100)-(LakeStMartin_LD.mnInfo.nodedemand(currentIndex,0)/100)-(LakeStMartin_OutCh.mnInfo.nodedemand(currentIndex,0)/100)
    'Step 5: Checking Target Storage to be between Min and Max
    if LakeStMartin_Target_Storage<=Num and LakeStMartin_Target_Storage>=Num Then 
    LakeStMartin.mnInfo.targetcontent(currentIndex,0)=LakeStMartin_Target_Storage*100
    elseif LakeStMartin_Target_Storage>Num Then
    LakeStMartin.mnInfo.targetcontent(currentIndex,0)=Num*100
    else
    LakeStMartin.mnInfo.targetcontent(currentIndex,0)=Num*100
    End if

 '---------------------------------Winnipeg River basin----------------------------------------
    'Lake StJoseph Target Storage 
    'Step 1: Calculating Outflow(t)=a+(b*DOY(t))+(c*Storage(t))+(d*inflow(t))+(e*outflow(t-1))
    StJoseph_Storage=StJoseph.mnInfo.start/100  
    Dim StJoseph_Inflow As Double=StJoseph_LF.mnInfo.inflow(currentIndex,0)/100+CatRiver.mnInfo.inflow(currentIndex,0)/100
    Dim StJoseph_Outflow_1 As Double
    if CurrMonth=1 Then
    StJoseph_Outflow_1=a_ST_1+(b_ST_1*DOY)+(c_ST_1*StJoseph_Storage)+(d_ST_1*(StJoseph_Inflow/86.4))+(e_ST_1*StJoseph_Outflow_PrvStep)
    elseif CurrMonth=2 Then
    StJoseph_Outflow_1=a_ST_2+(b_ST_2*DOY)+(c_ST_2*StJoseph_Storage)+(d_ST_2*(StJoseph_Inflow/86.4))+(e_ST_2*StJoseph_Outflow_PrvStep)
    elseif CurrMonth=3 Then
    StJoseph_Outflow_1=a_ST_3+(b_ST_3*DOY)+(c_ST_3*StJoseph_Storage)+(d_ST_3*(StJoseph_Inflow/86.4))+(e_ST_3*StJoseph_Outflow_PrvStep)
    elseif CurrMonth=4 Then
    StJoseph_Outflow_1=a_ST_4+(b_ST_4*DOY)+(c_ST_4*StJoseph_Storage)+(d_ST_4*(StJoseph_Inflow/86.4))+(e_ST_4*StJoseph_Outflow_PrvStep)
    elseif CurrMonth=5 Then
    StJoseph_Outflow_1=a_ST_5+(b_ST_5*DOY)+(c_ST_5*StJoseph_Storage)+(d_ST_5*(StJoseph_Inflow/86.4))+(e_ST_5*StJoseph_Outflow_PrvStep)
    elseif CurrMonth=6 Then
    StJoseph_Outflow_1=a_ST_6+(b_ST_6*DOY)+(c_ST_6*StJoseph_Storage)+(d_ST_6*(StJoseph_Inflow/86.4))+(e_ST_6*StJoseph_Outflow_PrvStep)
    elseif CurrMonth=7 Then
    StJoseph_Outflow_1=a_ST_7+(b_ST_7*DOY)+(c_ST_7*StJoseph_Storage)+(d_ST_7*(StJoseph_Inflow/86.4))+(e_ST_7*StJoseph_Outflow_PrvStep)
    elseif CurrMonth=8 Then
    StJoseph_Outflow_1=a_ST_8+(b_ST_8*DOY)+(c_ST_8*StJoseph_Storage)+(d_ST_8*(StJoseph_Inflow/86.4))+(e_ST_8*StJoseph_Outflow_PrvStep)
    elseif CurrMonth=9 Then
    StJoseph_Outflow_1=a_ST_9+(b_ST_9*DOY)+(c_ST_9*StJoseph_Storage)+(d_ST_9*(StJoseph_Inflow/86.4))+(e_ST_9*StJoseph_Outflow_PrvStep)
    elseif CurrMonth=10 Then
    StJoseph_Outflow_1=a_ST_10+(b_ST_10*DOY)+(c_ST_10*StJoseph_Storage)+(d_ST_10*(StJoseph_Inflow/86.4))+(e_ST_10*StJoseph_Outflow_PrvStep)
    elseif CurrMonth=11 Then
    StJoseph_Outflow_1=a_ST_11+(b_ST_11*DOY)+(c_ST_11*StJoseph_Storage)+(d_ST_11*(StJoseph_Inflow/86.4))+(e_ST_11*StJoseph_Outflow_PrvStep)
    elseif CurrMonth=12 Then
    StJoseph_Outflow_1=a_ST_12+(b_ST_12*DOY)+(c_ST_12*StJoseph_Storage)+(d_ST_12*(StJoseph_Inflow/86.4))+(e_ST_12*StJoseph_Outflow_PrvStep)
    End if
    'Step 2: Checking Outflow(t) to be between Min and Max
    Dim StJoseph_Outflow2 As Double
    if StJoseph_Outflow_1<=Num and StJoseph_Outflow_1>=Num Then
    StJoseph_Outflow2=StJoseph_Outflow_1
    elseif StJoseph_Outflow_1>Num Then
    StJoseph_Outflow2=Num
    else
    StJoseph_Outflow2=Num
    End if
    'Step 3: Finding & Allocating The Target Storage=S(t+1)=S(t)+Inflow(t)-Outflow(t)-LocalDemand(t)
    Dim StJoseph_Target_Storage As Double=StJoseph_Storage+StJoseph_Inflow-(StJoseph_Outflow2*86.4)-(StJoseph_LD.mnInfo.nodedemand(currentIndex,0)/100)
    'Step 4: Checking Target Storage to be between Min and Max
    if StJoseph_Target_Storage<=Num and StJoseph_Target_Storage>=Num Then
    StJoseph.mnInfo.targetcontent(currentIndex,0)=StJoseph_Target_Storage*100
    elseif StJoseph_Target_Storage>Num Then
    StJoseph.mnInfo.targetcontent(currentIndex,0)=Num*100
    else
    StJoseph.mnInfo.targetcontent(currentIndex,0)=Num*100
    End if

    'Lake StJoseph: Root and Rat Outflow (Dependent to Storage (t) of StJoseph and Storage (t-1) of LacSeul)
    Dim StJoseph_Outflow3 As Double=(StJoseph_Storage+StJoseph_Inflow-(StJoseph_LD.mnInfo.nodedemand(currentIndex,0)/100)-(StJoseph.mnInfo.targetcontent(currentIndex,0)/100))/86.4
    if StJoseph_Storage>Num or LacSeul_Storage_PrvStep>Num Then
    RatRapid_De.mnInfo.nodedemand(currentIndex,0)=(StJoseph_Outflow3-10)*86.4*100
    elseif StJoseph_Outflow3<=Num and StJoseph_Outflow3>=Num Then
    RootRiver_De.mnInfo.nodedemand(currentIndex,0)=StJoseph_Outflow3*86.4*100
    elseif StJoseph_Outflow3>Num Then
    RootRiver_De.mnInfo.nodedemand(currentIndex,0)=Num*86.4*100
    RatRapid_De.mnInfo.nodedemand(currentIndex,0)=(StJoseph_Outflow3-Num)*86.4*100
    End if

    'Outflow to LacSeul'
    Dim StJoseph_Outflow4 As Double=RootRiver_De.mnInfo.nodedemand(currentIndex,0)/100

    'LacSeul Target Storage 
    'Step 1: Calculating Outflow(t)=a+(b*DOY(t))+(c*Storage(t))+(d*inflow(t))+(e*outflow(t-1))
    LacSeul_Storage=LacSeul.mnInfo.start/100
    Dim LacSeul_Inflow As Double=(StJoseph_Outflow4)+(LacSeul_LF.mnInfo.inflow(currentIndex,0)/100)+(LacSeul_Inflow1.mnInfo.inflow(currentIndex,0)/100)+(LacSeul_Inflow2.mnInfo.inflow(currentIndex,0)/100)
    Dim LacSeul_Outflow_1 As Double
    if CurrMonth=1 Then
    LacSeul_Outflow_1=a_LS_1+(b_LS_1*DOY)+(c_LS_1*LacSeul_Storage)+(d_LS_1*(LacSeul_Inflow/86.4))+(e_LS_1*LacSeul_Outflow_PrvStep)
    elseif CurrMonth=2 Then
    LacSeul_Outflow_1=a_LS_2+(b_LS_2*DOY)+(c_LS_2*LacSeul_Storage)+(d_LS_2*(LacSeul_Inflow/86.4))+(e_LS_2*LacSeul_Outflow_PrvStep)
    elseif CurrMonth=3 Then
    LacSeul_Outflow_1=a_LS_3+(b_LS_3*DOY)+(c_LS_3*LacSeul_Storage)+(d_LS_3*(LacSeul_Inflow/86.4))+(e_LS_3*LacSeul_Outflow_PrvStep)
    elseif CurrMonth=4 and CurrDay<16 Then
    LacSeul_Outflow_1=a_LS_4+(b_LS_4*DOY)+(c_LS_4*LacSeul_Storage)+(d_LS_4*(LacSeul_Inflow/86.4))+(e_LS_4*LacSeul_Outflow_PrvStep)
    elseif CurrMonth=4 and CurrDay>15 Then
    LacSeul_Outflow_1=a_LS_5+(b_LS_5*DOY)+(c_LS_5*LacSeul_Storage)+(d_LS_5*(LacSeul_Inflow/86.4))+(e_LS_5*LacSeul_Outflow_PrvStep)
    elseif CurrMonth=5 Then
    LacSeul_Outflow_1=a_LS_6+(b_LS_6*DOY)+(c_LS_6*LacSeul_Storage)+(d_LS_6*(LacSeul_Inflow/86.4))+(e_LS_6*LacSeul_Outflow_PrvStep)
    elseif CurrMonth=6 Then
    LacSeul_Outflow_1=a_LS_7+(b_LS_7*DOY)+(c_LS_7*LacSeul_Storage)+(d_LS_7*(LacSeul_Inflow/86.4))+(e_LS_7*LacSeul_Outflow_PrvStep)
    elseif CurrMonth=7 Then
    LacSeul_Outflow_1=a_LS_8+(b_LS_8*DOY)+(c_LS_8*LacSeul_Storage)+(d_LS_8*(LacSeul_Inflow/86.4))+(e_LS_8*LacSeul_Outflow_PrvStep)
    elseif CurrMonth=8 Then
    LacSeul_Outflow_1=a_LS_9+(b_LS_9*DOY)+(c_LS_9*LacSeul_Storage)+(d_LS_9*(LacSeul_Inflow/86.4))+(e_LS_9*LacSeul_Outflow_PrvStep)
    elseif CurrMonth=9 Then
    LacSeul_Outflow_1=a_LS_10+(b_LS_10*DOY)+(c_LS_10*LacSeul_Storage)+(d_LS_10*(LacSeul_Inflow/86.4))+(e_LS_10*LacSeul_Outflow_PrvStep)
    elseif CurrMonth=10 Then
    LacSeul_Outflow_1=a_LS_11+(b_LS_11*DOY)+(c_LS_11*LacSeul_Storage)+(d_LS_11*(LacSeul_Inflow/86.4))+(e_LS_11*LacSeul_Outflow_PrvStep)
    elseif CurrMonth=11 Then
    LacSeul_Outflow_1=a_LS_12+(b_LS_12*DOY)+(c_LS_12*LacSeul_Storage)+(d_LS_12*(LacSeul_Inflow/86.4))+(e_LS_12*LacSeul_Outflow_PrvStep)
    elseif CurrMonth=12 Then
    LacSeul_Outflow_1=a_LS_13+(b_LS_13*DOY)+(c_LS_13*LacSeul_Storage)+(d_LS_13*(LacSeul_Inflow/86.4))+(e_LS_13*LacSeul_Outflow_PrvStep)
    End if
    'Step 2: Checking Outflow(t) to be between Min and Max
    Dim LacSeul_Outflow_2 As Double
    if LacSeul_Outflow_1<=Num and LacSeul_Outflow_1>=Num Then
    LacSeul_Outflow_2=LacSeul_Outflow_1
    elseif LacSeul_Outflow_1>Num Then
    LacSeul_Outflow_2=Num
    else
    LacSeul_Outflow_2=Num
    End if
    'Step 3: Finding & Allocating The Target Storage=S(t+1)=S(t)+Inflow(t)-Outflow(t)-LocalDemand(t)
    Dim LacSeul_Target_Storage As Double=LacSeul_Storage+LacSeul_Inflow-(LacSeul_Outflow_2*86.4)-(LacSeul_LD.mnInfo.nodedemand(currentIndex,0)/100)
    'Step 4: Checking Target Storage to be between Min and Max
    if LacSeul_Target_Storage<=Num and LacSeul_Target_Storage>=Num Then
    LacSeul.mnInfo.targetcontent(currentIndex,0)=LacSeul_Target_Storage*100
    elseif LacSeul_Target_Storage>Num Then
    LacSeul.mnInfo.targetcontent(currentIndex,0)=Num*100
    else
    LacSeul.mnInfo.targetcontent(currentIndex,0)=Num*100
    End if

    'LacSeul Outflow
    Dim LacSeul_Outflow3 As Double=LacSeul_Storage+LacSeul_Inflow-(LacSeul_LD.mnInfo.nodedemand(currentIndex,0)/100)-(LacSeul.mnInfo.targetcontent(currentIndex,0)/100)

    'Pakwash Target Storage
    'Step 1: Calculating Outflow(t)=a+(b*DOY(t))+(c*Storage(t))+(d*inflow(t))+(e*outflow(t-1))
    Pakwash_Storage=Pakwash.mnInfo.start/100
    Dim Pakwash_Inflow As Double=LacSeul_Outflow3+((Pakwash_Inflow1.mnInfo.inflow(currentIndex,0)+Pakwash_Inflow2.mnInfo.inflow(currentIndex,0)+Pakwash_Inflow3.mnInfo.inflow(currentIndex,0))/100)
    Dim Pakwash_Outflow_1 As Double
    Pakwash_Outflow_1=a_PW+(b_PW*DOY)+(c_PW*Pakwash_Storage)+(d_PW*(Pakwash_Inflow/86.4))+(e_PW*Pakwash_Outflow_PrvStep)
    'Step 2: Checking Outflow(t) to be between Min and Max
    Dim Pakwash_Outflow_2 As Double
    if Pakwash_Outflow_1<=Num and Pakwash_Outflow_1>=Num Then
    Pakwash_Outflow_2=Pakwash_Outflow_1
    elseif Pakwash_Outflow_1>Num Then
    Pakwash_Outflow_2=Num
    else
    Pakwash_Outflow_2=Num
    End if
    'Step 3: Finding & Allocating The Target Storage=S(t+1)=S(t)+Inflow(t)-Outflow(t)-LocalDemand(t)
    Dim Pakwash_Target_Storage As Double=Pakwash_Storage+Pakwash_Inflow-(Pakwash_Outflow_2*86.4)
    'Step 4: Checking Target Storage to be between Min and Max
    if Pakwash_Target_Storage<=Num and Pakwash_Target_Storage>=Num Then
    Pakwash.mnInfo.targetcontent(currentIndex,0)=Pakwash_Target_Storage*100
    elseif Pakwash_Target_Storage>Num Then
    Pakwash.mnInfo.targetcontent(currentIndex,0)=Num*100
    else
    Pakwash.mnInfo.targetcontent(currentIndex,0)=Num*100
    End if

    'Pakwash Outflow
    Dim Pakwash_Outflow3 As Double=Pakwash_Storage+Pakwash_Inflow-(Pakwash.mnInfo.targetcontent(currentIndex,0)/100)

    'Ball Target Storage 
    'Step 1: Calculating Outflow(t)=a+(b*DOY(t))+(c*Storage(t))+(d*inflow(t))+(e*outflow(t-1))
    Ball_Storage=Ball.mnInfo.start/100
    Dim Ball_Inflow As Double=Pakwash_Outflow3+((Ball_Inflow1.mnInfo.inflow(currentIndex,0)+Ball_Inflow2.mnInfo.inflow(currentIndex,0)+Ball_Inflow3.mnInfo.inflow(currentIndex,0))/100)
    Dim Ball_Outflow_1 As Double
    Ball_Outflow_1=a_Ba+(b_Ba*DOY)+(c_Ba*Ball_Storage)+(d_Ba*(Ball_Inflow/86.4))+(e_Ba*Ball_Outflow_PrvStep)
    'Step 2: Checking Outflow(t) to be between Min and Max
    Dim Ball_Outflow_2 As Double
    if Ball_Outflow_1<=Num and Ball_Outflow_1>=Num Then
    Ball_Outflow_2=Ball_Outflow_1
    elseif Ball_Outflow_1>Num Then
    Ball_Outflow_2=Num
    else
    Ball_Outflow_2=Num
    End if
    'Step 3: Finding & Allocating The Target Storage=S(t+1)=S(t)+Inflow(t)-Outflow(t)-LocalDemand(t)
    Dim Ball_Target_Storage As Double=Ball_Storage+Ball_Inflow-(Ball_Outflow_2*86.4)
    'Step 4: Checking Target Storage to be between Min and Max
    if Ball_Target_Storage<=Num and Ball_Target_Storage>=Num Then
    Ball.mnInfo.targetcontent(currentIndex,0)=Ball_Target_Storage*100
    elseif Ball_Target_Storage>Num Then
    Ball.mnInfo.targetcontent(currentIndex,0)=Num*100
    else
    Ball.mnInfo.targetcontent(currentIndex,0)=Num*100
    End if
    
    'Ball Outflow
    Dim Ball_Outflow3 As Double=Ball_Storage+Ball_Inflow-(Ball.mnInfo.targetcontent(currentIndex,0)/100)

    'Separation Target Storage 
    'Step 1: Calculating Outflow(t)=a+(b*DOY(t))+(c*Storage(t))+(d*inflow(t))+(e*outflow(t-1))
    Separation_Storage=Separation.mnInfo.start/100
    Dim Separation_Inflow As Double=Ball_Outflow3+(Separation_Inflow1.mnInfo.inflow(currentIndex,0)/100)
    Dim Separation_Outflow_1 As Double
    Separation_Outflow_1=a_SP+(b_SP*DOY)+(c_SP*Separation_Storage)+(d_SP*(Separation_Inflow/86.4))+(e_SP*Separation_Outflow_PrvStep)
    'Step 2: Checking Outflow(t) to be between Min and Max
    Dim Separation_Outflow_2 As Double
    if Separation_Outflow_1<=Num and Separation_Outflow_1>=Num Then
    Separation_Outflow_2=Separation_Outflow_1
    elseif Separation_Outflow_1>Num Then
    Separation_Outflow_2=Num
    else
    Separation_Outflow_2=Num
    End if
    'Step 3: Finding & Allocating The Target Storage=S(t+1)=S(t)+Inflow(t)-Outflow(t)-LocalDemand(t)
    Dim Separation_Target_Storage As Double=Separation_Storage+Separation_Inflow-(Separation_Outflow_2*86.4)
    'Step 4: Checking Target Storage to be between Min and Max
    if Separation_Target_Storage<=Num and Separation_Target_Storage>=Num Then
    Separation.mnInfo.targetcontent(currentIndex,0)=Separation_Target_Storage*100
    elseif Separation_Target_Storage>Num Then
    Separation.mnInfo.targetcontent(currentIndex,0)=Num*100
    else
    Separation.mnInfo.targetcontent(currentIndex,0)=Num*100
    End if
    
    'Separation Outflow
    Dim Separation_Outflow3 As Double=Separation_Storage+Separation_Inflow-(Separation.mnInfo.targetcontent(currentIndex,0)/100)

    'Umfreville Target Storage 
    'Step 1: Calculating Outflow(t)=a+(b*DOY(t))+(c*Storage(t))+(d*inflow(t))+(e*outflow(t-1))
    Umfreville_Storage=Umfreville.mnInfo.start/100
    Dim Umfreville_Inflow As Double=Separation_Outflow3+(Umfreville_Inflow1.mnInfo.inflow(currentIndex,0)/100)
    Dim Umfreville_Outflow_1 As Double
    Umfreville_Outflow_1=a_UF+(b_UF*DOY)+(c_UF*Umfreville_Storage)+(d_UF*(Umfreville_Inflow/86.4))+(e_UF*Umfreville_Outflow_PrvStep)
    'Step 2: Checking Outflow(t) to be between Min and Max
    Dim Umfreville_Outflow_2 As Double
    if Umfreville_Outflow_1<=Num and Umfreville_Outflow_1>=Num Then
    Umfreville_Outflow_2=Umfreville_Outflow_1
    elseif Umfreville_Outflow_1>Num Then
    Umfreville_Outflow_2=Num
    else
    Umfreville_Outflow_2=Num
    End if
    'Step 3: Finding & Allocating The Target Storage=S(t+1)=S(t)+Inflow(t)-Outflow(t)-LocalDemand(t)
    Dim Umfreville_Target_Storage As Double=Umfreville_Storage+Umfreville_Inflow-(Umfreville_Outflow_2*86.4)
    'Step 4: Checking Target Storage to be between Min and Max
    if Umfreville_Target_Storage<=Num and Umfreville_Target_Storage>=Num Then
    Umfreville.mnInfo.targetcontent(currentIndex,0)=Umfreville_Target_Storage*100
    elseif Umfreville_Target_Storage>Num Then
    Umfreville.mnInfo.targetcontent(currentIndex,0)=Num*100
    else
    Umfreville.mnInfo.targetcontent(currentIndex,0)=Num*100
    End if
    
    'Umfreville Outflow
    Dim Umfreville_Outflow3 As Double=Umfreville_Storage+Umfreville_Inflow-(Umfreville.mnInfo.targetcontent(currentIndex,0)/100)

    'LacLaCroix Target Storage 
    'Step 1: Calculating Outflow(t)=a+(b*DOY(t))+(c*Storage(t))+(d*inflow(t))+(e*outflow(t-1))
    LacLaCroix_Storage=LacLaCroix.mnInfo.start/100
    Dim LacLaCroix_Inflow As Double=LacLaCroix_Inflow1.mnInfo.inflow(currentIndex,0)/100
    Dim LacLaCroix_Outflow_1 As Double
    LacLaCroix_Outflow_1=a_LC+(b_LC*DOY)+(c_LC*LacLaCroix_Storage)+(d_LC*(LacLaCroix_Inflow/86.4))+(e_LC*LacLaCroix_Outflow_PrvStep)
    'Step 2: Checking Outflow(t) to be between Min and Max
    Dim LacLaCroix_Outflow_2 As Double
    if LacLaCroix_Outflow_1<=Num and LacLaCroix_Outflow_1>=Num Then
    LacLaCroix_Outflow_2=LacLaCroix_Outflow_1
    elseif LacLaCroix_Outflow_1>Num Then
    LacLaCroix_Outflow_2=Num
    else
    LacLaCroix_Outflow_2=Num
    End if
    'Step 3: Finding & Allocating The Target Storage=S(t+1)=S(t)+Inflow(t)-Outflow(t)-LocalDemand(t)
    Dim LacLaCroix_Target_Storage As Double=LacLaCroix_Storage+LacLaCroix_Inflow-(LacLaCroix_Outflow_2*86.4)
    'Step 4: Checking Target Storage to be between Min and Max
    if LacLaCroix_Target_Storage<=Num and LacLaCroix_Target_Storage>=Num Then
    LacLaCroix.mnInfo.targetcontent(currentIndex,0)=LacLaCroix_Target_Storage*100
    elseif LacLaCroix_Target_Storage>Num Then
    LacLaCroix.mnInfo.targetcontent(currentIndex,0)=Num*100
    else
    LacLaCroix.mnInfo.targetcontent(currentIndex,0)=Num*100
    End if

    ''LacLaCroix Outflow
    Dim LacLaCroix_Outflow3 As Double=LacLaCroix_Storage+LacLaCroix_Inflow-(LacLaCroix.mnInfo.targetcontent(currentIndex,0)/100)

    'Namakan Target Storage 
    'Step 1: Calculating Outflow(t)=a+(b*DOY(t))+(c*Storage(t))+(d*inflow(t))+(e*outflow(t-1))
    Namakan_Storage=Namakan.mnInfo.start/100
    Dim Namakan_Inflow As Double=LacLaCroix_Outflow3+(Namakan_LF.mnInfo.inflow(currentIndex,0)/100)
    Dim Namakan_Outflow_1 As Double
    if CurrMonth=1 Then
    Namakan_Outflow_1=a_NK_1+(b_NK_1*DOY)+(c_NK_1*Namakan_Storage)+(d_NK_1*(Namakan_Inflow/86.4))+(e_NK_1*Namakan_Outflow_PrvStep)
    elseif CurrMonth=2 Then
    Namakan_Outflow_1=a_NK_2+(b_NK_2*DOY)+(c_NK_2*Namakan_Storage)+(d_NK_2*(Namakan_Inflow/86.4))+(e_NK_2*Namakan_Outflow_PrvStep)
    elseif CurrMonth=3 Then
    Namakan_Outflow_1=a_NK_3+(b_NK_3*DOY)+(c_NK_3*Namakan_Storage)+(d_NK_3*(Namakan_Inflow/86.4))+(e_NK_3*Namakan_Outflow_PrvStep)
    elseif CurrMonth=4
    Namakan_Outflow_1=a_NK_4+(b_NK_4*DOY)+(c_NK_4*Namakan_Storage)+(d_NK_4*(Namakan_Inflow/86.4))+(e_NK_4*Namakan_Outflow_PrvStep)
    elseif CurrMonth=5
    Namakan_Outflow_1=a_NK_5+(b_NK_5*DOY)+(c_NK_5*Namakan_Storage)+(d_NK_5*(Namakan_Inflow/86.4))+(e_NK_5*Namakan_Outflow_PrvStep)
    elseif CurrMonth=6 Then
    Namakan_Outflow_1=a_NK_6+(b_NK_6*DOY)+(c_NK_6*Namakan_Storage)+(d_NK_6*(Namakan_Inflow/86.4))+(e_NK_6*Namakan_Outflow_PrvStep)
    elseif CurrMonth=7 Then
    Namakan_Outflow_1=a_NK_7+(b_NK_7*DOY)+(c_NK_7*Namakan_Storage)+(d_NK_7*(Namakan_Inflow/86.4))+(e_NK_7*Namakan_Outflow_PrvStep)
    elseif CurrMonth=8 Then
    Namakan_Outflow_1=a_NK_8+(b_NK_8*DOY)+(c_NK_8*Namakan_Storage)+(d_NK_8*(Namakan_Inflow/86.4))+(e_NK_8*Namakan_Outflow_PrvStep)
    elseif CurrMonth=9 Then
    Namakan_Outflow_1=a_NK_9+(b_NK_9*DOY)+(c_NK_9*Namakan_Storage)+(d_NK_9*(Namakan_Inflow/86.4))+(e_NK_9*Namakan_Outflow_PrvStep)
    elseif CurrMonth=10 Then
    Namakan_Outflow_1=a_NK_10+(b_NK_10*DOY)+(c_NK_10*Namakan_Storage)+(d_NK_10*(Namakan_Inflow/86.4))+(e_NK_10*Namakan_Outflow_PrvStep)
    elseif CurrMonth=11 Then
    Namakan_Outflow_1=a_NK_11+(b_NK_11*DOY)+(c_NK_11*Namakan_Storage)+(d_NK_11*(Namakan_Inflow/86.4))+(e_NK_11*Namakan_Outflow_PrvStep)
    elseif CurrMonth=12 Then
    Namakan_Outflow_1=a_NK_12+(b_NK_12*DOY)+(c_NK_12*Namakan_Storage)+(d_NK_12*(Namakan_Inflow/86.4))+(e_NK_12*Namakan_Outflow_PrvStep)
    End if
    'Step 2: Checking Outflow(t) to be between Min and Max
    Dim Namakan_Outflow_2 As Double
    if Namakan_Outflow_1<=Num and Namakan_Outflow_1>=Num Then
    Namakan_Outflow_2=Namakan_Outflow_1
    elseif Namakan_Outflow_1>Num Then
    Namakan_Outflow_2=Num
    else
    Namakan_Outflow_2=Num
    End if
    'Step 3: Finding & Allocating The Target Storage=S(t+1)=S(t)+Inflow(t)-Outflow(t)-LocalDemand(t)
    Dim Namakan_Target_Storage As Double=Namakan_Storage+Namakan_Inflow-(Namakan_Outflow_2*86.4)-(Namakan_LD.mnInfo.nodedemand(currentIndex,0)/100)
    'Step 4: Checking Target Storage to be between Min and Max
    if Namakan_Target_Storage<=Num and Namakan_Target_Storage>=Num Then
    Namakan.mnInfo.targetcontent(currentIndex,0)=Namakan_Target_Storage*100
    elseif Namakan_Target_Storage>Num Then
    Namakan.mnInfo.targetcontent(currentIndex,0)=Num*100
    else
    Namakan.mnInfo.targetcontent(currentIndex,0)=Num*100
    End if

    'Namakan Outflow
    Dim Namakan_Outflow3 As Double=Namakan_Storage+Namakan_Inflow-(Namakan_LD.mnInfo.nodedemand(currentIndex,0)/100)-(Namakan.mnInfo.targetcontent(currentIndex,0)/100)

    'Rainy Target Storage 
    'Step 1: Calculating Outflow(t)=a+(b*DOY(t))+(c*Storage(t))+(d*inflow(t))+(e*outflow(t-1))
    Rainy_Storage=Rainy.mnInfo.start/100
    Dim Rainy_Inflow As Double=Namakan_Outflow3+((Rainy_LF.mnInfo.inflow(currentIndex,0)+Rainy_Inflow1.mnInfo.inflow(currentIndex,0))/100)
    Dim Rainy_Outflow_1 As Double
    if CurrMonth=1 Then
    Rainy_Outflow_1=a_RL_1+(b_RL_1*DOY)+(c_RL_1*Rainy_Storage)+(d_RL_1*(Rainy_Inflow/86.4))+(e_RL_1*Rainy_Outflow_PrvStep)
    elseif CurrMonth=2 Then
    Rainy_Outflow_1=a_RL_2+(b_RL_2*DOY)+(c_RL_2*Rainy_Storage)+(d_RL_2*(Rainy_Inflow/86.4))+(e_RL_2*Rainy_Outflow_PrvStep)
    elseif CurrMonth=3 Then
    Rainy_Outflow_1=a_RL_3+(b_RL_3*DOY)+(c_RL_3*Rainy_Storage)+(d_RL_3*(Rainy_Inflow/86.4))+(e_RL_3*Rainy_Outflow_PrvStep)
    elseif CurrMonth=4
    Rainy_Outflow_1=a_RL_4+(b_RL_4*DOY)+(c_RL_4*Rainy_Storage)+(d_RL_4*(Rainy_Inflow/86.4))+(e_RL_4*Rainy_Outflow_PrvStep)
    elseif CurrMonth=5
    Rainy_Outflow_1=a_RL_5+(b_RL_5*DOY)+(c_RL_5*Rainy_Storage)+(d_RL_5*(Rainy_Inflow/86.4))+(e_RL_5*Rainy_Outflow_PrvStep)
    elseif CurrMonth=6 Then
    Rainy_Outflow_1=a_RL_6+(b_RL_6*DOY)+(c_RL_6*Rainy_Storage)+(d_RL_6*(Rainy_Inflow/86.4))+(e_RL_6*Rainy_Outflow_PrvStep)
    elseif CurrMonth=7 Then
    Rainy_Outflow_1=a_RL_7+(b_RL_7*DOY)+(c_RL_7*Rainy_Storage)+(d_RL_7*(Rainy_Inflow/86.4))+(e_RL_7*Rainy_Outflow_PrvStep)
    elseif CurrMonth=8 Then
    Rainy_Outflow_1=a_RL_8+(b_RL_8*DOY)+(c_RL_8*Rainy_Storage)+(d_RL_8*(Rainy_Inflow/86.4))+(e_RL_8*Rainy_Outflow_PrvStep)
    elseif CurrMonth=9 Then
    Rainy_Outflow_1=a_RL_9+(b_RL_9*DOY)+(c_RL_9*Rainy_Storage)+(d_RL_9*(Rainy_Inflow/86.4))+(e_RL_9*Rainy_Outflow_PrvStep)
    elseif CurrMonth=10 Then
    Rainy_Outflow_1=a_RL_10+(b_RL_10*DOY)+(c_RL_10*Rainy_Storage)+(d_RL_10*(Rainy_Inflow/86.4))+(e_RL_10*Rainy_Outflow_PrvStep)
    elseif CurrMonth=11 Then
    Rainy_Outflow_1=a_RL_11+(b_RL_11*DOY)+(c_RL_11*Rainy_Storage)+(d_RL_11*(Rainy_Inflow/86.4))+(e_RL_11*Rainy_Outflow_PrvStep)
    elseif CurrMonth=12 Then
    Rainy_Outflow_1=a_RL_12+(b_RL_12*DOY)+(c_RL_12*Rainy_Storage)+(d_RL_12*(Rainy_Inflow/86.4))+(e_RL_12*Rainy_Outflow_PrvStep)
    End if
    'Step 2: Checking Outflow(t) to be between Min and Max
    Dim Rainy_Outflow_2 As Double
    if Rainy_Outflow_1<=Num and Rainy_Outflow_1>=Num Then
    Rainy_Outflow_2=Rainy_Outflow_1
    elseif Rainy_Outflow_1>Num Then
    Rainy_Outflow_2=Num
    else
    Rainy_Outflow_2=Num
    End if
    'Step 3: Finding & Allocating The Target Storage=S(t+1)=S(t)+Inflow(t)-Outflow(t)-LocalDemand(t)
    Dim Rainy_Target_Storage As Double=Rainy_Storage+Rainy_Inflow-(Rainy_Outflow_2*86.4)-(Rainy_LD.mnInfo.nodedemand(currentIndex,0)/100)
    'Step 4: Checking Target Storage to be between Min and Max
    if Rainy_Target_Storage<=Num and Rainy_Target_Storage>=Num Then
    Rainy.mnInfo.targetcontent(currentIndex,0)=Rainy_Target_Storage*100
    elseif Rainy_Target_Storage>Num Then
    Rainy.mnInfo.targetcontent(currentIndex,0)=Num*100
    else
    Rainy.mnInfo.targetcontent(currentIndex,0)=Num*100
    End if

    'Rainy Outflow
    Dim Rainy_Outflow3 As Double=Rainy_Storage+Rainy_Inflow-(Rainy_LD.mnInfo.nodedemand(currentIndex,0)/100)-(Rainy.mnInfo.targetcontent(currentIndex,0)/100)

    'Lake of the Woods Target Storage 
    'Step 1: Calculating Outflow(t)=a+(b*DOY(t))+(c*Storage(t))+(d*inflow(t))+(e*outflow(t-1))
    Woods_Storage=Woods.mnInfo.start/100
    Dim Woods_Inflow As Double=Rainy_Outflow3+((Woods_LF.mnInfo.inflow(currentIndex,0)+Woods_Inflow1.mnInfo.inflow(currentIndex,0)+Woods_Inflow2.mnInfo.inflow(currentIndex,0))/100)
    Dim Woods_Outflow_1 As Double
    if CurrMonth=1 Then
    Woods_Outflow_1=a_LW_1+(b_LW_1*DOY)+(c_LW_1*Woods_Storage)+(d_LW_1*(Woods_Inflow/86.4))+(e_LW_1*Woods_Outflow_PrvStep)
    elseif CurrMonth=2 Then
    Woods_Outflow_1=a_LW_2+(b_LW_2*DOY)+(c_LW_2*Woods_Storage)+(d_LW_2*(Woods_Inflow/86.4))+(e_LW_2*Woods_Outflow_PrvStep)
    elseif CurrMonth=3 Then
    Woods_Outflow_1=a_LW_3+(b_LW_3*DOY)+(c_LW_3*Woods_Storage)+(d_LW_3*(Woods_Inflow/86.4))+(e_LW_3*Woods_Outflow_PrvStep)
    elseif CurrMonth=4
    Woods_Outflow_1=a_LW_4+(b_LW_4*DOY)+(c_LW_4*Woods_Storage)+(d_LW_4*(Woods_Inflow/86.4))+(e_LW_4*Woods_Outflow_PrvStep)
    elseif CurrMonth=5
    Woods_Outflow_1=a_LW_5+(b_LW_5*DOY)+(c_LW_5*Woods_Storage)+(d_LW_5*(Woods_Inflow/86.4))+(e_LW_5*Woods_Outflow_PrvStep)
    elseif CurrMonth=6 Then
    Woods_Outflow_1=a_LW_6+(b_LW_6*DOY)+(c_LW_6*Woods_Storage)+(d_LW_6*(Woods_Inflow/86.4))+(e_LW_6*Woods_Outflow_PrvStep)
    elseif CurrMonth=7 Then
    Woods_Outflow_1=a_LW_7+(b_LW_7*DOY)+(c_LW_7*Woods_Storage)+(d_LW_7*(Woods_Inflow/86.4))+(e_LW_7*Woods_Outflow_PrvStep)
    elseif CurrMonth=8 Then
    Woods_Outflow_1=a_LW_8+(b_LW_8*DOY)+(c_LW_8*Woods_Storage)+(d_LW_8*(Woods_Inflow/86.4))+(e_LW_8*Woods_Outflow_PrvStep)
    elseif CurrMonth=9 Then
    Woods_Outflow_1=a_LW_9+(b_LW_9*DOY)+(c_LW_9*Woods_Storage)+(d_LW_9*(Woods_Inflow/86.4))+(e_LW_9*Woods_Outflow_PrvStep)
    elseif CurrMonth=10 Then
    Woods_Outflow_1=a_LW_10+(b_LW_10*DOY)+(c_LW_10*Woods_Storage)+(d_LW_10*(Woods_Inflow/86.4))+(e_LW_10*Woods_Outflow_PrvStep)
    elseif CurrMonth=11 Then
    Woods_Outflow_1=a_LW_11+(b_LW_11*DOY)+(c_LW_11*Woods_Storage)+(d_LW_11*(Woods_Inflow/86.4))+(e_LW_11*Woods_Outflow_PrvStep)
    elseif CurrMonth=12 Then
    Woods_Outflow_1=a_LW_12+(b_LW_12*DOY)+(c_LW_12*Woods_Storage)+(d_LW_12*(Woods_Inflow/86.4))+(e_LW_12*Woods_Outflow_PrvStep)
    End if
    'Step 2: Checking Outflow(t) to be between Min and Max
    Dim Woods_Outflow_2 As Double
    if Woods_Outflow_1<=Num and Woods_Outflow_1>=Num Then
    Woods_Outflow_2=Woods_Outflow_1
    elseif Woods_Outflow_1>Num Then
    Woods_Outflow_2=Num
    else
    Woods_Outflow_2=Num
    End if
    'Step 3: Finding & Allocating The Target Storage=S(t+1)=S(t)+Inflow(t)-Outflow(t)-LocalDemand(t)
    Dim Woods_Target_Storage As Double=Woods_Storage+Woods_Inflow-(Woods_Outflow_2*86.4)-(Woods_LD.mnInfo.nodedemand(currentIndex,0)/100)
    'Step 4: Checking Target Storage to be between Min and Max
    if Woods_Target_Storage<=Num and Woods_Target_Storage>=Num Then
    Woods.mnInfo.targetcontent(currentIndex,0)=Woods_Target_Storage*100
    elseif Woods_Target_Storage>Num Then
    Woods.mnInfo.targetcontent(currentIndex,0)=Num*100
    else
    Woods.mnInfo.targetcontent(currentIndex,0)=Num*100
    End if

    'Woods Outflow
    Dim Woods_Outflow3 As Double=Woods_Storage+Woods_Inflow-(Woods_LD.mnInfo.nodedemand(currentIndex,0)/100)-(Woods.mnInfo.targetcontent(currentIndex,0)/100)

    'Minaki Target Storage 
    'Step1:Finding & Allocating The Target Storage=S(t+1)=a*Woods_Outflow_PrvStep+b
    Minaki_Storage=Minaki.mnInfo.start/100
    Dim Minaki_Target_Storage As Double
    ''Dim Minaki_Outflow3 As Double
    if Woods_Outflow3>=Num*86.4 Then
    Minaki_Target_Storage=(a_MN*(Woods_Outflow3/86.4))+b_MN
    'Step 2: Checking Target Storage to be between Min and Max
    if Minaki_Target_Storage<=Num and Minaki_Target_Storage>=Num Then
    Minaki.mnInfo.targetcontent(currentIndex,0)=Minaki_Target_Storage*100
    elseif Minaki_Target_Storage>Num Then
    Minaki.mnInfo.targetcontent(currentIndex,0)=Num*100
    else
    Minaki.mnInfo.targetcontent(currentIndex,0)=Num*100
    End if
    End if

    End if 'For iteration zero'

 '------------------------------Nelson and Churchill River Basin------------------------------
    'Initial Values
    LakeWinnipeg_Storage = LakeWinnipeg.mnInfo.start/100
    GrandRapids_Storage = GrandRapids.mnInfo.start/100
    Footprint_Storage = Footprint.mnInfo.start/100
    Splitlake_Storage = Splitlake.mnInfo.start/100
    Crosslake_Storage = Crosslake.mnInfo.start/100
    Sipiwesk_Storage = Sipiwesk.mnInfo.start/100
    Southern_Indian_Lake_Storage = Southern_Indian_Lake.mnInfo.start/100
    Notigi_Storage = Notigi.mnInfo.start/100
    Jenpeg_Storage_PrvStep = Jenpeg.mnInfo.start/100
    
    'Grand Rapids
    Dim GR_Hydro as double = GR_HydroTarget.mnInfo.inflow(currentIndex,0)/100
    Dim GR_Hydro_Outflow_1 as double
    Dim GR_Spill_Outflow_1 as double
    
    'calculate powerhouse outflow according to regressions
    GR_Hydro_Outflow_1 = GR_int_hydro + GR_a_hydro*GR_Hydro
    
    'calculate spillway outflow according to regressions
    GR_Spill_Outflow_1 = GR_int_spill + GR_a_spill*GrandRapids_Storage
    
    'Check if hydro outflow(t) is between the minimum and maximum value
    Dim GR_Hydro_Outflow_2 as double
    if GR_Hydro_Outflow_1 <= 0 then
    GR_Hydro_Outflow_2 = 0
    elseif GR_Hydro_Outflow_1 > 0 and GR_Hydro_Outflow_1 < Num then
    GR_Hydro_Outflow_2 = GR_Hydro_Outflow_1
    elseif GR_Hydro_Outflow_1 >= Num then 
    GR_Hydro_Outflow_2 = Num
    end if
    
    'Check if spill outflow(t) is between the minimum and maximum value
    Dim GR_Spill_Outflow_2 as double
    if GR_Spill_Outflow_1 <= 0 then
    GR_Spill_Outflow_2 = 0
    elseif GR_Spill_Outflow_1 > 0 and GR_Spill_Outflow_1 < Num then
    GR_Spill_Outflow_2 = GR_Spill_Outflow_1
    elseif GR_Spill_Outflow_1 >= Num then 
    GR_Spill_Outflow_2 = Num
    end if
    
    'Finding and allocating the target storages
    Dim GR_Inflow as double 
    GR_Inflow = GrandRapids_Inflow.mlInfo.flow/100
    Dim GR_TargetStorage as double 
    GR_TargetStorage = GrandRapids_Storage + GR_Inflow - ((GR_Hydro_Outflow_2+GR_Spill_Outflow_2)*86.4)

    'Checking target storage to be between min and max
    if GR_TargetStorage <= Num then
    GrandRapids.mnInfo.targetcontent(currentIndex,0) = Num*100
    elseif GR_TargetStorage >= Num then 
    GrandRapids.mnInfo.targetcontent(currentIndex,0) = Num*100
    elseif GR_TargetStorage < Num and GR_TargetStorage > Num then 
    GrandRapids.mnInfo.targetcontent(currentIndex,0) = GR_TargetStorage*100
    end if

   'Nelson Channels customization
   'East Channel customization
    Dim Nelson_East_Outflow as double
    if Jenpeg_Forebay <= (Num*100)
        'Y = a*(X-c)^b
        Nelson_East_Outflow = (Num*((((LakeWinnipeg_Forebay - (Num*100))/100)^1.5)*100))*(86400/1000)*(Nelson_East_Coef.mnInfo.inflow(currentIndex,0)/100)
        Nelson_East_Discharge.mlInfo.lo = Nelson_East_Outflow
         Nelson_East_Discharge.mlInfo.hi = Nelson_East_Outflow+500
     
    elseif Jenpeg_Forebay <= (Num*100) and Jenpeg_Forebay > (Num*100) then
        Nelson_East_Outflow = Num*((((LakeWinnipeg_Forebay - (Num*100))/100)^Num)*100)*(86400/1000)*(Nelson_East_Coef.mnInfo.inflow(currentIndex,0)/100)
         Nelson_East_Discharge.mlInfo.lo =  Nelson_East_Outflow 
         Nelson_East_Discharge.mlInfo.hi = Nelson_East_Outflow+500
    
    elseif  Jenpeg_Forebay <= (Num*100) and Jenpeg_Forebay > (Num*100) then
        Nelson_East_Outflow = Num*((((LakeWinnipeg_Forebay - (Num*100))/100)^Num)*100)*(86400/1000)*(Nelson_East_Coef.mnInfo.inflow(currentIndex,0)/100)
         Nelson_East_Discharge.mlInfo.lo = Nelson_East_Outflow 
         Nelson_East_Discharge.mlInfo.hi =  Nelson_East_Outflow+500
         
    elseif Jenpeg_Forebay <= (Num*100) and Jenpeg_Forebay > (Num*100) then
        Nelson_East_Outflow = Num*((((LakeWinnipeg_Forebay - (Num*100))/100)^Num)*100)*(86400/1000)*(Nelson_East_Coef.mnInfo.inflow(currentIndex,0)/100)
        Nelson_East_Discharge.mlInfo.lo = Nelson_East_Outflow
        Nelson_East_Discharge.mlInfo.hi = Nelson_East_Outflow+500
        
    elseif Jenpeg_Forebay <= (Num*100) and Jenpeg_Forebay > (Num*100) then
        Nelson_East_Outflow = Num*((((LakeWinnipeg_Forebay - (Num*100))/100)^Num)*100)*(86400/1000)*(Nelson_East_Coef.mnInfo.inflow(currentIndex,0)/100)
         Nelson_East_Discharge.mlInfo.lo = Nelson_East_Outflow 
         Nelson_East_Discharge.mlInfo.hi = Nelson_East_Outflow+500
         
    elseif Jenpeg_Forebay <= (Num*100) and Jenpeg_Forebay > (Num*100) then
        Nelson_East_Outflow = Num*((((LakeWinnipeg_Forebay - (Num*100))/100)^Num)*100)*(86400/1000)*(Nelson_East_Coef.mnInfo.inflow(currentIndex,0)/100)
         Nelson_East_Discharge.mlInfo.lo = Nelson_East_Outflow
         Nelson_East_Discharge.mlInfo.hi = Nelson_East_Outflow+500
        
    elseif Jenpeg_Forebay <= (Num*100) and Jenpeg_Forebay > (Num*100) then
        Nelson_East_Outflow = Num*((((LakeWinnipeg_Forebay - (Num*100))/100)^Num)*100)*(86400/1000)*(Nelson_East_Coef.mnInfo.inflow(currentIndex,0)/100)
         Nelson_East_Discharge.mlInfo.lo = Nelson_East_Outflow 
         Nelson_East_Discharge.mlInfo.hi = Nelson_East_Outflow+500
        
    elseif Jenpeg_Forebay <= (Num*100) and Jenpeg_Forebay > (Num*100) then
        Nelson_East_Outflow = Num*((((LakeWinnipeg_Forebay - (Num*100))/100)^Num)*100)*(86400/1000)*(Nelson_East_Coef.mnInfo.inflow(currentIndex,0)/100)
         Nelson_East_Discharge.mlInfo.lo = Nelson_East_Outflow
         Nelson_East_Discharge.mlInfo.hi = Nelson_East_Outflow+500
         
    elseif Jenpeg_Forebay <= (Num*100) and Jenpeg_Forebay > (Num*100) then
        Nelson_East_Outflow = Num*((((LakeWinnipeg_Forebay - (Num*100))/100)^Num)*100)*(86400/1000)*(Nelson_East_Coef.mnInfo.inflow(currentIndex,0)/100) 
         Nelson_East_Discharge.mlInfo.lo = Nelson_East_Outflow 
         Nelson_East_Discharge.mlInfo.hi = Nelson_East_Outflow+500
         
    elseif Jenpeg_Forebay > (Num*100) then
        Nelson_East_Outflow = Num*((((LakeWinnipeg_Forebay - (Num*100))/100)^Num)*100)*(86400/1000)*(Nelson_East_Coef.mnInfo.inflow(currentIndex,0)/100)
         Nelson_East_Discharge.mlInfo.lo = Nelson_East_Outflow
         Nelson_East_Discharge.mlInfo.hi = Nelson_East_Outflow+500
    End if
    
    'West Channel customization
    Dim Nelson_West_Outflow as double
    if Jenpeg_Forebay <= (Num*100)
        'Y = a*(X-c)^b
        if LakeWinnipeg_Forebay - (Num*100) <= (Num*100) then
        Nelson_West_Outflow = (Num*(86.4*100))
        Nelson_West_Discharge.mlInfo.lo = (Num*(86.4*100))
        Nelson_West_Discharge.mlInfo.hi = ((Num*86.4*100))+500
        elseif LakeWinnipeg_Forebay - (Num*100) > (Num*100) then
        Dim NW_Flow as double = (Num*((((LakeWinnipeg_Forebay - (Num*100))/100)^Num)*100))*(86400/1000)*(Nelson_West_Coef.mnInfo.inflow(currentIndex,0)/100)
        if NW_Flow > Num*86.4*100 then
        Nelson_West_Discharge.mlInfo.lo = Num*86.4*100
        Nelson_West_Discharge.mlInfo.hi = (Num*86.4*100)+500
        Nelson_West_Outflow = Num*86.4*100
        elseif NW_Flow <= Num*86.4*100
        Nelson_West_Discharge.mlInfo.lo = NW_Flow
        Nelson_West_Discharge.mlInfo.hi = NW_Flow + 500
        Nelson_West_Outflow = NW_Flow
        end if
        end if
    
    elseif Jenpeg_Forebay <= (Num*100) and Jenpeg_Forebay > (Num*100)
        'Y = a*(X-c)^b
        if LakeWinnipeg_Forebay-(Num*100) <= (Num*100) then
        Nelson_West_Outflow = Num*(86.4*100)
        Nelson_West_Discharge.mlInfo.lo = Num*(86.4*100)
        Nelson_West_Discharge.mlInfo.hi = ((Num*86.4*100))+500
        elseif LakeWinnipeg_Forebay-(Num*100) > (Num*100) then
        Dim NW_Flow as double = (Num*((((LakeWinnipeg_Forebay - (Num*100))/100)^Num)*100))*(86400/1000)*(Nelson_West_Coef.mnInfo.inflow(currentIndex,0)/100)
        if NW_Flow > Num*86.4*100 then
        Nelson_West_Outflow = Num*86.4*100
        Nelson_West_Discharge.mlInfo.lo = Num*86.4*100
        Nelson_West_Discharge.mlInfo.hi = (Num*86.4*100)+500
        elseif NW_Flow <= Num*86.4*100
        Nelson_West_Discharge.mlInfo.lo = NW_Flow
        Nelson_West_Discharge.mlInfo.hi = NW_Flow + 500
        Nelson_West_Outflow = NW_Flow
        end if
        end if
    
    elseif Jenpeg_Forebay <= (Num*100) and Jenpeg_Forebay > (Num*100)
        'Y = a*(X-c)^b
        if LakeWinnipeg_Forebay-(Num*100) <= (Num*100) then
        Nelson_West_Outflow = Num*(86.4)*100
        Nelson_West_Discharge.mlInfo.lo = Num*(86.4)*100
        Nelson_West_Discharge.mlInfo.hi = ((Num*86.4*100))+500
        elseif LakeWinnipeg_Forebay-(Num*100) > (Num*100) then
        Dim NW_Flow as double = (Num*((((LakeWinnipeg_Forebay - (Num*100))/100)^Num)*100))*(86400/1000)*(Nelson_West_Coef.mnInfo.inflow(currentIndex,0)/100)
        if NW_Flow > Num*86.4*100 then
        Nelson_West_Discharge.mlInfo.lo = Num*86.4*100
        Nelson_West_Discharge.mlInfo.hi = (Num*86.4*100)+500
        Nelson_West_Outflow = Num*86.4*100
        elseif NW_Flow <= Num*86.4*100
        Nelson_West_Discharge.mlInfo.lo = NW_Flow
        Nelson_West_Discharge.mlInfo.hi = NW_Flow + 500
        Nelson_West_Outflow = NW_Flow
        end if
        end if
    
    elseif Jenpeg_Forebay <= (Num*100) and Jenpeg_Forebay > (Num*100)
        'Y = a*(X-c)^b
        if LakeWinnipeg_Forebay-(Num*100) <= (Num*100) then
        Nelson_West_Outflow = Num*(86.4*100)
        Nelson_West_Discharge.mlInfo.lo = Num*(86.4*100)
        Nelson_West_Discharge.mlInfo.hi = ((Num*86.4*100))+500
        elseif LakeWinnipeg_Forebay-(Num*100) > (Num*100) then
        Dim NW_Flow as double = (Num*((((LakeWinnipeg_Forebay - (Num*100))/100)^Num)*100))*(86400/1000)*(Nelson_West_Coef.mnInfo.inflow(currentIndex,0)/100)
        if NW_Flow > Num*86.4*100 then
        Nelson_West_Discharge.mlInfo.lo = Num*86.4*100
        Nelson_West_Discharge.mlInfo.hi = (Num*86.4*100)+500
        Nelson_West_Outflow = Num*86.4*100
        elseif NW_Flow <= Num*86.4*100
        Nelson_West_Discharge.mlInfo.lo = NW_Flow
        Nelson_West_Discharge.mlInfo.hi = NW_Flow + 500
        Nelson_West_Outflow = NW_Flow
        end if
        end if
    
    elseif Jenpeg_Forebay <= (Num*100) and Jenpeg_Forebay > (Num*100)
        'Y = a*(X-c)^b
        if LakeWinnipeg_Forebay-(Num*100) <= (Num*100) then
        Nelson_West_Outflow = Num*(86.4*100)
        Nelson_West_Discharge.mlInfo.lo = Num*(86.4*100)
        Nelson_West_Discharge.mlInfo.hi = ((Num*86.4*100))+500
        elseif LakeWinnipeg_Forebay-(Num*100) > (Num*100) then
        Dim NW_Flow as double = (Num*((((LakeWinnipeg_Forebay - (Num*100))/100)^Num)*100))*(86400/1000)*(Nelson_West_Coef.mnInfo.inflow(currentIndex,0)/100)
        if NW_Flow > Num*86.4*100 then
        Nelson_West_Discharge.mlInfo.lo = Num*86.4*100
        Nelson_West_Discharge.mlInfo.hi = (Num*86.4*100)+500
        Nelson_West_Outflow = Num*86.4*100
        elseif NW_Flow <= Num*86.4*100
        Nelson_West_Discharge.mlInfo.lo = NW_Flow
        Nelson_West_Discharge.mlInfo.hi = NW_Flow + 500
        Nelson_West_Outflow = NW_Flow
        end if
        end if
    
    elseif Jenpeg_Forebay <= (Num*100) and Jenpeg_Forebay > (Num*100)
        'Y = a*(X-c)^b
        if LakeWinnipeg_Forebay-(Num*100) <= (Num*100) then
        Nelson_West_Outflow = Num(86.4*100)
        Nelson_West_Discharge.mlInfo.lo = Num*(86.4*100)
        Nelson_West_Discharge.mlInfo.hi = ((Num*86.4*100))+500
        elseif LakeWinnipeg_Forebay-(Num*100) > (Num*100) then
        Dim NW_Flow as double = (Num*((((LakeWinnipeg_Forebay - (Num*100))/100)^Num)*100))*(86400/1000)*(Nelson_West_Coef.mnInfo.inflow(currentIndex,0)/100)
        if NW_Flow > Num*86.4*100 then
        Nelson_West_Discharge.mlInfo.lo = Num*86.4*100
        Nelson_West_Discharge.mlInfo.hi = (Num*86.4*100)+500
        Nelson_West_Outflow = Num*86.4*100
        elseif NW_Flow <= Num*86.4*100
        Nelson_West_Discharge.mlInfo.lo = NW_Flow
        Nelson_West_Discharge.mlInfo.hi = NW_Flow + 500
        Nelson_West_Outflow = NW_Flow
        end if
        end if
    
    elseif Jenpeg_Forebay <= (Num*100) and Jenpeg_Forebay > (Num*100)
      '  'Y = a*(X-c)^b
        if LakeWinnipeg_Forebay-(Num*100) <= (Num*100) then
        Nelson_West_Outflow = Num*(86.4*100)
        Nelson_West_Discharge.mlInfo.lo = Num*(86.4*100)
        Nelson_West_Discharge.mlInfo.hi = ((Num*86.4*100))+500
        elseif LakeWinnipeg_Forebay-(Num*100) > (Num*100) then
        Dim NW_Flow as double = (Num*((((LakeWinnipeg_Forebay - (Num*100))/100)^Num)*100))*(86400/1000)*(Nelson_West_Coef.mnInfo.inflow(currentIndex,0)/100)
        if NW_Flow > Num*86.4*100 then
        Nelson_West_Discharge.mlInfo.lo = Num*86.4*100
        Nelson_West_Discharge.mlInfo.hi = (Num*86.4*100)+500
        Nelson_West_Outflow = Num*86.4*100
        elseif NW_Flow <= Num*86.4*100
        Nelson_West_Discharge.mlInfo.lo = NW_Flow
        Nelson_West_Discharge.mlInfo.hi = NW_Flow + 500
        Nelson_West_Outflow = NW_Flow
        end if 
        end if
    
    elseif Jenpeg_Forebay <= (Num*100) and Jenpeg_Forebay > (Num*100)
      '  'Y = a*(X-c)^b
        if LakeWinnipeg_Forebay-(Num*100) <= (Num*100) then
        Nelson_West_Outflow = Num*(86.4*100)
        Nelson_West_Discharge.mlInfo.lo = Num*(86.4*100)
        Nelson_West_Discharge.mlInfo.hi = ((Num*86.4*100))+500
        elseif LakeWinnipeg_Forebay-(Num*100) > (Num*100) then
        Dim NW_Flow as double = (Num*((((LakeWinnipeg_Forebay - (Num*100))/100)^Num)*100))*(86400/1000)*(Nelson_West_Coef.mnInfo.inflow(currentIndex,0)/100)
        if NW_Flow > Num*86.4*100 then
        Nelson_West_Discharge.mlInfo.lo = Num*86.4*100
        Nelson_West_Discharge.mlInfo.hi = (Num*86.4*100)+500
        Nelson_West_Outflow = Num*86.4*100
        elseif NW_Flow <= Num*86.4*100
        Nelson_West_Discharge.mlInfo.lo = NW_Flow
        Nelson_West_Discharge.mlInfo.hi = NW_Flow + 500
        Nelson_West_Outflow = NW_Flow
        end if
        end if
    
    elseif Jenpeg_Forebay <= (Num*100) and Jenpeg_Forebay > (Num*100)
        'Y = a*(X-c)^b
        if LakeWinnipeg_Forebay-(Num*100) <= (Num*100) then
        Nelson_West_Outflow = Num*(86.4*100)
        Nelson_West_Discharge.mlInfo.lo = Num*(86.4*100)
        Nelson_West_Discharge.mlInfo.hi = ((Num*86.4*100))+500
        elseif LakeWinnipeg_Forebay-(Num*100) > (Num*100) then
        Dim NW_Flow as double = (Num*((((LakeWinnipeg_Forebay - (Num*100))/100)^Num)*100))*(86400/1000)*(Nelson_West_Coef.mnInfo.inflow(currentIndex,0)/100)
        if NW_Flow > Num*86.4*100 then
        Nelson_West_Discharge.mlInfo.lo = Num*86.4*100
        Nelson_West_Discharge.mlInfo.hi = (Num*86.4*100)+500
        Nelson_West_Outflow = Num*86.4*100
        elseif NW_Flow <= Num*86.4*100
        Nelson_West_Discharge.mlInfo.lo = NW_Flow
        Nelson_West_Discharge.mlInfo.hi = NW_Flow + 500
        Nelson_West_Outflow = NW_Flow
        end if
        end if
    
    elseif Jenpeg_Forebay <= (Num*100) and Jenpeg_Forebay > (Num*100)
        'Y = a*(X-c)^b
        if LakeWinnipeg_Forebay-(Num*100) <= (Num*100) then
        Nelson_West_Outflow = Num*(86.4*100)
        Nelson_West_Discharge.mlInfo.lo = Num*(86.4*100)
        Nelson_West_Discharge.mlInfo.hi = ((Num*86.4*100))+500
        elseif LakeWinnipeg_Forebay-(Num*100) > (Num*100) then
        Dim NW_Flow as double = (Num*((((LakeWinnipeg_Forebay - (Num*100))/100)^Num)*100))*(86400/1000)*(Nelson_West_Coef.mnInfo.inflow(currentIndex,0)/100)
        if NW_Flow > Num*86.4*100 then
        Nelson_West_Discharge.mlInfo.lo = Num*86.4*100
        Nelson_West_Discharge.mlInfo.hi = (Num*86.4*100)+500
        Nelson_West_Outflow = Num*86.4*100
        elseif NW_Flow <= Num*86.4*100
        Nelson_West_Discharge.mlInfo.lo = NW_Flow
        Nelson_West_Discharge.mlInfo.hi = NW_Flow + 500
        Nelson_West_Outflow = NW_Flow
        end if
        end if 
    
    elseif Jenpeg_Forebay <= (216.7128*100) and Jenpeg_Forebay > (216.408*100)
        'Y = a*(X-c)^b
        if LakeWinnipeg_Forebay-(Num*100) <= (Num*100) then
        Nelson_West_Outflow = Num*(86.4*100)
        Nelson_West_Discharge.mlInfo.lo = Num*(86.4*100)
        Nelson_West_Discharge.mlInfo.hi = ((Num*86.4*100))+500
        elseif LakeWinnipeg_Forebay-(Num*100) > (Num*100) then
        Dim NW_Flow as double = (Num*((((LakeWinnipeg_Forebay - (Num*100))/100)^Num)*100))*(86400/1000)*(Nelson_West_Coef.mnInfo.inflow(currentIndex,0)/100)
        if NW_Flow >Num*86.4*100 then
        Nelson_West_Discharge.mlInfo.lo = Num*86.4*100
        Nelson_West_Discharge.mlInfo.hi = (Num*86.4*100)+500
        Nelson_West_Outflow = Num*86.4*100
        elseif NW_Flow <= Num*86.4*100
        Nelson_West_Discharge.mlInfo.lo = NW_Flow
        Nelson_West_Discharge.mlInfo.hi = NW_Flow + 500
        Nelson_West_Outflow = NW_Flow
        end if
        end if
    
    elseif Jenpeg_Forebay <= (Num*100) and Jenpeg_Forebay > (Num*100)
        'Y = a*(X-c)^b
        if LakeWinnipeg_Forebay-(Num*100) <= (Num*100) then
        Nelson_West_Outflow = Num*(86.4*100)
        Nelson_West_Discharge.mlInfo.lo = Num*(86.4*100)
        Nelson_West_Discharge.mlInfo.hi = ((Num*86.4*100))+500
        elseif LakeWinnipeg_Forebay-(Num*100) > (Num*100) then
        Dim NW_Flow as double = (Num*((((LakeWinnipeg_Forebay - (Num*100))/100)^Num)*100))*(86400/1000)*(Nelson_West_Coef.mnInfo.inflow(currentIndex,0)/100)
        if NW_Flow > Num*86.4*100 then
        Nelson_West_Discharge.mlInfo.lo = Num*86.4*100
        Nelson_West_Discharge.mlInfo.hi = (Num*86.4*100)+500
        Nelson_West_Outflow = Num*86.4*100
        elseif NW_Flow <= Num*86.4*100
        Nelson_West_Discharge.mlInfo.lo = NW_Flow
        Nelson_West_Discharge.mlInfo.hi = NW_Flow + 500
        Nelson_West_Outflow = NW_Flow
        end if
        end if
    
    elseif Jenpeg_Forebay <= (Num*100) and Jenpeg_Forebay > (Num*100)
       'Y = a*(X-c)^b
        if LakeWinnipeg_Forebay-(Num*100) <= (Num*100) then
        Nelson_West_Outflow = Num*(86.4*100)
        Nelson_West_Discharge.mlInfo.lo = Num*(86.4*100)
        Nelson_West_Discharge.mlInfo.hi = ((Num*86.4*100))+500
        elseif LakeWinnipeg_Forebay-(Num*100) > (Num*100) then
        Dim NW_Flow as double = (Num*((((LakeWinnipeg_Forebay - (Num*100))/100)^Num)*100))*(86400/1000)*(Nelson_West_Coef.mnInfo.inflow(currentIndex,0)/100)
        if NW_Flow > Num*86.4*100 then
        Nelson_West_Discharge.mlInfo.lo = Num*86.4*100
        Nelson_West_Discharge.mlInfo.hi = (Num*86.4*100)+500
        Nelson_West_Outflow = Num*86.4*100
        elseif NW_Flow <= Num*86.4*100
        Nelson_West_Discharge.mlInfo.lo = NW_Flow
        Nelson_West_Discharge.mlInfo.hi = NW_Flow + 500
        Nelson_West_Outflow = NW_Flow
        end if
        end if
    
    elseif Jenpeg_Forebay <= (Num*100) and Jenpeg_Forebay > (Num*100)
        'Y = a*(X-c)^b
        if LakeWinnipeg_Forebay-(Num*100) <= (Num*100) then
        Nelson_West_Outflow = Num*(86.4*100)
        Nelson_West_Discharge.mlInfo.lo =Num*(86.4*100)
        Nelson_West_Discharge.mlInfo.hi = ((Num*86.4*100))+500
        elseif LakeWinnipeg_Forebay-(Num*100) > (Num100) then
        Dim NW_Flow as double = (Num*((((LakeWinnipeg_Forebay - (Num*100))/100)^Num)*100))*(86400/1000)*(Nelson_West_Coef.mnInfo.inflow(currentIndex,0)/100)
        if NW_Flow > Num*86.4*100 then
        Nelson_West_Discharge.mlInfo.lo = Num*86.4*100
        Nelson_West_Discharge.mlInfo.hi = (Num*86.4*100)+500
        Nelson_West_Outflow = Num*86.4*100
        elseif NW_Flow <= Num*86.4*100
        Nelson_West_Discharge.mlInfo.lo = NW_Flow
        Nelson_West_Discharge.mlInfo.hi = NW_Flow + 500
        Nelson_West_Outflow = NW_Flow
        end if
        end if
    
    elseif Jenpeg_Forebay <= (Num*100) and Jenpeg_Forebay > (Num*100)
        'Y = a*(X-c)^b
        if LakeWinnipeg_Forebay-(Num*100) <= (Num*100) then
        Nelson_West_Outflow = Num*(86.4*100)
        Nelson_West_Discharge.mlInfo.lo = Num*(86.4*100)
        Nelson_West_Discharge.mlInfo.hi = ((Num*86.4*100))+500
        elseif LakeWinnipeg_Forebay-(Num*100) > (Num*100) then
        Dim NW_Flow as double = (Num*((((LakeWinnipeg_Forebay - (Num*100))/100)^Num)*100))*(86400/1000)*(Nelson_West_Coef.mnInfo.inflow(currentIndex,0)/100)
        if NW_Flow > Num*86.4*100 then
        Nelson_West_Discharge.mlInfo.lo = Num*86.4*100
        Nelson_West_Discharge.mlInfo.hi = (Num*86.4*100)+500
        Nelson_West_Outflow = Num*86.4*100
        elseif NW_Flow <= Num*86.4*100
        Nelson_West_Discharge.mlInfo.lo = NW_Flow
        Nelson_West_Discharge.mlInfo.hi = NW_Flow + 500
        Nelson_West_Outflow = NW_Flow
        end if
        end if
    
    elseif Jenpeg_Forebay <= (Num*100) and Jenpeg_Forebay > (Num*100)
        'Y = a*(X-c)^b
        if LakeWinnipeg_Forebay-(Num*100) <= (Num*100) then
        Nelson_West_Outflow = Num*(86.4*100)
        Nelson_West_Discharge.mlInfo.lo = Num*(86.4*100)
        Nelson_West_Discharge.mlInfo.hi = ((Num*86.4*100))+500
        elseif LakeWinnipeg_Forebay-(Num) > (Num*100) then
        Dim NW_Flow as double = (Num*((((LakeWinnipeg_Forebay - (Num*100))/100)^Num)*100))*(86400/1000)*(Nelson_West_Coef.mnInfo.inflow(currentIndex,0)/100)
        if NW_Flow > Num*86.4*100 then
        Nelson_West_Discharge.mlInfo.lo = Num*86.4*100
        Nelson_West_Discharge.mlInfo.hi = (Num*86.4*100)+500
        Nelson_West_Outflow = Num*86.4*100
        elseif NW_Flow <= Num*86.4*100
        Nelson_West_Discharge.mlInfo.lo = NW_Flow
        Nelson_West_Discharge.mlInfo.hi = NW_Flow + 500
        Nelson_West_Outflow = NW_Flow
        end if
        end if
    
    elseif Jenpeg_Forebay > (Num*100)
        'Y = a*(X-c)^b
        if LakeWinnipeg_Forebay-(Num*100) <= (Num*100) then
        Nelson_West_Outflow = Num*(86.4*100)
        Nelson_West_Discharge.mlInfo.lo = Num*(86.4*100)
        Nelson_West_Discharge.mlInfo.hi = ((Num*86.4*100))+500
        elseif LakeWinnipeg_Forebay-(Num*100) > (Num*100) then
        Dim NW_Flow as double = (Num*((((LakeWinnipeg_Forebay - (Num*100))/100)^Num)*100))*(86400/1000)*(Nelson_West_Coef.mnInfo.inflow(currentIndex,0)/100)
        if NW_Flow > Num*86.4*100 then
        Nelson_West_Discharge.mlInfo.lo = Num*86.4*100
        Nelson_West_Discharge.mlInfo.hi = (Num*86.4*100)+500
        Nelson_West_Outflow = Num*86.4*100
        elseif NW_Flow <= Num*86.4*100
        Nelson_West_Discharge.mlInfo.lo = NW_Flow
        Nelson_West_Discharge.mlInfo.hi = NW_Flow + 500
        Nelson_West_Outflow = NW_Flow
        end if
        end if
    end if
     
    'Target storage customization
    'Lake Winnipeg Inflow
    RedRiver_DSInflow_Node.mnInfo.inflow(currentIndex,0) = RedRiver_Outflow_Link.mlInfo.flow 
    RedRiver_DSInflow_Link.mlInfo.lo =  RedRiver_Outflow_Link.mlInfo.flow
    RedRiver_DSInflow_Link.mlInfo.hi =  RedRiver_Outflow_Link.mlInfo.flow + 1
    
    Dim PineFalls_Outflow As Double = PineFalls_Outflow_Link1.mlInfo.flow + PineFalls_Outflow_Link2.mlInfo.flow
    PineFalls_DSInflow_Node.mnInfo.inflow(currentIndex,0) = PineFalls_Outflow
    PineFalls_DSInflow_Link.mlInfo.lo =  PineFalls_Outflow
    PineFalls_DSInflow_Link.mlInfo.hi =  PineFalls_Outflow + 1

    Dim LW_Inflow as double = LakeWinnipeg_Inflow.mlInfo.flow/100
    Dim LW_Outflow_1 as double
    'calculate outflow 
    LW_Outflow_1 = (Nelson_West_Outflow + Nelson_East_Outflow)/(86.4*100)
    
    'Check if outflow(t) is between the minimum and maximum value
    Dim LW_Outflow_2 as double
    if LW_Outflow_1 <= Num then
    LW_Outflow_2 = Num
    elseif LW_Outflow_1 > Num and LW_Outflow_1 < Num then
    LW_Outflow_2 = LW_Outflow_1
    elseif LW_Outflow_1 >= Num then 
    LW_Outflow_2 = Num
    end if
    
    'Finding an allocating the target storages
    Dim LW_TargetStorage as double = LakeWinnipeg_Storage + LW_Inflow - (LW_Outflow_2*86.4)
    'Checking target storage to be between min and max
    if LW_TargetStorage <= Num then
    LakeWinnipeg.mnInfo.targetcontent(currentIndex,0) = Num*100
    elseif LW_TargetStorage >= Num then 
    LakeWinnipeg.mnInfo.targetcontent(currentIndex,0) = Num*100
    elseif LW_TargetStorage < Num and LW_TargetStorage > Num then 
    LakeWinnipeg.mnInfo.targetcontent(currentIndex,0) = LW_TargetStorage*100
    end if
    
    'Jenpeg
    Dim Jenpeg_TargetStorage_1 as double
    if LakeWinnipeg_Storage > Num then
        if CurrMonth >= 11 or CurrMonth <= 5 then
            Jenpeg_TargetStorage_1 = Num
        elseif CurrMonth <11 and CurrMonth > 5 then 
            Jenpeg_TargetStorage_1 = Num
        end if
    elseif LakeWinnipeg_Storage > Num and LakeWinnipeg_Storage <= Num then
        if CurrMonth = 4 then
            Jenpeg_TargetStorage_1 = Jenpeg_int_4 + Jenpeg_a_4*LakeWinnipeg_Storage + Jenpeg_b_4*Jenpeg_Storage_PrvStep
        elseif CurrMonth = 5 then
            Jenpeg_TargetStorage_1 = Jenpeg_int_5 + Jenpeg_a_5*LakeWinnipeg_Storage + Jenpeg_b_5*Jenpeg_Storage_PrvStep
        elseif CurrMonth = 6 then
            Jenpeg_TargetStorage_1 = Jenpeg_int_6 + Jenpeg_a_6*LakeWinnipeg_Storage + Jenpeg_b_6*Jenpeg_Storage_PrvStep
        elseif CurrMonth = 7 then
            Jenpeg_TargetStorage_1 = Jenpeg_int_7 + Jenpeg_a_7*LakeWinnipeg_Storage + Jenpeg_b_7*Jenpeg_Storage_PrvStep
        elseif CurrMonth = 8 then
            Jenpeg_TargetStorage_1 = Jenpeg_int_8 + Jenpeg_a_8*LakeWinnipeg_Storage + Jenpeg_b_8*Jenpeg_Storage_PrvStep
        end if
    elseif LakeWinnipeg_Storage <= Num then
        Dim DOW as integer    
        if CurrMonth >= 11 or CurrMonth <= 5 then
            if DOY >= 305 then 
                DOW = DOY - 304
            elseif DOY <= 152 then
                DOW = DOY + 61
            end if
            Jenpeg_TargetStorage_1 = Jenpeg_int + (Jenpeg_x*DOW) + (Jenpeg_y*LakeWinnipeg_Storage) + (Jenpeg_x2*(DOW^2)) + (Jenpeg_xy*DOW*LakeWinnipeg_Storage)
        end if 
    end if
    
    'Check if rate of change in outflow is within the maximum amount
    Dim Jenpeg_TargetStorage_2 as double '= Jenpeg_TargetStorage_1
    Dim Jenpeg_Outflow_1 as double = Jenpeg_Storage_PrvStep + (Jenpeg_Inflow.mlInfo.flow/100) - Jenpeg_TargetStorage_1
    if Jenpeg_Outflow_PrvStep - (Jenpeg_Outflow_1/86.4) > Num then
        Jenpeg_TargetStorage_2 = Jenpeg_Storage_PrvStep + (Jenpeg_Inflow.mlInfo.flow/100) - (Jenpeg_Outflow_PrvStep-Num)*86.4
    else if Jenpeg_Outflow_PrvStep - (Jenpeg_Outflow_1/86.4) < -Num then 
        Jenpeg_TargetStorage_2 = Jenpeg_Storage_PrvStep + (Jenpeg_Inflow.mlInfo.flow/100) - (Jenpeg_Outflow_PrvStep+Num)*86.4
    else if Jenpeg_Outflow_PrvStep - (Jenpeg_Outflow_1/86.4) >= -Num and Jenpeg_Outflow_PrvStep - (Jenpeg_Outflow_1/86.4) <= Num then
        Jenpeg_TargetStorage_2 = Jenpeg_TargetStorage_1
    end if 
    
    'Check if target storage is between minimum and maximum for each interval
    if LakeWinnipeg_Storage > Num then
        if Jenpeg_TargetStorage_2 <= Num then
        Jenpeg.mnInfo.targetcontent(currentIndex,0) = Num*100
        elseif Jenpeg_TargetStorage_2 >= Num then 
        Jenpeg.mnInfo.targetcontent(currentIndex,0) = Num*100
        elseif Jenpeg_TargetStorage_2 < Num and Jenpeg_TargetStorage_2 > Num then 
        Jenpeg.mnInfo.targetcontent(currentIndex,0) = Jenpeg_TargetStorage_2*100
        end if        
    elseif LakeWinnipeg_Storage > Num and LakeWinnipeg_Storage <= Num and CurrMonth < 9 and CurrMonth > 3 then
        if Jenpeg_TargetStorage_2 <= Num then
        Jenpeg.mnInfo.targetcontent(currentIndex,0) = Num*100
        elseif Jenpeg_TargetStorage_2 >= Num then 
        Jenpeg.mnInfo.targetcontent(currentIndex,0) = Num*100
        elseif Jenpeg_TargetStorage_2 < Num and Jenpeg_TargetStorage_2 > Num then 
        Jenpeg.mnInfo.targetcontent(currentIndex,0) = Jenpeg_TargetStorage_2*100
        end if
    elseif LakeWinnipeg_Storage <= Num and (CurrMonth >= 11 or CurrMonth <= 5) then
        if Jenpeg_TargetStorage_2 <= Num then
        Jenpeg.mnInfo.targetcontent(currentIndex,0) = Num*100
        elseif Jenpeg_TargetStorage_2 >= Num then 
        Jenpeg.mnInfo.targetcontent(currentIndex,0) = Num*100
        elseif Jenpeg_TargetStorage_2 < Num and Jenpeg_TargetStorage_2 > Num then 
        Jenpeg.mnInfo.targetcontent(currentIndex,0) = Jenpeg_TargetStorage_2*100
        end if 
    end if
    
    'Cross Lake
    'Calculate Discharge
    Dim CL_Outflow_1 as double 
    if Crosslake_Forebay > Num*100 then 
    CL_Outflow_1 = ((CL_a_1*(Crosslake_Forebay^2)/100) + (CL_b_1*Crosslake_Forebay) + (CL_int_1*100))/(86.4*100)
    elseif Crosslake_Forebay <= Num*100 then
    CL_Outflow_1 = ((CL_a_2*Crosslake_Forebay) + (CL_int_2*100))/(86.4*100)
    end if
    
    'Check if outflow is between minimum and maximum
    Dim CL_Outflow_2 as double
    if CL_Outflow_1 <= Num then
    CL_Outflow_2 = Num
    elseif CL_Outflow_1 > Num and CL_Outflow_1 < Num then
    CL_Outflow_2 = CL_Outflow_1
    elseif CL_Outflow_1 >= Num then 
    CL_Outflow_2 = Num
    end if
    
    'Calculate next target storage
    Dim CL_Inflow as double = Crosslake_Inflow.mlInfo.flow/100
    Dim CL_TargetStorage as Double
    CL_TargetStorage = Crosslake_Storage + CL_Inflow - CL_Outflow_2*86.4
    
    'Check if target storage is between min and max
    if CL_TargetStorage <= Num then
    Crosslake.mnInfo.targetcontent(currentIndex,0) = Num*100
    elseif CL_TargetStorage >= Num then 
    Crosslake.mnInfo.targetcontent(currentIndex,0) = Num*100
    elseif CL_TargetStorage < Num and CL_TargetStorage > Num then 
    Crosslake.mnInfo.targetcontent(currentIndex,0) = CL_TargetStorage*100
    end if
    
    'Sipiwesk
    'Calculate Discharge
    Dim SW_Outflow_1 as double 
    SW_Outflow_1 = ((SW_a*Sipiwesk_Forebay) + (SW_int*100))/(100)
    
    'Check if outflow is between minimum and maximum
    Dim SW_Outflow_2 as double
    if SW_Outflow_1 <= Num then
    SW_Outflow_2 = Num
    elseif SW_Outflow_1 > Num and SW_Outflow_1 < Num then
    SW_Outflow_2 = SW_Outflow_1
    elseif SW_Outflow_1 >= Num then 
    SW_Outflow_2 = Num
    end if
    
    'Calculate next target storage
    Dim SW_Inflow as double = Sipiwesk_Inflow.mlInfo.flow/100
    Dim SW_TargetStorage as Double
    SW_TargetStorage = Sipiwesk_Storage + SW_Inflow - SW_Outflow_2*86.4
    
    'Check if target storage is between min and max
    if SW_TargetStorage <= Num then
    Sipiwesk.mnInfo.targetcontent(currentIndex,0) = Num*100
    elseif SW_TargetStorage >= Num then 
    Sipiwesk.mnInfo.targetcontent(currentIndex,0) = Num*100
    elseif SW_TargetStorage < Num and SW_TargetStorage > Num then 
    Sipiwesk.mnInfo.targetcontent(currentIndex,0) = SW_TargetStorage*100
    end if
    
   'SouthBay Channel customization
    Dim SB_Flow as double
    if Notigi_Forebay <= (Num*100)
        'Y = a*(X-c)^b
        if Southern_Indian_Lake_Forebay <= Num*100 then
        Southern_Indian_Lake_Forebay = (Num*100)+ (Num*100)
        end if
        SB_Flow = (Num*((((Southern_Indian_Lake_Forebay - (Num*100))/100)^Num)*100))*(SouthBay_Coef.mnInfo.inflow(currentIndex,0)/100) 
        SouthBay_Discharge.mlInfo.lo = (Num*((((Southern_Indian_Lake_Forebay - (Num*100))/100)^Num)*100))*(86400/1000)*(SouthBay_Coef.mnInfo.inflow(currentIndex,0)/100)-500
         SouthBay_Discharge.mlInfo.hi = (Num*((((Southern_Indian_Lake_Forebay - (Num*100))/100)^Num)*100))*(86400/1000)*(SouthBay_Coef.mnInfo.inflow(currentIndex,0)/100)+500
     
    elseif Notigi_Forebay <= (Num*100) and Notigi_Forebay > (Num*100) then
           if Southern_Indian_Lake_Forebay <= Num*100 then
          Southern_Indian_Lake_Forebay = (Num*100)+ (Num*100)
          end if
         SB_Flow = (Num*((((Southern_Indian_Lake_Forebay - (Num*100))/100)^Num)*100))*(SouthBay_Coef.mnInfo.inflow(currentIndex,0)/100)
         SouthBay_Discharge.mlInfo.lo = (Num*((((Southern_Indian_Lake_Forebay - (Num*100))/100)^Num)*100))*(86400/1000)*(SouthBay_Coef.mnInfo.inflow(currentIndex,0)/100)-500
         SouthBay_Discharge.mlInfo.hi = (Num*((((Southern_Indian_Lake_Forebay - (Num*100))/100)^Num)*100))*(86400/1000)*(SouthBay_Coef.mnInfo.inflow(currentIndex,0)/100)+500 
    
    elseif Notigi_Forebay <= (Num*100) and Notigi_Forebay > (Num*100) then
          if Southern_Indian_Lake_Forebay <= Num*100 then
          Southern_Indian_Lake_Forebay = (Num*100)+ (Num*100)
          end if
         SB_Flow = (Num*((((Southern_Indian_Lake_Forebay - (Num*100))/100)^Num)*100))*(SouthBay_Coef.mnInfo.inflow(currentIndex,0)/100)
         SouthBay_Discharge.mlInfo.lo = (Num*((((Southern_Indian_Lake_Forebay - (Num*100))/100)^Num)*100))*(86400/1000)*(SouthBay_Coef.mnInfo.inflow(currentIndex,0)/100)-500
         SouthBay_Discharge.mlInfo.hi = (Num*((((Southern_Indian_Lake_Forebay - (Num*100))/100)^Num)*100))*(86400/1000)*(SouthBay_Coef.mnInfo.inflow(currentIndex,0)/100)+500 
         
    elseif Notigi_Forebay <= (Num*100) and Notigi_Forebay > (Num*100) then
          if Southern_Indian_Lake_Forebay <= Num*100 then
          Southern_Indian_Lake_Forebay = (Num*100)+ (Num*100)
          end if
         SB_Flow = (Num*((((Southern_Indian_Lake_Forebay - (Num*100))/100)^Num)*100))*(SouthBay_Coef.mnInfo.inflow(currentIndex,0)/100)
         SouthBay_Discharge.mlInfo.lo = (Num*((((Southern_Indian_Lake_Forebay - (Num*100))/100)^Num)*100))*(86400/1000)*(SouthBay_Coef.mnInfo.inflow(currentIndex,0)/100)-500
         SouthBay_Discharge.mlInfo.hi = (Num*((((Southern_Indian_Lake_Forebay - (Num*100))/100)^Num)*100))*(86400/1000)*(SouthBay_Coef.mnInfo.inflow(currentIndex,0)/100)+500
        
    elseif Notigi_Forebay <= (Num*100) and Notigi_Forebay > (Num*100) then
             if Southern_Indian_Lake_Forebay <= Num*100 then
          Southern_Indian_Lake_Forebay = (Num*100)+ (Num*100)
          end if
         SB_Flow = (Num*((((Southern_Indian_Lake_Forebay - (Num*100))/100)^Num)*100))*(SouthBay_Coef.mnInfo.inflow(currentIndex,0)/100)
         SouthBay_Discharge.mlInfo.lo = (Num*((((Southern_Indian_Lake_Forebay - (Num*100))/100)^Num)*100))*(86400/1000)*(SouthBay_Coef.mnInfo.inflow(currentIndex,0)/100)-500
         SouthBay_Discharge.mlInfo.hi = (Num*((((Southern_Indian_Lake_Forebay - (Num*100))/100)^Num)*100))*(86400/1000)*(SouthBay_Coef.mnInfo.inflow(currentIndex,0)/100)+500 
         
    elseif Notigi_Forebay <= (Num*100) and Notigi_Forebay > (Num*100) then
         if Southern_Indian_Lake_Forebay <= Num*100 then
          Southern_Indian_Lake_Forebay = (Num*100)+ (Num*100)
          end if
         SB_Flow = (Num*((((Southern_Indian_Lake_Forebay - (Num*100))/100)^Num)*100))*(SouthBay_Coef.mnInfo.inflow(currentIndex,0)/100)
         SouthBay_Discharge.mlInfo.lo = (Num*((((Southern_Indian_Lake_Forebay - (Num*100))/100)^Num)*100))*(86400/1000)*(SouthBay_Coef.mnInfo.inflow(currentIndex,0)/100)-500
         SouthBay_Discharge.mlInfo.hi = (Num*((((Southern_Indian_Lake_Forebay - (Num*100))/100)^Num)*100))*(86400/1000)*(SouthBay_Coef.mnInfo.inflow(currentIndex,0)/100)+500 
        
    elseif Notigi_Forebay <= (Num*100) and Notigi_Forebay > (Num*100) then
          if Southern_Indian_Lake_Forebay <= Num*100 then
          Southern_Indian_Lake_Forebay = (Num*100)+ (Num*100)
          end if
         SB_Flow = (Num*((((Southern_Indian_Lake_Forebay - (Num*100))/100)^Num)*100))*(SouthBay_Coef.mnInfo.inflow(currentIndex,0)/100)
        SouthBay_Discharge.mlInfo.lo = (Num*((((Southern_Indian_Lake_Forebay - (Num*100))/100)^Num)*100))*(86400/1000)*(SouthBay_Coef.mnInfo.inflow(currentIndex,0)/100)-500
         SouthBay_Discharge.mlInfo.hi = (Num*((((Southern_Indian_Lake_Forebay - (Num*100))/100)^Num)*100))*(86400/1000)*(SouthBay_Coef.mnInfo.inflow(currentIndex,0)/100)+500 
    
     elseif Notigi_Forebay <= (Num*100) and Notigi_Forebay > (Num*100) then
          if Southern_Indian_Lake_Forebay <= Num*100 then
          Southern_Indian_Lake_Forebay = (Num*100)+ (Num*100)
          end if
         SB_Flow = (Num*((((Southern_Indian_Lake_Forebay - (Num*100))/100)^Num)*100))*(SouthBay_Coef.mnInfo.inflow(currentIndex,0)/100)
        SouthBay_Discharge.mlInfo.lo = (Num*((((Southern_Indian_Lake_Forebay - (Num*100))/100)^Num)*100))*(86400/1000)*(SouthBay_Coef.mnInfo.inflow(currentIndex,0)/100)-500
        SouthBay_Discharge.mlInfo.hi = (Num*((((Southern_Indian_Lake_Forebay - (Num*100))/100)^Num)*100))*(86400/1000)*(SouthBay_Coef.mnInfo.inflow(currentIndex,0)/100)+500 
         
    elseif Notigi_Forebay <= (Num*100) and Notigi_Forebay > (Num*100) then
          if Southern_Indian_Lake_Forebay <= Num*100 then
          Southern_Indian_Lake_Forebay = (Num*100)+ (Num*100)
          end if
         SB_Flow = (Num*((((Southern_Indian_Lake_Forebay - (Num*100))/100)^Num)*100))*(SouthBay_Coef.mnInfo.inflow(currentIndex,0)/100)
         SouthBay_Discharge.mlInfo.lo = (Num*((((Southern_Indian_Lake_Forebay - (Num*100))/100)^Num)*100))*(86400/1000)*(SouthBay_Coef.mnInfo.inflow(currentIndex,0)/100)-500
         SouthBay_Discharge.mlInfo.hi = (Num*((((Southern_Indian_Lake_Forebay - (Num*100))/100)^Num)*100))*(86400/1000)*(SouthBay_Coef.mnInfo.inflow(currentIndex,0)/100)+500 
         
     elseif Notigi_Forebay <= (Num*100) and Notigi_Forebay > (Num*100) then
         if Southern_Indian_Lake_Forebay <= Num*100 then
          Southern_Indian_Lake_Forebay = (Num*100)+ (Num*100)
          end if
         SB_Flow = (Num*((((Southern_Indian_Lake_Forebay - (Num*100))/100)^Num)*100))*(SouthBay_Coef.mnInfo.inflow(currentIndex,0)/100)
         SouthBay_Discharge.mlInfo.lo = (Num*((((Southern_Indian_Lake_Forebay - (Num*100))/100)^Num)*100))*(86400/1000)*(SouthBay_Coef.mnInfo.inflow(currentIndex,0)/100)-500
         SouthBay_Discharge.mlInfo.hi = (Num*((((Southern_Indian_Lake_Forebay - (Num*100))/100)^Num)*100))*(86400/1000)*(SouthBay_Coef.mnInfo.inflow(currentIndex,0)/100)+500 
    
    elseif Notigi_Forebay <= (Num*100) and Notigi_Forebay > (Num*100) then
         if Southern_Indian_Lake_Forebay <= Num*100 then
          Southern_Indian_Lake_Forebay = (Num*100)+ (Num*100)
          end if
         SB_Flow = (Num*((((Southern_Indian_Lake_Forebay - (Num*100))/100)^Num)*100))*(SouthBay_Coef.mnInfo.inflow(currentIndex,0)/100)
         SouthBay_Discharge.mlInfo.lo = (Num*((((Southern_Indian_Lake_Forebay - (Num*100))/100)^Num)*100))*(86400/1000)*(SouthBay_Coef.mnInfo.inflow(currentIndex,0)/100)-500
         SouthBay_Discharge.mlInfo.hi = (Num*((((Southern_Indian_Lake_Forebay - (Num*100))/100)^Num)*100))*(86400/1000)*(SouthBay_Coef.mnInfo.inflow(currentIndex,0)/100)+500 
    
    elseif Notigi_Forebay <= (Num*100) and Notigi_Forebay > (Num*100) then
          if Southern_Indian_Lake_Forebay <= Num*100 then
          Southern_Indian_Lake_Forebay = (Num*100)+ (Num*100)
          end if
         SB_Flow = (Num*((((Southern_Indian_Lake_Forebay - (Num*100))/100)^Num)*100))*(SouthBay_Coef.mnInfo.inflow(currentIndex,0)/100)
        SouthBay_Discharge.mlInfo.lo = (Num*((((Southern_Indian_Lake_Forebay - (Num*100))/100)^Num)*100))*(86400/1000)*(SouthBay_Coef.mnInfo.inflow(currentIndex,0)/100)-500
         SouthBay_Discharge.mlInfo.hi = (Num*((((Southern_Indian_Lake_Forebay - (Num*100))/100)^Num)*100))*(86400/1000)*(SouthBay_Coef.mnInfo.inflow(currentIndex,0)/100)+500 
    
    elseif Notigi_Forebay <= (Num*100) and Notigi_Forebay > (Num*100) then
          if Southern_Indian_Lake_Forebay <= Num*100 then
          Southern_Indian_Lake_Forebay = (Num*100)+ (Num*100)
          end if
         SB_Flow = (Num*((((Southern_Indian_Lake_Forebay - (Num*100))/100)^Num)*100))*(SouthBay_Coef.mnInfo.inflow(currentIndex,0)/100) 
         SouthBay_Discharge.mlInfo.lo = (Num*((((Southern_Indian_Lake_Forebay - (Num*100))/100)^Num)*100))*(86400/1000)*(SouthBay_Coef.mnInfo.inflow(currentIndex,0)/100)-500
         SouthBay_Discharge.mlInfo.hi = (Num*((((Southern_Indian_Lake_Forebay - (Num*100))/100)^Num)*100))*(86400/1000)*(SouthBay_Coef.mnInfo.inflow(currentIndex,0)/100)+500
         
    elseif Notigi_Forebay <= (Num*100) and Notigi_Forebay > (Num*100) then
          if Southern_Indian_Lake_Forebay <= Num*100 then
          Southern_Indian_Lake_Forebay = (Num*100)+ (Num*100)
          end if
         SB_Flow = (Num*((((Southern_Indian_Lake_Forebay - (Num*100))/100)^Num)*100))*(SouthBay_Coef.mnInfo.inflow(currentIndex,0)/100)
         SouthBay_Discharge.mlInfo.lo = (Num*((((Southern_Indian_Lake_Forebay - (Num*100))/100)^Num)*100))*(86400/1000)*(SouthBay_Coef.mnInfo.inflow(currentIndex,0)/100)-500
         SouthBay_Discharge.mlInfo.hi = (Num*((((Southern_Indian_Lake_Forebay - (Num*100))/100)^Num)*100))*(86400/1000)*(SouthBay_Coef.mnInfo.inflow(currentIndex,0)/100)+500 
        
    elseif Notigi_Forebay <= (Num*100) and Notigi_Forebay > (Num*100) then
         if Southern_Indian_Lake_Forebay <= Num*100 then
          Southern_Indian_Lake_Forebay = (Num*100)+ (Num*100)
          end if
         SB_Flow = (Num*((((Southern_Indian_Lake_Forebay - (Num*100))/100)^Num)*100))*(SouthBay_Coef.mnInfo.inflow(currentIndex,0)/100)
         SouthBay_Discharge.mlInfo.lo = (Num*((((Southern_Indian_Lake_Forebay - (Num*100))/100)^Num)*100))*(86400/1000)*(SouthBay_Coef.mnInfo.inflow(currentIndex,0)/100)-500
         SouthBay_Discharge.mlInfo.hi = (Num*((((Southern_Indian_Lake_Forebay - (Num*100))/100)^Num)*100))*(86400/1000)*(SouthBay_Coef.mnInfo.inflow(currentIndex,0)/100)+500
    
    elseif Notigi_Forebay <= (Num*100) and Notigi_Forebay > (Num*100) then
         if Southern_Indian_Lake_Forebay <= Num*100 then
          Southern_Indian_Lake_Forebay = (Num*100)+ (Num*100)
          end if
         SB_Flow = (Num*((((Southern_Indian_Lake_Forebay - (Num*100))/100)^Num)*100))*(SouthBay_Coef.mnInfo.inflow(currentIndex,0)/100)
         SouthBay_Discharge.mlInfo.lo = (Num*((((Southern_Indian_Lake_Forebay - (Num*100))/100)^Num)*100))*(86400/1000)*(SouthBay_Coef.mnInfo.inflow(currentIndex,0)/100)-500
         SouthBay_Discharge.mlInfo.hi = (Num*((((Southern_Indian_Lake_Forebay - (Num*100))/100)^Num)*100))*(86400/1000)*(SouthBay_Coef.mnInfo.inflow(currentIndex,0)/100)+500 
    
    elseif Notigi_Forebay <= (Num*100) and Notigi_Forebay > (Num*100) then
          if Southern_Indian_Lake_Forebay <= Num*100 then
          Southern_Indian_Lake_Forebay = (Num*100)+ (Num*100)
          end if
         SB_Flow = (Num*((((Southern_Indian_Lake_Forebay - (Num*100))/100)^Num)*100))*(SouthBay_Coef.mnInfo.inflow(currentIndex,0)/100)
         SouthBay_Discharge.mlInfo.lo = (Num*((((Southern_Indian_Lake_Forebay - (Num*100))/100)^Num)*100))*(86400/1000)*(SouthBay_Coef.mnInfo.inflow(currentIndex,0)/100)-500
         SouthBay_Discharge.mlInfo.hi = (Num*((((Southern_Indian_Lake_Forebay - (Num*100))/100)^Num)*100))*(86400/1000)*(SouthBay_Coef.mnInfo.inflow(currentIndex,0)/100)+500 
         
    elseif Notigi_Forebay > (Num*100) then
         if Southern_Indian_Lake_Forebay <= Num*100 then
          Southern_Indian_Lake_Forebay = (Num*100)+ (Num*100)
          end if
         SB_Flow = (Num*((((Southern_Indian_Lake_Forebay - (Num*100))/100)^Num)*100))*(SouthBay_Coef.mnInfo.inflow(currentIndex,0)/100)
         SouthBay_Discharge.mlInfo.lo = (Num*((((Southern_Indian_Lake_Forebay - (Num*100))/100)^Num)*100))*(86400/1000)*(SouthBay_Coef.mnInfo.inflow(currentIndex,0)/100)-500
         SouthBay_Discharge.mlInfo.hi = (Num*((((Southern_Indian_Lake_Forebay - (Num*100))/100)^Num)*100))*(86400/1000)*(SouthBay_Coef.mnInfo.inflow(currentIndex,0)/100)+500
    End if     
 
    'Target Storage Customization
    'Missi
    'Check cutoff then calculate demand
    Dim Ch_Inflow as double = Churchill_Inflow.mlInfo.flow/(100*86.4)
    Dim Missi_Min as double = Missi_Min_Flow.mnInfo.inflow(currentIndex,0)/(100*86.4)
    Dim Missi_Flow_1 as double
    if Southern_Indian_Lake_Storage > Num and Ch_Inflow > Num then
    Missi_Flow_1 = Missi_a_1*(Ch_Inflow^2)+(Missi_b_1*Ch_Inflow)+Missi_int_1 + Missi_Min
    elseif Southern_Indian_Lake_Storage <= Num or Ch_Inflow <= Num then
    Missi_Flow_1 = Missi_a_2*Ch_Inflow + Missi_int_2 + Missi_Min
    end if 
    
    'Check change in flow is less than maximum
    Dim Missi_Flow_2 as double
    if Missi_Flow_1 - Missi_Flow_PrvStep > Num then
    Missi_Flow_2 = Missi_Flow_PrvStep + Num
    elseif Missi_Flow_1 - Missi_Flow_PrvStep < -Num then 
    Missi_Flow_2 = Missi_Flow_PrvStep - Num
    elseif Missi_Flow_1 - Missi_Flow_PrvStep <= Num and Missi_Flow_1 - Missi_Flow_PrvStep >= -Num then
    Missi_Flow_2 = Missi_Flow_1
    end if
    
    'Check if below maximum historical flow
    Dim Missi_Flow_3 as double 
    if Missi_Flow_2 >= Num then
    Missi_Flow_3 = Num
    elseif Missi_Flow_2 < Missi_Min then 
    Missi_Flow_3 = Missi_Min
    elseif Missi_Flow_2 < Num and Missi_Flow_2 > Missi_Min then
    Missi_Flow_3 = Missi_Flow_2
    end if
    
    Missi_Demand.mnInfo.nodedemand(currentIndex,0) = Missi_Flow_3*100*86.4
    
    '1. Calculate Outflow
    Dim SIL_Outflow_1 as double
    if CurrMonth = 1 then
    SIL_Outflow_1 = SIL_m_1*Ch_Inflow + SIL_b_1
    elseif CurrMonth = 2 then
    SIL_Outflow_1 = SIL_m_2*Ch_Inflow + SIL_b_2
    elseif CurrMonth = 3 then
    SIL_Outflow_1 = (SIL_a_3*(Ch_Inflow^2)) + (SIL_b_3*Ch_Inflow) + SIL_c_3
    elseif CurrMonth = 4 then
    SIL_Outflow_1 = (SIL_a_4*(Ch_Inflow^2)) + (SIL_b_4*Ch_Inflow) + SIL_c_4
    elseif CurrMonth = 5 then
    SIL_Outflow_1 = (SIL_a_5*(Ch_Inflow^2)) + (SIL_b_5*Ch_Inflow) + SIL_c_5
    elseif CurrMonth = 6 then
    SIL_Outflow_1 = (SIL_a_6*(Ch_Inflow^2)) + (SIL_b_6*Ch_Inflow) + SIL_c_6
    elseif CurrMonth = 7 then
    SIL_Outflow_1 = (SIL_a_7*(Ch_Inflow^2)) + (SIL_b_7*Ch_Inflow) + SIL_c_7
    elseif CurrMonth = 8 then
    SIL_Outflow_1 = (SIL_a_8*(Ch_Inflow^2)) + (SIL_b_8*Ch_Inflow) + SIL_c_8
    elseif CurrMonth = 9 then
    SIL_Outflow_1 = SIL_m_9*Ch_Inflow + SIL_b_9
    elseif CurrMonth = 10 then
    SIL_Outflow_1 = SIL_m_10*Ch_Inflow + SIL_b_10
    elseif CurrMonth = 11 then
    SIL_Outflow_1 = SIL_m_11*Ch_Inflow + SIL_b_11
    elseif CurrMonth = 12 then
    SIL_Outflow_1 = SIL_m_12*Ch_Inflow + SIL_b_12
    end if
    'Check if outflow is between historical maximum and minimum
    Dim SIL_Outflow_2 as double 
    if SIL_Outflow_1 >= Num then
    SIL_Outflow_2 = Num
    elseif SIL_Outflow_1 <= Num then 
    SIL_Outflow_2 = Num
    elseif SIL_Outflow_1 < Num and SIL_Outflow_1 > Num then
    SIL_Outflow_2 = SIL_Outflow_1
    end if
    
    'Calculate Target Storage
    Dim SIL_TargetStorage_1 as double
    Dim SIL_Inflow as double
    SIL_Inflow = Southern_Indian_Lake_Inflow.mlInfo.flow/(100*86.4)'cms
    SIL_TargetStorage_1 = Southern_Indian_Lake_Storage + ((SIL_Inflow - SIL_Outflow_2)*86.4)
    
    
    'determining upper and lower storage bounds
    Dim SIL_Stor_ub as double
    Dim SIL_Stor_lb as double
    Dim SIL_MedianStor as double
    SIL_MedianStor = SIL_Median_Stor.mnInfo.inflow(currentIndex,0)/(100)
    if SIL_ub_condition = 1 then 
        SIL_Stor_lb = Num
        SIL_Stor_ub = Num
    elseif SIL_ub_condition = 0 then
        SIL_Stor_ub = SIL_MedianStor + Num
        if Ch_Inflow <= 750 then
            SIL_Stor_lb = SIL_MedianStor - Num
        elseif Ch_inflow > 750 then
            SIL_Stor_lb = SIL_MedianStor - Num
        end if
    end if   
    
    'ensuring the target storage is between the upper and lower storage bounds
    Dim SIL_TargetStorage_2 as double
    if SIL_TargetStorage_1 <= SIL_Stor_lb then
    SIL_TargetStorage_2 = SIL_Stor_lb
    elseif SIL_TargetStorage_1 >= SIL_Stor_ub then 
    SIL_TargetStorage_2 = SIL_Stor_ub
    elseif SIL_TargetStorage_1 < SIL_Stor_ub and SIL_TargetStorage_1 > SIL_Stor_lb then 
    SIL_TargetStorage_2 = SIL_TargetStorage_1
    end if
    
    'Check if target storage is between historical minimum and maximum
    Dim SIL_TargetStorage_3 as double
    if SIL_TargetStorage_2 <= Num then
    SIL_TargetStorage_3 = Num
    elseif SIL_TargetStorage_2 >= Num then 
    SIL_TargetStorage_3 = Num
    elseif SIL_TargetStorage_2 < Num and SIL_TargetStorage_2 > Num then 
    SIL_TargetStorage_3 = SIL_TargetStorage_2
    end if
    
    Southern_Indian_Lake.mnInfo.targetcontent(currentIndex,0) = SIL_TargetStorage_3*100
    
    'Notigi
    '1. Defining target storage based on SIL elevation
    Dim Not_TargetStorage_1 as double
    if CurrMonth = 1 then
        Not_TargetStorage_1 = Not_m_1*SIL_TargetStorage_3 + Not_b_1'May have to change the SIL Storage Variable
    elseif CurrMonth = 2 then
        Not_TargetStorage_1 = Not_m_2*SIL_TargetStorage_3 + Not_b_2
    elseif CurrMonth = 3 then
        Not_TargetStorage_1 = Not_a_3*(SIL_TargetStorage_3^2)+ (Not_b_3*SIL_TargetStorage_3) + Not_c_3
    elseif CurrMonth = 4 then
        Not_TargetStorage_1 = Not_a_4*(SIL_TargetStorage_3^2)+ (Not_b_4*SIL_TargetStorage_3) + Not_c_4
    elseif CurrMonth = 5 then
        Not_TargetStorage_1 = Not_m_5*SIL_TargetStorage_3 + Not_b_5
    elseif CurrMonth = 6 then
        Not_TargetStorage_1 = Not_m_6*SIL_TargetStorage_3 + Not_b_6
    elseif CurrMonth = 7 then
        Not_TargetStorage_1 = Not_m_7*SIL_TargetStorage_3 + Not_b_7
    elseif CurrMonth = 8 then
        Not_TargetStorage_1 = Not_a_8*(SIL_TargetStorage_3^2)+ (Not_b_8*SIL_TargetStorage_3) + Not_c_8
    elseif CurrMonth = 9 then
        Not_TargetStorage_1 = Not_a_9*(SIL_TargetStorage_3^2)+ (Not_b_9*SIL_TargetStorage_3) + Not_c_9
    elseif CurrMonth = 10 then
        Not_TargetStorage_1 = Not_a_10*(SIL_TargetStorage_3^2)+ (Not_b_10*SIL_TargetStorage_3) + Not_c_10
	elseif CurrMonth = 11 then
        Not_TargetStorage_1 = Not_m_11*SIL_TargetStorage_3 + Not_b_11
    elseif CurrMonth = 12 then
        Not_TargetStorage_1 = Not_m_12*SIL_TargetStorage_3 + Not_b_12
    end if
    
    '2. Take 4 day average
    Dim Not_TargetStorage_2 as double
    Not_TargetStorage_2 = (Not_Stor_2+Not_Stor_3+Not_Stor_4+Not_TargetStorage_1)/4 '4 day avg
    
    '4. Calculate maximum discharge (Regression)
    Dim Notigi_max_discharge as double
    Dim Notigi_WSE as double = Notigi_Forebay/100
    if Notigi_WSE <= Num then
        Notigi_max_discharge = Not_max_m_1*Notigi_WSE + Not_max_b_1
    elseif Notigi_WSE > Num then
        Notigi_max_discharge = Not_max_m_2*Notigi_WSE + Not_max_b_2
    end if
    
    '4.5. Calculate minimum discharge (Regression)
    Dim Notigi_min_discharge as double
    Notigi_min_discharge = Not_min_a*(Notigi_WSE^3) + Not_min_b*(Notigi_WSE^2) + Not_min_c*(Notigi_WSE) + Not_min_d
    
    '5. Calculate Discharge
    Dim Not_Outflow_1 as double 
    Not_Outflow_1 = (Not_Stor_3 - Not_TargetStorage_2 + (Notigi_Inflow.mlInfo.flow/100))/86.4 'cms
    
    '5.5 ensure min not greater than max
    Dim Notigi_min_Q as double
    Dim Notigi_max_Q as double
    if Notigi_min_discharge >= Notigi_max_discharge then
        Notigi_min_Q = Num 'historical
        Notigi_max_Q = Num 'historical
    elseif Notigi_min_discharge < Notigi_max_discharge then
        Notigi_min_Q = Notigi_min_discharge
        Notigi_max_Q = Notigi_max_discharge
    end if
    
    '6. Check maximum discharge
    Dim Not_Outflow_2 as double
    if Not_Outflow_1 > Notigi_max_Q then
        Not_Outflow_2 = Notigi_max_Q
    elseif Not_Outflow_1 < Notigi_min_Q then
        Not_Outflow_2 = Notigi_min_Q
    elseif Not_Outflow_1 >= Notigi_min_Q and Not_Outflow_1 <= Notigi_max_Q then
        Not_Outflow_2 = Not_Outflow_1
    end if
    
    '7. Recalculate Notigi Target Storage
    Dim Not_TargetStorage_3 as double
    Not_TargetStorage_3 = Not_Stor_3 + (Notigi_Inflow.mlInfo.flow/100) - (Not_Outflow_2*86.4)
    
    '8. Check if Target Storage is between maximum and minimum
    if Not_TargetStorage_3 <= Num then
    Notigi.mnInfo.targetcontent(currentIndex,0) = Num*100
    elseif Not_TargetStorage_3 >= Num then 
    Notigi.mnInfo.targetcontent(currentIndex,0) = Num*100
    elseif Not_TargetStorage_3 < Num and Not_TargetStorage_3 > Num then 
    Notigi.mnInfo.targetcontent(currentIndex,0) = Not_TargetStorage_3*100
    end if
    
    'Footprint
    'Calculate Discharge
    Dim FP_Outflow_1 as double 
    FP_Outflow_1 = ((FP_a*(Footprint_Forebay^2)/100) + (FP_b*Footprint_Forebay) + (FP_int*100))/(86.4*100)
    
    'Check if outflow is between minimum and maximum
    Dim FP_Outflow_2 as double
    if FP_Outflow_1 <= Num then
    FP_Outflow_2 = Num
    elseif FP_Outflow_1 > Num and FP_Outflow_1 < Num then
    FP_Outflow_2 = FP_Outflow_1
    elseif FP_Outflow_1 >= Num then 
    FP_Outflow_2 = Num
    end if
    
    'Calculate next target storage
    Dim FP_Inflow as double = Footprint_Inflow.mlInfo.flow/100
    Dim FP_TargetStorage as Double
    FP_TargetStorage = Footprint_Storage + FP_Inflow - FP_Outflow_2*86.4
    
    'Check if target storage is between min and max
    if FP_TargetStorage <= Num then
    Footprint.mnInfo.targetcontent(currentIndex,0) = Num*100
    elseif FP_TargetStorage >= Num then 
    Footprint.mnInfo.targetcontent(currentIndex,0) = Num*100
    elseif FP_TargetStorage < Num and FP_TargetStorage > Num then 
    Footprint.mnInfo.targetcontent(currentIndex,0) = FP_TargetStorage*100
    end if
    
    'Split Lake
    'Calculate Discharge
    Dim SL_Outflow_1 as double 
    SL_Outflow_1 = ((SL_a*(Splitlake_Forebay^2)/100) + (SL_b*Splitlake_Forebay) + (SL_int*100))/(86.4*100)
    
    'Check if outflow is between minimum and maximum
    Dim SL_Outflow_2 as double
    if SL_Outflow_1 <= Num then
    SL_Outflow_2 = Num
    elseif SL_Outflow_1 > Num and SL_Outflow_1 < Num then
    SL_Outflow_2 = SL_Outflow_1
    elseif SL_Outflow_1 >= Num then 
    SL_Outflow_2 = Num
    end if
    
    'Calculate next target storage
    Dim SL_Inflow as double = Splitlake_Inflow.mlInfo.flow/100
    Dim SL_TargetStorage as Double
    SL_TargetStorage = Splitlake_Storage + SL_Inflow - SL_Outflow_2*86.4
    
    'Check if target storage is between min and max
    if SL_TargetStorage <= Num then
    Splitlake.mnInfo.targetcontent(currentIndex,0) = Num*100
    elseif SL_TargetStorage >= Num then 
    Splitlake.mnInfo.targetcontent(currentIndex,0) = Num*100
    elseif SL_TargetStorage < Num and SL_TargetStorage > Num then 
    Splitlake.mnInfo.targetcontent(currentIndex,0) = SL_TargetStorage*100
    end if

 '--------------------------------------------------------------------------------------------------
 '----------------------------------------End of  Defined Operations -------------------------------
 '--------------------------------------------------------------------------------------------------
    End Sub

	Private Sub OnIterationConverge()
    
    Dim currentIndex As Integer=myModel.mInfo.CurrentModelTimeStepIndex 'Current Time Step Index
    Dim CurrDate As Date=myModel.TimeStepManager.Index2Date(currentIndex,TypeIndexes.ModelIndex)'Finding the Current Date: Starting Date of Time Step
    Dim CurrEndDate As Date=myModel.TimeStepManager.Index2EndDate(currentIndex,TypeIndexes.ModelIndex)'Finding the Current Date: Ending Date of Time Step
    Dim DOY AS Integer=CurrDate.DayOfYear 'Finding the DOY of the Current Date
    Dim CurrYear As Integer=CurrDate.Year 'Finding the Year of the Current Date

    '-----------------------Update Initial Values and Check Results---------------------------
     
    'Shellmouth 
    if DOY=DD_StartDOY
    overtopped=0 'Updating overtoped condition for summer
    End if 

    Shellmouth_Level_Pr=Shellmouth_Level
    Shellmouth_Level=(2*(Shellmouth.mnInfo.head))-Shellmouth_Level 'updating the level for next time step
    Console.WriteLine("ConduitOpening (ft): " & ConduitOpening) 
    Shellmout_Outfolw_tm1=Shellmouth_Outflow_1 'Updating Previous Time Steps Conduit Outflow
    Brandon_Outflow=Brandon_Outflow_Link.mlInfo.flow/100 'Updating Brandon Discharge
    Holland_Outflow=Holland_Outflow_Link.mlInfo.flow/100 'Updating Holland Discharge

    'LakeWinnipegosis
    LakeWinnipegosis_Level=(2*(LakeWinnipegosis.mnInfo.head))-LakeWinnipegosis_Level 'updating the level for next time step
    'LakeManitoba
    LakeManitoba_Level=(2*(LakeManitoba.mnInfo.head))-LakeManitoba_Level 'updating the level for next time step
    if OP=1 Then 'defining operating condition for the next time step
    if LakeManitoba_Level>=Num Then
    OP=1
    elseif LakeManitoba_Level<Num and LakeManitoba_Level>=Num Then
    OP=3
    elseif LakeManitoba_Level<Num Then
    OP=4
    End if
    elseif OP=2 or OP=3 Then
    if LakeManitoba_Level>=Num Then
    OP=1
    elseif LakeManitoba_Level<Num and LakeManitoba_Level>=Num Then
    OP=2
    elseif LakeManitoba_Level<Num and LakeManitoba_Level>=Num Then
    OP=3
    elseif LakeManitoba_Level<Num Then
    OP=4
    End if
    elseif OP=4 Then
    if LakeManitoba_Level<Num Then
    OP=4
    elseif LakeManitoba_Level<Num and LakeManitoba_Level>=Num Then
    OP=2
    elseif LakeManitoba_Level>=Num Then
    OP=1
    End if
    End if
    'LakeStMartin'
    LakeStMartin_Level=(2*(LakeStMartin.mnInfo.head))-LakeStMartin_Level 'updating the level for next time step

    'Diversion and Floodway
    Southport_2Pr=Southport_Pr
    Southport_Pr=Actual_Southport
    Console.WriteLine("Poratge_Diversion (cms): " & Poratge_Diversion.mnInfo.nodedemand(currentIndex,0)/(100*86.4)) 'To check the result on output text file'
    Console.WriteLine("AssiniboineRiverDischarge (cms)= " & Actual_Southport) 'To check the result on output text file'
    Console.WriteLine("Floodway (cms): " & Floodway.mnInfo.nodedemand(currentIndex,0)/(100*86.4)) 'To check the result on output text file'
    Console.WriteLine("RedRiverDischarge (cms): " & RedRiver_Discharge) 'To check the result on output text file'
    Console.WriteLine("Delta (ft): " & Delta) 'To check the result on output text file'
    Console.WriteLine("JamesLevel (ft): " & Actual_JamesLevel) 'To check the result on output text file'
    Delta=0.4
    Actual_JamesLevel=0

    if LakeManitoba_Level<=(Num*0.3048)
    JamesTarget_Level=JamesTarget_Level1
    elseif LakeManitoba_Level>=(Num*0.3048)
    JamesTarget_Level=JamesTarget_Level2
    elseif LakeManitoba_Level>=(Num*0.3048) and PortageDiversion_MaxDischarge>Num
    JamesTarget_Level=JamesTarget_Level3
    End if

    if LakeManitoba_Level<=(Num*0.3048)
    Assiniboine_MaxDischarge=Assiniboine_MaxDischarge1
    elseif LakeManitoba_Level>=(Num*0.3048)
    Assiniboine_MaxDischarge=Assiniboine_MaxDischarge2
    elseif LakeManitoba_Level>=(Num*0.3048) and PortageDiversion_MaxDischarge>Num
    Assiniboine_MaxDischarge=Num
    End if

    'StJoseph
    StJoseph_Storage=StJoseph.mnInfo.stend/100  
    StJoseph_Outflow_PrvStep=((StJoseph_Root.mlInfo.flow)+(StJoseph_Rat.mlInfo.flow))/(86.4*100)
    'LacSeul'
    LacSeul_Storage=LacSeul.mnInfo.stend/100
    LacSeul_Storage_PrvStep=LacSeul.mnInfo.start/100
    LacSeul_Outflow_PrvStep=LacSeul_Outflow.mlInfo.flow/(86.4*100)
    'Pakwash'
    Pakwash_Storage=Pakwash.mnInfo.stend/100
    Pakwash_Outflow_PrvStep=Pakwash_Outflow.mlInfo.flow/(86.4*100)
    'Ball'
    Ball_Storage=Ball.mnInfo.stend/100
    Ball_Outflow_PrvStep=Ball_Outflow.mlInfo.flow/(86.4*100)
    'Separation'
    Separation_Storage=Separation.mnInfo.stend/100
    Separation_Outflow_PrvStep=Separation_Outflow.mlInfo.flow/(86.4*100)
    'Umfreville'
    Umfreville_Storage=Umfreville.mnInfo.stend/100
    Umfreville_Outflow_PrvStep=Umfreville_Outflow.mlInfo.flow/(86.4*100)
    'LacLaCroix'
    LacLaCroix_Storage=LacLaCroix.mnInfo.stend/100
    LacLaCroix_Outflow_PrvStep=LacLaCroix_Outflow.mlInfo.flow/(86.4*100)
    'Namakan'
    Namakan_Storage=Namakan.mnInfo.stend/100
    Namakan_Outflow_PrvStep=Namakan_Outflow.mlInfo.flow/(86.4*100)
    'Rainy'
    Rainy_Storage=Rainy.mnInfo.stend/100
    Rainy_Outflow_PrvStep=Rainy_Outflow.mlInfo.flow/(86.4*100)
    'Woods'
    Woods_Storage=Woods.mnInfo.stend/100
    Woods_Outflow_PrvStep=Woods_Outflow.mlInfo.flow/(86.4*100)
    'Minaki'
    Minaki_Storage=Minaki.mnInfo.stend/100

    'Footprint
    Footprint_Forebay = Footprint.mnInfo.head*100
    'Split Lake
    Splitlake_Forebay = Splitlake.mnInfo.head*100
    'Cross Lake
    Crosslake_Forebay = Crosslake.mnInfo.head*100
    'Sipiwesk
    Sipiwesk_Forebay = Sipiwesk.mnInfo.head*100
    'Jenpeg
    Jenpeg_Outflow_PrvStep = (Jenpeg_Discharge_Hydro.mlInfo.flow/(100*86.4)) + (Jenpeg_Discharge_Spill.mlInfo.flow/(100*86.4))'cms
    'SIL, Missi, & Notigi
    Missi_Flow_PrvStep = Missi_Discharge.mlInfo.flow/(100*86.4)
    Notigi_Flow_PrvStep = Notigi_Discharge.mlInfo.flow/(100*86.4)
    'West Channel
    LakeWinnipeg_Forebay = (LakeWinnipeg.mnInfo.head*100)
    Jenpeg_Forebay = (((((Jenpeg.mnInfo.stend/100)*1000)+Num)/Num)*100) + (Num*100) 
    Southern_Indian_Lake_Forebay = (Southern_Indian_Lake.mnInfo.head*100)
    Notigi_Forebay = (Notigi.mnInfo.head*100)
    
    Not_Stor_2 = Not_Stor_3
    Not_Stor_3 = Not_Stor_4
    Not_Stor_4 = Notigi.mnInfo.stend/100 

    if DOY = 274 then
        if Ch_Cumulative_Inflow <= Num then 
            SIL_ub_condition = 0
            Ch_Cumulative_Inflow = 0
            Ch_Cumulative_Inflow_Prev = 0
        elseif Ch_Cumulative_Inflow > Num then 
            SIL_ub_condition = 1        
            Ch_Cumulative_Inflow = 0
            Ch_Cumulative_Inflow_Prev = 0
        end if 
    elseif DOY >= 61 and DOY < 274 then 
        Ch_Cumulative_Inflow_Prev = Ch_Cumulative_Inflow
        Ch_Cumulative_Inflow = Ch_Cumulative_Inflow_Prev + (Churchill_Inflow.mlInfo.flow/(100*86.4))
    elseif DOY > 274 then
        Ch_Cumulative_Inflow = 0
        Ch_Cumulative_Inflow_Prev = 0
    elseif DOY < 61 then
        Ch_Cumulative_Inflow = 0
        Ch_Cumulative_Inflow_Prev = 0
    end if
    Console.WriteLine("--------------------------------------------------") 'Separator line on output text file    
	End Sub

	Private Sub OnFinished()

	End Sub

End Module