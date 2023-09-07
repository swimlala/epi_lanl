CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PARAM       N_PROF        N_CALIB       N_LEVELS   t   	N_HISTORY             	   title         Argo float vertical profile    institution       JAMSTEC    source        
Argo float     history       b2009-06-03T12:57:05Z creation;2009-09-01T09:17:44Z update;2015-06-11T11:28:45Z conversion to V3.1;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      
_FillValue               conventions       Argo reference table 1          6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER       	            	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME      	            	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME       	            	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS        	               	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER      	         	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION         	         	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE       	            	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DC_REFERENCE      	            	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR      	            	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE         	         	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE         	            	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO       	            	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION      	            	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE         	            	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD      	         	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
_FillValue        A.�~       standard_name         time   
resolution        >�����h�   axis      T           8`   JULD_QC       	         	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION         	         	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
_FillValue        A.�~       
resolution        >�����h�        8l   LATITUDE      	         	long_name         &Latitude of the station, best estimate     units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        standard_name         latitude   axis      Y           8t   	LONGITUDE         	         	long_name         'Longitude of the station, best estimate    units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        standard_name         	longitude      axis      X           8|   POSITION_QC       	         	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM        	            	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC       	         	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC       	         	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC       	         	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME      	            	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER         	         	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PRES      	         
   	long_name         )Sea water pressure, equals 0 at sea-level      
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   standard_name         sea_water_pressure     axis      Z        �  9�   PRES_QC       	            	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  ;p   PRES_ADJUSTED         	         	   	long_name         )Sea water pressure, equals 0 at sea-level      
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   standard_name         sea_water_pressure       �  ;�   PRES_ADJUSTED_QC      	            	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  =�   PRES_ADJUSTED_ERROR       	            	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  >(   TEMP      	         	   	long_name         $Sea temperature in-situ ITS-90 scale   
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_temperature        �  ?�   TEMP_QC       	            	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  A�   TEMP_ADJUSTED         	         	   	long_name         $Sea temperature in-situ ITS-90 scale   
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_temperature        �  B<   TEMP_ADJUSTED_QC      	            	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  D   TEMP_ADJUSTED_ERROR       	            	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  D�   PSAL      	         	   	long_name         Practical salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_salinity       �  FP   PSAL_QC       	            	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  H    PSAL_ADJUSTED         	         	   	long_name         Practical salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_salinity       �  H�   PSAL_ADJUSTED_QC      	            	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  Jd   PSAL_ADJUSTED_ERROR       	            	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  J�   	PARAMETER         	   
               	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  L�   SCIENTIFIC_CALIB_EQUATION         	   
               	long_name         'Calibration equation for this parameter    
_FillValue                 	   M8   SCIENTIFIC_CALIB_COEFFICIENT      	   
               	long_name         *Calibration coefficients for this equation     
_FillValue                 	   V8   SCIENTIFIC_CALIB_COMMENT      	   
               	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   _8   SCIENTIFIC_CALIB_DATE         	   
                	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  h8   HISTORY_INSTITUTION          	            	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    h�   HISTORY_STEP         	            	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    h�   HISTORY_SOFTWARE         	            	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    h�   HISTORY_SOFTWARE_RELEASE         	            	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    h�   HISTORY_REFERENCE            	            	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  h�   HISTORY_DATE         	             	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    i   HISTORY_ACTION           	            	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    i   HISTORY_PARAMETER            	            	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    i   HISTORY_START_PRES           	         	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         i,   HISTORY_STOP_PRES            	         	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         i0   HISTORY_PREVIOUS_VALUE           	         	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        i4   HISTORY_QCTEST           	            	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    i8Argo profile    3.1 1.2 19500101000000  5901576 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               %A   JA  20090603125705  20150614170513  A9_76264_037                    2C  D   APEX                            3512                            070407                          846 @�1��E�1   @�1���s@+H�9Xb�d� ě��1   ARGOS   A   A   A   Primary sampling: discrete [spot sampling CTD data]                                                                                                                                                                                                                @�  A��Ah  A���A�  A���B
  B��B/��BE33BY��Bn��B�33B�  B�33B�  B�ffB�  B���B�  B�33Bڙ�B䙚BB���C�CffC  CL�CL�C��CffC$33C)L�C.33C3� C8L�C=33CBL�CGffCQ33C[L�CeffCo�Cy33C��3C�� C���C��fC�� C��3C�� C��3C��3C��3C��fC�� C���C�� Cǌ�Č�CѦfC֙�Cۙ�C���C� C�3C� C���C�� D�3D�3D�fD� D��D�fD�fD$��D)�fD.� D3�fD8� D=ٚDB�3DG��DL�fDQ� DV�3D[ٚD`� De� Dj�fDo��Dt�fDy� D�)�D�` D���D��3D�)�D�p D���D��D�  D�p D�� D���D��D�ffDڰ D���D�#3D�Y�D�fD��3D�ff11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@���A  AfffA���A�33A�  B	��BffB/33BD��BY33BnffB�  B���B�  B���B�33B���B�ffB���B�  B�ffB�ffB�ffB�ffC  CL�C
�fC33C33C� CL�C$�C)33C.�C3ffC833C=�CB33CGL�CQ�C[33CeL�Co  Cy�C��fC��3C�� C���C��3C��fC�s3C��fC��fC��fC���C�s3C�� C³3Cǀ C̀ Cљ�C֌�Cی�C�� C�s3C�fC�s3C��C�s3D��D��D� DٚD�fD� D� D$�3D)� D.ٚD3� D8ٚD=�3DB��DG�3DL� DQ��DV��D[�3D`ٚDeٚDj� Do�fDt� Dy��D�&fD�\�D���D�� D�&fD�l�D���D��fD��D�l�D���D�ٚD�fD�c3Dڬ�D�ٚD�  D�VfD�3D�� D�c311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�33A���A��A���A���A�ĜA�n�A�+A��A��AҬA�&�AύPAʬA�`BAƋDAř�Aé�A��A�v�A��A�ZA��A�oA��/A��
A�JA�l�A���A��wA�&�A�oA�  A��uA�Q�A���A��#A�r�A���A�ȴAw��Ad��AX �AI�-A@��A9�A-�A)�wA#�7A��A�-A��AI�AI�A-A��AbNA/A�AO�A
-A��A%A%A%A�mA �@�@�+@��@�V@�$�@���@�t�@ܴ9@�@Ұ!@̋D@�@�G�@�M�@�S�@�M�@�t�@�I�@��m@�
=@��@�+@�V@�n�@���@�X@��@�9X@��y@�p�@�  @{t�@n��@e�@\��@U?}@Kt�@Fv�@<z�@6ff@/+@)��@&V@��@�!@/@-@{@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�33A���A��A���A���A�ĜA�n�A�+A��A��AҬA�&�AύPAʬA�`BAƋDAř�Aé�A��A�v�A��A�ZA��A�oA��/A��
A�JA�l�A���A��wA�&�A�oA�  A��uA�Q�A���A��#A�r�A���A�ȴAw��Ad��AX �AI�-A@��A9�A-�A)�wA#�7A��A�-A��AI�AI�A-A��AbNA/A�AO�A
-A��A%A%A%A�mA �@�@�+@��@�V@�$�@���@�t�@ܴ9@�@Ұ!@̋D@�@�G�@�M�@�S�@�M�@�t�@�I�@��m@�
=@��@�+@�V@�n�@���@�X@��@�9X@��y@�p�@�  @{t�@n��@e�@\��@U?}@Kt�@Fv�@<z�@6ff@/+@)��@&V@��@�!@/@-@{@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
49B
2-B
2-B
1'B
1'B
1'B
6FB
^5B
q�B
k�B
w�B
v�B
��B
ȴB��B�B)�B>wBe`B�B�uB��B��B�%Bv�Bm�BVBF�B<jB�B��B�yB�B�+B_;B&�B
��B
�}B
��B
p�B	��B	ZB	hB�sB�B��B	hB	�B	:^B	N�B	s�B	��B	�XB	�BB	��B	�5B	�B	��B
B	��B
B
B
+B
1B

=B
1B
+B
+B
%B
B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
	7B
	7B
VB
bB
bB
uB
{B
�B
�B
�B
�B
#�B
)�B
1'B
7LB
>wB
C�B
H�B
N�B
T�B
YB
]/B
aHB
ffB
iyB
k�B
p�B
t�B
y�B
|�B
� B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
49B
2-B
2-B
2-B
1'B
1'B
8RB
`BB
r�B
k�B
w�B
x�B
��B
��B��B�B,BA�BgmB�B�{B��B��B�7Bx�Bp�BYBG�B?}B�BB�B�'B�7BbNB-B
�B
��B
��B
u�B	�B	]/B	�B�B�B��B	oB	�B	;dB	O�B	t�B	��B	�XB	�HB	��B	�;B	�B	��B
B	��B
B
%B
1B
	7B

=B
1B
+B
1B
+B
B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
	7B
	7B
VB
bB
bB
uB
{B
�B
�B
�B
�B
#�B
)�B
1'B
7LB
>wB
C�B
H�B
N�B
T�B
YB
]/B
aHB
ffB
iyB
k�B
p�B
t�B
y�B
|�B
� B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ = PRES-SP(NextCycle), where SP is SURFACE PRESSURE from next cycle                                                                                                                                                                                     TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,PRES_ADJ,elapsed_time,alpha,tau),elapsed_time=P/mean_rise_rate,P=dbar since the start of the profile for each samples                                                                                                       None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            SP(NextCycle)=0.1(dbar)                                                                                                                                                                                                                                         None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE; PRES_ADJ_ERR : Manufacture sensor accuracy                                                                                                                                                                 TEMP_ADJ_ERR : SBE sensor accuracy                                                                                                                                                                                                                              Salinity Recalculation using PRES_ADJ; PSAL_ADJ_ERR : max(sum of RecalS & CTM errors, 0.01(PSS-78))                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         200906161426262009061614262620090616142626200906161426212009061614262120090616142621200908250000002009082500000020090825000000  JA  ARFMdecpA9_b                                                                20090603125705  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.5                                                                 20090603125705  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.0                                                                 20090603125706  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20090603125706  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20090603125707  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20090603125707  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8b                                                                20090603125708  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8b                                                                20090603125708  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20090603125708  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20090603130210                      G�O�G�O�G�O�                JA  ARFMdecpA9_b                                                                20090607065748  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.5                                                                 20090607065920  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.0                                                                 20090607065921  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20090607065921  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20090607065922  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20090607065922  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8b                                                                20090607065922  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8b                                                                20090607065922  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20090607065923  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20090607070332                      G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20090616142626  CV  PRES            G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20090616142626  IP  PSAL            G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20090616142621  CV  PSAL            G�O�G�O�G�O�                JM  ARSQOW  1.1 SeHyD1.0                                                        20090825000000  IP  PSAL            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20090901091722  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20090901091744                      G�O�G�O�G�O�                JM      JMFC1.0                                                                 20150611112836                      G�O�G�O�G�O�                JA  ARDU                                                                        20150614170513                      G�O�G�O�G�O�                