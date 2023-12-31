CDF   !   
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   t   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JAMSTEC    source        
Argo float     history       b2010-03-10T09:57:15Z creation;2015-03-10T06:12:31Z update;2015-06-11T11:34:08Z conversion to V3.1;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      
_FillValue               conventions       Argo reference table 1          6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
_FillValue        A.�~       standard_name         time   
resolution        >�����h�   axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
_FillValue        A.�~       
resolution        >�����h�        8l   LATITUDE               	long_name         &Latitude of the station, best estimate     units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        standard_name         latitude   axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        standard_name         	longitude      axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   standard_name         sea_water_pressure     axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  ;p   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   standard_name         sea_water_pressure       �  ;�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  =�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  >(   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_temperature        �  ?�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  A�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_temperature        �  B<   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  D   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  D�   PSAL         
      	   	long_name         Practical salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_salinity       �  FP   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  H    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_salinity       �  H�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  Jd   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  J�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  L�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   M8   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   V8   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   _8   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  h8   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    h�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    h�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    h�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    h�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  h�   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    i   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    i   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    i   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         i,   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         i0   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        i4   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    i8Argo profile    3.1 1.2 19500101000000  5901576 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               AA   JA  20100310095715  20150614170516  A9_76264_065                    2C  D   APEX                            3512                            070407                          846 @�w�ĥ[�1   @�w� 0��@+%�S����d��t�1   ARGOS   A   A   A   Primary sampling: discrete [spot sampling CTD data]                                                                                                                                                                                                                @���A  Ac33A�ffA�  A�33B��B  B/��BD  BX  Bl  B�33B���B���B�  B�ffB���B�ffBǙ�B�ffB�33B䙚B�  B���C� CffC��C� C33C  C33C$  C)ffC.��C3ffC8��C=ffCB� CG  CQ33CZ�fCe�CoffCy�C���C�� C�� C�s3C���C��3C�� C�s3C�s3C�ٚC��fC���C��fC�s3C�s3C̳3C�� Cր Cۀ C���C� C�3C� C���C�� D�fD�fD�3DٚDٚDٚDٚD$��D)�3D.ٚD3��D8�fD=�3DB��DG�3DL�3DQٚDV� D[� D`� De� Dj�fDo��Dt�3Dy�3D��D�Y�D�� D�� D�,�D�p D�� D���D�  D�p D��fD�� D�#3D�i�Dڰ D�� D�,�D�i�D��D�� D�\�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@���AffAa��A���A�33A�ffB33B��B/33BC��BW��Bk��B�  B�ffB�ffB���B�33B�ffB�33B�ffB�33B�  B�ffB���B���CffCL�C� CffC�C�fC�C#�fC)L�C.� C3L�C8� C=L�CBffCF�fCQ�CZ��Ce  CoL�Cy  C���C��3C��3C�ffC�� C��fC�s3C�ffC�ffC���C���C���C���C�ffC�ffC̦fCѳ3C�s3C�s3C�� C�s3C�fC�s3C��C��3D� D� D��D�3D�3D�3D�3D$�fD)��D.�3D3�fD8� D=��DB�fDG��DL��DQ�3DV��D[��D`ٚDeٚDj� Do�fDt��Dy��D��D�VfD���D���D�)�D�l�D���D��D��D�l�D��3D���D�  D�ffDڬ�D���D�)�D�ffD�D���D�Y�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A���A�ȴA�ȴAͼjA�x�A�`BA�=qA�33A��A�
=A�  A���A��A��A��yA��TA��;A˕�AƼjA�&�A�=qA�z�A�A�-A�^5A��A�
=A�XA�t�A���A��A��/Au��Ag��A_K�A[��AY%AR�AOC�AL�DAD�A5hsA+33A&�RA&-A!�-A5?A��A�A�AbNA
=AhsA�A�A�;A��A�!A ��@�\)@�Q�@���@��@�@��@��@���@�F@��@�X@�p�@�E�@��
@��@ʗ�@�t�@���@��`@�bN@�@��;@��9@��R@�Q�@�Q�@�G�@���@�{@�Ĝ@���@�5?@���@�
=@��@�Ĝ@�K�@w+@i��@\�D@Q�#@N5?@Fȴ@?�w@7;d@-��@&$�@!�#@l�@E�@�@�;@�@$�@
��@
M�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A���A�ȴA�ȴAͼjA�x�A�`BA�=qA�33A��A�
=A�  A���A��A��A��yA��TA��;A˕�AƼjA�&�A�=qA�z�A�A�-A�^5A��A�
=A�XA�t�A���A��A��/Au��Ag��A_K�A[��AY%AR�AOC�AL�DAD�A5hsA+33A&�RA&-A!�-A5?A��A�A�AbNA
=AhsA�A�A�;A��A�!A ��@�\)@�Q�@���@��@�@��@��@���@�F@��@�X@�p�@�E�@��
@��@ʗ�@�t�@���@��`@�bN@�@��;@��9@��R@�Q�@�Q�@�G�@���@�{@�Ĝ@���@�5?@���@�
=@��@�Ĝ@�K�@w+@i��@\�D@Q�#@N5?@Fȴ@?�w@7;d@-��@&$�@!�#@l�@E�@�@�;@�@$�@
��@
M�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
H�B
I�B
I�B
H�B
H�B
G�B
G�B
G�B
F�B
F�B
F�B
F�B
F�B
F�B
E�B
E�B
E�B
D�B
]/B
��Be`Bk�BffB?}B�=B#�B�B�XB��BYB
��B
�uB
T�B	�HB	�%B	VB	@�B	.B	\B��B�B��B�^BB��B	5?B	e`B	�7B	��B	��B	�{B	�7B	�dB	��B	��B	��B	��B	ɺB	��B	��B	��B	ȴB	�
B	�
B	�TB	�B	�B	�B	�fB	�TB	�NB	�mB	�B	�B	��B	��B	��B	��B	��B
  B
B
B
+B
	7B
DB
VB
hB
hB
�B
�B
�B
"�B
%�B
&�B
(�B
-B
2-B
:^B
B�B
I�B
Q�B
T�B
YB
]/B
cTB
k�B
o�B
r�B
u�B
v�B
x�B
|�B
�B
�B
�7B
�=11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
H�B
I�B
I�B
H�B
H�B
G�B
G�B
G�B
F�B
F�B
F�B
F�B
F�B
F�B
E�B
E�B
E�B
E�B
bNB
��BhsBo�BjBD�B�oB'�B�B�dB�B_;B
��B
��B
\)B	�sB	�=B	XB	B�B	1'B	hB	  B�B��B�qBÖB��B	6FB	ffB	�=B	��B	��B	��B	�7B	�dB	��B	��B	��B	��B	��B	��B	��B	��B	ɺB	�
B	�B	�TB	�B	�B	�B	�fB	�ZB	�NB	�mB	�B	�B	��B	��B	��B	��B	��B
  B
B
B
+B
	7B
DB
VB
hB
hB
�B
�B
�B
"�B
%�B
&�B
(�B
-B
2-B
:^B
B�B
I�B
Q�B
T�B
YB
]/B
cTB
k�B
o�B
r�B
u�B
v�B
x�B
|�B
�B
�B
�7B
�=11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#�
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
<e`B<#�
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
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ = PRES-SP(ThisCycle), where SP is SURFACE PRESSURE from this cycle                                                                                                                                                                                     TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,PRES_ADJ,elapsed_time,alpha,tau),elapsed_time=P/mean_rise_rate,P=dbar since the start of the profile for each samples                                                                                                       None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            SP(ThisCycle)=0.1(dbar)                                                                                                                                                                                                                                         None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE; PRES_ADJ_ERR : Manufacture sensor accuracy                                                                                                                                                                 TEMP_ADJ_ERR : SBE sensor accuracy                                                                                                                                                                                                                              Salinity Recalculation using PRES_ADJ; PSAL_ADJ_ERR : max(sum of RecalS & CTM errors, 0.01(PSS-78))                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201003132312102010052518492520100525184925201005251859432010052518594320100525185943201010040000002010100400000020101004000000  JA  ARFMdecpA9_d                                                                20100310095714  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20100310095715  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.0                                                                 20100310095716  IP  PRES            G�O�G�O�G�O�                JA  ARCArsal2.1a                                                                20100310095716  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20100310095717  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20100310095718  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20100310095718  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8c                                                                20100310095718  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8c                                                                20100310095718  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20100310095718  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20100310100721                      G�O�G�O�G�O�                JA  ARFMdecpA9_d                                                                20100314045841  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20100314050124  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.0                                                                 20100314050124  IP  PRES            G�O�G�O�G�O�                JA  ARCArsal2.1a                                                                20100314050124  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20100314050125  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20100314050126  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20100314050126  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8c                                                                20100314050126  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8c                                                                20100314050126  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20100314050126  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20100314050729                      G�O�G�O�G�O�                JM  ARGQJMQC1.0                                                                 20100313231210  CV  DAT$            G�O�G�O�F��=                JM  ARCAJMQC1.0                                                                 20100525184925  CV  PRES            G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20100525184925  IP  PSAL            G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20100525185943  CV  PSAL            G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2010V1                                                       20101004000000  IP  PSAL            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20101015025607  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20101015025653                      G�O�G�O�G�O�                JM  RENCREJM1.1c                                                                20150209100414  ED  SCIENTIFIC_CALIBG�O�G�O�G�O�                JA  ARDU                                                                        20150310061231                      G�O�G�O�G�O�                JM      JMFC1.0                                                                 20150611113357                      G�O�G�O�G�O�                JA  ARDU                                                                        20150614170516                      G�O�G�O�G�O�                