CDF   $   
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   t   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JAMSTEC    source        
Argo float     history       b2009-11-30T06:58:41Z creation;2015-03-10T06:12:39Z update;2015-06-11T11:32:13Z conversion to V3.1;     
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
_FillValue                    i8Argo profile    3.1 1.2 19500101000000  5901576 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               7A   JA  20091130065841  20150614170512  A9_76264_055                    2C  D   APEX                            3512                            070407                          846 @�^�$�8�1   @�^���/�@*n��O�;�d��;dZ1   ARGOS   A   A   A   Primary sampling: discrete [spot sampling CTD data]                                                                                                                                                                                                                @�33A��A`  A���A�  A陚B	33B��B0��BE33BZffBnffB���B�  B�ffB�  B�33B�  B�  B���B�33B�  B�  B�  B�ffC�C33C�3CL�C��CffCL�C$ffC)� C.33C3�C8ffC=�3CB� CGL�CP�fC[�CeL�Co��CyffC�� C��3C�� C�s3C�s3C�s3C�ffC�� C��3C���C���C���C��3C�Cǌ�C̀ Cљ�C֦fCی�C�� C噚C�s3CC��C���D� D� D�3DٚD� D�fD��D$� D)� D.��D3��D8�3D=�3DB�fDG� DL� DQ� DV�3D[��D`�3De� Dj�fDo�fDt� DyٚD�  D�l�D��3D�� D�,�D�ffD���D�� D�  D�\�D��fD��fD�)�D�i�Dڰ D�� D��D�ffD�D��3D�0 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@���A	��A\��A�33A�ffA�  BffB��B0  BDffBY��Bm��B�ffB���B�  B���B���B���B���B�ffB���Bٙ�B㙚BB�  C �fC  C� C�CffC33C�C$33C)L�C.  C2�fC833C=� CBL�CG�CP�3CZ�fCe�CoffCy33C��fC���C�ffC�Y�C�Y�C�Y�C�L�C��fC���C�� C�� C�� C���C C�s3C�ffCр C֌�C�s3C�ffC� C�Y�C� C�s3C�s3D�3D�3D�fD��D�3D��D��D$�3D)�3D.� D3��D8�fD=�fDB��DG�3DL�3DQ�3DV�fD[� D`�fDe�3Dj��Do��Dt�3Dy��D��D�ffD���D��D�&fD�` D��3D��D��D�VfD�� D�� D�#3D�c3Dک�D�ٚD�fD�` D�3D���D�)�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A���A���A���A���A���A�  A�A�A�A���A��A��A��A�z�A�A���A�Q�A���A�r�A�ffA��A�\)A�O�A���A��9A��A��A��^A�{A�G�A��A�JA���A���A�ĜAx�yAr�yA[�^ASC�AC�;A:=qA1S�A-
=A(-AȴA�`A�TA�A��AVA�A
��A	�A7LA7LA{AA ff@�1'@�$�@�I�@��y@�7@�\)@���@��@�~�@�bN@�{@�O�@�/@�{@���@ēu@�  @�?}@��@�b@�O�@��R@�\)@�O�@��;@��j@��@���@���@��@���@���@���@��@�Z@�33@��`@w��@n5?@bJ@X��@P�`@I�@B�!@4j@-V@"�@�-@�m@�^@��@Z@b@�/@
=q@{@/11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��A���A���A���A���A���A�  A�A�A�A���A��A��A��A�z�A�A���A�Q�A���A�r�A�ffA��A�\)A�O�A���A��9A��A��A��^A�{A�G�A��A�JA���A���A�ĜAx�yAr�yA[�^ASC�AC�;A:=qA1S�A-
=A(-AȴA�`A�TA�A��AVA�A
��A	�A7LA7LA{AA ff@�1'@�$�@�I�@��y@�7@�\)@���@��@�~�@�bN@�{@�O�@�/@�{@���@ēu@�  @�?}@��@�b@�O�@��R@�\)@�O�@��;@��j@��@���@���@��@���@���@���@��@�Z@�33@��`@w��@n5?@bJ@X��@P�`@I�@B�!@4j@-V@"�@�-@�m@�^@��@Z@b@�/@
=q@{@/11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
R�B
S�B
T�B
S�B
S�B
S�B
VB
VB
W
B
XB
ZB
ZB
^5B
�1B��B�'B�HB��B!�B)�BYBn�Bm�BR�B>wB#�B��B�}Bm�BP�B9XB�B
�B
��B
z�B
I�B	�B	��B	D�B	�B��B�B�B	�B	W
B	bNB	s�B	�B	�LB	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
%B
	7B
DB
JB
\B
uB
�B
�B
�B
 �B
"�B
&�B
(�B
-B
.B
5?B
;dB
?}B
E�B
K�B
P�B
VB
ZB
dZB
jB
q�B
t�B
x�B
z�B
|�B
� B
�B
�+B
�7B
�VB
�\11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
R�B
S�B
T�B
S�B
S�B
S�B
VB
VB
W
B
XB
ZB
ZB
`BB
��B��B�LB�ZBB"�B.B[#Bo�Bq�BS�B?}B&�B��BĜBo�BR�B;dB�B
�#B
�B
~�B
P�B	�)B	�B	I�B	!�B��B�B�B	�B	ZB	cTB	t�B	�B	�RB	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
%B
	7B
DB
JB
\B
uB
�B
�B
�B
 �B
"�B
&�B
(�B
-B
.B
5?B
;dB
?}B
E�B
K�B
P�B
VB
ZB
dZB
jB
q�B
t�B
x�B
z�B
|�B
� B
�B
�+B
�7B
�VB
�\11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#�
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
<�t�<#�
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
<49X<#�
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
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ = PRES-SP(NextCycle), where SP is SURFACE PRESSURE from next cycle                                                                                                                                                                                     TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,PRES_ADJ,elapsed_time,alpha,tau),elapsed_time=P/mean_rise_rate,P=dbar since the start of the profile for each samples                                                                                                       None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            SP(NextCycle)=0.2(dbar)                                                                                                                                                                                                                                         None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE; PRES_ADJ_ERR : Manufacture sensor accuracy                                                                                                                                                                 TEMP_ADJ_ERR : SBE sensor accuracy                                                                                                                                                                                                                              Salinity Recalculation using PRES_ADJ; PSAL_ADJ_ERR : max(sum of RecalS & CTM errors, 0.01(PSS-78))                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         200912131409192009121314091920091213140919200912131441252009121314412520091213144125201010040000002010100400000020101004000000  JA  ARFMdecpA9_c                                                                20091130065840  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20091130065841  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.0                                                                 20091130065841  IP  PRES            G�O�G�O�G�O�                JA  ARCArsal2.1a                                                                20091130065842  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20091130065842  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20091130065843  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20091130065843  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8c                                                                20091130065843  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8c                                                                20091130065843  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20091130065843  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20091130070507                      G�O�G�O�G�O�                JA  ARFMdecpA9_d                                                                20091204035730  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20091204035934  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.0                                                                 20091204035935  IP  PRES            G�O�G�O�G�O�                JA  ARCArsal2.1a                                                                20091204035935  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20091204035935  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20091204035937  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20091204035937  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8c                                                                20091204035937  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8c                                                                20091204035937  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20091204035937  QCP$                G�O�G�O�G�O�           10000JA  ARGQrelo2.1                                                                 20091204035937  CV  TIME            G�O�G�O�F��H                JA  ARGQrelo2.1                                                                 20091204035937  CV  LAT$            G�O�G�O�ASt�                JA  ARGQrelo2.1                                                                 20091204035937  CV  LON$            G�O�G�O��&�                JA  ARUP                                                                        20091204040415                      G�O�G�O�G�O�                JM  ARGQJMQC1.0                                                                 20091203231919  CV  DAT$            G�O�G�O�F��8                JM  ARCAJMQC1.0                                                                 20091213140919  CV  PRES            G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20091213140919  IP  PSAL            G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20091213144125  CV  PSAL            G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2010V1                                                       20101004000000  IP  PSAL            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20101015025541  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20101015025636                      G�O�G�O�G�O�                JM  RENCREJM1.1c                                                                20150209100400  ED  SCIENTIFIC_CALIBG�O�G�O�G�O�                JA  ARDU                                                                        20150310061239                      G�O�G�O�G�O�                JM      JMFC1.0                                                                 20150611113202                      G�O�G�O�G�O�                JA  ARDU                                                                        20150614170512                      G�O�G�O�G�O�                