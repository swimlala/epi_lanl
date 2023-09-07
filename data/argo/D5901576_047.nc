CDF   !   
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   t   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JAMSTEC    source        
Argo float     history       b2009-09-11T06:57:44Z creation;2015-03-10T06:12:37Z update;2015-06-11T11:30:40Z conversion to V3.1;     
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
_FillValue                    i8Argo profile    3.1 1.2 19500101000000  5901576 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               /A   JA  20090911065744  20150614170513  A9_76264_047                    2C  D   APEX                            3512                            070407                          846 @�Jƃ9D�1   @�J�i��@+�t��d���+1   ARGOS   A   A   A   Primary sampling: discrete [spot sampling CTD data]                                                                                                                                                                                                                @���A��AfffA�ffA�ffA���B	��B��B133BE33BY33Bl��B���B�ffB�ffB���B���B�  B�33B���Bљ�B�  B���B���B�  C  CL�CffC�fC� C��C�3C$� C)ffC.�C3L�C8  C=�CB� CG��CQ�3C[33Ce�Co�Cx�fC���C��fC�� C��3C�� C���C���C�� C�� C�� C���C���C���C�CǦfC̳3Cь�Cր CۦfC�� C��CꙚCC��fC���D�3D�3D� D�3D�3D��D�fD$�fD)��D.�3D3� D8�fD=��DB� DG� DL��DQ��DV��D[�3D`��De�3DjٚDoٚDt� DyٚD�,�D�` D��3D��3D�)�D�l�D��fD��3D�)�D�p D���D�� D�  D�ffDڦfD��fD�,�D�` D��D�� D��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�ffA33Ad��A���A���A�  B	33B33B0��BD��BX��BlffB���B�33B�33B�ffB���B���B�  Bƙ�B�ffB���B䙚BB���C �fC33CL�C��CffC� C��C$ffC)L�C.  C333C7�fC=  CBffCG� CQ��C[�Ce  Co  Cx��C�� C���C��3C��fC��3C���C�� C��3C��3C��3C���C���C�� C CǙ�C̦fCр C�s3Cۙ�C�s3C� C��C��C���C�� D��D��DٚD��D��D�fD� D$� D)�3D.��D3ٚD8� D=�fDB��DG��DL�3DQ�fDV�fD[��D`�fDe��Dj�3Do�3DtٚDy�3D�)�D�\�D�� D�� D�&fD�i�D��3D�� D�&fD�l�D���D���D��D�c3Dڣ3D��3D�)�D�\�D�D���D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�VA�S�A�O�A��A�9A�M�A�K�A�?}A�7LA�33A�/A�1A�G�A�Aڙ�A��yA�`BA�33A�(�A��jA���A���A�5?A���A�`BA���A���A�hsA�I�A���A��A�z�A�/A���A��A��A��Ayl�An$�Ae�AX�!AG��A>�A65?A3��A21A,�A%;dA$Q�Ax�AVA{A�^A��A��A
 �AZA�A�Az�A�;A�
A��A (�@�+@��@�&�@��@�1'@�x�@�-@�+@ڰ!@��@д9@�V@\@��w@��9@�O�@��@�
=@�ƨ@��#@��F@�@�I�@��@��h@���@��D@��+@��@�5?@�ƨ@��/@��@v��@l��@b�H@Y��@L1@E��@<�D@5/@/�;@)�#@$j@��@&�@/@��@?}@	�#@K�@�-11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�VA�S�A�O�A��A�9A�M�A�K�A�?}A�7LA�33A�/A�1A�G�A�Aڙ�A��yA�`BA�33A�(�A��jA���A���A�5?A���A�`BA���A���A�hsA�I�A���A��A�z�A�/A���A��A��A��Ayl�An$�Ae�AX�!AG��A>�A65?A3��A21A,�A%;dA$Q�Ax�AVA{A�^A��A��A
 �AZA�A�Az�A�;A�
A��A (�@�+@��@�&�@��@�1'@�x�@�-@�+@ڰ!@��@д9@�V@\@��w@��9@�O�@��@�
=@�ƨ@��#@��F@�@�I�@��@��h@���@��D@��+@��@�5?@�ƨ@��/@��@v��@l��@b�H@Y��@L1@E��@<�D@5/@/�;@)�#@$j@��@&�@/@��@?}@	�#@K�@�-11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB^5B_;B_;BZBT�BR�BR�BR�BQ�BQ�BR�BYBx�B��B��B�B�BZBr�Bx�B�\B��B��B�\B�Bt�BffBS�BK�B�B�HB��B2-B
�HB
��B
l�B
�B	�sB	�B	�B	F�B	B��B�B�B�B��B	VB	\B	"�B	=qB	k�B	�=B	�B	ɺB	�B	�ZB	�`B	�sB	�B	�B	��B
B
%B
B
B
B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
1B

=B
DB
bB
oB
uB
�B
�B
�B
�B
 �B
!�B
$�B
&�B
0!B
49B
:^B
@�B
E�B
I�B
R�B
VB
[#B
_;B
e`B
iyB
l�B
q�B
u�B
y�B
~�B
�B
�%B
�7B
�D11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B^5B_;B_;BZBT�BR�BR�BR�BQ�BQ�BR�BZBy�B��B��B�/B�B]/Bt�Bz�B�{B��B��B�bB�Bu�BhsBT�BO�B!�B�`B��B8RB
�`B
��B
q�B
�B	�B	�!B	�B	K�B	B��B�B�B�B��B	VB	oB	"�B	=qB	l�B	�DB	�B	��B	�B	�`B	�`B	�sB	�B	�B	��B
B
+B
%B
B
B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
1B

=B
DB
bB
oB
uB
�B
�B
�B
�B
 �B
!�B
$�B
&�B
0!B
49B
:^B
@�B
E�B
I�B
R�B
VB
[#B
_;B
e`B
iyB
l�B
q�B
u�B
y�B
~�B
�B
�%B
�7B
�D11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ = PRES-SP(NextCycle), where SP is SURFACE PRESSURE from next cycle                                                                                                                                                                                     TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,PRES_ADJ,elapsed_time,alpha,tau),elapsed_time=P/mean_rise_rate,P=dbar since the start of the profile for each samples                                                                                                       None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            SP(NextCycle)=0.1(dbar)                                                                                                                                                                                                                                         None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE; PRES_ADJ_ERR : Manufacture sensor accuracy                                                                                                                                                                 TEMP_ADJ_ERR : SBE sensor accuracy                                                                                                                                                                                                                              Salinity Recalculation using PRES_ADJ; PSAL_ADJ_ERR : max(sum of RecalS & CTM errors, 0.01(PSS-78))                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         200909241415352009092414153520090924141535200909241701482009092417014820090924170148201010040000002010100400000020101004000000  JA  ARFMdecpA9_b                                                                20090911065743  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.5                                                                 20090911065744  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.0                                                                 20090911065745  IP  PRES            G�O�G�O�G�O�                JA  ARCArsal2.1a                                                                20090911065745  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20090911065745  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20090911065746  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20090911065746  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8b                                                                20090911065746  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8b                                                                20090911065746  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20090911065746  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20090911070317                      G�O�G�O�G�O�                JA  ARFMdecpA9_b                                                                20090915035724  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.5                                                                 20090915035834  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.0                                                                 20090915035835  IP  PRES            G�O�G�O�G�O�                JA  ARCArsal2.1a                                                                20090915035835  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20090915035835  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20090915035836  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20090915035836  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8b                                                                20090915035836  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8b                                                                20090915035836  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20090915035837  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20090915040154                      G�O�G�O�G�O�                JM  ARGQJMQC1.0                                                                 20090914231635  CV  DAT$            G�O�G�O�F�VH                JM  ARCAJMQC1.0                                                                 20090924141535  CV  PRES            G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20090924141535  IP  PSAL            G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20090924170148  CV  PSAL            G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2010V1                                                       20101004000000  IP  PSAL            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20101015025545  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20101015025638                      G�O�G�O�G�O�                JM  RENCREJM1.1c                                                                20150209100348  ED  SCIENTIFIC_CALIBG�O�G�O�G�O�                JA  ARDU                                                                        20150310061237                      G�O�G�O�G�O�                JM      JMFC1.0                                                                 20150611113031                      G�O�G�O�G�O�                JA  ARDU                                                                        20150614170513                      G�O�G�O�G�O�                