CDF   !   
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   t   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JAMSTEC    source        
Argo float     history       b2009-10-21T06:56:49Z creation;2015-03-10T06:12:38Z update;2015-06-11T11:31:25Z conversion to V3.1;     
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
_FillValue                    i8Argo profile    3.1 1.2 19500101000000  5901576 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               3A   JA  20091021065649  20150614170512  A9_76264_051                    2C  D   APEX                            3512                            070407                          846 @�T�C�-1   @�T�*�@)��hr��d�333331   ARGOS   A   A   A   Primary sampling: discrete [spot sampling CTD data]                                                                                                                                                                                                                @�ffA��AfffA���A���A�  B
ffB33B2  BFffBZ  Bo��B�33B�ffB�ffB�33B���B�  B�ffB���B���B�ffB�33B�ffB�ffC� C��C� CffC�CffC33C$33C)� C.L�C3�C8��C=��CB� CG��CQffC[  Ce  CoL�CyL�C��fC��3C�� C���C�� C���C��fC��fC��fC���C���C���C�s3C���CǙ�C̳3C���C֙�C�� C�s3C�fC�3C�fC���C��fD�3D��D��DٚD��D�3D��D$ٚD)��D.�3D3�3D8��D=� DB�3DG��DL��DQ�3DV� D[� D`ٚDe� Dj��Do�3Dt�fDy� D�0 D�i�D��3D��D�&fD�c3D��fD��fD�,�D�` D��fD��3D��D�p Dڣ3D��D�  D�c3D�D��fD�L�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�ffA��AfffA���A���A�  B
ffB33B2  BFffBZ  Bo��B�33B�ffB�ffB�33B���B�  B�ffB���B���B�ffB�33B�ffB�ffC� C��C� CffC�CffC33C$33C)� C.L�C3�C8��C=��CB� CG��CQffC[  Ce  CoL�CyL�C��fC��3C�� C���C�� C���C��fC��fC��fC���C���C���C�s3C���CǙ�C̳3C���C֙�C�� C�s3C�fC�3C�fC���C��fD�3D��D��DٚD��D�3D��D$ٚD)��D.�3D3�3D8��D=� DB�3DG��DL��DQ�3DV� D[� D`ٚDe� Dj��Do�3Dt�fDy� D�0 D�i�D��3D��D�&fD�c3D��fD��fD�,�D�` D��fD��3D��D�p Dڣ3D��D�  D�c3D�D��fD�L�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�t�A�p�A�jA�=qA�33A�-A�"�A�{A��;A�A��A�&�AͅAƟ�A��A���A���A���A��A�  A��!A�VA��/A�jA��A���A�1A��PA���A�;dA�ffA33A{Aq�wAm��A`E�AVJAM��AG��AD�+A<E�A1&�A)��A!�#A�AAbNA"�AA	�-A  A
��A	�An�Ap�Az�AZAdZA�RA��A�P@���@��@��m@���@��7@��@�7L@�+@��@�v�@�J@���@�
=@�7L@�b@�X@��`@���@�@��7@�n�@�I�@���@��@���@�G�@���@��@�"�@���@�1@�1'@�^5@�ȴ@�1@z=q@rJ@fV@Z-@O��@H��@@�9@7�;@01'@'\)@!��@|�@G�@S�@ �@V@I�@
^5@
=@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�t�A�p�A�jA�=qA�33A�-A�"�A�{A��;A�A��A�&�AͅAƟ�A��A���A���A���A��A�  A��!A�VA��/A�jA��A���A�1A��PA���A�;dA�ffA33A{Aq�wAm��A`E�AVJAM��AG��AD�+A<E�A1&�A)��A!�#A�AAbNA"�AA	�-A  A
��A	�An�Ap�Az�AZAdZA�RA��A�P@���@��@��m@���@��7@��@�7L@�+@��@�v�@�J@���@�
=@�7L@�b@�X@��`@���@�@��7@�n�@�I�@���@��@���@�G�@���@��@�"�@���@�1@�1'@�^5@�ȴ@�1@z=q@rJ@fV@Z-@O��@H��@@�9@7�;@01'@'\)@!��@|�@G�@S�@ �@V@I�@
^5@
=@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
!�B
!�B
!�B
�B
 �B
 �B
!�B
$�B
@�B
�hBe`B��B�3B�'B�;B�BbB&�B>wB<jB/B�BhB�B�}B�7BA�BoB
�B
��B
z�B
 �B
B	ŢB	��B	ZB	2-B	uB	B��B�fB��B�RB��B��B��B��B�qB	{B	%B	hB	� B	�hB	�B	�-B	��B	��B	��B	�TB	�yB	�sB	�B	�B	�B	�B	�B	�B	��B	��B	��B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B
B
%B
+B

=B
VB
oB
�B
�B
�B
�B
�B
�B
#�B
$�B
(�B
1'B
8RB
=qB
D�B
J�B
S�B
ZB
]/B
cTB
iyB
o�B
t�B
v�B
{�B
� B
� B
�B
�B
�%B
�7B
�J11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
!�B
!�B
!�B
�B
 �B
 �B
!�B
$�B
B�B
��BiyB��B�XB�?B�NB�B{B)�B@�B@�B33B�B�B��BB�VBE�B�B
�5B
��B
�B
"�B
+B	ȴB	��B	_;B	6FB	�B	B��B�yB��B�^B��B��B��B��B�qB	�B	+B	bB	� B	�oB	�B	�-B	��B	��B	��B	�TB	�B	�yB	�B	�B	�B	�B	�B	�B	��B	��B	��B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B
B
%B
+B

=B
VB
oB
�B
�B
�B
�B
�B
�B
#�B
$�B
(�B
1'B
8RB
=qB
D�B
J�B
S�B
ZB
]/B
cTB
iyB
o�B
t�B
v�B
{�B
� B
� B
�B
�B
�%B
�7B
�J11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ = PRES-SP(NextCycle), where SP is SURFACE PRESSURE from next cycle                                                                                                                                                                                     TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,PRES_ADJ,elapsed_time,alpha,tau),elapsed_time=P/mean_rise_rate,P=dbar since the start of the profile for each samples                                                                                                       None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            SP(NextCycle)=0.0(dbar)                                                                                                                                                                                                                                         None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE; PRES_ADJ_ERR : Manufacture sensor accuracy                                                                                                                                                                 TEMP_ADJ_ERR : SBE sensor accuracy                                                                                                                                                                                                                              Salinity Recalculation using PRES_ADJ; PSAL_ADJ_ERR : max(sum of RecalS & CTM errors, 0.01(PSS-78))                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         200911031421042009110314210420091103142104200911031529262009110315292620091103152926201010040000002010100400000020101004000000  JA  ARFMdecpA9_b                                                                20091021065648  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.5                                                                 20091021065649  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.0                                                                 20091021065649  IP  PRES            G�O�G�O�G�O�                JA  ARCArsal2.1a                                                                20091021065649  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20091021065650  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20091021065651  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20091021065651  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8b                                                                20091021065651  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8b                                                                20091021065651  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20091021065651  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20091021070226                      G�O�G�O�G�O�                JA  ARFMdecpA9_b                                                                20091025035626  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.5                                                                 20091025035804  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.0                                                                 20091025035804  IP  PRES            G�O�G�O�G�O�                JA  ARCArsal2.1a                                                                20091025035805  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20091025035805  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20091025035806  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20091025035806  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8b                                                                20091025035806  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8b                                                                20091025035806  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20091025035807  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20091025040132                      G�O�G�O�G�O�                JM  ARGQJMQC1.0                                                                 20091024224640  CV  DAT$            G�O�G�O�F��8                JM  ARCAJMQC1.0                                                                 20091103142104  IP  PRES            G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20091103142104  IP  PSAL            G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20091103152926  CV  PSAL            G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2010V1                                                       20101004000000  IP  PSAL            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20101015025547  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20101015025638                      G�O�G�O�G�O�                JM  RENCREJM1.1c                                                                20150209100354  ED  SCIENTIFIC_CALIBG�O�G�O�G�O�                JA  ARDU                                                                        20150310061238                      G�O�G�O�G�O�                JM      JMFC1.0                                                                 20150611113116                      G�O�G�O�G�O�                JA  ARDU                                                                        20150614170512                      G�O�G�O�G�O�                