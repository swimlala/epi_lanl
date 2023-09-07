CDF      
      STRING16      STRING4       	DATE_TIME         N_PROF        STRING8       STRING64   @   N_PARAM       STRING2       STRING32       N_LEVELS   H   N_CALIB       	STRING256         	N_HISTORY             	   title         Argo float vertical profile    institution       JAMSTEC    source        
Argo float     history       b2004-04-04T18:49:24Z creation;2009-03-18T05:17:45Z update;2015-06-08T14:14:11Z conversion to V3.1;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                   	long_name         	Data type      
_FillValue               conventions       Argo reference table 1          6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS                        	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DATE_CREATION                  	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
_FillValue        A.�~       standard_name         time   
resolution        >�����h�   axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
_FillValue        A.�~       
resolution        >�����h�        8l   LATITUDE               	long_name         &Latitude of the station, best estimate     units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        standard_name         latitude   axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        standard_name         	longitude      axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PRES         	      
   	long_name         )Sea water pressure, equals 0 at sea-level      
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   standard_name         sea_water_pressure     axis      Z           9�   PRES_QC          	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  H  :�   PRES_ADJUSTED            	      	   	long_name         )Sea water pressure, equals 0 at sea-level      
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   standard_name         sea_water_pressure          ;   PRES_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  H  <(   PRES_ADJUSTED_ERROR          	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���        <p   TEMP         	      	   	long_name         $Sea temperature in-situ ITS-90 scale   
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_temperature           =�   TEMP_QC          	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  H  >�   TEMP_ADJUSTED            	      	   	long_name         $Sea temperature in-situ ITS-90 scale   
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_temperature           >�   TEMP_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  H  @   TEMP_ADJUSTED_ERROR          	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o        @`   PSAL         	      	   	long_name         Practical salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_salinity          A�   PSAL_QC          	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  H  B�   PSAL_ADJUSTED            	      	   	long_name         Practical salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_salinity          B�   PSAL_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  H  D   PSAL_ADJUSTED_ERROR          	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o        DP   	PARAMETER            
                	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  `  Ep   SCIENTIFIC_CALIB_EQUATION            
               	long_name         'Calibration equation for this parameter    
_FillValue                    E�   SCIENTIFIC_CALIB_COEFFICIENT         
               	long_name         *Calibration coefficients for this equation     
_FillValue                    K�   SCIENTIFIC_CALIB_COMMENT         
               	long_name         .Comment applying to this parameter calibration     
_FillValue                    Q�   SCIENTIFIC_CALIB_DATE            
               	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T  W�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    X$   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    X(   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    X,   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    X0   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  X4   HISTORY_DATE                     	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    Xt   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    X�   HISTORY_PARAMETER                         	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    X�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         X�   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         X�   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        X�   HISTORY_QCTEST                        	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    X�Argo profile    3.1 1.2 19500101000000  5900292 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               +A   JA  20040404184924  20150613224515  A4_26558_043                    2C  D   APEX                            700                             072602                          846 @�Zi5y�1   @�Zl�n]R@,dZ�1�d\bM��1   ARGOS   A   A   A   Primary sampling: discrete [spot sampling CTD data]                                                                                                                                                                                                                A33A�33A�  B��BE33Bl��B�  B���B���B�33B�33B�33C �fC��C� CL�C)L�C3�C=�3CG��C[L�Co� C��3C��fC���C��3C�� C���C�� Cǌ�Cѳ3C۳3C噚C�fC��3D�3D� D� D�3D�fDٚD��D$�fD)��D.� D3��D8�3D=�3DB� DG�3DN�DTY�DZ��D`��Dg�DmS3Ds�fDy��D�&fD�s3D���D�� D�0 D�ffD���D�l�D��fD�p D��3D�` D��3D���111111111111111111111111111111111111111111111111111111111111111111111111@�  Ac33A�ffB��B0ffBX  B���B�33B�ffB���B���B���B�ffCffCL�C�C$�C-�fC8� CBffCV�CjL�C~33C��C�  C��C�&fC�  C�&fC��3C��C��C�  C��C��D �fDs3D
�3D�fDy�D��Dl�D#y�D(` D-s3D2l�D7ffD<�fDA�3DF�fDL��DS�DYL�D_� De� DlfDr9�Dxl�D�� D���D�3D�I�D���D�� D�FfD��fD�@ D�ɚD�L�D칚D�<�D�3111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�ĜAξwAξwA���A�ĜA���A��A���A���A��A���A�&�A��RA�33A�(�A�dZA�ȴA�XA�~�AhAM��A;+A6��A#S�A��A�A&�A1'An�@��+@��@�v�@�(�@�D@��H@�V@�@�%@��@�Q�@�Ĝ@�j@��@���@��@�5?@��;@���@�Ĝ@�I�@��9@���@��T@�l�@�&�@�E�@�7L@��@��@~V@t�/@j��@b��@Z��@N$�@?��@/;d@#33@�`@�@�`@��111111111111111111111111111111111111111111111111111111111111111111111111A�ĜAξwAξwA���A�ĜA���A��A���A���A��A���A�&�A��RA�33A�(�A�dZA�ȴA�XA�~�AhAM��A;+A6��A#S�A��A�A&�A1'An�@��+@��@�v�@�(�@�D@��H@�V@�@�%@��@�Q�@�Ĝ@�j@��@���@��@�5?@��;@���@�Ĝ@�I�@��9@���@��T@�l�@�&�@�E�@�7L@��@��@~V@t�/@j��@b��@Z��@N$�@?��@/;d@#33@�`@�@�`@��111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	6FB	5?B	6FB	6FB	7LB	7LB	�fB
��B��B��B�By�B�!B��BDBVBbNB
ŢB
|�B	�'B	/B	%B	�B	e`B	%B	C�B	�B	��B	�B	B	B	��B	��B	�BB	�;B	�TB	�B	�B	�mB	�B	�mB	�sB	�B	��B	��B
B
B
B
+B
VB
VB
�B
�B
�B
 �B
#�B
$�B
&�B
,B
/B
49B
9XB
?}B
C�B
K�B
T�B
aHB
l�B
s�B
z�B
�B
�111111111111111111111111111111111111111111111111111111111111111111111111B	8RB	7LB	8RB	8RB	9XB	9XB	�sB
��B��B��B �B{�B�-B��BPBbBdZB
ǮB
~�B	�3B	1'B	1B	�B	gmB	1B	E�B	�+B	�B	�!B	ŢB	ĜB	��B	�
B	�NB	�NB	�`B	�B	�B	�B	�B	�B	�B	�B	��B	��B
B
1B
+B

=B
hB
hB
�B
�B
!�B
#�B
&�B
'�B
)�B
/B
2-B
7LB
<jB
B�B
F�B
N�B
XB
dZB
o�B
v�B
}�B
�%B
�+111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ = PRES-SP(NextCycle), where SP is SURFACE PRESSURE from next cycle                                                                                                                                                                                     none                                                                                                                                                                                                                                                            PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            SP(NextCycle) = 5.2 dbar                                                                                                                                                                                                                                        none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE; PRES_ADJ_ERR : Manufacture sensor accuracy                                                                                                                                                                 TEMP_ADJ_ERR : SBE sensor accuracy                                                                                                                                                                                                                              Salinity Recalculation using ADJUSTED Pressure                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         200404160000002004041600000020040416000000200409030000002004090300000020040903000000JA  ARFMfmtp2.0                                                                 20040404184924  IP                  G�O�G�O�G�O�                JA  ARGQrqcp2.2                                                                 20040404184925  QCP$                G�O�G�O�G�O�           1FB7CJM  ARCAJMQC                                                                    20040416000000  CV  PRES            G�O�G�O�G�O�                JM  ARCAJMQC                                                                    20040416000000  CV  PSAL            G�O�G�O�G�O�                JM  ARSQWJO 1   SeHyD1                                                          20040903000000  IP  PSAL            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20060131021151  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20060202032048                      G�O�G�O�G�O�                JM  AREVjmrvp1.2                                                                20090312113719  IP  PRES            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20090318051038  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20090318051745                      G�O�G�O�G�O�                JM      JMFC1.0                                                                 20150608141407                      G�O�G�O�G�O�                JA  ARDU                                                                        20150613224515                      G�O�G�O�G�O�                