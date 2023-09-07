CDF      
      STRING16      STRING4       	DATE_TIME         N_PROF        STRING8       STRING64   @   N_PARAM       STRING2       STRING32       N_LEVELS   H   N_CALIB       	STRING256         	N_HISTORY             	   title         Argo float vertical profile    institution       JAMSTEC    source        
Argo float     history       b2004-06-23T22:49:40Z creation;2009-03-18T05:18:04Z update;2015-06-08T14:15:44Z conversion to V3.1;     
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
_FillValue                    X�Argo profile    3.1 1.2 19500101000000  5900292 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               3A   JA  20040623224940  20150613224517  A4_26558_051                    2C  D   APEX                            700                             072602                          846 @�nh�ci1   @�njZ�$�@.5\(��dd     1   ARGOS   A   A   A   Primary sampling: discrete [spot sampling CTD data]                                                                                                                                                                                                                A33A�  A���B33BD  Bm��B���B�33B�33B���B���B���C��CffCL�C� C)ffC3��C=�3CG� C[�3Co� C�� C�� C���C���C���C��fC�Y�Cǳ3C���C�� C�� C�fC��3D�fD�fD� DٚD��D��D�3D$�fD)�fD.��D3� D8� D=ٚDB��DG� DN  DT33DZ��D`ٚDg3DmFfDs� Dy�3D�#3D�p D�� D��D�,�D�s3D��3D�i�D�� D�p D�� D�\�D��3D�� 111111111111111111111111111111111111111111111111111111111111111111111111@�  Ad��A�33BffB/33BX��B�ffB���B���B�ffB�ffB�ffB�33C33C�CL�C$33C.ffC8� CBL�CV� CjL�C}��C�&fC�33C��3C��3C��C�� C��C�33C�&fC�&fC��C��D ��Dy�D
�3D��D� D� D�fD#��D(y�D-� D2�3D7s3D<��DAl�DFs3DL�3DR�fDYL�D_��De�fDk��Dr33Dx�fD�|�D�ɚD�	�D�C3D��fD���D�<�D��3D�I�D�ɚD�9�D�fD�<�D���111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�ƨA��/A��HA��HA��
A���AՕ�A��#A�p�A�bNA��A�=qAȋDA�A��FA�`BA���A��A��DA�ffA���Al=qATffAB��A2=qA!�A�A
ZA��@���@��@��@��@�J@�`B@�$�@��`@˶F@�;d@�z�@�5?@���@�=q@��P@���@���@�(�@��m@�{@��F@��j@�X@��#@��@��H@�1'@�K�@�I�@��@+@sdZ@h��@\(�@S�m@F5?@8�@*�\@\)@O�@�y@	��@	�#111111111111111111111111111111111111111111111111111111111111111111111111A�ƨA��/A��HA��HA��
A���AՕ�A��#A�p�A�bNA��A�=qAȋDA�A��FA�`BA���A��A��DA�ffA���Al=qATffAB��A2=qA!�A�A
ZA��@���@��@��@��@�J@�`B@�$�@��`@˶F@�;d@�z�@�5?@���@�=q@��P@���@���@�(�@��m@�{@��F@��j@�X@��#@��@��H@�1'@�K�@�I�@��@+@sdZ@h��@\(�@S�m@F5?@8�@*�\@\)@O�@�y@	��@	�#111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	�qB	�jB	�qB	�jB	�jB	�dB	�dB	�LB	�jB	�/B
~�BJBo�B��BB.B@�B\)B0!B�B
��B	��B	O�B	A�B	!�B�B	.BÖB�5B	B	-B	N�B	�7B	�'B	�-B	�wB	ɺB	ÖB	��B	��B	�B	�BB	�yB	�B	�B	��B	��B	��B
  B
B
%B
DB
\B
{B
�B
�B
�B
!�B
(�B
.B
5?B
9XB
D�B
I�B
Q�B
ZB
cTB
m�B
u�B
|�B
�B
�111111111111111111111111111111111111111111111111111111111111111111111111B	�}B	�wB	�}B	�wB	�wB	�qB	�qB	�XB	�wB	�;B
�BVBq�B��BB0!BB�B^5B2-B��B
ÖB	��B	Q�B	C�B	#�B�B	0!BŢB�BB	1B	/B	Q�B	�DB	�3B	�?B	��B	��B	ƨB	��B	�
B	�)B	�TB	�B	�B	��B
  B
B
B
B
%B
	7B
VB
oB
�B
�B
�B
"�B
$�B
,B
1'B
8RB
<jB
G�B
L�B
T�B
]/B
ffB
p�B
x�B
� B
�+B
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
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ = PRES-SP(NextCycle), where SP is SURFACE PRESSURE from next cycle                                                                                                                                                                                     none                                                                                                                                                                                                                                                            PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            SP(NextCycle) = 5.2 dbar                                                                                                                                                                                                                                        none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE; PRES_ADJ_ERR : Manufacture sensor accuracy                                                                                                                                                                 TEMP_ADJ_ERR : SBE sensor accuracy                                                                                                                                                                                                                              Salinity Recalculation using ADJUSTED Pressure                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         200407050000002004070500000020040705000000200409030000002004090300000020040903000000JA  ARFMfmtp2.0                                                                 20040623224940  IP                  G�O�G�O�G�O�                JA  ARGQrqcp2.2                                                                 20040623224941  QCP$                G�O�G�O�G�O�           1FB7CJM  ARCAJMQC                                                                    20040705000000  IP  PRES            G�O�G�O�G�O�                JM  ARCAJMQC                                                                    20040705000000  CV  PSAL            G�O�G�O�G�O�                JM  ARSQWJO 1   SeHyD1                                                          20040903000000  IP  PSAL            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20060131021152  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20060202032152                      G�O�G�O�G�O�                JM  AREVjmrvp1.2                                                                20090312113720  IP  PRES            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20090318051110  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20090318051804                      G�O�G�O�G�O�                JM      JMFC1.0                                                                 20150608141535                      G�O�G�O�G�O�                JA  ARDU                                                                        20150613224517                      G�O�G�O�G�O�                