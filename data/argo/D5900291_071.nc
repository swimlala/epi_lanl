CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   I   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JAMSTEC    source        
Argo float     history       b2005-01-09T04:46:07Z creation;2014-07-22T10:04:56Z update;2015-06-08T13:53:38Z conversion to V3.1;     
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
resolution        =���   standard_name         sea_water_pressure     axis      Z        $  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  L  :�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   standard_name         sea_water_pressure       $  ;   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  L  <4   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     $  <�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_temperature        $  =�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  L  >�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_temperature        $  ?   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  L  @8   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     $  @�   PSAL         
      	   	long_name         Practical salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_salinity       $  A�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  L  B�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_salinity       $  C   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  L  D<   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     $  D�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  `  E�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    F   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    L   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    R   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T  X   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    X`   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    Xd   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    Xh   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    Xl   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  Xp   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    X�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    X�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    X�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         X�   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         X�   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        X�   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    X�Argo profile    3.1 1.2 19500101000000  5900291 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               GA   JA  20050109044607  20150617052515  A4_13305_071                    2C  D   APEX                            692                             072602                          846 @Ӡ<1~K1   @ӠAja�]@+XbM���clbM��1   ARGOS   A   A   A   Primary sampling: discrete [spot sampling CTD data]                                                                                                                                                                                                                A33A  A�  A�  B��BE33Bm33B�  B�ffB�33B�  Bڙ�B�ffC��CL�C� C��C)ffC3�3C=ffCG��C[33Co� C��fC��fC��fC��fC�s3C�� C���Cǌ�Cѳ3Cی�C�s3C�s3C��3D��D��DٚD�fD� D�fD�fD$�3D)� D.�fD3�fD8�fD=ٚDBٚDG�3DN3DTFfDZ�3D`� Dg3DmY�Ds��DyٚD�  D�i�D��3D�� D��D�l�D��3D�ffD���D�l�D�� D�\�D��3D�31111111111111111111111111111111111111111111111111111111111111111111111111   @�33@���AnffA�33B33B2��BZ��B���B�33B�  B���B�ffB�33B�  C�3C�fC  C$��C/�C8��CC  CV��Cj�fC~�3C�Y�C�Y�C�Y�C�&fC�s3C�L�C�@ C�ffC�@ C�&fC�&fC�ffD �fD�fD
�3D� D��D� D� D#��D(��D-� D2� D7� D<�3DA�3DF��DL��DS  DYl�D_��De��Dl33Drs3Dx�3D���D��fD� D�\�D���D�ٚD�P D��3D�Y�D�ٚD�L�D�ɚD�P D�� 1111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�z�A�z�A�z�A�S�A�E�A�I�A�?}A�oA�(�A�O�A�M�A���A�1A�1'A���A�~�A�XAw�FAeƨAR��AL1'A6�RA)+A'�
AhsA9XA��A��At�AC�A��A@�{@�ƨ@�(�@�(�@߅@�M�@ԛ�@��`@ɺ^@ƸR@�{@���@�x�@�V@��@�E�@���@��u@��7@��9@��`@���@�@�33@�;d@���@�G�@��j@|�@s��@i&�@`Q�@W�@K�@?+@2~�@#ƨ@��@��@	X@�u1111111111111111111111111111111111111111111111111111111111111111111111111   A�z�A�z�A�z�A�S�A�E�A�I�A�?}A�oA�(�A�O�A�M�A���A�1A�1'A���A�~�A�XAw�FAeƨAR��AL1'A6�RA)+A'�
AhsA9XA��A��At�AC�A��A@�{@�ƨ@�(�@�(�@߅@�M�@ԛ�@��`@ɺ^@ƸR@�{@���@�x�@�V@��@�E�@���@��u@��7@��9@��`@���@�@�33@�;d@���@�G�@��j@|�@s��@i&�@`Q�@W�@K�@?+@2~�@#ƨ@��@��@	X@�u1111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	�B	�B	�B	�B	�B	�B	%�B	>wB	�9B�%B=qBu�B�B�BZB
�/B
{B	�=B	:^B	  B��BɺB	B	y�B	C�B	�1B	ÖB	�B	��B	�B	�`B	��B	��B	��B	��B	�#B	�)B	�`B	�NB	�B	�fB	�B	�B	��B	�B	��B	��B
  B
B
DB
VB
bB
uB
�B
�B
!�B
&�B
)�B
-B
49B
<jB
>wB
D�B
J�B
O�B
XB
_;B
hsB
r�B
x�B
�B
�%B
�%1111111111111111111111111111111111111111111111111111111111111111111111111   B	�B	�B	�B	�B	�B	�B	&�B	?}B	�?B�+B?}Bw�B�)B�B\)B
�;B
�B	�JB	<jB	B��B��B	+B	{�B	E�B	�=B	ŢB	��B
B	�B	�mB	�B	��B	��B	�
B	�/B	�5B	�mB	�ZB	�B	�sB	�B	��B	��B	�B	��B
  B
B
B
PB
bB
oB
�B
�B
 �B
#�B
(�B
,B
/B
6FB
>wB
@�B
F�B
L�B
Q�B
ZB
aHB
jB
t�B
z�B
�B
�1B
�11111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - SP, where SP is SURFACE PRESSURE (minus 5 dbar for Apf-6,7,8) from next cycle.                                                                                                                                                           none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL(PRES_ADJUSTED,TEMP,Conductivity)                                                                                                                                                                                                           none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            SP(NextCycle) = 4.6 dbar                                                                                                                                                                                                                                        none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using reported SURFACE PRESSURE; PRES_ADJ_ERR : Manufacture sensor accuracy                                                                                                                                                               The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Salinity Recalculation using ADJUSTED Pressure                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         200501200000002005012000000020050120000000200506060000002005060600000020050606000000JA  ARFMfmtp2.1                                                                 20050109044607  IP                  G�O�G�O�G�O�                JA  ARGQrqcp2.3                                                                 20050109044610  QCP$                G�O�G�O�G�O�           1FB7CJA  ARUP                                                                        20050109045435                      G�O�G�O�G�O�                JA  ARFMfmtp2.1                                                                 20050112224705  IP                  G�O�G�O�G�O�                JA  ARGQrqcp2.3                                                                 20050112224705  QCP$                G�O�G�O�G�O�           1FB7CJA  ARGQrelo2.1                                                                 20050112224705  CV  TIME            G�O�G�O�F�                JA  ARGQrelo2.1                                                                 20050112224705  CV  LAT$            G�O�G�O�AZ��                JA  ARGQrelo2.1                                                                 20050112224705  CV  LON$            G�O�G�O��`�                JA  ARUP                                                                        20050112225344                      G�O�G�O�G�O�                JM  ARCAJMQC                                                                    20050120000000  CV  PRES            G�O�G�O�G�O�                JM  ARCAJMQC                                                                    20050120000000  CV  PSAL            G�O�G�O�G�O�                JM  ARSQWJO 1   SeHyD1                                                          20050606000000  IP  PSAL            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20060131021142  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20060202012858                      G�O�G�O�G�O�                JM  AREVjmrvp1.2                                                                20090312113659  IP  PRES            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20090318051005  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20090318051725                      G�O�G�O�G�O�                JM  AREQREJM1.0                                                                 20140715030650  CF  POSITION_QC     G�O�G�O�@�                  JA  RFMTcnvd2.1                                                                 20140722100343  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20140722100456                      G�O�G�O�G�O�                JM      JMFC1.0                                                                 20150608135328                      G�O�G�O�G�O�                JA  ARDU                                                                        20150617052515                      G�O�G�O�G�O�                