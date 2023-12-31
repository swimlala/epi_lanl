CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   o   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JAMSTEC    source        
Argo float     history       b2005-04-22T06:47:35Z creation;2011-06-06T00:20:32Z update;2015-06-09T09:27:57Z conversion to V3.1;     
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
_FillValue                  p  ;\   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   standard_name         sea_water_pressure       �  ;�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  p  =�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  =�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_temperature        �  ?�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  p  Ap   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_temperature        �  A�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  p  C�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  D   PSAL         
      	   	long_name         Practical salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_salinity       �  E�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  p  G�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_salinity       �  G�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  p  I�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  J    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  K�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   Ll   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   Ul   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ^l   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  gl   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    g�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    g�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    g�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    g�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  g�   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    h<   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    hL   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    hP   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         h`   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         hd   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        hh   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    hlArgo profile    3.1 1.2 19500101000000  5900492 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               ,A   JA  20050422064735  20150621114518  A5_21055_044                    2C  D   APEX                            1091                            061703                          846 @ӹ��Y�`1   @Ӻs�u@&�O�;dZ�d�G�{1   ARGOS   A   A   A   Primary sampling: discrete [spot sampling CTD data]                                                                                                                                                                                                                @�33A��Aa��A�  A�  A�33B	33B��B0  BD��BX  Bk33B��B�ffB�ffB���B�ffB���B���B�  B�33B�33B���B�ffB���C� C�C
�fCffC� C�fC� C$��C)�3C.� C3L�C8� C=� CB33CG33CQL�C[ffCe� Co� Cy��C��fC��fC�� C��fC��fC�� C���C�� C�s3C���C���C��3C�� C�� CǙ�C̳3Cь�Cֳ3C۳3C���C噚C�3C�� C��3C��fDٚD�fD� DٚD��D�D"L�D(� D.� D5  D;` DA� DG��DNfDTFfDZ��D`ٚDg  DmL�Dsy�Dy�3D�)�D�i�D���D��D�0 D�` D�� D��3D�  D�i�D���D���D�)�D�\�Dک�D��D��D�i�D�fD��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�33A��Aa��A�  A�  A�33B	33B��B0  BD��BX  Bk33B��B�ffB�ffB���B�ffB���B���B�  B�33B�33B���B�ffB���C� C�C
�fCffC� C�fC� C$��C)�3C.� C3L�C8� C=� CB33CG33CQL�C[ffCe� Co� Cy��C��fC��fC�� C��fC��fC�� C���C�� C�s3C���C���C��3C�� C�� CǙ�C̳3Cь�Cֳ3C۳3C���C噚C�3C�� C��3C��fDٚD�fD� DٚD��D�D"L�D(� D.� D5  D;` DA� DG��DNfDTFfDZ��D`ٚDg  DmL�Dsy�Dy�3D�)�D�i�D���D��D�0 D�` D�� D��3D�  D�i�D���D���D�)�D�\�Dک�D��D��D�i�D�fD��3222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�{A��A��A��A��A��A�"�A�"�A�(�A�-A�-A�/A�1'A�1'A�33A�$�A��+A�A��HA�l�A��;A�Q�A��A��/A��Av�\Al1Af�jA]|�AW\)AU�7AQ�AN�AC\)A=VA;G�A9�mA:�!A6jA4�jA3"�A0�A/�FA.bNA-VA+O�A*1A(ffA'�;A&ȴA%C�A$M�A#�A!�A!G�A�^AM�AbNAr�A��A=qAK�Ar�A"�At�A��A
�\A�;A�A��A�@�ff@�1'@�S�@�9X@�Q�@ҸR@�E�@���@��@�5?@�z�@���@��^@��h@�M�@���@�/@�dZ@�o@���@�n�@��+@�@�A�@tI�@m�T@d�@[t�@S�F@K@C33@:�@4�@+33@#33@��@�@��@�P@`B111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�{A��A��A��A��A��A�"�A�"�A�(�A�-A�-A�/A�1'A�1'A�33A�$�A��+A�A��HA�l�A��;A�Q�A��A��/A��Av�\Al1Af�jA]|�AW\)AU�7AQ�AN�AC\)A=VA;G�A9�mA:�!A6jA4�jA3"�A0�A/�FA.bNA-VA+O�A*1A(ffA'�;A&ȴA%C�A$M�A#�A!�A!G�A�^AM�AbNAr�A��A=qAK�Ar�A"�At�A��A
�\A�;A�A��A�@�ff@�1'@�S�@�9X@�Q�@ҸR@�E�@���@��@�5?@�z�@���@��^@��h@�M�@���@�/@�dZ@�o@���@�n�@��+@�@�A�@tI�@m�T@d�@[t�@S�F@K@C33@:�@4�@+33@#33@��@�@��@�P@`B222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	�RB	�RB	�RB	�XB	�RB	�RB	�RB	�RB	�FB	�^B	�^B	�^B	�^B	�dB	�^B	�XB{B'�B&�BA�B
��B
��B
:^B	�B	��B	�B	�DB	u�B	dZB	hsB	jB	o�B	v�B	�+B	�^B	�B	��B
?}B
;dB
cTB
�uB
��B
��B
��B
��B
ȴB
ƨB
ÖB
��B
�wB
�jB
�LB
�FB
�-B
�!B
�B
��B
��B
��B
��B
��B
�VB
�B
~�B
t�B
iyB
_;B
W
B
Q�B
M�B
H�B
B�B
7LB
'�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
!�B
#�B
$�B
'�B
+B
+B
.B
1'B
5?B
:^B
?}B
D�B
J�B
M�B
Q�B
VB
[#B
`BB
e`B
iyB
m�B
s�B
y�B
}�B
� B
�B
�1B
�J111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	�RB	�RB	�RB	�XB	�RB	�RB	�RB	�RB	�FB	�^B	�^B	�^B	�^B	�dB	�dB	��B�B-B1'BG�BB
�?B
B�B	��B	��B	�'B	�VB	y�B	gmB	iyB	l�B	q�B	{�B	�=B	�dB	�B	��B
A�B
<jB
cTB
�{B
��B
��B
��B
��B
ȴB
ƨB
ÖB
��B
�wB
�jB
�LB
�FB
�-B
�!B
�B
��B
��B
��B
��B
��B
�\B
�B
~�B
u�B
jB
`BB
W
B
Q�B
M�B
I�B
C�B
8RB
(�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
!�B
#�B
$�B
'�B
+B
+B
.B
1'B
5?B
:^B
?}B
D�B
J�B
M�B
Q�B
VB
[#B
`BB
e`B
iyB
m�B
s�B
y�B
}�B
� B
�B
�1B
�J222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<�j<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ = PRES-SP(NextCycle), where SP is SURFACE PRESSURE from next cycle                                                                                                                                                                                     TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,PRES_ADJ,elapsed_time,alpha,tau),elapsed_time=P/mean_rise_rate,P=dbar since the start of the profile for each samples                                                                                                       None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            SP(NextCycle)=0.0(dbar)                                                                                                                                                                                                                                         None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                      None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE; PRES_ADJ_ERR : Manufacture sensor accuracy                                                                                                                                                                 TEMP_ADJ_ERR : SBE sensor accuracy                                                                                                                                                                                                                              Salinity Recalculation using PRES_ADJ; PSAL_ADJ_ERR : max(sum of RecalS & CTM errors, 0.01(PSS-78))                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            TNPD: APEX float that truncated negative pressure drift                                                                                                                                                                                                         None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         200505030000002005050300000020050503000000200701262003342007012620033420070126200334200506090000002005060900000020050609000000  JA  ARFMfmtp2.2                                                                 20050422064735  IP                  G�O�G�O�G�O�                JA  ARGQrqcp2.3                                                                 20050422064735  QCP$                G�O�G�O�G�O�           1FB7CJA  ARUP                                                                        20050422065417                      G�O�G�O�G�O�                JA  ARFMfmtp2.2                                                                 20050425224939  IP                  G�O�G�O�G�O�                JA  ARGQrqcp2.3                                                                 20050425224940  QCP$                G�O�G�O�G�O�           1FB7CJA  ARUP                                                                        20050425225733                      G�O�G�O�G�O�                JM  ARCAJMQC                                                                    20050503000000  IP  PRES            G�O�G�O�G�O�                JM  ARCAJMQC                                                                    20050503000000  IP  PSAL            G�O�G�O�G�O�                JM  ARSQWJO 1   SeHyD1                                                          20050609000000  IP  PSAL            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20060310063414  IP                  G�O�G�O�G�O�                JA  RFMTcnv22.0                                                                 20060414072447  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20060417034754                      G�O�G�O�G�O�                JM  ARGQREJM1.0                                                                 20110527053308  CV  JULD            G�O�G�O�F���                JM  ARCAJMTM1.0                                                                 20070126200334  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  AREQREJM1.0                                                                 20110527053308  CF  PRES_ADJUSTED_QC@�33D��3G�O�                JM  AREQREJM1.0                                                                 20110527053308  CF  TEMP_ADJUSTED_QC@�33D��3G�O�                JM  AREQREJM1.0                                                                 20110527053308  CF  PSAL_ADJUSTED_QC@�33D��3G�O�                JA  RFMTcnvd2.1                                                                 20110606001843  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20110606002032                      G�O�G�O�G�O�                JM      JMFC1.0                                                                 20150609092748                      G�O�G�O�G�O�                JA  ARDU                                                                        20150621114518                      G�O�G�O�G�O�                