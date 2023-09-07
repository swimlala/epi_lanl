CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PARAM       N_PROF        N_CALIB       N_LEVELS   I   	N_HISTORY             	   title         Argo float vertical profile    institution       JAMSTEC    source        
Argo float     history       b2006-08-12T02:48:46Z creation;2009-03-18T05:17:36Z update;2015-06-08T14:05:05Z conversion to V3.1;     
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
resolution        =���   standard_name         sea_water_pressure     axis      Z        $  9�   PRES_QC       	            	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  L  :�   PRES_ADJUSTED         	         	   	long_name         )Sea water pressure, equals 0 at sea-level      
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   standard_name         sea_water_pressure       $  ;   PRES_ADJUSTED_QC      	            	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  L  <4   PRES_ADJUSTED_ERROR       	            	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     $  <�   TEMP      	         	   	long_name         $Sea temperature in-situ ITS-90 scale   
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_temperature        $  =�   TEMP_QC       	            	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  L  >�   TEMP_ADJUSTED         	         	   	long_name         $Sea temperature in-situ ITS-90 scale   
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_temperature        $  ?   TEMP_ADJUSTED_QC      	            	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  L  @8   TEMP_ADJUSTED_ERROR       	            	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     $  @�   PSAL      	         	   	long_name         Practical salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_salinity       $  A�   PSAL_QC       	            	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  L  B�   PSAL_ADJUSTED         	         	   	long_name         Practical salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_salinity       $  C   PSAL_ADJUSTED_QC      	            	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  L  D<   PSAL_ADJUSTED_ERROR       	            	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     $  D�   	PARAMETER         	   
               	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  E�   SCIENTIFIC_CALIB_EQUATION         	   
               	long_name         'Calibration equation for this parameter    
_FillValue                 	   F<   SCIENTIFIC_CALIB_COEFFICIENT      	   
               	long_name         *Calibration coefficients for this equation     
_FillValue                 	   O<   SCIENTIFIC_CALIB_COMMENT      	   
               	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   X<   SCIENTIFIC_CALIB_DATE         	   
                	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  a<   HISTORY_INSTITUTION          	            	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    a�   HISTORY_STEP         	            	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    a�   HISTORY_SOFTWARE         	            	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    a�   HISTORY_SOFTWARE_RELEASE         	            	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    a�   HISTORY_REFERENCE            	            	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  a�   HISTORY_DATE         	             	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    b   HISTORY_ACTION           	            	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    b   HISTORY_PARAMETER            	            	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    b    HISTORY_START_PRES           	         	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         b0   HISTORY_STOP_PRES            	         	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         b4   HISTORY_PREVIOUS_VALUE           	         	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        b8   HISTORY_QCTEST           	            	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    b<Argo profile    3.1 1.2 19500101000000  5900291 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  20060812024846  20150617054514  A4_13305_129                    2C  D   APEX                            692                             072602                          846 @�1;��F1   @�1<���z@- �n���c0(�\1   ARGOS   A   A   B   Primary sampling: discrete [spot sampling CTD data]                                                                                                                                                                                                                A33A  A���A�ffB  BE33Bl  B�  B���B���B���Bڙ�B���CffC��C�3C� C)�C3� C=�CGffC[ffCo33C���C��fC��fC��3C���C�� C��fCǳ3C�� Cۀ C� C�s3C���D�3D��D�3D��D�3D��D�3D$�fD)�3D.��D3ٚD8�fD=�fDB��DGٚDN  DT@ DZ�3D`��Dg  DmY�Ds�3DyٚD�,�D�ffD���D��D�0 D�s3D���D�ffD��D�i�D���D�` D��D���1111111111111111111111111111111111111111111111111111111111111111111111111   @�  @���Ap  A���B33B4ffB[33B���B�33B�ffB�ffB�33B�ffB�ffCffC� CL�C$�fC/L�C8�fCC33CW33Ck  CffC���C���C���C�s3C��fC���Cř�CϦfC�ffC�ffC�Y�C��3D �fD� D
�fD� D�fD� D�fD#ٚD(�fD-� D2��D7��D<��DA� DF��DM3DS33DY�fD_� De�3DlL�Dr�fDx��D��fD�� D�#3D�c3D���D���D�ffD�� D�c3D��3D�ffD�ٚD�c3D�6f1111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A١�A١�AٓuAُ\A�n�A�l�A�n�A�XA�I�A�v�A��jA���A��jA�&�A���A�5?A��mA��TA�bNA��Aq��AX$�AG�#A6bNA&A�A�9AM�A��A9XA
5?A�uA\)@�=q@�S�@�hs@�v�@ܬ@�Q�@�?}@ҟ�@�z�@ƸR@�Q�@��j@���@�9X@���@�dZ@�ƨ@�r�@�V@��@��`@�n�@��
@��@��@��@�C�@���@��@}�@r�\@g��@_��@N�@@�9@/�@$��@o@��@(�@�
1111111111111111111111111111111111111111111111111111111111111111111111141   A١�A١�AٓuAُ\A�n�A�l�A�n�A�XA�I�A�v�A��jA���A��jA�&�A���A�5?A��mA��TA�bNA��Aq��AX$�AG�#A6bNA&A�A�9AM�A��A9XA
5?A�uA\)@�=q@�S�@�hs@�v�@ܬ@�Q�@�?}@ҟ�@�z�@ƸR@�Q�@��j@���@�9X@���@�dZ@�ƨ@�r�@�V@��@��`@�n�@��
@��@��@��@�C�@���@��@}�@r�\@g��@_��@N�@@�9@/�@$��@o@��@(�@�
1111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
�)B
�)B
�/B
�/B
�)B
�)B
�)B
�B
�oB	��B
dZB
�NB��BDB1'B�By�B1'B
�DB
�B	�B	%BĜB��B�B	33B	�B	�B	��B	��B	�LB	B	�/B	�B	�RB	�XB	�dB	�;B	�B	�HB	�B	�
B	�
B	�/B	�NB	�B	��B	��B
  B
B
+B

=B
bB
�B
�B
 �B
�B
�B
�B
&�B
1'B
49B
=qB
D�B
K�B
P�B
[#B
e`B
o�B
w�B
�     B
�11111111111111111111111111111111111111111111111111111111111111111111111441   B
�/B
�/B
�5B
�5B
�/B
�/B
�/B
�#B
��B	��B
hsB
�TB��BVB5?B�B}�B7LB
�oB
&�B	�%B	
=BȴB��B�#B	6FB	�B	�B	��B	��B	�XB	ĜB	�;B	��B	�dB	�dB	�qB	�HB	�B	�TB	�#B	�B	�B	�;B	�ZB	��B	��B	��B
B
B
	7B
JB
oB
�B
�B
"�B
�B
�B
�B
(�B
33B
6FB
?}B
F�B
M�B
R�B
]/B
gmB
q�B
y�B
�G�O�B
�=1111111111111111111111111111111111111111111111111111111111111111111111141   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ = PRES-SP(NextCycle), where SP is SURFACE PRESSURE from next cycle                                                                                                                                                                                     none                                                                                                                                                                                                                                                            PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = celltm_sbe41(RecalS,TEMP,PRES_ADJUSTED,elapsed_time,alpha,tau),elapsed_time=P/mean_rise_rate,P= dbar since the start of the profile for each samples.                                                                                           none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            SP(NextCycle) = 4.2 dbar                                                                                                                                                                                                                                        none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE; PRES_ADJ_ERR : Manufacture sensor accuracy                                                                                                                                                                 TEMP_ADJ_ERR : SBE sensor accuracy                                                                                                                                                                                                                              Salinity Recalculation using PRES_ADJ; PSAL_ADJ_ERR : max(sum of RecalS & CTM errors, 0.01(PSS-78))                                                                                                                                                             none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         200608311858002006083118580020060831185800200701260458462007012604584620070126045846200710110000002007101100000020071011000000  JA  ARFMfmtp2.3                                                                 20060812024846  IP                  G�O�G�O�G�O�                JA  ARCAsspa2.1                                                                 20060812024846  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcp2.5                                                                 20060812024847  QCP$                G�O�G�O�G�O�           1FB7CJA  ARGQrqcp2.5                                                                 20060812024847  QCF$                G�O�G�O�G�O�            4800JA  ARGQaqcp2.5                                                                 20060812024847  QCP$                G�O�G�O�G�O�           1FB40JA  ARGQaqcp2.5                                                                 20060812024847  QCF$                G�O�G�O�G�O�            4800JA  ARUP                                                                        20060812025920                      G�O�G�O�G�O�                JA  ARFMfmtp2.3                                                                 20060816034039  IP                  G�O�G�O�G�O�                JA  ARCAsspa2.1                                                                 20060816034039  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcp2.5                                                                 20060816034040  QCP$                G�O�G�O�G�O�           1FB7CJA  ARGQrqcp2.5                                                                 20060816034040  QCF$                G�O�G�O�G�O�            4800JA  ARGQaqcp2.5                                                                 20060816034040  QCP$                G�O�G�O�G�O�           1FB40JA  ARGQaqcp2.5                                                                 20060816034040  QCF$                G�O�G�O�G�O�            4800JA  ARUP                                                                        20060816035729                      G�O�G�O�G�O�                JM  ARSQJMQC1.0                                                                 20060831155040  CF  TEMP            D��D��G�O�                JM  ARSQJMQC1.0                                                                 20060831155040  CF  PSAL            D�` D�` G�O�                JM  ARCAJMQC1.0                                                                 20060831185800  CV  PRES            G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20060831185800  CV  PSAL            G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20070126045846  CV  PSAL            G�O�G�O�G�O�                JM  ARSQWJO 2.0 SeHyD1.0                                                        20071011000000  IP  PSAL            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20071015075821  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20071015092553                      G�O�G�O�G�O�                JM  AREVjmrvp1.2                                                                20090312113710  IP  PRES            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20090318051024  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20090318051736                      G�O�G�O�G�O�                JM      JMFC1.0                                                                 20150608140500                      G�O�G�O�G�O�                JA  ARDU                                                                        20150617054514                      G�O�G�O�G�O�                