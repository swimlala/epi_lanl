CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PARAM       N_PROF        N_CALIB       N_LEVELS   s   	N_HISTORY             	   title         Argo float vertical profile    institution       JAMSTEC    source        
Argo float     history       b2006-05-25T06:50:50Z creation;2009-03-18T07:32:11Z update;2015-06-09T20:05:19Z conversion to V3.1;     
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
_FillValue                  t  ;l   PRES_ADJUSTED         	         	   	long_name         )Sea water pressure, equals 0 at sea-level      
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   standard_name         sea_water_pressure       �  ;�   PRES_ADJUSTED_QC      	            	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  =�   PRES_ADJUSTED_ERROR       	            	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  >    TEMP      	         	   	long_name         $Sea temperature in-situ ITS-90 scale   
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_temperature        �  ?�   TEMP_QC       	            	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  A�   TEMP_ADJUSTED         	         	   	long_name         $Sea temperature in-situ ITS-90 scale   
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_temperature        �  B,   TEMP_ADJUSTED_QC      	            	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  C�   TEMP_ADJUSTED_ERROR       	            	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  Dl   PSAL      	         	   	long_name         Practical salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_salinity       �  F8   PSAL_QC       	            	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  H   PSAL_ADJUSTED         	         	   	long_name         Practical salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_salinity       �  Hx   PSAL_ADJUSTED_QC      	            	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  JD   PSAL_ADJUSTED_ERROR       	            	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  J�   	PARAMETER         	   
               	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  L�   SCIENTIFIC_CALIB_EQUATION         	   
               	long_name         'Calibration equation for this parameter    
_FillValue                 	   M   SCIENTIFIC_CALIB_COEFFICIENT      	   
               	long_name         *Calibration coefficients for this equation     
_FillValue                 	   V   SCIENTIFIC_CALIB_COMMENT      	   
               	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   _   SCIENTIFIC_CALIB_DATE         	   
                	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  h   HISTORY_INSTITUTION          	            	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    h�   HISTORY_STEP         	            	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    h�   HISTORY_SOFTWARE         	            	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    h�   HISTORY_SOFTWARE_RELEASE         	            	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    h�   HISTORY_REFERENCE            	            	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  h�   HISTORY_DATE         	             	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    h�   HISTORY_ACTION           	            	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    h�   HISTORY_PARAMETER            	            	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    h�   HISTORY_START_PRES           	         	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         i   HISTORY_STOP_PRES            	         	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         i   HISTORY_PREVIOUS_VALUE           	         	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        i   HISTORY_QCTEST           	            	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    iArgo profile    3.1 1.2 19500101000000  5900650 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               >A   JA  20060525065050  20150614054516  A5_23632_062                    2C  D   APEX                            1557                            013004                          846 @���t��1   @��f���@4�Q���cn�x���1   ARGOS   A   A   A   Primary sampling: discrete [spot sampling CTD data]                                                                                                                                                                                                                @�33A  Ad��A�33A���A�  B	��B��B2  BF  BY��BlffB���B���B�ffB�  B�33B�33B���B�  BЙ�B�  B���B�  B���C33C33CL�C33CL�C��C33C$  C)� C.� C3� C833C=33CB�CGffCQ33CZ��Ce33Co� CyL�C���C��3C��fC���C��fC��fC�ٚC��fC���C���C�� C��3C��fC³3Cǳ3C���Cљ�C֦fC۳3C���C�� C� CC� C�� DٚD��D� DٚD� D� D� D$ٚD)��D.�3D3ٚD8� D=�3DB� DG�3DL� DQ��DV��D[�fD`� De� DjٚDo� Dt��Dy�3D��D�` D���D��D�&fD�ffD���D���D�)�D�i�D�� D��3D�  D�p DڦfD�� D�  D�Y�D�D�&f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�  AffAc33A�ffA���A�33B	33BffB1��BE��BY33Bl  B���B�ffB�33B���B�  B�  B���B���B�ffB���B䙚B���B�ffC�C�C33C�C33C�3C�C#�fC)ffC.ffC3ffC8�C=�CB  CGL�CQ�CZ�3Ce�CoffCy33C���C��fC���C�� C���C���C���C���C���C�� C�s3C��fC���C¦fCǦfC�� Cь�C֙�CۦfC�� C�3C�s3C��C�s3C��3D�3D�3D��D�3D��D��DٚD$�3D)�fD.��D3�3D8��D=��DB��DG��DL��DQ�fDV�fD[� D`ٚDeٚDj�3Do��Dt�3Dy��D�fD�\�D���D��fD�#3D�c3D��fD��D�&fD�ffD���D�� D��D�l�Dڣ3D���D��D�VfD�fD�#31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�z�A��jA���A��DA��A�~�A�v�A�l�A�`BA�7LA�\)A��uA�%A���A��\A�;dA��/A���A�jA���A�-A�l�A���A�Q�A�~�A��A�O�A��9A�oA�p�A�+A�p�A�|�A��HA��!A���A��A�
=A�ffA�S�A���A�ĜA�C�A�1'A���A{K�Aq\)Ael�A\z�AS�AJz�A>v�A8��A1��A+A*ĜA$��AK�AȴA-A��AoA�!A%A�AG�@��
@�S�@�"�@�K�@�V@؃@�?}@�5?@��@�@�/@���@�^5@�l�@��@�G�@�C�@���@�O�@�b@��@�@�z�@�O�@�\)@��@��F@�-@�z�@;d@y�7@pA�@e`B@a%@Z~�@S@M��@HQ�@B�\@8A�@3��@.v�@%��@��@t�@�y@C�@�P@	�71111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�z�A��jA���A��DA��A�~�A�v�A�l�A�`BA�7LA�\)A��uA�%A���A��\A�;dA��/A���A�jA���A�-A�l�A���A�Q�A�~�A��A�O�A��9A�oA�p�A�+A�p�A�|�A��HA��!A���A��A�
=A�ffA�S�A���A�ĜA�C�A�1'A���A{K�Aq\)Ael�A\z�AS�AJz�A>v�A8��A1��A+A*ĜA$��AK�AȴA-A��AoA�!A%A�AG�@��
@�S�@�"�@�K�@�V@؃@�?}@�5?@��@�@�/@���@�^5@�l�@��@�G�@�C�@���@�O�@�b@��@�@�z�@�O�@�\)@��@��F@�-@�z�@;d@y�7@pA�@e`B@a%@Z~�@S@M��@HQ�@B�\@8A�@3��@.v�@%��@��@t�@�y@C�@�P@	�71111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��BbB�B�B+B2-BH�BaHB�^B�B?}By�Bm�B�hB��B�?B��B��B�B�B�qB�bBB�VBdZBW
B/B�B�B��B�!Br�B@�B
�NB
bNB
"�B	�B	�3B	hsB	,B�B�RB�\B�Bu�B�1B�mB�B�LB�{B�hB��B�5B��B�}B�jB�qB�FB�B�hB�bB��B�dB��B�{B��B�^B�BB	  B	(�B	K�B	aHB	v�B	�PB	��B	�RB	�}B	��B	�
B	�5B	�B	�B	�B	��B	��B
B
PB
uB
�B
(�B
-B
5?B
;dB
A�B
E�B
H�B
R�B
W
B
YB
cTB
hsB
k�B
n�B
q�B
v�B
{�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��BhB�B�B+B2-BH�BbNB�dB�B@�Bz�Bn�B�hB��B�FB��B��B�B�#B�}B�bBŢB�\Be`BZB1'B�B�B��B�-Bt�BB�B
�`B
dZB
$�B	�B	�FB	jB	.B�B�dB�hB�%Bw�B�1B�yB�B�XB��B�oB��B�;B��B��B�qB�}B�LB�B�oB�hB�B�jB��B��B��B�^B�BB	  B	(�B	K�B	aHB	v�B	�PB	��B	�RB	�}B	��B	�
B	�5B	�B	�B	�B	��B	��B
B
PB
uB
�B
(�B
-B
5?B
;dB
A�B
E�B
H�B
R�B
W
B
YB
cTB
hsB
k�B
n�B
q�B
v�B
{�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - SP, where SP is SURFACE PRESSURE (minus 5 dbar for Apf-6,7,8) from next cycle.                                                                                                                                                           none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = RecalS = PSAL(PRES_ADJUSTED,TEMP,Conductivity)                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = celltm_sbe41(RecalS,TEMP,PRES_ADJUSTED,elapsed_time,alpha,tau),elapsed_time=P/mean_rise_rate,P= dbar since the start of the profile for each samples.                                                                                           none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            SP(NextCycle) = 0.1 dbar                                                                                                                                                                                                                                        none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using reported SURFACE PRESSURE. The quoted error is max [2.4, size of pressure adjustment] in dbar.                                                                                                                                      The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Salinity Recalculation using PRES_ADJ; PSAL_ADJ_ERR : max(sum of RecalS & CTM errors, 0.01(PSS-78))                                                                                                                                                             none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         200706281404322007062814043220070628140432200706290309352007062903093520070629030935200707020000002007070200000020070702000000  JA  ARFMfmtp2.2                                                                 20060525065050  IP                  G�O�G�O�G�O�                JA  ARCAsspa2.0                                                                 20060525065051  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcp2.4                                                                 20060525065051  QCP$                G�O�G�O�G�O�           3F6BCJA  ARUP                                                                        20060525065902                      G�O�G�O�G�O�                JA  ARFMfmtp2.2                                                                 20060529034459  IP                  G�O�G�O�G�O�                JA  ARCAsspa2.0                                                                 20060529034500  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcp2.4                                                                 20060529034500  QCP$                G�O�G�O�G�O�           3F6BCJA  ARUP                                                                        20060529035353                      G�O�G�O�G�O�                JA  ARUP                                                                        20060602051137                      G�O�G�O�G�O�                JA  ARGQrqcp2.5                                                                 20060627031715  QCP$                G�O�G�O�G�O�           1FB7CJA  ARGQaqcp2.5                                                                 20060627031715  QCP$                G�O�G�O�G�O�           1FB40JA  ARUP                                                                        20060627035412                      G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20070628140432  CV  PRES            G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20070628140432  CV  PSAL            G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20070629030935  CV  PSAL            G�O�G�O�G�O�                JM  ARSQWJO 2.0 SeHyD1.0                                                        20070702000000  IP  PSAL            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20071001072804  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20071001091734                      G�O�G�O�G�O�                JM  AREVjmrvp1.2                                                                20090312120642  IP  PRES            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20090318072726  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20090318073211                      G�O�G�O�G�O�                JM      JMFC1.0                                                                 20150609200508                      G�O�G�O�G�O�                JA  ARDU                                                                        20150614054516                      G�O�G�O�G�O�                