CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PARAM       N_PROF        N_CALIB       N_LEVELS   s   	N_HISTORY             	   title         Argo float vertical profile    institution       JAMSTEC    source        
Argo float     history       b2005-05-10T01:05:50Z creation;2009-03-18T07:19:45Z update;2015-06-09T19:32:34Z conversion to V3.1;     
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
_FillValue                  `  L�   SCIENTIFIC_CALIB_EQUATION         	   
               	long_name         'Calibration equation for this parameter    
_FillValue                    L�   SCIENTIFIC_CALIB_COEFFICIENT      	   
               	long_name         *Calibration coefficients for this equation     
_FillValue                    R�   SCIENTIFIC_CALIB_COMMENT      	   
               	long_name         .Comment applying to this parameter calibration     
_FillValue                    X�   SCIENTIFIC_CALIB_DATE         	   
                	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T  ^�   HISTORY_INSTITUTION          	            	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    _8   HISTORY_STEP         	            	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    _<   HISTORY_SOFTWARE         	            	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    _@   HISTORY_SOFTWARE_RELEASE         	            	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    _D   HISTORY_REFERENCE            	            	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  _H   HISTORY_DATE         	             	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    _�   HISTORY_ACTION           	            	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    _�   HISTORY_PARAMETER            	            	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    _�   HISTORY_START_PRES           	         	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         _�   HISTORY_STOP_PRES            	         	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         _�   HISTORY_PREVIOUS_VALUE           	         	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        _�   HISTORY_QCTEST           	            	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    _�Argo profile    3.1 1.2 19500101000000  5900649 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               A   JA  20050510010550  20150621172516  A5_23579_024                    2C  D   APEX                            1556                            013004                          846 @Ӿ[(d�1   @Ӿ_���b@5�E���c7��v�1   ARGOS   A   A   A   Primary sampling: discrete [spot sampling CTD data]                                                                                                                                                                                                                @�33A��Ac33A���A���A�  B��BffB0ffBC��BX��Bn  B���B���B�  B���B���B���B�33B�  B�33B���B�  B�ffB���CL�C� C� C�CffC�C� C$�C)ffC.� C333C8�C=�CBffCG�CQffC[33CeffCo� Cy  C��fC��fC�s3C��3C��3C��fC��fC��3C��fC���C���C���C�� C�C�ffC̳3CѦfC�� Cۙ�C���C噚C� C�fC��fC�ffDٚD�3DٚD��D� D�fD�3D$�3D)�3D.�3D3� D8� D=�3DB� DG� DL� DQ��DV�fD[� D`� De��Dj� DoٚDt� Dy�fD�,�D�p D�� D�ٚD�,�D�\�D���D���D�)�D�s3D���D���D�)�D�p Dک�D�ٚD�  D�Y�D�fD�f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�  A33Aa��A�  A�  A�33BffB  B0  BC33BXffBm��B�ffB���B���B�ffB�ffB���B�  B���B�  Bڙ�B���B�33B���C33CffCffC  CL�C  CffC$  C)L�C.ffC3�C8  C=  CBL�CG  CQL�C[�CeL�CoffCx�fC���C���C�ffC��fC��fC���C���C��fC���C���C�� C���C��3C�C�Y�C̦fCљ�Cֳ3Cی�C�� C��C�s3CC���C�Y�D�3D��D�3D�3D��D� D��D$��D)��D.��D3��D8��D=��DB��DGٚDLٚDQ�fDV� D[ٚD`ٚDe�fDjٚDo�3Dt��Dy� D�)�D�l�D���D��fD�)�D�Y�D���D�ٚD�&fD�p D���D��D�&fD�l�DڦfD��fD��D�VfD�3D�31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A��A���A���A��A��/Aç�A�l�A�p�A���A��#A��A�I�A��A���A���A�hsA�l�A�S�A�n�A�VA��#A�/A���A��A�v�A��A�&�A��yA�bNA� �A�^5A�p�A���A���A��A�C�A��A�bA��!A���A��`A�ZA}"�At-Af��AY�;AK"�AAK�A<bA4��A1+A.�A)��A#��A��A�wA��A�A
=qA/A1@�S�@�-@�^5@�ƨ@�x�@��@�1'@�K�@�Z@��^@��`@��j@���@���@�@��@�&�@�ƨ@�I�@�@���@���@��\@�1'@�$�@�j@��@��`@�K�@���@�I�@�ȴ@�A�@���@y&�@qX@ix�@]�@Y%@S@M?}@C��@=p�@4�/@,�j@(r�@#��@��@-@E�@x�@$�@
��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��A��A���A���A��A��/Aç�A�l�A�p�A���A��#A��A�I�A��A���A���A�hsA�l�A�S�A�n�A�VA��#A�/A���A��A�v�A��A�&�A��yA�bNA� �A�^5A�p�A���A���A��A�C�A��A�bA��!A���A��`A�ZA}"�At-Af��AY�;AK"�AAK�A<bA4��A1+A.�A)��A#��A��A�wA��A�A
=qA/A1@�S�@�-@�^5@�ƨ@�x�@��@�1'@�K�@�Z@��^@��`@��j@���@���@�@��@�&�@�ƨ@�I�@�@���@���@��\@�1'@�$�@�j@��@��`@�K�@���@�I�@�ȴ@�A�@���@y&�@qX@ix�@]�@Y%@S@M?}@C��@=p�@4�/@,�j@(r�@#��@��@-@E�@x�@$�@
��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B�B�B�B�B�B��B��B�jB1BuB{B{B{BuB;dBcTB�\B��B�?BĜBɺB��B��B��B�B��B��B�RB�B��B�Bm�BL�BbB��B�HB��B��Br�B{B
ȴB
cTB	�fB	��B	7LB��B�B_;B}�BaHB�PB�DB� Bx�BXBP�BP�BW
BW
BW
BS�BL�BM�BD�BN�BL�BQ�BT�BS�Bq�B�oB�qB�#B��B	�B	9XB	W
B	ffB	x�B	�hB	��B	�B	�dB	�}B	��B	��B	�B	�#B	�NB	�fB	�B	�B	�B	��B
B
hB
�B
!�B
,B
0!B
6FB
:^B
B�B
G�B
N�B
VB
ZB
_;B
dZB
iyB
m�B
s�B
v�B
z�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�B�B�B�B�B�B��B��B�jB1BuB{B{B{BuB;dBcTB�\B��B�?BĜBɺB��B��B��B�B��B��B�RB�B��B�Bm�BL�BbB��B�HB��B��Br�B{B
ȴB
cTB	�fB	��B	7LB��B�B_;B}�BaHB�PB�DB� Bx�BXBP�BP�BW
BW
BW
BS�BL�BM�BD�BN�BL�BQ�BT�BS�Bq�B�oB�qB�#B��B	�B	9XB	W
B	ffB	x�B	�hB	��B	�B	�dB	�}B	��B	��B	�B	�#B	�NB	�fB	�B	�B	�B	��B
B
hB
�B
!�B
,B
0!B
6FB
:^B
B�B
G�B
N�B
VB
ZB
_;B
dZB
iyB
m�B
s�B
v�B
z�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - SP, where SP is SURFACE PRESSURE (minus 5 dbar for Apf-6,7,8) from next cycle.                                                                                                                                                           none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL(PRES_ADJUSTED,TEMP,Conductivity)                                                                                                                                                                                                           none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            SP(NextCycle) = 0.1 dbar                                                                                                                                                                                                                                        none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using reported SURFACE PRESSURE. The quoted error is max [2.4, size of pressure adjustment] in dbar.                                                                                                                                      The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Salinity Recalculation using ADJUSTED Pressure                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         200505220000002005052200000020050522000000200506060000002005060600000020050606000000JA  ARFMfmtp2.2                                                                 20050510010550  IP                  G�O�G�O�G�O�                JA  ARGQrqcp2.3                                                                 20050510010550  QCP$                G�O�G�O�G�O�           1FB7CJA  ARUP                                                                        20050510025706                      G�O�G�O�G�O�                JA  ARFMfmtp2.2                                                                 20050513105048  IP                  G�O�G�O�G�O�                JA  ARGQrqcp2.3                                                                 20050513105049  QCP$                G�O�G�O�G�O�           1FB7CJA  ARUP                                                                        20050513105734                      G�O�G�O�G�O�                JM  ARCAJMQC                                                                    20050522000000  IP  PRES            G�O�G�O�G�O�                JM  ARCAJMQC                                                                    20050522000000  IP  PSAL            G�O�G�O�G�O�                JM  ARSQWJO 1   SeHyD1                                                          20050606000000  IP  PSAL            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20060908013357  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20060908013628                      G�O�G�O�G�O�                JM  AREVjmrvp1.2                                                                20090312120556  IP  PRES            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20090318071823  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20090318071945                      G�O�G�O�G�O�                JM      JMFC1.0                                                                 20150609193229                      G�O�G�O�G�O�                JA  ARDU                                                                        20150621172516                      G�O�G�O�G�O�                