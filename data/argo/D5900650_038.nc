CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PARAM       N_PROF        N_CALIB       N_LEVELS   s   	N_HISTORY             	   title         Argo float vertical profile    institution       JAMSTEC    source        
Argo float     history       b2005-09-27T06:50:22Z creation;2009-03-18T07:30:17Z update;2015-06-09T20:00:38Z conversion to V3.1;     
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
_FillValue                    _�Argo profile    3.1 1.2 19500101000000  5900650 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               &A   JA  20050927065022  20150614052517  A5_23632_038                    2C  D   APEX                            1557                            013004                          846 @���^Б1   @�� <�x@4�`A�7L�cPA�7K�1   ARGOS   A   A   A   Primary sampling: discrete [spot sampling CTD data]                                                                                                                                                                                                                @�ffA  Ah  A�33A���A�  BffBffB2  BD��BXffBlffB���B�33B�33B�ffB�33B�  B�33B�ffB�33B�ffB�ffBB���CL�C  C  C33CL�CL�C� C$� C)33C.��C3� C8ffC=L�CB� CG  CQffC[ffCeL�Co� Cy  C���C�� C�� C���C�� C���C��3C�� C���C��3C���C���C���C�Y�Cǀ C̙�CѦfC֦fC�� C���C�� CꙚCC��C�s3D�3D�3DٚD� D�fDٚD�3D$�3D)� D.ٚD3ٚD8� D=�3DB��DG� DL� DQ��DV��D[�3D`�fDeٚDj�3Do�3Dt��Dy�3D�#3D�i�D��fD�� D��D�p D���D���D�0 D�i�D��3D��D�)�D�ffDک�D��3D��D�` D�3D�f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�33AffAfffA�ffA�  A�33B  B  B1��BDffBX  Bl  B���B�  B�  B�33B�  B���B�  B�33B�  B�33B�33B�ffB�ffC33C�fC
�fC�C33C33CffC$ffC)�C.� C3ffC8L�C=33CBffCF�fCQL�C[L�Ce33CoffCx�fC���C�s3C�s3C���C��3C�� C��fC�s3C�� C��fC���C�� C���C�L�C�s3Č�Cљ�C֙�C۳3C���C�3C��C��C� C�ffD��D��D�3DٚD� D�3D��D$��D)ٚD.�3D3�3D8ٚD=��DB�fDG��DL��DQ�fDV�fD[��D`� De�3Dj��Do��Dt�3Dy��D�  D�ffD��3D���D��D�l�D��fD�ٚD�,�D�ffD�� D��fD�&fD�c3DڦfD�� D��D�\�D� D�31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A���A�VA�JA�VA���A��mA��/A��TAԣ�A�K�A�Q�A�ƨA�(�A�A��#A��!A�jA�9XA��A��A�/A���A��uA��A�ƨA��A��7A���A�9XA���A�v�A�A��/A��TA���A��yA��+A�$�A��!A��^A?}AwAlVAd �A`=qAYANVA@9XA5t�A0JA+33A&��A"I�A�A�A��A�
Av�A	�^A�-A �@��@���@�@�@�h@���@�~�@թ�@϶F@ț�@���@��9@�z�@��^@��w@��@�x�@�A�@��H@�  @�x�@�;d@���@��@���@��H@���@�Q�@�v�@��D@��#@��;@��@���@yG�@o�P@hbN@`b@V$�@M@Fff@?�P@9%@2^5@+@&{@��@S�@
=@��@��@�m@	7L1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A���A���A�VA�JA�VA���A��mA��/A��TAԣ�A�K�A�Q�A�ƨA�(�A�A��#A��!A�jA�9XA��A��A�/A���A��uA��A�ƨA��A��7A���A�9XA���A�v�A�A��/A��TA���A��yA��+A�$�A��!A��^A?}AwAlVAd �A`=qAYANVA@9XA5t�A0JA+33A&��A"I�A�A�A��A�
Av�A	�^A�-A �@��@���@�@�@�h@���@�~�@թ�@϶F@ț�@���@��9@�z�@��^@��w@��@�x�@�A�@��H@�  @�x�@�;d@���@��@���@��H@���@�Q�@�v�@��D@��#@��;@��@���@yG�@o�P@hbN@`b@V$�@M@Fff@?�P@9%@2^5@+@&{@��@S�@
=@��@��@�m@	7L1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBB�BG�BE�BT�B_;B`BBbNB�B��B�FB��B�B��B�BB!�BZBt�B{�B|�B��B�bB�VB�JB�7B� Bv�Br�B]/BO�B8RB�B�5BĜB��BE�B"�B
�B
�XB
u�B	ÖB	�wB	�B	W
B	&�B	%B�NB�PBcTBaHBs�B�B�7B�oB�oB�1B� B�hB��B��B��B��B�B�!B��B�'B��B��B��B�5B��B��B��B	DB	#�B	9XB	XB	hsB	x�B	�%B	��B	��B	�B	�LB	ɺB	�B	�ZB	�B	�B	��B	��B	��B
B
+B
DB
�B
�B
%�B
+B
2-B
9XB
>wB
E�B
J�B
P�B
W
B
\)B
bNB
gmB
k�B
o�B
s�B
w�B
z�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BB�BG�BE�BT�B_;B`BBbNB�B��B�FB��B�B��B�BB!�BZBt�B{�B|�B��B�bB�VB�JB�7B� Bv�Br�B]/BO�B8RB�B�5BĜB��BE�B"�B
�B
�XB
u�B	ÖB	�wB	�B	W
B	&�B	%B�NB�PBcTBaHBs�B�B�7B�oB�oB�1B� B�hB��B��B��B��B�B�!B��B�'B��B��B��B�5B��B��B��B	DB	#�B	9XB	XB	hsB	x�B	�%B	��B	��B	�B	�LB	ɺB	�B	�ZB	�B	�B	��B	��B	��B
B
+B
DB
�B
�B
%�B
+B
2-B
9XB
>wB
E�B
J�B
P�B
W
B
\)B
bNB
gmB
k�B
o�B
s�B
w�B
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
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - SP, where SP is SURFACE PRESSURE (minus 5 dbar for Apf-6,7,8) from next cycle.                                                                                                                                                           none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL(PRES_ADJUSTED,TEMP,Conductivity)                                                                                                                                                                                                           none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            SP(NextCycle) = 0.1 dbar                                                                                                                                                                                                                                        none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using reported SURFACE PRESSURE. The quoted error is max [2.4, size of pressure adjustment] in dbar.                                                                                                                                      The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Salinity Recalculation using ADJUSTED Pressure                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         200510100000002005101000000020051010000000200604190000002006041900000020060419000000JA  ARFMfmtp2.2                                                                 20050927065022  IP                  G�O�G�O�G�O�                JA  ARGQrqcp2.3                                                                 20050927065023  QCP$                G�O�G�O�G�O�           1FB7CJA  ARUP                                                                        20050927070357                      G�O�G�O�G�O�                JA  ARFMfmtp2.2                                                                 20051001005254  IP                  G�O�G�O�G�O�                JA  ARGQrqcp2.3                                                                 20051001005254  QCP$                G�O�G�O�G�O�           1FB7CJA  ARUP                                                                        20051001010856                      G�O�G�O�G�O�                JM  ARCAJMQC                                                                    20051010000000  CV  PRES            G�O�G�O�G�O�                JM  ARCAJMQC                                                                    20051010000000  IP  PSAL            G�O�G�O�G�O�                JM  ARSQWJO 1   SeHyD1                                                          20060419000000  IP  PSAL            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20060908013439  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20060908013703                      G�O�G�O�G�O�                JM  AREVjmrvp1.2                                                                20090312120634  IP  PRES            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20090318072500  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20090318073017                      G�O�G�O�G�O�                JM      JMFC1.0                                                                 20150609200027                      G�O�G�O�G�O�                JA  ARDU                                                                        20150614052517                      G�O�G�O�G�O�                