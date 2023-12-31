CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PARAM       N_PROF        N_CALIB       N_LEVELS   s   	N_HISTORY             	   title         Argo float vertical profile    institution       JAMSTEC    source        
Argo float     history       b2005-09-17T06:47:58Z creation;2009-03-18T07:28:59Z update;2015-06-09T20:00:23Z conversion to V3.1;     
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
_FillValue                    _�Argo profile    3.1 1.2 19500101000000  5900650 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               %A   JA  20050917064758  20150614052511  A5_23632_037                    2C  D   APEX                            1557                            013004                          846 @������V1   @���@y\�@4�t��cG�;dZ1   ARGOS   A   A   A   Primary sampling: discrete [spot sampling CTD data]                                                                                                                                                                                                                @���AffAh  A�ffA���A���B	��B  B0��BE��BZffBn  B�ffB�ffB�ffB�  B���B�33B���B�ffBЙ�B�  B�ffB���B���C� C33C  C  C��CL�CffC$ffC)L�C.33C333C8� C=ffCBL�CGL�CQ33CZ�fCeffCo� Cy��C��fC���C��3C�ffC���C���C��fC���C���C�� C�� C��fC��fC�Cǳ3C̳3CѦfC�� Cی�C���C� C�fC�fC��3C�s3DٚD��D�3D� D� D��D�3D$�fD)��D.� D3�3D8�3D=��DB��DG��DL��DQ� DV� D[ٚD`��DeٚDj� Do��Dt��DyٚD��D�l�D�� D�� D�,�D�p D��3D�� D��D�Y�D���D��D��D�l�Dڠ D��fD�,�D�` D�D�S31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�33A33Ad��A���A�  A�33B��B33B0  BD��BY��Bm33B�  B�  B�  B���B�ffB���B�ffB�  B�33Bٙ�B�  B�ffB�33CL�C  C
��C��CffC�C33C$33C)�C.  C3  C8L�C=33CB�CG�CQ  CZ�3Ce33CoL�CyffC���C�� C���C�L�C�s3C�� C���C�� C�� C��fC�ffC���C���C�s3CǙ�C̙�Cь�C֦fC�s3C�� C�ffC��C��C���C�Y�D��D� D�fD�3D�3D� D�fD$��D)��D.�3D3�fD8�fD=� DB� DG��DL� DQ�3DV�3D[��D`� De��Dj�3Do��Dt��Dy��D�3D�ffD���D��D�&fD�i�D���D�ٚD�fD�S3D��fD��3D�fD�ffDڙ�D�� D�&fD�Y�D�3D�L�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�=qA���A��#AԺ^AԲ-AԬAԩ�A�9XA���A�A�l�A�1A�^5AA�?}A�ZA�ȴA��A�t�A��A�/A��A�A�dZA���A���A�C�A���A�JA���A���A��A��HA�A�`BA���A�VA���A��jA���A��PA���AxbNAe�mAV��AMt�AD$�A<�A3\)A-A(ĜA$�A"$�A�!A%AM�AK�A�A	|�A�A�+A �/@�bN@�I�@�j@�V@�Ĝ@��/@�"�@��@��@���@��u@��+@���@�
=@�1@�@�o@���@�b@�@�o@�1'@���@�G�@�v�@�p�@�V@�S�@�/@�A�@��!@��D@���@���@vE�@nE�@b�@X�u@RJ@J-@D��@=�@5�@.ȴ@(b@#S�@��@��@l�@��@�@	��@$�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�=qA���A��#AԺ^AԲ-AԬAԩ�A�9XA���A�A�l�A�1A�^5AA�?}A�ZA�ȴA��A�t�A��A�/A��A�A�dZA���A���A�C�A���A�JA���A���A��A��HA�A�`BA���A�VA���A��jA���A��PA���AxbNAe�mAV��AMt�AD$�A<�A3\)A-A(ĜA$�A"$�A�!A%AM�AK�A�A	|�A�A�+A �/@�bN@�I�@�j@�V@�Ĝ@��/@�"�@��@��@���@��u@��+@���@�
=@�1@�@�o@���@�b@�@�o@�1'@���@�G�@�v�@�p�@�V@�S�@�/@�A�@��!@��D@���@���@vE�@nE�@b�@X�u@RJ@J-@D��@=�@5�@.ȴ@(b@#S�@��@��@l�@��@�@	��@$�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�VB�PB�PB�JB�DB�JB�DB��B�B�mB�B��B�BB6FB��B�^B�B�B\BH�BoB  B	7BjB+B.B%B�B��B�B�ZB�LB�1B��Bq�B"�B
��B
�bB
��B
W
B	��B	F�BƨBv�B]/B`BB_;B_;Bl�B� Bv�B�\B�!B�VB�DB�Br�B� B�%B�{B�{B�PB�B~�B|�B�B�=B��B��B�'B��B��B�B	{B	&�B	N�B	n�B	�B	�VB	��B	��B	�!B	��B	��B	�HB	�sB	�B	�B	�B	��B
B
B
1B

=B
bB
�B
!�B
)�B
1'B
9XB
>wB
D�B
I�B
Q�B
XB
^5B
bNB
ffB
jB
o�B
t�B
y�B
}�B
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�VB�PB�PB�JB�DB�JB�DB��B�B�mB�B��B�BB6FB��B�^B�B�B\BH�BoB  B	7BjB+B.B%B�B��B�B�ZB�LB�1B��Bq�B"�B
��B
�bB
��B
W
B	��B	F�BƨBv�B]/B`BB_;B_;Bl�B� Bv�B�\B�!B�VB�DB�Br�B� B�%B�{B�{B�PB�B~�B|�B�B�=B��B��B�'B��B��B�B	{B	&�B	N�B	n�B	�B	�VB	��B	��B	�!B	��B	��B	�HB	�sB	�B	�B	�B	��B
B
B
1B

=B
bB
�B
!�B
)�B
1'B
9XB
>wB
D�B
I�B
Q�B
XB
^5B
bNB
ffB
jB
o�B
t�B
y�B
}�B
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - SP, where SP is SURFACE PRESSURE (minus 5 dbar for Apf-6,7,8) from next cycle.                                                                                                                                                           none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL(PRES_ADJUSTED,TEMP,Conductivity)                                                                                                                                                                                                           none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            SP(NextCycle) = 0.2 dbar                                                                                                                                                                                                                                        none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using reported SURFACE PRESSURE. The quoted error is max [2.4, size of pressure adjustment] in dbar.                                                                                                                                      The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Salinity Recalculation using ADJUSTED Pressure                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         200509300000002005093000000020050930000000200604190000002006041900000020060419000000JA  ARFMfmtp2.2                                                                 20050917064758  IP                  G�O�G�O�G�O�                JA  ARGQrqcp2.3                                                                 20050917064759  QCP$                G�O�G�O�G�O�           1FB7CJA  ARUP                                                                        20050917065624                      G�O�G�O�G�O�                JA  ARFMfmtp2.2                                                                 20050921005440  IP                  G�O�G�O�G�O�                JA  ARGQrqcp2.3                                                                 20050921005440  QCP$                G�O�G�O�G�O�           1FB7CJA  ARUP                                                                        20050921010918                      G�O�G�O�G�O�                JM  ARCAJMQC                                                                    20050930000000  CV  PRES            G�O�G�O�G�O�                JM  ARCAJMQC                                                                    20050930000000  IP  PSAL            G�O�G�O�G�O�                JM  ARSQWJO 1   SeHyD1                                                          20060419000000  IP  PSAL            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20060908013438  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20060908013702                      G�O�G�O�G�O�                JM  AREVjmrvp1.2                                                                20090312120634  IP  PRES            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20090318072336  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20090318072859                      G�O�G�O�G�O�                JM      JMFC1.0                                                                 20150609200018                      G�O�G�O�G�O�                JA  ARDU                                                                        20150614052511                      G�O�G�O�G�O�                