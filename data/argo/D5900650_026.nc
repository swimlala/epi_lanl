CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PARAM       N_PROF        N_CALIB       N_LEVELS   s   	N_HISTORY             	   title         Argo float vertical profile    institution       JAMSTEC    source        
Argo float     history       b2005-05-30T06:46:53Z creation;2009-03-18T07:30:09Z update;2015-06-09T19:58:18Z conversion to V3.1;     
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
_FillValue                    _�Argo profile    3.1 1.2 19500101000000  5900650 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               A   JA  20050530064653  20150614052514  A5_23632_026                    2C  D   APEX                            1557                            013004                          846 @�Á3���1   @�Åi�6�@2V�+J�cB-V1   ARGOS   A   A   A   Primary sampling: discrete [spot sampling CTD data]                                                                                                                                                                                                                @���A��AfffA���A���A�  B��B��B1��BE��BY��BlffB�ffB�ffB�  B�ffB���B���B�33B�  B�ffB���B䙚B�33B�33CffC�C��CffC� C� C  C$� C)L�C.� C3�C8L�C=�CB�CGL�CQ�C[L�CeL�Co� CyffC��3C��fC�s3C�� C��3C��fC��fC��3C�� C�� C��fC���C��3C¦fCǦfČ�Cѳ3C֦fCۙ�C���C噚CꙚC�3C�� C�s3D��D�3D��D�3D� D�fD�3D$� D)� D.��D3�fD8�fD=ٚDB�fDG��DL�3DQ��DV�3D[ٚD`ٚDe�fDj�3Do�fDt�3Dy� D�&fD�p D�� D�� D�  D�i�D���D�� D�  D�c3D���D�� D�#3D�c3Dڣ3D�ٚD�)�D�c3D�D�ٚ1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @���A33Ad��A���A�  A�33BffBffB133BE33BY33Bl  B�33B�33B���B�33B�ffB���B�  B���B�33Bڙ�B�ffB�  B�  CL�C  C� CL�CffCffC�fC$ffC)33C.ffC3  C833C=  CB  CG33CQ  C[33Ce33CoffCyL�C��fC���C�ffC�s3C��fC���C���C��fC�s3C��3C���C�� C��fC�CǙ�C̀ CѦfC֙�Cی�C�� C��C��C�fC��3C�ffD�fD��D�fD��D��D� D��D$ٚD)��D.�fD3� D8� D=�3DB� DG�3DL��DQ�fDV��D[�3D`�3De� Dj��Do� Dt��Dy��D�#3D�l�D���D���D��D�ffD���D���D��D�` D��fD���D�  D�` Dڠ D��fD�&fD�` D�fD��f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�hsAͣ�A�ƨA͍PA�9XA���A���A�JḀ�A�|�AɋDA��#A��A��hA���A�ȴA�O�A�ƨA���A��PA�;dA�\)A���A�JA���A���A��HA�|�A�S�A�l�A��FA�ĜA�ĜA���A��wA��A��A�ZA�A��9A�A�S�A�oA|ffAn��Ad��AZ��AM/AE�TA<A1/A+A&�yA$�\A��A{Ar�A+A�7A�AbA��@���@�G�@��@�
=@�X@�b@�`B@���@Ɵ�@�-@���@��@�x�@��+@�33@�n�@�+@���@��w@�z�@�S�@�&�@�o@���@�J@���@��@�1'@�@�+@�r�@��F@�1'@}@vȴ@m��@e��@[��@Tj@N�+@D�/@@�9@8Ĝ@1��@+S�@%�@!G�@��@G�@Z@bN@t�@ �1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�hsAͣ�A�ƨA͍PA�9XA���A���A�JḀ�A�|�AɋDA��#A��A��hA���A�ȴA�O�A�ƨA���A��PA�;dA�\)A���A�JA���A���A��HA�|�A�S�A�l�A��FA�ĜA�ĜA���A��wA��A��A�ZA�A��9A�A�S�A�oA|ffAn��Ad��AZ��AM/AE�TA<A1/A+A&�yA$�\A��A{Ar�A+A�7A�AbA��@���@�G�@��@�
=@�X@�b@�`B@���@Ɵ�@�-@���@��@�x�@��+@�33@�n�@�+@���@��w@�z�@�S�@�&�@�o@���@�J@���@��@�1'@�@�+@�r�@��F@�1'@}@vȴ@m��@e��@[��@Tj@N�+@D�/@@�9@8Ĝ@1��@+S�@%�@!G�@��@G�@Z@bN@t�@ �1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
�\B
�PB
�B
�B
�!B
�-B
�?B
�dB\BhsBu�Bl�B�B��B��B7LB?}BZBz�B�B�bB��B��B�9B��B��B��B�`B�;B��BÖB��B�Bx�BW
B0!B�B+B�ZBǮBr�B
=B
��B
bB	�?B	r�B	�B��B�oBe`BW
BVBt�B�B�}B�}B�B��B��B�mBǮB��Bp�Bw�Bn�BhsBe`BiyBr�Bz�B�VB�?BɺB�)B�B	�B	0!B	N�B	]/B	n�B	�+B	��B	��B	�!B	�XB	�}B	��B	�B	�5B	�ZB	�B	�B	��B	��B
B
JB
oB
�B
!�B
-B
33B
8RB
A�B
E�B
L�B
R�B
XB
^5B
bNB
ffB
jB
n�B
r�B
x�B
{�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
�\B
�PB
�B
�B
�!B
�-B
�?B
�dB\BhsBu�Bl�B�B��B��B7LB?}BZBz�B�B�bB��B��B�9B��B��B��B�`B�;B��BÖB��B�Bx�BW
B0!B�B+B�ZBǮBr�B
=B
��B
bB	�?B	r�B	�B��B�oBe`BW
BVBt�B�B�}B�}B�B��B��B�mBǮB��Bp�Bw�Bn�BhsBe`BiyBr�Bz�B�VB�?BɺB�)B�B	�B	0!B	N�B	]/B	n�B	�+B	��B	��B	�!B	�XB	�}B	��B	�B	�5B	�ZB	�B	�B	��B	��B
B
JB
oB
�B
!�B
-B
33B
8RB
A�B
E�B
L�B
R�B
XB
^5B
bNB
ffB
jB
n�B
r�B
x�B
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
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - SP, where SP is SURFACE PRESSURE (minus 5 dbar for Apf-6,7,8) from next cycle.                                                                                                                                                           none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL(PRES_ADJUSTED,TEMP,Conductivity)                                                                                                                                                                                                           none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            SP(NextCycle) = 0.1 dbar                                                                                                                                                                                                                                        none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using reported SURFACE PRESSURE. The quoted error is max [2.4, size of pressure adjustment] in dbar.                                                                                                                                      The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Salinity Recalculation using ADJUSTED Pressure                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         200506120000002005061200000020050612000000200604190000002006041900000020060419000000JA  ARFMfmtp2.2                                                                 20050530064653  IP                  G�O�G�O�G�O�                JA  ARGQrqcp2.3                                                                 20050530064654  QCP$                G�O�G�O�G�O�           1FB7CJA  ARUP                                                                        20050530065435                      G�O�G�O�G�O�                JA  ARFMfmtp2.2                                                                 20050603005109  IP                  G�O�G�O�G�O�                JA  ARGQrqcp2.3                                                                 20050603005110  QCP$                G�O�G�O�G�O�           1FB7CJA  ARUP                                                                        20050603010007                      G�O�G�O�G�O�                JM  ARCAJMQC                                                                    20050612000000  IP  PRES            G�O�G�O�G�O�                JM  ARCAJMQC                                                                    20050612000000  IP  PSAL            G�O�G�O�G�O�                JM  ARSQWJO 1   SeHyD1                                                          20060419000000  IP  PSAL            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20060908013429  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20060908013654                      G�O�G�O�G�O�                JM  AREVjmrvp1.2                                                                20090312120631  IP  PRES            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20090318072450  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20090318073009                      G�O�G�O�G�O�                JM      JMFC1.0                                                                 20150609195811                      G�O�G�O�G�O�                JA  ARDU                                                                        20150614052514                      G�O�G�O�G�O�                