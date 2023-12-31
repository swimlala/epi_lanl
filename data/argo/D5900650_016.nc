CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   s   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JAMSTEC    source        
Argo float     history       b2005-02-19T02:48:58Z creation;2014-07-22T22:21:21Z update;2015-06-09T19:56:25Z conversion to V3.1;     
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
_FillValue                  t  ;l   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   standard_name         sea_water_pressure       �  ;�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  =�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  >    TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_temperature        �  ?�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  A�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_temperature        �  B,   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  C�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  Dl   PSAL         
      	   	long_name         Practical salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_salinity       �  F8   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  H   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_salinity       �  Hx   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  JD   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  J�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  `  L�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    L�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    R�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    X�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T  ^�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    _8   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    _<   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    _@   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    _D   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  _H   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    _�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    _�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    _�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         _�   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         _�   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        _�   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    _�Argo profile    3.1 1.2 19500101000000  5900650 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               A   JA  20050219024858  20150614052517  A5_23632_016                    2C  D   APEX                            1557                            013004                          846 @Ӫy��Sx1   @Ӫ~gY E@4��hr��b�C��1   ARGOS   A   A   A   Primary sampling: discrete [spot sampling CTD data]                                                                                                                                                                                                                @���A��Aa��A�ffA���A�  B	33B  B0ffBFffBY33Bm33B�33B�33B�33B���B�  B�  B�ffBƙ�B�ffB�33B���B�  B���C� C�C�C�C33C33CffC$L�C)  C.� C3ffC8��C=��CBffCGL�CQL�C[L�Ce33CoffCx��C�s3C�s3C�s3C��3C��fC��fC���C�� C�� C��3C��fC�s3C���C³3Cǀ C���CѦfC֦fCی�C�fC�s3C�s3C��C� C��3D�3D��D� D�3DٚD��D�fD$��D)� D.��D3� D8� D=�fDBٚDG��DL�fDQ��DV��D[��D`�3De��Dj� Do��Dt� Dy�3D�)�D�l�D���D��D�#3D�ffD���D�� D�0 D�` D��fD��D�&fD�p Dڰ D���D�&fD�l�D� D�Ff1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @���A  A`  A���A�  A�33B��B��B0  BF  BX��Bl��B�  B�  B�  B���B���B���B�33B�ffB�33B�  B䙚B���B�ffCffC  C  C  C�C�CL�C$33C(�fC.ffC3L�C8� C=� CBL�CG33CQ33C[33Ce�CoL�Cx�3C�ffC�ffC�ffC��fC���C���C���C��3C��3C��fC���C�ffC���C¦fC�s3C�� Cљ�C֙�Cۀ C���C�ffC�ffC� C�s3C��fD��D�3DٚD��D�3D�fD� D$�3D)��D.�fD3ٚD8ٚD=� DB�3DG�3DL� DQ�3DV�3D[�fD`��De�fDjٚDo�fDt��Dy��D�&fD�i�D���D��fD�  D�c3D��fD���D�,�D�\�D��3D��fD�#3D�l�Dڬ�D��D�#3D�i�D��D�C31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�JA�
=A�JA�1A�1A�oA��A�VA�JA�oA��+A�9XA���A��!A��7A�`BA��A���A�5?A���A���A���A�-A�I�A�  A�p�A�ƨA�l�A���A�+A��A��A�/A�"�A�A��/A��A�v�Ay�#Aw`BAm�Aa7LAUG�ALQ�AB�HA<$�A:{A5A.�A)`BA%�A!��A�;A�DAn�AjA�7A|�A5?@�^5@�l�@�b@��@�9X@�K�@ܬ@���@ӶF@�n�@�C�@�O�@�j@�dZ@���@�dZ@�Z@���@���@��m@�X@�J@�n�@�@���@�v�@�C�@�O�@��u@�bN@��^@�l�@��F@�J@�Q�@��+@���@z�H@q��@iG�@`��@V�+@Lj@Fȴ@?�w@:�\@3ƨ@/��@)hs@#�F@V@J@�@�7@��@	��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�JA�
=A�JA�1A�1A�oA��A�VA�JA�oA��+A�9XA���A��!A��7A�`BA��A���A�5?A���A���A���A�-A�I�A�  A�p�A�ƨA�l�A���A�+A��A��A�/A�"�A�A��/A��A�v�Ay�#Aw`BAm�Aa7LAUG�ALQ�AB�HA<$�A:{A5A.�A)`BA%�A!��A�;A�DAn�AjA�7A|�A5?@�^5@�l�@�b@��@�9X@�K�@ܬ@���@ӶF@�n�@�C�@�O�@�j@�dZ@���@�dZ@�Z@���@���@��m@�X@�J@�n�@�@���@�v�@�C�@�O�@��u@�bN@��^@�l�@��F@�J@�Q�@��+@���@z�H@q��@iG�@`��@V�+@Lj@Fȴ@?�w@:�\@3ƨ@/��@)hs@#�F@V@J@�@�7@��@	��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBhBoBoBuBuB{B{BuBuBuBiyBv�B{�B�BB�B�ZB�`B�B�FB�3B��B�PBgmBQ�B+BuB��B�}B�\B�JBQ�B�B
�B
�bB
G�B
B	�B	�-B	��B	W
B��BǮB��Bm�BP�BM�BO�BG�BH�BK�BYBR�BK�BC�BG�BH�B?}B<jB49B7LB2-B>wB<jB=qBJ�BVBM�Bn�Bz�B�PB��BĜB�sB	%B	%�B	D�B	XB	z�B	�B	�bB	��B	�!B	�^B	�}B	�jB	��B	�
B	��B	�NB	�yB	�B	��B	��B	��B
+B
VB
�B
 �B
(�B
1'B
:^B
@�B
D�B
H�B
N�B
R�B
YB
]/B
bNB
ffB
k�B
o�B
s�B
x�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BhBoBoBuBuB{B{BuBuBuBiyBv�B{�B�BB�B�ZB�`B�B�FB�3B��B�PBgmBQ�B+BuB��B�}B�\B�JBQ�B�B
�B
�bB
G�B
B	�B	�-B	��B	W
B��BǮB��Bm�BP�BM�BO�BG�BH�BK�BYBR�BK�BC�BG�BH�B?}B<jB49B7LB2-B>wB<jB=qBJ�BVBM�Bn�Bz�B�PB��BĜB�sB	%B	%�B	D�B	XB	z�B	�B	�bB	��B	�!B	�^B	�}B	�jB	��B	�
B	��B	�NB	�yB	�B	��B	��B	��B
+B
VB
�B
 �B
(�B
1'B
:^B
@�B
D�B
H�B
N�B
R�B
YB
]/B
bNB
ffB
k�B
o�B
s�B
x�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - SP, where SP is SURFACE PRESSURE (minus 5 dbar for Apf-6,7,8) from next cycle.                                                                                                                                                           none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL(PRES_ADJUSTED,TEMP,Conductivity)                                                                                                                                                                                                           none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            SP(NextCycle) = 0.1 dbar                                                                                                                                                                                                                                        none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using reported SURFACE PRESSURE; PRES_ADJ_ERR : Manufacture sensor accuracy                                                                                                                                                               The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Salinity Recalculation using ADJUSTED Pressure                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         200503020000002005030200000020050302000000200506090000002005060900000020050609000000JA  ARFMfmtp2.1                                                                 20050219024858  IP                  G�O�G�O�G�O�                JA  ARGQrqcp2.3                                                                 20050219024859  QCP$                G�O�G�O�G�O�           1FB7CJA  ARUP                                                                        20050219025442                      G�O�G�O�G�O�                JA  ARFMfmtp2.1                                                                 20050223004912  IP                  G�O�G�O�G�O�                JA  ARGQrqcp2.3                                                                 20050223004913  QCP$                G�O�G�O�G�O�           1FB7CJA  ARGQrelo2.1                                                                 20050223004913  CV  TIME            G�O�G�O�F�S�                JA  ARGQrelo2.1                                                                 20050223004913  CV  LAT$            G�O�G�O�A��m                JA  ARGQrelo2.1                                                                 20050223004913  CV  LON$            G�O�G�O���Z                JA  ARUP                                                                        20050223005434                      G�O�G�O�G�O�                JM  ARCAJMQC                                                                    20050302000000  CV  PRES            G�O�G�O�G�O�                JM  ARCAJMQC                                                                    20050302000000  IP  PSAL            G�O�G�O�G�O�                JM  ARSQWJO 1   SeHyD1                                                          20050609000000  IP  PSAL            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20060310063556  IP                  G�O�G�O�G�O�                JA  RFMTcnv22.0                                                                 20060414072359  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20060418030304                      G�O�G�O�G�O�                JM  AREVjmrvp1.2                                                                20090312120628  IP  PRES            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20090318072532  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20090318073046                      G�O�G�O�G�O�                JM  AREQREJM1.0                                                                 20140715031138  CF  POSITION_QC     G�O�G�O�@�                  JA  RFMTcnvd2.1                                                                 20140722221946  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20140722222121                      G�O�G�O�G�O�                JM      JMFC1.0                                                                 20150609195615                      G�O�G�O�G�O�                JA  ARDU                                                                        20150614052517                      G�O�G�O�G�O�                