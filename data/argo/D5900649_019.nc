CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PARAM       N_PROF        N_CALIB       N_LEVELS   s   	N_HISTORY             	   title         Argo float vertical profile    institution       JAMSTEC    source        
Argo float     history       b2005-03-20T18:48:33Z creation;2009-04-07T03:46:13Z update;2015-06-09T19:31:35Z conversion to V3.1;     
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
_FillValue                    _�Argo profile    3.1 1.0 19500101000000  5900649 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               A   JA  20050320184833  20150621172515  A5_23579_019                    2C  D   APEX                            1556                            013004                          846 @ӱ�8��1   @ӱ��h@5������b�j~��1   ARGOS   A   A   A   Primary sampling: discrete [spot sampling CTD data]                                                                                                                                                                                                                @�33AffAfffA�33A���A�33B��B  B/33BD��BY��BnffB�  B�ffB�33B�ffB���B�  B�ffB�  B���B���B䙚B홚B�33C ��C33C  C��C��CffCffC$ffC)� C.33C3L�C833C=ffCBL�CG  CQ�C[ffCe� Co33Cy��C���C���C�� C�� C��fC���C��3C��fC��fC���C�� C���C��3C¦fCǦfC���Cљ�C֦fCۙ�C�fC�3C�� C��C��3C��3D�fD� D�3D�fD�fD�fD�3D$�3D)�fD.�fD3��D8�fD=� DB��DG�fDL�3DQ�fDV� D[ٚD`�3De�3Dj�3Do��Dt��DyٚD�)�D�i�D���D���D�)�D�\�D���D���D��D�` D��fD��3D�)�D�i�Dک�D��fD�)�D�i�D��D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�  A��Ad��A�ffA�  A�ffBffB��B.��BDffBY33Bn  B���B�33B�  B�33B���B���B�33B���BЙ�Bڙ�B�ffB�ffB�  C �3C�C
�fC�3C�3CL�CL�C$L�C)ffC.�C333C8�C=L�CB33CF�fCQ  C[L�CeffCo�Cy� C���C�� C�s3C�s3C���C�� C��fC���C���C�� C�s3C���C��fC�CǙ�C�� Cь�C֙�Cی�C���C�fC�3C� C��fC��fD� DٚD��D� D� D� D��D$��D)� D.� D3�fD8� D=��DB�3DG� DL��DQ� DVٚD[�3D`��De��Dj��Do�fDt�fDy�3D�&fD�ffD��fD��D�&fD�Y�D���D��D��D�\�D��3D�� D�&fD�ffDڦfD��3D�&fD�ffD�D��f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�?}A�?}A�;dA�A�A�5?A�oA�VA�VA�VA�VA�VA�
=A�1A�A���A�t�A��FA��A�%A���A�G�A�ƨA�?}A��\A�ĜA�VA��jA�t�A��/A��-A�n�A�&�A��A��A�E�A��A���A�ffA��A��`A�ȴA��PA�A���A��TA�JA~�\AlM�A^��AUO�AN-AG�A<A�A733A2JA*�A#�wA��A�uA�9AM�A�uA9XA�u@�E�@�hs@�@웦@�J@�|�@�E�@�?}@��@�t�@��@��#@��^@��+@��@���@��9@���@�@��h@��H@��;@���@�@�b@��@�1'@��#@���@�hs@���@�S�@|�/@so@iG�@`1'@X��@Qx�@L�@C��@;�
@5/@/�P@)��@$�@   @��@ȴ@~�@
=@
~�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�?}A�?}A�;dA�A�A�5?A�oA�VA�VA�VA�VA�VA�
=A�1A�A���A�t�A��FA��A�%A���A�G�A�ƨA�?}A��\A�ĜA�VA��jA�t�A��/A��-A�n�A�&�A��A��A�E�A��A���A�ffA��A��`A�ȴA��PA�A���A��TA�JA~�\AlM�A^��AUO�AN-AG�A<A�A733A2JA*�A#�wA��A�uA�9AM�A�uA9XA�u@�E�@�hs@�@웦@�J@�|�@�E�@�?}@��@�t�@��@��#@��^@��+@��@���@��9@���@�@��h@��H@��;@���@�@�b@��@�1'@��#@���@�hs@���@�S�@|�/@so@iG�@`1'@X��@Qx�@L�@C��@;�
@5/@/�P@)��@$�@   @��@ȴ@~�@
=@
~�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�3B�B[#Bq�Bu�B�B�bB��B��B�?BǮB��B�)B�TB��B
=BhB�B�B&�B�BhB�sB�B��B��Bp�BuB�5B�oB'�B
�XB
	7B	�B	0!B��B��B�qB��B��B�JB�+Bs�B[#B[#BVBS�BR�BN�BO�BQ�BO�BP�BO�BO�BL�BVBdZB~�B�PB�B�
B	%B	�B	5?B	E�B	e`B	�B	��B	��B	�B	�RB	ŢB	��B	�B	�BB	�fB	�B	�B	��B	��B
%B
\B
�B
"�B
+B
2-B
7LB
;dB
B�B
I�B
N�B
T�B
ZB
_;B
cTB
hsB
m�B
r�B
u�B
z�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�3B�B[#Bq�Bu�B�B�bB��B��B�?BǮB��B�)B�TB��B
=BhB�B�B&�B�BhB�sB�B��B��Bp�BuB�5B�oB'�B
�XB
	7B	�B	0!B��B��B�qB��B��B�JB�+Bs�B[#B[#BVBS�BR�BN�BO�BQ�BO�BP�BO�BO�BL�BVBdZB~�B�PB�B�
B	%B	�B	5?B	E�B	e`B	�B	��B	��B	�B	�RB	ŢB	��B	�B	�BB	�fB	�B	�B	��B	��B
%B
\B
�B
"�B
+B
2-B
7LB
;dB
B�B
I�B
N�B
T�B
ZB
_;B
cTB
hsB
m�B
r�B
u�B
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
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - SP, where SP is SURFACE PRESSURE (minus 5 dbar for Apf-6,7,8) from next cycle.                                                                                                                                                           none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL(PRES_ADJUSTED,TEMP,Conductivity)                                                                                                                                                                                                           none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            SP(NextCycle) = 0.1 dbar                                                                                                                                                                                                                                        none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using reported SURFACE PRESSURE. The quoted error is max [2.4, size of pressure adjustment] in dbar.                                                                                                                                      The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Salinity Recalculation using ADJUSTED Pressure                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         200504010000002005040100000020050401000000200506060000002005060600000020050606000000JA  ARFMfmtp2.2                                                                 20050320184833  IP                  G�O�G�O�G�O�                JA  ARGQrqcp2.3                                                                 20050320184834  QCP$                G�O�G�O�G�O�           1FB7CJA  ARUP                                                                        20050320185417                      G�O�G�O�G�O�                JA  ARFMfmtp2.2                                                                 20050324124912  IP                  G�O�G�O�G�O�                JA  ARGQrqcp2.3                                                                 20050324124913  QCP$                G�O�G�O�G�O�           1FB7CJA  ARUP                                                                        20050324125509                      G�O�G�O�G�O�                JM  ARCAJMQC                                                                    20050401000000  IP  PRES            G�O�G�O�G�O�                JM  ARCAJMQC                                                                    20050401000000  IP  PSAL            G�O�G�O�G�O�                JM  ARSQWJO 1   SeHyD1                                                          20050606000000  IP  PSAL            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20060310063553  IP                  G�O�G�O�G�O�                JA  RFMTcnv22.0                                                                 20060414072356  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20060418030331                      G�O�G�O�G�O�                JM  AREVjmrvp1.2                                                                20090312120555  IP  PRES            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20090318071800  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20090318071924                      G�O�G�O�G�O�                JA  ARDU                                                                        20090407034613                      G�O�G�O�G�O�                JM      JMFC1.0                                                                 20150609193130                      G�O�G�O�G�O�                JA  ARDU                                                                        20150621172515                      G�O�G�O�G�O�                