CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PARAM       N_PROF        N_CALIB       N_LEVELS   s   	N_HISTORY             	   title         Argo float vertical profile    institution       JAMSTEC    source        
Argo float     history       b2005-03-11T02:48:24Z creation;2009-03-18T07:29:00Z update;2015-06-09T19:56:50Z conversion to V3.1;     
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
_FillValue                    _�Argo profile    3.1 1.0 19500101000000  5900650 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               A   JA  20050311024824  20150614052511  A5_23632_018                    2C  D   APEX                            1557                            013004                          846 @ӯyD��1   @ӯ{*z@4޸Q��c     1   ARGOS   A   A   A   Primary sampling: discrete [spot sampling CTD data]                                                                                                                                                                                                                @���A��A`  A�33A�  A�ffB��B33B0  BE��BX��BlffB�  B���B���B�33B���B�  B���B�  BЙ�B���B�ffB�  B�  CL�C�C� C  CL�C33CL�C$��C)ffC.� C3ffC8L�C=L�CB�CG�CQ  C[�Cd��Co33Cy33C��fC��3C�s3C�s3C�ffC�ٚC���C�� C��fC�� C�� C�� C���C³3CǙ�C�� Cљ�Cր C���C�3C�3CꙚC� C��C��3D�3D��D�fDٚD�3D�3D�3D$�3D)��D.��D3��D8�fD=�fDB� DGٚDL� DQ�3DV� D[�3D`�fDe��Dj�3Do��Dt��Dy� D�)�D�c3D�� D���D�)�D�ffD���D��D��D�l�D���D�� D�)�D�s3Dڣ3D���D�  D�ffD� D�  1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�33A��A[33A���A���A�  B��B  B.��BDffBW��Bk33B~��B�33B�33B���B�  B�ffB�33B�ffB�  B�33B���B�ffB�ffC  C��C33C�3C  C�fC  C$L�C)�C.33C3�C8  C=  CA��CF��CP�3CZ��Cd� Cn�fCx�fC�� C���C�L�C�L�C�@ C��3C��fC���C�� C���C���C���C�s3C�C�s3C̙�C�s3C�Y�CۦfC���C��C�s3C�Y�C�ffC���D� D�fD�3D�fD� D� D� D$� D)��D.��D3��D8�3D=�3DB��DG�fDL��DQ� DV��D[� D`�3De��Dj� Do�fDt��Dy��D�  D�Y�D��fD��3D�  D�\�D�� D�� D�3D�c3D��3D��fD�  D�i�Dڙ�D��3D�fD�\�D�fD�f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�Q�A�Q�A�M�A�\)A�`BA�XA�7LA��TA��^A���A��DA�r�A�ffA�ZA�VA�S�A�=qA�bA�t�A��mA�A��wA�$�A���A�v�A��A�XA�?}A�VA��/A��;A�9XA��yA�^5A�Q�A�5?A�VA�ȴA�x�A��A���A���A�&�Av�Aj�A]��AP=qAGl�A@�HA:I�A2��A+��A&A�A!33AVA�\AA�A9XA	33AI�A�A $�@��\@��@�~�@��@�v�@ۥ�@���@�7L@�x�@�t�@�|�@��@�G�@�O�@�-@�7L@���@�&�@�ȴ@��w@��D@�V@�9X@��@�bN@�ȴ@���@���@�@�Q�@��h@��w@�%@��H@y��@p�9@hA�@_��@Y7L@R��@J�@C�
@<9X@5?}@.$�@'\)@!G�@?}@A�@�F@��@�@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�Q�A�Q�A�M�A�\)A�`BA�XA�7LA��TA��^A���A��DA�r�A�ffA�ZA�VA�S�A�=qA�bA�t�A��mA�A��wA�$�A���A�v�A��A�XA�?}A�VA��/A��;A�9XA��yA�^5A�Q�A�5?A�VA�ȴA�x�A��A���A���A�&�Av�Aj�A]��AP=qAGl�A@�HA:I�A2��A+��A&A�A!33AVA�\AA�A9XA	33AI�A�A $�@��\@��@�~�@��@�v�@ۥ�@���@�7L@�x�@�t�@�|�@��@�G�@�O�@�-@�7L@���@�&�@�ȴ@��w@��D@�V@�9X@��@�bN@�ȴ@���@���@�@�Q�@��h@��w@�%@��H@y��@p�9@hA�@_��@Y7L@R��@J�@C�
@<9X@5?}@.$�@'\)@!G�@?}@A�@�F@��@�@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB9XB]/Be`Bt�By�Bz�Bx�B�B�bB�bB�VB�PB�PB�VB�PB�oB�uB��B��B�!B��B��B�TB�B��B%B�B�B�B��B��B��Bp�BYB9XBB�NB�dB�=BR�B
��B
}�B	��B	�B	7LB�B��B�BgmBN�B7LB.B2-BK�BZBS�BXBQ�BD�B?}BB�B>wB=qB7LB:^B5?B;dBE�BVBgmB}�B��B�B��B�B	�B	:^B	W
B	bNB	v�B	�%B	�uB	��B	�-B	�jB	B	ɺB	��B	�B	�)B	�HB	�fB	�B	�B	��B
B
bB
�B
 �B
'�B
/B
49B
;dB
A�B
G�B
M�B
S�B
YB
_;B
cTB
hsB
m�B
r�B
v�B
{�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B9XB]/Be`Bt�By�Bz�Bx�B�B�bB�bB�VB�PB�PB�VB�PB�oB�uB��B��B�!B��B��B�TB�B��B%B�B�B�B��B��B��Bp�BYB9XBB�NB�dB�=BR�B
��B
}�B	��B	�B	7LB�B��B�BgmBN�B7LB.B2-BK�BZBS�BXBQ�BD�B?}BB�B>wB=qB7LB:^B5?B;dBE�BVBgmB}�B��B�B��B�B	�B	:^B	W
B	bNB	v�B	�%B	�uB	��B	�-B	�jB	B	ɺB	��B	�B	�)B	�HB	�fB	�B	�B	��B
B
bB
�B
 �B
'�B
/B
49B
;dB
A�B
G�B
M�B
S�B
YB
_;B
cTB
hsB
m�B
r�B
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
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - SP, where SP is SURFACE PRESSURE (minus 5 dbar for Apf-6,7,8) from next cycle.                                                                                                                                                           none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL(PRES_ADJUSTED,TEMP,Conductivity)                                                                                                                                                                                                           none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            SP(NextCycle) = 0.3 dbar                                                                                                                                                                                                                                        none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using reported SURFACE PRESSURE. The quoted error is max [2.4, size of pressure adjustment] in dbar.                                                                                                                                      The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Salinity Recalculation using ADJUSTED Pressure                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         200503220000002005032200000020050322000000200506090000002005060900000020050609000000JA  ARFMfmtp2.2                                                                 20050311024824  IP                  G�O�G�O�G�O�                JA  ARGQrqcp2.3                                                                 20050311024824  QCP$                G�O�G�O�G�O�           1FB7CJA  ARUP                                                                        20050311025450                      G�O�G�O�G�O�                JM  ARCAJMQC                                                                    20050322000000  CV  PRES            G�O�G�O�G�O�                JM  ARCAJMQC                                                                    20050322000000  IP  PSAL            G�O�G�O�G�O�                JM  ARSQWJO 1   SeHyD1                                                          20050609000000  IP  PSAL            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20060310063556  IP                  G�O�G�O�G�O�                JA  RFMTcnv22.0                                                                 20060414072359  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20060418030305                      G�O�G�O�G�O�                JM  AREVjmrvp1.2                                                                20090312120629  IP  PRES            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20090318072337  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20090318072900                      G�O�G�O�G�O�                JM      JMFC1.0                                                                 20150609195639                      G�O�G�O�G�O�                JA  ARDU                                                                        20150614052511                      G�O�G�O�G�O�                