CDF       
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PARAM       N_PROF        N_CALIB       N_LEVELS   s   	N_HISTORY             	   title         Argo float vertical profile    institution       JAMSTEC    source        
Argo float     history       b2007-12-27T18:51:36Z creation;2009-04-06T05:42:53Z update;2015-06-07T02:29:45Z conversion to V3.1;     
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
_FillValue                    iArgo profile    3.1 1.2 19500101000000  4900650 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               XA   JA  20071227185136  20150613102511  A5_24111_088                    2C  D   APEX                            1717                            013004                          846 @Ԯ��a�]1   @Ԯډ���@Df�+J�d�n��1   ARGOS   A   A   A   Primary sampling: discrete [spot sampling CTD data]                                                                                                                                                                                                                @���A  A^ffA���A�ffA�  B	��BffB133BE33BY33Bm��B�33B���B�33B���B�ffB�33B���B�  B�  Bڙ�B�  B�  B�ffC�C�CL�C�CffC�CL�C#�fC)�C.L�C3ffC8  C=33CBL�CG  CQ�CZ�fCe�Co�Cy33C�� C���C��3C�� C��3C��3C���C��3C��fC��3C�� C�s3C���C�Cǀ C̙�C�s3Cֳ3C���C���C��C��CC���C�� D� D� DٚDٚD� D� D� D$ٚD)�3D.��D3ٚD8��D=ٚDBٚDG�3DLٚDQٚDV�3D[��D`� De�fDj�3Do�fDt�3Dy� D�  D�ffD���D�� D�,�D�` D���D�� D��D�l�D���D��fD��D�c3Dڣ3D��3D�)�D�` D��D�  1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�33A��A[33A�33A���A�ffB��B��B0ffBDffBXffBl��B���B�33B���B�33B�  B���B�33Bƙ�BЙ�B�33B䙚BB�  C �fC�fC�C�fC33C�fC�C#�3C(�fC.�C333C7��C=  CB�CF��CP�fCZ�3Cd�fCn�fCy  C�ffC��3C���C��fC���C���C�s3C���C���C���C��fC�Y�C�s3C�s3C�ffC̀ C�Y�C֙�C۳3C�� C�s3C�s3C� C� C��fD�3D�3D��D��D�3D�3D�3D$��D)�fD.��D3��D8� D=��DB��DG�fDL��DQ��DV�fD[� D`�3De��Dj�fDoٚDt�fDy�3D��D�` D��3D�ٚD�&fD�Y�D��fD�ٚD�3D�ffD��fD�� D�fD�\�Dڜ�D���D�#3D�Y�D�fD���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A.�+A.r�A.�DA.�DA.�DA.r�A.bNA.ffA.ZA.�A.bA.{A.JA.1A-�A-�-A-�A,��A+l�A)
=A&�/A%"�AA5?AAC�A5?A�yAoA�AƨA�wAQ�A�AVA�^AoA7LA
=AQ�A	��A	A	�PA�TA
��A�FA�A�PA�7@��T@��^@���@���@��@ە�@�33@��@�7L@Η�@���@ǅ@���@��7@���@� �@�?}@���@���@��-@��F@�;d@� �@�-@�~�@��`@�7L@��T@��@�(�@�G�@��T@;d@{@w�@tZ@p  @lz�@h�9@e�@`Q�@]�@X�`@V$�@S��@P��@J�@CC�@<�/@65?@0A�@*�!@%�-@!&�@ȴ@��@�w@�@�^@$�@33@  @�@�@ Ĝ?�/1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A.�+A.r�A.�DA.�DA.�DA.r�A.bNA.ffA.ZA.�A.bA.{A.JA.1A-�A-�-A-�A,��A+l�A)
=A&�/A%"�AA5?AAC�A5?A�yAoA�AƨA�wAQ�A�AVA�^AoA7LA
=AQ�A	��A	A	�PA�TA
��A�FA�A�PA�7@��T@��^@���@���@��@ە�@�33@��@�7L@Η�@���@ǅ@���@��7@���@� �@�?}@���@���@��-@��F@�;d@� �@�-@�~�@��`@�7L@��T@��@�(�@�G�@��T@;d@{@w�@tZ@p  @lz�@h�9@e�@`Q�@]�@X�`@V$�@S��@P��@J�@CC�@<�/@65?@0A�@*�!@%�-@!&�@ȴ@��@�w@�@�^@$�@33@  @�@�@ Ĝ?�/1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBĜBĜBŢBŢBŢBÖBBB��B�}B�}B��B��BBŢB��B�NB��B;dB�B��B�B�B�B!�B%�B �B�BVB1BDB��B�B�
B��BƨB��B�3B�LB�?B�B��B��BB+B
=B1BB��B�B�B�fB�fB�5BŢB�!B�dB�
B�
B�
B�
B��B�B�)B�#B�/B�NB�B�B��B
=BuB)�B6FBG�BYBgmBt�B�B�hB��B�9B��B��B�
B�fB�B��B	1B	�B	"�B	1'B	:^B	D�B	M�B	aHB	u�B	�1B	��B	�!B	��B	��B	�HB	�B	��B
B
DB
�B
#�B
.B
9XB
E�B
K�B
S�B
[#1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BĜBĜBŢBŢBŢBÖBBB��B�}B�}B��B��BBŢB��B�NB��B<jB�!B��B�B�B�B!�B&�B!�B�BVB1BJB��B�B�B��BƨB��B�3B�LB�FB�B��B��BB1B
=B	7BB��B�B�B�mB�mB�;BƨB�!B�dB�
B�
B�B�B��B�B�/B�#B�/B�NB�B�B��B
=B{B)�B6FBG�BYBgmBt�B�B�hB��B�9B��B��B�
B�fB�B��B	1B	�B	"�B	1'B	:^B	D�B	M�B	aHB	u�B	�1B	��B	�!B	��B	��B	�HB	�B	��B
B
DB
�B
#�B
.B
9XB
E�B
K�B
S�B
[#1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - SP, where SP is SURFACE PRESSURE (minus 5 dbar for Apf-6,7,8) from next cycle.                                                                                                                                                           none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = RecalS = PSAL(PRES_ADJUSTED,TEMP,Conductivity)                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = celltm_sbe41(RecalS,TEMP,PRES_ADJUSTED,elapsed_time,alpha,tau),elapsed_time=P/mean_rise_rate,P= dbar since the start of the profile for each samples.                                                                                           none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            SP(NextCycle) = 0.2 dbar                                                                                                                                                                                                                                        none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using reported SURFACE PRESSURE. The quoted error is max [2.4, size of pressure adjustment] in dbar.                                                                                                                                      The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Salinity Recalculation using PRES_ADJUSTED. PSAL_ADJ_ERR : SBE sensor accuracy & CTM adjustment                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         200801091351582008010913515820080109135158200801091401162008010914011620080109140116200809050000002008090500000020080905000000  JA  ARFMdecpA5_a                                                                20071227185133  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.4                                                                 20071227185136  IP                  G�O�G�O�G�O�                JA  ARCArsal2.1                                                                 20071227185136  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20071227185137  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20071227185141  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20071227185141  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.7d                                                                20071227185141  QCP$                G�O�G�O�G�O�            EB40JA  ARGQaqcp2.7d                                                                20071227185141  QCP$                G�O�G�O�G�O�            EB40JA  ARGQrqcpt16b                                                                20071227185142  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20071227190554                      G�O�G�O�G�O�                JA  ARFMdecpA5_a                                                                20071231154415  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.4                                                                 20071231154419  IP                  G�O�G�O�G�O�                JA  ARCArsal2.1                                                                 20071231154419  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20071231154420  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20071231154424  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20071231154424  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.7d                                                                20071231154424  QCP$                G�O�G�O�G�O�            EB40JA  ARGQaqcp2.7d                                                                20071231154424  QCP$                G�O�G�O�G�O�            EB40JA  ARGQrqcpt16b                                                                20071231154424  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20071231190949                      G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20080109135158  CV  PRES            G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20080109135158  IP  PSAL            G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20080109140116  CV  PSAL            G�O�G�O�G�O�                JM  ARSQWJO 2.0 SeHyD1.0                                                        20080905000000  IP  PSAL            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20080925024521  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20080925060440                      G�O�G�O�G�O�                JM  AREVjmrvp1.2                                                                20090312112536  IP  PRES            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20090318003353  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20090318003741                      G�O�G�O�G�O�                JA  ARDU                                                                        20090406054253                      G�O�G�O�G�O�                JM      JMFC1.0                                                                 20150607022938                      G�O�G�O�G�O�                JA  ARDU                                                                        20150613102511                      G�O�G�O�G�O�                