CDF   '   
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PARAM       N_PROF        N_CALIB       N_LEVELS   s   	N_HISTORY             	   title         Argo float vertical profile    institution       JAMSTEC    source        
Argo float     history       b2009-02-01T15:57:13Z creation;2009-08-20T07:43:17Z update;2015-06-07T15:10:08Z conversion to V3.1;     
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
_FillValue                    iArgo profile    3.1 1.2 19500101000000  4900898 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               ^A   JA  20090201155713  20150613142511  A9_60144_094                    2C  D   APEX                            2414                            061305                          846 @�T�	�v1   @�UHpB�@C��
=p��d$9XbN1   ARGOS   A   A   A   Primary sampling: discrete [spot sampling CTD data]                                                                                                                                                                                                                @���A33Ad��A�ffA�  A�33B	��BffB0ffBD��BY33BlffB���B���B�  B���B���B�ffB�  Bƙ�B�33B�33B䙚B�  B�  C �fC  CL�C��C�CL�C�fC$�C)ffC.  C333C7��C=33CA�fCGL�CQffCZ�fCeffCoL�CyffC��3C�� C��fC��3C�� C�� C��fC��fC���C��3C��fC�s3C��3C³3C�ffC�s3Cь�C֙�C�� C���C���CꙚC�fC�s3C��fD�3D�fD�3D�fDٚDٚD� D$��D)ٚD.�fD3�3D8�3D=�3DBٚDG�fDL� DQ��DV�3D[��D`��De� Dj�fDoٚDt�fDy�3D��D�p D���D���D�  D�ffD���D�� D�  D�p D���D��D�)�D�s3Dڰ D��D�)�D�c3D�D�I�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @���A33A\��A�ffA�  A�33B��BffB.ffBB��BW33BjffB33B���B�  B���B���B�ffB�  Bř�B�33B�33B㙚B�  B�  C ffC� C
��CL�C��C��CffC#��C(�fC-� C2�3C7L�C<�3CAffCF��CP�fCZffCd�fCn��Cx�fC�s3C�� C�ffC�s3C�� C�@ C�ffC�ffC�L�C�s3C�ffC�33C�s3C�s3C�&fC�33C�L�C�Y�Cۀ C�L�C��C�Y�C�ffC�33C�ffD�3D�fD�3D�fD��D��D� D$��D)��D.�fD3�3D8�3D=�3DB��DG�fDL� DQ��DV�3D[��D`��De� Dj�fDo��Dt�fDy�3D�	�D�` D���D���D� D�VfD���D�� D� D�` D���D�ٚD��D�c3Dڠ D�ٚD��D�S3D�D�9�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A;oA;�A;&�A;/A;?}A;7LA;O�A;XA;S�A;S�A;S�A;S�A;\)A;\)A;`BA;`BA;O�A;K�A0�HA. �A+��A)��A(�HA'G�A'\)A(�A&�A)`BA(ZA&~�A&�A#��A#dZA!t�A ��A�`A;dAG�A�DAO�A?}AAC�AȴA��A�-AO�A�/A��A��A	|�An�A~�Av�Aƨ@���@�`B@���@�\@�$�@�(�@���@��@�hs@�5?@�@��#@��7@�/@��-@��D@���@�Z@��@��j@�A�@�S�@�  @�bN@���@��@�\)@��`@|�j@xbN@t1@o��@k��@f��@cdZ@`bN@\�@X��@U`B@Q��@J��@Dj@?+@9�#@3��@/l�@*~�@&{@!��@�-@=q@�+@33@�@dZ@Q�@��@^5?��;?�(�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A;oA;�A;&�A;/A;?}A;7LA;O�A;XA;S�A;S�A;S�A;S�A;\)A;\)A;`BA;`BA;O�A;K�A0�HA. �A+��A)��A(�HA'G�A'\)A(�A&�A)`BA(ZA&~�A&�A#��A#dZA!t�A ��A�`A;dAG�A�DAO�A?}AAC�AȴA��A�-AO�A�/A��A��A	|�An�A~�Av�Aƨ@���@�`B@���@�\@�$�@�(�@���@��@�hs@�5?@�@��#@��7@�/@��-@��D@���@�Z@��@��j@�A�@�S�@�  @�bN@���@��@�\)@��`@|�j@xbN@t1@o��@k��@f��@cdZ@`bN@\�@X��@U`B@Q��@J��@Dj@?+@9�#@3��@/l�@*~�@&{@!��@�-@=q@�+@33@�@dZ@Q�@��@^5?��;?�(�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B�B�)B{B5?B@�B>wB<jBD�B]/BG�B�Bv�Bm�Bk�BP�B`BBM�BR�BE�B6FB&�B!�B�B9XB`BB[#BS�BR�BM�BE�B>wB:^B6FB0!B,B&�B�B�B\BB��B��B�B�B�mB�ZB�;B�BB��B�
B�)B�BB�`B�BBJB�B)�B;dBL�BZBiyBy�B�=B��B��B�^BǮB�
B�`B�B	B	VB	�B	$�B	0!B	;dB	G�B	\)B	m�B	� B	�bB	��B	�'B	��B	��B	�TB	�B	��B
DB
�B
"�B
0!B
;dB
E�B
M�B
T�B
\)1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B!�B�/B�B6FB@�B?}B<jBC�B^5BF�B�Bw�Bm�Bm�BP�BaHBN�BS�BF�B7LB&�B"�B�B9XB`BB\)BS�BS�BN�BF�B>wB;dB7LB0!B-B'�B�B�BbB%B  B��B�B�B�sB�`B�;B�HB��B�B�/B�BB�`B�BBPB�B)�B;dBL�BZBiyBy�B�=B��B��B�^BǮB�
B�`B�B	B	VB	�B	$�B	0!B	;dB	G�B	\)B	m�B	� B	�bB	��B	�'B	��B	��B	�TB	�B	��B
DB
�B
"�B
0!B
;dB
E�B
M�B
T�B
\)1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ = PRES-SP(NextCycle), where SP is SURFACE PRESSURE from next cycle                                                                                                                                                                                     TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,PRES_ADJ,elapsed_time,alpha,tau),elapsed_time=P/mean_rise_rate,P=dbar since the start of the profile for each samples                                                                                                       None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            SP(NextCycle)=0.5(dbar)                                                                                                                                                                                                                                         None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE; PRES_ADJ_ERR : Manufacture sensor accuracy                                                                                                                                                                 TEMP_ADJ_ERR : SBE sensor accuracy                                                                                                                                                                                                                              Salinity Recalculation using PRES_ADJ; PSAL_ADJ_ERR : max(sum of RecalS & CTM errors, 0.01(PSS-78))                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         200902141332462009021413324620090214133246200902141336582009021413365820090214133658200908190000002009081900000020090819000000  JA  ARFMdecpA9_b                                                                20090201155712  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.5                                                                 20090201155713  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.0                                                                 20090201155714  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20090201155714  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20090201155715  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20090201155715  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8b                                                                20090201155715  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8b                                                                20090201155715  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20090201155715  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20090201160139                      G�O�G�O�G�O�                JA  ARFMdecpA9_b                                                                20090205095610  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.5                                                                 20090205095700  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.0                                                                 20090205095700  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20090205095700  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20090205095701  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20090205095701  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8b                                                                20090205095701  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8b                                                                20090205095701  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20090205095702  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20090205100107                      G�O�G�O�G�O�                JA  ARFMdecpA9_b                                                                20090205164012  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.5                                                                 20090422045144  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.0                                                                 20090422045144  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20090422045144  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20090422045146  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20090422045146  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8b                                                                20090422045146  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8b                                                                20090422045146  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20090422045146  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20090422045656                      G�O�G�O�G�O�                JM  ARGQJMQC1.0                                                                 20090204233243  CV  DAT$            G�O�G�O�F���                JM  ARCAJMQC1.0                                                                 20090214133246  CV  PRES            G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20090214133246  IP  PSAL            G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20090214133658  CV  PSAL            G�O�G�O�G�O�                JM  ARSQOW  1.1 SeHyD1.0                                                        20090819000000  IP  PSAL            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20090820074209  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20090820074317                      G�O�G�O�G�O�                JM      JMFC1.0                                                                 20150607150956                      G�O�G�O�G�O�                JA  ARDU                                                                        20150613142511                      G�O�G�O�G�O�                