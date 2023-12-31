CDF   !   
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   s   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JAMSTEC    source        
Argo float     history       b2009-09-12T00:55:03Z creation;2015-03-10T02:35:27Z update;2015-06-07T17:17:54Z conversion to V3.1;     
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
_FillValue                  �  L�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   M   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   V   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   _   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  h   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    h�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    h�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    h�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    h�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  h�   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    h�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    h�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    h�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         i   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         i   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        i   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    iArgo profile    3.1 1.2 19500101000000  4900902 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               tA   JA  20090912005503  20150616074514  A9_60146_116                    2C  D   APEX                            2416                            061305                          846 @�J�Zt�1   @�J�q� @<��1&��d
=p��
1   ARGOS   A   A   A   Primary sampling: discrete [spot sampling CTD data]                                                                                                                                                                                                                @�33A��Ad��A�33A�33A�  B
��B33B2��BF��BZ  Bm��B���B�33B�ffB���B�33B���B�  Bƙ�B�ffB���B䙚B�33B�ffC33CffCL�C�C�C�fCL�C$�C)�C.33C3ffC8ffC=ffCB��CGffCQ� C[L�Ce33Co33Cx�fC��3C���C��fC�s3C��3C��fC���C�� C���C���C��fC��fC�� C³3Cǳ3C̳3Cѳ3C֦fCۦfC�fC�fC�s3C�fC��fC���D��D��D�fDٚD�3D�fD�fD$�3D)�3D.�3D3�fD8� D=�3DBٚDG��DLٚDQ� DV��D[�3D`�fDe�fDj�fDo� Dt�3Dy�fD�,�D�` D��fD�� D�,�D�\�D���D�� D��D�` D���D���D�)�D�l�Dڠ D�� D�&fD�Y�D�fD�31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @���A	��AY��A���A���A�ffB  BffB0  BD  BW33Bj��B~ffB���B�  B�33B���B�ffB���B�33B�  B�ffB�33B���B�  C � C�3C
��CffCffC33C��C#ffC(ffC-� C2�3C7�3C<�3CA�fCF�3CP��CZ��Cd� Cn� Cx33C�Y�C�@ C�L�C��C�Y�C�L�C�@ C�&fC�33C�@ C�L�C�L�C�&fC�Y�C�Y�C�Y�C�Y�C�L�C�L�C�L�C�L�C��C�L�C�L�C�33D� D� D��D��D�fD��D��D$�fD)�fD.�fD3��D8�3D=�fDB��DG� DL��DQ�3DV��D[�fD`��De��Dj��Do�3Dt�fDy��D�fD�I�D�� D�ɚD�fD�FfD��fD�ɚD�fD�I�D��3D��fD�3D�VfDډ�D�ɚD� D�C3D� D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��Aֺ^A�9XA�AԓuA�VA���Aӡ�A��A�A�^5A��!A���A���A��A��A�dZA�ffA���A��\A�E�A�(�A��A�7LA�{A���A���A���A���A���A���A�^5A�9XA��A��A��jA��^A��AyƨAw�;Ar1'Ah�Ae�FA[�ASx�AN=qAJ��AE�
AC7LA@�A>��A:�A6�uA3��A0��A-��A+��A(Q�A%XA"5?A A�A�9A��AA�A�!A��AA��A�A�-A 9X@�ff@�u@�dZ@��@Ǯ@���@� �@�%@�S�@���@��9@���@��F@�Z@�o@���@�j@�V@�I�@{ƨ@vff@t�@nV@jM�@gl�@a��@\z�@T�j@N�+@I%@@ �@9x�@3��@.ff@(�9@#��@ 1'@S�@��@-@�@ƨ@��@?}@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Aֺ^A�9XA�AԓuA�VA���Aӡ�A��A�A�^5A��!A���A���A��A��A�dZA�ffA���A��\A�E�A�(�A��A�7LA�{A���A���A���A���A���A���A�^5A�9XA��A��A��jA��^A��AyƨAw�;Ar1'Ah�Ae�FA[�ASx�AN=qAJ��AE�
AC7LA@�A>��A:�A6�uA3��A0��A-��A+��A(Q�A%XA"5?A A�A�9A��AA�A�!A��AA��A�A�-A 9X@�ff@�u@�dZ@��@Ǯ@���@� �@�%@�S�@���@��9@���@��F@�Z@�o@���@�j@�V@�I�@{ƨ@vff@t�@nV@jM�@gl�@a��@\z�@T�j@N�+@I%@@ �@9x�@3��@.ff@(�9@#��@ 1'@S�@��@-@�@ƨ@��@?}@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB+BoB\B%BB��B�B��BoB��B�Bx�BffBQ�B5?B:^B�B)�B�B1B�B�3B�3B�RB�}BǮB�}B�!B�uBm�B�B
�yB
��B
�B
��B
�DB
`BB
33B
�B	�B	�FB	��B	XB	2-B	�B	�B	
=B��B��B		7B	B�B�B�#B��B��B�}B�-B��B��B�\B�Bw�Bp�BgmBYBM�BD�B<jB6FB%�B�B\B
=B	7B
=B�B#�B/B?}BVBl�B|�B��B�'B�wB��B�BB�B	+B	�B	&�B	L�B	`BB	n�B	�VB	��B	ǮB	�;B	�B
	7B
�B
&�B
1'B
<jB
E�B
K�B
T�B
\)B
bNB
gmB
k�B
q�B
u�B
{�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B,BoB\B%BB��B�B��B�B��B�%By�BiyBVB:^B<jB �B+B�BDB�)B�9B�3B�RB�}BȴB��B�'B��Bp�B!�B
�B
��B
�B
��B
�VB
cTB
49B
"�B	��B	�LB	��B	ZB	33B	�B	�B	DB	  B��B	
=B	B�B�B�)B��B��B��B�3B��B��B�bB�Bx�Bq�BhsBZBN�BE�B=qB7LB&�B�BbBDB
=BDB�B#�B0!B?}BVBl�B|�B��B�'B�wB��B�BB�B	+B	�B	&�B	L�B	`BB	n�B	�VB	��B	ǮB	�;B	�B
	7B
�B
&�B
1'B
<jB
E�B
K�B
T�B
\)B
bNB
gmB
k�B
q�B
u�B
{�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ = PRES-SP(NextCycle), where SP is SURFACE PRESSURE from next cycle                                                                                                                                                                                     TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,PRES_ADJ,elapsed_time,alpha,tau),elapsed_time=P/mean_rise_rate,P=dbar since the start of the profile for each samples                                                                                                       None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            SP(NextCycle)=0.7(dbar)                                                                                                                                                                                                                                         None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE; PRES_ADJ_ERR : Manufacture sensor accuracy                                                                                                                                                                 TEMP_ADJ_ERR : SBE sensor accuracy                                                                                                                                                                                                                              Salinity Recalculation using PRES_ADJ; PSAL_ADJ_ERR : max(sum of RecalS & CTM errors, 0.01(PSS-78))                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         200909250218562009092502185620090925021856200909250515052009092505150520090925051505201010150000002010101500000020101015000000  JA  ARFMdecpA9_b                                                                20090912005502  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.5                                                                 20090912005503  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.0                                                                 20090912005504  IP  PRES            G�O�G�O�G�O�                JA  ARCArsal2.1a                                                                20090912005504  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20090912005504  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20090912005506  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20090912005506  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8b                                                                20090912005506  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8b                                                                20090912005506  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20090912005506  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20090912010317                      G�O�G�O�G�O�                JA  ARFMdecpA9_b                                                                20090915185457  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.5                                                                 20090915185723  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.0                                                                 20090915185723  IP  PRES            G�O�G�O�G�O�                JA  ARCArsal2.1a                                                                20090915185723  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20090915185724  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20090915185725  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20090915185725  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8b                                                                20090915185725  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8b                                                                20090915185725  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20090915185725  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20090915190131                      G�O�G�O�G�O�                JM  ARGQJMQC1.0                                                                 20090915112037  CV  DAT$            G�O�G�O�F�W�                JM  ARCAJMQC1.0                                                                 20090925021856  CV  PRES            G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20090925021856  IP  PSAL            G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20090925051505  CV  PSAL            G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2010V1                                                       20101015000000  IP  PSAL            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20101104043232  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20101104043321                      G�O�G�O�G�O�                JM  RENCREJM1.1c                                                                20150209092721  ED  SCIENTIFIC_CALIBG�O�G�O�G�O�                JA  ARDU                                                                        20150310023527                      G�O�G�O�G�O�                JM      JMFC1.0                                                                 20150607171745                      G�O�G�O�G�O�                JA  ARDU                                                                        20150616074514                      G�O�G�O�G�O�                