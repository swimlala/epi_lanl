CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PARAM       N_PROF        N_CALIB       N_LEVELS   s   	N_HISTORY             	   title         Argo float vertical profile    institution       JAMSTEC    source        
Argo float     history       b2009-02-11T12:55:19Z creation;2009-08-20T07:43:16Z update;2015-06-07T15:10:22Z conversion to V3.1;     
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
_FillValue                    iArgo profile    3.1 1.2 19500101000000  4900898 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               _A   JA  20090211125519  20150613142512  A9_60144_095                    2C  D   APEX                            2414                            061305                          846 @����u1   @��{�u�@C�^5?|��d"���m1   ARGOS   A   A   A   Primary sampling: discrete [spot sampling CTD data]                                                                                                                                                                                                                @�33A  AfffA���A���A�33BffB��B133BE��BY��Bn  B�  B�ffB�ffB�ffB�  B�  B�  B���B�33Bڙ�B�33B�ffB���C  CffC33C33C33CffC�fC$  C)L�C-�fC3L�C8  C<��CBL�CG�CP��C[L�Ce33Co� CyffC��3C�� C���C�� C���C��fC���C��fC��fC�� C�s3C�s3C���C¦fCǀ C�ffCѳ3C�� C۳3C�3C�3C�fC�fC��fC�� D� D��D��D�3D�fD�fDٚD$�3D)��D.��D3��D8��D=ٚDB�fDG��DL��DQ�fDV�fD[� D`��De��Dj� Do� DtٚDy�3D�  D�p D���D���D�&fD�` D��3D��fD�&fD�i�D��3D�� D�#3D�ffDڠ D��3D�)�D�ffD�D�L�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @���A33AY��A�33A�33A���B33BffB.  BBffBVffBj��B|��B���B���B���B�ffB�ffB�ffB�33BΙ�B�  B♚B���B�  C 33C��C
ffCffCffC��C�C#33C(� C-�C2� C733C<  CA� CFL�CP  CZ� CdffCn�3Cx��C�L�C�Y�C�33C�Y�C�&fC�@ C�33C�@ C�@ C��C��C��C�33C�@ C��C�  C�L�C�Y�C�L�C�L�C�L�C�@ C�@ C�@ C��D��D�fD��D� D�3D�3D�fD$� D)�fD.��D3��D8��D=�fDB�3DG��DL��DQ�3DV�3D[��D`�fDe�fDj��Do��Dt�fDy� D�fD�VfD�� D��3D��D�FfD���D���D��D�P D���D��fD�	�D�L�DچfD�ɚD� D�L�D� D�331111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A<�A<z�A<�A;�mA;�A;;dA;;dA;;dA;C�A;C�A;K�A;33A:��A:�jA:��A:�jA8��A3�-A0-A+�A*��A,  A+\)A,�A+�A+�A*�RA)33A'��A%G�A#�A#7LA!t�A v�A�^A��A�A�TA�yA�!A��A`BA{AM�A%A-A/A�A�/A+A
ĜA�^A{A 5?@�j@�o@�\@�`B@�{@�/@ް!@ؼj@��T@́@�Ĝ@�~�@��h@�J@�33@�Q�@�`B@� �@��@��;@�o@�@�v�@���@��R@�o@�z�@��h@�9X@���@l�@{C�@w�@s"�@o�@j�!@f5?@a�#@^�+@[��@W|�@M��@F�@@A�@9�7@4�j@/�P@*M�@&5?@!�^@��@��@��@o@��@�@��@@�@ A�?���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A<�A<z�A<�A;�mA;�A;;dA;;dA;;dA;C�A;C�A;K�A;33A:��A:�jA:��A:�jA8��A3�-A0-A+�A*��A,  A+\)A,�A+�A+�A*�RA)33A'��A%G�A#�A#7LA!t�A v�A�^A��A�A�TA�yA�!A��A`BA{AM�A%A-A/A�A�/A+A
ĜA�^A{A 5?@�j@�o@�\@�`B@�{@�/@ް!@ؼj@��T@́@�Ĝ@�~�@��h@�J@�33@�Q�@�`B@� �@��@��;@�o@�@�v�@���@��R@�o@�z�@��h@�9X@���@l�@{C�@w�@s"�@o�@j�!@f5?@a�#@^�+@[��@W|�@M��@F�@@A�@9�7@4�j@/�P@*M�@&5?@!�^@��@��@��@o@��@�@��@@�@ A�?���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBJ�BK�BJ�BI�BH�BG�BG�BG�BH�BH�BI�BI�BJ�BP�BZBjB�dBVB`BBS�BG�BdZBaHB|�Bz�Bx�Bs�Bm�BaHBP�BC�B?}B33B2-B33B1'BA�B<jB>wBD�BT�B^5B\)B]/BYBQ�BK�BE�B?}B8RB2-B(�B�BhBDBB��B��B�B�TB�ZB�;B��B��B��B�/B�;B�fB�yB�B��B  BJB�B#�B6FBA�BO�B_;Bo�B|�B�JB�{B��B�'B�wBɺB�B�fB��B	B	oB	�B	&�B	49B	O�B	e`B	{�B	�hB	��B	�FB	ǮB	�B	�sB	��B
B
JB
�B
%�B
1'B
9XB
B�B
K�B
S�B
Z1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BJ�BK�BJ�BI�BH�BG�BG�BG�BH�BH�BI�BI�BJ�BP�BZBk�B�qBbBbNBT�BG�BdZB`BB|�Bz�By�Bt�Bn�BbNBQ�BC�B@�B49B2-B49B1'BB�B=qB>wBD�BT�B^5B]/B]/BZBQ�BL�BF�B@�B9XB33B)�B�BoBJBB  B��B�B�TB�`B�BB��B��B��B�5B�BB�fB�yB�B��B  BPB�B$�B6FBA�BP�B_;Bo�B}�B�JB�{B��B�'B�wB��B�B�fB��B	B	oB	�B	&�B	49B	O�B	e`B	{�B	�hB	��B	�FB	ǮB	�B	�sB	��B
B
JB
�B
%�B
1'B
9XB
B�B
K�B
S�B
Z1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ = PRES-SP(NextCycle), where SP is SURFACE PRESSURE from next cycle                                                                                                                                                                                     TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,PRES_ADJ,elapsed_time,alpha,tau),elapsed_time=P/mean_rise_rate,P=dbar since the start of the profile for each samples                                                                                                       None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            SP(NextCycle)=0.8(dbar)                                                                                                                                                                                                                                         None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE; PRES_ADJ_ERR : Manufacture sensor accuracy                                                                                                                                                                 TEMP_ADJ_ERR : SBE sensor accuracy                                                                                                                                                                                                                              Salinity Recalculation using PRES_ADJ; PSAL_ADJ_ERR : max(sum of RecalS & CTM errors, 0.01(PSS-78))                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         200902241349292009022413492920090224134929200902241404042009022414040420090224140404200908190000002009081900000020090819000000  JA  ARFMfmtp2.5                                                                 20090211125519  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20090211125520  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20090211125524  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8a                                                                20090211125524  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20090211125524  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20090211131030                      G�O�G�O�G�O�                JA  ARFMfmtp2.5                                                                 20090215164054  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20090215164058  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20090215164108  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8a                                                                20090215164108  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20090215164110  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20090215202945                      G�O�G�O�G�O�                JM  ARGQJMQC1.0                                                                 20090214223244  CV  DAT$            G�O�G�O�F��{                JM  ARCAJMQC1.0                                                                 20090224134929  CV  PRES            G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20090224134929  CV  PSAL            G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20090224140404  CV  PSAL            G�O�G�O�G�O�                JM  ARSQOW  1.1 SeHyD1.0                                                        20090819000000  IP  PSAL            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20090820074206  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20090820074316                      G�O�G�O�G�O�                JM      JMFC1.0                                                                 20150607151014                      G�O�G�O�G�O�                JA  ARDU                                                                        20150613142512                      G�O�G�O�G�O�                