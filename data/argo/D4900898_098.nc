CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PARAM       N_PROF        N_CALIB       N_LEVELS   s   	N_HISTORY             	   title         Argo float vertical profile    institution       JAMSTEC    source        
Argo float     history       b2009-03-13T12:56:41Z creation;2009-08-20T07:43:31Z update;2015-06-07T15:11:04Z conversion to V3.1;     
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
_FillValue                    iArgo profile    3.1 1.2 19500101000000  4900898 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               bA   JA  20090313125641  20150613142516  A9_60144_098                    2C  D   APEX                            2414                            061305                          846 @�S�Q�o1   @�T��$@C��`A�7�d�vȴ91   ARGOS   A   A   A   Primary sampling: discrete [spot sampling CTD data]                                                                                                                                                                                                                @���A  AfffA�  A�33A�33B��BffB0��BD��BY��Bl  B�ffB���B�  B�ffB���B���B���Bƙ�B�  B�  B���B�  B�ffC�CL�C33C33C�C� CL�C$L�C)33C.33C3ffC8  C=L�CB�CF�fCQ� C[��Ce� CoffCyffC�� C��3C���C��fC��fC��3C��3C���C���C��fC�� C��3C�� C³3Cǀ C�s3Cр C�� C۳3C���C�3C�fC��C���C�� D�3DٚD� D�3D��D�fDٚD$�fD)�3D.��D3�3D8� D=��DB�fDG��DL�fDQٚDVٚD[�fD`��De�fDj��DoٚDt� Dy� D�#3D�ffD��fD�� D�#3D�c3D��fD���D�)�D�c3D���D���D�#3D�c3Dک�D��fD�&fD�ffD�fD�f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�33A��A[33A�ffA���A噚B��B��B.  BB  BV��Bi33B~  B�ffB���B�  B�ffB�33B�33B�33Bϙ�Bٙ�B�ffB홚B�  C ffC��C
� C� CffC��C��C#��C(� C-� C2�3C7L�C<��CAffCF33CP��CZ�fCd��Cn�3Cx�3C�ffC�Y�C�@ C�L�C�L�C�Y�C�Y�C�33C�33C�L�C�ffC�Y�C�ffC�Y�C�&fC��C�&fC�ffC�Y�C�@ C�Y�C�L�C�33C�@ C�&fD�fD��D�3D�fD� D��D��D$��D)�fD.� D3�fD8�3D=��DB��DG� DL��DQ��DV��D[��D`� De��Dj��Do��Dt�3Dy�3D��D�P D�� D�ɚD��D�L�D�� D��fD�3D�L�D��3D��fD��D�L�Dړ3D�� D� D�P D� D�  1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A8�`A8�`A8�A8��A8�uA7�A7�PA7|�A7hsA7C�A7/A7+A7"�A7
=A6��A6�A6�+A4Q�A1�PA1��A1hsA0��A0bA/�
A0Q�A0^5A0A0VA2-A0�A/ƨA/�PA,��A+
=A'��A'&�A&5?A#��A"M�A!�FA bNAC�A��A�uAA��A��Ap�AE�A33A;dA�A	�A{A�AK�@��h@�z�@���@���@�z�@�\)@ڟ�@���@Լj@�~�@� �@�hs@�A�@��@�ȴ@�7L@��@�I�@�K�@�/@���@�X@��!@���@�b@�x�@���@�j@���@�@y��@t�j@ol�@h��@d9X@_;d@\�@Y��@V�@N�y@G�@@b@:��@5O�@/��@*��@&E�@"J@ȴ@��@1'@��@�@��@(�@	X@�@�H?�V1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A8�`A8�`A8�A8��A8�uA7�A7�PA7|�A7hsA7C�A7/A7+A7"�A7
=A6��A6�A6�+A4Q�A1�PA1��A1hsA0��A0bA/�
A0Q�A0^5A0A0VA2-A0�A/ƨA/�PA,��A+
=A'��A'&�A&5?A#��A"M�A!�FA bNAC�A��A�uAA��A��Ap�AE�A33A;dA�A	�A{A�AK�@��h@�z�@���@���@�z�@�\)@ڟ�@���@Լj@�~�@� �@�hs@�A�@��@�ȴ@�7L@��@�I�@�K�@�/@���@�X@��!@���@�b@�x�@���@�j@���@�@y��@t�j@ol�@h��@d9X@_;d@\�@Y��@V�@N�y@G�@@b@:��@5O�@/��@*��@&E�@"J@ȴ@��@1'@��@�@��@(�@	X@�@�H?�V1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�\B�VB�VB�PB�JB�=B�DB�DB�DB�JB�DB�DB�JB�DB�=B�7B�Bs�Be`Bm�Bn�Bp�Br�Bz�B�B�hB�jB#�Bm�B�B�DB�\B�1By�B]/BbNB\)BQ�BI�BO�B_;BhsBr�Bu�Be`B]/BYBP�BN�BO�BI�B<jB/B"�B�B\B%B��B�B�B�yB�fB�TB�BB�/B�;B�;B�/B�BB�HB�ZB�B��B	7B�B-B;dBG�BP�B[#Bk�Bx�B�B�{B��B�3BĜB��B�fB��B	DB	�B	$�B	-B	5?B	M�B	bNB	{�B	�JB	��B	�?B	ƨB	�B	�mB	�B	��B
DB
�B
�B
)�B
2-B
:^B
D�B
L�B
X1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�\B�VB�VB�PB�JB�=B�DB�DB�DB�JB�DB�DB�JB�DB�=B�7B�Bt�Be`Bm�Bn�Bp�Br�Bz�B�B�hB�jB"�Bn�B�%B�DB�hB�7B{�B^5BcTB]/BR�BI�BO�B_;BhsBs�Bv�BffB^5BYBP�BN�BP�BJ�B=qB0!B#�B�BbB+B��B�B�B�B�mB�TB�HB�5B�;B�;B�5B�BB�HB�ZB�B��B
=B�B-B;dBG�BP�B[#Bk�Bx�B�B�{B��B�3BĜB��B�fB��B	DB	�B	$�B	-B	5?B	M�B	bNB	{�B	�JB	��B	�?B	ƨB	�B	�mB	�B	��B
DB
�B
�B
)�B
2-B
:^B
D�B
L�B
X1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ = PRES-SP(NextCycle), where SP is SURFACE PRESSURE from next cycle                                                                                                                                                                                     TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,PRES_ADJ,elapsed_time,alpha,tau),elapsed_time=P/mean_rise_rate,P=dbar since the start of the profile for each samples                                                                                                       None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            SP(NextCycle)=0.7(dbar)                                                                                                                                                                                                                                         None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE; PRES_ADJ_ERR : Manufacture sensor accuracy                                                                                                                                                                 TEMP_ADJ_ERR : SBE sensor accuracy                                                                                                                                                                                                                              Salinity Recalculation using PRES_ADJ; PSAL_ADJ_ERR : max(sum of RecalS & CTM errors, 0.01(PSS-78))                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         200903261410292009032614102920090326141029200903261408062009032614080620090326140806200908190000002009081900000020090819000000  JA  ARFMdecpA9_b                                                                20090313125640  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.5                                                                 20090313125641  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.0                                                                 20090313125641  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20090313125641  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20090313125643  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20090313125643  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8b                                                                20090313125643  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8b                                                                20090313125643  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20090313125643  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20090313130529                      G�O�G�O�G�O�                JA  ARFMdecpA9_b                                                                20090317095527  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.5                                                                 20090317095750  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.0                                                                 20090317095750  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20090317095751  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20090317095752  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20090317095752  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8b                                                                20090317095752  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8b                                                                20090317095752  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20090317095752  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20090317100211                      G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20090326141029  CV  PRES            G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20090326141029  CV  PSAL            G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20090326140806  CV  PSAL            G�O�G�O�G�O�                JM  ARSQOW  1.1 SeHyD1.0                                                        20090819000000  IP  PSAL            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20090820074145  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20090820074331                      G�O�G�O�G�O�                JM      JMFC1.0                                                                 20150607151056                      G�O�G�O�G�O�                JA  ARDU                                                                        20150613142516                      G�O�G�O�G�O�                