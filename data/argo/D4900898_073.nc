CDF   !   
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PARAM       N_PROF        N_CALIB       N_LEVELS   s   	N_HISTORY             	   title         Argo float vertical profile    institution       JAMSTEC    source        
Argo float     history       b2008-07-06T13:23:47Z creation;2009-08-20T07:43:24Z update;2015-06-07T15:05:02Z conversion to V3.1;     
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
_FillValue                    iArgo profile    3.1 1.2 19500101000000  4900898 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               IA   JA  20080706132347  20150613142514  A9_60144_073                    2C  D   APEX                            2414                            061305                          846 @���j�d�1   @�����(@D-p��
=�d��n�1   ARGOS   A   A   A   Primary sampling: discrete [spot sampling CTD data]                                                                                                                                                                                                                @�ffA��AfffA�ffA���A���B
��B��B2ffBE33BZ  Bn  B�  B���B���B�  B���B�  B���Bƙ�B���B�33B㙚BB�33C�C  C33C33C��C33C33C$L�C)� C.ffC3�C8ffC=L�CBffCG�CQ33C[  Ce��CoL�Cy� C���C�� C���C�� C���C�� C��3C�� C��fC���C�ffC��fC�ffC�C�� C�� C���Cֳ3CۦfC�� C�3C��CC�� C���D�3DٚD� D� D�3D��DٚD$� D)��D.ٚD3ٚD8�fD=ٚDB��DG� DLٚDQ��DVٚD[�fD`��De�fDj� DoٚDt�3DyٚD��D�c3D��fD��fD��D�VfD��3D��fD��D�\�D���D�� D��D�i�Dڰ D��fD��D�l�D� D�f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�ffA	��A^ffA�ffA���A���B��B��B0ffBC33BX  Bl  B�  B���B���B�  B���B�  B���Bř�B���B�33B♚B홚B�33C ��C� C
�3C�3C�C�3C�3C#��C)  C-�fC2��C7�fC<��CA�fCF��CP�3CZ� Ce�Cn��Cy  C�Y�C�� C�L�C�� C�Y�C�� C�s3C�� C�ffC�Y�C�&fC�ffC�&fC�L�Cǀ C̀ Cь�C�s3C�ffC�� C�s3C�L�C�Y�C� C�Y�D�3D��D� D� D�3D��D��D$� D)��D.��D3��D8�fD=��DB��DG� DL��DQ��DV��D[�fD`��De�fDj� Do��Dt�3Dy��D�	�D�S3D��fD��fD�	�D�FfD��3D��fD�	�D�L�D���D�� D��D�Y�Dڠ D��fD��D�\�D� D�f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��Ah�Ac�#AaK�Aa&�A_�A^-A[|�AV�uAKl�AF��A9�
A4��A2�A/��A/K�A,�A+�A)l�A%��A$v�A�AQ�A�#A�9A�AS�AXAE�A|�A�
Al�AS�A1A"�AZA��A��A��A�A&�AhsAM�A�/A�hAA�A�A(�A��Az�A
Q�A��AG�AO�@�O�@�I�@�C�@���@�n�@��@��@���@�&�@У�@ͩ�@�@��#@��@��9@�ƨ@��\@�b@�o@��@���@���@��@��@�@��@��!@��
@�X@~ȴ@{"�@w
=@sS�@pb@l�D@h�9@dz�@`��@]�-@ZM�@U�T@R��@Lz�@E�-@>�R@9��@4��@.E�@)��@$1@�;@��@�`@p�@�@$�@C�@	&�@$�@dZ@7L?�v�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Ah�Ac�#AaK�Aa&�A_�A^-A[|�AV�uAKl�AF��A9�
A4��A2�A/��A/K�A,�A+�A)l�A%��A$v�A�AQ�A�#A�9A�AS�AXAE�A|�A�
Al�AS�A1A"�AZA��A��A��A�A&�AhsAM�A�/A�hAA�A�A(�A��Az�A
Q�A��AG�AO�@�O�@�I�@�C�@���@�n�@��@��@���@�&�@У�@ͩ�@�@��#@��@��9@�ƨ@��\@�b@�o@��@���@���@��@��@�@��@��!@��
@�X@~ȴ@{"�@w
=@sS�@pb@l�D@h�9@dz�@`��@]�-@ZM�@U�T@R��@Lz�@E�-@>�R@9��@4��@.E�@)��@$1@�;@��@�`@p�@�@$�@C�@	&�@$�@dZ@7L?�v�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�BB�B�yB�B�B�B��B��B�B#�BdZB{�B�7B��B�'B��B�\B�BjBiyBG�B"�B�B�B)�B9XBC�BJ�B�\Br�BffBP�B9XB7LB$�B�B�BO�B�dBȴB�B��B�B�B
=BB	7BDB!�B�B�B�BJBB��B��B�B�B�sB�HB�;B�)B�B�B�B�B�#B�/B�HB�mB��BB�B'�B:^BJ�B[#BiyB|�B�DB��B��B�9B�}B��B�B�ZB�B��B	DB	�B	 �B	,B	9XB	C�B	W
B	k�B	�B	�bB	��B	�9B	B	��B	�`B	�B	��B

=B
�B
"�B
.B
49B
>wB
H�B
P�B
W
1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�TB�B�yB�B�B�B��B��B�B)�BffB|�B�=B��B�-B��B�bB�+Bk�Bk�BI�B"�B �B�B)�B9XBC�BI�B�bBs�BgmBR�B:^B8RB%�B�B�BL�B�dBȴB�B��B�B�B
=BB	7BDB"�B�B�B�BPBB��B��B�B�B�yB�NB�BB�/B�B�#B�#B�#B�)B�5B�NB�mB��BB�B'�B:^BJ�B[#BiyB|�B�DB��B��B�9B�}B��B�B�ZB�B��B	DB	�B	 �B	,B	9XB	C�B	W
B	k�B	�B	�bB	��B	�9B	B	��B	�`B	�B	��B

=B
�B
"�B
.B
49B
>wB
H�B
P�B
W
1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ = PRES-SP(NextCycle), where SP is SURFACE PRESSURE from next cycle                                                                                                                                                                                     TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,PRES_ADJ,elapsed_time,alpha,tau),elapsed_time=P/mean_rise_rate,P=dbar since the start of the profile for each samples                                                                                                       None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            SP(NextCycle)=0.5(dbar)                                                                                                                                                                                                                                         None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE; PRES_ADJ_ERR : Manufacture sensor accuracy                                                                                                                                                                 TEMP_ADJ_ERR : SBE sensor accuracy                                                                                                                                                                                                                              Salinity Recalculation using PRES_ADJ; PSAL_ADJ_ERR : max(sum of RecalS & CTM errors, 0.01(PSS-78))                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         200807191432192008071914321920080719143219200807191453352008071914533520080719145335200908190000002009081900000020090819000000  JA  ARFMdecpA9_b                                                                20080706132336  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.4                                                                 20080706132347  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20080706132349  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20080706132359  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8a                                                                20080706132359  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20080706132401  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20080706141207                      G�O�G�O�G�O�                JA  ARFMdecpA9_b                                                                20080710155428  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.4                                                                 20080710155433  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20080710155433  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20080710155437  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8a                                                                20080710155438  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20080710155438  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20080710190958                      G�O�G�O�G�O�                JA  ARFMdecpA9_b                                                                20080710155428  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.5                                                                 20090422045056  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.0                                                                 20090422045057  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20090422045057  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20090422045058  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20090422045058  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8b                                                                20090422045058  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8b                                                                20090422045058  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20090422045058  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20090422045647                      G�O�G�O�G�O�                JM  ARGQJMQC1.0                                                                 20080709225520  CV  DAT$            G�O�G�O�F��v                JM  ARCAJMQC1.0                                                                 20080719143219  CV  PRES            G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20080719143219  IP  PSAL            G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20080719145335  CV  PSAL            G�O�G�O�G�O�                JM  ARSQOW  1.1 SeHyD1.0                                                        20090819000000  IP  PSAL            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20090820074219  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20090820074324                      G�O�G�O�G�O�                JM      JMFC1.0                                                                 20150607150453                      G�O�G�O�G�O�                JA  ARDU                                                                        20150613142514                      G�O�G�O�G�O�                