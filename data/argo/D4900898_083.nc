CDF   !   
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PARAM       N_PROF        N_CALIB       N_LEVELS   s   	N_HISTORY             	   title         Argo float vertical profile    institution       JAMSTEC    source        
Argo float     history       b2008-10-14T18:57:05Z creation;2009-08-20T07:43:18Z update;2015-06-07T15:07:31Z conversion to V3.1;     
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
_FillValue                    iArgo profile    3.1 1.2 19500101000000  4900898 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               SA   JA  20081014185705  20150613142511  A9_60144_083                    2C  D   APEX                            2414                            061305                          846 @���6͎�1   @���6�a@C��S����d�E���1   ARGOS   A   A   A   Primary sampling: discrete [spot sampling CTD data]                                                                                                                                                                                                                @���AffAa��A���A���A���B33B��B0��BE33BXffBn  B�ffB���B�  B���B���B���B�33B���B�ffB�  B�ffB�ffB�ffC33CL�CffCL�CffC�C�fC$� C(�fC.  C333C8ffC=  CA�3CGL�CQ33CZ�3Ce  Co33Cx��C�s3C��fC��fC�� C�� C���C���C��fC��fC���C���C��fC�s3C�C�s3C̙�CѦfC֙�C۳3C�3C噚C��CC��C���D� DٚDٚD� D�3DٚD� D$�3D)��D.�3D3ٚD8�fD=�3DB�fDGٚDL� DQ��DV�3D[ٚD`� DeٚDj��Do�3Dt��Dy� D�)�D�i�D�� D��3D�&fD�Y�D���D�� D�  D�i�D�� D��fD�&fD�l�DڦfD���D��D�\�D�D�` 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�ffA33AVffA�  A�  A�33BffB  B.  BBffBU��Bk33B�  B�33B���B�33B�33B�33B���B�ffB�  Bٙ�B�  B�  B�  C � C��C
�3C��C�3CffC33C#��C(33C-L�C2� C7�3C<L�CA  CF��CP� CZ  CdL�Cn� Cx�C��C�L�C�L�C�ffC�ffC�33C�@ C�L�C�L�C�@ C�@ C�L�C��C�@ C��C�@ C�L�C�@ C�Y�C�Y�C�@ C�33C�@ C�33C�33D�3D��D��D�3D�fD��D�3D$�fD)� D.�fD3��D8��D=�fDB��DG��DL�3DQ��DV�fD[��D`�3De��Dj� Do�fDt� Dy�3D�3D�S3D���D���D� D�C3D��fD�ɚD�	�D�S3D���D�� D� D�VfDڐ D��fD�3D�FfD�3D�I�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�%A���A��+A�~�A�v�A�S�A�K�Ay��A`z�AN�AF-A?p�A<Q�A89XA6{A2�A4��A6A3�;A3\)A2r�A1G�A/��A.=qA-��A,�`A+%A+�A);dA(z�A((�A'S�A&�A&��A&^5A&��A'%A%�7A"�\A"1A!��A7LA��A�TAp�A�FA�7A/A��An�A
�yA��A�uA�@��w@��@�1@�?}@�p�@���@߅@۶F@��@ѩ�@��T@ʗ�@Ə\@���@�X@�n�@�9X@�;d@�/@��;@���@�~�@��h@��7@�n�@��@�  @��/@�-@�I�@{33@v�@t(�@o�;@k@gK�@c�F@`b@\��@Yhs@U@Mp�@F{@?�@9��@3o@-��@(r�@$Z@ b@1@%@�T@�@v�@��@	��@v�@ƨ@ r�?���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�%A���A��+A�~�A�v�A�S�A�K�Ay��A`z�AN�AF-A?p�A<Q�A89XA6{A2�A4��A6A3�;A3\)A2r�A1G�A/��A.=qA-��A,�`A+%A+�A);dA(z�A((�A'S�A&�A&��A&^5A&��A'%A%�7A"�\A"1A!��A7LA��A�TAp�A�FA�7A/A��An�A
�yA��A�uA�@��w@��@�1@�?}@�p�@���@߅@۶F@��@ѩ�@��T@ʗ�@Ə\@���@�X@�n�@�9X@�;d@�/@��;@���@�~�@��h@��7@�n�@��@�  @��/@�-@�I�@{33@v�@t(�@o�;@k@gK�@c�F@`b@\��@Yhs@U@Mp�@F{@?�@9��@3o@-��@(r�@$Z@ b@1@%@�T@�@v�@��@	��@v�@ƨ@ r�?���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B�B�B�5B�;B�BB�#BD�B��B�BB&�BL�B:^B7LB �BS�Br�Bl�Bl�Bl�BffB\)BZBZBW
BH�BQ�BH�BD�BI�BG�BI�BT�B`BB}�B�7B�Bn�Bu�B�Bv�BiyB_;BR�BJ�B?}B6FB,B%�B'�B�BbBB��B�B�`B�fB�mB�fB�fB�TB�TB�ZB�NB�HB�ZB�`B�yB�B��B  BJB�B)�B6FBF�BS�BbNBr�B�B�uB��B�!B��B��B�B�fB��B	B	VB	�B	#�B	/B	9XB	S�B	jB	~�B	�hB	��B	�?B	ƨB	��B	�TB	�B	��B
+B
uB
!�B
+B
5?B
@�B
H�B
R�B
Z1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B�B�B�5B�;B�BB�fBQ�B�B�5BB(�BN�B;dB9XB�BS�Bs�Bl�Bm�Bm�BgmB]/BZB[#BXBH�BR�BH�BD�BJ�BG�BI�BT�B`BB}�B�=B�Bn�Bu�B�Bw�BjB`BBS�BK�B@�B7LB-B%�B(�B�BhBB��B�B�`B�mB�sB�mB�mB�ZB�ZB�`B�NB�NB�`B�fB�yB�B��B  BJB�B)�B6FBF�BS�BbNBr�B�B�uB��B�!B��B��B�B�fB��B	B	VB	�B	#�B	/B	9XB	S�B	jB	~�B	�hB	��B	�?B	ƨB	��B	�TB	�B	��B
+B
uB
!�B
+B
5?B
@�B
H�B
R�B
Z1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<49X<T��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ = PRES-SP(NextCycle), where SP is SURFACE PRESSURE from next cycle                                                                                                                                                                                     TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,PRES_ADJ,elapsed_time,alpha,tau),elapsed_time=P/mean_rise_rate,P=dbar since the start of the profile for each samples                                                                                                       None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            SP(NextCycle)=0.7(dbar)                                                                                                                                                                                                                                         None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE; PRES_ADJ_ERR : Manufacture sensor accuracy                                                                                                                                                                 TEMP_ADJ_ERR : SBE sensor accuracy                                                                                                                                                                                                                              Salinity Recalculation using PRES_ADJ; PSAL_ADJ_ERR : max(sum of RecalS & CTM errors, 0.01(PSS-78))                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         200810271420332008102714203320081027142033200810271433292008102714332920081027143329200908190000002009081900000020090819000000  JA  ARFMdecpA9_b                                                                20081014185702  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.4                                                                 20081014185705  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20081014185706  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20081014185710  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8a                                                                20081014185710  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20081014185710  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20081014191932                      G�O�G�O�G�O�                JA  ARFMdecpA9_b                                                                20081018163752  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.4                                                                 20081018163808  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20081018163812  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20081018163820  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8a                                                                20081018163821  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20081018163822  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20081018203751                      G�O�G�O�G�O�                JA  ARFMdecpA9_b                                                                20081018163752  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.5                                                                 20090422045119  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.0                                                                 20090422045119  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20090422045119  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20090422045120  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20090422045120  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8b                                                                20090422045120  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8b                                                                20090422045120  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20090422045121  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20090422045656                      G�O�G�O�G�O�                JM  ARGQJMQC1.0                                                                 20081018012541  CV  DAT$            G�O�G�O�F��v                JM  ARCAJMQC1.0                                                                 20081027142033  CV  PRES            G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20081027142033  IP  PSAL            G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20081027143329  CV  PSAL            G�O�G�O�G�O�                JM  ARSQOW  1.1 SeHyD1.0                                                        20090819000000  IP  PSAL            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20090820074209  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20090820074318                      G�O�G�O�G�O�                JM      JMFC1.0                                                                 20150607150723                      G�O�G�O�G�O�                JA  ARDU                                                                        20150613142511                      G�O�G�O�G�O�                