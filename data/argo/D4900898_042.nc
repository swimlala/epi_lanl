CDF   !   
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PARAM       N_PROF        N_CALIB       N_LEVELS   s   	N_HISTORY             	   title         Argo float vertical profile    institution       JAMSTEC    source        
Argo float     history       b2007-08-31T18:58:34Z creation;2009-08-20T07:43:28Z update;2015-06-07T14:57:44Z conversion to V3.1;     
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
_FillValue                    iArgo profile    3.1 1.2 19500101000000  4900898 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               *A   JA  20070831185834  20150613142517  A9_60144_042                    2C  D   APEX                            2414                            061305                          846 @ԑU}X]�1   @ԑW;�GK@C��hr��d��Q�1   ARGOS   A   A   A   Primary sampling: discrete [spot sampling CTD data]                                                                                                                                                                                                                @���A  Ak33A�  A�33A�33B	��B33B2ffBE33BY33Bn  B�ffB���B�  B�33B�33B�  B�  B���B�  B�33B䙚B�33B�  C�CL�C
�fC33C  CL�C�C$ffC)�C-��C333C8  C<��CA��CF��CQ  C[ffCe33CoffCyffC��3C���C��3C��3C��fC�� C�� C��3C���C��3C���C�s3C�� C³3CǦfC�� C���C�� Cی�C�� C�� CꙚC��C��fC���D� D��D� D� D�fD�fDٚD$��D)ٚD.�3D3� D8� D=�fDB� DG�fDL��DQ��DV��D[��D`�3DeٚDj�fDo��Dt�3Dy��D�)�D�p D���D�� D�)�D�c3D��3D��D�,�D�s3D���D��fD��D�l�Dڣ3D���D�&fD�ffD��D��31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @���AffAa��A�33A�ffA�ffB33B��B0  BB��BV��Bk��B~ffB���B���B�  B�  B���B���Bř�B���B�  B�ffB�  B���C � C�3C
L�C��CffC�3C� C#��C(� C-33C2��C7ffC<33CA33CF33CPffCZ��Cd��Cn��Cx��C�ffC�L�C�ffC�ffC�Y�C�33C�33C�ffC�@ C�ffC�@ C�&fC�33C�ffC�Y�C�s3Cр C�s3C�@ C�33C�s3C�L�C�@ C�Y�C�L�D��D�fD��D��D� D� D�3D$�3D)�3D.��D3��D8��D=� DB��DG� DL�3DQ�fDV�3D[�fD`��De�3Dj� Do�fDt��Dy�fD�fD�\�D���D���D�fD�P D�� D��fD��D�` D���D��3D�	�D�Y�Dڐ D�ٚD�3D�S3D�D�� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A���A��FA���A�/A�Al�uA^�jAW�FAI�hABjA7��A-�mA,ȴA'��A$�uA"��A"�HA!�;A#�PA#��A#`BA"�RA"�`A!A ��A!�A �yA ��A�AoA��A-AdZAS�A�AjAz�A�A��A1'A�RA��AK�A�RA��A�7A�A
bAS�A�A�jA 1'@���@�@�@�K�@��@�O�@��#@��m@�"�@�S�@Ǿw@Ý�@�ƨ@���@��D@�+@���@�V@�ff@�n�@�dZ@���@���@��R@��@���@��F@���@���@�%@~{@z��@u?}@p�u@lZ@h  @ct�@_�@[o@XQ�@U/@Qhs@L9X@E�@?|�@8�`@2n�@-?}@)�@%?}@ A�@�m@|�@I�@X@O�@o@�u@V@�F@%?�5?1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A���A���A��FA���A�/A�Al�uA^�jAW�FAI�hABjA7��A-�mA,ȴA'��A$�uA"��A"�HA!�;A#�PA#��A#`BA"�RA"�`A!A ��A!�A �yA ��A�AoA��A-AdZAS�A�AjAz�A�A��A1'A�RA��AK�A�RA��A�7A�A
bAS�A�A�jA 1'@���@�@�@�K�@��@�O�@��#@��m@�"�@�S�@Ǿw@Ý�@�ƨ@���@��D@�+@���@�V@�ff@�n�@�dZ@���@���@��R@��@���@��F@���@���@�%@~{@z��@u?}@p�u@lZ@h  @ct�@_�@[o@XQ�@U/@Qhs@L9X@E�@?|�@8�`@2n�@-?}@)�@%?}@ A�@�m@|�@I�@X@O�@o@�u@V@�F@%?�5?1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBffBe`BaHB{BuB;dBC�B�B��B�%B�B�B��B��B��B��B�{B��B��BɺB�ZB�yB�B��B�B�BBB1BBB+B1BBDBJBoB �B(�B+B8RB@�BC�B@�B9XB5?B+B$�B�B�BuBPBB��B�B�B�B�`B�HB�5B�B�B��B�#B�#B�5B�;B�NB�NB�fB��B1BuB�B+B?}BN�B\)Bp�B� B�DB��B�B�RBB��B�fB��B	B	hB	�B	)�B	49B	<jB	H�B	YB	l�B	~�B	��B	�B	�}B	��B	�;B	�B
  B
VB
�B
#�B
1'B
8RB
@�B
G�B
N�B
VB
Z1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BffBffB}�B�B�BI�BJ�B�B�B�=B�3B�9B��B��B�B��B�{B��B��BɺB�ZB�yB�B��B�B�BBB1BBB+B1BBDBJBoB �B(�B+B8RBA�BD�BA�B:^B6FB+B%�B�B�B{BVB%B��B�B�B�B�fB�HB�;B�B�
B��B�)B�)B�5B�BB�TB�NB�fB��B1BuB�B+B?}BN�B\)Bp�B� B�DB��B�B�RBB��B�fB��B	B	hB	�B	)�B	49B	<jB	H�B	YB	l�B	~�B	��B	�B	�}B	��B	�;B	�B
  B
VB
�B
#�B
1'B
8RB
@�B
G�B
N�B
VB
Z1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<�`B<#�
<D��<e`B<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ = PRES-SP(NextCycle), where SP is SURFACE PRESSURE from next cycle                                                                                                                                                                                     TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,PRES_ADJ,elapsed_time,alpha,tau),elapsed_time=P/mean_rise_rate,P=dbar since the start of the profile for each samples                                                                                                       None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            SP(NextCycle)=0.6(dbar)                                                                                                                                                                                                                                         None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE; PRES_ADJ_ERR : Manufacture sensor accuracy                                                                                                                                                                 TEMP_ADJ_ERR : SBE sensor accuracy                                                                                                                                                                                                                              Salinity Recalculation using PRES_ADJ; PSAL_ADJ_ERR : max(sum of RecalS & CTM errors, 0.01(PSS-78))                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         200709130850062007091308500620070913085006200709130908552007091309085520070913090855200908190000002009081900000020090819000000  JA  ARFMdecpA9_b                                                                20070831185831  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.4                                                                 20070831185834  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20070831185835  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20070831185838  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.7c                                                                20070831185839  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16a                                                                20070831185839  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20070831190954                      G�O�G�O�G�O�                JA  ARFMdecpA9_b                                                                20070904155900  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.4                                                                 20070904155904  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20070904155904  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20070904155908  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.7c                                                                20070904155909  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16a                                                                20070904155909  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20070904160608                      G�O�G�O�G�O�                JA  ARFMdecpA9_b                                                                20070904155900  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.5                                                                 20090422044947  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.0                                                                 20090422044948  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20090422044948  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20090422044949  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20090422044949  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8b                                                                20090422044949  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8b                                                                20090422044949  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20090422044949  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20090422045638                      G�O�G�O�G�O�                JM  ARGQJMQC1.0                                                                 20070903232627  CV  DAT$            G�O�G�O�F���                JM  ARCAJMQC1.0                                                                 20070913085006  CV  PRES            G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20070913085006  IP  PSAL            G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20070913090855  CV  PSAL            G�O�G�O�G�O�                JM  ARSQOW  1.1 SeHyD1.0                                                        20090819000000  IP  PSAL            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20090820074154  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20090820074328                      G�O�G�O�G�O�                JM      JMFC1.0                                                                 20150607145736                      G�O�G�O�G�O�                JA  ARDU                                                                        20150613142517                      G�O�G�O�G�O�                