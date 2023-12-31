CDF   !   
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PARAM       N_PROF        N_CALIB       N_LEVELS   s   	N_HISTORY             	   title         Argo float vertical profile    institution       JAMSTEC    source        
Argo float     history       b2007-09-20T18:59:04Z creation;2009-08-20T07:43:36Z update;2015-06-07T14:58:12Z conversion to V3.1;     
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
_FillValue                    iArgo profile    3.1 1.2 19500101000000  4900898 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               ,A   JA  20070920185904  20150613142519  A9_60144_044                    2C  D   APEX                            2414                            061305                          846 @ԖU��}|1   @ԖX��0�@C������d��v�1   ARGOS   A   A   A   Primary sampling: discrete [spot sampling CTD data]                                                                                                                                                                                                                @�ffA��Ad��A�ffA�ffA���B
��B33B0��BF  BW��Bm��B�  B���B���B�  B���B�  B�33B���B�ffB�ffB���B�  B���C  CL�C�C� C33C  CffC$L�C)�C.�C3�C8L�C<�fCBL�CG  CQ��C[� Ce33Co33CyL�C��fC���C��3C�� C�� C�s3C���C���C���C���C��fC�� C�� C¦fCǦfC�� Cь�C֦fCی�C�fC�3CꙚC� C�s3C��3D� D��DٚD��DٚD� D��D$ٚD)ٚD.ٚD3� D8��D=��DB� DGٚDL��DQ� DV�3D[� D`��De�3Dj� DoٚDt� DyٚD�)�D�ffD�� D���D��D�ffD���D��3D�0 D�l�D���D���D�&fD�s3DڦfD��D�  D�i�D�D�Y�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�  AffAY��A���A���A�33B  BffB.  BC33BT��Bj��B33B�ffB�ffB���B�ffB���B���B�ffB�  B�  B�ffB왚B�33C L�C��C
ffC��C� CL�C�3C#��C(ffC-ffC2ffC7��C<33CA��CFL�CP�fCZ��Cd� Cn� Cx��C�L�C�@ C�Y�C�&fC�&fC��C�@ C�33C�33C�@ C�L�C�&fC�ffC�L�C�L�C�ffC�33C�L�C�33C�L�C�Y�C�@ C�&fC��C�Y�D�3D��D��D� D��D�3D� D$��D)��D.��D3�3D8� D=� DB�3DG��DL��DQ�3DV�fD[�3D`� De�fDj�3Do��Dt�3Dy��D�3D�P D���D��fD�3D�P D��3D���D��D�VfD��fD��fD� D�\�Dڐ D��3D�	�D�S3D�3D�C31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A�  A�/A���A�I�Av�/Ai
=A[?}AM�AL�AD~�A:�HA6ȴA4��A2A�A0n�A.M�A+�;A*��A(ȴA'\)A&VA%A#��A!�TA�A��A��AG�A�A�\A|�A^5A�A��Ar�AbA5?A|�AȴA^5AG�AZAA33A�A��AS�AQ�A�#@�{@�Ĝ@���@�@��@�@��@�I�@�$�@�X@�&�@�M�@�l�@î@�b@���@��@�n�@��@���@��@�v�@���@���@���@��T@��H@��@�X@��@�@���@}O�@x�u@t��@q&�@n{@j=q@g\)@d�@`��@\z�@XQ�@T�@O��@H  @AX@:��@5�@1%@,�@(Ĝ@%�h@!��@�+@�H@�+@o@b@?}@	G�@�R@1@��?��;1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��A�  A�/A���A�I�Av�/Ai
=A[?}AM�AL�AD~�A:�HA6ȴA4��A2A�A0n�A.M�A+�;A*��A(ȴA'\)A&VA%A#��A!�TA�A��A��AG�A�A�\A|�A^5A�A��Ar�AbA5?A|�AȴA^5AG�AZAA33A�A��AS�AQ�A�#@�{@�Ĝ@���@�@��@�@��@�I�@�$�@�X@�&�@�M�@�l�@î@�b@���@��@�n�@��@���@��@�v�@���@���@���@��T@��H@��@�X@��@�@���@}O�@x�u@t��@q&�@n{@j=q@g\)@d�@`��@\z�@XQ�@T�@O��@H  @AX@:��@5�@1%@,�@(Ĝ@%�h@!��@�+@�H@�+@o@b@?}@	G�@�R@1@��?��;1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBcTBdZB� BB�DB49Bo�B[#BffB1BDB�B�B&�B1'B9XBI�BQ�BQ�BK�BJ�BT�BQ�BL�B<jB49B=qBG�BJ�BL�BN�B6FB/B/B'�B+B9XBE�BK�BH�B?}BE�BH�B:^B#�B(�B!�B�BhB	7BB��B��B��B�B�mB�TB�HB�)B�B�
B�B�;B�BB�HB�;B�TB�`B�B�B��BbB!�B/B=qBI�BXBaHBt�B�7B��B��B�XBɺB��B�NB�B��B	B	\B	�B	'�B	33B	?}B	L�B	dZB	x�B	�VB	��B	�'B	B	��B	�;B	�B	��B
B
uB
�B
)�B
33B
>wB
E�B
M�B
S�B
Y1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BcTBffB�B�
B��B<jBv�BaHBgmBJB\B�B�B'�B2-B:^BJ�BR�BR�BL�BK�BVBR�BM�B=qB49B=qBG�BJ�BL�BO�B7LB/B0!B'�B+B9XBE�BK�BI�B?}BE�BI�B;dB$�B)�B!�B�BoB
=BB��B��B��B�B�sB�ZB�NB�/B�B�
B�B�BB�BB�HB�BB�TB�`B�B�B��BbB!�B0!B=qBI�BXBaHBt�B�7B��B��B�XBɺB��B�NB�B��B	B	\B	�B	'�B	33B	?}B	L�B	dZB	x�B	�VB	��B	�'B	B	��B	�;B	�B	��B
B
uB
�B
)�B
33B
>wB
E�B
M�B
S�B
Y1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<��
<�h<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ = PRES-SP(NextCycle), where SP is SURFACE PRESSURE from next cycle                                                                                                                                                                                     TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,PRES_ADJ,elapsed_time,alpha,tau),elapsed_time=P/mean_rise_rate,P=dbar since the start of the profile for each samples                                                                                                       None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            SP(NextCycle)=0.7(dbar)                                                                                                                                                                                                                                         None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE; PRES_ADJ_ERR : Manufacture sensor accuracy                                                                                                                                                                 TEMP_ADJ_ERR : SBE sensor accuracy                                                                                                                                                                                                                              Salinity Recalculation using PRES_ADJ; PSAL_ADJ_ERR : max(sum of RecalS & CTM errors, 0.01(PSS-78))                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         200710031338432007100313384320071003133843200710031344432007100313444320071003134443200908190000002009081900000020090819000000  JA  ARFMdecpA9_b                                                                20070920185901  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.4                                                                 20070920185904  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20070920185905  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20070920185909  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.7c                                                                20070920185909  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16a                                                                20070920185909  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20070920191103                      G�O�G�O�G�O�                JA  ARFMdecpA9_b                                                                20070924155445  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.4                                                                 20070924155449  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20070924155450  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20070924155454  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.7c                                                                20070924155454  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16a                                                                20070924155454  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20070924160201                      G�O�G�O�G�O�                JA  ARFMdecpA9_b                                                                20070924155445  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.5                                                                 20090422044952  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.0                                                                 20090422044952  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20090422044952  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20090422044953  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20090422044953  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8b                                                                20090422044954  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8b                                                                20090422044954  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20090422044954  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20090422045633                      G�O�G�O�G�O�                JM  ARGQJMQC1.0                                                                 20070924000000  CV  DAT$            G�O�G�O�F���                JM  ARCAJMQC1.0                                                                 20071003133843  CV  PRES            G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20071003133843  CV  PSAL            G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20071003134443  CV  PSAL            G�O�G�O�G�O�                JM  ARSQOW  1.1 SeHyD1.0                                                        20090819000000  IP  PSAL            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20090820074203  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20090820074336                      G�O�G�O�G�O�                JM      JMFC1.0                                                                 20150607145804                      G�O�G�O�G�O�                JA  ARDU                                                                        20150613142519                      G�O�G�O�G�O�                