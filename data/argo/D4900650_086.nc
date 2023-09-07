CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PARAM       N_PROF        N_CALIB       N_LEVELS   s   	N_HISTORY             	   title         Argo float vertical profile    institution       JAMSTEC    source        
Argo float     history       b2007-12-07T18:52:21Z creation;2009-03-18T00:38:26Z update;2015-06-07T02:29:21Z conversion to V3.1;     
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
_FillValue                    iArgo profile    3.1 1.2 19500101000000  4900650 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               VA   JA  20071207185221  20150613102517  A5_24111_086                    2C  D   APEX                            1717                            013004                          846 @ԩ�dPg-1   @ԩ�z�H@DbI�^5�dp��
=1   ARGOS   A   A   A   Primary sampling: discrete [spot sampling CTD data]                                                                                                                                                                                                                @�  A  Aa��A���A�ffA���B	33B��B133BC33BW33BnffB���B���B���B�  B���B���B���Bƙ�B�33B�  B�ffB���B�33C� CL�CffC�CffCL�CffC$ffC)� C.�C3ffC833C=  CA�fCF�fCQ33C[�CeL�Co�Cy  C�s3C�� C�� C���C���C�� C��3C��fC���C���C���C��fC�� C³3CǙ�C̙�Cљ�C�s3C�Y�C���C噚C��C�3C��3C���D��D� D��D�fDٚD�3D�fD$� D)ٚD.��D3��D8ٚD=� DB�fDGٚDL� DQ�fDV� D[� D`��De�fDj�3Do��Dt�3Dy�3D�  D�l�D��fD���D�  D�l�D��fD��D�  D�i�D��fD���D��D�ffDڣ3D�� D�  D�VfD�3D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @���A��A^ffA�33A���A�33BffB  B0ffBBffBVffBm��B�33B�ffB�33B���B�33B�33B�33B�33B���Bڙ�B�  B�ffB���CL�C�C33C�fC33C�C33C$33C)L�C-�fC333C8  C<��CA�3CF�3CQ  CZ�fCe�Cn�fCx��C�Y�C�ffC�ffC�s3C�s3C��fC���C���C��3C�� C�� C���C��fC�Cǀ C̀ Cр C�Y�C�@ C�� C� C�s3CC���C��3D� D�3D��D��D��D�fD��D$�3D)��D.� D3��D8��D=�3DB��DG��DL�3DQ��DV�3D[�3D`� De��Dj�fDo� Dt�fDy�fD��D�ffD�� D��fD��D�ffD�� D��3D��D�c3D�� D��fD�fD�` Dڜ�D�ٚD��D�P D��D��f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A<9XA<1'A<M�A<M�A<M�A<5?A<E�A<M�A<z�A<�+A<�9A<��A<�A=
=A=�PA0  A$~�A��A �9A $�A��A�HA"1'A�;A��AhsAK�Al�AĜA�-A�HA�jA"�A��A��AJA��AO�AVA`BA�A;dAz�A�A�TA�TA
ffA�PA^5A�TA  �@�&�@�9X@���@�dZ@噚@߅@�ƨ@�p�@ҧ�@́@�1@��m@��h@�J@��@��m@��9@�hs@���@�~�@��-@���@�r�@�Z@��#@���@�V@���@��j@�@�C�@�7L@|�@yX@u`B@p��@l��@gl�@b�@_�P@]�@Y�@W\)@T�@Nv�@G�;@@��@;o@6@.��@)�@$�/@ bN@ƨ@bN@`B@�7@�+@"�@�@@��@x�?��R1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A<9XA<1'A<M�A<M�A<M�A<5?A<E�A<M�A<z�A<�+A<�9A<��A<�A=
=A=�PA0  A$~�A��A �9A $�A��A�HA"1'A�;A��AhsAK�Al�AĜA�-A�HA�jA"�A��A��AJA��AO�AVA`BA�A;dAz�A�A�TA�TA
ffA�PA^5A�TA  �@�&�@�9X@���@�dZ@噚@߅@�ƨ@�p�@ҧ�@́@�1@��m@��h@�J@��@��m@��9@�hs@���@�~�@��-@���@�r�@�Z@��#@���@�V@���@��j@�@�C�@�7L@|�@yX@u`B@p��@l��@gl�@b�@_�P@]�@Y�@W\)@T�@Nv�@G�;@@��@;o@6@.��@)�@$�/@ bN@ƨ@bN@`B@�7@�+@"�@�@@��@x�?��R1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBVBVBXBXBW
BT�BVBVB[#B\)B^5BcTBffBiyBq�B��B�/B�XB��B�B�^B�HB�BB	7BPBJB�sBB��B��BBVB�B%B�BB��B��B��BJBDBoB�B�B�BhBDB+B��B�B��B��B�mB�;B�B��B�B�
B�
B�B��B�B�B�/B�/B�NB�B�B�B��BJB�B+B;dBD�BW
BffBr�B�B�\B��B�B�qBɺB�B�`B�B	B	\B	�B	#�B	.B	6FB	>wB	T�B	gmB	|�B	�VB	��B	�?B	ǮB	��B	�ZB	�B
  B
VB
�B
%�B
1'B
;dB
D�B
K�B
Q�B
Y1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BVBVBXBXBW
BT�BVBVB[#B\)B^5BcTBffBiyBx�B��B�;B�XB��B�B�XB�;B�BB	7BPBVB�sBB��B��BBVB�B+B�BB��B��B��BJBDBoB�B�B�BoBDB1B��B�B��B��B�sB�BB�B�B�#B�
B�B�B��B�B�B�/B�/B�NB�B�B�B��BJB�B+B;dBD�BW
BffBr�B�B�\B��B�B�qBɺB�B�`B�B	B	\B	�B	#�B	.B	6FB	>wB	T�B	gmB	|�B	�VB	��B	�?B	ǮB	��B	�ZB	�B
  B
VB
�B
%�B
1'B
;dB
D�B
K�B
Q�B
Y1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - SP, where SP is SURFACE PRESSURE (minus 5 dbar for Apf-6,7,8) from next cycle.                                                                                                                                                           none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = RecalS = PSAL(PRES_ADJUSTED,TEMP,Conductivity)                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = celltm_sbe41(RecalS,TEMP,PRES_ADJUSTED,elapsed_time,alpha,tau),elapsed_time=P/mean_rise_rate,P= dbar since the start of the profile for each samples.                                                                                           none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            SP(NextCycle) = 0.2 dbar                                                                                                                                                                                                                                        none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using reported SURFACE PRESSURE. The quoted error is max [2.4, size of pressure adjustment] in dbar.                                                                                                                                      The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Salinity Recalculation using PRES_ADJUSTED. PSAL_ADJ_ERR : SBE sensor accuracy & CTM adjustment                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         200712221355272007122213552720071222135527200712221413242007122214132420071222141324200809050000002008090500000020080905000000  JA  ARFMdecpA5_a                                                                20071207185219  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.4                                                                 20071207185221  IP                  G�O�G�O�G�O�                JA  ARCArsal2.1                                                                 20071207185222  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20071207185222  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20071207185226  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20071207185226  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.7c                                                                20071207185226  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.7c                                                                20071207185226  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20071207185227  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20071207190809                      G�O�G�O�G�O�                JA  ARFMdecpA5_a                                                                20071211160145  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.4                                                                 20071211160156  IP                  G�O�G�O�G�O�                JA  ARCArsal2.1                                                                 20071211160157  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20071211160157  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20071211160202  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20071211160202  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.7c                                                                20071211160202  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.7c                                                                20071211160202  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20071211160202  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20071211191028                      G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20071222135527  CV  PRES            G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20071222135527  IP  PSAL            G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20071222141324  CV  PSAL            G�O�G�O�G�O�                JM  ARSQWJO 2.0 SeHyD1.0                                                        20080905000000  IP  PSAL            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20080925024522  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20080925060526                      G�O�G�O�G�O�                JM  AREVjmrvp1.2                                                                20090312112536  IP  PRES            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20090318003509  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20090318003826                      G�O�G�O�G�O�                JM      JMFC1.0                                                                 20150607022915                      G�O�G�O�G�O�                JA  ARDU                                                                        20150613102517                      G�O�G�O�G�O�                