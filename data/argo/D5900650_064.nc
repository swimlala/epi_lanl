CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PARAM       N_PROF        N_CALIB       N_LEVELS   i   	N_HISTORY             	   title         Argo float vertical profile    institution       JAMSTEC    source        
Argo float     history       b2006-06-14T04:50:22Z creation;2009-03-18T07:30:50Z update;2015-06-09T20:05:40Z conversion to V3.1;     
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
_FillValue                  l  ;D   PRES_ADJUSTED         	         	   	long_name         )Sea water pressure, equals 0 at sea-level      
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   standard_name         sea_water_pressure       �  ;�   PRES_ADJUSTED_QC      	            	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  l  =T   PRES_ADJUSTED_ERROR       	            	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  =�   TEMP      	         	   	long_name         $Sea temperature in-situ ITS-90 scale   
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_temperature        �  ?d   TEMP_QC       	            	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  l  A   TEMP_ADJUSTED         	         	   	long_name         $Sea temperature in-situ ITS-90 scale   
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_temperature        �  At   TEMP_ADJUSTED_QC      	            	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  l  C   TEMP_ADJUSTED_ERROR       	            	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  C�   PSAL      	         	   	long_name         Practical salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_salinity       �  E(   PSAL_QC       	            	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  l  F�   PSAL_ADJUSTED         	         	   	long_name         Practical salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_salinity       �  G8   PSAL_ADJUSTED_QC      	            	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  l  H�   PSAL_ADJUSTED_ERROR       	            	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  IH   	PARAMETER         	   
               	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  J�   SCIENTIFIC_CALIB_EQUATION         	   
               	long_name         'Calibration equation for this parameter    
_FillValue                 	   K|   SCIENTIFIC_CALIB_COEFFICIENT      	   
               	long_name         *Calibration coefficients for this equation     
_FillValue                 	   T|   SCIENTIFIC_CALIB_COMMENT      	   
               	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ]|   SCIENTIFIC_CALIB_DATE         	   
                	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  f|   HISTORY_INSTITUTION          	            	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    f�   HISTORY_STEP         	            	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    g    HISTORY_SOFTWARE         	            	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    g   HISTORY_SOFTWARE_RELEASE         	            	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    g   HISTORY_REFERENCE            	            	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  g   HISTORY_DATE         	             	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    gL   HISTORY_ACTION           	            	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    g\   HISTORY_PARAMETER            	            	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    g`   HISTORY_START_PRES           	         	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         gp   HISTORY_STOP_PRES            	         	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         gt   HISTORY_PREVIOUS_VALUE           	         	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        gx   HISTORY_QCTEST           	            	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    g|Argo profile    3.1 1.2 19500101000000  5900650 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               @A   JA  20060614045022  20150614054514  A5_23632_064                    2C  D   APEX                            1557                            013004                          846 @�"}~�/1   @�"�=@v@4{��S���c�&�x��1   ARGOS   A   A   A   Primary sampling: discrete [spot sampling CTD data]                                                                                                                                                                                                                @�  A��AfffA���A���A�  B	��B��B2��BF��BZ  Bm33B�  B�ffB���B���B�  B���B���Bƙ�BЙ�B���B㙚B�  B�ffC� C�fCffC�fC�CffCffC$��C)33C.� C3L�C833C=33CB33CG  CQ  C[33Ce33CoffCyffC��3C��fC�� C��fC��fC���C��3C��fC��3C��fC��fC��fC��3C Cǀ C̦fC�s3Cֳ3C۳3C���C�� C�3C��C�� C���D��DٚD�3D��D��D�fD��D$�fD)��D.��D3�fD8��D=ٚDB� DG�fDL��DQ� DV��D[�fD`� DeٚDj��Do�fDt� Dy�fD�,�D�p D���D�� D��D�VfD��fD���D�&fD�0 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�ffA��Aa��A�ffA�ffA陚BffB��B1��BE��BX��Bl  B�ffB���B�  B�  B�ffB�33B�  B�  B�  B�33B�  B�ffB���C33C��C�C��C��C�C�C$L�C(�fC.33C3  C7�fC<�fCA�fCF�3CP�3CZ�fCd�fCo�Cy�C���C�� C���C�� C�� C�ffC���C�� C���C�� C�� C�� C���C�Y�C�Y�C̀ C�L�C֌�Cی�C�fC噚C��C�ffC���C�s3D�fD�fD� D��D��D�3D��D$�3D)��D.�fD3�3D8��D=�fDB��DG�3DL��DQ��DV��D[�3D`��De�fDj��Do�3Dt��Dy�3D�#3D�ffD�� D��fD�3D�L�D���D��3D��D�&f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�9XA�5?A�{A��A��#A���A�A�A�%A�/A��/A�ȴA��7A�O�A��A�^5A��HA�K�A��A�ffA���A�C�A�bA�M�A�JA���A��mA�z�A�A�A��A�^5A�
=A��A��hA��RA���A�A� �A��A�x�A��FA��A�?}A{�As�hA]��AUVAN�AEK�A@jA;33A1VA( �A%�A"�A��AXA��A��A$�A1A�uA�hA$�A�R@��T@���@�;d@�~�@ڧ�@�`B@�;d@��#@���@�Q�@��^@���@�n�@��^@��`@���@�|�@��D@��^@�5?@��@��@�=q@��\@�7L@�j@��R@���@� �@���@�C�@�A�@��#@xr�@rJ@j�H@f��@Z�@L�D@E��@9�#111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�9XA�5?A�{A��A��#A���A�A�A�%A�/A��/A�ȴA��7A�O�A��A�^5A��HA�K�A��A�ffA���A�C�A�bA�M�A�JA���A��mA�z�A�A�A��A�^5A�
=A��A��hA��RA���A�A� �A��A�x�A��FA��A�?}A{�As�hA]��AUVAN�AEK�A@jA;33A1VA( �A%�A"�A��AXA��A��A$�A1A�uA�hA$�A�R@��T@���@�;d@�~�@ڧ�@�`B@�;d@��#@���@�Q�@��^@���@�n�@��^@��`@���@�|�@��D@��^@�5?@��@��@�=q@��\@�7L@�j@��R@���@� �@���@�C�@�A�@��#@xr�@rJ@j�H@f��@Z�@L�D@E��@9�#111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
�)B
�/B
�5B
�/B�BT�B�{B�9BbBXB�1B��B�'B�dBŢBB��B�jB�FB�RB�FB�^B�!B�wB��BĜB�BB��B�fB�)B��B�BŢB��B�BbB�LB}�BK�B-B
�TB
e`B	�B	��B	hB�;BB��B��B�B�B{�B|�B{�B�+B�+B�+B��B��B�FB�B��B��B��B��B�BB��B�!B�qB�XB�B�B	�B	1'B	<jB	=qB	VB	~�B	��B	��B	��B	��B	�B	�FB	�wB	B	��B	�NB	�fB	�B	�B	�B	��B	��B	��B	��B

=B
�B
�B
!�B
%�B
1'B
?}B
D�B
O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
�)B
�/B
�5B
�HB�BVB��B�?BuBZB�7B��B�'B�dBŢBÖB��B�jB�LB�RB�LB�dB�'B�wB��BŢB�HB��B�mB�)B��B�BȴB��B�1BuB�^B�BN�B0!B
�fB
iyB	�B	�B	uB�HBĜB��B��B�+B�B|�B}�B}�B�1B�1B�7B��B��B�FB�B��B��B��B��B�HB��B�!B�wB�XB�B�B	�B	1'B	<jB	>wB	VB	~�B	��B	��B	��B	��B	�B	�FB	�wB	B	��B	�NB	�fB	�B	�B	�B	��B	��B	��B	��B

=B
�B
�B
!�B
%�B
1'B
?}B
D�B
O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - SP, where SP is SURFACE PRESSURE (minus 5 dbar for Apf-6,7,8) from next cycle.                                                                                                                                                           none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = RecalS = PSAL(PRES_ADJUSTED,TEMP,Conductivity)                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = celltm_sbe41(RecalS,TEMP,PRES_ADJUSTED,elapsed_time,alpha,tau),elapsed_time=P/mean_rise_rate,P= dbar since the start of the profile for each samples.                                                                                           none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            SP(NextCycle) = 0.3 dbar                                                                                                                                                                                                                                        none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using reported SURFACE PRESSURE. The quoted error is max [2.4, size of pressure adjustment] in dbar.                                                                                                                                      The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Salinity Recalculation using PRES_ADJ; PSAL_ADJ_ERR : max(sum of RecalS & CTM errors, 0.01(PSS-78))                                                                                                                                                             none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         200706281404262007062814042620070628140426200706290309222007062903092220070629030922200707020000002007070200000020070702000000  JA  ARFMfmtp2.2                                                                 20060614045022  IP                  G�O�G�O�G�O�                JA  ARCAsspa2.0                                                                 20060614045023  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcp2.4                                                                 20060614045023  QCP$                G�O�G�O�G�O�           3F6BCJA  ARUP                                                                        20060614045735                      G�O�G�O�G�O�                JA  ARFMfmtp2.2                                                                 20060618034326  IP                  G�O�G�O�G�O�                JA  ARCAsspa2.1                                                                 20060618034326  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcp2.5                                                                 20060618034327  QCP$                G�O�G�O�G�O�           1FB7CJA  ARGQaqcp2.5                                                                 20060618034327  QCP$                G�O�G�O�G�O�           1FB40JA  ARGQrelo2.1                                                                 20060618034327  CV  TIME            G�O�G�O�F�
                JA  ARGQrelo2.1                                                                 20060618034327  CV  LAT$            G�O�G�O�A��/                JA  ARGQrelo2.1                                                                 20060618034327  CV  LON$            G�O�G�O��I7                JA  ARUP                                                                        20060618035342                      G�O�G�O�G�O�                JM  ARGQJMQC1.0                                                                 20060617172138  CV  DAT$            G�O�G�O�F�
                JM  ARGQJMQC1.0                                                                 20060617172138  CV  LAT$            G�O�G�O�A��;                JM  ARGQJMQC1.0                                                                 20060617172138  CV  LON$            G�O�G�O��H�                JM  ARCAJMQC1.0                                                                 20070628140426  CV  PRES            G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20070628140426  CV  PSAL            G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20070629030922  CV  PSAL            G�O�G�O�G�O�                JM  ARSQWJO 2.0 SeHyD1.0                                                        20070702000000  IP  PSAL            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20071001072803  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20071001091732                      G�O�G�O�G�O�                JM  AREVjmrvp1.2                                                                20090312120643  IP  PRES            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20090318072547  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20090318073050                      G�O�G�O�G�O�                JM      JMFC1.0                                                                 20150609200533                      G�O�G�O�G�O�                JA  ARDU                                                                        20150614054514                      G�O�G�O�G�O�                