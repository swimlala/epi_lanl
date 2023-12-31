CDF   !   
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   s   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JAMSTEC    source        
Argo float     history       b2009-10-19T15:56:53Z creation;2015-03-10T02:10:35Z update;2015-06-07T15:16:26Z conversion to V3.1;     
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
_FillValue                    iArgo profile    3.1 1.2 19500101000000  4900898 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               xA   JA  20091019155653  20150613142517  A9_60144_120                    2C  D   APEX                            2414                            061305                          846 @�TOb�$1   @�TR!/hV@C���Q��c�=p��
1   ARGOS   A   A   A   Primary sampling: discrete [spot sampling CTD data]                                                                                                                                                                                                                @�  AffAa��A�  A���A陚B
ffB��B2ffBF��BXffBl  B���B�33B�  B���B���B���B�  B�  B���B�33B�  B�ffB�ffC� C� C  CL�C�C�fCL�C$  C)ffC.33C3  C8ffC=33CB�CG  CQ  C[� CeffCo�Cy�C��fC��fC��fC��fC��fC���C�� C���C�� C���C���C�� C��3C C�s3C̳3CѦfCֳ3CۦfC�3C�3C��C�� C��3C��fDٚD�3D� D� DٚD�3D� D$� D)ٚD.�fD3� D8��D=ٚDBٚDG�fDLٚDQ��DV� D[ٚD`� DeٚDj�3Do��Dt�fDyٚD��D�l�D��3D��fD�&fD�i�D�� D��3D�0 D�ffD�� D���D�,�D�Y�Dڬ�D��fD�)�D�l�D�fD�f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�  AffAY��A�  A���A噚BffB��B0ffBD��BVffBj  B33B�33B�  B���B���B���B�  B�  B���B�33B�  B�ffB�ffC  C  C
� C��C��CffC��C#� C(�fC-�3C2� C7�fC<�3CA��CF� CP� C[  Cd�fCn��Cx��C�ffC�ffC�ffC�ffC�ffC�Y�C�� C�Y�C�@ C�Y�C�L�C�@ C�s3C�@ C�33C�s3C�ffC�s3C�ffC�s3C�s3C�L�C� C�s3C�ffD��D�3D� D� D��D�3D� D$� D)��D.�fD3� D8��D=��DB��DG�fDL��DQ��DV� D[��D`� De��Dj�3Do��Dt�fDy��D��D�\�D��3D��fD�fD�Y�D�� D��3D�  D�VfD�� D���D��D�I�Dڜ�D��fD��D�\�D�fD��f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�v�A��DA��DA��hA���A���A��7A�r�A�VA�n�A|��Ae��A["�AQ��AJ�RAHȴAGS�AE�AC��ABQ�AAK�A?�^A>  A:��A9dZA8(�A6��A5K�A4�yA4ȴA3�7A3��A3"�A2�HA4E�A4�DA4bNA3�mA2�!A2bNA/��A,Q�A)�^A'�A$�yA#dZA �A�wA�A�HA-A$�A�TA�/AA	33AhsAVA ��@�\)@�E�@�b@�  @�Q�@��@��;@ׅ@ҸR@ͩ�@ʇ+@�Z@�\)@��j@�K�@���@��T@�M�@�E�@��#@�ƨ@��@�1@��@��T@�|�@��@}��@wK�@sdZ@p  @k�m@gl�@c@`Q�@\z�@T�j@LI�@D�@=�T@7��@1�#@,�@(r�@%�@!7L@�/@x�@�-@~�@K�@dZ@bN@O�@��?�|�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�v�A��DA��DA��hA���A���A��7A�r�A�VA�n�A|��Ae��A["�AQ��AJ�RAHȴAGS�AE�AC��ABQ�AAK�A?�^A>  A:��A9dZA8(�A6��A5K�A4�yA4ȴA3�7A3��A3"�A2�HA4E�A4�DA4bNA3�mA2�!A2bNA/��A,Q�A)�^A'�A$�yA#dZA �A�wA�A�HA-A$�A�TA�/AA	33AhsAVA ��@�\)@�E�@�b@�  @�Q�@��@��;@ׅ@ҸR@ͩ�@ʇ+@�Z@�\)@��j@�K�@���@��T@�M�@�E�@��#@�ƨ@��@�1@��@��T@�|�@��@}��@wK�@sdZ@p  @k�m@gl�@c@`Q�@\z�@T�j@LI�@D�@=�T@7��@1�#@,�@(r�@%�@!7L@�/@x�@�-@~�@K�@dZ@bN@O�@��?�|�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB\)B\)B[#B\)B[#B[#B[#B[#B\)B��B��B�-B��B}�B[#BXBO�BG�BA�BQ�BP�BQ�BL�B8RB/B'�B�B�B#�B/B+BE�B9XBYB�DB��B�jBǮBƨB��BƨB�3B��B��B�\B�1B{�Bm�BgmBaHBO�BN�BN�BH�B@�B6FB0!B#�B�B\B1BB��B�B�sB�TB�HB�/B�/B�#B�#B�NB�;B�`B�BBoB#�B:^BT�BhsBx�B�=B��B��B�-B�}B��B�#B�fB��B	B	hB	�B	%�B	<jB	W
B	l�B	�B	��B	�B	��B	��B	�5B	�B	��B
%B
uB
�B
(�B
5?B
?}B
H�B
O�B
Y1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B\)B\)B[#B\)B[#B[#B[#B[#BaHB��BB�LB�B�B\)BYBP�BH�BB�BR�BQ�BR�BN�B9XB0!B(�B�B�B#�B0!B+BE�B9XBXB�DB��B�jBȴBƨB��BǮB�9B��B��B�\B�7B|�Bn�BhsBbNBP�BO�BO�BI�BA�B7LB1'B$�B�BbB1BB��B�B�yB�ZB�NB�5B�/B�)B�)B�NB�;B�`B�BBoB#�B:^BT�BhsBx�B�=B��B��B�-B�}B��B�#B�fB��B	B	hB	�B	%�B	<jB	W
B	l�B	�B	��B	�B	��B	��B	�5B	�B	��B
%B
uB
�B
(�B
5?B
?}B
H�B
O�B
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
<F?<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ = PRES-SP(NextCycle), where SP is SURFACE PRESSURE from next cycle                                                                                                                                                                                     TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,PRES_ADJ,elapsed_time,alpha,tau),elapsed_time=P/mean_rise_rate,P=dbar since the start of the profile for each samples                                                                                                       None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            SP(NextCycle)=0.5(dbar)                                                                                                                                                                                                                                         None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE; PRES_ADJ_ERR : Manufacture sensor accuracy                                                                                                                                                                 TEMP_ADJ_ERR : SBE sensor accuracy                                                                                                                                                                                                                              Salinity Recalculation using PRES_ADJ; PSAL_ADJ_ERR : max(sum of RecalS & CTM errors, 0.01(PSS-78))                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         200911011402432009110114024320091101140243200911011522502009110115225020091101152250201010040000002010100400000020101004000000  JA  ARFMdecpA9_b                                                                20091019155652  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.5                                                                 20091019155653  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.0                                                                 20091019155653  IP  PRES            G�O�G�O�G�O�                JA  ARCArsal2.1a                                                                20091019155654  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20091019155654  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20091019155655  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20091019155655  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8b                                                                20091019155655  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8b                                                                20091019155655  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20091019155655  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20091019160306                      G�O�G�O�G�O�                JA  ARFMdecpA9_b                                                                20091023065525  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.5                                                                 20091023065816  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.0                                                                 20091023065817  IP  PRES            G�O�G�O�G�O�                JA  ARCArsal2.1a                                                                20091023065817  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20091023065817  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20091023065818  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20091023065818  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8b                                                                20091023065818  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8b                                                                20091023065818  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20091023065818  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20091023070245                      G�O�G�O�G�O�                JM  ARGQJMQC1.0                                                                 20091022232748  CV  DAT$            G�O�G�O�F���                JM  ARCAJMQC1.0                                                                 20091101140243  CV  PRES            G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20091101140243  IP  PSAL            G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20091101152250  CV  PSAL            G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2010V1                                                       20101004000000  IP  PSAL            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20101014014545  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20101015004933                      G�O�G�O�G�O�                JM  RENCREJM1.1c                                                                20150209092152  ED  SCIENTIFIC_CALIBG�O�G�O�G�O�                JA  ARDU                                                                        20150310021035                      G�O�G�O�G�O�                JM      JMFC1.0                                                                 20150607151612                      G�O�G�O�G�O�                JA  ARDU                                                                        20150613142517                      G�O�G�O�G�O�                