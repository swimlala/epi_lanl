CDF   '   
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   s   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JAMSTEC    source        
Argo float     history       b2009-08-10T15:57:35Z creation;2015-03-10T02:10:38Z update;2015-06-07T15:14:43Z conversion to V3.1;     
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
_FillValue                    iArgo profile    3.1 1.2 19500101000000  4900898 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               qA   JA  20090810155735  20150613142514  A9_60144_113                    2C  D   APEX                            2414                            061305                          846 @�Bԇ��$1   @�B��5y�@C�-V�d�/��1   ARGOS   A   A   A   Primary sampling: discrete [spot sampling CTD data]                                                                                                                                                                                                                @���A  Ai��A���A�33A�ffB	��B  B2  BE33BX��Bn  B�  B�ffB�  B�  B�33B�33B���B�33BЙ�B�ffB�  BB���CffC�C� C  CL�C�C�C$L�C)ffC.�C3ffC8�C=  CA��CGffCQL�C[  CeffCoffCyffC���C���C���C�� C��fC���C���C���C��3C�s3C���C�s3C�ffC�� CǦfC̳3CѦfC֦fCۙ�C�3C�3C��C�fC��3C���D� D��D�3D� D�3D�fDٚD$�3D)� D.��D3��D8��D=� DB�3DG� DL��DQ��DV��D[��D`�3De�fDjٚDo�fDt��Dy�fD�#3D�c3D���D���D�#3D�ffD���D�� D��D�ffD���D�ٚD�,�D�c3Dڣ3D�� D�&fD�` D�fD�� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�33A33A\��A�33A���A�  BffB��B.��BB  BU��Bj��B|��B���B�ffB�ffB���B���B�  Bř�B�  B���B�ffB�  B�  C ��CL�C
�3C33C� CL�CL�C#� C(��C-L�C2��C7L�C<33CA  CF��CP� CZ33Cd��Cn��Cx��C�&fC�33C�33C��C�@ C�33C�33C�33C�L�C��C�&fC��C�  C�Y�C�@ C�L�C�@ C�@ C�33C�L�C�L�C�&fC�@ C�L�C�33D��D�fD� D��D� D�3D�fD$� D)��D.�fD3y�D8��D=��DB� DG��DL�fDQ��DV�fD[��D`� De�3Dj�fDo�3Dt��Dy�3D�	�D�I�D��3D��3D�	�D�L�D�� D��fD�  D�L�D�� D�� D�3D�I�Dډ�D��fD��D�FfD�|�D��f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A�\)A�A��A��A�l�A�XAn9XAbA�AW�PAR��ALz�AI��AI��AHz�AG;dAC�^ABr�AAdZA<�uA:ĜA8z�A5�FA3�FA2E�A1��A1`BA05?A/�
A/��A/C�A.��A.��A.{A-�A,ZA*�A)XA((�A&I�A$ȴA#G�A $�A��A�Ap�A  AĜA\)A��AAXA&�A
�/A�+Av�A�TA Q�@��@�F@�ȴ@���@�
=@�ƨ@�dZ@ղ-@ѩ�@͡�@���@��H@���@�Z@�7L@�l�@�M�@�?}@��j@�Z@���@�X@�@��@��m@��u@��`@}@y�#@t�@q&�@l��@hĜ@dz�@`A�@[�
@W�@O+@F�R@A%@;@5@0�@*~�@&��@"�H@��@�\@�@^5@�P@j@	7L@{@dZ@�7?�|�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A���A�\)A�A��A��A�l�A�XAn9XAbA�AW�PAR��ALz�AI��AI��AHz�AG;dAC�^ABr�AAdZA<�uA:ĜA8z�A5�FA3�FA2E�A1��A1`BA05?A/�
A/��A/C�A.��A.��A.{A-�A,ZA*�A)XA((�A&I�A$ȴA#G�A $�A��A�Ap�A  AĜA\)A��AAXA&�A
�/A�+Av�A�TA Q�@��@�F@�ȴ@���@�
=@�ƨ@�dZ@ղ-@ѩ�@͡�@���@��H@���@�Z@�7L@�l�@�M�@�?}@��j@�Z@���@�X@�@��@��m@��u@��`@}@y�#@t�@q&�@l��@hĜ@dz�@`A�@[�
@W�@O+@F�R@A%@;@5@0�@*~�@&��@"�H@��@�\@�@^5@�P@j@	7L@{@dZ@�7?�|�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB6FB33B0!B-B33B�wB�B_;B�=B��B��B��B�B�fB�mB�mB�sB�BB��B��B��B�B�B�fB��B��BB(�BQ�BffBv�B{�B�B�1B�PB�DB�+B�B|�B~�Bv�BhsB`BB[#BZBVBYBVBP�BM�BC�B;dB5?B-B'�B �BhB%B��B��B�B�B�sB�TB�HB�;B�/B�BB�BB�ZB�B��B%BuB �B/B>wBM�BZBiyBv�B�%B��B�B�RBƨB��B�TB�B��B	DB	�B	&�B	2-B	L�B	ffB	w�B	�JB	��B	�-B	ȴB	�B	�`B	��B
B
hB
 �B
)�B
33B
<jB
D�B
L�B
R�B
V1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B7LB49B0!B/B=qBȴB&�Be`B�\B�BĜB��B�B�mB�sB�yB�yB�BB��B��B��B��B�B�fB��B  BB(�BQ�BffBv�B{�B�B�1B�VB�JB�1B�%B}�B~�Bw�BiyBaHB]/BZBVBYBW
BP�BN�BD�B<jB6FB.B(�B!�BoB+B��B��B�B�B�yB�ZB�NB�BB�5B�BB�HB�`B�B��B%BuB �B0!B?}BM�BZBiyBv�B�+B��B�B�RBƨB�B�ZB�B��B	JB	�B	&�B	2-B	L�B	ffB	w�B	�JB	��B	�-B	ȴB	�B	�`B	��B
B
hB
 �B
)�B
33B
<jB
D�B
L�B
R�B
V1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<7�4<7�4<T��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ = PRES-SP(NextCycle), where SP is SURFACE PRESSURE from next cycle                                                                                                                                                                                     TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,PRES_ADJ,elapsed_time,alpha,tau),elapsed_time=P/mean_rise_rate,P=dbar since the start of the profile for each samples                                                                                                       None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            SP(NextCycle)=0.8(dbar)                                                                                                                                                                                                                                         None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE; PRES_ADJ_ERR : Manufacture sensor accuracy                                                                                                                                                                 TEMP_ADJ_ERR : SBE sensor accuracy                                                                                                                                                                                                                              Salinity Recalculation using PRES_ADJ; PSAL_ADJ_ERR : max(sum of RecalS & CTM errors, 0.01(PSS-78))                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         200908231418582009082314185820090823141858200908231529212009082315292120090823152921201010040000002010100400000020101004000000  JA  ARFMdecpA9_b                                                                20090810155734  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.5                                                                 20090810155735  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.0                                                                 20090810155735  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20090810155736  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20090810155737  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20090810155737  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8b                                                                20090810155737  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8b                                                                20090810155737  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20090810155737  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20090810160418                      G�O�G�O�G�O�                JA  ARFMdecpA9_b                                                                20090814095500  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.5                                                                 20090814095750  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.0                                                                 20090814095750  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20090814095751  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20090814095752  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20090814095752  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8b                                                                20090814095752  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8b                                                                20090814095752  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20090814095752  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20090814100111                      G�O�G�O�G�O�                JA  ARCArsal2.1a                                                                20090821003858  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20090821003858  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20090821003859  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20090821003859  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8b                                                                20090821003859  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8b                                                                20090821003859  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20090821003859  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20090821005108                      G�O�G�O�G�O�                JM  ARGQJMQC1.0                                                                 20090813232003  CV  DAT$            G�O�G�O�F��                JM  ARCAJMQC1.0                                                                 20090823141858  CV  PRES            G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20090823141858  CV  PSAL            G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20090823152921  CV  PSAL            G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2010V1                                                       20101004000000  IP  PSAL            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20101014014538  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20101015004929                      G�O�G�O�G�O�                JM  RENCREJM1.1c                                                                20150209092142  ED  SCIENTIFIC_CALIBG�O�G�O�G�O�                JA  ARDU                                                                        20150310021038                      G�O�G�O�G�O�                JM      JMFC1.0                                                                 20150607151435                      G�O�G�O�G�O�                JA  ARDU                                                                        20150613142514                      G�O�G�O�G�O�                