CDF   (   
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   s   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JAMSTEC    source        
Argo float     history       b2009-07-11T09:56:21Z creation;2015-03-10T02:10:40Z update;2015-06-07T15:14:01Z conversion to V3.1;     
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
_FillValue                    iArgo profile    3.1 1.2 19500101000000  4900898 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               nA   JA  20090711095621  20150613142514  A9_60144_110                    2C  D   APEX                            2414                            061305                          846 @�;M���1   @�;Ow`P@C�j~��#�d�1&�1   ARGOS   A   A   A   Primary sampling: discrete [spot sampling CTD data]                                                                                                                                                                                                                @�  A��Ah  A�33A���A�ffB
  BffB0ffBE��BY33Bm��B�  B���B���B�33B���B�33B�  B���BЙ�B�33B���B�  B���C33CL�C33CffC33C�CffC$33C)�C.�C3L�C7��C=�CBffCG33CQffC[  CeffCo33Cx�fC���C��fC��fC��fC���C�� C���C�� C���C��fC���C��fC�ffC�Cǌ�C̦fCљ�C֙�C۳3C�fC�� C��C�fC�� C���D� D��D�3D�3D� D�fD� D$�fD)�3D.�3D3� D8� D=ٚDB�fDGٚDL�3DQٚDV�3D[��D`ٚDe� Dj��Do� Dt��Dy�3D�  D�p D���D��fD��D�i�D���D�� D��D�\�D���D�� D�)�D�i�DڦfD���D�  D�i�D� D��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�ffA��A[33A���A�ffA�  B��B33B-33BBffBV  BjffB|��B�33B�33B���B�33B���B�ffB�33B�  Bٙ�B�33B�ffB�33C ffC� C
ffC��CffCL�C��C#ffC(L�C-L�C2� C7  C<L�CA��CFffCP��CZ33Cd��CnffCx�C�33C�@ C�@ C�@ C�33C�Y�C�33C��C�33C�@ C�33C�@ C�  C�33C�&fC�@ C�33C�33C�L�C�@ C�Y�C�&fC�@ C�Y�C�&fD��D��D� D� D��D�3D��D$�3D)� D.� D3��D8��D=�fDB�3DG�fDL� DQ�fDV� D[�fD`�fDe��Dj�fDo��Dt��Dy� D�fD�VfD�� D���D�  D�P D��3D��fD�  D�C3D�� D��fD� D�P Dڌ�D��3D�fD�P D�fD��31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A��`A���A���AqhsAfffAb�A_�wA[;dAW�wAS��AQ`BAP~�AL�RAH�uADn�AB��AA+A??}A<�\A;+A8��A6�A4$�A3?}A25?A/�TA1�A1?}A133A1��A2��A2$�A1`BA-�PA+x�A*��A)VA(ZA'C�A%��A$M�A �!A�PAE�A�`AbNA\)A�mAA��AK�AI�A	��AI�A��A/@�|�@��h@�@��@���@�`B@�@��@�@�5?@�bN@�  @\@��@�"�@��^@�o@���@� �@���@��D@�  @�(�@���@���@��7@��+@�1@{�m@y�@t(�@q%@k��@gl�@c�@_��@\j@X��@O;d@GK�@?|�@:��@5O�@0�@,z�@(A�@$Z@ ��@��@�@�@�7@��@dZ@��@�-@�@ �1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��A��`A���A���AqhsAfffAb�A_�wA[;dAW�wAS��AQ`BAP~�AL�RAH�uADn�AB��AA+A??}A<�\A;+A8��A6�A4$�A3?}A25?A/�TA1�A1?}A133A1��A2��A2$�A1`BA-�PA+x�A*��A)VA(ZA'C�A%��A$M�A �!A�PAE�A�`AbNA\)A�mAA��AK�AI�A	��AI�A��A/@�|�@��h@�@��@���@�`B@�@��@�@�5?@�bN@�  @\@��@�"�@��^@�o@���@� �@���@��D@�  @�(�@���@���@��7@��+@�1@{�m@y�@t(�@q%@k��@gl�@c�@_��@\j@X��@O;d@GK�@?|�@:��@5O�@0�@,z�@(A�@$Z@ ��@��@�@�@�7@��@dZ@��@�-@�@ �1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBɺB�B�mB�B�hB�3B�XB�3B�FBŢB�XBȴB��B��B�yB��B��B�
B�;B�`B�B�yB�sB�HB�BB�5B�BB�B9XBK�Bv�B��B��B��B�7By�Bu�Bn�Bv�Bt�Bp�Bl�BZBVBJ�BO�BJ�B]/BbNB\)BS�BL�BD�B<jB5?B)�B#�B�BJBB��B�B�B�B�B�fB�ZB�NB�TB�`B�B��B  BbB�B#�B0!B>wBO�BaHBo�B�B�uB��B�9B��BɺB�B�NB��B	B	\B	�B	$�B	0!B	L�B	dZB	|�B	�PB	��B	�'B	��B	��B	�HB	�B	��B

=B
�B
"�B
-B
6FB
>wB
F�B
M�B
T�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B�#B�B&�B��B�?B�dB�?B�RBǮB�^BɺB��B��B�B��B��B�B�BB�fB�B�B�yB�HB�HB�;B�;B�B9XBK�Bv�B��B��B��B�=Bz�Bv�Bo�Bw�Bt�Bq�Bm�BZBW
BJ�BP�BJ�B]/BcTB]/BT�BM�BF�B<jB6FB+B$�B�BPBB��B�B�B�B�B�mB�`B�TB�ZB�fB�B��B  BbB�B#�B1'B>wBO�BaHBo�B�B�{B��B�9B��BɺB�B�NB��B	B	bB	�B	$�B	0!B	L�B	dZB	|�B	�PB	��B	�'B	��B	��B	�HB	�B	��B

=B
�B
"�B
-B
6FB
>wB
F�B
M�B
T�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ = PRES-SP(NextCycle), where SP is SURFACE PRESSURE from next cycle                                                                                                                                                                                     TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,PRES_ADJ,elapsed_time,alpha,tau),elapsed_time=P/mean_rise_rate,P=dbar since the start of the profile for each samples                                                                                                       None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            SP(NextCycle)=0.8(dbar)                                                                                                                                                                                                                                         None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE; PRES_ADJ_ERR : Manufacture sensor accuracy                                                                                                                                                                 TEMP_ADJ_ERR : SBE sensor accuracy                                                                                                                                                                                                                              Salinity Recalculation using PRES_ADJ; PSAL_ADJ_ERR : max(sum of RecalS & CTM errors, 0.01(PSS-78))                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         200907241351202009072413512020090724135120200908060402582009080604025820090806040258201010040000002010100400000020101004000000  JA  ARFMdecpA9_b                                                                20090711095620  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.5                                                                 20090711095621  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.0                                                                 20090711095621  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20090711095621  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20090711095622  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20090711095622  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8b                                                                20090711095623  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8b                                                                20090711095623  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20090711095623  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20090711100338                      G�O�G�O�G�O�                JA  ARFMdecpA9_b                                                                20090715065448  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.5                                                                 20090715065730  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.0                                                                 20090715065730  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20090715065731  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20090715065732  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20090715065732  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8b                                                                20090715065732  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8b                                                                20090715065732  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20090715065732  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20090715070141                      G�O�G�O�G�O�                JA  ARCArsal2.1a                                                                20090821003853  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20090821003854  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20090821003855  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20090821003855  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8b                                                                20090821003855  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8b                                                                20090821003855  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20090821003855  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20090821005510                      G�O�G�O�G�O�                JM  ARGQJMQC1.0                                                                 20090714230114  CV  DAT$            G�O�G�O�F��{                JM  ARGQJMQC1.0                                                                 20090714230114  CV  LAT$            G�O�G�O�B�H                JM  ARCAJMQC1.0                                                                 20090724135120  CV  PRES            G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20090724135120  CV  PSAL            G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20090806040258  CV  PSAL            G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2010V1                                                       20101004000000  IP  PSAL            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20101014014533  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20101015004925                      G�O�G�O�G�O�                JM  RENCREJM1.1c                                                                20150209092138  ED  SCIENTIFIC_CALIBG�O�G�O�G�O�                JA  ARDU                                                                        20150310021040                      G�O�G�O�G�O�                JM      JMFC1.0                                                                 20150607151348                      G�O�G�O�G�O�                JA  ARDU                                                                        20150613142514                      G�O�G�O�G�O�                