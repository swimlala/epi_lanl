CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS   ;   N_CALIB       	N_HISTORY                     <   	DATA_TYPE                  comment       	Data type      
_FillValue                    0�   FORMAT_VERSION                 comment       File format version    
_FillValue                    0�   HANDBOOK_VERSION               comment       Data handbook version      
_FillValue                    0�   REFERENCE_DATE_TIME                 comment       !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    1    PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    1   PROJECT_NAME                  comment       Name of the project    
_FillValue                  @  1   PI_NAME                   comment       "Name of the principal investigator     
_FillValue                  @  1X   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  1�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       <0..N, 0 : launch cycle (if exists), 1 : first complete cycle   
_FillValue         ��        1�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    1�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    1�   DATE_CREATION                   comment       Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    1�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    1�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     1�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    2   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    2   INST_REFERENCE                    	long_name         Instrument type    conventions       Brand, type, serial number     
_FillValue                  @  2   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    2\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
_FillValue        A.�~            2`   JULD_QC                	long_name         Quality on Date and Time   conventions       Argo reference table 2     
_FillValue                    2h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
_FillValue        A.�~            2l   LATITUDE               	long_name         &Latitude of the station, best estimate     units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�             2t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�             2|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    2�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    2�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    2�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    2�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    2�   PRES         
      	   	long_name         SEA PRESSURE   
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    comment       $In situ measurement, sea surface = 0   C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���      �  2�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  <  3�   PRES_ADJUSTED            
      	   	long_name         SEA PRESSURE   
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    comment       $In situ measurement, sea surface = 0   C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���      �  3�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  <  4�   PRES_ADJUSTED_ERROR          
         	long_name         SEA PRESSURE   
_FillValue        G�O�   units         decibar    comment       WContains the error on the adjusted values as determined by the delayed mode QC process.    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���      �  4�   TEMP         
      	   	long_name         $SEA TEMPERATURE IN SITU ITS-90 SCALE   
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      comment       In situ measurement    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o      �  5�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  <  6�   TEMP_ADJUSTED            
      	   	long_name         $SEA TEMPERATURE IN SITU ITS-90 SCALE   
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      comment       In situ measurement    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o      �  7    TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  <  7�   TEMP_ADJUSTED_ERROR          
         	long_name         $SEA TEMPERATURE IN SITU ITS-90 SCALE   
_FillValue        G�O�   units         degree_Celsius     comment       WContains the error on the adjusted values as determined by the delayed mode QC process.    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o      �  8(   PSAL         
      	   	long_name         PRACTICAL SALINITY     
_FillValue        G�O�   units         psu    	valid_min                	valid_max         B(     comment       In situ measurement    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o      �  9   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  <  :    PSAL_ADJUSTED            
      	   	long_name         PRACTICAL SALINITY     
_FillValue        G�O�   units         psu    	valid_min                	valid_max         B(     comment       In situ measurement    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o      �  :<   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  <  ;(   PSAL_ADJUSTED_ERROR          
         	long_name         PRACTICAL SALINITY     
_FillValue        G�O�   units         psu    comment       WContains the error on the adjusted values as determined by the delayed mode QC process.    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o      �  ;d   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  <P   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    <�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ?�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    B�   CALIBRATION_DATE            	             
_FillValue                  ,  E�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    E�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    E�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    E�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    E�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  E�   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    E�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    F   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    F   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         F    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         F$   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        F(   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    F,Argo profile    2.2 1.2 19500101000000  4900568 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               tA   AO  20071220101759  20080829173255  0961_34242_116                  2C  D   APEX_SBE_1533                                                   846 @Ԭ��� 1   @Ԭ��p @9U�    �c��@   1   ARGOS   A   A   A   @���A(  A���A�ffA�ffB  B533BP  Bm��B�33B�33B�33B���Bƙ�B֙�B���B�ffC�C  C33C�fC#33C+ffC4� C>ffCH�CR33C]� ChL�CtL�C�33C�  C�ٚC��fC��3C�@ C���C��C��3CɌ�C�s3C�s3C�@ C�@ DFfD�DfDL�D&fD&Y�D.�fD7� DA3DKfDU� D`�fDl3Dx3Dz311111111111111111111111111111111111111111111111111111111111 @�33A#33A�34A�  A�  B��B4  BN��BlfgB���B���B���B�33B�  B�  B�33B���C��C
�3C�fC��C"�fC+�C433C>�CG��CQ�fC]33Ch  Ct  C��C�ٚC��4C�� C���C��C�fgC��4C���C�fgC�L�C�L�C��C��D33DgD�3D9�D3D&FgD.�3D7��DA  DJ�3DUl�D`s3Dl  Dx  Dz  11111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�
=A���A�
=A�A�JA�oA��A��A��A��wA���A��A�O�A�ffA��TA�A�A��#A��A� �A���A�I�A��7A���A�?}A}�7AqAn�9Ajz�Ac�
A_G�AYoAQ��AL��AE�wAA%A8Q�A3
=A,�\A&A!�AoA33A�^A 1'@��-@�&�@���@��`@�`B@���@��@�X@��R@�@�ȴ@���@|(�@u`B@t�D11111111111111111111111111111111111111111111111111111111111 A�
=A���A�
=A�A�JA�oA��A��A��A��wA���A��A�O�A�ffA��TA�A�A��#A��A� �A���A�I�A��7A���A�?}A}�7AqAn�9Ajz�Ac�
A_G�AYoAQ��AL��AE�wAA%A8Q�A3
=A,�\A&A!�AoA33A�^A 1'@��-@�&�@���@��`@�`B@���@��@�X@��R@�@�ȴ@���@|(�@u`B@t�D11111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB� B�B�B�B�B�B�B�B�Bz�Bt�B9XBjBbB�ZBÖB��Bq�BH�BVB
��B
�B
�}B
��B
q�B
33B
%�B
JB	�B	�5B	�jB	��B	{�B	S�B	8RB	VB�B�B�qB��B�oB{�BT�BF�B<jB33B1'B/B>wB\)B�+B�B��B��B	!�B	@�B	o�B	�JB	�\11111111111111111111111111111111111111111111111111111111111 B�9B�$B�5B�)B�+B�'B�'B�+B�_B{�BvIB@�BpB9B�B��B��Bt*BK�B�B
��B
�gB
�wB
��B
uB
4B
&�B
B	��B	߷B	��B	��B	}rB	UB	9�B	tB��B�AB�MB�.B��B}�BV2BG�B=�B4;B2iB/�B?SB\�B��B��B�]B�(B	",B	@�B	o�B	��B	��11111111111111111111111111111111111111111111111111111111111 ;�`B;�`B;�`B;�`B;�`B;�`B;�`B;�`B;�`B;�`B;�`B<#�
<t�;�`B;�`B;�`B;�`B;�`B;�`B;�`B;�`B;�`B;�`B;�`B;�`B;ě�;�`B;�`B;�`B;�`B;�`B;�`B;�`B;ě�;�`B;ě�;ě�;ě�;ě�;ě�;ě�;�`B;ě�;ě�;ě�;ě�;ě�;ě�;ě�;ě�;ě�;ě�;ě�;ě�;ě�;ě�;ě�;ě�;ě�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - (REPORTED_SURFACE_PRESSURE) for PADJ                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al,2007,JAOT & effects of pressure adjustments                                                                                                                                                            PADJ REPORTED_SURFACE_PRESSURE =0.3 dbar                                                                                                                                                                                                                        none                                                                                                                                                                                                                                                            CTM alpha = 0.0267 & tau = 18.6 s with error equal to the correction                                                                                                                                                                                            Pressures adjusted using reported surface pressure when warranted.                                                                                                                                                                                              none                                                                                                                                                                                                                                                            Salinity corrected for pressure sensor calibration drift and cell thermal mass                                                                                                                                                                                  20080714141043              20080825204837  AO  ARGQ                                                                        20071220101759  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20071220101759  QCF$                G�O�G�O�G�O�0               PM  ARSQPADJV1.1                                                                20080714141043  QC  PRES            @���Dz3                    PM  ARSQCTM V1.1                                                                20080714141043  QC  PSAL            @���Dz3                    PM  ARSQSIQCV2.0WOD2001 & Argo                                                  20080829173157  IP                  G�O�G�O�G�O�                