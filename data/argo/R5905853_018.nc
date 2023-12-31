CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:25:46Z creation;2022-06-04T17:25:47Z conversion to V3.1      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8<   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8@   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8H   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8L   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8T   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8\   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8d   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8h   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8p   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8t   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8x   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8|   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9|   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M(   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tx   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �0   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �p   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �(   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ޘ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �    HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �D   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �T   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �X   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �h   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �l   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �p   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20220604172546  20220610131506  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @��~�l�1   @���$i@-�^5?|��c��^5?}1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�33@�  A   A   A@  A`  A���A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�ffB�ffB�  B�  B�  B�  B�  B�  B�ffB���B�  B�  B�  B�  B�  B�ffB�  B�  B�  B�  B�  B�  B�  B�  B�  B�  BB�  B���B���C   C�CL�C��C�fC
  C  C  C  C  C  C�fC  C  C  C  C   C"  C$  C&�C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CS�fCV  CX  CZ  C\  C^  C`  Ca�fCd  Cf  Ch�Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @{@�p�@�=qA�A!�AA�Aa�A�\)A��\A��\A��\A��\AЏ\A��\A��\B G�BG�BG�BG�B G�B(G�B0G�B8G�B@G�BHG�BPG�BXG�B`G�BhG�BpG�BxG�B�#�B�#�B��=B��=B�#�B�#�B�#�B�#�B�#�B�#�B��=B��B�#�B�#�B�#�B�#�B�#�BĊ=B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�qB�#�B��B��C �C+�C^�C޹C�RC
�C�C�C�C�C�C�RC�C�C�C�C �C"�C$�C&+�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CS�RCV�CX�CZ�C\�C^�C`�Ca�RCd�Cf�Ch+�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D {D �{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D	{D	�{D
{D
�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D {D �{D!{D!�{D"{D"�{D#{D#�{D${D$�{D%{D%�{D&{D&�{D'{D'�{D({D(�{D){D)�{D*{D*�{D+{D+�{D,{D,�{D-{D-�{D.{D.�{D/{D/�{D0{D0�{D1{D1�{D2{D2�{D3{D3�{D4{D4�{D5{D5�{D6{D6�{D7{D7�{D8{D8�{D9{D9�{D:{D:�{D;{D;�{D<{D<�{D={D=�{D>{D>�{D?{D?�{D@{D@�{DA{DA�{DB{DB�{DC{DC�{DD{DD�{DE{DE�{DF{DF�{DG{DG�{DH{DH�{DI{DI�{DJ{DJ�{DK{DK�{DL{DL�{DM{DM�{DN{DN�{DO{DO�{DP{DP�{DQ{DQ�{DR{DR�{DS{DS�{DT{DT�{DU{DU�{DV{DV�{DW{DW�{DX{DX�{DY{DY�{DZ{DZ�{D[{D[�{D\{D\�{D]{D]�{D^{D^�{D_{D_�{D`{D`�{Da{Da�{Db{Db�{Dc{Dc�{Dd{Dd�{De{De�{Df{Df�{Dg{Dg�{Dh{Dh�{Di{Di�{Dj{Dj�{Dk{Dk�{Dl{Dl�{Dm{Dm�{Dn{Dn�{Do{Do�{Dp{Dp�{Dq{Dq�{Dr{Dr�{Ds{Ds�{Dt{Dt�{Du{Du�{Dv{Dv�{Dw{Dw�{Dx{Dx�{Dy{Dy�{Dz{Dz�{D{{D{�{D|{D|�{D}{D}�{D~{D~�{D{D�{D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D=D��=D�=D�B=DÂ=D��=D�=D�B=DĂ=D��=D�=D�B=Dł=D��=D�=D�B=DƂ=D��=D�=D�B=Dǂ=D��=D�=D�B=DȂ=D��=D�=D�B=Dɂ=D��=D�=D�B=Dʂ=D��=D�=D�B=D˂=D��=D�=D�B=D̂=D��=D�=D�B=D͂=D��=D�=D�B=D΂=D��=D�=D�B=Dς=D��=D�=D�B=DЂ=D��=D�=D�B=Dт=D��=D�=D�B=D҂=D��=D�=D�B=Dӂ=D��=D�=D�B=DԂ=D��=D�=D�B=DՂ=D��=D�=D�B=Dւ=D��=D�=D�B=Dׂ=D��=D�=D�B=D؂=D��=D�=D�B=Dق=D��=D�=D�B=Dڂ=D��=D�=D�B=Dۂ=D��=D�=D�B=D܂=D��=D�=D�B=D݂=D��=D�=D�B=Dނ=D��=D�=D�B=D߂=D��=D�=D�B=D��=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D��=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A���A�ٴA���A޹�Aޮ}AޮA޲�AްUAިXAޭAޞ�Aޏ�Aތ�Aއ_AނAAހ�A�}�A�t�A�r|A�poA�m)A�h>A�c�A�?�A�EA��Aܺ^Aܠ'Aܓ@A܀�A�a�A�J�A�%�A�s�A�+A�c AԤ�AӤ�A��.A�K�A�4A��A��AĵA��A��A��A��A�.IA�� A���A�1�A�<6A��}A�\�A���A��4A�:^A��A��A���A���A�@�A��A��A��}A��A�1�A�MA�iA�U�A���A�[�A�/�A���A��A�(A�V�A��4A�IA��5A{�TAw��AsffAm�0Aj��Ag��AdĜA`�A\�uAY��AU�mAS|AP��API�AN�oAK�jAGbNAF$�AD��AA�A?��A>XA<��A;F�A9A4�HA4VmA4	�A2�A2'�A0یA0�A/�[A/&A./�A-�A,��A,sA,�A,&A*�IA)
=A'��A&ںA&S&A%�A#�A#��A#v`A#.IA"t�A!�A!]dA x�A dZA!E�A"S&A"ɆA"�A!h
A �~A�`A �A�Ae�A�AD�A��A&AJ�A-�A;A��Al�A	lA��Ap�Av�A�A/�AA	lA��A�AS�Am]AzxA�KA1'A;�A��A�A��A��A��A�$A]�AMjA iA8�A�A�	A͟A&A�,AB�A.�A�A/A
_�A	��A	H�A�MAr�A��A�A;dAdZAl�A��A��A�AA��A:�A��A��Au�A^5A�A�AJ#AA خA ��@���@��3@���@���@�M�@�ݘ@��g@���@�˒@�Ĝ@�$�@��a@�p;@�%F@��@�6z@�:*@�@�˒@��@�%F@�M@��@�8�@��o@�b�@��@�A�@��@�	l@���@�0U@��@���@�C@�T�@��@�U2@�.�@�!@�=@��@��@���@��@��@�C-@�g�@�l�@���@ߙ�@�B�@�Ov@ݲ-@�s�@��@ܛ�@�˒@���@��@ٹ�@أ@�M@��@��@�^�@���@���@�Ĝ@Թ$@��@Ӑ�@�X�@�@�@Ѯ@�qv@��@�Ɇ@�y>@υ@�xl@�{@���@�>�@�@���@��r@�خ@͜�@�<6@�w�@˓@�� @���@�v`@�l�@�)�@ɠ�@�6z@Ȱ�@�|�@�IR@�)_@���@�W�@�@�n/@��@Ĝx@�N�@�@�hs@���@µ�@r@�:*@���@��~@�O@��@�ߤ@��@��z@��@��@�4@��6@�J�@�-�@��@�-w@��@��]@�͟@��B@�C@��@��A@�G@�j�@�w2@�k�@�/@�Ĝ@��@���@�j@��@���@�S�@�?@�_@��K@��@�|�@�خ@��.@��w@�F@��@���@��L@��R@���@�Ta@�7@���@��@���@�M@���@���@��@��U@��1@��u@�_@�x�@���@�H�@�(�@�� @�"�@�V�@�"h@��@��@���@��@���@���@�p;@�N�@�1�@��@�X�@���@���@�V�@�Ft@��.@�(�@�ݘ@��@�U�@���@��E@���@��@���@�O�@��@�h
@�#:@�{@�/�@�j�@���@�~�@�1�@�{@���@�5�@�	l@��@��$@�c @�-�@��m@�s�@��@��@�5?@� �@��H@���@�E9@��@��@���@�[�@���@�Q�@�Z�@�W?@��@���@�6@��@�)_@���@���@�h
@�^5@�W�@�6@��@�=@���@�5?@���@��y@��A@�Z�@�?@���@���@���@�a@�8@�+@���@�Ov@��K@�e,@��K@��e@��@�Ta@�8�@�(�@��@��#@��"@�n/@�K�@���@���@���@��=@��4@�W?@��@�i�@�D�@� �@��;@�P�@��@��@���@��@�{J@�]d@�Q@�u�@��@��h@���@���@��@�<�@��@��q@�K�@�+@�!�@�@�ی@��m@��R@��1@�{�@�c @�x@���@���@�j�@�b�@�;d@�V@��`@��[@���@�z�@�Z�@�8�@���@�ƨ@���@�s�@�T�@��@���@��$@��O@��@�]d@��@���@�?}@���@�͟@��9@��@�_@�<�@�*�@��@�$@U�@o@~�@~�6@~$�@}a�@}V@|�u@|C-@{�W@{��@{�P@{;d@z�'@z�@y@y��@ys�@y?}@x�?@xM@w�@w�@w�4@w�@v��@uj@t�$@t��@th�@s˒@s&@rJ�@qQ�@p�e@ph�@p7�@pG@o{J@n�}@nc @nO@m�@m@mj@l�[@l_@l9X@lM@l1@k�
@k,�@j�X@j;�@i�@i�M@ip�@iS&@iq@h�@hm�@h"h@h�@g�}@gK�@f�2@f��@f4@e^�@d��@d]d@c�@c��@c|�@cg�@c i@b;�@a�T@a@a�-@ac@`�?@`bN@_��@_�@@_l�@_/�@^�8@^�@^5?@^ �@]@]k�@\ѷ@\�@[�@[\)@[Mj@[
=@Z��@Y�o@Yԕ@Yx�@Y?}@Y;@X��@XtT@XPH@X"h@We�@W�@V�H@V�}@V�A@VH�@U�j@Uc@U�@T�@T�@S�[@S��@SO@S"�@R�@RM�@Q��@Qhs@QIR@Q2a@Q!�@Q	l@P�|@P�`@P��@O�@N�M@N5?@MVm@M	l@LĜ@L:�@K��@K6z@J��@J�\@J.�@I��@H��@H�u@Hj@H�@G{J@G@F��@F8�@E�@E�"@Ea�@EQ�@E%@D��@DG@C�@C|�@CC�@B��@B�@A��@A��@A|@A8�@@�@@~(@@�@?��@?@O@>�x@>�@=�@=s�@=q@<e�@;�+@;�*@;��@;|�@;�@:~�@9�T@9x�@9%@8�[@8�@8z�@7RT@7,�@7�@6��@6V@6W�@6M�@6H�@6)�@5@4��@4Xy@3�@3&@3�@2�y@2��@2�1@2;�@1�Z@1��@1T�@1�@0�@0�@0]d@0,=@0�@/�}@/;d@.�R@.H�@.J@-�#@-@-�t@-�~@-IR@-F@-%@,֡@,�9@,z�@,7�@+�@+��@+t�@+=@*�M@*�\@*��@*l�@*$�@)�@)�=@)�@(��@(h�@(%�@("h@'��@'��@'�@@'x@'>�@&ں@&s�@&_�@&L0@&)�@%�Z@%��@%w2@%k�@%m]@%}�@%x�@%��@%�h@%k�@%@@$�[@$�z@$��@$PH@$(�@$	�@$@$	�@#��@#�Q@#�0@#X�@#�@"�@"�R@"�b@"kQ@"-@!�@!s�@!N<@!V@ ��@ bN@��@��@�6@s@C@ں@�'@��@i�@M�@@�@�@��@��@w2@F@�v@oi@-�@7@~@@��@�
@�f@&@�@
=@�@�@��@�@\�@;�@6�@e@�o@��@�d@@�X@rG@N<@%F@�@�@��@�@l"@!@��@خ@�@��@qv@A�@9�@ i@��@�}@~�@YK@GE@	@��@^�@#�@�	@�@��@�@m�@C-@M@@�]@�@خ@�6@��@�k@��@l�@W?@9�@C@�@��@�h@\�@�#@�@��@s�@Vm@�P@ی@�@�u@M@�@�@�@��@~�@e�@�s@�A@YK@6�@C�@J�@=q@�>@u�@a�@��@�_@m�@!@�@�@G@��@�*@{J@F�@�@ i1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A���A�ٴA���A޹�Aޮ}AޮA޲�AްUAިXAޭAޞ�Aޏ�Aތ�Aއ_AނAAހ�A�}�A�t�A�r|A�poA�m)A�h>A�c�A�?�A�EA��Aܺ^Aܠ'Aܓ@A܀�A�a�A�J�A�%�A�s�A�+A�c AԤ�AӤ�A��.A�K�A�4A��A��AĵA��A��A��A��A�.IA�� A���A�1�A�<6A��}A�\�A���A��4A�:^A��A��A���A���A�@�A��A��A��}A��A�1�A�MA�iA�U�A���A�[�A�/�A���A��A�(A�V�A��4A�IA��5A{�TAw��AsffAm�0Aj��Ag��AdĜA`�A\�uAY��AU�mAS|AP��API�AN�oAK�jAGbNAF$�AD��AA�A?��A>XA<��A;F�A9A4�HA4VmA4	�A2�A2'�A0یA0�A/�[A/&A./�A-�A,��A,sA,�A,&A*�IA)
=A'��A&ںA&S&A%�A#�A#��A#v`A#.IA"t�A!�A!]dA x�A dZA!E�A"S&A"ɆA"�A!h
A �~A�`A �A�Ae�A�AD�A��A&AJ�A-�A;A��Al�A	lA��Ap�Av�A�A/�AA	lA��A�AS�Am]AzxA�KA1'A;�A��A�A��A��A��A�$A]�AMjA iA8�A�A�	A͟A&A�,AB�A.�A�A/A
_�A	��A	H�A�MAr�A��A�A;dAdZAl�A��A��A�AA��A:�A��A��Au�A^5A�A�AJ#AA خA ��@���@��3@���@���@�M�@�ݘ@��g@���@�˒@�Ĝ@�$�@��a@�p;@�%F@��@�6z@�:*@�@�˒@��@�%F@�M@��@�8�@��o@�b�@��@�A�@��@�	l@���@�0U@��@���@�C@�T�@��@�U2@�.�@�!@�=@��@��@���@��@��@�C-@�g�@�l�@���@ߙ�@�B�@�Ov@ݲ-@�s�@��@ܛ�@�˒@���@��@ٹ�@أ@�M@��@��@�^�@���@���@�Ĝ@Թ$@��@Ӑ�@�X�@�@�@Ѯ@�qv@��@�Ɇ@�y>@υ@�xl@�{@���@�>�@�@���@��r@�خ@͜�@�<6@�w�@˓@�� @���@�v`@�l�@�)�@ɠ�@�6z@Ȱ�@�|�@�IR@�)_@���@�W�@�@�n/@��@Ĝx@�N�@�@�hs@���@µ�@r@�:*@���@��~@�O@��@�ߤ@��@��z@��@��@�4@��6@�J�@�-�@��@�-w@��@��]@�͟@��B@�C@��@��A@�G@�j�@�w2@�k�@�/@�Ĝ@��@���@�j@��@���@�S�@�?@�_@��K@��@�|�@�خ@��.@��w@�F@��@���@��L@��R@���@�Ta@�7@���@��@���@�M@���@���@��@��U@��1@��u@�_@�x�@���@�H�@�(�@�� @�"�@�V�@�"h@��@��@���@��@���@���@�p;@�N�@�1�@��@�X�@���@���@�V�@�Ft@��.@�(�@�ݘ@��@�U�@���@��E@���@��@���@�O�@��@�h
@�#:@�{@�/�@�j�@���@�~�@�1�@�{@���@�5�@�	l@��@��$@�c @�-�@��m@�s�@��@��@�5?@� �@��H@���@�E9@��@��@���@�[�@���@�Q�@�Z�@�W?@��@���@�6@��@�)_@���@���@�h
@�^5@�W�@�6@��@�=@���@�5?@���@��y@��A@�Z�@�?@���@���@���@�a@�8@�+@���@�Ov@��K@�e,@��K@��e@��@�Ta@�8�@�(�@��@��#@��"@�n/@�K�@���@���@���@��=@��4@�W?@��@�i�@�D�@� �@��;@�P�@��@��@���@��@�{J@�]d@�Q@�u�@��@��h@���@���@��@�<�@��@��q@�K�@�+@�!�@�@�ی@��m@��R@��1@�{�@�c @�x@���@���@�j�@�b�@�;d@�V@��`@��[@���@�z�@�Z�@�8�@���@�ƨ@���@�s�@�T�@��@���@��$@��O@��@�]d@��@���@�?}@���@�͟@��9@��@�_@�<�@�*�@��@�$@U�@o@~�@~�6@~$�@}a�@}V@|�u@|C-@{�W@{��@{�P@{;d@z�'@z�@y@y��@ys�@y?}@x�?@xM@w�@w�@w�4@w�@v��@uj@t�$@t��@th�@s˒@s&@rJ�@qQ�@p�e@ph�@p7�@pG@o{J@n�}@nc @nO@m�@m@mj@l�[@l_@l9X@lM@l1@k�
@k,�@j�X@j;�@i�@i�M@ip�@iS&@iq@h�@hm�@h"h@h�@g�}@gK�@f�2@f��@f4@e^�@d��@d]d@c�@c��@c|�@cg�@c i@b;�@a�T@a@a�-@ac@`�?@`bN@_��@_�@@_l�@_/�@^�8@^�@^5?@^ �@]@]k�@\ѷ@\�@[�@[\)@[Mj@[
=@Z��@Y�o@Yԕ@Yx�@Y?}@Y;@X��@XtT@XPH@X"h@We�@W�@V�H@V�}@V�A@VH�@U�j@Uc@U�@T�@T�@S�[@S��@SO@S"�@R�@RM�@Q��@Qhs@QIR@Q2a@Q!�@Q	l@P�|@P�`@P��@O�@N�M@N5?@MVm@M	l@LĜ@L:�@K��@K6z@J��@J�\@J.�@I��@H��@H�u@Hj@H�@G{J@G@F��@F8�@E�@E�"@Ea�@EQ�@E%@D��@DG@C�@C|�@CC�@B��@B�@A��@A��@A|@A8�@@�@@~(@@�@?��@?@O@>�x@>�@=�@=s�@=q@<e�@;�+@;�*@;��@;|�@;�@:~�@9�T@9x�@9%@8�[@8�@8z�@7RT@7,�@7�@6��@6V@6W�@6M�@6H�@6)�@5@4��@4Xy@3�@3&@3�@2�y@2��@2�1@2;�@1�Z@1��@1T�@1�@0�@0�@0]d@0,=@0�@/�}@/;d@.�R@.H�@.J@-�#@-@-�t@-�~@-IR@-F@-%@,֡@,�9@,z�@,7�@+�@+��@+t�@+=@*�M@*�\@*��@*l�@*$�@)�@)�=@)�@(��@(h�@(%�@("h@'��@'��@'�@@'x@'>�@&ں@&s�@&_�@&L0@&)�@%�Z@%��@%w2@%k�@%m]@%}�@%x�@%��@%�h@%k�@%@@$�[@$�z@$��@$PH@$(�@$	�@$@$	�@#��@#�Q@#�0@#X�@#�@"�@"�R@"�b@"kQ@"-@!�@!s�@!N<@!V@ ��@ bN@��@��@�6@s@C@ں@�'@��@i�@M�@@�@�@��@��@w2@F@�v@oi@-�@7@~@@��@�
@�f@&@�@
=@�@�@��@�@\�@;�@6�@e@�o@��@�d@@�X@rG@N<@%F@�@�@��@�@l"@!@��@خ@�@��@qv@A�@9�@ i@��@�}@~�@YK@GE@	@��@^�@#�@�	@�@��@�@m�@C-@M@@�]@�@خ@�6@��@�k@��@l�@W?@9�@C@�@��@�h@\�@�#@�@��@s�@Vm@�P@ی@�@�u@M@�@�@�@��@~�@e�@�s@�A@YK@6�@C�@J�@=q@�>@u�@a�@��@�_@m�@!@�@�@G@��@�*@{J@F�@�@ i1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B��B��B��B��B��B��B��B�_B�EB�+B�_B�yB�yB��B�KB�B��B�KB��B�1B�#B��B��B�MB	]dB	Z�B	Y�B	V�B	UB	S�B	Q B	PHB	OB	9>B	3�B	�B	�B	 OB�hB�B��B�$B	!B	C�B	bhB	��B	�oB	�	B	ňB	��B
�B
��B
�~B
�-B
��B
��B
��B
�B
��B
� B
��B
�uB
jB
V�B
��B
��B
��B
o�B
]B
]IB
,�B
 �B
G_B
hsB
p;B
vB
b4B
FB
jB
�B	��B	�$B	�B	��B	��B	m�B	T,B	@4B	(>B	
#B�^B�B��B�uB�\B�dB�aB�'B�*B�@B��B��B�B�CB��B�SB��B�dB��B�+B��BǔB˒B��B�pB�^BŢB�?B�,B�MB�B�B�B�B��B�B�0B��B��B��B	�B	
=B	�B	jB	'�B	9�B	RoB	p�B	��B	�rB	�6B	�B	��B	~wB	|�B	�IB	�B	�B	��B	i�B	c�B	g�B	lqB	nB	oB	t�B	|B	�B	�<B	��B	��B	��B	�zB	��B	�B	�lB	��B	�YB	��B	��B	�dB	��B	�qB	��B	�tB	ĜB	��B	ƎB	�7B	ɆB	ȴB	��B	��B	āB	ðB	�%B	��B	��B	ƨB	�tB	�SB	��B	��B	ʌB	��B	�KB	ňB	B	��B	��B	�(B	��B	��B	�B	�mB	�yB	��B	��B	�!B	��B	��B	ߊB	߾B	�HB	�-B	ޞB	ںB	�mB	�uB	�B	̈́B	�6B	�BB	бB	�kB	�+B	�?B	�YB	׍B	�B	�@B	ӏB	�aB	�B	�yB	�;B	�'B	�B	�nB	�B	��B	�B	�tB	�VB	ܒB	��B	�B	��B	�HB	�B	�FB	�FB	�B	�B	��B	�>B	�B	��B	�B	�B	�@B	�B	�TB	��B	�B	��B	�nB	�B	�B	�TB	�B	�ZB	�B	�B	�NB	�NB	�`B	�TB	�B	��B	߾B	�!B	�5B	߾B	��B	�B	�B	�zB	�B	�@B	� B	�hB	�B	�B	�B	��B	�&B	� B	��B	�B	�:B	��B	��B	�B	�OB	�B	�B	��B	��B	�oB	�B	�B	�AB	�B	�5B	�IB	��B	�B	�B	��B	�B	�iB	��B	� B	�B	�OB	�OB	�UB	��B	�|B	�|B	��B	�B	�9B	�TB	�B	�B	��B	��B	�nB	�?B	��B	��B	�fB	��B	�B	�B	�8B	��B	��B	��B	�dB	�wB	��B	�(B	�BB	��B	�B
 �B
uB
�B
�B
�B
�B
�B
�B
�B

rB
B
)B
B

	B
�B
�B
�B
YB
%B
tB
�B

#B

rB
	�B
	B

	B
	B
�B
1B
�B
B
YB
tB
�B
zB
�B
�B
�B
�B
�B
�B
B
�B
B
�B
%B
?B
�B
	B
�B
�B
B
�B
�B
EB
_B
	B
�B
	7B
�B
B
PB
VB
\B
�B
�B
�B
�B
�B
}B
 B
�B
�B
TB
B
B
�B
�B
2B
2B
�B
$B
�B

B
YB
�B
sB
�B
yB
�B
KB
B
�B
�B
�B
�B
�B
�B
QB
B
�B
B
7B
	B
�B
�B
�B
xB
�B
�B
�B
�B
�B
�B
�B
B
dB
/B
dB
�B
�B
�B
!B
VB
�B
�B
�B
 B
 'B
 'B
 �B
!|B
!�B
!�B
"�B
"hB
"�B
"�B
"�B
"�B
"�B
# B
#TB
#�B
#�B
$�B
%�B
%�B
&2B
&�B
'8B
)�B
*�B
+B
+B
+B
,B
+�B
+�B
,"B
,�B
*�B
(�B
)*B
*B
,�B
-CB
-wB
-�B
-�B
-�B
-�B
/iB
/�B
/�B
/�B
0B
/�B
/�B
/�B
/�B
/�B
/�B
0�B
1'B
1vB
1�B
1vB
1�B
1�B
2-B
2B
2GB
2|B
2|B
2�B
2�B
3B
3B
3MB
3�B
3�B
3�B
3�B
3�B
3�B
3�B
4TB
4�B
4�B
5?B
5?B
5tB
5ZB
5�B
5�B
5�B
6+B
6zB
6�B
6�B
6�B
6�B
7B
7�B
7fB
7�B
8B
8RB
8RB
8RB
8lB
8�B
8�B
9$B
9$B
9$B
9>B
9�B
9�B
9�B
9�B
9�B
:B
:*B
;B
;JB
;0B
;0B
;�B
<B
<�B
=<B
=VB
=qB
=�B
=�B
=�B
>BB
>BB
>�B
>�B
>�B
>�B
?.B
?cB
?}B
?}B
?}B
?�B
@ B
@B
@�B
@�B
@�B
AB
AB
A;B
AUB
A�B
A�B
A�B
A�B
B[B
BuB
B�B
B�B
C-B
C�B
C�B
DMB
DgB
DgB
DMB
D�B
ESB
EmB
E�B
EmB
E�B
FB
F?B
F�B
F�B
F�B
F�B
GB
G+B
G�B
G�B
G�B
G�B
H1B
H1B
IB
IB
H�B
I7B
I�B
I�B
I�B
J#B
J#B
J#B
J=B
JrB
JXB
JrB
K)B
KDB
KDB
KxB
KxB
K�B
K�B
LB
L0B
L~B
L�B
MB
MB
MB
MB
MPB
M�B
NB
NVB
N<B
NpB
NpB
N�B
NpB
N<B
N�B
OBB
O�B
PHB
P�B
Q B
Q B
QhB
Q�B
RB
RTB
RTB
R�B
R�B
S[B
S�B
S�B
S�B
T,B
T�B
T�B
UB
UMB
UgB
UgB
UgB
U�B
VB
V9B
V9B
V�B
W
B
W�B
W�B
XB
XyB
XyB
X�B
X�B
Y1B
YB
Y�B
ZB
ZkB
Z�B
[#B
[=B
[�B
\CB
\�B
\�B
\�B
\�B
]/B
]�B
]�B
]�B
^jB
^�B
^�B
`�B
abB
a-B
a�B
a�B
a�B
a�B
a�B
a�B
a�B
bNB
bNB
bNB
b�B
cTB
c�B
c�B
c�B
c�B
dtB
d�B
eFB
e�B
e�B
e�B
e,B
eB
e,B
e�B
e�B
e�B
fLB
f�B
g8B
g�B
h
B
h>B
h>B
h
B
hsB
hsB
h>B
g�B
g�B
g�B
h
B
hXB
h�B
h>B
h�B
i*B
i�B
jB
i�B
i�B
jB
kkB
k�B
k�B
k�B
kkB
kQB
k6B
kkB
k�B
k�B
k�B
l=B
lWB
lWB
l�B
l�B
l�B
mB
l�B
m�B
nIB
n�B
o5B
o5B
o5B
oB
o5B
p!B
p�B
q[B
qB
qB
q'B
q'B
q[B
q�B
q�B
q�B
q�B
q�B
q�B
qvB
q�B
q�B
qvB
qvB
q�B
q�B
q�B
rGB
rB
r-B
rB
q�B
rB
raB
r�B
r�B
r|B
rGB
r�B
r�B
r�B
r�B
r�B
s3B
s3B
s�B
s�B
tTB
t�B
t�B
t�B
t�B
u%B
uZB
uZB
utB
utB
u�B
u�B
u�B
v+B
vFB
v`B
v�B
v�B
v�B
v�B
v�B
v�B
w2B
wLB
wfB
w�B
wLB
wfB
w2B
w�B
w�B
xB
xB
xB
xB
x8B
x�B
x�B
x�B
x�B
x�B
x�B
y	B
y	B
y$B
yrB
y�B
y�B
z*B
z*B
zxB
z�B
z�B
z�B
z�B
z�B
z�B
z�B
{0B
{B
{JB
{JB
{JB
{B
{B
{�B
{�B
{�B
{�B
{�B
|PB
|�B
|�B
|�B
}"B
|�B
}�B
}�B
}�B
}�B
~(B
~]B
~�B
~wB
~�B
cB
}B
� B
�B
�4B
��B
��B
�'B
�uB
�[B
�AB
�'B
�B
��B
�B
�{B
��B
��B
�3B
��B
��B
��B
��B
�?B
�%1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B��B��B��B��B��B��B��B�_B�EB�+B�_B�yB�yB��B�KB�B��B�KB��B�1B�#B��B��B�MB	]dB	Z�B	Y�B	V�B	UB	S�B	Q B	PHB	OB	9>B	3�B	�B	�B	 OB�hB�B��B�$B	!B	C�B	bhB	��B	�oB	�	B	ňB	��B
�B
��B
�~B
�-B
��B
��B
��B
�B
��B
� B
��B
�uB
jB
V�B
��B
��B
��B
o�B
]B
]IB
,�B
 �B
G_B
hsB
p;B
vB
b4B
FB
jB
�B	��B	�$B	�B	��B	��B	m�B	T,B	@4B	(>B	
#B�^B�B��B�uB�\B�dB�aB�'B�*B�@B��B��B�B�CB��B�SB��B�dB��B�+B��BǔB˒B��B�pB�^BŢB�?B�,B�MB�B�B�B�B��B�B�0B��B��B��B	�B	
=B	�B	jB	'�B	9�B	RoB	p�B	��B	�rB	�6B	�B	��B	~wB	|�B	�IB	�B	�B	��B	i�B	c�B	g�B	lqB	nB	oB	t�B	|B	�B	�<B	��B	��B	��B	�zB	��B	�B	�lB	��B	�YB	��B	��B	�dB	��B	�qB	��B	�tB	ĜB	��B	ƎB	�7B	ɆB	ȴB	��B	��B	āB	ðB	�%B	��B	��B	ƨB	�tB	�SB	��B	��B	ʌB	��B	�KB	ňB	B	��B	��B	�(B	��B	��B	�B	�mB	�yB	��B	��B	�!B	��B	��B	ߊB	߾B	�HB	�-B	ޞB	ںB	�mB	�uB	�B	̈́B	�6B	�BB	бB	�kB	�+B	�?B	�YB	׍B	�B	�@B	ӏB	�aB	�B	�yB	�;B	�'B	�B	�nB	�B	��B	�B	�tB	�VB	ܒB	��B	�B	��B	�HB	�B	�FB	�FB	�B	�B	��B	�>B	�B	��B	�B	�B	�@B	�B	�TB	��B	�B	��B	�nB	�B	�B	�TB	�B	�ZB	�B	�B	�NB	�NB	�`B	�TB	�B	��B	߾B	�!B	�5B	߾B	��B	�B	�B	�zB	�B	�@B	� B	�hB	�B	�B	�B	��B	�&B	� B	��B	�B	�:B	��B	��B	�B	�OB	�B	�B	��B	��B	�oB	�B	�B	�AB	�B	�5B	�IB	��B	�B	�B	��B	�B	�iB	��B	� B	�B	�OB	�OB	�UB	��B	�|B	�|B	��B	�B	�9B	�TB	�B	�B	��B	��B	�nB	�?B	��B	��B	�fB	��B	�B	�B	�8B	��B	��B	��B	�dB	�wB	��B	�(B	�BB	��B	�B
 �B
uB
�B
�B
�B
�B
�B
�B
�B

rB
B
)B
B

	B
�B
�B
�B
YB
%B
tB
�B

#B

rB
	�B
	B

	B
	B
�B
1B
�B
B
YB
tB
�B
zB
�B
�B
�B
�B
�B
�B
B
�B
B
�B
%B
?B
�B
	B
�B
�B
B
�B
�B
EB
_B
	B
�B
	7B
�B
B
PB
VB
\B
�B
�B
�B
�B
�B
}B
 B
�B
�B
TB
B
B
�B
�B
2B
2B
�B
$B
�B

B
YB
�B
sB
�B
yB
�B
KB
B
�B
�B
�B
�B
�B
�B
QB
B
�B
B
7B
	B
�B
�B
�B
xB
�B
�B
�B
�B
�B
�B
�B
B
dB
/B
dB
�B
�B
�B
!B
VB
�B
�B
�B
 B
 'B
 'B
 �B
!|B
!�B
!�B
"�B
"hB
"�B
"�B
"�B
"�B
"�B
# B
#TB
#�B
#�B
$�B
%�B
%�B
&2B
&�B
'8B
)�B
*�B
+B
+B
+B
,B
+�B
+�B
,"B
,�B
*�B
(�B
)*B
*B
,�B
-CB
-wB
-�B
-�B
-�B
-�B
/iB
/�B
/�B
/�B
0B
/�B
/�B
/�B
/�B
/�B
/�B
0�B
1'B
1vB
1�B
1vB
1�B
1�B
2-B
2B
2GB
2|B
2|B
2�B
2�B
3B
3B
3MB
3�B
3�B
3�B
3�B
3�B
3�B
3�B
4TB
4�B
4�B
5?B
5?B
5tB
5ZB
5�B
5�B
5�B
6+B
6zB
6�B
6�B
6�B
6�B
7B
7�B
7fB
7�B
8B
8RB
8RB
8RB
8lB
8�B
8�B
9$B
9$B
9$B
9>B
9�B
9�B
9�B
9�B
9�B
:B
:*B
;B
;JB
;0B
;0B
;�B
<B
<�B
=<B
=VB
=qB
=�B
=�B
=�B
>BB
>BB
>�B
>�B
>�B
>�B
?.B
?cB
?}B
?}B
?}B
?�B
@ B
@B
@�B
@�B
@�B
AB
AB
A;B
AUB
A�B
A�B
A�B
A�B
B[B
BuB
B�B
B�B
C-B
C�B
C�B
DMB
DgB
DgB
DMB
D�B
ESB
EmB
E�B
EmB
E�B
FB
F?B
F�B
F�B
F�B
F�B
GB
G+B
G�B
G�B
G�B
G�B
H1B
H1B
IB
IB
H�B
I7B
I�B
I�B
I�B
J#B
J#B
J#B
J=B
JrB
JXB
JrB
K)B
KDB
KDB
KxB
KxB
K�B
K�B
LB
L0B
L~B
L�B
MB
MB
MB
MB
MPB
M�B
NB
NVB
N<B
NpB
NpB
N�B
NpB
N<B
N�B
OBB
O�B
PHB
P�B
Q B
Q B
QhB
Q�B
RB
RTB
RTB
R�B
R�B
S[B
S�B
S�B
S�B
T,B
T�B
T�B
UB
UMB
UgB
UgB
UgB
U�B
VB
V9B
V9B
V�B
W
B
W�B
W�B
XB
XyB
XyB
X�B
X�B
Y1B
YB
Y�B
ZB
ZkB
Z�B
[#B
[=B
[�B
\CB
\�B
\�B
\�B
\�B
]/B
]�B
]�B
]�B
^jB
^�B
^�B
`�B
abB
a-B
a�B
a�B
a�B
a�B
a�B
a�B
a�B
bNB
bNB
bNB
b�B
cTB
c�B
c�B
c�B
c�B
dtB
d�B
eFB
e�B
e�B
e�B
e,B
eB
e,B
e�B
e�B
e�B
fLB
f�B
g8B
g�B
h
B
h>B
h>B
h
B
hsB
hsB
h>B
g�B
g�B
g�B
h
B
hXB
h�B
h>B
h�B
i*B
i�B
jB
i�B
i�B
jB
kkB
k�B
k�B
k�B
kkB
kQB
k6B
kkB
k�B
k�B
k�B
l=B
lWB
lWB
l�B
l�B
l�B
mB
l�B
m�B
nIB
n�B
o5B
o5B
o5B
oB
o5B
p!B
p�B
q[B
qB
qB
q'B
q'B
q[B
q�B
q�B
q�B
q�B
q�B
q�B
qvB
q�B
q�B
qvB
qvB
q�B
q�B
q�B
rGB
rB
r-B
rB
q�B
rB
raB
r�B
r�B
r|B
rGB
r�B
r�B
r�B
r�B
r�B
s3B
s3B
s�B
s�B
tTB
t�B
t�B
t�B
t�B
u%B
uZB
uZB
utB
utB
u�B
u�B
u�B
v+B
vFB
v`B
v�B
v�B
v�B
v�B
v�B
v�B
w2B
wLB
wfB
w�B
wLB
wfB
w2B
w�B
w�B
xB
xB
xB
xB
x8B
x�B
x�B
x�B
x�B
x�B
x�B
y	B
y	B
y$B
yrB
y�B
y�B
z*B
z*B
zxB
z�B
z�B
z�B
z�B
z�B
z�B
z�B
{0B
{B
{JB
{JB
{JB
{B
{B
{�B
{�B
{�B
{�B
{�B
|PB
|�B
|�B
|�B
}"B
|�B
}�B
}�B
}�B
}�B
~(B
~]B
~�B
~wB
~�B
cB
}B
� B
�B
�4B
��B
��B
�'B
�uB
�[B
�AB
�'B
�B
��B
�B
�{B
��B
��B
�3B
��B
��B
��B
��B
�?B
�%1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104849  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604172546  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604172546  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604172547                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605022555  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605022555  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610131506                      G�O�G�O�G�O�                