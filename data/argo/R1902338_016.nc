CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2020-06-03T00:38:04Z creation;2020-06-03T00:38:06Z conversion to V3.1      
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
_FillValue                 �  I    PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p0   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �@   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �(   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �P   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ݐ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �<   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �L   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �P   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �`   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �d   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �h   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �lArgo profile    3.1 1.2 19500101000000  20200603003804  20200603005524  1902338                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA                                  2B  A   NAVIS_A                         0922                            ARGO                            863 @�5��8�1   @�68�9�C�V�@E	7KƧ�1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @���@���A   A   A@  A`  A�  A���A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CK�fCN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$y�D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<y�D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�&f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@�
>A�A!�AA�Aa�A��\A�\)A��\A��\A��\AЏ\A��\A��\B G�BG�BG�BG�B G�B(G�B0G�B8G�B@G�BHG�BPG�BXG�B`G�BhG�BpG�BxG�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�C �C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CK�RCN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D {D �{D{D�{D{D�{D�D�{D{D�{D{D�{D{D�{D{D�{D{D�{D	{D	�{D
{D
�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D {D �{D!{D!�{D"{D"�{D#{D#�{D${D$~D%{D%�{D&{D&�{D'{D'�{D({D(�{D){D)�{D*{D*�{D+{D+�{D,{D,�{D-{D-�{D.{D.�{D/{D/�{D0{D0�{D1{D1�{D2{D2�{D3{D3�{D4{D4�{D5{D5�{D6{D6�{D7{D7�{D8{D8�{D9{D9�{D:{D:�{D;{D;�{D<{D<~D={D=�{D>{D>�{D?{D?�{D@{D@�{DA{DA�{DB{DB�{DC{DC�{DD{DD�{DE{DE�{DF{DF�{DG{DG�{DH{DH�{DI{DI�{DJ{DJ�{DK{DK�{DL{DL�{DM{DM�{DN{DN�{DO{DO�{DP{DP�{DQ{DQ�{DR{DR�{DS{DS�{DT{DT�{DU{DU�{DV{DV�{DW{DW�{DX{DX�{DY{DY�{DZ{DZ�{D[{D[�{D\{D\�{D]{D]�{D^{D^�{D_{D_�{D`{D`�{Da{Da�{Db{Db�{Dc{Dc�{Dd{Dd�{De{De�{Df{Df�{Dg{Dg�{Dh{Dh�{Di{Di�{Dj{Dj�{Dk{Dk�{Dl{Dl�{Dm{Dm�{Dn{Dn�{Do{Do�{Dp{Dp�{Dq{Dq�{Dr{Dr�{Ds{Ds�{Dt{Dt�{Du{Du�{Dv{Dv�{Dw{Dw�{Dx{Dx�{Dy{Dy�{Dz{Dz�{D{{D{�{D|{D|�{D}{D}�{D~{D~�{D{D�{D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D��
D�B=D��=D��=D�=D�B=D=D��=D�=D�B=DÂ=D��=D�=D�B=DĂ=D��=D�=D�B=Dł=D��=D�=D�B=DƂ=D��=D�=D�B=Dǂ=D��=D�=D�B=DȂ=D��=D�=D�B=Dɂ=D��=D�=D�B=Dʂ=D��=D�=D�B=D˂=D��=D�=D�B=D̂=D��=D�=D�B=D͂=D��=D�=D�B=D΂=D��=D�=D�B=Dς=D��=D�=D�B=DЂ=D��=D�=D�B=Dт=D��=D�=D�B=D҂=D��=D�=D�B=Dӂ=D��=D�=D�B=DԂ=D��=D�=D�B=DՂ=D��=D�=D�B=Dւ=D��=D�=D�B=Dׂ=D��=D�=D�B=D؂=D��=D�=D�B=Dق=D��=D�=D�B=Dڂ=D��=D�=D�B=Dۂ=D��=D�=D�B=D܂=D��=D�=D�B=D݂=D��=D�=D�B=Dނ=D��=D�=D�B=D߂=D��=D�=D�B=D��=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D��=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D��=D��=D�=D�B=D��pD��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�pD�(�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A^$�A^$�A^ �A^JA^{A^bA^  A]��A]33A]�A]�A\��A\��A\�A\�A\�`A\�yA\��A\ĜA\ĜA\�RA\�!A\�9A\�9A\�jA\�jA\z�A\9XA[��A[�AZ~�AXQ�AT  AS�-AS��AS��AS`BASO�ASS�AS`BASt�AS�AS|�ASp�ASK�ASC�ASK�ASO�ASO�ASK�AS?}AS33AS+AS&�AS�AS�AS�AS/AS��ATI�AU�AW�^AVȴAUS�AU&�AU�ATZASl�ARȴARQ�AQ��AP��AOp�AOoANr�AM��AMdZAL��AK��AK�AK�AK��AJ��AH9XAF  AEƨAE�AE?}AE+AEoAD��AD�9AD-AC�7ABbNAB{AA��AA33A@�A?��A?p�A>�`A=�-A=+A<�RA<�jA=
=A=;dA=
=A=+A=hsA=dZA<�yA<^5A;A;�A:��A:z�A:(�A9�A9�A9��A9?}A8��A8bNA7l�A6�yA6��A6bA5p�A4�A4$�A3x�A2��A1��A1dZA0�HA/�A.��A-�A+�hA+%A*��A*^5A*-A)�PA(��A( �A'A'�A&��A&ĜA&�jA&�!A&r�A& �A%�wA%l�A%%A$v�A$1A#A#��A#S�A"�A"��A"�A"1'A"bA!��A!�#A!�
A!�
A!�FA!�A!dZA!?}A �A ��A ȴA =qA   A�mA�#AAl�A��A�9A�+A�Av�AI�A�Al�A�A�RAjA9XA�wAG�A�9AffA5?A�mA�PA&�A��A^5AC�A��A��A33Av�A�A��A`BA;dA"�A&�A+A�A��Ar�A�A�TA��A�AoA��A��AM�A�PA��A�9A��AZA�Al�A
��A
��A
�A
n�A
JA	ƨA	"�A  A7LA�RA-A�;A�
A��AA33AbA�wA�hA"�A�uA�A ��@�dZ@�V@���@�?}@���@�9X@�+@��@�V@���@���@��
@��y@�M�@�1@�R@���@�?}@�Q�@�G�@�j@��@�D@�Z@�A�@��
@�l�@���@��@���@�&�@�\)@ݺ^@���@�I�@�b@��@�&�@�9X@�
=@�-@�`B@�%@�z�@Ӆ@ҸR@��@�Z@Ϯ@�t�@�+@�v�@��T@�/@���@�Ĝ@̓u@�Z@�+@ʏ\@���@�7L@��`@���@�ƨ@�Ĝ@��H@�{@���@�G�@��7@��@�9X@���@�5?@�=q@�-@��@���@�?}@�bN@��F@�M�@�7L@�1'@�|�@�\)@�ƨ@���@��-@�Q�@��@�p�@���@��@�E�@�{@�J@��@�p�@��@��@�Z@�Q�@�bN@��@�S�@�@���@�`B@�X@�G�@�&�@��@��`@���@�9X@���@�S�@���@�J@��-@��h@�G�@��@���@�  @�|�@��H@���@�/@��`@�A�@���@���@��P@���@��P@��@�S�@�V@�M�@��#@��7@��`@� �@��m@��F@�|�@���@���@��\@���@��#@���@�p�@�x�@���@�p�@���@��/@�j@��@��@���@��w@��y@�ff@�-@�@�O�@�&�@��u@�+@�~�@�5?@��@���@�x�@��@��u@��@���@���@���@�%@��j@�z�@��@��P@�S�@�ƨ@�  @��w@�\)@���@�-@���@��@��@�I�@��@���@���@��@�x�@���@�Ĝ@��D@��u@�r�@� �@� �@���@�\)@�
=@��H@��@�dZ@�C�@��!@��@���@��h@���@��9@���@���@�Z@�w@��@��@~�@~@}�T@}�T@}��@}?}@{�m@{o@z-@y�7@x�`@xQ�@w�@w�P@w�P@w+@v�y@v�y@vE�@t�/@sƨ@r��@pb@k�F@i�@hbN@g|�@g
=@f��@f5?@fE�@f{@ep�@ep�@e�@f@fȴ@h  @g+@e/@d�D@d�@c�m@d(�@dZ@dZ@dZ@c�
@a��@`�`@a�@ax�@ax�@a��@c"�@d�/@c�
@c�F@d�j@b��@`bN@^��@^5?@]��@_�P@b�!@b=q@ax�@^ȴ@_l�@_l�@_�w@`Q�@`�@`r�@` �@_�@_|�@_
=@^5?@\�/@\�/@\�j@\�D@\j@[��@Z��@Y�#@Y��@Y��@Y��@Y�^@ZJ@ZM�@Z�@Y�@Y��@ZJ@Z�@Z=q@Y��@X�@X �@Xb@X  @W�@W�P@W
=@Vv�@U�@U��@U�h@U�@U�-@U�-@U��@UO�@V�R@W��@X  @W��@W�@VE�@T��@T(�@TZ@Vȴ@Y&�@Y��@Y�@Y��@Yx�@XĜ@XQ�@Y��@YG�@X��@X �@W�@U��@U�T@U?}@S��@Sƨ@T1@S�
@T(�@T�D@T��@T�@T�@U�@U�@V@V{@VE�@Vv�@Vv�@VE�@V$�@U�@U�-@U�h@U�@UV@UV@T��@T�@T�/@Tj@S�
@St�@R�@R��@Rn�@R=q@Q�#@Qx�@QG�@Q7L@P��@P�9@P �@O�@O��@O�@N�R@Nȴ@N�@N�@Nȴ@N�R@Nv�@N{@M�T@M�-@M�@M�@M?}@MV@Lj@L9X@L1@L1@L9X@Kƨ@K�F@Kt�@K"�@Ko@K@J�\@JM�@J=q@J=q@JJ@I�@I�@I��@Ihs@I7L@I%@H��@H��@H�@HQ�@H �@H  @H  @G��@G��@G��@G|�@G+@F�R@F��@FE�@E�T@E�h@E�@D�D@C��@C�@B��@B�\@Bn�@B=q@A�@A�#@A�#@A�^@A�7@AX@A�@@��@@�u@@1'@@ �@@b@?��@?�P@?�P@?|�@?\)@?\)@?;d@>��@>��@>�+@>ff@>ff@>ff@>V@>E�@>{@=��@=��@=�h@=�@=p�@=�@=p�@=`B@=O�@=?}@=�@=�@=V@<�@<�@<�@<�@<�@<�/@<��@<�@<z�@<I�@<1@;��@;�
@;ƨ@;�F@;��@;��@;��@;t�@;S�@;S�@;S�@;C�@;33@;o@:�H@:��@:��@:��@:��@:��@:�\@:~�@:~�@:n�@:^5@:^5@:^5@:M�@:-@:J@9��@9�#@9�7@9G�@97L@97L@97L@9&�@9&�@9&�@9�@8��@8��@8�u@8bN@81'@8 �@8b@8b@8  @7�;@7��@7��@7��@7�w@7l�@7\)@7K�@7�@6��@6�y@6�@6�R@6��@6�+@6ff@6V@65?@6@5�T@5��@5@5�-@5��@5�h@5�@5�@5�@5p�@5`B@5`B@5�@4�@4��@4��@4Z@4I�@49X@4(�@41@3��@3ƨ@3�@3t�@3C�@3@2�H@2�H@2��@2��@2��@2n�@2M�@2=q@2-@2-@2�@2�@2�@2J@2J@1��@1�#@1��@1��@1x�@1hs@1X@17L@1%@0��@0��@0��@0��@0��@0��@0�`@0�`@0��@0�9@0r�@0bN@0A�@0A�@01'@01'@0b@/�w@/��@/�P@/|�@/l�@/l�@/l�@/l�@/\)@/\)@/K�@/+@/�@/
=@.��@.��@.�y@.�R@.�R@.��@.�+@.�+@.ff@.ff@.ff@.V@.V@.V@.{@-�@-�T@-��@-@-@-��@-�h@-�h@-�@-p�@-`B@-O�@-/@-V@-V@-V@,��@,�/@,��@,�j@,�@,�D@,z�@,z�@,j@,(�@,(�@,(�@,�@+��@+��@+��@+��@+��@+�m@+�
@+�
@+�F@+��@+�@+�@+�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A^$�A^$�A^ �A^JA^{A^bA^  A]��A]33A]�A]�A\��A\��A\�A\�A\�`A\�yA\��A\ĜA\ĜA\�RA\�!A\�9A\�9A\�jA\�jA\z�A\9XA[��A[�AZ~�AXQ�AT  AS�-AS��AS��AS`BASO�ASS�AS`BASt�AS�AS|�ASp�ASK�ASC�ASK�ASO�ASO�ASK�AS?}AS33AS+AS&�AS�AS�AS�AS/AS��ATI�AU�AW�^AVȴAUS�AU&�AU�ATZASl�ARȴARQ�AQ��AP��AOp�AOoANr�AM��AMdZAL��AK��AK�AK�AK��AJ��AH9XAF  AEƨAE�AE?}AE+AEoAD��AD�9AD-AC�7ABbNAB{AA��AA33A@�A?��A?p�A>�`A=�-A=+A<�RA<�jA=
=A=;dA=
=A=+A=hsA=dZA<�yA<^5A;A;�A:��A:z�A:(�A9�A9�A9��A9?}A8��A8bNA7l�A6�yA6��A6bA5p�A4�A4$�A3x�A2��A1��A1dZA0�HA/�A.��A-�A+�hA+%A*��A*^5A*-A)�PA(��A( �A'A'�A&��A&ĜA&�jA&�!A&r�A& �A%�wA%l�A%%A$v�A$1A#A#��A#S�A"�A"��A"�A"1'A"bA!��A!�#A!�
A!�
A!�FA!�A!dZA!?}A �A ��A ȴA =qA   A�mA�#AAl�A��A�9A�+A�Av�AI�A�Al�A�A�RAjA9XA�wAG�A�9AffA5?A�mA�PA&�A��A^5AC�A��A��A33Av�A�A��A`BA;dA"�A&�A+A�A��Ar�A�A�TA��A�AoA��A��AM�A�PA��A�9A��AZA�Al�A
��A
��A
�A
n�A
JA	ƨA	"�A  A7LA�RA-A�;A�
A��AA33AbA�wA�hA"�A�uA�A ��@�dZ@�V@���@�?}@���@�9X@�+@��@�V@���@���@��
@��y@�M�@�1@�R@���@�?}@�Q�@�G�@�j@��@�D@�Z@�A�@��
@�l�@���@��@���@�&�@�\)@ݺ^@���@�I�@�b@��@�&�@�9X@�
=@�-@�`B@�%@�z�@Ӆ@ҸR@��@�Z@Ϯ@�t�@�+@�v�@��T@�/@���@�Ĝ@̓u@�Z@�+@ʏ\@���@�7L@��`@���@�ƨ@�Ĝ@��H@�{@���@�G�@��7@��@�9X@���@�5?@�=q@�-@��@���@�?}@�bN@��F@�M�@�7L@�1'@�|�@�\)@�ƨ@���@��-@�Q�@��@�p�@���@��@�E�@�{@�J@��@�p�@��@��@�Z@�Q�@�bN@��@�S�@�@���@�`B@�X@�G�@�&�@��@��`@���@�9X@���@�S�@���@�J@��-@��h@�G�@��@���@�  @�|�@��H@���@�/@��`@�A�@���@���@��P@���@��P@��@�S�@�V@�M�@��#@��7@��`@� �@��m@��F@�|�@���@���@��\@���@��#@���@�p�@�x�@���@�p�@���@��/@�j@��@��@���@��w@��y@�ff@�-@�@�O�@�&�@��u@�+@�~�@�5?@��@���@�x�@��@��u@��@���@���@���@�%@��j@�z�@��@��P@�S�@�ƨ@�  @��w@�\)@���@�-@���@��@��@�I�@��@���@���@��@�x�@���@�Ĝ@��D@��u@�r�@� �@� �@���@�\)@�
=@��H@��@�dZ@�C�@��!@��@���@��h@���@��9@���@���@�Z@�w@��@��@~�@~@}�T@}�T@}��@}?}@{�m@{o@z-@y�7@x�`@xQ�@w�@w�P@w�P@w+@v�y@v�y@vE�@t�/@sƨ@r��@pb@k�F@i�@hbN@g|�@g
=@f��@f5?@fE�@f{@ep�@ep�@e�@f@fȴ@h  @g+@e/@d�D@d�@c�m@d(�@dZ@dZ@dZ@c�
@a��@`�`@a�@ax�@ax�@a��@c"�@d�/@c�
@c�F@d�j@b��@`bN@^��@^5?@]��@_�P@b�!@b=q@ax�@^ȴ@_l�@_l�@_�w@`Q�@`�@`r�@` �@_�@_|�@_
=@^5?@\�/@\�/@\�j@\�D@\j@[��@Z��@Y�#@Y��@Y��@Y��@Y�^@ZJ@ZM�@Z�@Y�@Y��@ZJ@Z�@Z=q@Y��@X�@X �@Xb@X  @W�@W�P@W
=@Vv�@U�@U��@U�h@U�@U�-@U�-@U��@UO�@V�R@W��@X  @W��@W�@VE�@T��@T(�@TZ@Vȴ@Y&�@Y��@Y�@Y��@Yx�@XĜ@XQ�@Y��@YG�@X��@X �@W�@U��@U�T@U?}@S��@Sƨ@T1@S�
@T(�@T�D@T��@T�@T�@U�@U�@V@V{@VE�@Vv�@Vv�@VE�@V$�@U�@U�-@U�h@U�@UV@UV@T��@T�@T�/@Tj@S�
@St�@R�@R��@Rn�@R=q@Q�#@Qx�@QG�@Q7L@P��@P�9@P �@O�@O��@O�@N�R@Nȴ@N�@N�@Nȴ@N�R@Nv�@N{@M�T@M�-@M�@M�@M?}@MV@Lj@L9X@L1@L1@L9X@Kƨ@K�F@Kt�@K"�@Ko@K@J�\@JM�@J=q@J=q@JJ@I�@I�@I��@Ihs@I7L@I%@H��@H��@H�@HQ�@H �@H  @H  @G��@G��@G��@G|�@G+@F�R@F��@FE�@E�T@E�h@E�@D�D@C��@C�@B��@B�\@Bn�@B=q@A�@A�#@A�#@A�^@A�7@AX@A�@@��@@�u@@1'@@ �@@b@?��@?�P@?�P@?|�@?\)@?\)@?;d@>��@>��@>�+@>ff@>ff@>ff@>V@>E�@>{@=��@=��@=�h@=�@=p�@=�@=p�@=`B@=O�@=?}@=�@=�@=V@<�@<�@<�@<�@<�@<�/@<��@<�@<z�@<I�@<1@;��@;�
@;ƨ@;�F@;��@;��@;��@;t�@;S�@;S�@;S�@;C�@;33@;o@:�H@:��@:��@:��@:��@:��@:�\@:~�@:~�@:n�@:^5@:^5@:^5@:M�@:-@:J@9��@9�#@9�7@9G�@97L@97L@97L@9&�@9&�@9&�@9�@8��@8��@8�u@8bN@81'@8 �@8b@8b@8  @7�;@7��@7��@7��@7�w@7l�@7\)@7K�@7�@6��@6�y@6�@6�R@6��@6�+@6ff@6V@65?@6@5�T@5��@5@5�-@5��@5�h@5�@5�@5�@5p�@5`B@5`B@5�@4�@4��@4��@4Z@4I�@49X@4(�@41@3��@3ƨ@3�@3t�@3C�@3@2�H@2�H@2��@2��@2��@2n�@2M�@2=q@2-@2-@2�@2�@2�@2J@2J@1��@1�#@1��@1��@1x�@1hs@1X@17L@1%@0��@0��@0��@0��@0��@0��@0�`@0�`@0��@0�9@0r�@0bN@0A�@0A�@01'@01'@0b@/�w@/��@/�P@/|�@/l�@/l�@/l�@/l�@/\)@/\)@/K�@/+@/�@/
=@.��@.��@.�y@.�R@.�R@.��@.�+@.�+@.ff@.ff@.ff@.V@.V@.V@.{@-�@-�T@-��@-@-@-��@-�h@-�h@-�@-p�@-`B@-O�@-/@-V@-V@-V@,��@,�/@,��@,�j@,�@,�D@,z�@,z�@,j@,(�@,(�@,(�@,�@+��@+��@+��@+��@+��@+�m@+�
@+�
@+�F@+��@+�@+�@+�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B��B��B��B��B��B��BɺBĜB�}B�qB�qB�jB�jB�dB�dB�dB�dB�dB�^B�^B�XB�XB�XB�XB�^B�^B�LB�3B�B��B��Bz�BD�B:^B9XB9XB5?B49B49B5?B6FB7LB7LB6FB5?B7LB;dB=qB>wBB�BE�BF�BF�BF�BF�BG�BH�BJ�BS�Be`B��B"�BiyBk�BjBiyBiyBbNB_;B[#BXBQ�BF�BB�B>wB7LB49B.B �B!�B!�B�B�BB�fB�ZB�NB�;B�5B�/B�#B�B��B��BǮBĜBB�qB�RB�B�B��B��B��B��B��B��B��B��B��B�B�B�B��B��B��B��B��B��B��B��B��B��B�uB�VB�%B~�B� B|�Bw�Br�BjBbNB\)BR�BN�BJ�BB�B7LB'�B�B{BbB\BVB	7BB
��B
��B
��B
��BBBJBPBDBDBDBJB
=B%BB	7B+BBBB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
��B
��B
��B
��B
��B
�B
�B
�B
�B
�sB
�mB
�`B
�TB
�HB
�;B
�5B
�)B
�B
�B
��B
��B
ɺB
B
�wB
�dB
�LB
�3B
�'B
�!B
�B
�!B
�!B
�-B
�-B
�!B
�B
��B
��B
��B
��B
��B
��B
�{B
�bB
�PB
�+B
�%B
�B
�B
�B
~�B
{�B
z�B
y�B
x�B
w�B
u�B
r�B
m�B
hsB
ffB
cTB
aHB
aHB
`BB
_;B
^5B
XB
T�B
T�B
Q�B
L�B
H�B
C�B
=qB
:^B
8RB
7LB
5?B
33B
0!B
)�B
"�B
�B
�B
�B
�B
uB
bB
DB
+B
%B
B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�fB	�NB	�)B	�
B	��B	��B	��B	��B	��B	ȴB	ŢB	ĜB	B	��B	��B	�jB	�^B	�FB	�-B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�oB	�JB	�=B	�1B	�+B	�1B	�DB	�=B	�B	�B	�B	�7B	�JB	�JB	�DB	�1B	�B	� B	{�B	x�B	x�B	x�B	|�B	|�B	w�B	v�B	q�B	q�B	r�B	o�B	l�B	k�B	k�B	k�B	k�B	l�B	l�B	m�B	m�B	n�B	o�B	m�B	k�B	iyB	hsB	hsB	jB	l�B	m�B	o�B	p�B	p�B	q�B	q�B	q�B	o�B	o�B	o�B	o�B	o�B	p�B	n�B	n�B	m�B	n�B	m�B	m�B	m�B	m�B	m�B	m�B	m�B	m�B	m�B	m�B	n�B	p�B	q�B	q�B	r�B	r�B	r�B	r�B	r�B	r�B	r�B	s�B	s�B	u�B	u�B	t�B	u�B	w�B	x�B	}�B	{�B	z�B	{�B	{�B	~�B	�B	�%B	�%B	�+B	�%B	�B	�1B	�%B	�B	�B	�B	�B	�%B	�%B	�%B	�+B	�+B	�B	�B	�%B	�%B	�%B	�1B	�7B	�1B	�1B	�DB	�\B	�\B	�VB	�PB	�VB	�VB	�PB	�JB	�=B	�1B	�%B	�B	�B	�B	�B	�B	�B	�B	�B	�1B	�DB	�DB	�DB	�DB	�JB	�PB	�hB	�uB	�uB	�oB	�oB	�oB	�hB	�hB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�FB	�qB	��B	��B	ƨB	ÖB	��B	�}B	�}B	��B	ĜB	��B	��B	��B	��B	�B	�B	�#B	�/B	�5B	�5B	�;B	�;B	�;B	�5B	�5B	�)B	�)B	�/B	�)B	�)B	�)B	�)B	�/B	�5B	�5B	�;B	�;B	�HB	�NB	�NB	�TB	�TB	�ZB	�`B	�`B	�fB	�fB	�mB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B
  B
B
%B
%B
	7B
	7B
1B
	7B

=B
hB
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
�B
�B
�B
 �B
%�B
%�B
#�B
%�B
'�B
(�B
-B
/B
/B
0!B
1'B
49B
5?B
6FB
6FB
7LB
9XB
:^B
;dB
;dB
<jB
=qB
?}B
@�B
B�B
C�B
D�B
D�B
D�B
F�B
H�B
H�B
J�B
K�B
L�B
L�B
M�B
O�B
O�B
O�B
P�B
P�B
Q�B
P�B
Q�B
R�B
R�B
S�B
VB
VB
VB
W
B
YB
[#B
[#B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
^5B
aHB
bNB
cTB
dZB
dZB
e`B
ffB
hsB
iyB
iyB
iyB
jB
k�B
k�B
l�B
n�B
o�B
p�B
q�B
r�B
r�B
s�B
t�B
t�B
u�B
u�B
v�B
v�B
w�B
x�B
y�B
z�B
{�B
|�B
}�B
~�B
�B
�B
�B
�+B
�1B
�7B
�7B
�DB
�DB
�DB
�JB
�PB
�VB
�\B
�bB
�bB
�oB
�oB
�oB
�uB
�{B
�{B
�{B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�!B
�'B
�'B
�'B
�'B
�'B
�'B
�-B
�-B
�3B
�3B
�3B
�?B
�FB
�LB
�LB
�LB
�LB
�LB
�RB
�RB
�RB
�XB
�^B
�dB
�qB
�qB
�wB
�wB
�}B
��B
��B
��B
��B
��B
B
ÖB
ÖB
ĜB
ŢB
ŢB
ŢB
ƨB
ƨB
ǮB
ȴB
ȴB
ɺB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�
B
�
B
�B
�B
�B
�B
�B
�)B
�)B
�/B
�;B
�BB
�BB
�HB
�HB
�NB
�ZB
�ZB
�`B
�`B
�fB
�fB
�fB
�fB
�mB
�mB
�sB
�sB
�yB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B  B  B  BBBBBBBBBBBBB%B%B+B+B+B1B1B	7B	7B
=B
=B
=B
=BDBJBJBJBJBPBPBPBPBPBVBVBVBVB\B\BbBb1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B��B��B��B��B��B��BɺBĜB�}B�qB�qB�jB�jB�dB�dB�dB�dB�dB�^B�^B�XB�XB�XB�XB�^B�^B�LB�3B�B��B��Bz�BD�B:^B9XB9XB5?B49B49B5?B6FB7LB7LB6FB5?B7LB;dB=qB>wBB�BE�BF�BF�BF�BF�BG�BH�BJ�BS�Be`B��B"�BiyBk�BjBiyBiyBbNB_;B[#BXBQ�BF�BB�B>wB7LB49B.B �B!�B!�B�B�BB�fB�ZB�NB�;B�5B�/B�#B�B��B��BǮBĜBB�qB�RB�B�B��B��B��B��B��B��B��B��B��B�B�B�B��B��B��B��B��B��B��B��B��B��B�uB�VB�%B~�B� B|�Bw�Br�BjBbNB\)BR�BN�BJ�BB�B7LB'�B�B{BbB\BVB	7BB
��B
��B
��B
��BBBJBPBDBDBDBJB
=B%BB	7B+BBBB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
��B
��B
��B
��B
��B
�B
�B
�B
�B
�sB
�mB
�`B
�TB
�HB
�;B
�5B
�)B
�B
�B
��B
��B
ɺB
B
�wB
�dB
�LB
�3B
�'B
�!B
�B
�!B
�!B
�-B
�-B
�!B
�B
��B
��B
��B
��B
��B
��B
�{B
�bB
�PB
�+B
�%B
�B
�B
�B
~�B
{�B
z�B
y�B
x�B
w�B
u�B
r�B
m�B
hsB
ffB
cTB
aHB
aHB
`BB
_;B
^5B
XB
T�B
T�B
Q�B
L�B
H�B
C�B
=qB
:^B
8RB
7LB
5?B
33B
0!B
)�B
"�B
�B
�B
�B
�B
uB
bB
DB
+B
%B
B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�fB	�NB	�)B	�
B	��B	��B	��B	��B	��B	ȴB	ŢB	ĜB	B	��B	��B	�jB	�^B	�FB	�-B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�oB	�JB	�=B	�1B	�+B	�1B	�DB	�=B	�B	�B	�B	�7B	�JB	�JB	�DB	�1B	�B	� B	{�B	x�B	x�B	x�B	|�B	|�B	w�B	v�B	q�B	q�B	r�B	o�B	l�B	k�B	k�B	k�B	k�B	l�B	l�B	m�B	m�B	n�B	o�B	m�B	k�B	iyB	hsB	hsB	jB	l�B	m�B	o�B	p�B	p�B	q�B	q�B	q�B	o�B	o�B	o�B	o�B	o�B	p�B	n�B	n�B	m�B	n�B	m�B	m�B	m�B	m�B	m�B	m�B	m�B	m�B	m�B	m�B	n�B	p�B	q�B	q�B	r�B	r�B	r�B	r�B	r�B	r�B	r�B	s�B	s�B	u�B	u�B	t�B	u�B	w�B	x�B	}�B	{�B	z�B	{�B	{�B	~�B	�B	�%B	�%B	�+B	�%B	�B	�1B	�%B	�B	�B	�B	�B	�%B	�%B	�%B	�+B	�+B	�B	�B	�%B	�%B	�%B	�1B	�7B	�1B	�1B	�DB	�\B	�\B	�VB	�PB	�VB	�VB	�PB	�JB	�=B	�1B	�%B	�B	�B	�B	�B	�B	�B	�B	�B	�1B	�DB	�DB	�DB	�DB	�JB	�PB	�hB	�uB	�uB	�oB	�oB	�oB	�hB	�hB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�FB	�qB	��B	��B	ƨB	ÖB	��B	�}B	�}B	��B	ĜB	��B	��B	��B	��B	�B	�B	�#B	�/B	�5B	�5B	�;B	�;B	�;B	�5B	�5B	�)B	�)B	�/B	�)B	�)B	�)B	�)B	�/B	�5B	�5B	�;B	�;B	�HB	�NB	�NB	�TB	�TB	�ZB	�`B	�`B	�fB	�fB	�mB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B
  B
B
%B
%B
	7B
	7B
1B
	7B

=B
hB
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
�B
�B
�B
 �B
%�B
%�B
#�B
%�B
'�B
(�B
-B
/B
/B
0!B
1'B
49B
5?B
6FB
6FB
7LB
9XB
:^B
;dB
;dB
<jB
=qB
?}B
@�B
B�B
C�B
D�B
D�B
D�B
F�B
H�B
H�B
J�B
K�B
L�B
L�B
M�B
O�B
O�B
O�B
P�B
P�B
Q�B
P�B
Q�B
R�B
R�B
S�B
VB
VB
VB
W
B
YB
[#B
[#B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
^5B
aHB
bNB
cTB
dZB
dZB
e`B
ffB
hsB
iyB
iyB
iyB
jB
k�B
k�B
l�B
n�B
o�B
p�B
q�B
r�B
r�B
s�B
t�B
t�B
u�B
u�B
v�B
v�B
w�B
x�B
y�B
z�B
{�B
|�B
}�B
~�B
�B
�B
�B
�+B
�1B
�7B
�7B
�DB
�DB
�DB
�JB
�PB
�VB
�\B
�bB
�bB
�oB
�oB
�oB
�uB
�{B
�{B
�{B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�!B
�'B
�'B
�'B
�'B
�'B
�'B
�-B
�-B
�3B
�3B
�3B
�?B
�FB
�LB
�LB
�LB
�LB
�LB
�RB
�RB
�RB
�XB
�^B
�dB
�qB
�qB
�wB
�wB
�}B
��B
��B
��B
��B
��B
B
ÖB
ÖB
ĜB
ŢB
ŢB
ŢB
ƨB
ƨB
ǮB
ȴB
ȴB
ɺB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�
B
�
B
�B
�B
�B
�B
�B
�)B
�)B
�/B
�;B
�BB
�BB
�HB
�HB
�NB
�ZB
�ZB
�`B
�`B
�fB
�fB
�fB
�fB
�mB
�mB
�sB
�sB
�yB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B  B  B  BBBBBBBBBBBBB%B%B+B+B+B1B1B	7B	7B
=B
=B
=B
=BDBJBJBJBJBPBPBPBPBPBVBVBVBVB\B\BbBb1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA19c                                                                20200603093802  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20200603003804  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20200603003804  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20200603003805  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20200603003806  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20200603003806  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20200603003806  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20200603003806  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20200603003806  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20200603003806                      G�O�G�O�G�O�                JA  ARUP                                                                        20200603005524                      G�O�G�O�G�O�                