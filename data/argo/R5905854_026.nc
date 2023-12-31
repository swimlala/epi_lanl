CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:49:07Z creation;2022-06-04T17:49:07Z conversion to V3.1      
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
_FillValue                 �  I4   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M$   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p|   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tl   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �X   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �t   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ޤ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �0   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �4   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �D   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �H   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �L   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20220604174907  20220610141505  5905854                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA                                  2B  A   APEX                            8422                            2.11.2                          846 @��� �Q)1   @��Άr�K@.F�-�cB�G�{1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   A   A@  A`  A~ffA�  A�  A�  A�  A�  A�  A�  B   B  B33B  B!33B&  B/��B8��BB  BG��BN  BX  B`  Bh  Bp  By��B~��B���B���B�  B���B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�33B�ffB�  B�33B�  B�  B�  B�  B�  B�  B䙚B�  B�33B���B���B�  B�  C   C  C  C  C  C
  C�CL�C��C�fC�fC  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C3�fC6  C8  C:  C;�fC>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CQ�fCT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.�fD/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@�fDA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw�fDx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D��3D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @{@�=q@�=qA�A!�AA�Aa�A�A��\A��\A��\A��\AЏ\A��\A��\B G�BG�Bz�BG�B!z�B&G�B/�HB9{BBG�BG�HBNG�BXG�B`G�BhG�BpG�By�HB{B��B��B�#�B��B�#�B�#�B�#�B��B�#�B�#�B�#�B�#�B�#�B�#�B�W
B��=B�#�B�W
B�#�B�#�B�#�B�#�B�#�B�#�B�qB�#�B�W
B��B��B�#�B�#�C �C�C�C�C�C
�C+�C^�C޹C�RC�RC�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C3�RC6�C8�C:�C;�RC>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CQ�RCT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��)C��)C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D {D �{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D	{D	�{D
{D
�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D {D �{D!{D!�{D"{D"�{D#{D#�{D${D$�{D%{D%�{D&{D&�{D'{D'�{D({D(�{D){D)�{D*{D*�{D+{D+�{D,{D,�{D-{D-�{D.{D.��D/{D/�{D0{D0�{D1{D1�{D2{D2�{D3{D3�{D4{D4�{D5{D5�{D6{D6�{D7{D7�{D8{D8�{D9{D9�{D:{D:�{D;{D;�{D<{D<�{D={D=�{D>{D>�{D?{D?�{D@{D@��DA{DA�{DB{DB�{DC{DC�{DD{DD�{DE{DE�{DF{DF�{DG{DG�{DH{DH�{DI{DI�{DJ{DJ�{DK{DK�{DL{DL�{DM{DM�{DN{DN�{DO{DO�{DP{DP�{DQ{DQ�{DR{DR�{DS{DS�{DT{DT�{DU{DU�{DV{DV�{DW{DW�{DX{DX�{DY{DY�{DZ{DZ�{D[{D[�{D\{D\�{D]{D]�{D^{D^�{D_{D_�{D`{D`�{Da{Da�{Db{Db�{Dc{Dc�{Dd{Dd�{De{De�{Df{Df�{Dg{Dg�{Dh{Dh�{Di{Di�{Dj{Dj�{Dk{Dk�{Dl{Dl�{Dm{Dm�{Dn{Dn�{Do{Do�{Dp{Dp�{Dq{Dq�{Dr{Dr�{Ds{Ds�{Dt{Dt�{Du{Du�{Dv{Dv�{Dw{Dw��Dx{Dx�{Dy{Dy�{Dz{Dz�{D{{D{�{D|{D|�{D}{D}�{D~{D~�{D{D�{D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��pD��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D=D��=D�=D�B=DÂ=D��=D�=D�B=DĂ=D��=D�=D�B=Dł=D��=D�=D�B=DƂ=D��=D�=D�B=Dǂ=D��=D�=D�B=DȂ=D��=D�=D�B=Dɂ=D��=D�=D�B=Dʂ=D��=D�=D�B=D˂=D��=D�=D�B=D̂=D��=D�=D�B=D͂=D��=D�=D�B=D΂=D��=D�=D�B=Dς=D��=D�=D�B=DЂ=D��=D�=D�B=Dт=D��=D�=D�B=D҂=D��=D�=D�B=Dӂ=D��=D�=D�B=DԂ=D��=D�=D�B=DՂ=D��=D�=D�B=Dւ=D��=D�=D�B=Dׂ=D��=D�=D�B=D؂=D��=D�=D�B=Dق=D��=D�=D�B=Dڂ=D��=D�=D�B=Dۂ=D��=D�=D�B=D܂=D��=D�=D�B=D݂=D��=D�=D�B=Dނ=D��=D�=D�B=D߂=D��=D�=D�B=D��=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D��=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D��pD��=D�=D�B=D��=D��
D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D�
D��=D�=D�B=D��=D��=D�=D�B=111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�A��A��A�A��A⠐A��A⠐A⠐A✬A��A�:A�A⢜A��A��A�RA�(A�+A₪A��AᄁA���A�/�A�x8Aۡ�A�:�AԂA�t�A�NpA���A��9A�DgA�e�A�˒A�VA��A�=<A��sA��$A���A�@�A�n�A���A�>BA��?A�g�A��A���A��9A���A�EA�($A���A���A���A��7A��*A�A� A�4A�eA�p�A��A�e�A��A���A�l�A�N<A|�Aw�An�Aa�3A`#:A\��AY��AV��AS��AS�AP�AM�AK!AI�)AF��AD�FAA��A@��A?U�A?�A=~�A:��A:�	A9�A8�A7(�A5w2A3��A1��A0�kA/MA.&�A,�jA+֡A+;�A*�<A*�A)��A),=A(�2A(
=A'�1A'XA&�!A%jA$��A$�A$�A%��A%�_A$~�A"4nA �A ��A B[A��A��A��A�VAh
A#:AJAϫA��A�,A�6AJ�A%A��A~�A�A� AffA�A�?A��AZAK�AU2A��A�tAXyA��A5?AffA1'A�A^�A�"A�)A�*A�AC�A��A)�A��A��AI�A��A��AffA(�AA�A��A-wAffA
�A
��A
+kA	t�A� At�A�A�KA�'A�	A�}A�A�}A�A|�A �A�KAk�AAȴA|A?�A�oA~�A��A�)A��Ae,A ��A @�S&@��&@��	@�`�@�q@���@���@�'R@���@�@@��}@���@��@��/@��@��@�u@��+@�f@�	l@�@�r�@퇔@�(�@�\)@�@�l�@�0@�@�z@�$�@�'@�O@�@@�ی@�@�e�@�@�F@�m�@�=�@���@⿱@�\�@�O@᯸@���@��Q@�L�@�%@�͟@�~(@�l"@�#:@�j@ܜx@�خ@�a�@���@�K^@٬q@��@�\�@�Vm@֚@�7�@��@�*0@�s�@�Z@�?@�ԕ@҂A@ќ@�0�@��@з�@�PH@ϖS@��H@� �@�G�@��p@�@�@���@�͟@�{J@�!�@���@ȆY@�Ta@�7@��Z@Ǔ�@���@�e�@��@�A�@�2a@� i@Đ.@�9X@��@ó�@Å�@�n/@�33@��@F@�n�@�ff@�ff@�c @�($@�j@��@�Q�@��M@�@��?@�H@��j@���@�p�@�F�@��@���@�kQ@��+@���@�F�@��@��F@�?}@��@���@���@��A@�Q�@�M@�X�@�q@��@��v@��e@�S�@��P@�q@���@���@�J�@���@��S@��@��@�9X@��0@�}�@�;d@�@@�Ɇ@�j@��9@�]�@�q@��5@���@�H@���@�P�@��@�z�@��w@�dZ@��"@��6@�C�@��g@�~�@�c�@�Mj@�1�@��K@��@�]d@�4@��k@�J#@��K@��h@�� @���@���@��@�:�@�@���@��}@���@�Ov@�O@���@���@�T�@��@��@��9@�B[@��@�qv@�<6@�ߤ@��U@��6@��Y@�7�@��@�s@�C@���@��@��.@�c�@�&�@�G@��j@��t@���@��:@��:@���@�j@�/�@���@��<@�u%@�?@�#:@���@��@���@�=@��@���@���@�{�@�J�@�(�@��@��@��	@�e,@�$t@��O@�H�@��"@�Q�@��@��5@���@�^5@�<�@�M@��@���@���@�G�@� i@���@�]d@��@��C@�o @�Y@�Ĝ@���@���@��@�0U@��j@���@�X@��@�V@���@���@�8@�ߤ@���@���@�@���@�q@��@���@�n�@�:�@�ϫ@�A�@�+@�@��@�5?@��@��~@�a�@�(�@��`@���@�($@�@���@��
@���@��@�o�@�iD@�Z�@�F@�+�@��H@���@�6@���@���@�|�@�a�@�H�@�+�@���@��m@�~�@�1'@���@��@���@�qv@�.I@��X@���@���@���@�tT@�,=@�7@��@���@�33@��[@��@�� @�[�@�r@��@Y@~��@~�@~R�@~	@}��@}�h@}@|��@|c�@{��@{'�@{�@z��@z�@z�6@zW�@z
�@y�@y��@y��@y%@x��@x��@xA�@w��@w�0@w6z@vߤ@vM�@u�S@u�@ue,@ue,@uL�@u?}@u�@t�@sn/@s&@r� @rYK@q��@qk�@pM@o�w@os@n�@n��@n� @n~�@n
�@ma�@m�@l��@l�e@l]d@k��@k�@kH�@j�<@jOv@jC�@i��@i#�@g��@g33@f��@fV@f�@e��@d�P@dZ@c��@c��@cC�@bJ�@a�X@a�@`�O@`��@_��@_'�@^@�@]o @]/@]2a@]-w@]�@\�.@\>B@\	�@[��@[dZ@Z��@Z�@Y��@X��@Xz�@XC-@W��@W��@W��@W�@V_�@U�^@Uo @Uq@T!@S��@S�@S@O@SC@R�@RV@R	@Ru@Ru@R �@Q�N@Q=�@PĜ@P��@P��@P��@PM@O��@O�q@Oj�@OK�@N��@NC�@M��@M�n@Mu�@M<6@L�Y@L,=@L�@K�A@Kݘ@K�V@KO@J�"@J�1@Jd�@I��@I��@I��@I��@Ix�@H�P@HɆ@H�@G�@G��@G;d@F��@FkQ@F �@E��@E�@EY�@E=�@E%F@D�5@D(�@C�6@CRT@C(@C�@C i@Bߤ@B��@BV@B#:@A�N@Ax�@@��@@K^@?�K@?�@>�@>h
@>($@=�H@=L�@<�|@<��@;�K@:�@:~�@:s�@:H�@:
�@9��@9}�@8�5@8S�@8~@7��@7RT@7,�@7�@7�@6��@6�h@6GE@5IR@4ی@4A�@4�@4	�@3�&@3�[@3l�@3(@3@2��@2�@2��@2$�@1�t@1p�@1�@0��@0N�@0b@/�A@/�@/ƨ@/|�@/�@.��@.\�@.$�@.e@-�T@-��@-w2@-L�@-@,�`@,�z@,!@+�K@+iD@+S@*��@*��@*v�@*l�@*@�@*($@)�3@)L�@)�@(��@(��@(M@("h@(�@(x@'��@'��@'��@'��@'��@'��@'�k@'��@'1�@&� @&\�@&Q@&H�@&1�@%�Z@%�7@%:�@$��@$q@$PH@$:�@$$@$	�@#��@#8@"��@"��@"��@"d�@"8�@!�9@!w2@!o @!hs@!\�@!a�@!A @!7L@ ��@ Ĝ@ ��@ z�@ PH@�+@�:@��@�$@�	@��@a|@6�@e@�@��@�@��@��@S&@�|@�[@|�@:�@�@��@dZ@��@��@v�@L0@+k@�@�^@��@f�@S&@f�@\�@Dg@ \@�f@�@�@��@e�@'R@�]@��@&@�@�@��@~�@i�@W�@C�@&�@�.@��@�-@�"@|@x�@A @8�@q@�@�j@*�@�@ݘ@��@��@t�@H�@!-@��@�6@�+@q�@	@��@O�@0�@�@;@�v@[�@6@"h@�@�@�F@��@��@{J@j�@]�@E9@6z@,�@�@�H@�<@�@��@h
@@�)@��@c�@&�@ \@;@�@?�@A�@<�@~@�]@�A@ݘ@�@O@@O@"�@
�8@
�@
�@
�!@
W�@
�@	�@	��@	�X@	��@	zx@	j@	T�@	Dg@	*0@	�@	�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�A��A��A�A��A⠐A��A⠐A⠐A✬A��A�:A�A⢜A��A��A�RA�(A�+A₪A��AᄁA���A�/�A�x8Aۡ�A�:�AԂA�t�A�NpA���A��9A�DgA�e�A�˒A�VA��A�=<A��sA��$A���A�@�A�n�A���A�>BA��?A�g�A��A���A��9A���A�EA�($A���A���A���A��7A��*A�A� A�4A�eA�p�A��A�e�A��A���A�l�A�N<A|�Aw�An�Aa�3A`#:A\��AY��AV��AS��AS�AP�AM�AK!AI�)AF��AD�FAA��A@��A?U�A?�A=~�A:��A:�	A9�A8�A7(�A5w2A3��A1��A0�kA/MA.&�A,�jA+֡A+;�A*�<A*�A)��A),=A(�2A(
=A'�1A'XA&�!A%jA$��A$�A$�A%��A%�_A$~�A"4nA �A ��A B[A��A��A��A�VAh
A#:AJAϫA��A�,A�6AJ�A%A��A~�A�A� AffA�A�?A��AZAK�AU2A��A�tAXyA��A5?AffA1'A�A^�A�"A�)A�*A�AC�A��A)�A��A��AI�A��A��AffA(�AA�A��A-wAffA
�A
��A
+kA	t�A� At�A�A�KA�'A�	A�}A�A�}A�A|�A �A�KAk�AAȴA|A?�A�oA~�A��A�)A��Ae,A ��A @�S&@��&@��	@�`�@�q@���@���@�'R@���@�@@��}@���@��@��/@��@��@�u@��+@�f@�	l@�@�r�@퇔@�(�@�\)@�@�l�@�0@�@�z@�$�@�'@�O@�@@�ی@�@�e�@�@�F@�m�@�=�@���@⿱@�\�@�O@᯸@���@��Q@�L�@�%@�͟@�~(@�l"@�#:@�j@ܜx@�خ@�a�@���@�K^@٬q@��@�\�@�Vm@֚@�7�@��@�*0@�s�@�Z@�?@�ԕ@҂A@ќ@�0�@��@з�@�PH@ϖS@��H@� �@�G�@��p@�@�@���@�͟@�{J@�!�@���@ȆY@�Ta@�7@��Z@Ǔ�@���@�e�@��@�A�@�2a@� i@Đ.@�9X@��@ó�@Å�@�n/@�33@��@F@�n�@�ff@�ff@�c @�($@�j@��@�Q�@��M@�@��?@�H@��j@���@�p�@�F�@��@���@�kQ@��+@���@�F�@��@��F@�?}@��@���@���@��A@�Q�@�M@�X�@�q@��@��v@��e@�S�@��P@�q@���@���@�J�@���@��S@��@��@�9X@��0@�}�@�;d@�@@�Ɇ@�j@��9@�]�@�q@��5@���@�H@���@�P�@��@�z�@��w@�dZ@��"@��6@�C�@��g@�~�@�c�@�Mj@�1�@��K@��@�]d@�4@��k@�J#@��K@��h@�� @���@���@��@�:�@�@���@��}@���@�Ov@�O@���@���@�T�@��@��@��9@�B[@��@�qv@�<6@�ߤ@��U@��6@��Y@�7�@��@�s@�C@���@��@��.@�c�@�&�@�G@��j@��t@���@��:@��:@���@�j@�/�@���@��<@�u%@�?@�#:@���@��@���@�=@��@���@���@�{�@�J�@�(�@��@��@��	@�e,@�$t@��O@�H�@��"@�Q�@��@��5@���@�^5@�<�@�M@��@���@���@�G�@� i@���@�]d@��@��C@�o @�Y@�Ĝ@���@���@��@�0U@��j@���@�X@��@�V@���@���@�8@�ߤ@���@���@�@���@�q@��@���@�n�@�:�@�ϫ@�A�@�+@�@��@�5?@��@��~@�a�@�(�@��`@���@�($@�@���@��
@���@��@�o�@�iD@�Z�@�F@�+�@��H@���@�6@���@���@�|�@�a�@�H�@�+�@���@��m@�~�@�1'@���@��@���@�qv@�.I@��X@���@���@���@�tT@�,=@�7@��@���@�33@��[@��@�� @�[�@�r@��@Y@~��@~�@~R�@~	@}��@}�h@}@|��@|c�@{��@{'�@{�@z��@z�@z�6@zW�@z
�@y�@y��@y��@y%@x��@x��@xA�@w��@w�0@w6z@vߤ@vM�@u�S@u�@ue,@ue,@uL�@u?}@u�@t�@sn/@s&@r� @rYK@q��@qk�@pM@o�w@os@n�@n��@n� @n~�@n
�@ma�@m�@l��@l�e@l]d@k��@k�@kH�@j�<@jOv@jC�@i��@i#�@g��@g33@f��@fV@f�@e��@d�P@dZ@c��@c��@cC�@bJ�@a�X@a�@`�O@`��@_��@_'�@^@�@]o @]/@]2a@]-w@]�@\�.@\>B@\	�@[��@[dZ@Z��@Z�@Y��@X��@Xz�@XC-@W��@W��@W��@W�@V_�@U�^@Uo @Uq@T!@S��@S�@S@O@SC@R�@RV@R	@Ru@Ru@R �@Q�N@Q=�@PĜ@P��@P��@P��@PM@O��@O�q@Oj�@OK�@N��@NC�@M��@M�n@Mu�@M<6@L�Y@L,=@L�@K�A@Kݘ@K�V@KO@J�"@J�1@Jd�@I��@I��@I��@I��@Ix�@H�P@HɆ@H�@G�@G��@G;d@F��@FkQ@F �@E��@E�@EY�@E=�@E%F@D�5@D(�@C�6@CRT@C(@C�@C i@Bߤ@B��@BV@B#:@A�N@Ax�@@��@@K^@?�K@?�@>�@>h
@>($@=�H@=L�@<�|@<��@;�K@:�@:~�@:s�@:H�@:
�@9��@9}�@8�5@8S�@8~@7��@7RT@7,�@7�@7�@6��@6�h@6GE@5IR@4ی@4A�@4�@4	�@3�&@3�[@3l�@3(@3@2��@2�@2��@2$�@1�t@1p�@1�@0��@0N�@0b@/�A@/�@/ƨ@/|�@/�@.��@.\�@.$�@.e@-�T@-��@-w2@-L�@-@,�`@,�z@,!@+�K@+iD@+S@*��@*��@*v�@*l�@*@�@*($@)�3@)L�@)�@(��@(��@(M@("h@(�@(x@'��@'��@'��@'��@'��@'��@'�k@'��@'1�@&� @&\�@&Q@&H�@&1�@%�Z@%�7@%:�@$��@$q@$PH@$:�@$$@$	�@#��@#8@"��@"��@"��@"d�@"8�@!�9@!w2@!o @!hs@!\�@!a�@!A @!7L@ ��@ Ĝ@ ��@ z�@ PH@�+@�:@��@�$@�	@��@a|@6�@e@�@��@�@��@��@S&@�|@�[@|�@:�@�@��@dZ@��@��@v�@L0@+k@�@�^@��@f�@S&@f�@\�@Dg@ \@�f@�@�@��@e�@'R@�]@��@&@�@�@��@~�@i�@W�@C�@&�@�.@��@�-@�"@|@x�@A @8�@q@�@�j@*�@�@ݘ@��@��@t�@H�@!-@��@�6@�+@q�@	@��@O�@0�@�@;@�v@[�@6@"h@�@�@�F@��@��@{J@j�@]�@E9@6z@,�@�@�H@�<@�@��@h
@@�)@��@c�@&�@ \@;@�@?�@A�@<�@~@�]@�A@ݘ@�@O@@O@"�@
�8@
�@
�@
�!@
W�@
�@	�@	��@	�X@	��@	zx@	j@	T�@	Dg@	*0@	�@	�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�B�TB�B�:B� B�:B�TB�TB�nB�B�B�B��B�&B�nB�nB�B��B�B��B�OB	jB	āB	�tB	�!B	�B	��B	��B	�6B	��B	��B	��B	�B	�OB
HB
!-B
U�B
^�B
�_B
�=B
��B
߾B
�B
��B
��B�B�B�B9B
�"B
�hB
�B
�3B
�B
��B
��B
��B
�qB
�B
��B
s�B
��B
g�B
K�B
-�B
�B	ԕB	�KB	��B	�ZB	�3B	O�B	�B��B��B�pB�2B�B��BۦBɆB�;B�B��B�B�DB�B��B��B��B��B��B��B��B�5B�5B�AB��B�vB��B�LB��B��BżBżBƨBȚB�)B�jBڠB�|B��B�*B��B	�B	0B	QB	,�B	8�B	CGB	BuB	CB	K�B	]�B	i�B	}�B	��B	��B	��B	��B	��B	��B	��B	�*B	ƨB	�RB	��B	�zB	�fB	��B	ˬB	�B	�\B	˒B	ǮB	��B	�HB	�aB	�&B	�~B	�B	͟B	�B	�NB	�uB	��B	ԕB	��B	��B	ԕB	��B	�SB	�+B	��B	�dB	ݘB	��B	ߊB	�VB	��B	߾B	ߊB	ߊB	߾B	�pB	ޞB	��B	�]B	�B	��B	��B	�B	�yB	�sB	��B	՛B	�FB	��B	�,B	��B	��B	ּB	��B	��B	�$B	�
B	רB	ؓB	خB	��B	��B	�dB	��B	�CB	�yB	�$B	��B	��B	�QB	�KB	�KB	��B	�+B	��B	�
B	�SB	�aB	�FB	ԯB	�_B	ٴB	��B	�WB	�	B	��B	ܬB	�CB	��B	�B	��B	�	B	�B	چB	��B	�)B	یB	�qB	��B	�B	�dB	�IB	�B	��B	�IB	�IB	�IB	�B	ܒB	ܒB	�B	�/B	��B	�B	�5B	��B	�5B	�5B	�!B	޸B	��B	��B	ߊB	��B	�jB	�dB	�/B	�/B	��B	�CB	�CB	��B	߾B	��B	�B	��B	��B	��B	�B	߾B	�B	�vB	�B	��B	�HB	�:B	�:B	��B	�&B	��B	��B	��B	��B	��B	�B	�B	��B	�sB	�B	�sB	�B	��B	�XB	�$B	�
B	�sB	�$B	�B	�mB	�B	��B	��B	�B	�B	�fB	�B	�B	�_B	�yB	�B	�B	�B	��B	��B	�B	��B	��B	�_B	�DB	�B	�_B	�B	��B	��B	�B	�/B	�B	�B	�5B	��B	�UB	�;B	�;B	�;B	�B	�OB	�B	�B	�OB	�5B	�5B	�B	�5B	�B	��B	��B	�nB	��B	��B	�?B	��B	�TB	��B	��B	�?B	�ZB	��B	�%B	��B	��B	�zB	��B	��B	��B	��B	�fB	�B	�RB	�XB	��B	��B	�JB	��B	��B	��B	��B	�B	�<B	�qB	�qB	��B	�(B	�]B	�]B	��B	�cB	��B	��B
 iB
 �B
 iB
B
�B
�B
�B
'B
�B
AB
[B
aB
�B
�B
�B
B
�B
9B
�B
�B
YB
YB
tB
�B
�B
+B
�B
�B
B
fB
�B
�B
	B
	B
	lB
	�B
	�B
	�B
	�B
	�B

#B

XB

�B

�B
DB
�B
�B
�B
�B
~B
~B
�B
�B
B
PB
�B
�B
�B
�B
VB
BB
�B
B
4B
B
4B
4B
B
B
�B
�B
�B
�B
�B
 B
�B
�B
&B
[B
�B
B
�B
B
�B
�B
�B
B
SB
9B
�B
SB
�B

B
�B
�B
�B
B
�B
1B
B
QB
#B
=B
�B
B
xB
/B
�B
�B
�B
VB
pB
�B
 BB
 vB
 �B
!bB
!�B
#:B
#nB
#�B
$ZB
$�B
%FB
%zB
%�B
%�B
&�B
'�B
)B
)yB
)yB
(�B
'�B
'mB
'�B
'�B
'�B
(�B
)�B
*KB
*�B
+QB
+B
+QB
+kB
,�B
-�B
-�B
./B
.IB
.�B
.cB
.B
-�B
./B
/ B
.�B
.�B
.�B
/ B
.�B
/ B
/B
.�B
/OB
/�B
0!B
0!B
0B
0�B
1B
1vB
2aB
2�B
2�B
2�B
2�B
2�B
3MB
3�B
3�B
3�B
3�B
4TB
4TB
4nB
4�B
4�B
4�B
5ZB
5%B
6+B
6`B
6`B
6FB
6`B
6FB
6B
6+B
7B
7LB
7fB
7�B
88B
88B
8�B
9XB
9XB
9XB
9>B
9rB
9XB
9rB
:DB
=qB
>(B
>BB
>]B
>�B
>�B
>�B
?B
?cB
?�B
?HB
?}B
@ B
@�B
AB
AUB
A�B
A�B
A�B
BuB
B�B
B�B
B�B
CGB
C�B
C�B
D�B
D�B
D�B
EB
E�B
F?B
F�B
F�B
F�B
F�B
F�B
G+B
GEB
G_B
GzB
G�B
HKB
H�B
H�B
IlB
I7B
IRB
I�B
I�B
I�B
I�B
J=B
J#B
J=B
J=B
J�B
J�B
J�B
KB
KB
K)B
K�B
K�B
K�B
K�B
K�B
L0B
LJB
MB
MB
MB
L�B
MB
L�B
L�B
L�B
L�B
MB
M6B
MPB
M�B
M�B
M�B
NVB
N�B
N�B
N�B
N�B
OB
OB
OvB
O�B
O�B
P.B
PbB
PbB
PHB
PbB
P�B
P�B
P�B
QNB
Q�B
Q�B
R B
R�B
S[B
S�B
S�B
S�B
T,B
T,B
TFB
T�B
UMB
U�B
VB
VB
VB
VB
VSB
V�B
V�B
V�B
WYB
W�B
XB
XyB
X�B
XyB
YeB
YKB
Y�B
ZQB
ZQB
ZkB
ZkB
ZB
Y�B
Y�B
Z7B
Z�B
[#B
[qB
[�B
\CB
\)B
\�B
\�B
]B
]B
]/B
]dB
]�B
]�B
^�B
^5B
^�B
^�B
^�B
_!B
_VB
_�B
_pB
_pB
_pB
_�B
_VB
_;B
_�B
_VB
_�B
`'B
`�B
aB
aB
aB
aB
abB
a�B
a�B
b4B
b�B
b�B
b�B
b�B
b�B
b�B
b�B
b�B
c B
c�B
c�B
d&B
dZB
d�B
d�B
d�B
d�B
d�B
d�B
eFB
e�B
fLB
f�B
gmB
g�B
h
B
h$B
h$B
h>B
hXB
hsB
hsB
h�B
h�B
h�B
h�B
h�B
i�B
i�B
i�B
i�B
iyB
i_B
i*B
h�B
h�B
iB
h�B
h�B
h�B
h�B
h�B
i�B
i�B
i�B
jB
jKB
jB
j�B
k6B
kQB
k�B
l�B
m�B
ncB
ncB
n�B
o5B
oOB
oiB
oiB
o5B
n�B
n�B
n�B
o�B
oiB
n�B
o B
o�B
o�B
pB
poB
p�B
q'B
qvB
q�B
q�B
r-B
q�B
q�B
qAB
q�B
q�B
q�B
q�B
q�B
q�B
rGB
r�B
r�B
r�B
s�B
t�B
t�B
u%B
u?B
u?B
u�B
u�B
u�B
u�B
u�B
u�B
vB
v`B
v`B
vzB
v�B
v�B
v�B
v�B
v�B
v�B
v�B
w2B
wLB
w�B
wfB
wfB
w�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
y	B
y	B
yXB
yXB
yrB
y�B
y�B
y�B
zxB
z�B
{0B
{0B
{JB
{JB
{B
|B
|B
|B
|B
|PB
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
}"B
}B
}"B
}B
}"B
}qB
}qB
}�B
~(B
~wB
~]B
~wB
B
.B
B
B
B
.B
.B
HB
cB
�B
�B
�B
�B
� B
�B
�iB
�B
�B
� B
�;B
�oB
��B
��B
��B
��B
��B
��B
��B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�B�TB�B�:B� B�:B�TB�TB�nB�B�B�B��B�&B�nB�nB�B��B�B��B�OB	jB	āB	�tB	�!B	�B	��B	��B	�6B	��B	��B	��B	�B	�OB
HB
!-B
U�B
^�B
�_B
�=B
��B
߾B
�B
��B
��B�B�B�B9B
�"B
�hB
�B
�3B
�B
��B
��B
��B
�qB
�B
��B
s�B
��B
g�B
K�B
-�B
�B	ԕB	�KB	��B	�ZB	�3B	O�B	�B��B��B�pB�2B�B��BۦBɆB�;B�B��B�B�DB�B��B��B��B��B��B��B��B�5B�5B�AB��B�vB��B�LB��B��BżBżBƨBȚB�)B�jBڠB�|B��B�*B��B	�B	0B	QB	,�B	8�B	CGB	BuB	CB	K�B	]�B	i�B	}�B	��B	��B	��B	��B	��B	��B	��B	�*B	ƨB	�RB	��B	�zB	�fB	��B	ˬB	�B	�\B	˒B	ǮB	��B	�HB	�aB	�&B	�~B	�B	͟B	�B	�NB	�uB	��B	ԕB	��B	��B	ԕB	��B	�SB	�+B	��B	�dB	ݘB	��B	ߊB	�VB	��B	߾B	ߊB	ߊB	߾B	�pB	ޞB	��B	�]B	�B	��B	��B	�B	�yB	�sB	��B	՛B	�FB	��B	�,B	��B	��B	ּB	��B	��B	�$B	�
B	רB	ؓB	خB	��B	��B	�dB	��B	�CB	�yB	�$B	��B	��B	�QB	�KB	�KB	��B	�+B	��B	�
B	�SB	�aB	�FB	ԯB	�_B	ٴB	��B	�WB	�	B	��B	ܬB	�CB	��B	�B	��B	�	B	�B	چB	��B	�)B	یB	�qB	��B	�B	�dB	�IB	�B	��B	�IB	�IB	�IB	�B	ܒB	ܒB	�B	�/B	��B	�B	�5B	��B	�5B	�5B	�!B	޸B	��B	��B	ߊB	��B	�jB	�dB	�/B	�/B	��B	�CB	�CB	��B	߾B	��B	�B	��B	��B	��B	�B	߾B	�B	�vB	�B	��B	�HB	�:B	�:B	��B	�&B	��B	��B	��B	��B	��B	�B	�B	��B	�sB	�B	�sB	�B	��B	�XB	�$B	�
B	�sB	�$B	�B	�mB	�B	��B	��B	�B	�B	�fB	�B	�B	�_B	�yB	�B	�B	�B	��B	��B	�B	��B	��B	�_B	�DB	�B	�_B	�B	��B	��B	�B	�/B	�B	�B	�5B	��B	�UB	�;B	�;B	�;B	�B	�OB	�B	�B	�OB	�5B	�5B	�B	�5B	�B	��B	��B	�nB	��B	��B	�?B	��B	�TB	��B	��B	�?B	�ZB	��B	�%B	��B	��B	�zB	��B	��B	��B	��B	�fB	�B	�RB	�XB	��B	��B	�JB	��B	��B	��B	��B	�B	�<B	�qB	�qB	��B	�(B	�]B	�]B	��B	�cB	��B	��B
 iB
 �B
 iB
B
�B
�B
�B
'B
�B
AB
[B
aB
�B
�B
�B
B
�B
9B
�B
�B
YB
YB
tB
�B
�B
+B
�B
�B
B
fB
�B
�B
	B
	B
	lB
	�B
	�B
	�B
	�B
	�B

#B

XB

�B

�B
DB
�B
�B
�B
�B
~B
~B
�B
�B
B
PB
�B
�B
�B
�B
VB
BB
�B
B
4B
B
4B
4B
B
B
�B
�B
�B
�B
�B
 B
�B
�B
&B
[B
�B
B
�B
B
�B
�B
�B
B
SB
9B
�B
SB
�B

B
�B
�B
�B
B
�B
1B
B
QB
#B
=B
�B
B
xB
/B
�B
�B
�B
VB
pB
�B
 BB
 vB
 �B
!bB
!�B
#:B
#nB
#�B
$ZB
$�B
%FB
%zB
%�B
%�B
&�B
'�B
)B
)yB
)yB
(�B
'�B
'mB
'�B
'�B
'�B
(�B
)�B
*KB
*�B
+QB
+B
+QB
+kB
,�B
-�B
-�B
./B
.IB
.�B
.cB
.B
-�B
./B
/ B
.�B
.�B
.�B
/ B
.�B
/ B
/B
.�B
/OB
/�B
0!B
0!B
0B
0�B
1B
1vB
2aB
2�B
2�B
2�B
2�B
2�B
3MB
3�B
3�B
3�B
3�B
4TB
4TB
4nB
4�B
4�B
4�B
5ZB
5%B
6+B
6`B
6`B
6FB
6`B
6FB
6B
6+B
7B
7LB
7fB
7�B
88B
88B
8�B
9XB
9XB
9XB
9>B
9rB
9XB
9rB
:DB
=qB
>(B
>BB
>]B
>�B
>�B
>�B
?B
?cB
?�B
?HB
?}B
@ B
@�B
AB
AUB
A�B
A�B
A�B
BuB
B�B
B�B
B�B
CGB
C�B
C�B
D�B
D�B
D�B
EB
E�B
F?B
F�B
F�B
F�B
F�B
F�B
G+B
GEB
G_B
GzB
G�B
HKB
H�B
H�B
IlB
I7B
IRB
I�B
I�B
I�B
I�B
J=B
J#B
J=B
J=B
J�B
J�B
J�B
KB
KB
K)B
K�B
K�B
K�B
K�B
K�B
L0B
LJB
MB
MB
MB
L�B
MB
L�B
L�B
L�B
L�B
MB
M6B
MPB
M�B
M�B
M�B
NVB
N�B
N�B
N�B
N�B
OB
OB
OvB
O�B
O�B
P.B
PbB
PbB
PHB
PbB
P�B
P�B
P�B
QNB
Q�B
Q�B
R B
R�B
S[B
S�B
S�B
S�B
T,B
T,B
TFB
T�B
UMB
U�B
VB
VB
VB
VB
VSB
V�B
V�B
V�B
WYB
W�B
XB
XyB
X�B
XyB
YeB
YKB
Y�B
ZQB
ZQB
ZkB
ZkB
ZB
Y�B
Y�B
Z7B
Z�B
[#B
[qB
[�B
\CB
\)B
\�B
\�B
]B
]B
]/B
]dB
]�B
]�B
^�B
^5B
^�B
^�B
^�B
_!B
_VB
_�B
_pB
_pB
_pB
_�B
_VB
_;B
_�B
_VB
_�B
`'B
`�B
aB
aB
aB
aB
abB
a�B
a�B
b4B
b�B
b�B
b�B
b�B
b�B
b�B
b�B
b�B
c B
c�B
c�B
d&B
dZB
d�B
d�B
d�B
d�B
d�B
d�B
eFB
e�B
fLB
f�B
gmB
g�B
h
B
h$B
h$B
h>B
hXB
hsB
hsB
h�B
h�B
h�B
h�B
h�B
i�B
i�B
i�B
i�B
iyB
i_B
i*B
h�B
h�B
iB
h�B
h�B
h�B
h�B
h�B
i�B
i�B
i�B
jB
jKB
jB
j�B
k6B
kQB
k�B
l�B
m�B
ncB
ncB
n�B
o5B
oOB
oiB
oiB
o5B
n�B
n�B
n�B
o�B
oiB
n�B
o B
o�B
o�B
pB
poB
p�B
q'B
qvB
q�B
q�B
r-B
q�B
q�B
qAB
q�B
q�B
q�B
q�B
q�B
q�B
rGB
r�B
r�B
r�B
s�B
t�B
t�B
u%B
u?B
u?B
u�B
u�B
u�B
u�B
u�B
u�B
vB
v`B
v`B
vzB
v�B
v�B
v�B
v�B
v�B
v�B
v�B
w2B
wLB
w�B
wfB
wfB
w�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
y	B
y	B
yXB
yXB
yrB
y�B
y�B
y�B
zxB
z�B
{0B
{0B
{JB
{JB
{B
|B
|B
|B
|B
|PB
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
}"B
}B
}"B
}B
}"B
}qB
}qB
}�B
~(B
~wB
~]B
~wB
B
.B
B
B
B
.B
.B
HB
cB
�B
�B
�B
�B
� B
�B
�iB
�B
�B
� B
�;B
�oB
��B
��B
��B
��B
��B
��B
��B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104945  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604174907  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604174907  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604174907                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605024915  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605024915  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610141505                      G�O�G�O�G�O�                