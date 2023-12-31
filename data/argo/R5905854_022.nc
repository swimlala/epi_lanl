CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:48:22Z creation;2022-06-04T17:48:22Z conversion to V3.1      
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
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20220604174822  20220610141505  5905854                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA                                  2B  A   APEX                            8422                            2.11.2                          846 @�ۻ���1   @�ۼP6�@0*=p��
�cu�7Kƨ1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   A   AA��Aa��A�  A�  A�  A�  A�  A���A�  A�  A�33B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�ffB���B�  B���B�33B���B���B�  B�33B˙�B�  B�  B�ffB���B�  B�  B�  B���B�33B�  B���B���C L�C�fC�C  C�fC
  C  C  C  C  C  C�CL�C�fC�fC  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� DyfDy�fDz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�C3Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @{@�=q@�=qA�A!�AB�RAb�RA��\A��\A��\A��\A��\A�\)A��\A��\A�BG�BG�BG�B G�B(G�B0G�B8G�B@G�BHG�BPG�BXG�B`G�BhG�BpG�BxG�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B��B��=B��B�#�B��B�W
B��qB��B�#�B�W
B˽qB�#�B�#�Bي=B��B�#�B�#�B�#�B��B�W
B�#�B��B��C ^�C�RC+�C�C�RC
�C�C�C�C�C�C+�C^�C�RC�RC�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D {D �{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D	{D	�{D
{D
�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D {D �{D!{D!�{D"{D"�{D#{D#�{D${D$�{D%{D%�{D&{D&�{D'{D'�{D({D(�{D){D)�{D*{D*�{D+{D+�{D,{D,�{D-{D-�{D.{D.�{D/{D/�{D0{D0�{D1{D1�{D2{D2�{D3{D3�{D4{D4�{D5{D5�{D6{D6�{D7{D7�{D8{D8�{D9{D9�{D:{D:�{D;{D;�{D<{D<�{D={D=�{D>{D>�{D?{D?�{D@{D@�{DA{DA�{DB{DB�{DC{DC�{DD{DD�{DE{DE�{DF{DF�{DG{DG�{DH{DH�{DI{DI�{DJ{DJ�{DK{DK�{DL{DL�{DM{DM�{DN{DN�{DO{DO�{DP{DP�{DQ{DQ�{DR{DR�{DS{DS�{DT{DT�{DU{DU�{DV{DV�{DW{DW�{DX{DX�{DY{DY�{DZ{DZ�{D[{D[�{D\{D\�{D]{D]�{D^{D^�{D_{D_�{D`{D`�{Da{Da�{Db{Db�{Dc{Dc�{Dd{Dd�{De{De�{Df{Df�{Dg{Dg�{Dh{Dh�{Di{Di�{Dj{Dj�{Dk{Dk�{Dl{Dl�{Dm{Dm�{Dn{Dn�{Do{Do�{Dp{Dp�{Dq{Dq�{Dr{Dr�{Ds{Ds�{Dt{Dt�{Du{Du�{Dv{Dv�{Dw{Dw�{Dx{Dx�{Dy
�Dy��Dz{Dz�{D{{D{�{D|{D|�{D}{D}�{D~{D~�{D{D�{D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��
D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D=D��=D�=D�B=DÂ=D��=D�=D�B=DĂ=D��=D�=D�B=Dł=D��=D�=D�B=DƂ=D��=D�=D�B=Dǂ=D��=D�=D�B=DȂ=D��=D�=D�B=Dɂ=D��=D�=D�B=Dʂ=D��=D�=D�B=D˂=D��=D�=D�B=D̂=D��=D�=D�B=D͂=D��=D�=D�B=D΂=D��=D�=D�B=Dς=D��=D�=D�B=DЂ=D��=D�=D�B=Dт=D��=D�=D�B=D҂=D��=D�=D�B=Dӂ=D��=D�=D�B=DԂ=D��=D�=D�B=DՂ=D��=D�=D�B=Dւ=D��=D�=D�B=Dׂ=D��=D�=D�B=D؂=D��=D�=D�B=Dق=D��=D�=D�EpDڂ=D��=D�=D�B=Dۂ=D��=D�=D�B=D܂=D��=D�=D�B=D݂=D��=D�=D�B=Dނ=D��=D�=D�B=D߂=D��=D�=D�B=D��=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D��=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D�
1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�C�A�E�A�XEA�R�A�>wA�C-A�I�A�8�A�6A�7�A�7�A�7�A�3�A�6�A�4nA�:^A�7�A�3�A�2�A�5A�7�A�2�A�-wA�-�A�+kA�($A�(�A�.}A�.�A�/OA�.}A�/�A�1�A�.IA�-CA�&A�"�A�VA�!bA�SA�{�A�gmA��jA�̘A�u�A�/�A̬�A�XA�u�A�*�Aȼ�A�<�A�cTA�3�A��A���A��.A�֡A�-�A��A�bA�kA�@A��A��@A�\�A��A��A�>�A���A�t�A{�DAxkQAw0�Ap�CAlMAi�3Ab�A^�AZ��AX�AW�ASHAPAN\)AM.�AK6zAH��AG+�AC/�A@��A=4nA:��A:�A9S&A8{�A5�4A3��A21A0H�A.A,��A,ϫA,�A,($A*�gA*!-A)L�A'��A'�A%ƨA$�4A$�A#B[A"L�A!tTA!0�A �DA �A �A bNA ��A!��A!�fA!xA�KA�)A֡A�jA*�APHA M�A  �A�A CA�A��A_�A��A�0A=�A1�A�A<6A;AOA�AXA6zAخA�XA?}A&�A��AH�A�4A&�A�7A($AqA�A��A��AFtA�dA��A�uAW?A
�A��A�A��A�	AqA}�A�AɆA�hA�A}VA7�A
��A
M�A	�A	Z�A	�A�ArGA(A��A8AqA�A�:A�AȴAd�A�9AE�A��A6AGA��A�pA�qA4�A��A}VA�A qvA &�@��k@�8@��O@�0U@��@�6z@��@�ں@���@�n�@��@���@�E9@�#:@�j@��F@�Y�@�֡@�q@�t�@�^5@�($@�GE@�4@��B@�N<@�	@�˒@�@�F�@��@�\@�j@�4n@�}�@��/@� �@���@�[W@藍@�@�@�j@�b@�'�@栐@�c�@�ԕ@�-w@��@�M@���@�)_@�4@���@�6z@���@ව@��@��W@߾w@�;d@���@�Z@ݓ@�;@ܪe@�g8@۹�@��@ښ�@��@ف�@�<6@��@خ}@�z@�GE@�-�@��@׈f@�@@ֻ�@�H@�ݘ@�J�@Խ<@��@ӍP@�p�@Ҿ@�?@ѻ0@�f�@Е@ϿH@�n/@��P@�[�@���@͕�@�{J@��@̀�@�U2@�1'@�ϫ@˄M@�\�@�@O@�/�@ʹ$@�|�@�L0@��@ɾw@�"�@Ȟ�@ǣn@��@��@��M@ƔF@�@ų�@Ţ�@�j�@��@�H�@�"h@�S�@�4@��@��B@¾@\@�e@���@�P�@���@���@��	@��@��8@���@��)@��9@���@��+@�E�@��W@���@�4�@��8@��@�0U@�	�@�@�~@��@���@�#�@���@�a|@���@���@�hs@�33@��F@���@�S�@�+�@�ߤ@���@���@�&�@��h@��@��@��@�/@���@��!@�M@�33@�D�@�
�@��6@�rG@���@�1'@�4@��Q@��@�v�@�@��j@��T@��z@��H@�y�@��@��h@�)�@��m@�ϫ@�ƨ@�|�@�6z@���@���@�{�@�[�@�x@��@��@��a@��@���@��@�x@�F@�%F@�Y@���@�c @��K@�a�@�E9@��@��5@���@��,@�Ĝ@���@�u�@�H�@�-@�@��o@���@�f�@�%F@��@�l�@���@�c�@�F@�8@�.I@��@��	@���@��@���@�j@�4@���@��Y@�3�@��@��{@��@��@���@�N�@��H@�@��@�s�@�GE@�1@��@��t@��@�E9@���@��p@��L@��Y@�[�@��@��N@�v`@�>�@��K@���@�l�@���@���@�=@��y@��@���@���@���@�(�@�~�@��@���@�3�@��
@���@�B�@��@���@���@�~(@�Z@�S�@�8�@�7@���@�s@��@���@��@�s�@�
�@���@�7L@���@���@��+@�;�@�
�@��)@��@���@�w2@�\�@�+@��`@�z@�1�@��9@���@���@�hs@�N<@�q@��@���@���@���@�r�@�H�@�!�@���@��K@�j�@�V@���@�}V@�V@�R�@�Q�@�Ov@��@���@���@�qv@�\�@�H�@�+@�S@��@�v�@�bN@�.�@��j@���@�zx@�0�@�%@��2@���@��1@���@���@�h�@�I�@�($@�a@qv@�@~�2@~��@~5?@}�h@}Vm@|��@|�$@|y>@|2�@{�w@{qv@z�X@zkQ@y�@y��@yT�@x�	@x�`@x��@x�@w��@w��@v��@v�+@vff@u��@uq@tĜ@t�o@t  @s;d@r�!@r&�@q�X@p�@p�o@p��@pM@o�*@o�	@o;d@n�c@nv�@n�@m��@m�>@m��@lĜ@le�@l@k� @k~�@j��@jTa@i`B@h��@g��@gƨ@g��@g�@fq�@f$�@e��@eN<@eq@e�@d��@d�[@d�_@dV�@c�r@c��@c�P@cv`@c]�@cP�@c>�@bd�@a��@a/@`��@`��@`C-@`:�@`6@`'R@`�@_�Q@_��@_�f@_A�@_)_@_�@^��@^�L@^�r@]�@\��@\!@[�]@[�W@[�F@[v`@[s@[x@[]�@Z�8@Z{�@Z�@Y�#@Y��@Y�-@Yq@X�@X�@Xoi@Xq@XS�@X@W�@W��@W9�@W�@WS@V҉@V��@V�@Vp;@V�@V �@V�@U�j@U�X@Ue,@U	l@T�@T�u@TPH@SRT@R�]@R�F@Rl�@RO@Q�=@Q`B@Q-w@Q@P��@P�e@P[�@O�g@Oqv@OP�@O�@N�}@Nz@N-@Mԕ@M�@M��@MS&@M%F@L��@L�@L4n@K�@K;d@J��@J�@J��@J=q@I��@I�~@Iq@H��@Hh�@HC-@Hb@G�@G�P@GO@Fȴ@Fl�@FOv@F�@E�z@Ek�@E-w@D�@D�O@D�.@Dr�@D1@C��@Cx@C�@B��@BTa@B.�@A��@A�^@Aa�@@��@@h�@@D�@?�@?��@?8@>�,@>��@>��@=��@=��@=7L@=�@<�@<j@;��@;��@;]�@:�@:_�@:L0@:!�@9��@9�@9Y�@9%@8��@8��@8[�@8(�@8�@7��@6��@6GE@6{@5��@5\�@5%F@4��@4�j@4�@3�W@3��@3=@2�H@2�@2ff@2u@1��@1�@1�@1�"@0��@0Ft@0K^@0b@/�$@/O@.�@.�L@.e@-�3@-��@-p�@,�5@,�D@,l"@,1@+�P@+qv@+S�@+@*�b@*�@*s�@*#:@*	@)��@)u�@)k�@)=�@(֡@(��@(�@(y>@(u�@(h�@(:�@(~@'�g@'�{@'=@&�F@&�@%��@%*0@%;@$�|@$�K@$�E@$��@$��@$�D@$m�@$-�@#{J@#O@#/�@"��@"�@"��@"M�@!�@!��@!�H@!�"@!G�@ �E@ *�@�@@C�@!-@�@��@�\@s�@?@��@ԕ@��@�@k�@j@<6@�f@��@��@��@]d@,=@�@��@�
@��@n/@
=@�2@��@҉@��@�@�'@�'@��@��@��@q�@0U@�C@o @8�@�@�@�@��@��@S�@1'@1@��@{J@qv@Z�@��@��@�@��@�}@n�@5?@+k@!�@�)@��@Vm@�@�@��@��@e�@H@,=@�r@�w@]�@F�@>�@4�@�@�}@�r@n�@d�@V@Ov@J�@!�@�o1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�C�A�E�A�XEA�R�A�>wA�C-A�I�A�8�A�6A�7�A�7�A�7�A�3�A�6�A�4nA�:^A�7�A�3�A�2�A�5A�7�A�2�A�-wA�-�A�+kA�($A�(�A�.}A�.�A�/OA�.}A�/�A�1�A�.IA�-CA�&A�"�A�VA�!bA�SA�{�A�gmA��jA�̘A�u�A�/�A̬�A�XA�u�A�*�Aȼ�A�<�A�cTA�3�A��A���A��.A�֡A�-�A��A�bA�kA�@A��A��@A�\�A��A��A�>�A���A�t�A{�DAxkQAw0�Ap�CAlMAi�3Ab�A^�AZ��AX�AW�ASHAPAN\)AM.�AK6zAH��AG+�AC/�A@��A=4nA:��A:�A9S&A8{�A5�4A3��A21A0H�A.A,��A,ϫA,�A,($A*�gA*!-A)L�A'��A'�A%ƨA$�4A$�A#B[A"L�A!tTA!0�A �DA �A �A bNA ��A!��A!�fA!xA�KA�)A֡A�jA*�APHA M�A  �A�A CA�A��A_�A��A�0A=�A1�A�A<6A;AOA�AXA6zAخA�XA?}A&�A��AH�A�4A&�A�7A($AqA�A��A��AFtA�dA��A�uAW?A
�A��A�A��A�	AqA}�A�AɆA�hA�A}VA7�A
��A
M�A	�A	Z�A	�A�ArGA(A��A8AqA�A�:A�AȴAd�A�9AE�A��A6AGA��A�pA�qA4�A��A}VA�A qvA &�@��k@�8@��O@�0U@��@�6z@��@�ں@���@�n�@��@���@�E9@�#:@�j@��F@�Y�@�֡@�q@�t�@�^5@�($@�GE@�4@��B@�N<@�	@�˒@�@�F�@��@�\@�j@�4n@�}�@��/@� �@���@�[W@藍@�@�@�j@�b@�'�@栐@�c�@�ԕ@�-w@��@�M@���@�)_@�4@���@�6z@���@ව@��@��W@߾w@�;d@���@�Z@ݓ@�;@ܪe@�g8@۹�@��@ښ�@��@ف�@�<6@��@خ}@�z@�GE@�-�@��@׈f@�@@ֻ�@�H@�ݘ@�J�@Խ<@��@ӍP@�p�@Ҿ@�?@ѻ0@�f�@Е@ϿH@�n/@��P@�[�@���@͕�@�{J@��@̀�@�U2@�1'@�ϫ@˄M@�\�@�@O@�/�@ʹ$@�|�@�L0@��@ɾw@�"�@Ȟ�@ǣn@��@��@��M@ƔF@�@ų�@Ţ�@�j�@��@�H�@�"h@�S�@�4@��@��B@¾@\@�e@���@�P�@���@���@��	@��@��8@���@��)@��9@���@��+@�E�@��W@���@�4�@��8@��@�0U@�	�@�@�~@��@���@�#�@���@�a|@���@���@�hs@�33@��F@���@�S�@�+�@�ߤ@���@���@�&�@��h@��@��@��@�/@���@��!@�M@�33@�D�@�
�@��6@�rG@���@�1'@�4@��Q@��@�v�@�@��j@��T@��z@��H@�y�@��@��h@�)�@��m@�ϫ@�ƨ@�|�@�6z@���@���@�{�@�[�@�x@��@��@��a@��@���@��@�x@�F@�%F@�Y@���@�c @��K@�a�@�E9@��@��5@���@��,@�Ĝ@���@�u�@�H�@�-@�@��o@���@�f�@�%F@��@�l�@���@�c�@�F@�8@�.I@��@��	@���@��@���@�j@�4@���@��Y@�3�@��@��{@��@��@���@�N�@��H@�@��@�s�@�GE@�1@��@��t@��@�E9@���@��p@��L@��Y@�[�@��@��N@�v`@�>�@��K@���@�l�@���@���@�=@��y@��@���@���@���@�(�@�~�@��@���@�3�@��
@���@�B�@��@���@���@�~(@�Z@�S�@�8�@�7@���@�s@��@���@��@�s�@�
�@���@�7L@���@���@��+@�;�@�
�@��)@��@���@�w2@�\�@�+@��`@�z@�1�@��9@���@���@�hs@�N<@�q@��@���@���@���@�r�@�H�@�!�@���@��K@�j�@�V@���@�}V@�V@�R�@�Q�@�Ov@��@���@���@�qv@�\�@�H�@�+@�S@��@�v�@�bN@�.�@��j@���@�zx@�0�@�%@��2@���@��1@���@���@�h�@�I�@�($@�a@qv@�@~�2@~��@~5?@}�h@}Vm@|��@|�$@|y>@|2�@{�w@{qv@z�X@zkQ@y�@y��@yT�@x�	@x�`@x��@x�@w��@w��@v��@v�+@vff@u��@uq@tĜ@t�o@t  @s;d@r�!@r&�@q�X@p�@p�o@p��@pM@o�*@o�	@o;d@n�c@nv�@n�@m��@m�>@m��@lĜ@le�@l@k� @k~�@j��@jTa@i`B@h��@g��@gƨ@g��@g�@fq�@f$�@e��@eN<@eq@e�@d��@d�[@d�_@dV�@c�r@c��@c�P@cv`@c]�@cP�@c>�@bd�@a��@a/@`��@`��@`C-@`:�@`6@`'R@`�@_�Q@_��@_�f@_A�@_)_@_�@^��@^�L@^�r@]�@\��@\!@[�]@[�W@[�F@[v`@[s@[x@[]�@Z�8@Z{�@Z�@Y�#@Y��@Y�-@Yq@X�@X�@Xoi@Xq@XS�@X@W�@W��@W9�@W�@WS@V҉@V��@V�@Vp;@V�@V �@V�@U�j@U�X@Ue,@U	l@T�@T�u@TPH@SRT@R�]@R�F@Rl�@RO@Q�=@Q`B@Q-w@Q@P��@P�e@P[�@O�g@Oqv@OP�@O�@N�}@Nz@N-@Mԕ@M�@M��@MS&@M%F@L��@L�@L4n@K�@K;d@J��@J�@J��@J=q@I��@I�~@Iq@H��@Hh�@HC-@Hb@G�@G�P@GO@Fȴ@Fl�@FOv@F�@E�z@Ek�@E-w@D�@D�O@D�.@Dr�@D1@C��@Cx@C�@B��@BTa@B.�@A��@A�^@Aa�@@��@@h�@@D�@?�@?��@?8@>�,@>��@>��@=��@=��@=7L@=�@<�@<j@;��@;��@;]�@:�@:_�@:L0@:!�@9��@9�@9Y�@9%@8��@8��@8[�@8(�@8�@7��@6��@6GE@6{@5��@5\�@5%F@4��@4�j@4�@3�W@3��@3=@2�H@2�@2ff@2u@1��@1�@1�@1�"@0��@0Ft@0K^@0b@/�$@/O@.�@.�L@.e@-�3@-��@-p�@,�5@,�D@,l"@,1@+�P@+qv@+S�@+@*�b@*�@*s�@*#:@*	@)��@)u�@)k�@)=�@(֡@(��@(�@(y>@(u�@(h�@(:�@(~@'�g@'�{@'=@&�F@&�@%��@%*0@%;@$�|@$�K@$�E@$��@$��@$�D@$m�@$-�@#{J@#O@#/�@"��@"�@"��@"M�@!�@!��@!�H@!�"@!G�@ �E@ *�@�@@C�@!-@�@��@�\@s�@?@��@ԕ@��@�@k�@j@<6@�f@��@��@��@]d@,=@�@��@�
@��@n/@
=@�2@��@҉@��@�@�'@�'@��@��@��@q�@0U@�C@o @8�@�@�@�@��@��@S�@1'@1@��@{J@qv@Z�@��@��@�@��@�}@n�@5?@+k@!�@�)@��@Vm@�@�@��@��@e�@H@,=@�r@�w@]�@F�@>�@4�@�@�}@�r@n�@d�@V@Ov@J�@!�@�o1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	�
B	��B	�mB	�
B	��B	�
B	�sB	��B	��B	��B	��B	�
B	��B	��B	��B	��B	�
B	��B	��B	��B	��B	��B	��B	�8B	�RB	�B	�8B	�8B	�mB	�RB	�RB	�RB	�mB	�B	�B	��B	�LB	�2B	�2B	�2B	�5B	�-B	b�B	RB	A�B	O�B	N�B	J�B	D�B	C{B	@�B	_�B	l"B	��B	�B	�hB	�B	� B	��B	��B	�B	W
B	P�B	C�B	EB	G�B	H1B	I�B	R�B	RB	YKB	[	B	\�B	XyB	]�B	S�B	G�B	�B��B��B��BޞB�gB�<BбB�BԯB̘B��B�PB��B�oB�
B�mB�FB��B��B��B��B��B�FB��B�QB��B�;B�B��B�xB��B�9B��B��BյBڠB��B��B�B�8B��B	1B	\B	B	K�B	UMB	U�B	T{B	bhB	h�B	n�B	x�B	~BB	�QB	��B	�B	��B	��B	�nB	�,B	�B	�vB	��B	��B	żB	��B	�	B	��B	��B	�uB	�3B	��B	�SB	��B	��B	�SB	�aB	�EB	��B	רB	�B	՛B	�9B	ևB	��B	͹B	ˬB	̘B	�B	�(B	��B	҉B	�B	�gB	�,B	�4B	��B	ѝB	��B	�HB	ΥB	̳B	�bB	��B	��B	�<B	�"B	��B	��B	��B	�VB	�pB	ΥB	ΊB	�B	уB	�&B	�:B	�NB	�(B	��B	��B	�B	��B	� B	�NB	�NB	�4B	�B	� B	҉B	�oB	�hB	��B	�B	҉B	�oB	҉B	��B	��B	�yB	چB	��B	�IB	��B	�B	�B	�vB	��B	�dB	�)B	�B	�kB	��B	�yB	��B	�)B	ޞB	�B	��B	�pB	��B	��B	�nB	�tB	�B	�B	�-B	��B	�B	��B	�B	�B	�NB	�B	��B	�FB	�tB	��B	�B	�ZB	�B	�B	�2B	��B	�B	�B	�B	��B	�B	��B	�hB	�,B	�B	�FB	��B	��B	��B	�'B	��B	��B	ߊB	�VB	ߊB	�pB	ߤB	ߊB	ߊB	��B	�'B	��B	��B	�'B	�B	�B	�hB	�B	�\B	�B	�B	��B	��B	��B	�B	�zB	�,B	�2B	�B	�B	�`B	�B	��B	��B	�2B	�sB	�B	�kB	�qB	��B	�IB	�}B	�B	�}B	�5B	�B	��B	�B	��B	�5B	�OB	��B	�UB	��B	�oB	�B	�OB	�B	�OB	�iB	��B	�B	�UB	�;B	�!B	�!B	�oB	�'B	�[B	�UB	��B	�B	�UB	�B	��B	��B	�[B	�GB	��B	�B	��B	�B	�B	�B	��B	��B	�zB	�FB	�FB	�zB	�fB	��B	�fB	�LB	��B	�+B	�FB	��B	��B	��B	�B	�lB	�fB	�FB	��B	�B	��B	��B	��B	��B	�RB	��B	��B	��B	��B	��B	��B	�fB	�%B	��B	��B	�B	��B	��B	�fB	��B	��B	��B	��B	��B	��B	�VB	�cB	��B	��B	�cB	�B	��B	��B	�HB	�B	��B	�cB	�}B	��B
 B
oB
�B
�B
UB
oB
[B
�B
B
�B
�B
�B
�B
3B
SB
�B
�B
B
?B
YB
�B
�B
�B
EB
EB
B
+B
�B
�B
fB
�B
�B
	lB

=B

#B

#B

#B

XB

XB

XB

XB

�B

�B
^B
^B
�B
�B
0B
~B
�B
�B
�B
�B
�B
pB
�B
�B
BB
\B
�B
bB
�B
�B
�B
B
4B
�B
�B
�B
TB
oB
&B
[B
�B
�B
�B
�B
�B
MB
�B
B
9B
B
B
mB
YB
�B
sB
YB
�B
�B
�B
�B
7B
�B
�B
#B
=B
WB
qB
qB
B
�B
/B
�B
�B
�B
�B
jB
�B
�B
VB
�B
�B
�B
 BB
 �B
 �B
 �B
!B
!�B
"hB
"�B
#�B
#�B
#�B
$B
$&B
$tB
$�B
%B
%,B
%B
%FB
%zB
%�B
%�B
%�B
&�B
'8B
'mB
'�B
(>B
(sB
(�B
(�B
*B
*�B
*�B
+B
+B
+B
+6B
+6B
+�B
+�B
+�B
,=B
,�B
,�B
-CB
-�B
./B
.�B
/ B
.�B
.�B
.�B
/5B
/5B
/OB
/OB
/OB
/OB
/5B
/OB
/�B
0oB
0�B
1B
1�B
2B
2B
2aB
2GB
2�B
2�B
3�B
3�B
3�B
3�B
3�B
3�B
4B
4TB
4�B
5B
5ZB
5tB
5�B
6+B
6zB
6�B
7B
7�B
7�B
8B
8B
8�B
8�B
8�B
8�B
8�B
8�B
8�B
8�B
9>B
9rB
9�B
9�B
9�B
9�B
:DB
:xB
:^B
:^B
:�B
:�B
;�B
<�B
<jB
<jB
<PB
<�B
=B
=�B
=�B
>(B
>wB
>�B
>�B
>�B
?cB
?�B
?�B
@iB
@�B
@�B
AB
AB
@�B
BAB
B�B
CB
CGB
CaB
C�B
C�B
C{B
C{B
C�B
C�B
C�B
DMB
D�B
D�B
D�B
D�B
EB
D�B
E�B
E�B
F%B
FYB
FtB
F�B
F�B
F�B
F�B
GB
GEB
GzB
G�B
G�B
G�B
G�B
HfB
H�B
IB
IB
H�B
IB
H�B
H�B
I7B
IlB
IlB
I�B
I�B
I�B
IlB
IlB
I�B
J#B
J	B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
KDB
KDB
KDB
KDB
KxB
K�B
K�B
K�B
K�B
LB
LB
LJB
L�B
L�B
L�B
L�B
MPB
M6B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
N<B
NpB
N�B
O\B
OvB
O�B
O�B
O�B
O�B
O�B
PbB
P�B
P�B
P�B
P�B
Q B
Q B
Q4B
Q�B
R B
RB
R B
RTB
RTB
R�B
R�B
S&B
S@B
S[B
S�B
S�B
S�B
T,B
TaB
T�B
T�B
T�B
T�B
U2B
U�B
U�B
U�B
VB
V9B
VmB
V�B
V�B
V�B
W
B
W?B
W�B
W�B
X+B
X�B
YeB
YeB
YB
Y�B
ZQB
Z7B
ZkB
Z�B
Z�B
Z�B
[#B
[=B
[WB
[�B
[�B
[�B
\B
\CB
\�B
\CB
\)B
\CB
\�B
\�B
\�B
]~B
]dB
]~B
^B
^jB
^�B
^�B
_�B
_�B
`B
`'B
`'B
_�B
`�B
aHB
bhB
bhB
b�B
b�B
bNB
b4B
b4B
bhB
b�B
b�B
c B
cTB
c�B
d�B
d�B
d�B
eFB
e�B
e�B
e�B
ffB
ffB
f�B
gB
gB
gRB
g�B
g�B
g�B
g�B
g�B
g�B
g�B
g�B
h
B
h$B
hsB
h�B
h�B
h�B
i*B
iDB
iDB
iDB
i_B
i_B
i�B
i�B
i�B
i�B
jeB
jB
jB
j�B
j�B
j�B
kB
k�B
k�B
k�B
k�B
k�B
lWB
l�B
m)B
m]B
m�B
m�B
m�B
nB
nB
ncB
n}B
n�B
n�B
n�B
o B
n�B
oB
oOB
o�B
o�B
o�B
o�B
pB
p!B
p;B
pUB
pUB
p�B
qB
qAB
qAB
q[B
qAB
q[B
qAB
q[B
q�B
q�B
q�B
q�B
q�B
rB
r-B
raB
r�B
r�B
r�B
r�B
r�B
s3B
s3B
shB
s�B
s�B
s�B
tB
t�B
t�B
t�B
t�B
t�B
uB
u%B
u%B
u%B
u?B
utB
u�B
v+B
vFB
v`B
vzB
v�B
v�B
v�B
v�B
wB
w�B
w�B
w�B
w�B
w�B
xlB
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	�
B	��B	�mB	�
B	��B	�
B	�sB	��B	��B	��B	��B	�
B	��B	��B	��B	��B	�
B	��B	��B	��B	��B	��B	��B	�8B	�RB	�B	�8B	�8B	�mB	�RB	�RB	�RB	�mB	�B	�B	��B	�LB	�2B	�2B	�2B	�5B	�-B	b�B	RB	A�B	O�B	N�B	J�B	D�B	C{B	@�B	_�B	l"B	��B	�B	�hB	�B	� B	��B	��B	�B	W
B	P�B	C�B	EB	G�B	H1B	I�B	R�B	RB	YKB	[	B	\�B	XyB	]�B	S�B	G�B	�B��B��B��BޞB�gB�<BбB�BԯB̘B��B�PB��B�oB�
B�mB�FB��B��B��B��B��B�FB��B�QB��B�;B�B��B�xB��B�9B��B��BյBڠB��B��B�B�8B��B	1B	\B	B	K�B	UMB	U�B	T{B	bhB	h�B	n�B	x�B	~BB	�QB	��B	�B	��B	��B	�nB	�,B	�B	�vB	��B	��B	żB	��B	�	B	��B	��B	�uB	�3B	��B	�SB	��B	��B	�SB	�aB	�EB	��B	רB	�B	՛B	�9B	ևB	��B	͹B	ˬB	̘B	�B	�(B	��B	҉B	�B	�gB	�,B	�4B	��B	ѝB	��B	�HB	ΥB	̳B	�bB	��B	��B	�<B	�"B	��B	��B	��B	�VB	�pB	ΥB	ΊB	�B	уB	�&B	�:B	�NB	�(B	��B	��B	�B	��B	� B	�NB	�NB	�4B	�B	� B	҉B	�oB	�hB	��B	�B	҉B	�oB	҉B	��B	��B	�yB	چB	��B	�IB	��B	�B	�B	�vB	��B	�dB	�)B	�B	�kB	��B	�yB	��B	�)B	ޞB	�B	��B	�pB	��B	��B	�nB	�tB	�B	�B	�-B	��B	�B	��B	�B	�B	�NB	�B	��B	�FB	�tB	��B	�B	�ZB	�B	�B	�2B	��B	�B	�B	�B	��B	�B	��B	�hB	�,B	�B	�FB	��B	��B	��B	�'B	��B	��B	ߊB	�VB	ߊB	�pB	ߤB	ߊB	ߊB	��B	�'B	��B	��B	�'B	�B	�B	�hB	�B	�\B	�B	�B	��B	��B	��B	�B	�zB	�,B	�2B	�B	�B	�`B	�B	��B	��B	�2B	�sB	�B	�kB	�qB	��B	�IB	�}B	�B	�}B	�5B	�B	��B	�B	��B	�5B	�OB	��B	�UB	��B	�oB	�B	�OB	�B	�OB	�iB	��B	�B	�UB	�;B	�!B	�!B	�oB	�'B	�[B	�UB	��B	�B	�UB	�B	��B	��B	�[B	�GB	��B	�B	��B	�B	�B	�B	��B	��B	�zB	�FB	�FB	�zB	�fB	��B	�fB	�LB	��B	�+B	�FB	��B	��B	��B	�B	�lB	�fB	�FB	��B	�B	��B	��B	��B	��B	�RB	��B	��B	��B	��B	��B	��B	�fB	�%B	��B	��B	�B	��B	��B	�fB	��B	��B	��B	��B	��B	��B	�VB	�cB	��B	��B	�cB	�B	��B	��B	�HB	�B	��B	�cB	�}B	��B
 B
oB
�B
�B
UB
oB
[B
�B
B
�B
�B
�B
�B
3B
SB
�B
�B
B
?B
YB
�B
�B
�B
EB
EB
B
+B
�B
�B
fB
�B
�B
	lB

=B

#B

#B

#B

XB

XB

XB

XB

�B

�B
^B
^B
�B
�B
0B
~B
�B
�B
�B
�B
�B
pB
�B
�B
BB
\B
�B
bB
�B
�B
�B
B
4B
�B
�B
�B
TB
oB
&B
[B
�B
�B
�B
�B
�B
MB
�B
B
9B
B
B
mB
YB
�B
sB
YB
�B
�B
�B
�B
7B
�B
�B
#B
=B
WB
qB
qB
B
�B
/B
�B
�B
�B
�B
jB
�B
�B
VB
�B
�B
�B
 BB
 �B
 �B
 �B
!B
!�B
"hB
"�B
#�B
#�B
#�B
$B
$&B
$tB
$�B
%B
%,B
%B
%FB
%zB
%�B
%�B
%�B
&�B
'8B
'mB
'�B
(>B
(sB
(�B
(�B
*B
*�B
*�B
+B
+B
+B
+6B
+6B
+�B
+�B
+�B
,=B
,�B
,�B
-CB
-�B
./B
.�B
/ B
.�B
.�B
.�B
/5B
/5B
/OB
/OB
/OB
/OB
/5B
/OB
/�B
0oB
0�B
1B
1�B
2B
2B
2aB
2GB
2�B
2�B
3�B
3�B
3�B
3�B
3�B
3�B
4B
4TB
4�B
5B
5ZB
5tB
5�B
6+B
6zB
6�B
7B
7�B
7�B
8B
8B
8�B
8�B
8�B
8�B
8�B
8�B
8�B
8�B
9>B
9rB
9�B
9�B
9�B
9�B
:DB
:xB
:^B
:^B
:�B
:�B
;�B
<�B
<jB
<jB
<PB
<�B
=B
=�B
=�B
>(B
>wB
>�B
>�B
>�B
?cB
?�B
?�B
@iB
@�B
@�B
AB
AB
@�B
BAB
B�B
CB
CGB
CaB
C�B
C�B
C{B
C{B
C�B
C�B
C�B
DMB
D�B
D�B
D�B
D�B
EB
D�B
E�B
E�B
F%B
FYB
FtB
F�B
F�B
F�B
F�B
GB
GEB
GzB
G�B
G�B
G�B
G�B
HfB
H�B
IB
IB
H�B
IB
H�B
H�B
I7B
IlB
IlB
I�B
I�B
I�B
IlB
IlB
I�B
J#B
J	B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
KDB
KDB
KDB
KDB
KxB
K�B
K�B
K�B
K�B
LB
LB
LJB
L�B
L�B
L�B
L�B
MPB
M6B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
N<B
NpB
N�B
O\B
OvB
O�B
O�B
O�B
O�B
O�B
PbB
P�B
P�B
P�B
P�B
Q B
Q B
Q4B
Q�B
R B
RB
R B
RTB
RTB
R�B
R�B
S&B
S@B
S[B
S�B
S�B
S�B
T,B
TaB
T�B
T�B
T�B
T�B
U2B
U�B
U�B
U�B
VB
V9B
VmB
V�B
V�B
V�B
W
B
W?B
W�B
W�B
X+B
X�B
YeB
YeB
YB
Y�B
ZQB
Z7B
ZkB
Z�B
Z�B
Z�B
[#B
[=B
[WB
[�B
[�B
[�B
\B
\CB
\�B
\CB
\)B
\CB
\�B
\�B
\�B
]~B
]dB
]~B
^B
^jB
^�B
^�B
_�B
_�B
`B
`'B
`'B
_�B
`�B
aHB
bhB
bhB
b�B
b�B
bNB
b4B
b4B
bhB
b�B
b�B
c B
cTB
c�B
d�B
d�B
d�B
eFB
e�B
e�B
e�B
ffB
ffB
f�B
gB
gB
gRB
g�B
g�B
g�B
g�B
g�B
g�B
g�B
g�B
h
B
h$B
hsB
h�B
h�B
h�B
i*B
iDB
iDB
iDB
i_B
i_B
i�B
i�B
i�B
i�B
jeB
jB
jB
j�B
j�B
j�B
kB
k�B
k�B
k�B
k�B
k�B
lWB
l�B
m)B
m]B
m�B
m�B
m�B
nB
nB
ncB
n}B
n�B
n�B
n�B
o B
n�B
oB
oOB
o�B
o�B
o�B
o�B
pB
p!B
p;B
pUB
pUB
p�B
qB
qAB
qAB
q[B
qAB
q[B
qAB
q[B
q�B
q�B
q�B
q�B
q�B
rB
r-B
raB
r�B
r�B
r�B
r�B
r�B
s3B
s3B
shB
s�B
s�B
s�B
tB
t�B
t�B
t�B
t�B
t�B
uB
u%B
u%B
u%B
u?B
utB
u�B
v+B
vFB
v`B
vzB
v�B
v�B
v�B
v�B
wB
w�B
w�B
w�B
w�B
w�B
xlB
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104943  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604174822  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604174822  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604174822                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605024830  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605024830  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610141505                      G�O�G�O�G�O�                