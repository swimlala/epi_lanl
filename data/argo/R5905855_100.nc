CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:28:28Z creation;2022-06-04T19:28:28Z conversion to V3.1      
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
_FillValue                 �  I@   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M0   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �P   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �@   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �`   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �<   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �@   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �D   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �H   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �L   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604192828  20220610161505  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               dA   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @٠Ʌ�[1   @٠���A@,o��v��d$bM��1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @33@�  @�  A   A   A@  A^ffA�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  BxffB�33B�  B�  B�  B�  B�  B���B���B�  B�  B�ffB���B���B�  B�  B���B���B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B���B�  C   C  C  C  C33C	�fC�fC�fC  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0�C2�C4  C5�fC8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb�Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� DxfDx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�C3Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@\)@|(�@�{@�{A
=A?
=A]p�A
=A��A��A��A��AυA߅A�A��BBBBB'B/B7B?BGBOBWB_BgBoBx(�B�{B��HB��HB��HB��HB��HB��B��B��HB��HB�G�B�z�B��B��HB��HB��B��B��HB��HB��HB��HB��HB�{B��HB��HB��HB��HB��HB��HB��HB��B��HB��HC�C�C�C#�C	�
C�
C�
C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C0
>C2
>C3�C5�
C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Cb
>Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RD |)D �)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D	|)D	�)D
|)D
�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D |)D �)D!|)D!�)D"|)D"�)D#|)D#�)D$|)D$�)D%|)D%�)D&|)D&�)D'|)D'�)D(|)D(�)D)|)D)�)D*|)D*�)D+|)D+�)D,|)D,�)D-|)D-�)D.|)D.�)D/|)D/�)D0|)D0�)D1|)D1�)D2|)D2�)D3|)D3�)D4|)D4�)D5|)D5�)D6|)D6�)D7|)D7�)D8|)D8�)D9|)D9�)D:|)D:�)D;|)D;�)D<|)D<�)D=|)D=�)D>|)D>�)D?|)D?�)D@|)D@�)DA|)DA�)DB|)DB�)DC|)DC�)DD|)DD�)DE|)DE�)DF|)DF�)DG|)DG�)DH|)DH�)DI|)DI�)DJ|)DJ�)DK|)DK�)DL|)DL�)DM|)DM�)DN|)DN�)DO|)DO�)DP|)DP�)DQ|)DQ�)DR|)DR�)DS|)DS�)DT|)DT�)DU|)DU�)DV|)DV�)DW|)DW�)DX|)DX�)DY|)DY�)DZ|)DZ�)D[|)D[�)D\|)D\�)D]|)D]�)D^|)D^�)D_|)D_�)D`|)D`�)Da|)Da�)Db|)Db�)Dc|)Dc�)Dd|)Dd�)De|)De�)Df|)Df�)Dg|)Dg�)Dh|)Dh�)Di|)Di�)Dj|)Dj�)Dk|)Dk�)Dl|)Dl�)Dm|)Dm�)Dn|)Dn�)Do|)Do�)Dp|)Dp�)Dq|)Dq�)Dr|)Dr�)Ds|)Ds�)Dt|)Dt�)Du|)Du�)Dv|)Dv�)Dw|)Dx�Dx|)Dx�)Dy|)Dy�)Dz|)Dz�)D{|)D{�)D||)D|�)D}|)D}�)D~|)D~�)D|)D�)D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�AGD�~D¾D��D�>D�~DþD��D�>D�~DľD��D�>D�~DžD��D�>D�~DƾD��D�>D�~DǾD��D�>D�~DȾD��D�>D�~DɾD��D�>D�~DʾD��D�>D�~D˾D��D�>D�~D̾D��D�>D�~D;D��D�>D�~DξD��D�>D�~DϾD��D�>D�~DоD��D�>D�~DѾD��D�>D�~DҾD��D�>D�~DӾD��D�>D�~DԾD��D�>D�~DվD��D�>D�~D־D��D�>D�~D׾D��D�>D�~DؾD��D�>D�~DپD��D�>D�~DھD��D�>D�~D۾D��D�>D�~DܾD��D�>D�~DݾD��D�AGD�~D޾D��D�>D�~D߾D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D��D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D��D��D�>D�z�D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111AֵtAֵ�Aּ6AֿA��AֽA־wA��A���A��9A��gA���A��EA��#A��#A��A�ƨA��3AֹXA֤@A֡-A֞�A֟VA֟!A֜CA֝A֟�A֣nA֧�A֦�A֏\AքA�`�A�:�A��A��3AՄ�A�C�A��`A���A�-A�Y�A�B[Aǲ�A�@Aœ�A�{A�OA�՛A�/�A��4A�0�A�2�A���A���A�jA��?A��jA���A���A�P�A��A��tA�0�A�{�A�N�A���A���A���A�
�A��DA�jA�b�A���A�AUA�xlA���A�S[A���A�Z�A���A�ٴA�ɺA���A�/OA�TaA��A�S[A���A��RA�4�A� Az��Aw9XAt�6Aqc�Amh
Ai�Af�Aa!�A]��A[��AY�FAWy>AUϫARںAOC�ALG�AK�AI��AHB[AG(AE��AC�AB*0A@�>A>��A<v`A;�A7�!A3K�A1��A.��A.4A,qA,�A,qA,uA+��A,�A-xA,�ZA+�1A(�6A&�A$;dA"��A!O�A H�A b�A�<ADgA�A\)A�*A2aA��A��A�XA��A iA�{A��A=A	A(�A�"AJ#A��A_A!A�kAuAA�wA��A#�A�A�A&�A�[A��A�'A�KAQ�A�?A��A��A��A2aAxA
�
A
�AA	:�A%FA�OA��A:*Aw�A	lA��A+A��A��A�BA^5A͟AĜA��AA�A>BA��A�A�Ae�A�MAZ�A �A+A��AJ�A)�A�EAA�4A|A>BA��A|�AzxAa�AƨA8A'�A ��A ��A ,=@�>�@�Ɇ@�($@��"@�>�@��@�kQ@��@���@�x@�m]@���@��@�H�@�$@� i@�.�@�t�@�33@�@��@��@��@��m@�n@��@�E�@��@쿱@�d�@��@�@�<6@�:*@�p�@�ی@�Y@���@�^�@���@��z@��@�F@�@�@�7@���@�y�@��@�r@�	@���@��v@���@��@�5�@�Y@ގ�@���@�x�@��@ܣ@�_�@��+@ۅ�@�҉@�I�@��6@��@؈�@��@��"@��@�rG@�0�@ԩ�@��@�zx@�]d@��T@Ѱ�@� i@�H@�O@ΔF@���@�� @���@͖S@�!-@�`�@�u@�e�@��2@�iD@��@��@�m�@��g@�|@�Mj@�@��@��U@��@��@ĺ�@�y>@��>@�t�@�	l@¤�@�Ft@�  @�\)@��y@�tT@���@�7L@��@���@��@���@�U2@��N@�p�@�6z@���@��@�}V@�0U@�ϫ@��"@��@�҉@��@�~@��@��@�zx@�=@�q@��|@��@�Q@�
�@���@�\�@�'�@��,@��x@�z�@�C-@��@��@��@�k�@�5�@�Ɇ@�ff@�+k@��W@�Q�@��/@���@��@��@�o @�5�@���@�3�@���@�U�@�8@�*0@��M@���@�V�@��
@�'�@��D@�U2@�A�@�@��g@��{@��[@��@�oi@�ԕ@��@���@��@�2a@��@��@�M�@���@�_p@��5@�d�@��@��D@��N@�T�@�%@���@�Xy@�GE@�1'@��@��@�'�@�ȴ@��+@�e@��@���@�`B@�/@���@��X@��@�}V@�Ov@��@��H@�f�@�5�@��@��@���@�E�@�&�@��X@�K�@��@��<@�y>@��@��@��j@�j�@��@���@�oi@�q�@�n�@�L0@�	@��'@�H�@��@���@���@�`�@�0U@�ݘ@��K@��@�t�@��@��f@���@�z�@�3�@� �@���@�)_@���@�5?@���@��^@�u�@�C�@��@���@��@�D�@��&@���@�4�@�!-@�+@���@�p;@�&�@���@�|�@�|@�X�@��@�Ĝ@��@�~@��W@���@�{J@�Vm@�=@�Y@��?@�y>@�U2@�B[@�*�@��z@���@���@���@�n/@�Vm@�?}@�'�@���@���@�e�@�4n@�.�@�.�@��@�ƨ@��:@��@�5�@���@���@��D@�q@�Ta@��@��@���@���@�[W@�=�@��@��Y@�L0@�#:@��@�|�@��@��@���@���@�~(@�w�@�c @�Ft@�G@���@��{@�rG@�`B@��@���@�YK@��@��@��@��@e�@�@~ȴ@~��@~��@~n�@~$�@}�@}|@}/@}�@|tT@|@{�F@{C�@z�X@z4@y��@y�7@y-w@x�@x7@w�@w!-@v��@v�r@vYK@u�@u*0@tm�@tXy@s�@r�R@q��@q��@q`B@p��@p|�@p"h@o�@np;@m�@mzx@m;@ly>@l�@k�@k4�@j��@jff@j@i�@i+@h��@hz�@h4n@g�@g�k@gMj@f҉@fGE@f$�@e�#@e?}@d��@dH@c6z@b�6@bL0@a��@ac�@a�@`��@`/�@_�@_� @_��@_�q@_P�@^�F@^O@]�9@]`B@\ی@\�?@\��@\bN@[��@[@O@Z��@ZE�@YS&@X�P@X�@XU2@W�@W�@W=@W�@V�b@V@�@U�^@U`B@T��@TFt@T	�@S˒@Sn/@SO@R��@R��@Rq�@RGE@Q�@Q0�@P�z@PV�@O�@N�"@Nxl@Nu@M��@Mc�@L�@L<�@K�@K��@K��@KK�@J�H@J�r@JJ@I�3@I�t@I+�@H��@H�5@H��@H�`@He�@G� @GdZ@G�@F��@FOv@F1�@F
�@E�t@E�"@E-w@D��@E�@D��@D�.@Dc�@Dx@C��@C��@C�@B��@B-@A�o@A��@A^�@@�|@@�@@�	@@��@@1@?��@?o@>��@>W�@>.�@=��@=�@;�r@;�w@;A�@;Y@:�"@:�<@:��@:��@:)�@9�C@9�@9Y�@8�p@8Ĝ@8y>@82�@8�@7�&@7�
@7�w@7t�@7$t@6҉@6L0@5�Z@5o @4�5@4�/@4��@4?�@3��@3��@3W?@3�@2�c@2�'@2�!@2��@2i�@2;�@2 �@1�H@1��@1Q�@0�@0z�@0PH@01'@0�@/s@/"�@/�@/�@.��@.�@-ԕ@-�h@-*0@,�I@,m�@,Xy@,?�@,�@+�{@+'�@*�"@*ȴ@*�@*\�@*8�@*�@)��@)ϫ@)��@)u�@)T�@)�@(�@(֡@(Ɇ@(Ĝ@(��@(�@(w�@(g8@(M@(�@'��@'�0@'��@'j�@'
=@&��@&ں@&}V@&H�@%�)@%��@%^�@%#�@$�@$�@$Ft@$1'@$@#�r@#�;@#ݘ@#�F@#9�@"v�@"�@!�d@!��@!IR@!�@ �P@ �/@ ��@ oi@ -�@��@]�@S@H�@ԕ@�=@Q�@q@��@Q�@*�@�m@��@y�@F�@�@͟@��@l�@�@	@�@�"@��@�)@��@Q�@'R@G@��@~�@C�@�@�@��@��@�h@��@a|@!�@�@�7@��@��@��@j@X@5�@�@Ɇ@��@Q�@�@l�@�@� @}V@\�@+k@u@�d@�=@V@��@�9@��@|�@l"@bN@Q�@�@��@.I@�@�M@�m@��@a|@C�@�@@@�N@�=@Vm@?}@�@��@�O@�@�Y@A�@�]@��@��@�@t�@U�@�@�@S@
�s@
�b@
��@
n�@
;�@
�@	�j@	�@	�3@	��@	|@	-w@	@�5@�U@��@�@:�@x@�@�W@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111AֵtAֵ�Aּ6AֿA��AֽA־wA��A���A��9A��gA���A��EA��#A��#A��A�ƨA��3AֹXA֤@A֡-A֞�A֟VA֟!A֜CA֝A֟�A֣nA֧�A֦�A֏\AքA�`�A�:�A��A��3AՄ�A�C�A��`A���A�-A�Y�A�B[Aǲ�A�@Aœ�A�{A�OA�՛A�/�A��4A�0�A�2�A���A���A�jA��?A��jA���A���A�P�A��A��tA�0�A�{�A�N�A���A���A���A�
�A��DA�jA�b�A���A�AUA�xlA���A�S[A���A�Z�A���A�ٴA�ɺA���A�/OA�TaA��A�S[A���A��RA�4�A� Az��Aw9XAt�6Aqc�Amh
Ai�Af�Aa!�A]��A[��AY�FAWy>AUϫARںAOC�ALG�AK�AI��AHB[AG(AE��AC�AB*0A@�>A>��A<v`A;�A7�!A3K�A1��A.��A.4A,qA,�A,qA,uA+��A,�A-xA,�ZA+�1A(�6A&�A$;dA"��A!O�A H�A b�A�<ADgA�A\)A�*A2aA��A��A�XA��A iA�{A��A=A	A(�A�"AJ#A��A_A!A�kAuAA�wA��A#�A�A�A&�A�[A��A�'A�KAQ�A�?A��A��A��A2aAxA
�
A
�AA	:�A%FA�OA��A:*Aw�A	lA��A+A��A��A�BA^5A͟AĜA��AA�A>BA��A�A�Ae�A�MAZ�A �A+A��AJ�A)�A�EAA�4A|A>BA��A|�AzxAa�AƨA8A'�A ��A ��A ,=@�>�@�Ɇ@�($@��"@�>�@��@�kQ@��@���@�x@�m]@���@��@�H�@�$@� i@�.�@�t�@�33@�@��@��@��@��m@�n@��@�E�@��@쿱@�d�@��@�@�<6@�:*@�p�@�ی@�Y@���@�^�@���@��z@��@�F@�@�@�7@���@�y�@��@�r@�	@���@��v@���@��@�5�@�Y@ގ�@���@�x�@��@ܣ@�_�@��+@ۅ�@�҉@�I�@��6@��@؈�@��@��"@��@�rG@�0�@ԩ�@��@�zx@�]d@��T@Ѱ�@� i@�H@�O@ΔF@���@�� @���@͖S@�!-@�`�@�u@�e�@��2@�iD@��@��@�m�@��g@�|@�Mj@�@��@��U@��@��@ĺ�@�y>@��>@�t�@�	l@¤�@�Ft@�  @�\)@��y@�tT@���@�7L@��@���@��@���@�U2@��N@�p�@�6z@���@��@�}V@�0U@�ϫ@��"@��@�҉@��@�~@��@��@�zx@�=@�q@��|@��@�Q@�
�@���@�\�@�'�@��,@��x@�z�@�C-@��@��@��@�k�@�5�@�Ɇ@�ff@�+k@��W@�Q�@��/@���@��@��@�o @�5�@���@�3�@���@�U�@�8@�*0@��M@���@�V�@��
@�'�@��D@�U2@�A�@�@��g@��{@��[@��@�oi@�ԕ@��@���@��@�2a@��@��@�M�@���@�_p@��5@�d�@��@��D@��N@�T�@�%@���@�Xy@�GE@�1'@��@��@�'�@�ȴ@��+@�e@��@���@�`B@�/@���@��X@��@�}V@�Ov@��@��H@�f�@�5�@��@��@���@�E�@�&�@��X@�K�@��@��<@�y>@��@��@��j@�j�@��@���@�oi@�q�@�n�@�L0@�	@��'@�H�@��@���@���@�`�@�0U@�ݘ@��K@��@�t�@��@��f@���@�z�@�3�@� �@���@�)_@���@�5?@���@��^@�u�@�C�@��@���@��@�D�@��&@���@�4�@�!-@�+@���@�p;@�&�@���@�|�@�|@�X�@��@�Ĝ@��@�~@��W@���@�{J@�Vm@�=@�Y@��?@�y>@�U2@�B[@�*�@��z@���@���@���@�n/@�Vm@�?}@�'�@���@���@�e�@�4n@�.�@�.�@��@�ƨ@��:@��@�5�@���@���@��D@�q@�Ta@��@��@���@���@�[W@�=�@��@��Y@�L0@�#:@��@�|�@��@��@���@���@�~(@�w�@�c @�Ft@�G@���@��{@�rG@�`B@��@���@�YK@��@��@��@��@e�@�@~ȴ@~��@~��@~n�@~$�@}�@}|@}/@}�@|tT@|@{�F@{C�@z�X@z4@y��@y�7@y-w@x�@x7@w�@w!-@v��@v�r@vYK@u�@u*0@tm�@tXy@s�@r�R@q��@q��@q`B@p��@p|�@p"h@o�@np;@m�@mzx@m;@ly>@l�@k�@k4�@j��@jff@j@i�@i+@h��@hz�@h4n@g�@g�k@gMj@f҉@fGE@f$�@e�#@e?}@d��@dH@c6z@b�6@bL0@a��@ac�@a�@`��@`/�@_�@_� @_��@_�q@_P�@^�F@^O@]�9@]`B@\ی@\�?@\��@\bN@[��@[@O@Z��@ZE�@YS&@X�P@X�@XU2@W�@W�@W=@W�@V�b@V@�@U�^@U`B@T��@TFt@T	�@S˒@Sn/@SO@R��@R��@Rq�@RGE@Q�@Q0�@P�z@PV�@O�@N�"@Nxl@Nu@M��@Mc�@L�@L<�@K�@K��@K��@KK�@J�H@J�r@JJ@I�3@I�t@I+�@H��@H�5@H��@H�`@He�@G� @GdZ@G�@F��@FOv@F1�@F
�@E�t@E�"@E-w@D��@E�@D��@D�.@Dc�@Dx@C��@C��@C�@B��@B-@A�o@A��@A^�@@�|@@�@@�	@@��@@1@?��@?o@>��@>W�@>.�@=��@=�@;�r@;�w@;A�@;Y@:�"@:�<@:��@:��@:)�@9�C@9�@9Y�@8�p@8Ĝ@8y>@82�@8�@7�&@7�
@7�w@7t�@7$t@6҉@6L0@5�Z@5o @4�5@4�/@4��@4?�@3��@3��@3W?@3�@2�c@2�'@2�!@2��@2i�@2;�@2 �@1�H@1��@1Q�@0�@0z�@0PH@01'@0�@/s@/"�@/�@/�@.��@.�@-ԕ@-�h@-*0@,�I@,m�@,Xy@,?�@,�@+�{@+'�@*�"@*ȴ@*�@*\�@*8�@*�@)��@)ϫ@)��@)u�@)T�@)�@(�@(֡@(Ɇ@(Ĝ@(��@(�@(w�@(g8@(M@(�@'��@'�0@'��@'j�@'
=@&��@&ں@&}V@&H�@%�)@%��@%^�@%#�@$�@$�@$Ft@$1'@$@#�r@#�;@#ݘ@#�F@#9�@"v�@"�@!�d@!��@!IR@!�@ �P@ �/@ ��@ oi@ -�@��@]�@S@H�@ԕ@�=@Q�@q@��@Q�@*�@�m@��@y�@F�@�@͟@��@l�@�@	@�@�"@��@�)@��@Q�@'R@G@��@~�@C�@�@�@��@��@�h@��@a|@!�@�@�7@��@��@��@j@X@5�@�@Ɇ@��@Q�@�@l�@�@� @}V@\�@+k@u@�d@�=@V@��@�9@��@|�@l"@bN@Q�@�@��@.I@�@�M@�m@��@a|@C�@�@@@�N@�=@Vm@?}@�@��@�O@�@�Y@A�@�]@��@��@�@t�@U�@�@�@S@
�s@
�b@
��@
n�@
;�@
�@	�j@	�@	�3@	��@	|@	-w@	@�5@�U@��@�@:�@x@�@�W@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
��B
��B
�B
�0B
�B
��B
��B
��B
��B
�B
��B
��B
��B
�B
��B
�0B
�eB
�B
��B
�B
��B
�|B
��B
�DB
�YB
��B
��B
�%B�B&LB*�B*�B*KB,B/iB2�B6�B3�B'�B"�BDB#BB=B"NB3hB<�BCGBF?B@4BF�BFYBc�B�B��B��B��B�jB��B��B��B�FB�KB��B��B�B�mB��B�zB�mB�B�B��B�aB��B��B�_B�5B��Bx�B^�BQ�BH�B-�B�B
�6B
یB
�CB
�RB
jB
_�B
B'B
B
�B	�!B	�B	ΊB	��B	�sB	�fB	v`B	jKB	a-B	V�B	LdB	;�B	-]B	�B	mB	�B	6B	<B	1B	�B	�B	�B	B	�B	�B	1B�"B�|B��B�B��B��B	�B	�B	�B	I�B	ezB	iDB	b�B	VmB	HfB	@ B	6�B	/iB	*KB	6�B	AoB	C�B	EB	J�B	M�B	Q�B	a|B	`�B	P�B	Z�B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	�FB	�B	�hB	�'B	�HB	��B	�B	�FB	��B	��B	�)B	�MB	�VB	�<B	��B	�B	��B	�EB	��B	�EB	�B	�#B	�VB	�6B	��B	��B	̳B	ΥB	�jB	�B	�B	�B	��B	ǮB	οB	��B	��B	��B
YB
-B
aB
 �B	�B	��B	�}B
AB
MB

B
]B
�B
�B
B
KB

B
�B
�B
�B
B
B
 B
�B
�B
�B
�B

XB
�B
B
�B
	B
1B
+B
�B
%B
SB
9B
�B
 OB
 iB
 B
 �B	��B	��B	�<B	�B	��B	�PB	��B	�dB	��B	�B	�LB	��B	�lB	��B	��B	�B	��B	��B	�LB	��B	��B	�`B	�FB	��B	�FB	�?B	�B	�B	�aB	�GB	��B	�B	�B	�[B	�'B	��B	�B	�B	�TB	��B	��B	��B	��B	��B	��B	��B	�lB	�B	�B	�B	��B	��B	�B	��B	�B	�2B	��B	�?B	��B	�B	�aB	�vB	�B	��B	�B	�}B	�UB	�B	�B	��B	�AB	�B	�B	�vB	��B	�B	�B	�FB	��B	��B	��B	��B	��B	�B	�aB	��B	�B	�B	�9B	�LB	�B	�XB	��B	��B	��B	�6B	��B	�dB	��B	��B	��B	�JB	�B	�B	�VB	�"B	�<B	�qB	��B	��B	��B	�(B	�B	�BB	��B	�]B	��B	�B	��B	��B
  B
 OB
 �B
oB
UB
�B
�B
�B
B
�B
�B
�B
{B
{B
�B
MB
gB
�B
�B
�B
�B
9B
mB
�B
%B
YB
tB
YB
B
�B
�B
�B
_B
�B
zB
�B
�B
�B
	B
�B
�B
�B
	B
	7B
	RB
	�B
	�B
	�B
	�B
	�B
	�B
	�B

�B
B
^B
xB
^B
xB
xB
�B
�B
~B
�B
B
�B
pB
\B
�B
�B
�B
bB
�B
4B
�B
�B
�B
�B
�B
&B
uB
�B
aB
{B
�B
2B
gB
�B
B
B
9B
�B
�B
?B
�B
�B
�B
B
_B
�B
�B
KB
KB
eB
�B
B
B
�B
�B
QB
kB
7B
7B
7B
QB
kB
kB
	B
	B
�B
WB
qB
�B
�B
�B
/B
�B
�B
5B
B
B
B
B
�B
�B
B
pB
 'B
 BB
 �B
 �B
!B
!bB
!-B
!-B
!�B
!�B
!�B
"�B
"�B
"hB
"�B
# B
#nB
$&B
$B
$&B
$ZB
$�B
$�B
%B
$�B
%B
%FB
%�B
%�B
&B
&2B
&�B
&�B
'B
'B
'B
'�B
'�B
'�B
'�B
(XB
(XB
(�B
(�B
(�B
)�B
)�B
)�B
*B
*�B
+B
+�B
+kB
+kB
,B
,qB
-wB
-�B
-�B
.B
.IB
.�B
/OB
/�B
/�B
0B
0�B
0�B
1'B
2B
2-B
2B
1�B
1�B
2-B
2|B
2�B
4B
4�B
4�B
5%B
6`B
6`B
6FB
6`B
6�B
6�B
7LB
72B
72B
72B
7B
6�B
6�B
6�B
6�B
6�B
72B
88B
8�B
8�B
8�B
9$B
:DB
:DB
:�B
:�B
:�B
;dB
;�B
;�B
;�B
<B
<�B
<�B
="B
=�B
=qB
=�B
=�B
>(B
>�B
>wB
>�B
?HB
?�B
?�B
?�B
@B
@B
@4B
@�B
A;B
A�B
A�B
BAB
B�B
B�B
B�B
C�B
C�B
C�B
DMB
D�B
EB
ESB
EmB
E�B
E�B
E�B
F?B
F�B
G�B
GzB
G�B
G�B
H1B
HB
H�B
I�B
I�B
J=B
JrB
J�B
J�B
KB
KDB
KDB
KDB
K)B
KxB
K�B
L0B
LJB
L�B
L�B
L�B
L�B
L�B
L�B
MB
MB
M�B
NpB
NVB
NVB
N�B
N�B
N�B
NVB
NVB
N"B
M�B
N"B
NB
NB
NpB
NpB
N�B
N�B
OB
O\B
O�B
P�B
PbB
PbB
P�B
P�B
Q B
Q�B
R B
RB
Q�B
QNB
Q�B
QhB
QNB
Q�B
RB
R:B
RoB
RoB
SB
S�B
SuB
S�B
S�B
S�B
S�B
TB
T�B
U2B
UMB
T�B
T�B
UB
UMB
UMB
U2B
UgB
U�B
VSB
V�B
W?B
XyB
X�B
X�B
X�B
X_B
Y1B
ZB
ZQB
[=B
[qB
[�B
[�B
\CB
\B
\B
\xB
\]B
\B
[�B
\)B
]/B
]IB
]B
]~B
]�B
]�B
]�B
]�B
]�B
^B
]�B
]�B
^5B
^�B
_pB
`\B
`�B
aB
abB
abB
a|B
a|B
abB
abB
abB
abB
aHB
aHB
abB
a�B
a�B
a�B
bB
bhB
b�B
b�B
c:B
cnB
c�B
c�B
d&B
d&B
d�B
d�B
eFB
ezB
e`B
e�B
fB
f2B
f2B
fB
f2B
fB
e�B
e�B
e�B
e�B
fB
fB
f2B
ffB
gB
g8B
g8B
g8B
g�B
g�B
h>B
hXB
hXB
h$B
h>B
h>B
hXB
hsB
hsB
h�B
h�B
h�B
h�B
h�B
i*B
i*B
i*B
iDB
i_B
iyB
i�B
i�B
i�B
i�B
i�B
i�B
i�B
jKB
j0B
jKB
jB
jB
kB
kkB
kQB
k�B
k�B
k�B
l"B
l"B
l=B
l"B
lWB
l"B
l"B
l�B
m)B
mCB
mwB
m�B
m�B
m�B
n/B
n/B
n}B
n}B
n�B
oOB
o�B
o�B
p�B
p�B
p�B
p�B
qB
qAB
q�B
q�B
q�B
rB
rB
rGB
r�B
r�B
r�B
r�B
sMB
sMB
sMB
s�B
tnB
t�B
t�B
uB
u%B
utB
u�B
v+B
v+B
vzB
vzB
v�B
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
w�B
w�B
xB
xlB
xlB
xlB
x�B
y	B
y�B
zxB
z�B
z�B
z�B
{0B
{B
{B
{B
{B
{�B
{�B
{�B
|B
|B
|B
|B
|PB
}B
}qB
}qB
}�B
}�B
}�B
~B
~(B
~]B
~]B
~wB
~wB
~�B
B
cB
�B
�B
�B
�B
�B
�OB
��B
��B
��B
��B
�B
�;B
��B
�oB
��B
��B
�'B
�AB
�AB
�uB
��B
��B
�B
�B
�B
�{B
��B
��B
��B
�3B
�MB
�3B
�gB
�MB
��B
�B
�S111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
��B
��B
�B
�0B
�B
��B
��B
��B
��B
�B
��B
��B
��B
�B
��B
�0B
�eB
�B
��B
�B
��B
�|B
��B
�DB
�YB
��B
��B
�%B�B&LB*�B*�B*KB,B/iB2�B6�B3�B'�B"�BDB#BB=B"NB3hB<�BCGBF?B@4BF�BFYBc�B�B��B��B��B�jB��B��B��B�FB�KB��B��B�B�mB��B�zB�mB�B�B��B�aB��B��B�_B�5B��Bx�B^�BQ�BH�B-�B�B
�6B
یB
�CB
�RB
jB
_�B
B'B
B
�B	�!B	�B	ΊB	��B	�sB	�fB	v`B	jKB	a-B	V�B	LdB	;�B	-]B	�B	mB	�B	6B	<B	1B	�B	�B	�B	B	�B	�B	1B�"B�|B��B�B��B��B	�B	�B	�B	I�B	ezB	iDB	b�B	VmB	HfB	@ B	6�B	/iB	*KB	6�B	AoB	C�B	EB	J�B	M�B	Q�B	a|B	`�B	P�B	Z�B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	�FB	�B	�hB	�'B	�HB	��B	�B	�FB	��B	��B	�)B	�MB	�VB	�<B	��B	�B	��B	�EB	��B	�EB	�B	�#B	�VB	�6B	��B	��B	̳B	ΥB	�jB	�B	�B	�B	��B	ǮB	οB	��B	��B	��B
YB
-B
aB
 �B	�B	��B	�}B
AB
MB

B
]B
�B
�B
B
KB

B
�B
�B
�B
B
B
 B
�B
�B
�B
�B

XB
�B
B
�B
	B
1B
+B
�B
%B
SB
9B
�B
 OB
 iB
 B
 �B	��B	��B	�<B	�B	��B	�PB	��B	�dB	��B	�B	�LB	��B	�lB	��B	��B	�B	��B	��B	�LB	��B	��B	�`B	�FB	��B	�FB	�?B	�B	�B	�aB	�GB	��B	�B	�B	�[B	�'B	��B	�B	�B	�TB	��B	��B	��B	��B	��B	��B	��B	�lB	�B	�B	�B	��B	��B	�B	��B	�B	�2B	��B	�?B	��B	�B	�aB	�vB	�B	��B	�B	�}B	�UB	�B	�B	��B	�AB	�B	�B	�vB	��B	�B	�B	�FB	��B	��B	��B	��B	��B	�B	�aB	��B	�B	�B	�9B	�LB	�B	�XB	��B	��B	��B	�6B	��B	�dB	��B	��B	��B	�JB	�B	�B	�VB	�"B	�<B	�qB	��B	��B	��B	�(B	�B	�BB	��B	�]B	��B	�B	��B	��B
  B
 OB
 �B
oB
UB
�B
�B
�B
B
�B
�B
�B
{B
{B
�B
MB
gB
�B
�B
�B
�B
9B
mB
�B
%B
YB
tB
YB
B
�B
�B
�B
_B
�B
zB
�B
�B
�B
	B
�B
�B
�B
	B
	7B
	RB
	�B
	�B
	�B
	�B
	�B
	�B
	�B

�B
B
^B
xB
^B
xB
xB
�B
�B
~B
�B
B
�B
pB
\B
�B
�B
�B
bB
�B
4B
�B
�B
�B
�B
�B
&B
uB
�B
aB
{B
�B
2B
gB
�B
B
B
9B
�B
�B
?B
�B
�B
�B
B
_B
�B
�B
KB
KB
eB
�B
B
B
�B
�B
QB
kB
7B
7B
7B
QB
kB
kB
	B
	B
�B
WB
qB
�B
�B
�B
/B
�B
�B
5B
B
B
B
B
�B
�B
B
pB
 'B
 BB
 �B
 �B
!B
!bB
!-B
!-B
!�B
!�B
!�B
"�B
"�B
"hB
"�B
# B
#nB
$&B
$B
$&B
$ZB
$�B
$�B
%B
$�B
%B
%FB
%�B
%�B
&B
&2B
&�B
&�B
'B
'B
'B
'�B
'�B
'�B
'�B
(XB
(XB
(�B
(�B
(�B
)�B
)�B
)�B
*B
*�B
+B
+�B
+kB
+kB
,B
,qB
-wB
-�B
-�B
.B
.IB
.�B
/OB
/�B
/�B
0B
0�B
0�B
1'B
2B
2-B
2B
1�B
1�B
2-B
2|B
2�B
4B
4�B
4�B
5%B
6`B
6`B
6FB
6`B
6�B
6�B
7LB
72B
72B
72B
7B
6�B
6�B
6�B
6�B
6�B
72B
88B
8�B
8�B
8�B
9$B
:DB
:DB
:�B
:�B
:�B
;dB
;�B
;�B
;�B
<B
<�B
<�B
="B
=�B
=qB
=�B
=�B
>(B
>�B
>wB
>�B
?HB
?�B
?�B
?�B
@B
@B
@4B
@�B
A;B
A�B
A�B
BAB
B�B
B�B
B�B
C�B
C�B
C�B
DMB
D�B
EB
ESB
EmB
E�B
E�B
E�B
F?B
F�B
G�B
GzB
G�B
G�B
H1B
HB
H�B
I�B
I�B
J=B
JrB
J�B
J�B
KB
KDB
KDB
KDB
K)B
KxB
K�B
L0B
LJB
L�B
L�B
L�B
L�B
L�B
L�B
MB
MB
M�B
NpB
NVB
NVB
N�B
N�B
N�B
NVB
NVB
N"B
M�B
N"B
NB
NB
NpB
NpB
N�B
N�B
OB
O\B
O�B
P�B
PbB
PbB
P�B
P�B
Q B
Q�B
R B
RB
Q�B
QNB
Q�B
QhB
QNB
Q�B
RB
R:B
RoB
RoB
SB
S�B
SuB
S�B
S�B
S�B
S�B
TB
T�B
U2B
UMB
T�B
T�B
UB
UMB
UMB
U2B
UgB
U�B
VSB
V�B
W?B
XyB
X�B
X�B
X�B
X_B
Y1B
ZB
ZQB
[=B
[qB
[�B
[�B
\CB
\B
\B
\xB
\]B
\B
[�B
\)B
]/B
]IB
]B
]~B
]�B
]�B
]�B
]�B
]�B
^B
]�B
]�B
^5B
^�B
_pB
`\B
`�B
aB
abB
abB
a|B
a|B
abB
abB
abB
abB
aHB
aHB
abB
a�B
a�B
a�B
bB
bhB
b�B
b�B
c:B
cnB
c�B
c�B
d&B
d&B
d�B
d�B
eFB
ezB
e`B
e�B
fB
f2B
f2B
fB
f2B
fB
e�B
e�B
e�B
e�B
fB
fB
f2B
ffB
gB
g8B
g8B
g8B
g�B
g�B
h>B
hXB
hXB
h$B
h>B
h>B
hXB
hsB
hsB
h�B
h�B
h�B
h�B
h�B
i*B
i*B
i*B
iDB
i_B
iyB
i�B
i�B
i�B
i�B
i�B
i�B
i�B
jKB
j0B
jKB
jB
jB
kB
kkB
kQB
k�B
k�B
k�B
l"B
l"B
l=B
l"B
lWB
l"B
l"B
l�B
m)B
mCB
mwB
m�B
m�B
m�B
n/B
n/B
n}B
n}B
n�B
oOB
o�B
o�B
p�B
p�B
p�B
p�B
qB
qAB
q�B
q�B
q�B
rB
rB
rGB
r�B
r�B
r�B
r�B
sMB
sMB
sMB
s�B
tnB
t�B
t�B
uB
u%B
utB
u�B
v+B
v+B
vzB
vzB
v�B
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
w�B
w�B
xB
xlB
xlB
xlB
x�B
y	B
y�B
zxB
z�B
z�B
z�B
{0B
{B
{B
{B
{B
{�B
{�B
{�B
|B
|B
|B
|B
|PB
}B
}qB
}qB
}�B
}�B
}�B
~B
~(B
~]B
~]B
~wB
~wB
~�B
B
cB
�B
�B
�B
�B
�B
�OB
��B
��B
��B
��B
�B
�;B
��B
�oB
��B
��B
�'B
�AB
�AB
�uB
��B
��B
�B
�B
�B
�{B
��B
��B
��B
�3B
�MB
�3B
�gB
�MB
��B
�B
�S111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105249  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604192828  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604192828  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604192828                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605042836  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605042836  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610161505                      G�O�G�O�G�O�                