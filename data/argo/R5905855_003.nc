CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:11:01Z creation;2022-06-04T19:11:03Z conversion to V3.1      
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
_FillValue                 �  I(   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  pP   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t<   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �x   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �d   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �H   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �    SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �    SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �    SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �    HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �L   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �P   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �T   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �X   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �\   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20220604191101  20220610151507  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @ج�N�@y1   @ج�S?V@0	��l�D�d�
=p�1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B���B�  B�33B�  B�  B�  B���B�  B�  B�33B�33B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B���B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C�C33C�C��C�fC   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C5�fC8  C:�C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Ci�fCl  Cn  Cp  Cr  Ct  Cv  Cx  Cz�C|�C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5�fD6fD6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�C3Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@�{@�{A
=A?
=A_
=A
=A��A��A��A��AυA߅A�A��BBBBB'B/B7B?BGBOBWB_BgBoBwBB��HB�z�B��HB�{B��HB��HB��HB��B��HB��HB�{B�{B��B��HB��HB��HB��HB��HB��HB��HB��HB��HB�{B߮B��HB��HB��HB��HB��HB��HB��HB��HC�C�C�C�C	�C�C�C�C�C�C
>C#�C
>C�qC�
C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�
C7�C:
>C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�
Ck�Cm�Co�Cq�Cs�Cu�Cw�Cz
>C|
>C}�C�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC�C�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RD |)D �)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D	|)D	�)D
|)D
�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D |)D �)D!|)D!�)D"|)D"�)D#|)D#�)D$|)D$�)D%|)D%�)D&|)D&�)D'|)D'�)D(|)D(�)D)|)D)�)D*|)D*�)D+|)D+�)D,|)D,�)D-|)D-�)D.|)D.�)D/|)D/�)D0|)D0�)D1|)D1�)D2|)D2�)D3|)D3�)D4|)D4�)D5��D6�D6|)D6�)D7|)D7�)D8|)D8�)D9|)D9�)D:|)D:�)D;|)D;�)D<|)D<�)D=|)D=�)D>|)D>�)D?|)D?�)D@|)D@�)DA|)DA�)DB|)DB�)DC|)DC�)DD|)DD�)DE|)DE�)DF|)DF�)DG|)DG�)DH|)DH�)DI|)DI�)DJ|)DJ�)DK|)DK�)DL|)DL�)DM|)DM�)DN|)DN�)DO|)DO�)DP|)DP�)DQ|)DQ�)DR|)DR�)DS|)DS�)DT|)DT�)DU|)DU�)DV|)DV�)DW|)DW�)DX|)DX�)DY|)DY�)DZ|)DZ�)D[|)D[�)D\|)D\�)D]|)D]�)D^|)D^�)D_|)D_�)D`|)D`�)Da|)Da�)Db|)Db�)Dc|)Dc�)Dd|)Dd�)De|)De�)Df|)Df�)Dg|)Dg�)Dh|)Dh�)Di|)Di�)Dj|)Dj�)Dk|)Dk�)Dl|)Dl�)Dm|)Dm�)Dn|)Dn�)Do|)Do�)Dp|)Dp�)Dq|)Dq�)Dr|)Dr�)Ds|)Ds�)Dt|)Dt�)Du|)Du�)Dv|)Dv�)Dw|)Dw�)Dx|)Dx�)Dy|)Dy�)Dz|)Dz�)D{|)D{�)D||)D|�)D}|)D}�)D~|)D~�)D|)D�)D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D¾D��D�>D�~DþD��D�>D�~DľD��D�>D�~DžD��D�>D�~DƾD��D�>D�~DǾD��D�>D�~DȾD��D�>D�~DɾD��D�>D�~DʾD��D�AGD�~D˾D��D�>D�~D̾D��D�>D�~D;D��D�>D�~DξD��D�>D�~DϾD��D�>D�~DоD��D�>D�~DѾD��D�>D�~DҾD��D�>D�~DӾD��D�>D�~DԾD��D�>D�~DվD��D�>D�~D־D��D�>D�~D׾D��D�>D�~DؾD��D�>D�~DپD��D�>D�~DھD��D�>D�~D۾D��D�>D�~DܾD��D�>D�~DݾD��D�>D�~D޾D��D�>D�~D߾D��D�>D�~D�D���D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D��D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�qA�rA�t�A�s�A�s�A�r|A�hsA�h�A�i�A�j�A�j�A�jA�jA�kA�jA�lWA�oiA�o�A�qAA�r�A�s�A�t�A�wfA�x�A�y�A�z�A�x�A�wfAȊ	AȪeA��A�چA�!�A�2�AɆ�A�~(A˨�A˝~A��EA���Aƻ�A�!�AĵAČ~A�y�A�gA�Q�AýqA�VmA��cA�S&A�c�A���A�:*A���A�VmA���A���A��gA�}�A��A���A��GA�^�A�8A�L0A���A�1�A���A�iA��qA��A��A��yA�%FA�"4A�!�A���A�qA���A�kQA��FA���A���A��zA�5?A���A���A~��AyƨAtc Ao�8AmH�Ai�Ad��A`&�A\�AXL�AS��AQݘANsAK�AJ$�AH��AHYAGGEAF�AE�hAA�4A@�)A@TaA>PHA<�A=�A="�A<K^A;��A9�mA9 �A6p�A4E9A3�A1f�A0�A/_A.J�A-"hA+�NA*7LA)s�A)B[A)sA(��A(xA'ƨA'��A'%FA&�pA&��A&s�A%��A$1�A"�5A"&�A!�fA �>A 2aA iA1A�QA��A#:A]�A�A2�A��AA�A�AA��A~�A�zA�AbNA�A�eA��ARTA�A�wA�A��A��A=qAݘA�LA��A"hAw�A!A��A�\AMjA��A��A|AA A��A[WAc�AuA
��A
~A	��A	rGA	8�A	%A�RAbNAT�A��A�AffACA�{AA~�A33A�A@OA�Ae�A �9A Z�A S&A �@��@�C@��@���@��@�/�@�oi@���@�F@�͟@�H�@���@�%@���@��\@�7@��@�;@�O�@�@�q�@�ԕ@�1@��D@�˒@�m]@�&�@��/@�e�@�{@��@��v@艠@�o @�"h@�+�@�@�@��@�/@�E�@�t@�u�@��@�B[@�L�@�ں@ދD@��@�e,@�@��y@�|�@��;@�X@��@�6@�<6@��@�n�@�a|@�@�qv@��[@�h�@���@�1�@��@��v@�@ӄM@�_p@�`B@�o@҆Y@�S�@��@Р�@Ѓ@�?�@��@��T@�s�@Ή�@��@��@��T@��@ͧ�@��@̀�@�9X@�5?@��.@˩*@�x�@�=@��c@ʵ@ʬ@�}V@���@�,�@Ƞ�@�>B@��@�O�@���@�kQ@� �@���@ļj@� �@��z@ÖS@��@�{�@���@�y>@�c @�e@���@���@��	@�y�@�5�@���@��\@���@���@�y�@�C@���@�8�@��w@�^�@�'�@��B@�g8@�1@�\)@��"@�Q�@�4�@��<@��+@�]d@�3�@�'R@��@���@��F@�+@��j@�a|@��
@�J#@�4�@��H@�~(@�J�@��@��P@�B�@��@���@�~(@�Q@�6@�M@���@�N<@��?@���@��@���@�x�@�q@���@��}@��@�M@���@��@�Vm@�o@���@��@��F@��K@���@�IR@���@���@��o@�#:@��V@�F�@���@�]d@�9X@��@�@�g�@�,�@��@��@�ں@���@��\@���@�oi@��@���@���@�O@��<@�j@�%�@��;@�e,@���@��@�~�@��]@���@��@���@��e@���@���@�q@���@��@�PH@��@�خ@��[@��@��@���@��@���@�IR@�	l@���@�~�@�E�@���@��S@�(�@��v@��@���@�V�@�G@���@���@�k�@�6z@��_@�d�@�x@�_p@��@�ѷ@��<@���@���@�Q�@��@���@�m]@�IR@�C�@�,�@���@���@�u%@�[�@�S�@�<�@�-�@�
�@�\�@��@�%@��B@�c�@� �@���@��}@��@@�v`@�_p@�G�@��@���@���@��@�d�@�@�o @�7L@��"@���@�s�@�^5@�H�@�5?@��m@��@��@�t�@�6z@��@�8�@�$�@��@���@���@�/�@���@��)@��z@��u@�y>@�Ta@�@��D@��@���@�O�@�A�@�0�@���@���@�H@��@��a@��P@�T�@�IR@�=@�,�@��@��]@��@�@t�@)_@~�]@~�A@}��@}X@}�@|Ɇ@|��@|M@{s@z��@z{�@y�@x��@xr�@we�@v�@v�@v�b@v��@vxl@v-@uk�@t�@s��@s��@r.�@q�M@q0�@p֡@p��@pr�@o�A@o�
@n�@n��@m�@mO�@lɆ@k��@k��@ke�@kY@j��@j�@j�b@j:*@i��@i��@i�d@i�@i��@i�~@iVm@i@htT@hx@g��@g6z@g>�@gU�@f��@f��@fd�@fd�@fYK@f �@em]@e%F@e�@d�j@dPH@d-�@c��@cF�@b�s@b1�@a�@a0�@`��@`�p@`PH@_��@_�
@_�q@_x@_]�@_@O@^��@^�h@^�F@^^5@^{@]�@]�C@]|@]G�@\�v@\��@\�@\�.@\u�@\Xy@[�;@[��@[dZ@[8@[o@Z�@Z�6@Z^5@Yf�@YB�@X�@W�+@W��@W��@W8@V�y@V��@V\�@U�j@U�'@U��@U�"@Us�@U�@T�j@TXy@S�]@S�a@SX�@R��@R�h@Rh
@R)�@Q��@Q��@Q�=@Q#�@P��@P��@P`�@P,=@O�$@Oa@O�@N�1@NGE@M��@Mzx@L��@LXy@L"h@K�K@KZ�@KY@J�,@J��@Jp;@JYK@I�D@I��@I2a@HĜ@HN�@Hx@G��@Gs@GY@F�M@F~�@E�o@Eԕ@Ee,@E0�@E%@D��@Dz�@D$@C�@C;d@B�"@B��@Bxl@BH�@B�@A�T@A�T@A\�@AV@@֡@@�@@V�@@�@?W?@>��@>�b@>��@>c @>Q@>�@=�@=�h@=7L@<�P@<��@<��@<  @;O@:ߤ@:�@:J�@:+k@9�z@9}�@9�@8��@8q@8Q�@8~@8�@7�m@7��@7~�@6�"@6��@6��@6q�@6@�@6e@5��@5�t@5�h@5p�@5Dg@4�|@4�@4S�@4�@3�@3��@3\)@3�@2�!@2��@2~�@2GE@2{@1�@1�^@1��@1��@1�C@1��@1��@1#�@0w�@0$@/� @/s@/�@.��@.�F@.xl@.8�@.�@-��@-Q�@,�P@,q@+��@+��@+Z�@+&@+�@*��@*:*@)�T@)�@)��@)�'@)k�@)8�@)�@(�@(m�@(`�@(Z@(6@(@'�W@'��@'b�@&�s@&��@&_�@&C�@%�3@%f�@%#�@%@$�/@$�9@$�z@$tT@$9X@#��@#��@#�0@#��@#y�@#�@"�@"?@!�)@!��@![W@!<6@!&�@ ��@ �5@ �9@ q@ 4n@�]@��@t�@e�@a@Z�@A�@!-@�@�,@�}@u%@;�@�D@ϫ@��@��@S&@=�@V@�)@�@j@7�@�]@�6@�@��@o�@8@�@��@�b@��@s�@5?@�@�@O�@+@�@��@w�@<�@7@�r@��@��@n/@E9@�@�@�b@xl@_�@R�@1�@��@c�@5�@��@�@�@e�@�@�;@��@��@�	@dZ@J#@C@�M@�,@_�@Q@W�@L0@��@��@��@hs@B�@�@��@��@�@q@[�@Q�@?�@/�@�@  @˒@O@�@��@��@��@� @�F@}V@d�@^5@
�@�z@�S@Vm@4@;@�e@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�qA�rA�t�A�s�A�s�A�r|A�hsA�h�A�i�A�j�A�j�A�jA�jA�kA�jA�lWA�oiA�o�A�qAA�r�A�s�A�t�A�wfA�x�A�y�A�z�A�x�A�wfAȊ	AȪeA��A�چA�!�A�2�AɆ�A�~(A˨�A˝~A��EA���Aƻ�A�!�AĵAČ~A�y�A�gA�Q�AýqA�VmA��cA�S&A�c�A���A�:*A���A�VmA���A���A��gA�}�A��A���A��GA�^�A�8A�L0A���A�1�A���A�iA��qA��A��A��yA�%FA�"4A�!�A���A�qA���A�kQA��FA���A���A��zA�5?A���A���A~��AyƨAtc Ao�8AmH�Ai�Ad��A`&�A\�AXL�AS��AQݘANsAK�AJ$�AH��AHYAGGEAF�AE�hAA�4A@�)A@TaA>PHA<�A=�A="�A<K^A;��A9�mA9 �A6p�A4E9A3�A1f�A0�A/_A.J�A-"hA+�NA*7LA)s�A)B[A)sA(��A(xA'ƨA'��A'%FA&�pA&��A&s�A%��A$1�A"�5A"&�A!�fA �>A 2aA iA1A�QA��A#:A]�A�A2�A��AA�A�AA��A~�A�zA�AbNA�A�eA��ARTA�A�wA�A��A��A=qAݘA�LA��A"hAw�A!A��A�\AMjA��A��A|AA A��A[WAc�AuA
��A
~A	��A	rGA	8�A	%A�RAbNAT�A��A�AffACA�{AA~�A33A�A@OA�Ae�A �9A Z�A S&A �@��@�C@��@���@��@�/�@�oi@���@�F@�͟@�H�@���@�%@���@��\@�7@��@�;@�O�@�@�q�@�ԕ@�1@��D@�˒@�m]@�&�@��/@�e�@�{@��@��v@艠@�o @�"h@�+�@�@�@��@�/@�E�@�t@�u�@��@�B[@�L�@�ں@ދD@��@�e,@�@��y@�|�@��;@�X@��@�6@�<6@��@�n�@�a|@�@�qv@��[@�h�@���@�1�@��@��v@�@ӄM@�_p@�`B@�o@҆Y@�S�@��@Р�@Ѓ@�?�@��@��T@�s�@Ή�@��@��@��T@��@ͧ�@��@̀�@�9X@�5?@��.@˩*@�x�@�=@��c@ʵ@ʬ@�}V@���@�,�@Ƞ�@�>B@��@�O�@���@�kQ@� �@���@ļj@� �@��z@ÖS@��@�{�@���@�y>@�c @�e@���@���@��	@�y�@�5�@���@��\@���@���@�y�@�C@���@�8�@��w@�^�@�'�@��B@�g8@�1@�\)@��"@�Q�@�4�@��<@��+@�]d@�3�@�'R@��@���@��F@�+@��j@�a|@��
@�J#@�4�@��H@�~(@�J�@��@��P@�B�@��@���@�~(@�Q@�6@�M@���@�N<@��?@���@��@���@�x�@�q@���@��}@��@�M@���@��@�Vm@�o@���@��@��F@��K@���@�IR@���@���@��o@�#:@��V@�F�@���@�]d@�9X@��@�@�g�@�,�@��@��@�ں@���@��\@���@�oi@��@���@���@�O@��<@�j@�%�@��;@�e,@���@��@�~�@��]@���@��@���@��e@���@���@�q@���@��@�PH@��@�خ@��[@��@��@���@��@���@�IR@�	l@���@�~�@�E�@���@��S@�(�@��v@��@���@�V�@�G@���@���@�k�@�6z@��_@�d�@�x@�_p@��@�ѷ@��<@���@���@�Q�@��@���@�m]@�IR@�C�@�,�@���@���@�u%@�[�@�S�@�<�@�-�@�
�@�\�@��@�%@��B@�c�@� �@���@��}@��@@�v`@�_p@�G�@��@���@���@��@�d�@�@�o @�7L@��"@���@�s�@�^5@�H�@�5?@��m@��@��@�t�@�6z@��@�8�@�$�@��@���@���@�/�@���@��)@��z@��u@�y>@�Ta@�@��D@��@���@�O�@�A�@�0�@���@���@�H@��@��a@��P@�T�@�IR@�=@�,�@��@��]@��@�@t�@)_@~�]@~�A@}��@}X@}�@|Ɇ@|��@|M@{s@z��@z{�@y�@x��@xr�@we�@v�@v�@v�b@v��@vxl@v-@uk�@t�@s��@s��@r.�@q�M@q0�@p֡@p��@pr�@o�A@o�
@n�@n��@m�@mO�@lɆ@k��@k��@ke�@kY@j��@j�@j�b@j:*@i��@i��@i�d@i�@i��@i�~@iVm@i@htT@hx@g��@g6z@g>�@gU�@f��@f��@fd�@fd�@fYK@f �@em]@e%F@e�@d�j@dPH@d-�@c��@cF�@b�s@b1�@a�@a0�@`��@`�p@`PH@_��@_�
@_�q@_x@_]�@_@O@^��@^�h@^�F@^^5@^{@]�@]�C@]|@]G�@\�v@\��@\�@\�.@\u�@\Xy@[�;@[��@[dZ@[8@[o@Z�@Z�6@Z^5@Yf�@YB�@X�@W�+@W��@W��@W8@V�y@V��@V\�@U�j@U�'@U��@U�"@Us�@U�@T�j@TXy@S�]@S�a@SX�@R��@R�h@Rh
@R)�@Q��@Q��@Q�=@Q#�@P��@P��@P`�@P,=@O�$@Oa@O�@N�1@NGE@M��@Mzx@L��@LXy@L"h@K�K@KZ�@KY@J�,@J��@Jp;@JYK@I�D@I��@I2a@HĜ@HN�@Hx@G��@Gs@GY@F�M@F~�@E�o@Eԕ@Ee,@E0�@E%@D��@Dz�@D$@C�@C;d@B�"@B��@Bxl@BH�@B�@A�T@A�T@A\�@AV@@֡@@�@@V�@@�@?W?@>��@>�b@>��@>c @>Q@>�@=�@=�h@=7L@<�P@<��@<��@<  @;O@:ߤ@:�@:J�@:+k@9�z@9}�@9�@8��@8q@8Q�@8~@8�@7�m@7��@7~�@6�"@6��@6��@6q�@6@�@6e@5��@5�t@5�h@5p�@5Dg@4�|@4�@4S�@4�@3�@3��@3\)@3�@2�!@2��@2~�@2GE@2{@1�@1�^@1��@1��@1�C@1��@1��@1#�@0w�@0$@/� @/s@/�@.��@.�F@.xl@.8�@.�@-��@-Q�@,�P@,q@+��@+��@+Z�@+&@+�@*��@*:*@)�T@)�@)��@)�'@)k�@)8�@)�@(�@(m�@(`�@(Z@(6@(@'�W@'��@'b�@&�s@&��@&_�@&C�@%�3@%f�@%#�@%@$�/@$�9@$�z@$tT@$9X@#��@#��@#�0@#��@#y�@#�@"�@"?@!�)@!��@![W@!<6@!&�@ ��@ �5@ �9@ q@ 4n@�]@��@t�@e�@a@Z�@A�@!-@�@�,@�}@u%@;�@�D@ϫ@��@��@S&@=�@V@�)@�@j@7�@�]@�6@�@��@o�@8@�@��@�b@��@s�@5?@�@�@O�@+@�@��@w�@<�@7@�r@��@��@n/@E9@�@�@�b@xl@_�@R�@1�@��@c�@5�@��@�@�@e�@�@�;@��@��@�	@dZ@J#@C@�M@�,@_�@Q@W�@L0@��@��@��@hs@B�@�@��@��@�@q@[�@Q�@?�@/�@�@  @˒@O@�@��@��@��@� @�F@}V@d�@^5@
�@�z@�S@Vm@4@;@�e@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B��B��B��B��B��B��B��B��B�fB��B��B�B��B�B��B��B��B��B��B�B�B��B��B��B��B��B��B�B	  B	 B	+kB	>�B	u�B	�B	��B
B
��B
�BaB%`BC�B�B��B�&B�B��B�wBŢB��B��B�B1B�?B�B��B�pB�B�BۦBچB��B�9B�EB�B�kB�B�jBרB�[B�CB�$B��B��B�rBΊB�B��BQ4B�B
�B
�B
��B
��B
uZB
a�B
C�B
,"B
�B	��B	��B	��B	uZB	cnB	TFB	J#B	9>B	+�B	�B	6B	�B��B��B��B�lB�ZB��B��B�aB��B	B	B	�B	�B	mB	=B	�B	MB	bB	
�B	UB��B	UB��B�vB�B�fB�B�B�B�B	�B	5B	MB	^OB	oiB	vFB	|�B	�B	�#B	��B	�.B	��B	��B	�B	�[B	�:B	��B	��B	y�B	c�B	[�B	^B	_pB	abB	f2B	iDB	m�B	q�B	~�B	��B	��B	��B	�}B	��B	�bB	��B	��B	��B	�vB	��B	��B	�BB	�jB	��B	��B	�B	�#B	��B	��B	��B	�tB	��B	�hB	�GB	��B	��B	�B	�B	�IB	��B	��B	�cB	�}B	��B	��B	�B	��B	�;B	�GB	�B	��B	��B	��B	��B	�B	��B	�B	��B	��B	��B	�|B	�iB	�5B	�vB	�B	�ZB	�`B	�B	�%B	��B	�B	��B	�8B	��B	��B	��B	�0B	��B	� B	�[B	żB	ŢB	�B	�HB	�JB	��B	�6B	��B	��B	��B	��B	�BB	��B	�"B	�B	��B	��B	�<B	�OB	ªB	��B	�{B	ĶB	āB	�B	�3B	�gB	ĶB	��B	�KB	ȴB	ȴB	�B	��B	ʦB	��B	�^B	��B	�B	�<B	��B	ϫB	� B	�B	ѷB	��B	��B	��B	��B	ԯB	�B	�B	�B	�YB	��B	��B	�sB	רB	�B	��B	�KB	�KB	�eB	ٚB	ٴB	�eB	�B	�WB	��B	�	B	��B	��B	��B	�#B	�=B	�#B	��B	�=B	�WB	�WB	�qB	�]B	��B	�B	�jB	�5B	�B	�'B	��B	�HB	�B	�4B	�B	�hB	�B	�B	��B	�B	��B	�B	��B	�ZB	��B	�NB	�:B	�2B	�B	�RB	��B	��B	��B	�B	�WB	�=B	�B	��B	��B	�CB	�IB	��B	��B	�B	�B	�B	��B	�iB	�OB	��B	��B	�B	��B	�TB	�tB	��B	��B	�fB	��B	�lB	��B	��B	��B	��B	��B	�B	�B	�jB	��B	�B	�qB	�BB	��B	��B	�]B	�BB	�]B	�.B	��B	�}B
 OB
 B
 iB
 OB
 �B
 �B	��B
 OB
B
�B
[B
[B
�B
;B
UB
 iB	�qB	�B	�dB	�dB	��B	�wB	��B	�}B	��B
�B
�B
�B
9B
�B
%B
�B
B
�B
_B
_B
+B
�B
�B
tB
%B
tB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
1B
�B
�B
�B
�B
�B

�B
~B
6B
�B
�B
<B
(B
�B
�B
�B
�B
 B
4B
NB
�B
�B
�B
�B
�B
�B
�B
bB
B
NB
NB
�B
�B
�B
�B
oB
�B
uB
�B
�B
MB
�B
�B
mB
?B
�B
EB
+B
+B
EB
�B
1B
�B
�B
�B
�B
�B
B
7B
�B
�B
�B
7B
�B
#B
	B
qB
�B
�B
�B
]B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
pB
pB
�B
 BB
 \B
 BB
 �B
!�B
"4B
"B
"B
"4B
"�B
#�B
$�B
%B
%zB
%zB
%�B
%�B
&�B
&�B
&�B
&�B
'�B
'�B
'�B
(sB
(�B
)*B
)DB
)*B
)yB
)�B
)�B
)�B
*B
+B
+�B
,�B
-�B
-�B
./B
./B
.}B
/B
/�B
/�B
0B
0UB
0oB
0�B
0�B
1'B
1�B
2-B
2-B
2�B
2�B
3hB
3�B
3�B
3�B
3�B
4nB
4B
4nB
4nB
4�B
5?B
5�B
5�B
5�B
6`B
72B
8�B
9rB
9�B
9�B
9�B
9�B
:DB
:^B
:�B
:�B
;0B
;dB
;�B
;B
;�B
;�B
<PB
<�B
<�B
=B
=�B
=�B
=<B
<�B
<�B
<�B
=�B
>�B
>B
=�B
=�B
>(B
>B
>(B
>wB
>�B
>wB
>BB
>�B
?HB
?cB
@B
A;B
A�B
A�B
A�B
A�B
A�B
A�B
BB
BB
B[B
B�B
B�B
B�B
CGB
C{B
C�B
C�B
D3B
D3B
DgB
D�B
D�B
EB
E9B
ESB
EmB
ESB
ESB
E�B
E�B
FB
F?B
F?B
F%B
E�B
F%B
E�B
E�B
F?B
F�B
F�B
F�B
F�B
GB
GzB
G�B
G�B
H1B
HKB
H1B
H1B
H�B
H�B
IB
IB
I7B
I�B
I�B
I�B
J	B
J#B
JXB
KDB
K�B
LdB
LJB
L~B
L�B
MB
M6B
M6B
M6B
MPB
MjB
MjB
M�B
NB
NVB
NpB
NpB
N�B
N�B
N�B
OB
OB
N�B
OB
OBB
O\B
O�B
O�B
PB
PbB
P�B
P�B
P}B
P�B
P�B
P�B
P�B
Q4B
QNB
QhB
QhB
Q�B
RB
RTB
R:B
R B
RB
RB
R�B
S&B
S&B
S&B
S�B
S�B
S�B
S�B
S�B
T,B
T�B
T�B
T�B
T�B
U�B
VB
VB
V9B
V�B
V�B
V�B
V�B
WYB
W�B
XB
X+B
X+B
X+B
XyB
XyB
X�B
YB
Y1B
YeB
Y�B
YB
Y�B
Y�B
Y�B
ZQB
ZQB
ZkB
Z�B
Z�B
Z�B
Z�B
[#B
[	B
Z�B
Z�B
Z�B
[WB
[�B
[�B
[�B
[�B
[�B
\B
\CB
\]B
\xB
\�B
]B
]dB
]~B
]�B
]�B
]�B
]�B
]~B
^B
^jB
^jB
^�B
^�B
^�B
_VB
_pB
_�B
_�B
_�B
`B
`B
`B
`vB
aB
aHB
a�B
a�B
a�B
a�B
b4B
bNB
bhB
bhB
b�B
b�B
b�B
c B
cTB
c�B
c�B
c�B
d&B
dZB
dZB
d�B
eB
e�B
e�B
e�B
e�B
ffB
f�B
f�B
f�B
gB
g8B
gB
gmB
gmB
g�B
g�B
g�B
g�B
g�B
g�B
h>B
h�B
h�B
h�B
iDB
iDB
i_B
i_B
i_B
i�B
i�B
i�B
i�B
jeB
jeB
jB
jeB
jB
j�B
jB
j�B
j�B
j�B
k6B
k6B
k�B
k�B
k�B
k�B
lB
lB
l=B
lqB
l�B
l�B
mB
mCB
mwB
m�B
m�B
m�B
nB
n/B
nIB
ncB
ncB
n}B
n�B
n�B
o B
oOB
oiB
o�B
o�B
o�B
pB
p!B
p;B
poB
p�B
p�B
p�B
p�B
qAB
q[B
q�B
q�B
q�B
q�B
rGB
r�B
r�B
r�B
s�B
s�B
s�B
tB
t9B
tTB
tTB
t�B
t�B
t�B
t�B
uB
u%B
u�B
u�B
u�B
u�B
vB
v+B
v`B
vzB
v�B
v�B
wB
w2B
wLB
w�B
w�B
w�B
w�B
w�B
w�B
w�B
xB
x�B
y>B
y>B
yXB
y�B
y�B
y�B
y�B
y�B
y�B
zDB
z^B
z^B
z�B
z�B
{B
{B
{�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B��B��B��B��B��B��B��B��B�fB��B��B�B��B�B��B��B��B��B��B�B�B��B��B��B��B��B��B�B	  B	 B	+kB	>�B	u�B	�B	��B
B
��B
�BaB%`BC�B�B��B�&B�B��B�wBŢB��B��B�B1B�?B�B��B�pB�B�BۦBچB��B�9B�EB�B�kB�B�jBרB�[B�CB�$B��B��B�rBΊB�B��BQ4B�B
�B
�B
��B
��B
uZB
a�B
C�B
,"B
�B	��B	��B	��B	uZB	cnB	TFB	J#B	9>B	+�B	�B	6B	�B��B��B��B�lB�ZB��B��B�aB��B	B	B	�B	�B	mB	=B	�B	MB	bB	
�B	UB��B	UB��B�vB�B�fB�B�B�B�B	�B	5B	MB	^OB	oiB	vFB	|�B	�B	�#B	��B	�.B	��B	��B	�B	�[B	�:B	��B	��B	y�B	c�B	[�B	^B	_pB	abB	f2B	iDB	m�B	q�B	~�B	��B	��B	��B	�}B	��B	�bB	��B	��B	��B	�vB	��B	��B	�BB	�jB	��B	��B	�B	�#B	��B	��B	��B	�tB	��B	�hB	�GB	��B	��B	�B	�B	�IB	��B	��B	�cB	�}B	��B	��B	�B	��B	�;B	�GB	�B	��B	��B	��B	��B	�B	��B	�B	��B	��B	��B	�|B	�iB	�5B	�vB	�B	�ZB	�`B	�B	�%B	��B	�B	��B	�8B	��B	��B	��B	�0B	��B	� B	�[B	żB	ŢB	�B	�HB	�JB	��B	�6B	��B	��B	��B	��B	�BB	��B	�"B	�B	��B	��B	�<B	�OB	ªB	��B	�{B	ĶB	āB	�B	�3B	�gB	ĶB	��B	�KB	ȴB	ȴB	�B	��B	ʦB	��B	�^B	��B	�B	�<B	��B	ϫB	� B	�B	ѷB	��B	��B	��B	��B	ԯB	�B	�B	�B	�YB	��B	��B	�sB	רB	�B	��B	�KB	�KB	�eB	ٚB	ٴB	�eB	�B	�WB	��B	�	B	��B	��B	��B	�#B	�=B	�#B	��B	�=B	�WB	�WB	�qB	�]B	��B	�B	�jB	�5B	�B	�'B	��B	�HB	�B	�4B	�B	�hB	�B	�B	��B	�B	��B	�B	��B	�ZB	��B	�NB	�:B	�2B	�B	�RB	��B	��B	��B	�B	�WB	�=B	�B	��B	��B	�CB	�IB	��B	��B	�B	�B	�B	��B	�iB	�OB	��B	��B	�B	��B	�TB	�tB	��B	��B	�fB	��B	�lB	��B	��B	��B	��B	��B	�B	�B	�jB	��B	�B	�qB	�BB	��B	��B	�]B	�BB	�]B	�.B	��B	�}B
 OB
 B
 iB
 OB
 �B
 �B	��B
 OB
B
�B
[B
[B
�B
;B
UB
 iB	�qB	�B	�dB	�dB	��B	�wB	��B	�}B	��B
�B
�B
�B
9B
�B
%B
�B
B
�B
_B
_B
+B
�B
�B
tB
%B
tB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
1B
�B
�B
�B
�B
�B

�B
~B
6B
�B
�B
<B
(B
�B
�B
�B
�B
 B
4B
NB
�B
�B
�B
�B
�B
�B
�B
bB
B
NB
NB
�B
�B
�B
�B
oB
�B
uB
�B
�B
MB
�B
�B
mB
?B
�B
EB
+B
+B
EB
�B
1B
�B
�B
�B
�B
�B
B
7B
�B
�B
�B
7B
�B
#B
	B
qB
�B
�B
�B
]B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
pB
pB
�B
 BB
 \B
 BB
 �B
!�B
"4B
"B
"B
"4B
"�B
#�B
$�B
%B
%zB
%zB
%�B
%�B
&�B
&�B
&�B
&�B
'�B
'�B
'�B
(sB
(�B
)*B
)DB
)*B
)yB
)�B
)�B
)�B
*B
+B
+�B
,�B
-�B
-�B
./B
./B
.}B
/B
/�B
/�B
0B
0UB
0oB
0�B
0�B
1'B
1�B
2-B
2-B
2�B
2�B
3hB
3�B
3�B
3�B
3�B
4nB
4B
4nB
4nB
4�B
5?B
5�B
5�B
5�B
6`B
72B
8�B
9rB
9�B
9�B
9�B
9�B
:DB
:^B
:�B
:�B
;0B
;dB
;�B
;B
;�B
;�B
<PB
<�B
<�B
=B
=�B
=�B
=<B
<�B
<�B
<�B
=�B
>�B
>B
=�B
=�B
>(B
>B
>(B
>wB
>�B
>wB
>BB
>�B
?HB
?cB
@B
A;B
A�B
A�B
A�B
A�B
A�B
A�B
BB
BB
B[B
B�B
B�B
B�B
CGB
C{B
C�B
C�B
D3B
D3B
DgB
D�B
D�B
EB
E9B
ESB
EmB
ESB
ESB
E�B
E�B
FB
F?B
F?B
F%B
E�B
F%B
E�B
E�B
F?B
F�B
F�B
F�B
F�B
GB
GzB
G�B
G�B
H1B
HKB
H1B
H1B
H�B
H�B
IB
IB
I7B
I�B
I�B
I�B
J	B
J#B
JXB
KDB
K�B
LdB
LJB
L~B
L�B
MB
M6B
M6B
M6B
MPB
MjB
MjB
M�B
NB
NVB
NpB
NpB
N�B
N�B
N�B
OB
OB
N�B
OB
OBB
O\B
O�B
O�B
PB
PbB
P�B
P�B
P}B
P�B
P�B
P�B
P�B
Q4B
QNB
QhB
QhB
Q�B
RB
RTB
R:B
R B
RB
RB
R�B
S&B
S&B
S&B
S�B
S�B
S�B
S�B
S�B
T,B
T�B
T�B
T�B
T�B
U�B
VB
VB
V9B
V�B
V�B
V�B
V�B
WYB
W�B
XB
X+B
X+B
X+B
XyB
XyB
X�B
YB
Y1B
YeB
Y�B
YB
Y�B
Y�B
Y�B
ZQB
ZQB
ZkB
Z�B
Z�B
Z�B
Z�B
[#B
[	B
Z�B
Z�B
Z�B
[WB
[�B
[�B
[�B
[�B
[�B
\B
\CB
\]B
\xB
\�B
]B
]dB
]~B
]�B
]�B
]�B
]�B
]~B
^B
^jB
^jB
^�B
^�B
^�B
_VB
_pB
_�B
_�B
_�B
`B
`B
`B
`vB
aB
aHB
a�B
a�B
a�B
a�B
b4B
bNB
bhB
bhB
b�B
b�B
b�B
c B
cTB
c�B
c�B
c�B
d&B
dZB
dZB
d�B
eB
e�B
e�B
e�B
e�B
ffB
f�B
f�B
f�B
gB
g8B
gB
gmB
gmB
g�B
g�B
g�B
g�B
g�B
g�B
h>B
h�B
h�B
h�B
iDB
iDB
i_B
i_B
i_B
i�B
i�B
i�B
i�B
jeB
jeB
jB
jeB
jB
j�B
jB
j�B
j�B
j�B
k6B
k6B
k�B
k�B
k�B
k�B
lB
lB
l=B
lqB
l�B
l�B
mB
mCB
mwB
m�B
m�B
m�B
nB
n/B
nIB
ncB
ncB
n}B
n�B
n�B
o B
oOB
oiB
o�B
o�B
o�B
pB
p!B
p;B
poB
p�B
p�B
p�B
p�B
qAB
q[B
q�B
q�B
q�B
q�B
rGB
r�B
r�B
r�B
s�B
s�B
s�B
tB
t9B
tTB
tTB
t�B
t�B
t�B
t�B
uB
u%B
u�B
u�B
u�B
u�B
vB
v+B
v`B
vzB
v�B
v�B
wB
w2B
wLB
w�B
w�B
w�B
w�B
w�B
w�B
w�B
xB
x�B
y>B
y>B
yXB
y�B
y�B
y�B
y�B
y�B
y�B
zDB
z^B
z^B
z�B
z�B
{B
{B
{�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105226  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604191101  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604191102  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604191103                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605041111  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605041111  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610151507                      G�O�G�O�G�O�                