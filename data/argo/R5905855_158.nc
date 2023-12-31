CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2023-06-13T06:49:02Z creation;2023-06-13T06:49:03Z conversion to V3.1      
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
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20230613064902  20230613073048  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @�2����R1   @�2�\(�@0�     �c�z�G�1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @,��@�  @�  @���AffA@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B ffB(ffB0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�ffB�  B�ffB���B���B�  B���B�  B�  B�  BЙ�B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C�C�C�fC  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB�CD�CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�C3Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�i�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @(��@|(�@�{@��HAp�A?
=A_
=A
=A��A��A��A��AυA߅A�A��BBBB (�B((�B/B7B?BGBOBWB_BgBoBwBB��HB��HB��HB��HB��HB��HB��HB��HB��HB�G�B��HB�G�B�z�B��B��HB��B��HB��HB��HB�z�BӮB׮B��HB��HB��HB��HB��HB��HB��HB��HB��HB��HC�C�C�C�C	�C�C
>C
>C�
C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CB
>CD
>CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC�C��C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC�C�C��RD |)D �)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D	|)D	�)D
|)D
�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D��D�)D |)D �)D!|)D!�)D"|)D"�)D#|)D#�)D$|)D$�)D%|)D%�)D&|)D&�)D'|)D'�)D(|)D(�)D)|)D)�)D*|)D*�)D+|)D+�)D,|)D,�)D-|)D-�)D.|)D.�)D/|)D/�)D0|)D0�)D1|)D1�)D2|)D2�)D3|)D3�)D4|)D4�)D5|)D5�)D6|)D6�)D7|)D7�)D8|)D8�)D9|)D9�)D:|)D:�)D;|)D;�)D<|)D<�)D=|)D=�)D>|)D>�)D?|)D?�)D@|)D@�)DA|)DA�)DB|)DB�)DC|)DC�)DD|)DD�)DE|)DE�)DF|)DF�)DG|)DG�)DH|)DH�)DI|)DI�)DJ|)DJ�)DK|)DK�)DL|)DL�)DM|)DM�)DN|)DN�)DO|)DO�)DP|)DP�)DQ|)DQ�)DR|)DR�)DS|)DS�)DT|)DT�)DU|)DU�)DV|)DV�)DW|)DW�)DX|)DX�)DY|)DY�)DZ|)DZ�)D[|)D[�)D\|)D\�)D]|)D]�)D^|)D^�)D_|)D_�)D`|)D`�)Da|)Da�)Db|)Db�)Dc|)Dc�)Dd|)Dd�)De|)De�)Df|)Df�)Dg|)Dg�)Dh|)Dh�)Di|)Di�)Dj|)Dj�)Dk|)Dk�)Dl|)Dl�)Dm|)Dm�)Dn|)Dn�)Do|)Do�)Dp|)Dp�)Dq|)Dq�)Dr|)Dr�)Ds|)Ds�)Dt|)Dt�)Du|)Du�)Dv|)Dv�)Dw|)Dw�)Dx|)Dx�)Dy|)Dy�)Dz|)Dz�)D{|)D{�)D||)D|�)D}|)D}�)D~|)D~�)D|)D�)D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��GD��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D¾D��D�>D�~DþD��D�>D�~DľD��D�>D�~DžD��D�>D�~DƾD��D�>D�~DǾD��D�>D�~DȾD��D�>D�~DɾD��D�>D�~DʾD��D�>D�~D˾D��D�>D�~D̾D��D�>D�~D;D��D�>D�~DξD��D�>D�~DϾD��D�>D�~DоD��D�>D�~DѾD��D�>D�~DҾD��D�>D�~DӾD��D�>D�~DԾD��D�>D�~DվD��D�>D�~D־D��D�>D�~D׾D��D�>D�~DؾD��D�AGD�~DپD��D�>D�~DھD��D�>D�~D۾D��D�>D�~DܾD��D�>D�~DݾD��D�>D�~D޾D��D�>D�~D߾D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D��D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�g�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  Aθ�AθAέAέ�AΣnAΕ�AΏ(AΒoA·�A�jA�h�A�\�A�L�A�3hA�/�A�-A�+A�*eA�)*A�'�A�%zA�$�A�%A�!�A��A�VA�!A��A�CA��A�YA��A��A�
rA���Aͷ�A�:*A�EmA�N<A�:*A�
�Aŏ�A�CaA��A�DgA�B�A��aA�kA��A��mA�(A�kA���A�m)A��-A�J�A�eA�)�A�=A�y�A�
rA��LA���A���A���A��9A�ffA�.�A��IA��/A�c�A�|PA���A��A�($A��A��*A��A���A��A�5tA��A�6�A�\�A�8RA�ԕA��A�pA�6�A���A��qA�<�A�E�A�o A��vA�r�A�C-A�5�A��A}��A{\�Ay��Av��AtArW?ApߤAn��Al=AhXAde�Ab��A`�8A^��A]�$A[�4AWh�ANr�AL��AHe�AE5�AC��AAL�A=��A=YA<$�A8T�A5ffA3�:A2�hA2/�A1��A0
�A-ffA)�A'��A'L�A'��A&خA$a�A"�{A!rGA �aA �.A rGA�A�Af�A�oA�AݘA�`A�A+kAi�A�6A��A<�Ac Av�A:�A7A!A�hA�UA)�A'RAA8�A�)Al"A�A'RAc�A�[Ad�A��A4nA�RAH�A�A�;A�<A�A�A%A�AA��A�A5?Al"A��A�eA�A�AS&A�dA��AGEA
�A	ѷA	�A	kQA�A|A�A�]A��AN<A�fAR�A�PA�A(A�hAc�A[�A��Ap;A ��A ��A _@�k�@���@���@�&�@�*�@��@�=�@��@�l�@�O@��.@�7�@�@��@�j�@��)@�Z�@�a|@�I�@��@�t�@��3@���@�2a@�@�GE@崢@��@�o @��"@�O@�R@�/�@�"@�2a@���@�E�@��@�*@���@�h@�[�@�`�@���@ަL@���@��@���@��@�~�@�($@�c@��@��@�~�@��@�d�@Ձ@���@�?@Ӵ�@�33@�q�@�	@Ѵ�@ч�@�j�@�B�@Р�@�-w@ά�@�h�@�K^@��
@�~�@��c@�%�@�  @��@�|�@�@�@�qv@��M@�0U@�n/@Ə\@��A@���@�_�@���@Ê	@��@��	@���@�u%@�Q�@��D@���@��z@��"@�E9@���@�M�@�˒@�t�@��@���@��@�1'@��z@�s�@���@�Y�@��I@��g@���@���@�7�@��@���@�{�@� �@��.@���@�v`@�T�@�"�@��@��@��@��Q@�@�Z�@��[@��O@�=q@�hs@�%F@���@���@�+k@���@���@��$@�0�@��m@��x@�:�@��W@��"@�&@��c@��j@�r�@��.@��3@���@�B�@���@�($@���@�a@�F�@���@���@���@�~�@�A�@��@��#@�J�@��@���@�-@�@�  @��@���@�C�@� �@���@��@��@�u%@�-@���@�o @���@�v�@�V@�@�ݘ@�o @�5�@���@�ѷ@��A@�@�ԕ@��*@�W?@�q@��@�Ɇ@�}V@�:�@���@���@���@��@�d�@���@��M@�a@�X�@�N<@�+�@��@��@��H@��9@�Q�@�$@�	@��@���@�m]@��@��@�tT@��@���@��{@�IR@��c@��}@�~(@�H�@�$�@��@��)@��:@�4�@��1@�8�@��@��@���@��m@���@���@�Ov@�YK@��+@�	@��@��@���@��+@�C-@�e@�p�@��v@�7@�7@�x@��f@�X@��0@�`B@��@�h�@�#:@�H@�/�@���@��f@�e,@��@���@�V@�$�@�_@��$@�N<@�@�}V@��)@���@�s@�iD@�@@�M@�	�@� �@��@��z@�Mj@���@���@��@��]@��@�R�@�|�@���@�� @���@��A@�Xy@�g8@�oi@�ff@�Z@��W@�ԕ@��h@�>�@�&�@��@��@���@��E@��`@��@�g8@�%�@���@��@��@�`B@�Dg@�8@��@��@�C�@�b@��@��z@���@��f@�f�@�>�@�!-@��@��@��@���@�w�@�Z�@�%�@��@o�@8@~��@~�r@}�D@}\�@|[�@{��@{H�@z�@zq�@z.�@z�@y�@y�@y�@yY�@x�E@x��@xPH@xx@w�W@w�K@wO@v��@v�@u�@u�d@u��@u�M@u*0@t�Y@t%�@s>�@r��@r0U@q�@p��@p2�@o��@oO@n�"@nc @n�@mc@mV@l��@l��@lbN@l?�@l,=@k��@kE9@j� @j$�@iB�@i�@h�?@h�@h�/@h��@hy>@h<�@g�A@g��@g/�@g�@f�8@fOv@f#:@e�.@d�9@du�@c�@c��@cJ#@b�@b��@a��@a�@b{@a�3@a�-@a[W@`�P@`��@`�u@`l"@_�+@_�P@_;d@^�H@^�x@^��@^i�@^=q@^J@]��@]k�@\Ĝ@\z�@\I�@\7�@\/�@\�@[�@[S�@[(@Z�}@Z�r@Zxl@Z^5@Z;�@Z�@Y�@Y^�@Y�@X��@X�I@Wn/@W i@U��@T��@S�@SS�@S�@Rn�@Q��@Q}�@QN<@Q@Q�@Q�@Q;@P�f@P�@P�@O��@N��@N��@N0U@M�h@MVm@Ll"@K�@K�:@K��@KX�@K�@J��@Jn�@I��@IT�@I \@H�@H��@H7�@HS�@G�W@GS�@F��@FR�@F@Eԕ@E�@E4@E�@D��@D�o@De�@D7�@C��@C�k@C��@C��@C��@Cx@B�,@B8�@B4@A�Z@A��@A��@AY�@A+@@��@@~@?�$@?|�@?E9@>��@>!�@=x�@=0�@<�@<�$@<C-@<!@;��@;�@:�m@:�\@:H�@9ϫ@9�"@9j@9?}@8��@8$@7�}@7�k@7o�@7�@6�6@6@�@5�@5�N@5��@5T�@5�@4�|@4��@4�4@4��@4K^@4 �@3�@3��@3��@3�@3A�@2��@2�@2��@2V@2_@1e,@0�	@0�U@0/�@/��@/o�@/dZ@/H�@/�@.~�@-�@-��@-[W@,֡@,��@,��@,h�@,e�@,6@+��@+�F@+�F@+�k@+y�@+)_@*��@*R�@)�^@)7L@(��@(e�@'�&@'��@'��@'K�@&�]@&��@&��@&C�@%��@%�=@%`B@%�@$��@$�)@$M@#�;@#�{@#U�@#;d@#!-@"�8@"}V@"@!s�@!F@!�@ Ĝ@ [�@�@�a@��@|�@|�@~�@A�@$t@�@�H@�X@��@��@h
@3�@�@�@ԕ@�C@Y�@?}@/@��@ �@�@�F@dZ@�@�@҉@ȴ@�}@�1@�+@ff@	@�@�@��@��@c�@A @-w@�@Ĝ@�?@��@�e@��@�@oi@]d@4n@~@b@��@�K@�F@��@Z�@E9@�@��@��@�@v�@l�@GE@!�@�@u@�@��@�#@ԕ@|@(�@q@��@Ɇ@�e@�@��@l"@:�@�+@�@�$@o�@RT@4�@'�@C@Y@�@�@��@��@�o@�n@Y�@=�@#�@��@��@��@��@Xy@�@�g@e�@Mj@8@�y@��@��@i�@Ta@?@;�@=q@;�@:*@:*@8�@&�@_@_@��@��@|@e,@F@�?@��@�u@|�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  Aθ�AθAέAέ�AΣnAΕ�AΏ(AΒoA·�A�jA�h�A�\�A�L�A�3hA�/�A�-A�+A�*eA�)*A�'�A�%zA�$�A�%A�!�A��A�VA�!A��A�CA��A�YA��A��A�
rA���Aͷ�A�:*A�EmA�N<A�:*A�
�Aŏ�A�CaA��A�DgA�B�A��aA�kA��A��mA�(A�kA���A�m)A��-A�J�A�eA�)�A�=A�y�A�
rA��LA���A���A���A��9A�ffA�.�A��IA��/A�c�A�|PA���A��A�($A��A��*A��A���A��A�5tA��A�6�A�\�A�8RA�ԕA��A�pA�6�A���A��qA�<�A�E�A�o A��vA�r�A�C-A�5�A��A}��A{\�Ay��Av��AtArW?ApߤAn��Al=AhXAde�Ab��A`�8A^��A]�$A[�4AWh�ANr�AL��AHe�AE5�AC��AAL�A=��A=YA<$�A8T�A5ffA3�:A2�hA2/�A1��A0
�A-ffA)�A'��A'L�A'��A&خA$a�A"�{A!rGA �aA �.A rGA�A�Af�A�oA�AݘA�`A�A+kAi�A�6A��A<�Ac Av�A:�A7A!A�hA�UA)�A'RAA8�A�)Al"A�A'RAc�A�[Ad�A��A4nA�RAH�A�A�;A�<A�A�A%A�AA��A�A5?Al"A��A�eA�A�AS&A�dA��AGEA
�A	ѷA	�A	kQA�A|A�A�]A��AN<A�fAR�A�PA�A(A�hAc�A[�A��Ap;A ��A ��A _@�k�@���@���@�&�@�*�@��@�=�@��@�l�@�O@��.@�7�@�@��@�j�@��)@�Z�@�a|@�I�@��@�t�@��3@���@�2a@�@�GE@崢@��@�o @��"@�O@�R@�/�@�"@�2a@���@�E�@��@�*@���@�h@�[�@�`�@���@ަL@���@��@���@��@�~�@�($@�c@��@��@�~�@��@�d�@Ձ@���@�?@Ӵ�@�33@�q�@�	@Ѵ�@ч�@�j�@�B�@Р�@�-w@ά�@�h�@�K^@��
@�~�@��c@�%�@�  @��@�|�@�@�@�qv@��M@�0U@�n/@Ə\@��A@���@�_�@���@Ê	@��@��	@���@�u%@�Q�@��D@���@��z@��"@�E9@���@�M�@�˒@�t�@��@���@��@�1'@��z@�s�@���@�Y�@��I@��g@���@���@�7�@��@���@�{�@� �@��.@���@�v`@�T�@�"�@��@��@��@��Q@�@�Z�@��[@��O@�=q@�hs@�%F@���@���@�+k@���@���@��$@�0�@��m@��x@�:�@��W@��"@�&@��c@��j@�r�@��.@��3@���@�B�@���@�($@���@�a@�F�@���@���@���@�~�@�A�@��@��#@�J�@��@���@�-@�@�  @��@���@�C�@� �@���@��@��@�u%@�-@���@�o @���@�v�@�V@�@�ݘ@�o @�5�@���@�ѷ@��A@�@�ԕ@��*@�W?@�q@��@�Ɇ@�}V@�:�@���@���@���@��@�d�@���@��M@�a@�X�@�N<@�+�@��@��@��H@��9@�Q�@�$@�	@��@���@�m]@��@��@�tT@��@���@��{@�IR@��c@��}@�~(@�H�@�$�@��@��)@��:@�4�@��1@�8�@��@��@���@��m@���@���@�Ov@�YK@��+@�	@��@��@���@��+@�C-@�e@�p�@��v@�7@�7@�x@��f@�X@��0@�`B@��@�h�@�#:@�H@�/�@���@��f@�e,@��@���@�V@�$�@�_@��$@�N<@�@�}V@��)@���@�s@�iD@�@@�M@�	�@� �@��@��z@�Mj@���@���@��@��]@��@�R�@�|�@���@�� @���@��A@�Xy@�g8@�oi@�ff@�Z@��W@�ԕ@��h@�>�@�&�@��@��@���@��E@��`@��@�g8@�%�@���@��@��@�`B@�Dg@�8@��@��@�C�@�b@��@��z@���@��f@�f�@�>�@�!-@��@��@��@���@�w�@�Z�@�%�@��@o�@8@~��@~�r@}�D@}\�@|[�@{��@{H�@z�@zq�@z.�@z�@y�@y�@y�@yY�@x�E@x��@xPH@xx@w�W@w�K@wO@v��@v�@u�@u�d@u��@u�M@u*0@t�Y@t%�@s>�@r��@r0U@q�@p��@p2�@o��@oO@n�"@nc @n�@mc@mV@l��@l��@lbN@l?�@l,=@k��@kE9@j� @j$�@iB�@i�@h�?@h�@h�/@h��@hy>@h<�@g�A@g��@g/�@g�@f�8@fOv@f#:@e�.@d�9@du�@c�@c��@cJ#@b�@b��@a��@a�@b{@a�3@a�-@a[W@`�P@`��@`�u@`l"@_�+@_�P@_;d@^�H@^�x@^��@^i�@^=q@^J@]��@]k�@\Ĝ@\z�@\I�@\7�@\/�@\�@[�@[S�@[(@Z�}@Z�r@Zxl@Z^5@Z;�@Z�@Y�@Y^�@Y�@X��@X�I@Wn/@W i@U��@T��@S�@SS�@S�@Rn�@Q��@Q}�@QN<@Q@Q�@Q�@Q;@P�f@P�@P�@O��@N��@N��@N0U@M�h@MVm@Ll"@K�@K�:@K��@KX�@K�@J��@Jn�@I��@IT�@I \@H�@H��@H7�@HS�@G�W@GS�@F��@FR�@F@Eԕ@E�@E4@E�@D��@D�o@De�@D7�@C��@C�k@C��@C��@C��@Cx@B�,@B8�@B4@A�Z@A��@A��@AY�@A+@@��@@~@?�$@?|�@?E9@>��@>!�@=x�@=0�@<�@<�$@<C-@<!@;��@;�@:�m@:�\@:H�@9ϫ@9�"@9j@9?}@8��@8$@7�}@7�k@7o�@7�@6�6@6@�@5�@5�N@5��@5T�@5�@4�|@4��@4�4@4��@4K^@4 �@3�@3��@3��@3�@3A�@2��@2�@2��@2V@2_@1e,@0�	@0�U@0/�@/��@/o�@/dZ@/H�@/�@.~�@-�@-��@-[W@,֡@,��@,��@,h�@,e�@,6@+��@+�F@+�F@+�k@+y�@+)_@*��@*R�@)�^@)7L@(��@(e�@'�&@'��@'��@'K�@&�]@&��@&��@&C�@%��@%�=@%`B@%�@$��@$�)@$M@#�;@#�{@#U�@#;d@#!-@"�8@"}V@"@!s�@!F@!�@ Ĝ@ [�@�@�a@��@|�@|�@~�@A�@$t@�@�H@�X@��@��@h
@3�@�@�@ԕ@�C@Y�@?}@/@��@ �@�@�F@dZ@�@�@҉@ȴ@�}@�1@�+@ff@	@�@�@��@��@c�@A @-w@�@Ĝ@�?@��@�e@��@�@oi@]d@4n@~@b@��@�K@�F@��@Z�@E9@�@��@��@�@v�@l�@GE@!�@�@u@�@��@�#@ԕ@|@(�@q@��@Ɇ@�e@�@��@l"@:�@�+@�@�$@o�@RT@4�@'�@C@Y@�@�@��@��@�o@�n@Y�@=�@#�@��@��@��@��@Xy@�@�g@e�@Mj@8@�y@��@��@i�@Ta@?@;�@=q@;�@:*@:*@8�@&�@_@_@��@��@|@e,@F@�?@��@�u@|�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
��B
��B
��B
�XB
�#B
�lB
�lB
�7B
��B
��B
��B
��B
�fB
��B
��B
��B
��B
��B
��B
�zB
��B
��B
�zB
�zB
��B
�zB
�zB
�zB
�zB
�+B
�B
�EB
�_B
��B
��B
�dB
��B
��B
��BKBuB
��B
�`B
��B
�KB�B�B�Bq'B��B��B��B�`B�B��B��B��B�YB�mBƎB��B��B�B�B�B�B��B�dBбB�	B�=B��B��B�'B��B��B~Br�BvBd�BU�BN"BDMB=�B<�B;�B(
B�B
��B
��B
� B
��B
��B
��B
o5B
Y�B
=B
)�B
$tB
=B
(B
B	�`B	�B	��B	�YB	�=B	��B	�B	��B	�RB	��B	��B	z�B	oB	`�B	OB	A�B	?�B	6�B	4nB	.B	xB	�B	�B	 �B�nB��B�CB�kB��B�B�)B��B�B��BȀB�RB��B�iB�B�hB�%B�+B�B�1B�vB�uB�B�B��B	�B	�B	!|B	8�B	K^B	8�B	�B	.B	�B	�B	�B	!�B	2aB	K^B	o�B	utB	|PB	�4B	��B	� B	�MB	��B	�'B	B	}VB	z^B	yrB	yXB	y�B	z�B	z�B	|PB	�~B	�NB	��B	��B	�B	��B	�HB	��B	�;B	�vB	��B	�sB	��B	��B	�=B	�WB	��B	�5B	�/B	�B	� B	��B	�IB	�B	��B	��B	��B	��B	��B	��B	�3B	��B	��B	��B	�]B	��B	��B	�[B	�B	�hB	�-B	��B	�wB	�)B	�CB	�WB	��B	�B	��B	��B	��B	�$B	��B	��B	�}B	�>B	�-B	�qB	�>B	�NB	��B	�5B	�B	�`B	��B	��B	�OB	��B	�CB	�B	�,B	�LB	�yB	��B	��B	��B	�B	ȴB	�~B	�B	�B	�-B	�B	��B	�	B	��B	�xB	�CB	��B	�)B	ݘB	ݘB	��B	�B	�~B	�jB	��B	�\B	�HB	�B	��B	�B	� B	�TB	�B	�TB	��B	�B	�`B	�,B	��B	�B	��B	��B	�B	�B	�_B	�*B	��B	�kB	�WB	��B	�]B	�wB	�]B	�B	�/B	�B	�B	��B	��B	��B	�0B	�B	�B	��B	�B	�cB	�B	�B	�B	�B	�'B	�B	�B	�B	�B	��B	��B	�MB	��B	�fB	�`B	�?B	�B	�wB	�qB	��B	�B
  B
 �B
;B
[B
B
GB
�B
�B
B
�B
B
%B
B
1B
B
	B

�B
)B
)B
�B
0B
dB
�B
~B
PB
�B
B
�B
�B
\B
�B
�B
�B
�B
�B
�B
�B
bB
�B
NB
�B
�B
�B
[B
�B
FB
�B
�B
�B
�B
�B
�B
�B
�B
�B
mB
SB
�B
�B
YB
�B
_B
+B
1B
7B
]B
)B
�B
�B
�B
OB
B
B
B
dB
IB
�B
~B
~B
�B
B
�B
�B
IB
�B
~B
IB
xB
�B
CB
�B
)B
�B
IB
�B
B
!B
!HB
!HB
!B
!|B
!�B
"hB
"�B
"�B
# B
#�B
$B
$@B
$&B
#�B
#nB
#TB
#nB
$tB
%`B
%�B
%�B
%�B
%`B
%`B
%FB
%FB
%�B
&�B
(
B
-�B
7B
7�B
7�B
6�B
6zB
7�B
5�B
6�B
8�B
5�B
6zB
5�B
7B
88B
8RB
7fB
6zB
6�B
8�B
8�B
9XB
<�B
=qB
<�B
;�B
:�B
;dB
;�B
;�B
;JB
;�B
<jB
<�B
;dB
:xB
:B
9�B
:B
:^B
:B
8�B
8�B
9>B
9�B
9$B
9$B
9�B
9�B
9�B
9�B
:^B
:�B
;JB
;�B
=qB
=�B
>BB
AB
B�B
CB
DMB
D�B
EB
EmB
E�B
FtB
G�B
I�B
J	B
I7B
HKB
H�B
IRB
J	B
J�B
K�B
LJB
LJB
L�B
MB
M6B
L�B
MB
L�B
L�B
L�B
MB
L�B
M�B
M�B
M�B
NB
N"B
NVB
N�B
N�B
N�B
NpB
N<B
NVB
NVB
N<B
N<B
N"B
N"B
M�B
M�B
M�B
M�B
NB
N"B
N�B
N�B
O(B
OBB
O�B
O�B
OvB
O�B
O\B
OvB
OvB
O�B
O�B
PB
PB
PB
O�B
PB
PB
PHB
PHB
P.B
P.B
PHB
PHB
P�B
Q B
QhB
R B
R:B
SB
S�B
S�B
TFB
TaB
T�B
UB
UMB
U�B
VB
VSB
V�B
V�B
W�B
XB
W�B
W�B
X+B
X_B
X�B
YeB
Z7B
[=B
[=B
[�B
\�B
]IB
]IB
]/B
\�B
\�B
\�B
\�B
]dB
]�B
]�B
]IB
]�B
^B
^5B
^�B
^�B
_B
_�B
`vB
bB
c�B
eB
e�B
fLB
fLB
f�B
f�B
f�B
gB
g�B
g�B
g�B
g�B
g�B
gB
gB
g8B
g�B
i_B
i*B
h�B
iB
iyB
i�B
jKB
jB
kB
k6B
kB
kB
j�B
j�B
jeB
jKB
k6B
k�B
kB
i�B
h�B
g8B
g8B
g�B
gB
f�B
f�B
f�B
f�B
f�B
gB
gmB
g�B
g�B
hsB
kB
k�B
l"B
k�B
kQB
kkB
k�B
k�B
kkB
j�B
j�B
j�B
j�B
j�B
j�B
j�B
k�B
l"B
l=B
l=B
lqB
m)B
nIB
o B
o5B
o5B
n�B
n}B
ncB
ncB
n�B
n�B
oiB
o5B
o B
oB
pB
q�B
raB
raB
raB
r|B
sMB
s�B
s�B
s�B
s�B
s�B
tB
t�B
u?B
v�B
v`B
vzB
vFB
u�B
u�B
u�B
u�B
u�B
vB
v`B
v�B
v�B
v�B
v�B
v�B
v�B
wLB
w�B
w�B
w�B
xB
xRB
x�B
x�B
y	B
y�B
y�B
zB
zB
z*B
z�B
z�B
z�B
{B
{0B
{0B
{JB
{�B
{�B
|B
|B
|6B
|PB
|�B
|�B
|�B
|�B
}"B
}B
}�B
}�B
~(B
~�B
}B
}B
}B
}B
}B
�B
��B
��B
��B
�;B
�UB
��B
��B
��B
��B
�AB
�[B
��B
��B
�B
�GB
�-B
�{B
�B
�3B
�MB
��B
��B
��B
�SB
��B
��B
��B
��B
��B
�?B
�tB
��B
�+B
��B
��B
��B
��B
��B
��B
�B
�B
�B
�#B
�rB
��B
��B
��B
��B
��B
�0B
�dB
�~B
��B
��B
�B
��B
��B
��B
��B
��B
�"B
�<B
��B
�BB
��B
��B
��B
��B
��B
�.B
�HB
��B
�}B
�bB
��B
�B
�hB
��B
��B
��B
��B
��B
��B
��B
�B
�:B
��B
��B
��B
�&B
�[B
��B
��B
�,B
�FB
�aB
�aB
�aB
�{B
�{B
��B
��B
��B
��B
��B
�2B
�2B
��B
��B
��B
��B
�B
�B
�9B
�mB
�mB
��B
��B
��B
��B
��B
��B
��B
��B
�YB
�sB
��B
��B
��B
��B
��B
��B
�B
�EB
��B
��B
��B
��B
�B
�1B
�KB
�KB
�KB
�KB
�eB
�eB
�eB
�kB
�kB
��B
��B
��B
�	B
�=B
�=B
�WB
��B
��B
��B
�xB
�]B
�]B
��B
��B
�/B
�IB
�~B
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
�B
��B
��B
��B
��B
��B
��B
��B
��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
��B
��B
��B
�XB
�#B
�lB
�lB
�7B
��B
��B
��B
��B
�fB
��B
��B
��B
��B
��B
��B
�zB
��B
��B
�zB
�zB
��B
�zB
�zB
�zB
�zB
�+B
�B
�EB
�_B
��B
��B
�dB
��B
��B
��BKBuB
��B
�`B
��B
�KB�B�B�Bq'B��B��B��B�`B�B��B��B��B�YB�mBƎB��B��B�B�B�B�B��B�dBбB�	B�=B��B��B�'B��B��B~Br�BvBd�BU�BN"BDMB=�B<�B;�B(
B�B
��B
��B
� B
��B
��B
��B
o5B
Y�B
=B
)�B
$tB
=B
(B
B	�`B	�B	��B	�YB	�=B	��B	�B	��B	�RB	��B	��B	z�B	oB	`�B	OB	A�B	?�B	6�B	4nB	.B	xB	�B	�B	 �B�nB��B�CB�kB��B�B�)B��B�B��BȀB�RB��B�iB�B�hB�%B�+B�B�1B�vB�uB�B�B��B	�B	�B	!|B	8�B	K^B	8�B	�B	.B	�B	�B	�B	!�B	2aB	K^B	o�B	utB	|PB	�4B	��B	� B	�MB	��B	�'B	B	}VB	z^B	yrB	yXB	y�B	z�B	z�B	|PB	�~B	�NB	��B	��B	�B	��B	�HB	��B	�;B	�vB	��B	�sB	��B	��B	�=B	�WB	��B	�5B	�/B	�B	� B	��B	�IB	�B	��B	��B	��B	��B	��B	��B	�3B	��B	��B	��B	�]B	��B	��B	�[B	�B	�hB	�-B	��B	�wB	�)B	�CB	�WB	��B	�B	��B	��B	��B	�$B	��B	��B	�}B	�>B	�-B	�qB	�>B	�NB	��B	�5B	�B	�`B	��B	��B	�OB	��B	�CB	�B	�,B	�LB	�yB	��B	��B	��B	�B	ȴB	�~B	�B	�B	�-B	�B	��B	�	B	��B	�xB	�CB	��B	�)B	ݘB	ݘB	��B	�B	�~B	�jB	��B	�\B	�HB	�B	��B	�B	� B	�TB	�B	�TB	��B	�B	�`B	�,B	��B	�B	��B	��B	�B	�B	�_B	�*B	��B	�kB	�WB	��B	�]B	�wB	�]B	�B	�/B	�B	�B	��B	��B	��B	�0B	�B	�B	��B	�B	�cB	�B	�B	�B	�B	�'B	�B	�B	�B	�B	��B	��B	�MB	��B	�fB	�`B	�?B	�B	�wB	�qB	��B	�B
  B
 �B
;B
[B
B
GB
�B
�B
B
�B
B
%B
B
1B
B
	B

�B
)B
)B
�B
0B
dB
�B
~B
PB
�B
B
�B
�B
\B
�B
�B
�B
�B
�B
�B
�B
bB
�B
NB
�B
�B
�B
[B
�B
FB
�B
�B
�B
�B
�B
�B
�B
�B
�B
mB
SB
�B
�B
YB
�B
_B
+B
1B
7B
]B
)B
�B
�B
�B
OB
B
B
B
dB
IB
�B
~B
~B
�B
B
�B
�B
IB
�B
~B
IB
xB
�B
CB
�B
)B
�B
IB
�B
B
!B
!HB
!HB
!B
!|B
!�B
"hB
"�B
"�B
# B
#�B
$B
$@B
$&B
#�B
#nB
#TB
#nB
$tB
%`B
%�B
%�B
%�B
%`B
%`B
%FB
%FB
%�B
&�B
(
B
-�B
7B
7�B
7�B
6�B
6zB
7�B
5�B
6�B
8�B
5�B
6zB
5�B
7B
88B
8RB
7fB
6zB
6�B
8�B
8�B
9XB
<�B
=qB
<�B
;�B
:�B
;dB
;�B
;�B
;JB
;�B
<jB
<�B
;dB
:xB
:B
9�B
:B
:^B
:B
8�B
8�B
9>B
9�B
9$B
9$B
9�B
9�B
9�B
9�B
:^B
:�B
;JB
;�B
=qB
=�B
>BB
AB
B�B
CB
DMB
D�B
EB
EmB
E�B
FtB
G�B
I�B
J	B
I7B
HKB
H�B
IRB
J	B
J�B
K�B
LJB
LJB
L�B
MB
M6B
L�B
MB
L�B
L�B
L�B
MB
L�B
M�B
M�B
M�B
NB
N"B
NVB
N�B
N�B
N�B
NpB
N<B
NVB
NVB
N<B
N<B
N"B
N"B
M�B
M�B
M�B
M�B
NB
N"B
N�B
N�B
O(B
OBB
O�B
O�B
OvB
O�B
O\B
OvB
OvB
O�B
O�B
PB
PB
PB
O�B
PB
PB
PHB
PHB
P.B
P.B
PHB
PHB
P�B
Q B
QhB
R B
R:B
SB
S�B
S�B
TFB
TaB
T�B
UB
UMB
U�B
VB
VSB
V�B
V�B
W�B
XB
W�B
W�B
X+B
X_B
X�B
YeB
Z7B
[=B
[=B
[�B
\�B
]IB
]IB
]/B
\�B
\�B
\�B
\�B
]dB
]�B
]�B
]IB
]�B
^B
^5B
^�B
^�B
_B
_�B
`vB
bB
c�B
eB
e�B
fLB
fLB
f�B
f�B
f�B
gB
g�B
g�B
g�B
g�B
g�B
gB
gB
g8B
g�B
i_B
i*B
h�B
iB
iyB
i�B
jKB
jB
kB
k6B
kB
kB
j�B
j�B
jeB
jKB
k6B
k�B
kB
i�B
h�B
g8B
g8B
g�B
gB
f�B
f�B
f�B
f�B
f�B
gB
gmB
g�B
g�B
hsB
kB
k�B
l"B
k�B
kQB
kkB
k�B
k�B
kkB
j�B
j�B
j�B
j�B
j�B
j�B
j�B
k�B
l"B
l=B
l=B
lqB
m)B
nIB
o B
o5B
o5B
n�B
n}B
ncB
ncB
n�B
n�B
oiB
o5B
o B
oB
pB
q�B
raB
raB
raB
r|B
sMB
s�B
s�B
s�B
s�B
s�B
tB
t�B
u?B
v�B
v`B
vzB
vFB
u�B
u�B
u�B
u�B
u�B
vB
v`B
v�B
v�B
v�B
v�B
v�B
v�B
wLB
w�B
w�B
w�B
xB
xRB
x�B
x�B
y	B
y�B
y�B
zB
zB
z*B
z�B
z�B
z�B
{B
{0B
{0B
{JB
{�B
{�B
|B
|B
|6B
|PB
|�B
|�B
|�B
|�B
}"B
}B
}�B
}�B
~(B
~�B
}B
}B
}B
}B
}B
�B
��B
��B
��B
�;B
�UB
��B
��B
��B
��B
�AB
�[B
��B
��B
�B
�GB
�-B
�{B
�B
�3B
�MB
��B
��B
��B
�SB
��B
��B
��B
��B
��B
�?B
�tB
��B
�+B
��B
��B
��B
��B
��B
��B
�B
�B
�B
�#B
�rB
��B
��B
��B
��B
��B
�0B
�dB
�~B
��B
��B
�B
��B
��B
��B
��B
��B
�"B
�<B
��B
�BB
��B
��B
��B
��B
��B
�.B
�HB
��B
�}B
�bB
��B
�B
�hB
��B
��B
��B
��B
��B
��B
��B
�B
�:B
��B
��B
��B
�&B
�[B
��B
��B
�,B
�FB
�aB
�aB
�aB
�{B
�{B
��B
��B
��B
��B
��B
�2B
�2B
��B
��B
��B
��B
�B
�B
�9B
�mB
�mB
��B
��B
��B
��B
��B
��B
��B
��B
�YB
�sB
��B
��B
��B
��B
��B
��B
�B
�EB
��B
��B
��B
��B
�B
�1B
�KB
�KB
�KB
�KB
�eB
�eB
�eB
�kB
�kB
��B
��B
��B
�	B
�=B
�=B
�WB
��B
��B
��B
�xB
�]B
�]B
��B
��B
�/B
�IB
�~B
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
�B
��B
��B
��B
��B
��B
��B
��B
��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20230613064855  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8b                                                                20230613064902  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20230613064902  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20230613064903                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20230613064903  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20230613064903  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20230613073048                      G�O�G�O�G�O�                