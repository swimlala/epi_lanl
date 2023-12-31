CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:24:15Z creation;2022-06-04T19:24:15Z conversion to V3.1      
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
_FillValue                 �  I<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M,   PRES_ADJUSTED_QC         
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
_FillValue                 �  �@   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �0   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
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
resolution        :�o     �  �D   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ޼   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �    HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �$   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �(   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �h   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �x   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �|   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604192415  20220610161505  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               NA   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @�i~���1   @�iDt��@-���vȴ�c�$�/1   GPS     A   B   B   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @333@�  @�  A   AffA@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  BpffBw33B�ffB���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�ffB�33B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C�C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C+�fC.  C0  C2  C4  C6  C833C:33C;�3C>  C@  CB  CD�CF  CH  CJ  CK�fCN  CP  CR  CT  CV  CX�CZ�C\  C^  C`  Cb  Cd  Cf�Ch�Cj  Ck�fCn  Cp  Cq�fCt  Cv  Cx  Cz  C|  C~  C�fC�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D���D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @/\)@|(�@�{@�{Ap�A?
=A_
=A
=A��A��A��A��AυA߅A�A��BBBBB'B/B7B?BGBOBWB_BgBp(�Bv��B�G�B�z�B��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB�G�B�{BӮB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HC�C�C�C�C	�C
>C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�
C-�C/�C1�C3�C5�C8#�C:#�C;��C=�C?�CA�CD
>CE�CG�CI�CK�
CM�CO�CQ�CS�CU�CX
>CZ
>C[�C]�C_�Ca�Cc�Cf
>Ch
>Ci�Ck�
Cm�Co�Cq�
Cs�Cu�Cw�Cy�C{�C}�C�
C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC�C�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RD |)D �)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D	|)D	�)D
|)D
�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D |)D �)D!|)D!�)D"|)D"�)D#|)D#�)D$|)D$�)D%|)D%�)D&|)D&�)D'|)D'�)D(|)D(�)D)|)D)�)D*|)D*�)D+|)D+�)D,|)D,�)D-|)D-�)D.|)D.�)D/|)D/�)D0|)D0�)D1|)D1�)D2|)D2�)D3|)D3�)D4|)D4�)D5|)D5�)D6|)D6�)D7|)D7�)D8|)D8�)D9|)D9�)D:|)D:�)D;|)D;�)D<|)D<�)D=|)D=�)D>|)D>�)D?|)D?�)D@|)D@�)DA|)DA�)DB|)DB�)DC|)DC�)DD|)DD�)DE|)DE�)DF|)DF�)DG|)DG�)DH|)DH�)DI|)DI�)DJ|)DJ�)DK|)DK�)DL|)DL�)DM|)DM�)DN|)DN�)DO|)DO�)DP|)DP�)DQ|)DQ�)DR|)DR�)DS|)DS�)DT|)DT�)DU|)DU�)DV|)DV�)DW|)DW�)DX|)DX�)DY|)DY�)DZ|)DZ�)D[|)D[�)D\|)D\�)D]|)D]�)D^|)D^�)D_|)D_�)D`|)D`�)Da|)Da�)Db|)Db�)Dc|)Dc�)Dd|)Dd�)De|)De�)Df|)Df�)Dg|)Dg�)Dh|)Dh�)Di|)Di�)Dj|)Dj�)Dk|)Dk�)Dl|)Dl�)Dm|)Dm�)Dn|)Dn�)Do|)Do�)Dp|)Dp�)Dq|)Dq�)Dr|)Dr�)Ds|)Ds�)Dt|)Dt�)Du|)Du�)Dv|)Dv�)Dw|)Dw�)Dx|)Dx�)Dy|)Dy�)Dz|)Dz�)D{|)D{�)D||)D|�)D}|)D}�)D~|)D~�)D|)D�)D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�z�D���D��D�>D�~D���D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�AGD�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D¾D��D�>D�~DþD��D�>D�~DľD��D�>D�~DžD��D�>D�~DƾD��D�>D�~DǾD��D�>D�~DȾD��D�>D�~DɾD��D�>D�~DʾD��D�>D�~D˾D��D�>D�~D̾D��D�>D�~D;D��D�>D�~DξD��D�>D�~DϾD��D�>D�~DоD��D�>D�~DѾD��D�>D�~DҾD��D�>D�~DӾD��D�>D�~DԾD��D�>D�~DվD��D�>D�~D־D��D�>D�~D׾D��D�>D�~DؾD��D�>D�~DپD��D�>D�~DھD��D�>D�~D۾D��D�>D�~DܾD��D�>D�~DݾD��D�>D�~D޾D��D�>D�~D߾D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D��D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��G11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�t�A�|�A�p;A�m�A�s�A�)�A� �A��A�uA���A��A���A��[Aɹ�Aɷ�AɷAɴ�Aɲ�Aɰ�AɯOAɭ�AɭwAɮ�Aɮ}Aɭ�AɭAɫ�Aɪ�AɥzA��A�<�A�t�A�`A��DA���Aõ?A��A��ZAùXA�~�A��gA��A��-A�C�A��A��RA���A�0�A��_A���A���A�=�A��SA��YA�@A���A���A��yA�~�A��QA�xA�^5A���A�3�A��A��IA�A A�f2A���A��RA��]A�k�A��6A���A���A�NpA�YA�:*A�hsA�k�A�j�A�2�A��A��A�"hA��ZA��A�IRA�T�A���A��}A�1�A��dA�HA�u�A�t�Ax��Aqn�Am��Af��Ab�,A_p�AZ�oAY�_AW�AAT,=APD�AN �AMg8AM�AL�,AL�9ALU2AL�AJQ�AH`BAB��A>�zA; \A8��A8:�A6��A6��A7V�A7�A5J�A5;�A5��A5PHA4�A4�A2�A2W�A2�A1��A0��A0��A/��A.��A.&�A-��A-�A-�A,��A,T�A+�zA+�A*��A*QA)y>A(�A(xlA'��A'��A'��A&�A&��A&oiA&n/A&
�A%C-A%�A$Z�A#�CA#G�A#^5A#�A"?A!��A ��A��A7�A��A~�A��A��AA�AA��A�KA�MA�AA+�A�A�A�TA��AGA~A�YA�A�zA��Ah�A�A��A�{A[�A��A�Au%A`�A`BA�A�OA4A��AxA.IA�AA�A��AYA�mA��A�SA��A�+A�A�A�_A�AZ�A%A
��A
��A
JA	�nA	&Ad�A�A�A��A[WA�FA��ADgA�AA��AA�XAy>AVA ��A ��A �[A jA B�@���@��@��[@��x@�V@��@���@�)_@���@���@���@���@�\�@�)_@��@��s@�<�@��q@���@�3�@�[W@��}@�!@�]�@��@���@�@�+k@�$t@�&�@�X@�RT@�Ĝ@��+@�J#@�҉@�Q�@�m]@��M@�.�@�f@���@�|@�S@���@�!@�U2@�1@�Z�@�a|@�e@�7@�e@�M@� i@�PH@��@ݘ�@���@���@�.I@ڵ@�e�@�R�@�ƨ@�'�@��m@���@ׂ�@�P�@��@���@�Ov@�
�@�M@�1'@��j@ըX@�S�@�ی@��9@��M@ҝI@�*�@ъ	@���@�_@�?}@��"@ίO@�($@��	@̨�@��@�Ov@� �@��d@��s@���@�B�@ȣ@�+k@Ǯ@��@�ȴ@ƫ6@ƝI@�YK@��]@�c�@�4�@��@���@Ď�@�#:@��z@íC@Î�@�:�@�M@�c@�RT@�*0@���@���@���@�Q@�k�@���@�@�@�(�@��@��w@�/�@��f@��[@��r@��@��@��@���@�d�@�)�@���@���@��:@�|@���@�l"@���@�a�@�kQ@�*�@�e@�	@��d@�m]@�j@�X�@�/�@�	l@��@�u%@�=q@��F@��@�o@�@�҉@��@�N�@���@���@�2a@���@�C-@�!�@��+@���@�v`@�-w@���@��$@���@�� @�J@��$@�5�@��|@���@���@�GE@��@�˒@�|�@�B�@��@��R@�tT@�@��@�]�@�7L@���@���@�~@�@�@��T@��f@�&@���@�4n@���@�t�@�33@��y@��@�a|@��@��n@��	@�a�@��p@�@���@�o�@��@��z@�q@�A�@��#@���@��M@�L�@��@��m@�|�@�!@��d@��4@�Dg@�-w@��p@�{�@���@���@��S@�J�@��@��)@���@�v�@�M�@�#:@��m@��^@�n/@�@��j@�tT@�Q�@�<�@�J@��@��@�zx@�s�@�A�@��@��H@���@�r�@��@���@��n@�|@�P�@���@���@��@�c�@�@�@��@��*@�0�@��@��@���@��@�%�@��@��C@���@�e,@��@���@��v@��E@�c�@�=q@��@���@�L�@�@��E@�z@�L0@���@�H�@��]@�Xy@��W@�]�@��@�Ɇ@��D@�c @�M@�3�@��A@�o @��|@��,@��1@�M@���@���@��	@�\�@�2a@��@��p@�oi@���@���@�Q�@��@��c@�Ĝ@��@���@�[�@�Ft@�J@���@�zx@��@�u%@�I�@�1�@�$@�	@��H@�zx@�@���@��x@���@�'R@��@@O@~�!@~@}��@}j@} \@|��@|�)@|]d@{�@{y�@z�c@z�@z;�@yT�@x�?@x��@xl"@xK^@x-�@w��@wخ@w]�@v��@v;�@u��@u��@u�7@uV@t��@t]d@t%�@s�W@s�0@s@O@r�s@rz@r-@q��@q��@q��@qo @q�@p��@p:�@o��@o@O@n��@m�h@l�@l�Y@l�@k��@kX�@j�m@j)�@j	@i�Z@i�9@i��@i|@iIR@i�@h7�@g��@gF�@g�@f�@fe@e��@ex�@e�@d��@dV�@c��@cJ#@b��@b�+@bq�@bd�@bW�@a�@a&�@`N�@_�]@_��@_�w@_�4@^�,@^��@^��@^a|@^&�@]�>@]�X@]Dg@\�_@[��@[�K@[��@[�f@[,�@Z��@Z�\@Zp;@Z-@Y�9@Y��@X�[@X<�@Wƨ@W�{@W8@V�8@V�'@Vu%@VB[@V�@V_@U��@U��@Uc�@U4@T��@TA�@S��@S�P@Sx@SX�@S(@R�@R3�@Q�9@Q�C@Q�@Qq@P�u@P-�@O�@O�@@O�@N�H@Nȴ@N~�@NR�@N{@Mԕ@M��@M��@M�7@M*0@L��@Ly>@LXy@LQ�@K�@K�P@J�s@J@I��@Ip�@IB�@H�@H��@H��@Hm�@H9X@H�@G�
@G�P@GC@F�@Fp;@F	@E�@E�'@EIR@D�_@DM@DM@C�m@C�@@C�P@C,�@B��@B�@B�r@B#:@A�o@A�j@A�X@Ae,@A	l@@��@@��@@~(@@M@@/�@@7@?��@?�@?&@>�H@>}V@=��@=�H@=��@=Dg@<ی@<�@;�@;��@;v`@;,�@:��@:c @9�o@9�~@9e,@9<6@9@8��@8�@8��@8z�@8K^@7��@7Z�@7�@6��@6a|@6#:@6 �@5�@5`B@5�@4��@4|�@3�@3�q@2�2@2}V@2)�@1��@14@0��@0z�@0Ft@0 �@/�m@/��@/33@.ں@.�r@.ff@.@-�#@-�@-��@-*0@,��@,�u@,%�@+�@+�$@+&@*��@*��@*p;@*C�@*�@)�d@)s�@)+�@(�)@(�@(,=@'�*@'RT@&�c@&��@&L0@&�@%�@%�@%��@%x�@%Vm@$�@$��@$�D@$�@$q@$S�@#�+@#خ@#��@#�@#|�@#Mj@#(@"�@"��@"Q@"5?@"e@!�@!��@!�h@!8�@!�@ �e@ x@�q@�@b�@P�@��@q�@i�@C�@��@�o@[W@ѷ@��@�@�.@tT@l"@D�@1@  @��@�@�a@�f@_p@.I@�6@a|@B[@�@��@��@��@�@c�@+�@�@Ĝ@$@˒@��@dZ@8@�@ȴ@Q@&�@�@��@��@�t@��@��@J�@	l@�@��@�_@V�@C-@�@�g@�[@�@�:@iD@��@�b@Ov@�@�@�@��@��@��@S&@�@�U@��@C-@�@�0@��@U�11111111111111111111111111111111111111114411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�t�A�|�A�p;A�m�A�s�A�)�A� �A��A�uA���A��A���A��[Aɹ�Aɷ�AɷAɴ�Aɲ�Aɰ�AɯOAɭ�AɭwAɮ�Aɮ}Aɭ�AɭAɫ�Aɪ�AɥzA��A�<�A�t�A�`A��DA���Aõ?A��A��ZAùXA�~�A��gA��A��-A�C�A��A��RA���A�0�A��_A���A���A�=�A��SA��YA�@A���A���A��yA�~�A��QA�xA�^5A���A�3�A��A��IA�A A�f2A���A��RA��]A�k�A��6A���A���A�NpA�YA�:*A�hsA�k�A�j�A�2�A��A��A�"hA��ZA��A�IRA�T�A���A��}A�1�A��dA�HA�u�A�t�Ax��Aqn�Am��Af��Ab�,A_p�AZ�oAY�_AW�AAT,=APD�AN �AMg8AM�AL�,AL�9ALU2AL�AJQ�AH`BAB��A>�zA; \A8��A8:�A6��A6��A7V�A7�A5J�A5;�A5��A5PHA4�A4�A2�A2W�A2�A1��A0��A0��A/��A.��A.&�A-��A-�A-�A,��A,T�A+�zA+�A*��A*QA)y>A(�A(xlA'��A'��A'��A&�A&��A&oiA&n/A&
�A%C-A%�A$Z�A#�CA#G�A#^5A#�A"?A!��A ��A��A7�A��A~�A��A��AA�AA��A�KA�MA�AA+�A�A�A�TA��AGA~A�YA�A�zA��Ah�A�A��A�{A[�A��A�Au%A`�A`BA�A�OA4A��AxA.IA�AA�A��AYA�mA��A�SA��A�+A�A�A�_A�AZ�A%A
��A
��A
JA	�nA	&Ad�A�A�A��A[WA�FA��ADgA�AA��AA�XAy>AVA ��A ��A �[A jA B�@���@��@��[@��x@�V@��@���@�)_@���@���@���@���@�\�@�)_@��@��s@�<�@��q@���@�3�@�[W@��}@�!@�]�@��@���@�@�+k@�$t@�&�@�X@�RT@�Ĝ@��+@�J#@�҉@�Q�@�m]@��M@�.�@�f@���@�|@�S@���@�!@�U2@�1@�Z�@�a|@�e@�7@�e@�M@� i@�PH@��@ݘ�@���@���@�.I@ڵ@�e�@�R�@�ƨ@�'�@��m@���@ׂ�@�P�@��@���@�Ov@�
�@�M@�1'@��j@ըX@�S�@�ی@��9@��M@ҝI@�*�@ъ	@���@�_@�?}@��"@ίO@�($@��	@̨�@��@�Ov@� �@��d@��s@���@�B�@ȣ@�+k@Ǯ@��@�ȴ@ƫ6@ƝI@�YK@��]@�c�@�4�@��@���@Ď�@�#:@��z@íC@Î�@�:�@�M@�c@�RT@�*0@���@���@���@�Q@�k�@���@�@�@�(�@��@��w@�/�@��f@��[@��r@��@��@��@���@�d�@�)�@���@���@��:@�|@���@�l"@���@�a�@�kQ@�*�@�e@�	@��d@�m]@�j@�X�@�/�@�	l@��@�u%@�=q@��F@��@�o@�@�҉@��@�N�@���@���@�2a@���@�C-@�!�@��+@���@�v`@�-w@���@��$@���@�� @�J@��$@�5�@��|@���@���@�GE@��@�˒@�|�@�B�@��@��R@�tT@�@��@�]�@�7L@���@���@�~@�@�@��T@��f@�&@���@�4n@���@�t�@�33@��y@��@�a|@��@��n@��	@�a�@��p@�@���@�o�@��@��z@�q@�A�@��#@���@��M@�L�@��@��m@�|�@�!@��d@��4@�Dg@�-w@��p@�{�@���@���@��S@�J�@��@��)@���@�v�@�M�@�#:@��m@��^@�n/@�@��j@�tT@�Q�@�<�@�J@��@��@�zx@�s�@�A�@��@��H@���@�r�@��@���@��n@�|@�P�@���@���@��@�c�@�@�@��@��*@�0�@��@��@���@��@�%�@��@��C@���@�e,@��@���@��v@��E@�c�@�=q@��@���@�L�@�@��E@�z@�L0@���@�H�@��]@�Xy@��W@�]�@��@�Ɇ@��D@�c @�M@�3�@��A@�o @��|@��,@��1@�M@���@���@��	@�\�@�2a@��@��p@�oi@���@���@�Q�@��@��c@�Ĝ@��@���@�[�@�Ft@�J@���@�zx@��@�u%@�I�@�1�@�$@�	@��H@�zx@�@���@��x@���@�'R@��@@O@~�!@~@}��@}j@} \@|��@|�)@|]d@{�@{y�@z�c@z�@z;�@yT�@x�?@x��@xl"@xK^@x-�@w��@wخ@w]�@v��@v;�@u��@u��@u�7@uV@t��@t]d@t%�@s�W@s�0@s@O@r�s@rz@r-@q��@q��@q��@qo @q�@p��@p:�@o��@o@O@n��@m�h@l�@l�Y@l�@k��@kX�@j�m@j)�@j	@i�Z@i�9@i��@i|@iIR@i�@h7�@g��@gF�@g�@f�@fe@e��@ex�@e�@d��@dV�@c��@cJ#@b��@b�+@bq�@bd�@bW�@a�@a&�@`N�@_�]@_��@_�w@_�4@^�,@^��@^��@^a|@^&�@]�>@]�X@]Dg@\�_@[��@[�K@[��@[�f@[,�@Z��@Z�\@Zp;@Z-@Y�9@Y��@X�[@X<�@Wƨ@W�{@W8@V�8@V�'@Vu%@VB[@V�@V_@U��@U��@Uc�@U4@T��@TA�@S��@S�P@Sx@SX�@S(@R�@R3�@Q�9@Q�C@Q�@Qq@P�u@P-�@O�@O�@@O�@N�H@Nȴ@N~�@NR�@N{@Mԕ@M��@M��@M�7@M*0@L��@Ly>@LXy@LQ�@K�@K�P@J�s@J@I��@Ip�@IB�@H�@H��@H��@Hm�@H9X@H�@G�
@G�P@GC@F�@Fp;@F	@E�@E�'@EIR@D�_@DM@DM@C�m@C�@@C�P@C,�@B��@B�@B�r@B#:@A�o@A�j@A�X@Ae,@A	l@@��@@��@@~(@@M@@/�@@7@?��@?�@?&@>�H@>}V@=��@=�H@=��@=Dg@<ی@<�@;�@;��@;v`@;,�@:��@:c @9�o@9�~@9e,@9<6@9@8��@8�@8��@8z�@8K^@7��@7Z�@7�@6��@6a|@6#:@6 �@5�@5`B@5�@4��@4|�@3�@3�q@2�2@2}V@2)�@1��@14@0��@0z�@0Ft@0 �@/�m@/��@/33@.ں@.�r@.ff@.@-�#@-�@-��@-*0@,��@,�u@,%�@+�@+�$@+&@*��@*��@*p;@*C�@*�@)�d@)s�@)+�@(�)@(�@(,=@'�*@'RT@&�c@&��@&L0@&�@%�@%�@%��@%x�@%Vm@$�@$��@$�D@$�@$q@$S�@#�+@#خ@#��@#�@#|�@#Mj@#(@"�@"��@"Q@"5?@"e@!�@!��@!�h@!8�@!�@ �e@ x@�q@�@b�@P�@��@q�@i�@C�@��@�o@[W@ѷ@��@�@�.@tT@l"@D�@1@  @��@�@�a@�f@_p@.I@�6@a|@B[@�@��@��@��@�@c�@+�@�@Ĝ@$@˒@��@dZ@8@�@ȴ@Q@&�@�@��@��@�t@��@��@J�@	l@�@��@�_@V�@C-@�@�g@�[@�@�:@iD@��@�b@Ov@�@�@�@��@��@��@S&@�@�U@��@C-@�@�0@��@U�11111111111111111111111111111111111111114411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Bc Bb�Bb�Bb�BbhBa�Ba�Ba�Ba-B`�B`vB`�B`vB`�B`�B`�B`�B`�B`�BaHBa-BaBa-BaB`�B`�B`�Ba-Bb4B��B�B�pB
�B
eB
kQB
~B
�B
��B
�rB
��B
�B
�B
��B
�[B
�|B$�B/OB1vB.�B&�BG_BC�BM�B_VBwfB�rB�vB�GB��B��B�B��B��B��B��B�(BmB"BdB�B�B�B�B�6B�tB�MB�QB��B��B�B�B��B��B�B��Bk�BI�B�B�B
�*B
�,B
X�B
n�B
R�B
/B
EB	՛B	�!B	��B	iB	U�B	C�B	./B	'�B	;B	gB	�B	 iB�(B��B�}B	�B	OB	*B	0!B	)�B	 B	�B	7B	�B	 \B	>�B	[#B	�B	��B	�2B	�;B	��B
�B
fB
�B

�B
�B

�B
�B
�B
$B
.cB
3�B
:�B
?}B
CB
J#B
NVB
O�B
TFB
S�B
U2B
X�B
VB
U�B
W�B
U�B
VB
W�B
X�B
W
B
V�B
V�B
W�B
Y�B
[	B
XEB
TFB
T�B
]�B
]~B
X�B
R�B
OvB
M�B
N"B
P�B
P�B
U�B
X+B
YKB
YKB
]/B
W�B
Q�B
N<B
K�B
K)B
KB
M�B
N�B
T{B
Y�B
W�B
V9B
T�B
UMB
U�B
VB
V�B
WsB
V�B
WsB
V�B
U�B
R�B
SuB
RTB
Q�B
QNB
P�B
O�B
PbB
O�B
O\B
O(B
N"B
MB
LJB
K�B
L�B
L�B
M�B
KxB
H�B
F�B
D�B
D�B
GzB
I�B
I�B
H�B
E�B
?�B
?}B
>]B
9�B
7�B
5%B
2|B
1�B
/iB
-]B
+kB
*�B
)�B
)�B
)B
(�B
)*B
)�B
(�B
)yB
)DB
)*B
(�B
(�B
)_B
(�B
(XB
'�B
'�B
'B
&�B
&fB
%�B
%,B
$�B
$ZB
#�B
"�B
!bB
�B
B
B
�B
WB
_B
)B
�B
B
yB
sB
�B
SB
B
�B
[B
�B
�B
�B
�B
B
)B
	�B
B
B
�B
�B
�B
'B
�B
�B
�B
�B
�B
 �B	��B	��B	��B	��B	�JB	��B	��B	��B	�rB	�>B	�B	��B	�2B	�RB	��B	�fB	�B	��B	��B	�^B	��B	�(B	��B	��B	�}B	�cB
 OB	��B	�cB
  B
 �B
 B
 B	��B	��B	�HB	��B	�B	��B	��B	��B	�wB	�(B	��B	�BB	�wB	�]B	�BB	�wB	��B	��B	��B	��B	�.B	��B
 �B
 B
 B
 OB
 OB
 �B
 4B	��B	��B	��B
 �B	��B	��B	�}B	�}B	�cB	�B	��B	��B
 �B
 B
 B
  B
 OB
 �B
 �B
 �B
 �B
�B
�B
[B
�B
�B
�B
GB
B
�B
uB
�B
GB
�B
�B
�B
�B
�B
�B
SB
SB
SB
B
B
B
B
�B
SB
tB
�B
�B
�B
B
�B
+B
�B
�B
�B
	B
	�B
	lB
	�B
	�B
	�B

=B

rB

�B

�B

rB
)B
^B
�B
�B
�B
�B
dB
dB
�B
�B
�B
B
B
PB
�B
�B
B
B
"B
�B
B
B
�B
�B
�B
�B
�B
pB
�B
pB
pB
pB
pB
�B
B
B
�B
�B
B
HB
�B
 B
�B
�B
�B
aB
2B
�B
MB
gB
gB
�B
�B
B
B
mB
9B
�B
�B
�B
�B
�B
�B
B
�B
�B
+B
�B
�B
�B
yB
EB
B
�B
EB
�B
�B
�B
�B
KB
�B
�B
�B
B
B
�B
�B
�B
B
CB
]B
CB
�B
B
�B
�B
�B
jB
�B
B
 BB
 �B
 �B
 �B
 �B
 �B
!bB
!|B
!�B
!�B
"NB
"�B
"�B
"�B
#nB
# B
#�B
#�B
$B
$&B
$B
$�B
$�B
%FB
%,B
%zB
%�B
&�B
'RB
'�B
'�B
(>B
(>B
(>B
(
B
(>B
(�B
)yB
)_B
)�B
*�B
*�B
+6B
+B
+QB
+�B
+kB
+�B
,"B
,�B
-)B
-]B
-�B
-�B
.B
./B
./B
.�B
.�B
/B
/5B
/OB
/�B
0�B
0�B
0�B
0�B
1B
1[B
1�B
2aB
2�B
2�B
2�B
3B
3MB
3hB
3�B
4B
4TB
4�B
4�B
4�B
4�B
5B
5?B
5ZB
5�B
5�B
5�B
6FB
6�B
6�B
6�B
6�B
6�B
6�B
6�B
6�B
7fB
7�B
7�B
7�B
8B
8�B
9	B
9XB
9rB
9�B
9rB
9�B
9�B
:*B
:xB
:�B
:�B
:�B
:�B
;B
;�B
<6B
<jB
<�B
<�B
=VB
=�B
=�B
>B
>B
>]B
?.B
?�B
?�B
?�B
@ B
@B
@4B
@4B
@4B
AB
AoB
A�B
A�B
B'B
B�B
B�B
B�B
BuB
B�B
B�B
CaB
C�B
C�B
DMB
D3B
DMB
DB
D�B
D�B
EmB
E�B
E�B
E�B
E�B
F%B
FtB
FYB
FtB
FtB
F�B
F�B
GB
GzB
G�B
HB
HB
HKB
H�B
H�B
IB
H�B
I7B
IRB
IlB
I�B
J#B
JrB
J�B
J�B
J�B
KB
K^B
KxB
K�B
KxB
K�B
K�B
K�B
K�B
LJB
L~B
L�B
L�B
L�B
L�B
L�B
MB
M�B
M�B
M�B
M�B
M�B
NVB
N�B
N�B
OB
O\B
O\B
O\B
OvB
OvB
O�B
O�B
O�B
O�B
O�B
O�B
PHB
P�B
QB
Q B
QNB
QhB
Q�B
R B
RoB
R�B
R�B
S&B
S[B
S[B
SuB
S�B
S�B
S�B
S�B
TFB
TFB
T�B
T�B
T�B
UB
U�B
VmB
VmB
V�B
V�B
V�B
V�B
W
B
W
B
W$B
W�B
W�B
W�B
W�B
W�B
XB
XEB
XyB
X_B
X�B
X�B
X�B
X�B
X�B
YB
Y�B
Y�B
ZB
Z�B
ZkB
Z�B
Z�B
[	B
[#B
[�B
[�B
[�B
\)B
\�B
\�B
]/B
]dB
]dB
]�B
]�B
]�B
]�B
^B
]�B
^B
^jB
^�B
_B
_;B
_�B
_�B
_�B
_�B
`'B
`'B
`vB
`vB
`�B
`�B
a|B
a�B
a�B
bNB
b�B
b�B
cB
c:B
c:B
cnB
c�B
c�B
d@B
dZB
dZB
d�B
d�B
d�B
d�B
eFB
e`B
e�B
e�B
e�B
ffB
f�B
f�B
gB
gB
gRB
g8B
g�B
g�B
h
B
h>B
hsB
h�B
iB
iDB
i�B
i�B
i�B
jKB
jB
j�B
jB
j�B
j�B
k6B
kQB
kQB
kQB
kQB
kkB
k�B
k�B
lB
lB
l=B
lWB
l�B
lqB
l�B
l�B
l�B
l�B
l�B
l�B
m)B
mwB
mwB
m�B
n}B
n�B
o B
o B
n�B
o�B
o�B
o�B
o�B
p!B
pB
p�B
q'B
qAB
q[B
q[B
qvB
q[B
q�B
q�B
q�B
q�B
q�B
rB
rGB
raB
r|B
s3B
s�B
s�B
s�B
s�B
s�B
tB
tB
tB
tnB
tTB
t�B
u?B
utB
uZB
u�B
u�B
u�B
u�B
v`B
v`B
v�B
v�B
v�B
v�B
wB
v�B
wfB
w�B
w�B
w�B
xB
xlB
xRB
x�B
x�B
x�B
x�B
x�B
y	B
y�B
y�B
y�B
z*B
z^B
zDB
z^B
z^B
z^B
z�B
{B
{0B
{B
{�B
{�B
{�B
|B
|P11111111111111111111111111111111111111114411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Bc Bb�Bb�Bb�BbhBa�Ba�Ba�Ba-B`�B`vB`�B`vB`�B`�B`�B`�B`�B`�BaHBa-BaBa-BaB`�B`�B`�Ba-Bb4B��B�B�pB
�B
eB
kQB
~B
�B
��B
�rB
��B
�B
�B
��B
�[B
�|B$�B/OB1vB.�B&�BG_BC�BM�B_VBwfB�rB�vB�GB��B��B�B��B��B��B��B�(BmB"BdB�B�B�B�B�6B�tB�MB�QB��B��B�B�B��B��B�B��Bk�BI�B�B�B
�*B
�,B
X�B
n�B
R�B
/B
EB	՛B	�!B	��B	iB	U�B	C�B	./B	'�B	;B	gB	�B	 iB�(B��B�}B	�B	OB	*B	0!B	)�B	 B	�B	7B	�B	 \B	>�B	[#B	�B	��B	�2B	�;B	��B
�B
fB
�B

�B
�B

�B
�B
�B
$B
.cB
3�B
:�B
?}B
CB
J#B
NVB
O�B
TFB
S�B
U2B
X�B
VB
U�B
W�B
U�B
VB
W�B
X�B
W
B
V�B
V�B
W�B
Y�B
[	B
XEB
TFB
T�B
]�B
]~B
X�B
R�B
OvB
M�B
N"B
P�B
P�B
U�B
X+B
YKB
YKB
]/B
W�B
Q�B
N<B
K�B
K)B
KB
M�B
N�B
T{B
Y�B
W�B
V9B
T�B
UMB
U�B
VB
V�B
WsB
V�B
WsB
V�B
U�B
R�B
SuB
RTB
Q�B
QNB
P�B
O�B
PbB
O�B
O\B
O(B
N"B
MB
LJB
K�B
L�B
L�B
M�B
KxB
H�B
F�B
D�B
D�B
GzB
I�B
I�B
H�B
E�B
?�B
?}B
>]B
9�B
7�B
5%B
2|B
1�B
/iB
-]B
+kB
*�B
)�B
)�B
)B
(�B
)*B
)�B
(�B
)yB
)DB
)*B
(�B
(�B
)_B
(�B
(XB
'�B
'�B
'B
&�B
&fB
%�B
%,B
$�B
$ZB
#�B
"�B
!bB
�B
B
B
�B
WB
_B
)B
�B
B
yB
sB
�B
SB
B
�B
[B
�B
�B
�B
�B
B
)B
	�B
B
B
�B
�B
�B
'B
�B
�B
�B
�B
�B
 �B	��B	��B	��B	��B	�JB	��B	��B	��B	�rB	�>B	�B	��B	�2B	�RB	��B	�fB	�B	��B	��B	�^B	��B	�(B	��B	��B	�}B	�cB
 OB	��B	�cB
  B
 �B
 B
 B	��B	��B	�HB	��B	�B	��B	��B	��B	�wB	�(B	��B	�BB	�wB	�]B	�BB	�wB	��B	��B	��B	��B	�.B	��B
 �B
 B
 B
 OB
 OB
 �B
 4B	��B	��B	��B
 �B	��B	��B	�}B	�}B	�cB	�B	��B	��B
 �B
 B
 B
  B
 OB
 �B
 �B
 �B
 �B
�B
�B
[B
�B
�B
�B
GB
B
�B
uB
�B
GB
�B
�B
�B
�B
�B
�B
SB
SB
SB
B
B
B
B
�B
SB
tB
�B
�B
�B
B
�B
+B
�B
�B
�B
	B
	�B
	lB
	�B
	�B
	�B

=B

rB

�B

�B

rB
)B
^B
�B
�B
�B
�B
dB
dB
�B
�B
�B
B
B
PB
�B
�B
B
B
"B
�B
B
B
�B
�B
�B
�B
�B
pB
�B
pB
pB
pB
pB
�B
B
B
�B
�B
B
HB
�B
 B
�B
�B
�B
aB
2B
�B
MB
gB
gB
�B
�B
B
B
mB
9B
�B
�B
�B
�B
�B
�B
B
�B
�B
+B
�B
�B
�B
yB
EB
B
�B
EB
�B
�B
�B
�B
KB
�B
�B
�B
B
B
�B
�B
�B
B
CB
]B
CB
�B
B
�B
�B
�B
jB
�B
B
 BB
 �B
 �B
 �B
 �B
 �B
!bB
!|B
!�B
!�B
"NB
"�B
"�B
"�B
#nB
# B
#�B
#�B
$B
$&B
$B
$�B
$�B
%FB
%,B
%zB
%�B
&�B
'RB
'�B
'�B
(>B
(>B
(>B
(
B
(>B
(�B
)yB
)_B
)�B
*�B
*�B
+6B
+B
+QB
+�B
+kB
+�B
,"B
,�B
-)B
-]B
-�B
-�B
.B
./B
./B
.�B
.�B
/B
/5B
/OB
/�B
0�B
0�B
0�B
0�B
1B
1[B
1�B
2aB
2�B
2�B
2�B
3B
3MB
3hB
3�B
4B
4TB
4�B
4�B
4�B
4�B
5B
5?B
5ZB
5�B
5�B
5�B
6FB
6�B
6�B
6�B
6�B
6�B
6�B
6�B
6�B
7fB
7�B
7�B
7�B
8B
8�B
9	B
9XB
9rB
9�B
9rB
9�B
9�B
:*B
:xB
:�B
:�B
:�B
:�B
;B
;�B
<6B
<jB
<�B
<�B
=VB
=�B
=�B
>B
>B
>]B
?.B
?�B
?�B
?�B
@ B
@B
@4B
@4B
@4B
AB
AoB
A�B
A�B
B'B
B�B
B�B
B�B
BuB
B�B
B�B
CaB
C�B
C�B
DMB
D3B
DMB
DB
D�B
D�B
EmB
E�B
E�B
E�B
E�B
F%B
FtB
FYB
FtB
FtB
F�B
F�B
GB
GzB
G�B
HB
HB
HKB
H�B
H�B
IB
H�B
I7B
IRB
IlB
I�B
J#B
JrB
J�B
J�B
J�B
KB
K^B
KxB
K�B
KxB
K�B
K�B
K�B
K�B
LJB
L~B
L�B
L�B
L�B
L�B
L�B
MB
M�B
M�B
M�B
M�B
M�B
NVB
N�B
N�B
OB
O\B
O\B
O\B
OvB
OvB
O�B
O�B
O�B
O�B
O�B
O�B
PHB
P�B
QB
Q B
QNB
QhB
Q�B
R B
RoB
R�B
R�B
S&B
S[B
S[B
SuB
S�B
S�B
S�B
S�B
TFB
TFB
T�B
T�B
T�B
UB
U�B
VmB
VmB
V�B
V�B
V�B
V�B
W
B
W
B
W$B
W�B
W�B
W�B
W�B
W�B
XB
XEB
XyB
X_B
X�B
X�B
X�B
X�B
X�B
YB
Y�B
Y�B
ZB
Z�B
ZkB
Z�B
Z�B
[	B
[#B
[�B
[�B
[�B
\)B
\�B
\�B
]/B
]dB
]dB
]�B
]�B
]�B
]�B
^B
]�B
^B
^jB
^�B
_B
_;B
_�B
_�B
_�B
_�B
`'B
`'B
`vB
`vB
`�B
`�B
a|B
a�B
a�B
bNB
b�B
b�B
cB
c:B
c:B
cnB
c�B
c�B
d@B
dZB
dZB
d�B
d�B
d�B
d�B
eFB
e`B
e�B
e�B
e�B
ffB
f�B
f�B
gB
gB
gRB
g8B
g�B
g�B
h
B
h>B
hsB
h�B
iB
iDB
i�B
i�B
i�B
jKB
jB
j�B
jB
j�B
j�B
k6B
kQB
kQB
kQB
kQB
kkB
k�B
k�B
lB
lB
l=B
lWB
l�B
lqB
l�B
l�B
l�B
l�B
l�B
l�B
m)B
mwB
mwB
m�B
n}B
n�B
o B
o B
n�B
o�B
o�B
o�B
o�B
p!B
pB
p�B
q'B
qAB
q[B
q[B
qvB
q[B
q�B
q�B
q�B
q�B
q�B
rB
rGB
raB
r|B
s3B
s�B
s�B
s�B
s�B
s�B
tB
tB
tB
tnB
tTB
t�B
u?B
utB
uZB
u�B
u�B
u�B
u�B
v`B
v`B
v�B
v�B
v�B
v�B
wB
v�B
wfB
w�B
w�B
w�B
xB
xlB
xRB
x�B
x�B
x�B
x�B
x�B
y	B
y�B
y�B
y�B
z*B
z^B
zDB
z^B
z^B
z^B
z�B
{B
{0B
{B
{�B
{�B
{�B
|B
|P11111111111111111111111111111111111111114411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105244  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604192415  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604192415  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604192415                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605042423  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605042423  QCF$                G�O�G�O�G�O�            4000JA  ARUP                                                                        20220610161505                      G�O�G�O�G�O�                