CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:23:58Z creation;2022-06-04T19:23:59Z conversion to V3.1      
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
_FillValue                 �  ID   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M8   PRES_ADJUSTED_QC         
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
_FillValue                 �  �l   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �`   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �$   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �X   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �L   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �L   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �L   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �L   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �x   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �|   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20220604192358  20220610161505  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               LA   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @�dy?V�1   @�dyӠm:@-�����c�C��%1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B33B�  B���B�33B�  B���B�ffB�ffB�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B왚B�ffB�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C 33C!��C$  C&  C(  C*  C+�fC-�fC0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL� CN  CO�fCQ�fCT  CU�fCW�fCZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�,�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @@|(�@�{@�{A
=A?
=A_
=A
=A��A��A��A��AυA߅A�A��BBBBB'B/B7B?BGBOBWB_BgBoBwB~��B��HB�z�B�{B��HB��B�G�B�G�B��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB�{B��HB��HB��HB��HB��HB�z�B�G�B��HB��HB��HB��HC�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C #�C!�qC#�C%�C'�C)�C+�
C-�
C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CLp�CM�CO�
CQ�
CS�CU�
CW�
CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��C��RC��RC��RC��RC��RC��RC��RC��RC��RC�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RD |)D �)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D	|)D	�)D
|)D
�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D |)D �)D!|)D!�)D"|)D"�)D#|)D#�)D$|)D$�)D%|)D%�)D&|)D&�)D'|)D'�)D(|)D(�)D)|)D)�)D*|)D*�)D+|)D+�)D,|)D,�)D-|)D-�)D.|)D.�)D/|)D/�)D0|)D0�)D1|)D1�)D2|)D2�)D3|)D3�)D4|)D4�)D5|)D5�)D6|)D6�)D7|)D7�)D8|)D8�)D9|)D9�)D:|)D:�)D;|)D;�)D<|)D<�)D=|)D=�)D>|)D>�)D?|)D?�)D@|)D@�)DA|)DA�)DB|)DB�)DC|)DC�)DD|)DD�)DE|)DE�)DF|)DF�)DG|)DG�)DH|)DH�)DI|)DI�)DJ|)DJ�)DK|)DK�)DL|)DL�)DM|)DM�)DN|)DN�)DO|)DO�)DP|)DP�)DQ|)DQ�)DR|)DR�)DS|)DS�)DT|)DT�)DU|)DU�)DV|)DV�)DW|)DW�)DX|)DX�)DY|)DY�)DZ|)DZ�)D[|)D[�)D\|)D\�)D]|)D]�)D^|)D^�)D_|)D_�)D`|)D`�)Da|)Da�)Db|)Db�)Dc|)Dc�)Dd|)Dd�)De|)De�)Df|)Df�)Dg|)Dg�)Dh|)Dh�)Di|)Di�)Dj|)Dj�)Dk|)Dk�)Dl|)Dl�)Dm|)Dm�)Dn|)Dn�)Do|)Do�)Dp|)Dp�)Dq|)Dq�)Dr|)Dr�)Ds|)Ds�)Dt|)Dt�)Du|)Du�)Dv|)Dv�)Dw|)Dw�)Dx|)Dx�)Dy|)Dy�)Dz|)Dz�)D{|)D{�)D||)D|�)D}|)D}�)D~|)D~�)D|)D�)D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D¾D��D�>D�~DþD��D�>D�~DľD��D�>D�~DžD��D�>D�~DƾD��D�>D�~DǾD��D�>D�~DȾD��D�>D�~DɾD��D�>D�~DʾD��D�>D�~D˾D��D�>D�~D̾D��D�>D�~D;D��D�>D�~DξD��D�>D�~DϾD��D�>D�~DоD��D�>D�~DѾD��D�>D�~DҾD��D�>D�~DӾD��D�>D�~DԾD��D�>D�~DվD��D�>D�~D־D��D�>D�~D׾D��D�>D�~DؾD��D�>D�~DپD��D�>D�~DھD��D�>D�~D۾D��D�>D�~DܾD��D�>D�~DݾD��D�>D�~D޾D��D�>D�~D߾D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D��D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�*�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��A��A���A���A���A��A��A��A��mA��
A��A��A���A���A��8A��
A���A��A��mA���A��>A��DA��A��sA��DA��A��
A��`A��2A�ΥAƲ�A�?�Aü�A�8�A�/A��vA���A��A�YA�PA���A���A�bA���A�-A�O�A���A�;�A�'A�w�A���A�;A�tTA���A�
�A�NpA�W
A�u�A�T�A���A�'�A��-A��RA�33A�&�A��QA�k�A���A�0�A��0A���A��-A���A��A��A���A��A��A�sMAz�mAsTaAl�4AiخAh��Ag��Ad�MA_�>AZ�1AX�AW-wAUO�ARM�AP�dANb�AH��AH�AH�AI��AI�}AH��AG�AC��AB�=A=Q�A<�fA=Q�A=O�A<  A:w2A9�nA9N�A8�zA7]dA5�jA4��A4��A4s�A4OvA3�/A3X�A3�A3VA2�8A3%FA2�.A2�\A2S&A1�A02�A/�5A/� A/�pA/�eA/ZA.��A.+�A-MA+A)��A(OvA'T�A%�XA%'RA%%A$��A#�$A#�A"�A"G�A!��A jA��A�RAm�A:*A#:A^5A֡A�+AA AVA�SA��Aj�A�A�VA%FA��AA��A�"A��A �A��A�*AL�A(A�XA.IA��AMA�XA'RA�3A&AݘA�XA�>Ap�A�_A�'A%AL�A;�A%A�A\�A
�zA
cA
h
A
�A	��A	�A	��A	n�A�HA>BA��A��AیA0UA��A_pA)_A�A7A�fAK^A�SAI�AA�6A��Av`Ai�AqA �CA "�@�@�:�@�o�@�#�@�
�@���@�,=@�!�@�H�@���@��r@�j@�&�@��z@���@�zx@�w�@�"@��@�@�@�l�@�&�@�J�@��W@�S�@���@�x@�_@��@�IR@�m�@���@�o @��@�o @��@�M�@�@��@�0U@���@㰊@�_p@�A�@��@���@�\�@�	�@�n/@�M@���@݁�@��@ܫ6@�~(@��.@�O�@�_�@��g@��@��N@�^�@�'�@�}V@մ�@�+@�u%@���@�_p@��@Ҽj@�=q@ѧ�@�A�@��@�D�@ψf@��@·+@�2�@���@�خ@�:�@��/@̪e@�}V@�z@�3�@˒:@��@��@�ی@ʠ�@�(�@ɀ4@�F�@�=�@��"@Ȣ4@��j@��@�$�@��K@�S&@��@�m�@��@��@ãn@�W?@�,�@¬@�8�@�1@��)@�S�@��5@��@�i�@�A�@�*�@���@�2a@�l"@���@�@�Ta@��Z@���@�!-@���@�N�@�u@���@�k�@�ߤ@��1@�K^@�	@��W@���@��@���@�Q�@�S@���@�W�@�7@���@�?}@��@���@��+@�h�@�_�@�Ov@�!@��H@��@���@��@���@�:�@�%@��@��@�Ĝ@��I@�$@��n@�Q�@��@� i@��@��]@���@��p@���@���@��o@�7�@��W@���@�E9@�Ĝ@�~@��K@��M@�E9@�ߤ@��@�v�@��@��n@�*0@���@�)�@��"@�C�@��<@�w�@�E�@��Z@��@�_p@�G�@�'�@��@�Ta@�(�@��A@�ƨ@��F@���@�X@�H�@�4@��@��2@��z@���@� �@���@�Z�@�@��@���@�Xy@�:�@�#:@��g@���@�t�@�J�@��@���@�H�@�0U@��@�� @���@�S&@��@��@���@��=@��@�j�@�^�@�L�@�o@���@��9@�.I@���@���@��E@��@�oi@�L0@�!@��@���@��@���@���@���@�X@��@��@��U@�y>@�)�@��A@��@��f@�X@�+@���@�{@��@��@�=@��5@��$@���@�bN@�3�@�G@���@�A @�)_@��]@�n�@�~@��@��k@�4@�0�@�o@��L@�K^@��@��N@��M@�S&@�+�@��[@�_�@��@���@�X@�F@�'�@���@�2�@��@�l�@�Y@�Ɇ@��.@�m�@�($@��t@�iD@��c@���@��@�V�@��A@�p�@��@���@�}V@�B[@�4@��g@���@��$@�~�@�o�@�hs@�IR@�ߤ@�q�@�%�@���@���@��M@�N<@��'@�_@ƨ@1�@~��@~#:@}��@}T�@}%@|�@|'R@{�@{8@{
=@z��@z��@z�r@zJ�@z($@y��@y��@y*0@x�)@x��@w�@v��@v�'@v�+@v_�@v1�@u�@u��@u�@t�p@tu�@t%�@s�@sqv@s/�@s
=@r��@r0U@qԕ@q��@qDg@p�@p �@o1�@n��@n��@n�@nC�@m��@mϫ@m�~@mp�@mDg@l�|@lw�@k�P@kS�@kK�@k!-@j�,@j��@jM�@i��@iA @i \@i%@h_@g�6@gqv@g�@fQ@e�@e \@d�9@d�_@c�g@cC�@b�M@b��@b8�@a��@a�@am]@`�@`'R@_�g@_�{@_4�@^�@^�,@^��@^s�@^M�@^#:@]��@\ѷ@\m�@[�F@[{J@[O@Z�@Z{@X��@X��@XK^@W� @W�	@WP�@W;d@W�@V��@V�@VTa@U�7@T�5@Th�@S�	@SK�@SS@R�@Rں@R��@Rp;@R_@Q��@QS&@P��@P��@Pu�@O˒@OS�@O@N��@M�>@M��@Ma�@L�P@LXy@K��@KW?@J҉@JC�@I��@IA @H��@HɆ@H��@HɆ@H�o@G�@F�]@F��@FTa@F4@E�@E?}@E;@D�f@D��@DU2@C|�@CH�@B�"@B{�@A��@A@A@A�C@Ax�@A8�@A�@@��@@I�@?��@?\)@?'�@>�2@>��@>Ov@=�o@=p�@=?}@=4@=�@<�z@<`�@<G@;n/@:�L@:\�@9�.@9�d@9}�@9p�@8֡@7�Q@7��@6��@6��@6{�@61�@5ϫ@5�@4�@4�$@4U2@3�@3�k@3b�@3
=@2�H@2��@2)�@1�>@1��@1��@1Y�@0��@0��@02�@/�@/��@/�4@/a@/O@/C�@/A�@/,�@/�@.ں@.�<@.��@.�+@.q�@.E�@-�@-�H@-��@-m]@-�@,�@,u�@+�+@+�6@+��@+j�@+�@*ȴ@*�A@*^5@)�@)�'@)Dg@(��@(PH@(�@'� @'�f@'iD@'+@&�@&W�@&e@%�d@%�@%�X@%�M@%Y�@%+�@$��@$�I@$%�@#�@#�K@#�w@#��@#��@#9�@"�@"�r@"H�@!�.@!��@!c@!%@ ѷ@ �z@ ~(@ ?�@ %�@�Q@�F@��@H�@��@�}@u%@5?@��@��@?}@ \@�@��@�@�?@��@m�@?�@�@� @�@y�@\)@;d@�@�B@�m@�X@ȴ@�@�@��@�x@s�@M�@�o@�n@Y�@+�@�E@�@��@�u@��@~(@U2@�@��@o�@(@�6@kQ@=q@�d@��@8�@%@�)@��@A�@�]@�+@�@��@�@@�P@e�@K�@�@�@�B@�b@�A@p;@d�@^5@Ov@H�@-@�Z@�z@�-@T�@+�@�@�/@�@q@l"@g8@g8@[�@(�@�@�6@��@�$@qv@_p@/�@�@�s@�1@xl@=q@�@�9@��@�@p�@<6@�@��@�@��@Z@7�@�@�@��@x@&@�@
�y@
��@
�A@
!�@
{@	�Z@	�C@	�~@	k�@	X@	+�@	@	�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��A��A���A���A���A��A��A��A��mA��
A��A��A���A���A��8A��
A���A��A��mA���A��>A��DA��A��sA��DA��A��
A��`A��2A�ΥAƲ�A�?�Aü�A�8�A�/A��vA���A��A�YA�PA���A���A�bA���A�-A�O�A���A�;�A�'A�w�A���A�;A�tTA���A�
�A�NpA�W
A�u�A�T�A���A�'�A��-A��RA�33A�&�A��QA�k�A���A�0�A��0A���A��-A���A��A��A���A��A��A�sMAz�mAsTaAl�4AiخAh��Ag��Ad�MA_�>AZ�1AX�AW-wAUO�ARM�AP�dANb�AH��AH�AH�AI��AI�}AH��AG�AC��AB�=A=Q�A<�fA=Q�A=O�A<  A:w2A9�nA9N�A8�zA7]dA5�jA4��A4��A4s�A4OvA3�/A3X�A3�A3VA2�8A3%FA2�.A2�\A2S&A1�A02�A/�5A/� A/�pA/�eA/ZA.��A.+�A-MA+A)��A(OvA'T�A%�XA%'RA%%A$��A#�$A#�A"�A"G�A!��A jA��A�RAm�A:*A#:A^5A֡A�+AA AVA�SA��Aj�A�A�VA%FA��AA��A�"A��A �A��A�*AL�A(A�XA.IA��AMA�XA'RA�3A&AݘA�XA�>Ap�A�_A�'A%AL�A;�A%A�A\�A
�zA
cA
h
A
�A	��A	�A	��A	n�A�HA>BA��A��AیA0UA��A_pA)_A�A7A�fAK^A�SAI�AA�6A��Av`Ai�AqA �CA "�@�@�:�@�o�@�#�@�
�@���@�,=@�!�@�H�@���@��r@�j@�&�@��z@���@�zx@�w�@�"@��@�@�@�l�@�&�@�J�@��W@�S�@���@�x@�_@��@�IR@�m�@���@�o @��@�o @��@�M�@�@��@�0U@���@㰊@�_p@�A�@��@���@�\�@�	�@�n/@�M@���@݁�@��@ܫ6@�~(@��.@�O�@�_�@��g@��@��N@�^�@�'�@�}V@մ�@�+@�u%@���@�_p@��@Ҽj@�=q@ѧ�@�A�@��@�D�@ψf@��@·+@�2�@���@�خ@�:�@��/@̪e@�}V@�z@�3�@˒:@��@��@�ی@ʠ�@�(�@ɀ4@�F�@�=�@��"@Ȣ4@��j@��@�$�@��K@�S&@��@�m�@��@��@ãn@�W?@�,�@¬@�8�@�1@��)@�S�@��5@��@�i�@�A�@�*�@���@�2a@�l"@���@�@�Ta@��Z@���@�!-@���@�N�@�u@���@�k�@�ߤ@��1@�K^@�	@��W@���@��@���@�Q�@�S@���@�W�@�7@���@�?}@��@���@��+@�h�@�_�@�Ov@�!@��H@��@���@��@���@�:�@�%@��@��@�Ĝ@��I@�$@��n@�Q�@��@� i@��@��]@���@��p@���@���@��o@�7�@��W@���@�E9@�Ĝ@�~@��K@��M@�E9@�ߤ@��@�v�@��@��n@�*0@���@�)�@��"@�C�@��<@�w�@�E�@��Z@��@�_p@�G�@�'�@��@�Ta@�(�@��A@�ƨ@��F@���@�X@�H�@�4@��@��2@��z@���@� �@���@�Z�@�@��@���@�Xy@�:�@�#:@��g@���@�t�@�J�@��@���@�H�@�0U@��@�� @���@�S&@��@��@���@��=@��@�j�@�^�@�L�@�o@���@��9@�.I@���@���@��E@��@�oi@�L0@�!@��@���@��@���@���@���@�X@��@��@��U@�y>@�)�@��A@��@��f@�X@�+@���@�{@��@��@�=@��5@��$@���@�bN@�3�@�G@���@�A @�)_@��]@�n�@�~@��@��k@�4@�0�@�o@��L@�K^@��@��N@��M@�S&@�+�@��[@�_�@��@���@�X@�F@�'�@���@�2�@��@�l�@�Y@�Ɇ@��.@�m�@�($@��t@�iD@��c@���@��@�V�@��A@�p�@��@���@�}V@�B[@�4@��g@���@��$@�~�@�o�@�hs@�IR@�ߤ@�q�@�%�@���@���@��M@�N<@��'@�_@ƨ@1�@~��@~#:@}��@}T�@}%@|�@|'R@{�@{8@{
=@z��@z��@z�r@zJ�@z($@y��@y��@y*0@x�)@x��@w�@v��@v�'@v�+@v_�@v1�@u�@u��@u�@t�p@tu�@t%�@s�@sqv@s/�@s
=@r��@r0U@qԕ@q��@qDg@p�@p �@o1�@n��@n��@n�@nC�@m��@mϫ@m�~@mp�@mDg@l�|@lw�@k�P@kS�@kK�@k!-@j�,@j��@jM�@i��@iA @i \@i%@h_@g�6@gqv@g�@fQ@e�@e \@d�9@d�_@c�g@cC�@b�M@b��@b8�@a��@a�@am]@`�@`'R@_�g@_�{@_4�@^�@^�,@^��@^s�@^M�@^#:@]��@\ѷ@\m�@[�F@[{J@[O@Z�@Z{@X��@X��@XK^@W� @W�	@WP�@W;d@W�@V��@V�@VTa@U�7@T�5@Th�@S�	@SK�@SS@R�@Rں@R��@Rp;@R_@Q��@QS&@P��@P��@Pu�@O˒@OS�@O@N��@M�>@M��@Ma�@L�P@LXy@K��@KW?@J҉@JC�@I��@IA @H��@HɆ@H��@HɆ@H�o@G�@F�]@F��@FTa@F4@E�@E?}@E;@D�f@D��@DU2@C|�@CH�@B�"@B{�@A��@A@A@A�C@Ax�@A8�@A�@@��@@I�@?��@?\)@?'�@>�2@>��@>Ov@=�o@=p�@=?}@=4@=�@<�z@<`�@<G@;n/@:�L@:\�@9�.@9�d@9}�@9p�@8֡@7�Q@7��@6��@6��@6{�@61�@5ϫ@5�@4�@4�$@4U2@3�@3�k@3b�@3
=@2�H@2��@2)�@1�>@1��@1��@1Y�@0��@0��@02�@/�@/��@/�4@/a@/O@/C�@/A�@/,�@/�@.ں@.�<@.��@.�+@.q�@.E�@-�@-�H@-��@-m]@-�@,�@,u�@+�+@+�6@+��@+j�@+�@*ȴ@*�A@*^5@)�@)�'@)Dg@(��@(PH@(�@'� @'�f@'iD@'+@&�@&W�@&e@%�d@%�@%�X@%�M@%Y�@%+�@$��@$�I@$%�@#�@#�K@#�w@#��@#��@#9�@"�@"�r@"H�@!�.@!��@!c@!%@ ѷ@ �z@ ~(@ ?�@ %�@�Q@�F@��@H�@��@�}@u%@5?@��@��@?}@ \@�@��@�@�?@��@m�@?�@�@� @�@y�@\)@;d@�@�B@�m@�X@ȴ@�@�@��@�x@s�@M�@�o@�n@Y�@+�@�E@�@��@�u@��@~(@U2@�@��@o�@(@�6@kQ@=q@�d@��@8�@%@�)@��@A�@�]@�+@�@��@�@@�P@e�@K�@�@�@�B@�b@�A@p;@d�@^5@Ov@H�@-@�Z@�z@�-@T�@+�@�@�/@�@q@l"@g8@g8@[�@(�@�@�6@��@�$@qv@_p@/�@�@�s@�1@xl@=q@�@�9@��@�@p�@<6@�@��@�@��@Z@7�@�@�@��@x@&@�@
�y@
��@
�A@
!�@
{@	�Z@	�C@	�~@	k�@	X@	+�@	@	�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   Bj�Bj�Bj�Bj�BkBkBkBk6BkQBk6Bk6Bj�BkBkBkBk6BkQBk6BkQBkkBkkBk�BkkBk�Bk�Bk�Bk�Bk�Bk�Bn}B�jB��B�DB	��B	��B
�JB
�,BY�B7BI�Bz�Bz�Bx�Bx�B�B��B��BуB�DB�)B�-B��B�EB��B�B��B�+B��B�B�B��BnIB^OBWYBI�B5�B.IB�BGB
��B
�=B
��B
{0B
shB
n�B
gRB
I�B
$�B
�B	�CB	��B	�B	|�B	u�B	k�B	U�B	?HB	"4B	�B	"B	B��B�CB�B�WB��B�B	�B	.cB	K�B	J�B	)DB	1�B	A�B	P�B	~BB	��B	�0B	׍B	�bB	��B	�B	�B	��B
tB
	7B
MB
"4B
'RB
0�B
6�B
A�B
K)B
S[B
X�B
ZkB
^5B
\B
N�B
K�B
L�B
PbB
O�B
M�B
I7B
EB
>B
.cB
 �B
SB
hB
�B
vB
�B
�B
�B
�B
tB
3B
�B	�.B
�B
B
	B
�B	�}B	��B	�B	��B	��B
 �B
 B	��B	�B	��B	߾B	�WB	�EB	�B	��B	�B	��B	��B	�B	��B
  B
)B
�B
�B
�B
4B
}B
�B
	B
�B
mB
?B
�B
�B
B
;B
&�B
-)B
-�B
-)B
,B
+QB
)yB
*0B
,�B
+QB
-]B
,qB
*�B
)*B
'�B
%B
#�B
$ZB
$�B
!�B
 �B
$@B
$�B
$�B
"NB
"4B
"B
#�B
"NB
"B
!�B
!|B
!B
 �B
�B
�B
=B
kB
�B
B
�B

rB

rB
�B
oB
�B
FB
,B
�B
�B
:B
�B
xB
KB
�B
%B
B
)B
�B
B
�B
B

	B
	�B
	�B
	RB
	B
fB
KB
�B
�B
?B
%B
B
�B
�B
�B
�B
MB
�B
3B
GB
-B
�B
�B
�B
uB
B
AB
uB
uB
�B
-B
�B
oB
 B
 4B	�cB	��B	�wB	��B	�B	��B	��B	�}B	��B	��B	�B	��B	�wB	��B	��B	�}B
  B	�.B	�.B	�cB	�cB	�B	��B	��B	��B
UB
�B
�B
�B
�B
mB
�B
�B
SB
�B
�B
�B
�B
B
-B
[B
 B
 �B
 �B
 �B
UB
�B
AB
�B
�B
aB
B
gB
3B
B
gB
B
�B
�B
mB
9B
9B
�B
�B
B
B
�B
{B
�B
�B
�B
MB
�B
B
�B
�B
�B
�B
�B
?B
YB
tB
YB
?B
YB
�B
�B
�B
B
B
B
�B
B
B
B
B
1B
�B
�B
�B
	B
	�B
	�B

	B

	B
	�B

	B
	�B

�B
)B
DB
DB
DB
^B
xB
xB
DB
^B
xB
�B
�B
B
0B
xB
~B
B
�B
�B
"B
�B
�B
�B
vB
�B
�B
B
�B
 B
B
4B
4B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
&B
�B
�B
�B
�B
�B
�B
�B
,B
FB
aB
�B
�B
�B
�B
�B
�B
2B
2B
�B
gB
�B
�B
�B
�B
B
�B
mB
SB
�B
�B
$B
$B

B

B
�B
�B
�B
�B
�B

B
�B
�B
�B
7B
	B
=B
qB
qB
qB
=B
WB
qB
�B
B
xB
�B
�B
B
~B
~B
�B
~B
�B
�B
B
~B
�B
B
B
5B
�B
B
OB
OB
OB
�B
�B
;B
�B
 BB
 BB
 BB
 �B
 'B
 \B
 �B
!-B
!HB
!�B
"�B
$tB
$�B
$�B
%B
%FB
%�B
%�B
%�B
%�B
%�B
&B
&fB
&2B
&LB
&fB
'mB
(
B
'�B
(XB
(�B
(�B
)B
)�B
)�B
)�B
*B
*�B
*KB
+B
+6B
+QB
+�B
,=B
,�B
,�B
,�B
-B
-B
-CB
-�B
.B
.}B
.�B
.�B
/B
/ B
/�B
0;B
0oB
0�B
1B
1'B
1�B
1�B
2|B
3�B
3�B
4B
4�B
5B
5ZB
5ZB
5�B
6FB
6`B
6`B
6�B
6�B
7B
6�B
7LB
7fB
7�B
7�B
7�B
8B
7�B
8B
8RB
8lB
8�B
8�B
8�B
9�B
9�B
:DB
:xB
;JB
;�B
;�B
<B
<�B
<�B
=VB
=VB
=�B
=�B
=�B
=�B
>B
>�B
>�B
?B
?cB
?�B
@�B
A B
A B
AUB
A;B
AUB
A�B
BAB
B[B
B[B
B'B
BAB
B�B
B�B
BAB
B�B
C�B
C�B
C�B
C�B
DgB
D�B
D�B
D�B
EB
E9B
EB
EB
E9B
EmB
EmB
E�B
E�B
E�B
F%B
F?B
FYB
FYB
FYB
F?B
G+B
F�B
GzB
G�B
G�B
G�B
G�B
G_B
G�B
G�B
HfB
H�B
IB
I7B
I7B
I�B
I�B
JXB
J�B
KDB
K�B
LJB
LJB
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
M�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
PbB
PbB
P}B
P�B
QhB
Q�B
RTB
RoB
RTB
R:B
R�B
R�B
TB
TFB
T{B
T�B
T�B
U�B
U�B
U�B
UgB
VB
V�B
V�B
V�B
W$B
W�B
W�B
W�B
W�B
W�B
W�B
XB
X_B
X�B
Y1B
YeB
Y�B
Y�B
Y�B
ZB
ZQB
Z�B
Z�B
Z�B
Z�B
[	B
[#B
[qB
[�B
\]B
\xB
\�B
\�B
\�B
\�B
]dB
^5B
^5B
^�B
^�B
^�B
_;B
_pB
`BB
`B
`'B
`�B
`�B
`�B
aHB
a|B
a�B
a�B
bB
bNB
bhB
b�B
b�B
b�B
cB
cTB
cnB
c�B
c�B
c�B
c�B
c�B
c�B
c�B
c�B
d&B
d@B
d@B
dZB
dZB
dtB
d�B
d�B
d�B
eB
eFB
e�B
e�B
fB
fB
fLB
ffB
f�B
f�B
gB
gB
gmB
g�B
g�B
h>B
hsB
h�B
h�B
h�B
h�B
i*B
i�B
i�B
jB
jKB
j0B
jKB
jeB
jB
j�B
j�B
kB
k�B
k�B
k�B
k�B
k�B
k�B
lB
l�B
lqB
l�B
l�B
l�B
mCB
m�B
m�B
m�B
nB
n/B
n/B
n}B
n}B
n�B
n�B
oB
oOB
oOB
o�B
o�B
p!B
poB
p�B
p�B
p�B
p�B
p�B
p�B
p�B
q'B
qvB
qvB
q�B
q�B
q�B
q�B
rB
r|B
raB
raB
rGB
raB
r�B
raB
raB
r�B
r�B
sB
sMB
s�B
s�B
t9B
tnB
t�B
tnB
tnB
tnB
t�B
t�B
uB
u%B
u�B
u�B
vB
vFB
v�B
v�B
w2B
wfB
w�B
w�B
x8B
xlB
xlB
xlB
x�B
x�B
x�B
x�B
y	B
yXB
y�B
yrB
y�B
y�B
y�B
y�B
y�B
y�B
y�B
zB
zDB
zxB
zxB
z�B
{B
z�B
{JB
{�B
{�B
{�B
{�B
{�B
{�B
|B
|6B
|jB
|�B
|�B
|�B
|�B
}B
}"B
}<B
}�B
}�B
}�B
~]B
~wB
~�B
~�B
~�B
.B
cB
�B
�B
�B
� B
�B
�B
�OB
�iB
��B
�B
� B
�UB
�UB
��B
��B
��B
��B
�AB
�[B
�uB
��B
��B
��B
��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   Bj�Bj�Bj�Bj�BkBkBkBk6BkQBk6Bk6Bj�BkBkBkBk6BkQBk6BkQBkkBkkBk�BkkBk�Bk�Bk�Bk�Bk�Bk�Bn}B�jB��B�DB	��B	��B
�JB
�,BY�B7BI�Bz�Bz�Bx�Bx�B�B��B��BуB�DB�)B�-B��B�EB��B�B��B�+B��B�B�B��BnIB^OBWYBI�B5�B.IB�BGB
��B
�=B
��B
{0B
shB
n�B
gRB
I�B
$�B
�B	�CB	��B	�B	|�B	u�B	k�B	U�B	?HB	"4B	�B	"B	B��B�CB�B�WB��B�B	�B	.cB	K�B	J�B	)DB	1�B	A�B	P�B	~BB	��B	�0B	׍B	�bB	��B	�B	�B	��B
tB
	7B
MB
"4B
'RB
0�B
6�B
A�B
K)B
S[B
X�B
ZkB
^5B
\B
N�B
K�B
L�B
PbB
O�B
M�B
I7B
EB
>B
.cB
 �B
SB
hB
�B
vB
�B
�B
�B
�B
tB
3B
�B	�.B
�B
B
	B
�B	�}B	��B	�B	��B	��B
 �B
 B	��B	�B	��B	߾B	�WB	�EB	�B	��B	�B	��B	��B	�B	��B
  B
)B
�B
�B
�B
4B
}B
�B
	B
�B
mB
?B
�B
�B
B
;B
&�B
-)B
-�B
-)B
,B
+QB
)yB
*0B
,�B
+QB
-]B
,qB
*�B
)*B
'�B
%B
#�B
$ZB
$�B
!�B
 �B
$@B
$�B
$�B
"NB
"4B
"B
#�B
"NB
"B
!�B
!|B
!B
 �B
�B
�B
=B
kB
�B
B
�B

rB

rB
�B
oB
�B
FB
,B
�B
�B
:B
�B
xB
KB
�B
%B
B
)B
�B
B
�B
B

	B
	�B
	�B
	RB
	B
fB
KB
�B
�B
?B
%B
B
�B
�B
�B
�B
MB
�B
3B
GB
-B
�B
�B
�B
uB
B
AB
uB
uB
�B
-B
�B
oB
 B
 4B	�cB	��B	�wB	��B	�B	��B	��B	�}B	��B	��B	�B	��B	�wB	��B	��B	�}B
  B	�.B	�.B	�cB	�cB	�B	��B	��B	��B
UB
�B
�B
�B
�B
mB
�B
�B
SB
�B
�B
�B
�B
B
-B
[B
 B
 �B
 �B
 �B
UB
�B
AB
�B
�B
aB
B
gB
3B
B
gB
B
�B
�B
mB
9B
9B
�B
�B
B
B
�B
{B
�B
�B
�B
MB
�B
B
�B
�B
�B
�B
�B
?B
YB
tB
YB
?B
YB
�B
�B
�B
B
B
B
�B
B
B
B
B
1B
�B
�B
�B
	B
	�B
	�B

	B

	B
	�B

	B
	�B

�B
)B
DB
DB
DB
^B
xB
xB
DB
^B
xB
�B
�B
B
0B
xB
~B
B
�B
�B
"B
�B
�B
�B
vB
�B
�B
B
�B
 B
B
4B
4B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
&B
�B
�B
�B
�B
�B
�B
�B
,B
FB
aB
�B
�B
�B
�B
�B
�B
2B
2B
�B
gB
�B
�B
�B
�B
B
�B
mB
SB
�B
�B
$B
$B

B

B
�B
�B
�B
�B
�B

B
�B
�B
�B
7B
	B
=B
qB
qB
qB
=B
WB
qB
�B
B
xB
�B
�B
B
~B
~B
�B
~B
�B
�B
B
~B
�B
B
B
5B
�B
B
OB
OB
OB
�B
�B
;B
�B
 BB
 BB
 BB
 �B
 'B
 \B
 �B
!-B
!HB
!�B
"�B
$tB
$�B
$�B
%B
%FB
%�B
%�B
%�B
%�B
%�B
&B
&fB
&2B
&LB
&fB
'mB
(
B
'�B
(XB
(�B
(�B
)B
)�B
)�B
)�B
*B
*�B
*KB
+B
+6B
+QB
+�B
,=B
,�B
,�B
,�B
-B
-B
-CB
-�B
.B
.}B
.�B
.�B
/B
/ B
/�B
0;B
0oB
0�B
1B
1'B
1�B
1�B
2|B
3�B
3�B
4B
4�B
5B
5ZB
5ZB
5�B
6FB
6`B
6`B
6�B
6�B
7B
6�B
7LB
7fB
7�B
7�B
7�B
8B
7�B
8B
8RB
8lB
8�B
8�B
8�B
9�B
9�B
:DB
:xB
;JB
;�B
;�B
<B
<�B
<�B
=VB
=VB
=�B
=�B
=�B
=�B
>B
>�B
>�B
?B
?cB
?�B
@�B
A B
A B
AUB
A;B
AUB
A�B
BAB
B[B
B[B
B'B
BAB
B�B
B�B
BAB
B�B
C�B
C�B
C�B
C�B
DgB
D�B
D�B
D�B
EB
E9B
EB
EB
E9B
EmB
EmB
E�B
E�B
E�B
F%B
F?B
FYB
FYB
FYB
F?B
G+B
F�B
GzB
G�B
G�B
G�B
G�B
G_B
G�B
G�B
HfB
H�B
IB
I7B
I7B
I�B
I�B
JXB
J�B
KDB
K�B
LJB
LJB
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
M�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
PbB
PbB
P}B
P�B
QhB
Q�B
RTB
RoB
RTB
R:B
R�B
R�B
TB
TFB
T{B
T�B
T�B
U�B
U�B
U�B
UgB
VB
V�B
V�B
V�B
W$B
W�B
W�B
W�B
W�B
W�B
W�B
XB
X_B
X�B
Y1B
YeB
Y�B
Y�B
Y�B
ZB
ZQB
Z�B
Z�B
Z�B
Z�B
[	B
[#B
[qB
[�B
\]B
\xB
\�B
\�B
\�B
\�B
]dB
^5B
^5B
^�B
^�B
^�B
_;B
_pB
`BB
`B
`'B
`�B
`�B
`�B
aHB
a|B
a�B
a�B
bB
bNB
bhB
b�B
b�B
b�B
cB
cTB
cnB
c�B
c�B
c�B
c�B
c�B
c�B
c�B
c�B
d&B
d@B
d@B
dZB
dZB
dtB
d�B
d�B
d�B
eB
eFB
e�B
e�B
fB
fB
fLB
ffB
f�B
f�B
gB
gB
gmB
g�B
g�B
h>B
hsB
h�B
h�B
h�B
h�B
i*B
i�B
i�B
jB
jKB
j0B
jKB
jeB
jB
j�B
j�B
kB
k�B
k�B
k�B
k�B
k�B
k�B
lB
l�B
lqB
l�B
l�B
l�B
mCB
m�B
m�B
m�B
nB
n/B
n/B
n}B
n}B
n�B
n�B
oB
oOB
oOB
o�B
o�B
p!B
poB
p�B
p�B
p�B
p�B
p�B
p�B
p�B
q'B
qvB
qvB
q�B
q�B
q�B
q�B
rB
r|B
raB
raB
rGB
raB
r�B
raB
raB
r�B
r�B
sB
sMB
s�B
s�B
t9B
tnB
t�B
tnB
tnB
tnB
t�B
t�B
uB
u%B
u�B
u�B
vB
vFB
v�B
v�B
w2B
wfB
w�B
w�B
x8B
xlB
xlB
xlB
x�B
x�B
x�B
x�B
y	B
yXB
y�B
yrB
y�B
y�B
y�B
y�B
y�B
y�B
y�B
zB
zDB
zxB
zxB
z�B
{B
z�B
{JB
{�B
{�B
{�B
{�B
{�B
{�B
|B
|6B
|jB
|�B
|�B
|�B
|�B
}B
}"B
}<B
}�B
}�B
}�B
~]B
~wB
~�B
~�B
~�B
.B
cB
�B
�B
�B
� B
�B
�B
�OB
�iB
��B
�B
� B
�UB
�UB
��B
��B
��B
��B
�AB
�[B
�uB
��B
��B
��B
��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105243  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604192358  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604192358  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604192359                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605042406  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605042406  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610161505                      G�O�G�O�G�O�                