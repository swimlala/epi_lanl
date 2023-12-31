CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:42:18Z creation;2022-06-04T17:42:19Z conversion to V3.1      
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
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20220604174218  20220610141505  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               mA   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @ٶ6gY =1   @ٶ6ؿ%�@.�r� Ĝ�c?t�j~�1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @,��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B ffB(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B���B�33B�  B���B�ffB�  B�  B�  B�  Bؙ�B���B���B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C�C�C  C  C  C�fC  C  C   C"  C$  C&  C(  C*  C+�fC-�fC0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL�CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ Dۼ�D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @(��@|(�@�{@�{A
=A?
=A_
=A
=A��A��A��A��AυA߅A�A��BBBB (�B'B/B7B?BGBOBWB_BgBoBwBB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB�z�B�z�B�{B��HB�z�B�G�B��HB��HB��HB��HB�z�BۮB߮B��HB��HB��HB��HB��HB��HB��HB��HC�C�C�C�C	�C�C�C
>C
>C�C�C�C�
C�C�C�C!�C#�C%�C'�C)�C+�
C-�
C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CL
>CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RD |)D �)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D	|)D	�)D
|)D
�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�D|)D�)D|)D�)D |)D �)D!|)D!�)D"|)D"�)D#|)D#�)D$|)D$�)D%|)D%�)D&|)D&�)D'|)D'�)D(|)D(�)D)|)D)�)D*|)D*�)D+|)D+�)D,|)D,�)D-|)D-�)D.|)D.�)D/|)D/�)D0|)D0�)D1|)D1�)D2|)D2�)D3|)D3�)D4|)D4�)D5|)D5�)D6|)D6�)D7|)D7�)D8|)D8�)D9|)D9�)D:|)D:�)D;|)D;�)D<|)D<�)D=|)D=�)D>|)D>�)D?|)D?�)D@|)D@�)DA|)DA�)DB|)DB�)DC|)DC�)DD|)DD�)DE|)DE�)DF|)DF�)DG|)DG�)DH|)DH�)DI|)DI�)DJ|)DJ�)DK|)DK�)DL|)DL�)DM|)DM�)DN|)DN�)DO|)DO�)DP|)DP�)DQ|)DQ�)DR|)DR�)DS|)DS�)DT|)DT�)DU|)DU�)DV|)DV�)DW|)DW�)DX|)DX�)DY|)DY�)DZ|)DZ�)D[|)D[�)D\|)D\�)D]|)D]�)D^|)D^�)D_|)D_�)D`|)D`�)Da|)Da�)Db|)Db�)Dc|)Dc�)Dd|)Dd�)De|)De�)Df|)Df�)Dg|)Dg�)Dh|)Dh�)Di|)Di�)Dj|)Dj�)Dk|)Dk�)Dl|)Dl�)Dm|)Dm�)Dn|)Dn�)Do|)Do�)Dp|)Dp�)Dq|)Dq�)Dr|)Dr�)Ds|)Ds�)Dt|)Dt�)Du|)Du�)Dv|)Dv�)Dw|)Dw�)Dx|)Dx�)Dy|)Dy�)Dz|)Dz�)D{|)D{�)D||)D|�)D}|)D}�)D~|)D~�)D|)D�)D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�z�D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D¾D��D�>D�~DþD��D�>D�~DľD��D�>D�~DžD��D�>D�~DƾD��D�>D�~DǾD��D�>D�~DȾD��D�>D�~DɾD��D�>D�~DʾD��D�>D�~D˾D��D�>D�~D̾D��D�>D�~D;D��D�>D�~DξD��D�>D�~DϾD��D�>D�~DоD��D�>D�~DѾD��D�>D�~DҾD��D�>D�~DӾD��D�>D�~DԾD��D�>D�~DվD��D�>D�~D־D��D�>D�~D׾D��D�>D�~DؾD��D�>D�~DپD��D�>D�~DھD��D�>D�~Dۺ�D��D�>D�~DܾD��D�>D�~DݾD��D�>D�~D޾D��D�>D�~D߾D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D��D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�z�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  AΫ�AΩ�AζzAΫ�AΩ�AΦAΙeAΘ�AΘ+AΘ�AΕMAΗ$AΖ�AΕ�AΕMAΕ�AΖSAΖ�AΗ�AΘ+AΙ1AΚ�AΚkAΚ�AΛ=AΜxAΝAΜ�AΜCAΝ�AΞAΝAΝ�AΞ�AΠ'AΠ�AΡ-AΟ!AΠ�AΝAΘ�A�D�A�;�Aɦ�A��A���A��dA��A���A���A�tA�e�A�+�A���A�K^A�_A�E�A�($A�~�A��A�R A�h�A|�	Ay�Av�+Aub�As� An�Aj�Ag33Ad�6Aa�A^��A\�pA\1�AY��AU�|AQR�AO��AN_AK�VAJRTAH˒AE�AC�ABݘAB�ABD�AA�A@r�A;�TA;=A9i�A7�A7,�A7qvA77�A5ںA3��A2-wA0g�A.(�A+��A)U�A%��A$��A"%A W?A�A�9A�A5?A~(A�A�UA#:A^�AVAu�Ag�A�XA�A�}A��A�DAݘA�A�A� A��Ax�Al�A��A?}AیA��A��A�A��A��AZ�AVmA��A?A��AA��A�kAt�A�A��A��A�OA�OA��AO�A��AGEAc�A��A8�A�A�A
��A
|A	�}A	W?A�AI�A�AH�A�9A��AQ�AX�A��AGA�sA�@AF�A��A��Aw2A$�Ac A �@�خ@���@��@�/�@�(@��r@��@�B�@��p@�z�@�#:@�A @�!�@�Q�@��V@��@��
@��Z@�O@�A�@��&@�Dg@��@�/�@��|@�ѷ@�=�@�F@�o@�l"@�_@�N<@��@��2@�g8@�q@��m@��@捹@�P@�o�@�(�@��@�]d@�  @�.I@��M@�z@��@�P@�+@��@���@��@ߙ�@�,�@��,@ް!@�PH@���@���@ݖS@�qv@�*0@��?@�_�@��@۬q@�H�@��@��/@څ�@�`�@�Z@�b@��)@�zx@ؿ�@��o@�j@��M@��[@ֽ<@��@�a|@���@�_p@��|@Բ�@ԛ�@�z@�+k@��@�zx@��@Ҁ�@�*�@��@�p�@��@Ь�@�$@��6@�:�@���@�*�@�Vm@��y@�kQ@��@˶F@�@@��@��@Ȁ�@Ȍ�@�@��@�zx@�8�@�>�@Ĵ9@�@�6z@��@o@�9X@���@�o @�-w@��9@�{@�خ@�.I@�?�@��@���@��z@�@�@���@��@��H@�f�@�S@��m@�oi@���@�_p@�IR@��@�B[@���@�y�@��@��R@�g8@�I�@�_@���@�?}@��@���@��A@��@�@��'@�P�@���@��,@���@��4@�Z�@��@���@�Q�@�ѷ@���@�a|@�E�@�#:@��@���@�@��@��@���@�Y�@�͟@��+@�*�@��@�L�@��[@�N�@���@�=�@���@�[�@��@�t�@�'�@���@�,=@��@@�.I@���@��@�!�@���@��@�?�@���@���@�Vm@��@��P@��@��)@�h�@� �@�_@�>B@�ԕ@��[@���@�33@�@��@�֡@�R�@���@��X@���@�zx@�<6@��@��A@��a@��3@���@���@�-w@�(@��@���@��x@�kQ@�A�@�H�@�?�@���@�L�@��5@��L@�m�@�2�@���@��4@�B�@��@���@���@���@�h�@�(�@���@�u�@�b�@�+�@��]@��_@�M@�G@���@��W@��@���@�a�@�	l@���@�ȴ@���@�r�@�)�@���@���@�6z@��K@��'@�~(@�GE@�1@��g@���@�_p@�H�@�@���@���@���@�[�@��@���@�c@�;d@���@�҉@���@�$@���@�x�@�\�@�*0@�
=@��@��u@� �@��A@��-@�zx@�F@�(�@��@�ی@�w�@�$�@��@�iD@���@��D@�5?@�O@��@��W@���@��~@�y�@�l�@�T�@��@���@��@�\�@�)�@�@��@��@���@��M@�IR@�(�@�
=@�;@��@���@���@�7�@���@���@�hs@�K�@�,�@�C@�Y@�Ĝ@�q�@�Q@P�@~H�@}�@}^�@|`�@|$@|b@{��@{9�@z��@z	@y�t@yX@y%@x�@x1'@w�V@w8@v�@v��@v;�@u�d@ux�@u<6@u@t�@t��@t �@s]�@r��@rs�@r
�@qc�@pی@p�O@pg8@o��@o1�@n��@n�\@na|@n-@n	@mm]@l��@lb@k�Q@k�@kY@j�]@j�!@j�A@jQ@j
�@i��@i��@if�@i�@h�o@h	�@g�q@gW?@f�@f��@fH�@e��@eVm@e�@d��@d-�@c��@c�@b��@b�@b��@a�#@a��@a��@a/@`<�@_�@_�k@_�@_e�@_�@^�,@^n�@]��@]�M@]Vm@]Dg@]@\ی@\K^@\ �@\@[�+@[,�@Zl�@Y�@YQ�@Y�@X��@X�?@X�e@X`�@X@W��@W=@V��@VGE@U�@U[W@Tѷ@T1'@S��@SdZ@SU�@S�@R��@R�\@Rv�@RGE@R�@Q�@Q��@P�v@PM@O��@O1�@N�"@N͟@N�\@N��@NB[@Nu@M�@Mc@M2a@L�@L�e@L[�@L�@K��@K�V@Kb�@K�@J�@Jq�@J3�@J($@J�@I��@H�|@Hr�@H%�@HG@G�m@G�:@Gg�@G�@F��@F&�@E�@E��@E}�@EF@D�	@D�O@C�[@CX�@B�@B�A@Aԕ@Aw2@A:�@@��@@��@@9X@?�@?�q@?��@?]�@?4�@?�@>��@=�@=��@=Vm@=V@<�z@<��@<u�@<g8@<I�@;ݘ@;]�@;6z@;"�@;"�@;�@:�R@:-@:u@9�C@9`B@9(�@8�`@8Ĝ@8�$@8Xy@8  @7� @7��@7X�@7�@6�@6s�@6.�@5��@5p�@4��@4��@4��@4y>@4PH@4(�@3��@3��@3�4@38@2��@2h
@1��@1��@1m]@1�@0ی@0�j@0c�@/��@/�{@/8@.��@.l�@-�.@-�j@-�X@-B�@,�f@,��@,�O@,�4@,��@,-�@+�r@+�[@+|�@+S�@+,�@+@*�c@*ں@*��@*�@*� @*h
@)��@)��@)}�@)L�@)0�@)�@(�`@(Q�@'�@'�@'x@'g�@'X�@'Mj@'J#@'9�@'&@&v�@&\�@&L0@&�@%�@%�@$��@$��@$`�@$D�@$6@$*�@$%�@$M@#�@#� @#��@#��@#x@#S@"�@"q�@!�@!�@!�X@!w2@!T�@!�@ �	@ ֡@ ��@ ��@ �@ `�@ -�@ �@ �@�;@��@E9@��@�r@a|@ff@h
@W�@!�@J@@��@�T@�@�f@��@��@�z@q@<�@�@��@=@�X@�1@xl@kQ@W�@C�@e@@�@��@ϫ@��@@��@zx@@��@��@j@!@�+@�@qv@�@҉@^5@�@u@�@��@hs@5�@%F@@ی@u�@Z@D�@4n@@�@�W@�a@��@n/@"�@(@��@��@��@E�@.�@��@�3@}�@?}@�@%@��@�K@�`@�?@��@oi@2�@�@��@{J@;d@S@�"@��@d�@)�@��@��@G�@%F@@�@h�@C-@ �@�@�@�r@��@�0@��@��@>�@�@S@
͟@
�1@
��@
��@
~�@
V@
0U@

�@	��@	�N@	��@	p�@	\�@	O�@	B�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  AΫ�AΩ�AζzAΫ�AΩ�AΦAΙeAΘ�AΘ+AΘ�AΕMAΗ$AΖ�AΕ�AΕMAΕ�AΖSAΖ�AΗ�AΘ+AΙ1AΚ�AΚkAΚ�AΛ=AΜxAΝAΜ�AΜCAΝ�AΞAΝAΝ�AΞ�AΠ'AΠ�AΡ-AΟ!AΠ�AΝAΘ�A�D�A�;�Aɦ�A��A���A��dA��A���A���A�tA�e�A�+�A���A�K^A�_A�E�A�($A�~�A��A�R A�h�A|�	Ay�Av�+Aub�As� An�Aj�Ag33Ad�6Aa�A^��A\�pA\1�AY��AU�|AQR�AO��AN_AK�VAJRTAH˒AE�AC�ABݘAB�ABD�AA�A@r�A;�TA;=A9i�A7�A7,�A7qvA77�A5ںA3��A2-wA0g�A.(�A+��A)U�A%��A$��A"%A W?A�A�9A�A5?A~(A�A�UA#:A^�AVAu�Ag�A�XA�A�}A��A�DAݘA�A�A� A��Ax�Al�A��A?}AیA��A��A�A��A��AZ�AVmA��A?A��AA��A�kAt�A�A��A��A�OA�OA��AO�A��AGEAc�A��A8�A�A�A
��A
|A	�}A	W?A�AI�A�AH�A�9A��AQ�AX�A��AGA�sA�@AF�A��A��Aw2A$�Ac A �@�خ@���@��@�/�@�(@��r@��@�B�@��p@�z�@�#:@�A @�!�@�Q�@��V@��@��
@��Z@�O@�A�@��&@�Dg@��@�/�@��|@�ѷ@�=�@�F@�o@�l"@�_@�N<@��@��2@�g8@�q@��m@��@捹@�P@�o�@�(�@��@�]d@�  @�.I@��M@�z@��@�P@�+@��@���@��@ߙ�@�,�@��,@ް!@�PH@���@���@ݖS@�qv@�*0@��?@�_�@��@۬q@�H�@��@��/@څ�@�`�@�Z@�b@��)@�zx@ؿ�@��o@�j@��M@��[@ֽ<@��@�a|@���@�_p@��|@Բ�@ԛ�@�z@�+k@��@�zx@��@Ҁ�@�*�@��@�p�@��@Ь�@�$@��6@�:�@���@�*�@�Vm@��y@�kQ@��@˶F@�@@��@��@Ȁ�@Ȍ�@�@��@�zx@�8�@�>�@Ĵ9@�@�6z@��@o@�9X@���@�o @�-w@��9@�{@�خ@�.I@�?�@��@���@��z@�@�@���@��@��H@�f�@�S@��m@�oi@���@�_p@�IR@��@�B[@���@�y�@��@��R@�g8@�I�@�_@���@�?}@��@���@��A@��@�@��'@�P�@���@��,@���@��4@�Z�@��@���@�Q�@�ѷ@���@�a|@�E�@�#:@��@���@�@��@��@���@�Y�@�͟@��+@�*�@��@�L�@��[@�N�@���@�=�@���@�[�@��@�t�@�'�@���@�,=@��@@�.I@���@��@�!�@���@��@�?�@���@���@�Vm@��@��P@��@��)@�h�@� �@�_@�>B@�ԕ@��[@���@�33@�@��@�֡@�R�@���@��X@���@�zx@�<6@��@��A@��a@��3@���@���@�-w@�(@��@���@��x@�kQ@�A�@�H�@�?�@���@�L�@��5@��L@�m�@�2�@���@��4@�B�@��@���@���@���@�h�@�(�@���@�u�@�b�@�+�@��]@��_@�M@�G@���@��W@��@���@�a�@�	l@���@�ȴ@���@�r�@�)�@���@���@�6z@��K@��'@�~(@�GE@�1@��g@���@�_p@�H�@�@���@���@���@�[�@��@���@�c@�;d@���@�҉@���@�$@���@�x�@�\�@�*0@�
=@��@��u@� �@��A@��-@�zx@�F@�(�@��@�ی@�w�@�$�@��@�iD@���@��D@�5?@�O@��@��W@���@��~@�y�@�l�@�T�@��@���@��@�\�@�)�@�@��@��@���@��M@�IR@�(�@�
=@�;@��@���@���@�7�@���@���@�hs@�K�@�,�@�C@�Y@�Ĝ@�q�@�Q@P�@~H�@}�@}^�@|`�@|$@|b@{��@{9�@z��@z	@y�t@yX@y%@x�@x1'@w�V@w8@v�@v��@v;�@u�d@ux�@u<6@u@t�@t��@t �@s]�@r��@rs�@r
�@qc�@pی@p�O@pg8@o��@o1�@n��@n�\@na|@n-@n	@mm]@l��@lb@k�Q@k�@kY@j�]@j�!@j�A@jQ@j
�@i��@i��@if�@i�@h�o@h	�@g�q@gW?@f�@f��@fH�@e��@eVm@e�@d��@d-�@c��@c�@b��@b�@b��@a�#@a��@a��@a/@`<�@_�@_�k@_�@_e�@_�@^�,@^n�@]��@]�M@]Vm@]Dg@]@\ی@\K^@\ �@\@[�+@[,�@Zl�@Y�@YQ�@Y�@X��@X�?@X�e@X`�@X@W��@W=@V��@VGE@U�@U[W@Tѷ@T1'@S��@SdZ@SU�@S�@R��@R�\@Rv�@RGE@R�@Q�@Q��@P�v@PM@O��@O1�@N�"@N͟@N�\@N��@NB[@Nu@M�@Mc@M2a@L�@L�e@L[�@L�@K��@K�V@Kb�@K�@J�@Jq�@J3�@J($@J�@I��@H�|@Hr�@H%�@HG@G�m@G�:@Gg�@G�@F��@F&�@E�@E��@E}�@EF@D�	@D�O@C�[@CX�@B�@B�A@Aԕ@Aw2@A:�@@��@@��@@9X@?�@?�q@?��@?]�@?4�@?�@>��@=�@=��@=Vm@=V@<�z@<��@<u�@<g8@<I�@;ݘ@;]�@;6z@;"�@;"�@;�@:�R@:-@:u@9�C@9`B@9(�@8�`@8Ĝ@8�$@8Xy@8  @7� @7��@7X�@7�@6�@6s�@6.�@5��@5p�@4��@4��@4��@4y>@4PH@4(�@3��@3��@3�4@38@2��@2h
@1��@1��@1m]@1�@0ی@0�j@0c�@/��@/�{@/8@.��@.l�@-�.@-�j@-�X@-B�@,�f@,��@,�O@,�4@,��@,-�@+�r@+�[@+|�@+S�@+,�@+@*�c@*ں@*��@*�@*� @*h
@)��@)��@)}�@)L�@)0�@)�@(�`@(Q�@'�@'�@'x@'g�@'X�@'Mj@'J#@'9�@'&@&v�@&\�@&L0@&�@%�@%�@$��@$��@$`�@$D�@$6@$*�@$%�@$M@#�@#� @#��@#��@#x@#S@"�@"q�@!�@!�@!�X@!w2@!T�@!�@ �	@ ֡@ ��@ ��@ �@ `�@ -�@ �@ �@�;@��@E9@��@�r@a|@ff@h
@W�@!�@J@@��@�T@�@�f@��@��@�z@q@<�@�@��@=@�X@�1@xl@kQ@W�@C�@e@@�@��@ϫ@��@@��@zx@@��@��@j@!@�+@�@qv@�@҉@^5@�@u@�@��@hs@5�@%F@@ی@u�@Z@D�@4n@@�@�W@�a@��@n/@"�@(@��@��@��@E�@.�@��@�3@}�@?}@�@%@��@�K@�`@�?@��@oi@2�@�@��@{J@;d@S@�"@��@d�@)�@��@��@G�@%F@@�@h�@C-@ �@�@�@�r@��@�0@��@��@>�@�@S@
͟@
�1@
��@
��@
~�@
V@
0U@

�@	��@	�N@	��@	p�@	\�@	O�@	B�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B��B��B��B��B�~B��B�IB�IB�IB�dB�~B�~B��B�~B��B��B��B��B��B��B��B��B��B��B��B�B��B�;B�VB�pB�pB�VB�VB�B��B��B�pB��B�pB�'B��B��B�mB	7B
�B
k�B
��B
��B
�6B
��B
��B
��B
��B
q'B
X�B
L�B
8�B
# B

	B	�`B	��B	یB	�B	��B	��B	�!B	�mB	��B	m)B	]�B	QNB	C{B	4�B	.}B	*KB	�B	�B�TB�qB��B�QB��B�lB��B�kB�B��B	�B	�B	NB	PB	�B	�B	�B	�B	!�B	BB	L�B	C�B	9$B	4TB	/OB	)yB	7B�HB��B�BܬB��B��BȚB��B�B�BB��B�?B�HB�B��B��B�BB�B�SB��B�%B�B�B��B�!B�<B��B	aB	B	O�B	R�B	J�B	L0B	q�B	t9B	mB	|�B	�B	�KB	��B	�mB	��B	�B	��B	�RB	��B	��B	��B	�-B	żB	�?B	ƨB	�B	�AB	��B	��B	��B	��B	�{B	�-B	ðB	��B	��B	�OB	��B	��B	�6B	̈́B	�B	�JB	̘B	��B	̳B	�PB	οB	��B	��B	бB	�}B	��B	��B	ϑB	��B	��B	�B	�fB	�)B	�VB	��B	�MB	��B	�EB	ٚB	�+B	�QB	��B	�8B	�B	�qB	յB	�]B	� B	�@B	�B	�B	�B	��B	�HB	�B	�@B	��B	�8B	�mB	�B	�B	�fB	�LB	�B	�`B	��B	�,B	�,B	�B	�B	��B	��B	�B	��B	�B	�B	��B	��B	��B	�B	�WB	�B	��B	�B	��B	��B	�B	�wB	�wB	��B	��B	�B	�;B	�'B	��B	�aB	�hB	�B	�nB	��B	�9B	�B	�9B	��B	�B	��B	�9B	��B	�B	�MB	�3B	�B	��B	�B	�B	�B	�B	�MB	�3B	�hB	�3B	�B	�B	�B	�B	�B	�B	�TB	��B	��B	�%B	�?B	�B	�9B	��B	��B	�9B	�B	�B	�B	��B	��B	��B	�B	�B	��B	�B	�B	�B	�vB	��B	�B	�MB	�B	��B	��B	�2B	�B	�fB	��B	�+B	�%B	�nB	�FB	�LB	��B	�RB	�$B	�>B	�^B	�B	��B	��B	�"B	��B	��B	�VB	��B	�BB	�B	�B	�}B	��B	��B	�}B	��B	��B
  B
 �B
 �B
 OB
 OB
 4B
 B
 4B
 B
 OB
 4B
 �B
 �B
 �B
 B
 B
UB
AB
[B
[B
�B
�B
�B
GB
�B
aB
-B
aB
�B
�B
�B
B
gB
-B
[B
;B
B
 �B
 iB
 B
 4B
 iB
 OB
 OB	��B	�HB	��B
 �B
 4B
 iB
 �B
 B
�B
�B
�B
B
MB
�B
9B
B
�B
%B
	lB
	�B
	B

	B
	�B

rB
)B
�B

�B
)B
�B
B
PB
B
B
�B
dB
�B
HB
�B
�B
�B
�B
�B
�B
oB
�B
,B
B
�B
�B
mB
mB
�B
YB
�B
�B
1B
1B
1B
�B
�B
�B
B
eB
B
qB
�B
)B
)B
)B
]B
�B
xB
�B
�B
/B
dB
B
IB
~B
�B
5B
�B
�B
!B
pB
pB
�B
�B
 \B
 �B
 �B
!-B
 �B
!bB
!�B
!�B
!�B
!�B
"B
"NB
"NB
"�B
"�B
#B
#:B
#�B
$&B
$&B
$@B
$tB
$�B
$�B
%,B
&2B
&2B
&�B
'B
'8B
'8B
'RB
'mB
(>B
(sB
(�B
)*B
*�B
+B
*�B
*�B
+�B
+�B
,qB
,�B
,�B
,�B
,�B
,�B
,WB
,"B
,=B
,"B
,"B
,B
,B
,WB
,�B
-)B
-]B
-�B
-�B
.cB
/OB
/OB
0!B
1�B
1�B
1�B
1�B
1�B
1vB
2B
2�B
2�B
3B
3B
3hB
3MB
3hB
49B
4B
49B
4�B
4�B
5B
5tB
5?B
5ZB
5tB
5�B
5�B
6FB
6FB
6�B
6zB
6�B
7LB
8B
8B
8B
8�B
9>B
9�B
:*B
9�B
:B
9�B
:DB
:xB
:xB
:�B
:�B
;JB
;JB
;JB
;B
;�B
<6B
<�B
<jB
<�B
<�B
<�B
=<B
=qB
=�B
>B
>(B
>wB
>wB
>wB
>�B
>�B
?}B
?�B
@ B
@ B
@OB
@OB
@iB
AB
AB
A;B
AUB
A�B
A�B
B�B
B�B
B�B
B�B
CaB
C{B
CGB
C�B
DgB
D�B
D�B
D�B
D�B
EB
EB
E�B
E�B
FYB
FYB
FYB
F�B
F�B
G+B
G+B
GB
F�B
G�B
HB
H�B
IB
I7B
IB
IRB
I7B
IlB
I�B
I�B
JrB
J�B
J�B
K)B
KxB
K�B
LJB
L�B
L�B
L�B
MB
MPB
MPB
MjB
M�B
M�B
M�B
M�B
N�B
N�B
OB
OvB
OvB
O�B
O�B
O�B
PB
PHB
P}B
PbB
P�B
P�B
Q B
Q4B
QhB
Q�B
Q�B
Q�B
Q�B
R B
RoB
R�B
R�B
R�B
R�B
S�B
S�B
TB
T,B
T,B
T{B
T�B
T�B
UB
U�B
U�B
U�B
U�B
VB
V9B
V9B
W
B
W$B
WsB
W�B
XEB
X_B
X�B
YKB
Y�B
Y�B
Y�B
ZB
ZB
Y�B
Y�B
ZB
ZkB
[=B
[�B
[�B
\B
\xB
\xB
\]B
\]B
\xB
]B
]IB
]dB
]IB
]/B
]IB
]�B
]�B
^B
^5B
^�B
^�B
^�B
^�B
^�B
_VB
_pB
_�B
_�B
_�B
`BB
`�B
`�B
a-B
aB
a�B
b4B
bNB
bhB
b�B
b�B
b�B
bhB
b�B
b�B
b�B
c B
c:B
c�B
c�B
cnB
c�B
c�B
c�B
dB
dZB
d�B
d�B
d�B
eFB
e�B
e�B
fB
f�B
f�B
f�B
f�B
f�B
f�B
f�B
gB
gmB
g�B
h
B
h>B
hsB
h�B
h�B
h�B
h�B
h�B
h�B
i_B
i�B
i�B
i�B
i�B
i�B
i�B
jB
j�B
kB
kB
kB
k6B
k6B
kB
k6B
kB
lB
k�B
k�B
lB
lWB
l�B
l�B
m)B
mwB
m]B
m�B
mwB
m�B
m�B
m�B
m�B
m�B
nB
m�B
n}B
nB
n�B
o5B
oB
oOB
oiB
oiB
o�B
o�B
pB
o�B
pB
p;B
p!B
poB
p�B
p�B
p�B
p�B
qB
q[B
q�B
q�B
q�B
q�B
q�B
rB
r-B
rB
rB
rB
r�B
r�B
sB
r�B
sMB
sMB
shB
s�B
s�B
tB
t�B
t�B
uB
uB
uB
u%B
u?B
uZB
uZB
utB
utB
u�B
u�B
u�B
u�B
vB
vFB
vzB
v�B
v�B
v�B
w2B
wfB
w�B
xB
xlB
x�B
x�B
x�B
y$B
yXB
y�B
y�B
y�B
y�B
zB
zDB
z*B
z^B
zxB
z�B
zxB
z�B
z�B
{B
{JB
{JB
{JB
{JB
{B
{�B
{B
{�B
{�B
|B
|PB
|jB
|jB
|jB
|jB
|�B
|�B
}"B
}�B
}�B
}�B
}�B
}�B
~B
~]B
~BB
~�B
~�B
~�B
~�B
HB
}B
�B
�B
�B
�OB
�OB
�iB
��B
�iB
��B
��B
��B
��B
��B
��B
��B
�oB
��B
�'B
�'B
�'B
�'B
�[B
�uB
��B
��B
��B
��B
�{B
��B
��B
��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B��B��B��B��B�~B��B�IB�IB�IB�dB�~B�~B��B�~B��B��B��B��B��B��B��B��B��B��B��B�B��B�;B�VB�pB�pB�VB�VB�B��B��B�pB��B�pB�'B��B��B�mB	7B
�B
k�B
��B
��B
�6B
��B
��B
��B
��B
q'B
X�B
L�B
8�B
# B

	B	�`B	��B	یB	�B	��B	��B	�!B	�mB	��B	m)B	]�B	QNB	C{B	4�B	.}B	*KB	�B	�B�TB�qB��B�QB��B�lB��B�kB�B��B	�B	�B	NB	PB	�B	�B	�B	�B	!�B	BB	L�B	C�B	9$B	4TB	/OB	)yB	7B�HB��B�BܬB��B��BȚB��B�B�BB��B�?B�HB�B��B��B�BB�B�SB��B�%B�B�B��B�!B�<B��B	aB	B	O�B	R�B	J�B	L0B	q�B	t9B	mB	|�B	�B	�KB	��B	�mB	��B	�B	��B	�RB	��B	��B	��B	�-B	żB	�?B	ƨB	�B	�AB	��B	��B	��B	��B	�{B	�-B	ðB	��B	��B	�OB	��B	��B	�6B	̈́B	�B	�JB	̘B	��B	̳B	�PB	οB	��B	��B	бB	�}B	��B	��B	ϑB	��B	��B	�B	�fB	�)B	�VB	��B	�MB	��B	�EB	ٚB	�+B	�QB	��B	�8B	�B	�qB	յB	�]B	� B	�@B	�B	�B	�B	��B	�HB	�B	�@B	��B	�8B	�mB	�B	�B	�fB	�LB	�B	�`B	��B	�,B	�,B	�B	�B	��B	��B	�B	��B	�B	�B	��B	��B	��B	�B	�WB	�B	��B	�B	��B	��B	�B	�wB	�wB	��B	��B	�B	�;B	�'B	��B	�aB	�hB	�B	�nB	��B	�9B	�B	�9B	��B	�B	��B	�9B	��B	�B	�MB	�3B	�B	��B	�B	�B	�B	�B	�MB	�3B	�hB	�3B	�B	�B	�B	�B	�B	�B	�TB	��B	��B	�%B	�?B	�B	�9B	��B	��B	�9B	�B	�B	�B	��B	��B	��B	�B	�B	��B	�B	�B	�B	�vB	��B	�B	�MB	�B	��B	��B	�2B	�B	�fB	��B	�+B	�%B	�nB	�FB	�LB	��B	�RB	�$B	�>B	�^B	�B	��B	��B	�"B	��B	��B	�VB	��B	�BB	�B	�B	�}B	��B	��B	�}B	��B	��B
  B
 �B
 �B
 OB
 OB
 4B
 B
 4B
 B
 OB
 4B
 �B
 �B
 �B
 B
 B
UB
AB
[B
[B
�B
�B
�B
GB
�B
aB
-B
aB
�B
�B
�B
B
gB
-B
[B
;B
B
 �B
 iB
 B
 4B
 iB
 OB
 OB	��B	�HB	��B
 �B
 4B
 iB
 �B
 B
�B
�B
�B
B
MB
�B
9B
B
�B
%B
	lB
	�B
	B

	B
	�B

rB
)B
�B

�B
)B
�B
B
PB
B
B
�B
dB
�B
HB
�B
�B
�B
�B
�B
�B
oB
�B
,B
B
�B
�B
mB
mB
�B
YB
�B
�B
1B
1B
1B
�B
�B
�B
B
eB
B
qB
�B
)B
)B
)B
]B
�B
xB
�B
�B
/B
dB
B
IB
~B
�B
5B
�B
�B
!B
pB
pB
�B
�B
 \B
 �B
 �B
!-B
 �B
!bB
!�B
!�B
!�B
!�B
"B
"NB
"NB
"�B
"�B
#B
#:B
#�B
$&B
$&B
$@B
$tB
$�B
$�B
%,B
&2B
&2B
&�B
'B
'8B
'8B
'RB
'mB
(>B
(sB
(�B
)*B
*�B
+B
*�B
*�B
+�B
+�B
,qB
,�B
,�B
,�B
,�B
,�B
,WB
,"B
,=B
,"B
,"B
,B
,B
,WB
,�B
-)B
-]B
-�B
-�B
.cB
/OB
/OB
0!B
1�B
1�B
1�B
1�B
1�B
1vB
2B
2�B
2�B
3B
3B
3hB
3MB
3hB
49B
4B
49B
4�B
4�B
5B
5tB
5?B
5ZB
5tB
5�B
5�B
6FB
6FB
6�B
6zB
6�B
7LB
8B
8B
8B
8�B
9>B
9�B
:*B
9�B
:B
9�B
:DB
:xB
:xB
:�B
:�B
;JB
;JB
;JB
;B
;�B
<6B
<�B
<jB
<�B
<�B
<�B
=<B
=qB
=�B
>B
>(B
>wB
>wB
>wB
>�B
>�B
?}B
?�B
@ B
@ B
@OB
@OB
@iB
AB
AB
A;B
AUB
A�B
A�B
B�B
B�B
B�B
B�B
CaB
C{B
CGB
C�B
DgB
D�B
D�B
D�B
D�B
EB
EB
E�B
E�B
FYB
FYB
FYB
F�B
F�B
G+B
G+B
GB
F�B
G�B
HB
H�B
IB
I7B
IB
IRB
I7B
IlB
I�B
I�B
JrB
J�B
J�B
K)B
KxB
K�B
LJB
L�B
L�B
L�B
MB
MPB
MPB
MjB
M�B
M�B
M�B
M�B
N�B
N�B
OB
OvB
OvB
O�B
O�B
O�B
PB
PHB
P}B
PbB
P�B
P�B
Q B
Q4B
QhB
Q�B
Q�B
Q�B
Q�B
R B
RoB
R�B
R�B
R�B
R�B
S�B
S�B
TB
T,B
T,B
T{B
T�B
T�B
UB
U�B
U�B
U�B
U�B
VB
V9B
V9B
W
B
W$B
WsB
W�B
XEB
X_B
X�B
YKB
Y�B
Y�B
Y�B
ZB
ZB
Y�B
Y�B
ZB
ZkB
[=B
[�B
[�B
\B
\xB
\xB
\]B
\]B
\xB
]B
]IB
]dB
]IB
]/B
]IB
]�B
]�B
^B
^5B
^�B
^�B
^�B
^�B
^�B
_VB
_pB
_�B
_�B
_�B
`BB
`�B
`�B
a-B
aB
a�B
b4B
bNB
bhB
b�B
b�B
b�B
bhB
b�B
b�B
b�B
c B
c:B
c�B
c�B
cnB
c�B
c�B
c�B
dB
dZB
d�B
d�B
d�B
eFB
e�B
e�B
fB
f�B
f�B
f�B
f�B
f�B
f�B
f�B
gB
gmB
g�B
h
B
h>B
hsB
h�B
h�B
h�B
h�B
h�B
h�B
i_B
i�B
i�B
i�B
i�B
i�B
i�B
jB
j�B
kB
kB
kB
k6B
k6B
kB
k6B
kB
lB
k�B
k�B
lB
lWB
l�B
l�B
m)B
mwB
m]B
m�B
mwB
m�B
m�B
m�B
m�B
m�B
nB
m�B
n}B
nB
n�B
o5B
oB
oOB
oiB
oiB
o�B
o�B
pB
o�B
pB
p;B
p!B
poB
p�B
p�B
p�B
p�B
qB
q[B
q�B
q�B
q�B
q�B
q�B
rB
r-B
rB
rB
rB
r�B
r�B
sB
r�B
sMB
sMB
shB
s�B
s�B
tB
t�B
t�B
uB
uB
uB
u%B
u?B
uZB
uZB
utB
utB
u�B
u�B
u�B
u�B
vB
vFB
vzB
v�B
v�B
v�B
w2B
wfB
w�B
xB
xlB
x�B
x�B
x�B
y$B
yXB
y�B
y�B
y�B
y�B
zB
zDB
z*B
z^B
zxB
z�B
zxB
z�B
z�B
{B
{JB
{JB
{JB
{JB
{B
{�B
{B
{�B
{�B
|B
|PB
|jB
|jB
|jB
|jB
|�B
|�B
}"B
}�B
}�B
}�B
}�B
}�B
~B
~]B
~BB
~�B
~�B
~�B
~�B
HB
}B
�B
�B
�B
�OB
�OB
�iB
��B
�iB
��B
��B
��B
��B
��B
��B
��B
�oB
��B
�'B
�'B
�'B
�'B
�[B
�uB
��B
��B
��B
��B
�{B
��B
��B
��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104929  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604174218  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604174219  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604174219                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605024226  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605024226  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610141505                      G�O�G�O�G�O�                