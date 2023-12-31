CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-04-17T00:35:37Z creation;2018-04-17T00:35:42Z conversion to V3.1;2019-12-19T07:40:32Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7T   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     88   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8X   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8\   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8d   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8h   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8p   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8x   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I0   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p(   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ͬ   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �@   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �P   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �T   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �X   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �\   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �`   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20180417003537  20200116221515  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0577_231                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @�[���1   @�[���� @4��H˒�dJ��O�;1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  A   A   A@  A`  A~ffA�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�ffB�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1fD1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�<�D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�|�D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�p 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�z�@�{@�{A
=A?
=A_
=A}p�A��A��A��A��AυA߅A�A��BBBBB'B/B7B?BGBOBWB_BgBoBwBB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��B�G�B��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HC�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RD |)D �)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D	|)D	�)D
|)D
�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D |)D �)D!|)D!�)D"|)D"�)D#|)D#�)D$|)D$�)D%|)D%�)D&|)D&�)D'|)D'�)D(|)D(�)D)|)D)�)D*|)D*�)D+|)D+�)D,|)D,�)D-|)D-�)D.|)D.�)D/|)D/�)D0|)D1�D1|)D1�)D2|)D2�)D3|)D3�)D4|)D4�)D5|)D5�)D6|)D6�)D7|)D7�)D8|)D8�)D9|)D9�)D:|)D:�)D;|)D;�)D<|)D<�)D=|)D=�)D>|)D>�)D?|)D?�)D@|)D@�)DA|)DA�)DB|)DB�)DC|)DC�)DD|)DD�)DE|)DE�)DF|)DF�)DG|)DG�)DH|)DH�)DI|)DI�)DJ|)DJ�)DK|)DK�)DL|)DL�)DM|)DM�)DN|)DN�)DO|)DO�)DP|)DP�)DQ|)DQ�)DR|)DR�)DS|)DS�)DT|)DT�)DU|)DU�)DV|)DV�)DW|)DW�)DX|)DX�)DY|)DY�)DZ|)DZ�)D[|)D[�)D\|)D\�)D]|)D]�)D^|)D^�)D_|)D_�)D`|)D`�)Da|)Da�)Db|)Db�)Dc|)Dc�)Dd|)Dd�)De|)De�)Df|)Df�)Dg|)Dg�)Dh|)Dh�)Di|)Di�)Dj|)Dj�)Dk|)Dk�)Dl|)Dl�)Dm|)Dm�)Dn|)Dn�)Do|)Do�)Dp|)Dp�)Dq|)Dq�)Dr|)Dr�)Ds|)Ds�)Dt|)Dt�)Du|)Du�)Dv|)Dv�)Dw|)Dw�)Dx|)Dx�)Dy|)Dy�)Dz|)Dz�)D{|)D{�)D||)D|�)D}|)D}�)D~|)D~�)D|)D�)D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D���D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�:�D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D¾D��D�>D�~DþD��D�>D�~DľD��D�>D�~DžD��D�>D�~DƾD��D�>D�~DǾD��D�>D�~DȾD��D�>D�~DɾD��D�>D�~DʾD��D�>D�~D˾D��D�>D�~D̾D��D�>D�~D;D��D�>D�~DξD��D�>D�~DϾD��D�>D�~DоD��D�>D�~DѾD��D�>D�~DҾD��D�>D�~DӾD��D�>D�~DԾD��D�>D�~DվD��D�>D�~D־D��D�>D�~D׾D��D�>D�~DؾD��D�>D�~DپD��D�>D�~DھD��D�>D�~D۾D��D�>D�~DܾD��D�>D�~DݾD��D�>D�~D޾D��D�:�D�~D߾D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�z�D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D��D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�AHD�n1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�~�AɁAɃAɁAɁA�~�A�~�AɃAɃA�|�A�&�A�bA��;A�z�A�33A��A��Aş�A�5?A�bA���A���A��mA���Aũ�Aş�Aĕ�A�XA�33A��A�A���A��mA��/A���Aô9Aé�Aß�AËDAËDA���A���A��/AÙ�A�I�A�&�A��HA��^A�XA�-A��A�"�A�+A��A���A�S�A���A���A��+A�ƨA�9XA���A��
A�A�A�$�A�|�A�A���A��A��9A�C�A��A�`BA��RA��A�7LA�=qA�^5A�bNA��wA�ZA�1A�n�A��TA�$�A��A�JA�XA�I�A�%A��`A��A�A�ZA���A�+A�JA�A��yA�=qA��A�M�A�~�A��A�r�A���A���A�5?A�I�A��jA�jA���A���A���A�p�A�ĜA� �A�~�A���A�ĜA�~�A���A�7LA��A���A~��A|jAxI�AvE�At��ArjAp�9AnVAl�Ai&�Ag��Af�`Af��Ad�HAc&�Aa��A^�uA]t�AZ�jAX��AWS�ATM�ASdZASS�ASS�AS?}AR��AP��AOp�AK��AIl�AH��AG?}AD �AA��A?�A>n�A>9XA=t�A<ZA:�RA8�RA7\)A6�/A6��A6(�A5O�A4$�A1�PA/ƨA-��A*��A(ĜA'+A&v�A&9XA%��A#�
A"bNA!��A!�hA!O�A (�A�7AVAE�A��A;dAjAp�A�HA\)A��AXA��A�AdZA
=AZAQ�A��A��AhsAQ�A�At�A�A
E�A	ƨA��Az�AM�AbAĜA�A"�AbA ��@��@�
=@��!@��@���@�hs@�7L@��/@�j@���@�@�M�@���@��@�(�@�  @��@��H@���@�K�@��#@��@���@�;d@��y@柾@���@�z�@◍@�O�@���@ޗ�@��@�  @۶F@��@�@��@�b@�=q@�Ĝ@҇+@�V@�S�@��#@́@�@�z�@ʰ!@�v�@�^5@��@�C�@�hs@���@°!@���@���@�  @�@���@��#@��j@�z�@�j@��@�dZ@��@��@�p�@�O�@�O�@��@��@��@�"�@�V@��-@�`B@��u@���@��@��@��@�~�@��h@���@��`@���@�I�@�1@���@��
@�ƨ@��@�|�@�C�@�
=@��\@���@�`B@��`@��u@�b@�
=@��\@�V@�M�@�-@�{@�@���@���@��h@�/@�Ĝ@��@���@��m@�S�@�K�@��H@���@��\@�v�@�v�@�V@���@���@�/@�Z@��
@���@��P@�S�@��@���@���@�ff@�=q@�M�@�~�@�^5@�@�{@��-@��-@��h@��`@��D@�  @���@��@��@���@���@���@�;d@��!@��!@���@�V@��T@���@�hs@���@��@��/@��/@��`@��/@���@��@�A�@�1@��;@��
@�|�@�+@�+@�"�@�
=@��H@��R@���@��+@�n�@�n�@�ff@�M�@���@��-@���@���@��@�X@�O�@�G�@�/@��/@�j@� �@�1@�ƨ@�K�@���@�  @�9X@��@���@��P@�l�@�"�@��@��@�ȴ@���@��+@�n�@�E�@�J@���@��@�X@��@�%@�Ĝ@���@�Z@�ƨ@���@�|�@�dZ@�S�@�+@���@�J@��T@��#@���@��^@���@���@�`B@��@�j@�9X@�ƨ@��@�|�@��y@�n�@�5?@���@���@�x�@�7L@�V@���@���@��9@���@��@��9@��@��@�I�@� �@���@��
@�dZ@���@�V@�-@�@�@��T@���@��-@�p�@��@�Ĝ@�r�@�A�@�(�@��;@��w@�t�@�+@�"�@�
=@��@��H@��H@��R@�n�@�V@�$�@�@���@�x�@�G�@�/@���@��`@��`@��@�I�@� �@���@�|�@�dZ@�33@�
=@���@��@��@�ff@���@�hs@�O�@�V@���@��j@�Ĝ@��@��D@�z�@�Z@��@+@~�@~�R@~�R@~�+@~$�@}�@}�T@}�-@}?}@|��@|��@|�D@|z�@|9X@{��@z��@z-@yX@x �@w�;@v�y@u�@t�@tj@t�/@v�+@u�@t��@t��@t�/@t1@s33@r�\@r-@q��@qx�@p��@o��@o\)@o
=@nȴ@n�+@nE�@m@mp�@m/@l�@lI�@l9X@l1@k�@kS�@k33@j�@j�H@j��@j�!@j-@i��@i�@h��@h��@hr�@hb@g�@g��@g�P@g\)@g�P@g�@g�P@f�@fv�@f@e@e�@d�@d(�@cS�@b��@b�\@b^5@b-@a��@aX@`��@`Ĝ@`��@`�@`�@`bN@_�;@_l�@_+@^�y@^�R@]��@\��@[�
@[ƨ@[ƨ@[�F@[ƨ@[ƨ@[�F@[�F@[��@[��@[��@[t�@[o@Z��@Z��@Z�@[@Z��@Z��@ZM�@Y�^@Yx�@Y7L@X��@XbN@XQ�@W�;@W��@W\)@V�@Vff@V5?@V{@U��@T��@T�j@T�D@T(�@Sƨ@SS�@So@R=q@Q�@Q�7@P��@P�@P  @O�@O\)@O;d@O�@Nȴ@Nv�@N5?@M�@M�@M�@L��@L�/@L�/@L�j@LI�@K��@J^5@I��@I�^@IX@H��@HQ�@H �@Hr�@H  @G��@GK�@F�@Fv�@F@E`B@D��@D�D@C�
@B�H@B~�@A��@A�#@A�^@A��@A��@A��@Ax�@A7L@A�@@r�@?�;@?�P@?;d@?
=@>�R@>{@=��@=��@=p�@=`B@=?}@<��@<��@<��@<�D@<Z@<(�@;��@;C�@:��@9��@9��@9�^@9��@9��@9&�@8��@8r�@8 �@7�;@7�w@7�@7�P@7|�@7\)@7
=@6�y@6�R@6��@6�+@6{@5�-@5�@5V@4�j@4I�@3C�@3o@2�@1�^@0��@0Ĝ@0A�@0  @/�@/�w@/|�@/
=@.ȴ@.�R@.�+@.E�@.$�@.@-�@-@-�-@-?}@,�/@,�D@,�@+��@+�F@+S�@+o@*�H@*��@*��@*~�@*^5@*=q@)��@)��@)x�@)�@)%@(�`@(��@(�`@(Ĝ@(�u@(A�@( �@(  @'�@'\)@'�@&�y@&ȴ@&ȴ@&ȴ@&��@&5?@%�@%�T@%@%�@%?}@%V@$�@$�D@$z�@$Z@$I�@$9X@$(�@$�@#ƨ@#�@#o@"��@"�\@"n�@"^5@"M�@"M�@"=q@"-@"�@!��@!��@!X@!%@!%@!%@ �`@ �9@ bN@   @�w@�P@�P@l�@�@�@�+@5?@@@�@��@�@�D@z�@Z@I�@I�@9X@9X@9X@(�@�@ƨ@�F@��@dZ@o@�!@~�@M�@-@�@��@�^@��@�7@G�@��@Ĝ@��@�@bN@1'@�@�@�P@|�@K�@�@��@�@ȴ@��@ff@V@E�@5?@5?@@��@@�h@O�@?}@V@��@��@z�@j@Z@9X@�@��@ƨ@�F@��@�@dZ@S�@C�@o@@�@��@�!@��@~�@^5@=q@-@�#@��@�7@7L@�`@Ĝ@��@r�@1'@ �@  @��@K�@
=@�y@�@��@�+@�+@v�@E�@$�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�~�AɁAɃAɁAɁA�~�A�~�AɃAɃA�|�A�&�A�bA��;A�z�A�33A��A��Aş�A�5?A�bA���A���A��mA���Aũ�Aş�Aĕ�A�XA�33A��A�A���A��mA��/A���Aô9Aé�Aß�AËDAËDA���A���A��/AÙ�A�I�A�&�A��HA��^A�XA�-A��A�"�A�+A��A���A�S�A���A���A��+A�ƨA�9XA���A��
A�A�A�$�A�|�A�A���A��A��9A�C�A��A�`BA��RA��A�7LA�=qA�^5A�bNA��wA�ZA�1A�n�A��TA�$�A��A�JA�XA�I�A�%A��`A��A�A�ZA���A�+A�JA�A��yA�=qA��A�M�A�~�A��A�r�A���A���A�5?A�I�A��jA�jA���A���A���A�p�A�ĜA� �A�~�A���A�ĜA�~�A���A�7LA��A���A~��A|jAxI�AvE�At��ArjAp�9AnVAl�Ai&�Ag��Af�`Af��Ad�HAc&�Aa��A^�uA]t�AZ�jAX��AWS�ATM�ASdZASS�ASS�AS?}AR��AP��AOp�AK��AIl�AH��AG?}AD �AA��A?�A>n�A>9XA=t�A<ZA:�RA8�RA7\)A6�/A6��A6(�A5O�A4$�A1�PA/ƨA-��A*��A(ĜA'+A&v�A&9XA%��A#�
A"bNA!��A!�hA!O�A (�A�7AVAE�A��A;dAjAp�A�HA\)A��AXA��A�AdZA
=AZAQ�A��A��AhsAQ�A�At�A�A
E�A	ƨA��Az�AM�AbAĜA�A"�AbA ��@��@�
=@��!@��@���@�hs@�7L@��/@�j@���@�@�M�@���@��@�(�@�  @��@��H@���@�K�@��#@��@���@�;d@��y@柾@���@�z�@◍@�O�@���@ޗ�@��@�  @۶F@��@�@��@�b@�=q@�Ĝ@҇+@�V@�S�@��#@́@�@�z�@ʰ!@�v�@�^5@��@�C�@�hs@���@°!@���@���@�  @�@���@��#@��j@�z�@�j@��@�dZ@��@��@�p�@�O�@�O�@��@��@��@�"�@�V@��-@�`B@��u@���@��@��@��@�~�@��h@���@��`@���@�I�@�1@���@��
@�ƨ@��@�|�@�C�@�
=@��\@���@�`B@��`@��u@�b@�
=@��\@�V@�M�@�-@�{@�@���@���@��h@�/@�Ĝ@��@���@��m@�S�@�K�@��H@���@��\@�v�@�v�@�V@���@���@�/@�Z@��
@���@��P@�S�@��@���@���@�ff@�=q@�M�@�~�@�^5@�@�{@��-@��-@��h@��`@��D@�  @���@��@��@���@���@���@�;d@��!@��!@���@�V@��T@���@�hs@���@��@��/@��/@��`@��/@���@��@�A�@�1@��;@��
@�|�@�+@�+@�"�@�
=@��H@��R@���@��+@�n�@�n�@�ff@�M�@���@��-@���@���@��@�X@�O�@�G�@�/@��/@�j@� �@�1@�ƨ@�K�@���@�  @�9X@��@���@��P@�l�@�"�@��@��@�ȴ@���@��+@�n�@�E�@�J@���@��@�X@��@�%@�Ĝ@���@�Z@�ƨ@���@�|�@�dZ@�S�@�+@���@�J@��T@��#@���@��^@���@���@�`B@��@�j@�9X@�ƨ@��@�|�@��y@�n�@�5?@���@���@�x�@�7L@�V@���@���@��9@���@��@��9@��@��@�I�@� �@���@��
@�dZ@���@�V@�-@�@�@��T@���@��-@�p�@��@�Ĝ@�r�@�A�@�(�@��;@��w@�t�@�+@�"�@�
=@��@��H@��H@��R@�n�@�V@�$�@�@���@�x�@�G�@�/@���@��`@��`@��@�I�@� �@���@�|�@�dZ@�33@�
=@���@��@��@�ff@���@�hs@�O�@�V@���@��j@�Ĝ@��@��D@�z�@�Z@��@+@~�@~�R@~�R@~�+@~$�@}�@}�T@}�-@}?}@|��@|��@|�D@|z�@|9X@{��@z��@z-@yX@x �@w�;@v�y@u�@t�@tj@t�/@v�+@u�@t��@t��@t�/@t1@s33@r�\@r-@q��@qx�@p��@o��@o\)@o
=@nȴ@n�+@nE�@m@mp�@m/@l�@lI�@l9X@l1@k�@kS�@k33@j�@j�H@j��@j�!@j-@i��@i�@h��@h��@hr�@hb@g�@g��@g�P@g\)@g�P@g�@g�P@f�@fv�@f@e@e�@d�@d(�@cS�@b��@b�\@b^5@b-@a��@aX@`��@`Ĝ@`��@`�@`�@`bN@_�;@_l�@_+@^�y@^�R@]��@\��@[�
@[ƨ@[ƨ@[�F@[ƨ@[ƨ@[�F@[�F@[��@[��@[��@[t�@[o@Z��@Z��@Z�@[@Z��@Z��@ZM�@Y�^@Yx�@Y7L@X��@XbN@XQ�@W�;@W��@W\)@V�@Vff@V5?@V{@U��@T��@T�j@T�D@T(�@Sƨ@SS�@So@R=q@Q�@Q�7@P��@P�@P  @O�@O\)@O;d@O�@Nȴ@Nv�@N5?@M�@M�@M�@L��@L�/@L�/@L�j@LI�@K��@J^5@I��@I�^@IX@H��@HQ�@H �@Hr�@H  @G��@GK�@F�@Fv�@F@E`B@D��@D�D@C�
@B�H@B~�@A��@A�#@A�^@A��@A��@A��@Ax�@A7L@A�@@r�@?�;@?�P@?;d@?
=@>�R@>{@=��@=��@=p�@=`B@=?}@<��@<��@<��@<�D@<Z@<(�@;��@;C�@:��@9��@9��@9�^@9��@9��@9&�@8��@8r�@8 �@7�;@7�w@7�@7�P@7|�@7\)@7
=@6�y@6�R@6��@6�+@6{@5�-@5�@5V@4�j@4I�@3C�@3o@2�@1�^@0��@0Ĝ@0A�@0  @/�@/�w@/|�@/
=@.ȴ@.�R@.�+@.E�@.$�@.@-�@-@-�-@-?}@,�/@,�D@,�@+��@+�F@+S�@+o@*�H@*��@*��@*~�@*^5@*=q@)��@)��@)x�@)�@)%@(�`@(��@(�`@(Ĝ@(�u@(A�@( �@(  @'�@'\)@'�@&�y@&ȴ@&ȴ@&ȴ@&��@&5?@%�@%�T@%@%�@%?}@%V@$�@$�D@$z�@$Z@$I�@$9X@$(�@$�@#ƨ@#�@#o@"��@"�\@"n�@"^5@"M�@"M�@"=q@"-@"�@!��@!��@!X@!%@!%@!%@ �`@ �9@ bN@   @�w@�P@�P@l�@�@�@�+@5?@@@�@��@�@�D@z�@Z@I�@I�@9X@9X@9X@(�@�@ƨ@�F@��@dZ@o@�!@~�@M�@-@�@��@�^@��@�7@G�@��@Ĝ@��@�@bN@1'@�@�@�P@|�@K�@�@��@�@ȴ@��@ff@V@E�@5?@5?@@��@@�h@O�@?}@V@��@��@z�@j@Z@9X@�@��@ƨ@�F@��@�@dZ@S�@C�@o@@�@��@�!@��@~�@^5@=q@-@�#@��@�7@7L@�`@Ĝ@��@r�@1'@ �@  @��@K�@
=@�y@�@��@�+@�+@v�@E�@$�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�\B
�7B
�{B
�FB
�wB
ŢB
ƨB
ŢB
ȴB
��B
��B
��B
��B
��B
�B�BuB�B�B�B�B�B�B�B�BuBhBuB�B�B.BK�B]/Bn�B�B�oB��B��B��B�9B�wB��B�B�fB��B��BDB#�B'�B0!B;dB>wB@�B<jB<jBI�BQ�BXBVBYB]/BbNBiyBffBXBL�BI�BE�B9XB-B8RB-B �B%BDBB  B�B�BBBB��B�yB��B��B�3B��B��B��B�B�Bu�B}�Bm�B^5BD�B5?B.B\B
�BB
��B
�TB
�B
�B
�#B
�?B
��B
��B
�B
x�B
x�B
t�B
VB
9XB
 �B
	7B	�B	��B	�B	�B	��B	�dB	��B	��B	��B	��B	��B	�7B	x�B	n�B	S�B	S�B	?}B	1'B	1'B	�B	'�B	49B	1'B	)�B	�B	B�B�B��B�B��B�}B�?BĜB��B��BĜB�RB�'B��B�B�LB�LB�'B��B��B�B�=Bv�Bk�Bx�Bx�B� B�B� Bk�Bl�Bx�Bw�Bt�Bk�Bm�Br�Bn�Bn�Bn�BgmB]/BZBL�B=qBF�BW
BW
BW
BVBL�B?}BF�BQ�BK�BH�BW
BS�BT�BM�BR�BP�BS�BYBQ�BB�B5?B=qBG�BD�BR�B]/B`BB]/B_;B_;B]/B[#BVBM�BD�B6FBO�BT�B`BB\)BO�B;dBL�BK�BS�BW
B^5B^5BbNB_;B[#BO�BO�BS�BYBZB\)BbNBjBffBffBe`BgmBcTBffBgmBm�Bs�By�B�1B�\B�%B�B�PB��B��B��B�uB�{B��B��B��B�B�B�LB�3B�9B��BB��B�qB��BB��B��B��B��B��B��B��B��B�B�HB�HB�fB�B�B�B�B�B��B��B��B��B	B	+B	+B	1B	1B	1B		7B	
=B	DB		7B	bB	hB	�B	�B	�B	#�B	)�B	-B	.B	/B	0!B	0!B	/B	/B	.B	/B	2-B	9XB	:^B	9XB	B�B	G�B	N�B	P�B	S�B	T�B	S�B	S�B	T�B	S�B	S�B	W
B	]/B	^5B	^5B	_;B	aHB	gmB	gmB	k�B	q�B	v�B	x�B	{�B	�B	~�B	�B	�B	}�B	~�B	|�B	}�B	�B	�B	�B	�B	�B	�B	�1B	�bB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�-B	�-B	�3B	�LB	�LB	�FB	�FB	�FB	�RB	�XB	�dB	�jB	�jB	�jB	�jB	B	B	ÖB	B	B	ÖB	ÖB	B	��B	��B	��B	ĜB	ƨB	ǮB	��B	��B	��B	��B	��B	�B	�B	�B	�)B	�5B	�/B	�5B	�;B	�;B	�;B	�5B	�;B	�TB	�TB	�ZB	�`B	�`B	�fB	�fB	�`B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
%B
%B
+B
+B
+B
+B
1B
1B
1B
B
B
+B
DB
JB
PB
JB
JB
JB
DB

=B
JB
JB
VB
\B
VB
\B
\B
\B
oB
oB
oB
oB
uB
hB
bB
uB
oB
uB
oB
oB
{B
�B
{B
�B
�B
{B
uB
�B
{B
{B
�B
�B
�B
�B
�B
�B
{B
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
�B
 �B
!�B
 �B
�B
 �B
!�B
 �B
!�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
!�B
&�B
 �B
"�B
$�B
#�B
 �B
!�B
"�B
$�B
#�B
#�B
!�B
 �B
#�B
#�B
$�B
$�B
#�B
#�B
#�B
#�B
#�B
#�B
%�B
%�B
%�B
%�B
'�B
&�B
'�B
'�B
&�B
%�B
%�B
&�B
(�B
(�B
)�B
(�B
+B
+B
+B
,B
-B
.B
,B
)�B
,B
,B
-B
,B
)�B
,B
,B
-B
/B
/B
/B
/B
-B
.B
0!B
0!B
0!B
1'B
0!B
/B
/B
0!B
0!B
0!B
-B
.B
/B
49B
49B
49B
49B
49B
49B
49B
49B
49B
49B
33B
33B
49B
5?B
6FB
6FB
5?B
6FB
5?B
49B
5?B
6FB
6FB
6FB
8RB
7LB
7LB
7LB
7LB
8RB
9XB
9XB
8RB
8RB
:^B
:^B
9XB
9XB
9XB
:^B
8RB
9XB
9XB
8RB
9XB
8RB
:^B
9XB
:^B
:^B
9XB
:^B
:^B
:^B
9XB
:^B
<jB
<jB
=qB
<jB
;dB
:^B
9XB
<jB
=qB
<jB
=qB
=qB
?}B
A�B
A�B
@�B
A�B
@�B
B�B
A�B
A�B
A�B
C�B
B�B
B�B
D�B
D�B
F�B
F�B
F�B
F�B
F�B
F�B
E�B
E�B
D�B
D�B
F�B
F�B
G�B
G�B
F�B
H�B
I�B
I�B
J�B
J�B
I�B
J�B
J�B
K�B
J�B
J�B
I�B
I�B
I�B
I�B
M�B
M�B
M�B
M�B
L�B
M�B
L�B
M�B
N�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
P�B
O�B
N�B
O�B
O�B
O�B
O�B
O�B
M�B
Q�B
P�B
N�B
P�B
S�B
S�B
T�B
VB
VB
T�B
T�B
VB
W
B
W
B
W
B
XB
XB
XB
XB
XB
W
B
W
B
XB
XB
YB
YB
XB
ZB
ZB
[#B
ZB
[#B
[#B
ZB
ZB
ZB
ZB
ZB
[#B
[#B
\)B
\)B
\)B
[#B
[#B
\)B
\)B
\)B
\)B
]/B
^5B
^5B
^5B
^5B
]/B
]/B
^5B
^5B
^5B
^5B
^5B
_;B
^5B
`BB
`BB
`BB
aHB
`BB
`BB
`BB
_;B
_;B
_;B
`BB
aHB
bNB
cTB
cTB
cTB
bNB
bNB
bNB
bNB
bNB
bNB
cTB
dZB
dZB
dZB
cTB
cTB
cTB
cTB
dZB
dZB
dZB
cTB
dZB
dZB
e`B
ffB
ffB
e`B
e`B
dZB
e`B
gmB
gmB
hsB
hsB
hsB
hsB
hsB
hsB
gmB
gmB
hsB
hsB
hsB
gmB
hsB
iyB
iyB
jB
jB
iyB
iyB
jB
jB
jB
iyB
k�B
k�B
k�B
k�B
jB
jB
k�B
k�B
k�B
k�B
k�B
l�B
l�B
m�B
l�B
l�B
m�B
m�B
n�B
n�B
m�B
m�B
n�B
n�B
m�B
n�B
n�B
n�B
o�B
o�B
p�B
p�B
o�B
p�B
p�B
p�B
p�B
q�B
p�B
p�B
q�B
q�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
p�B
q�B
r�B
q�B
q�B
r�B
r�B
r�B
r�B
s�B
r�B
r�B
q�B
s�B
t�B
t�B
t�B
t�B
u�B
u�B
t�B
u�B
u�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
��B
��B
��B
��B
��B
��B
��B
��B
�B
��B
��B
��B
��B
��B
��B
�+B
�YB
�lB
�B
��B
��B
��B
��B
�B�BMBB�B�B�B�B�B�B�B�B�B�B�B�B-�BK�B]IBo5B��B��B��B��B��B��B��B��B�1B��B�?B�$B�B$�B)*B1�B<�B@ BBAB=�B>�BKBR�BYBW$BZB^5BcnBj�Bh$BZ�BO�BK�BG�B;�B0;B9$B./B"�B	�B6B�BAB�B��B�B[BGB��B�QB�B��B��B�DB�B�eB��B��Bw�BBoiB`vBIB9$B0�B[B
�B
��B
��B
�[B
�iB
�dB
�>B
�sB
�HB
�9B
{�B
z�B
v`B
YB
="B
%�B
�B	�?B	�B	��B	��B	�MB	�wB	�B	��B	�\B	��B	�|B	��B	{JB	qB	W�B	U�B	B�B	3�B	3B	 �B	(�B	4nB	1[B	*eB	�B	gB�GB�/B�
B�QBөB�GB�lBƎB�AB�}B��B�*B�MB�fB��B��B��B�B�XB��B��B��By�BoB{0Bz�B��B��B� Bn/BnIByrBxlButBmBn}Bs�Bo�Bo�BoOBh�B^�B[WBO(B@�BH�BW�BXBW�BV�BN<BB'BHfBR�BMjBJ#BW�BT�BU�BN�BS�BR BT�BY�BR�BD�B88B?}BIRBF�BS�B]�B`�B]�B_�B_�B]~B[�BV�BN�BFB8lBP�BU�B`BB\�BQ4B>BM�BMjBT�BW�B^�B^�Bb�B_�B[�BQ4BQ4BUBZ7B[=B]/BcBj�BgBg8BfLBhXBd�Bg�Bh�Bn�Bt�Bz�B�fB�\B�_B�MB��B�B��B��B��B��B��B�vB��B��B��B��B��B�B��B��B�B�B��B�aB�6B�&B�B�bB̘B�oBՁB՛BںB�B��B�B��B�B��B�B�[B�LB�B�.B�VB	GB	_B	_B	KB	KB	fB		lB	
�B	�B		�B	�B	�B	�B		B	WB	$@B	*0B	-CB	./B	/5B	0UB	0UB	/iB	/�B	.cB	/�B	2�B	9rB	:�B	9�B	B�B	HB	OB	QB	T,B	UB	T,B	TFB	UgB	TaB	T�B	W�B	]dB	^OB	^�B	_�B	a|B	g�B	g�B	k�B	q�B	v�B	y$B	|B	�B	HB	�'B	�[B	~wB	HB	}VB	~BB	� B	�B	�3B	�B	�9B	�mB	��B	��B	��B	��B	�B	��B	��B	�B	��B	��B	��B	��B	��B	�&B	�B	�B	�]B	�;B	�GB	�|B	�hB	�fB	�fB	�`B	�`B	�`B	��B	�rB	��B	�jB	��B	��B	��B	��B	ªB	ÖB	ªB	ªB	��B	ðB	ªB	��B	��B	��B	��B	��B	��B	̘B	ѷB	��B	�B	�,B	�EB	�QB	�kB	�CB	�5B	�dB	�jB	�VB	�VB	�pB	ބB	ߊB	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	�B	�B	�B	��B	��B	�B	��B	��B	��B	��B	��B	��B	�!B	��B	�B	�B	��B	�B	�+B	�*B	�"B	�(B	�BB
 4B
 OB
'B
-B
MB
9B
%B
?B
EB
EB
EB
_B
KB
fB
fB
�B
�B
�B
^B
dB
jB
~B
~B
~B
xB

�B
~B
�B
�B
vB
�B
vB
�B
�B
oB
�B
�B
�B
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	B
�B
�B
�B
 �B
 �B
�B
 �B
!�B
 �B
�B
 �B
!�B
 �B
!�B
�B
�B
�B
�B
�B
�B
�B
	B
B
B
�B
!�B
&�B
!-B
#B
$�B
#�B
!B
"B
#B
$�B
$B
$&B
"B
!HB
#�B
#�B
%B
%B
#�B
$&B
#�B
$B
$B
$B
%�B
&B
&2B
%�B
'�B
'B
'�B
($B
'B
&B
&B
'B
)B
)B
*0B
)B
+B
+B
+B
,=B
-B
.B
,=B
*eB
,WB
,=B
-)B
,=B
*KB
,=B
,qB
-CB
/OB
/OB
/OB
/OB
-CB
.cB
0!B
0UB
0UB
1AB
0UB
/OB
/OB
0;B
0;B
0;B
-wB
.�B
/iB
4TB
49B
4TB
49B
49B
49B
49B
4TB
49B
49B
3MB
3hB
4nB
5?B
6FB
6FB
5tB
6`B
5tB
4�B
5ZB
6zB
6�B
6zB
8RB
7�B
7�B
7fB
7�B
8�B
9rB
9rB
8�B
8�B
:�B
:�B
9�B
9�B
9�B
:�B
8�B
9rB
9�B
8�B
9�B
8�B
:�B
9�B
:xB
:xB
9�B
:xB
:�B
:xB
9�B
:xB
<jB
<�B
=qB
<�B
;�B
:�B
9�B
<�B
=�B
<�B
=�B
=�B
?�B
A�B
A�B
@�B
A�B
@�B
B�B
A�B
A�B
A�B
C�B
B�B
B�B
D�B
D�B
F�B
F�B
F�B
F�B
F�B
F�B
E�B
E�B
D�B
D�B
F�B
F�B
G�B
G�B
F�B
H�B
I�B
I�B
J�B
J�B
I�B
J�B
J�B
K�B
J�B
J�B
I�B
I�B
I�B
I�B
M�B
M�B
NB
M�B
MB
NB
MB
NB
N�B
O�B
O�B
O�B
O�B
O�B
O�B
PB
O�B
QB
O�B
OB
O�B
O�B
PB
PB
P.B
N<B
RB
Q4B
OBB
Q4B
TB
T,B
U2B
VB
V9B
U2B
UMB
VB
W
B
W$B
W$B
X+B
X+B
X+B
X+B
X+B
WYB
WYB
X+B
XEB
Y1B
Y1B
XEB
Z7B
Z7B
[=B
ZQB
[=B
[WB
Z7B
Z7B
Z7B
ZQB
ZQB
[=B
[=B
\)B
\)B
\CB
[=B
[=B
\]B
\CB
\CB
\CB
]IB
^jB
^jB
^5B
^OB
]IB
]IB
^OB
^jB
^OB
^jB
^OB
_VB
^jB
`\B
`BB
`vB
aHB
`\B
`BB
`vB
_pB
_VB
_pB
`vB
abB
b�B
cTB
cTB
cTB
bhB
bhB
bhB
b�B
bhB
b�B
cnB
dZB
dZB
dtB
cnB
cnB
cnB
cnB
dtB
dZB
d�B
c�B
d�B
dtB
e�B
f�B
ffB
ezB
e�B
d�B
e�B
gmB
g�B
h�B
hsB
hsB
h�B
h�B
hsB
g�B
g�B
hsB
h�B
h�B
g�B
h�B
i�B
i�B
j�B
jB
i�B
i�B
j�B
j�B
j�B
i�B
k�B
k�B
k�B
k�B
j�B
j�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
m�B
l�B
l�B
m�B
m�B
n�B
n�B
m�B
m�B
n�B
n�B
m�B
n�B
n�B
n�B
o�B
o�B
p�B
p�B
o�B
p�B
p�B
p�B
p�B
q�B
p�B
p�B
q�B
q�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
p�B
q�B
r�B
q�B
q�B
r�B
r�B
r�B
r�B
s�B
r�B
r�B
q�B
s�B
t�B
t�B
t�B
t�B
u�B
u�B
t�B
u�B
u�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.06(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201804210039002018042100390020180421003900201806221329122018062213291220180622132912201804261707472018042617074720180426170747  JA  ARFMdecpA19c                                                                20180417093516  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180417003537  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180417003539  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180417003540  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180417003540  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180417003540  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180417003540  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180417003540  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180417003542  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180417003542                      G�O�G�O�G�O�                JA  ARUP                                                                        20180417005650                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180417153358  CV  JULD            G�O�G�O�F�ݱ                JM  ARCAJMQC2.0                                                                 20180420153900  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180420153900  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180426080747  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622042912  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116221515                      G�O�G�O�G�O�                