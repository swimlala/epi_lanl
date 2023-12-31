CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2017-12-06T00:35:20Z creation;2017-12-06T00:35:24Z conversion to V3.1;2019-12-19T07:54:58Z update;     
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
_FillValue                 �  I@   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p`   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tL   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �l   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �D   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �x   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �x   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �x   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �x   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �    HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �H   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �X   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �\   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �l   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �p   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �t   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �xArgo profile    3.1 1.2 19500101000000  20171206003520  20200115121517  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0576_186                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�:�i>� 1   @�:�'�} @;��b���d\�s�1   GPS     A   B   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�33A�  A�  B   B  B��B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Cs�fCv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>�fD?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�<�D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׃3D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D���D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�L�D�ff11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�z�@�{@�{A
=A?
=A_
=A
=A��A��A��A��AθRA߅A�A��BB\)BBB'B/B7B?BGBOBWB_BgBoBwBB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HC�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�
Cu�Cw�Cy�C{�C}�C�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RD |)D �)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D	|)D	�)D
|)D
�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D |)D �)D!|)D!�)D"|)D"�)D#|)D#�)D$|)D$�)D%|)D%�)D&|)D&�)D'|)D'�)D(|)D(�)D)|)D)�)D*|)D*�)D+|)D+�)D,|)D,�)D-|)D-�)D.|)D.�)D/|)D/�)D0|)D0�)D1|)D1�)D2|)D2�)D3|)D3�)D4|)D4�)D5|)D5�)D6|)D6�)D7|)D7�)D8|)D8�)D9|)D9�)D:|)D:�)D;|)D;�)D<|)D<�)D=|)D=�)D>��D>�)D?|)D?�)D@|)D@�)DA|)DA�)DB|)DB�)DC|)DC�)DD|)DD�)DE|)DE�)DF|)DF�)DG|)DG�)DH|)DH�)DI|)DI�)DJ|)DJ�)DK|)DK�)DL|)DL�)DM|)DM�)DN|)DN�)DO|)DO�)DP|)DP�)DQ|)DQ�)DR|)DR�)DS|)DS�)DT|)DT�)DU|)DU�)DV|)DV�)DW|)DW�)DX|)DX�)DY|)DY�)DZ|)DZ�)D[|)D[�)D\|)D\�)D]|)D]�)D^|)D^�)D_|)D_�)D`|)D`�)Da|)Da�)Db|)Db�)Dc|)Dc�)Dd|)Dd�)De|)De�)Df|)Df�)Dg|)Dg�)Dh|)Dh�)Di|)Di�)Dj|)Dj�)Dk|)Dk�)Dl|)Dl�)Dm|)Dm�)Dn|)Dn�)Do|)Do�)Dp|)Dp�)Dq|)Dq�)Dr|)Dr�)Ds|)Ds�)Dt|)Dt�)Du|)Du�)Dv|)Dv�)Dw|)Dw�)Dx|)Dx�)Dy|)Dy�)Dz|)Dz�)D{|)D{�)D||)D|�)D}|)D}�)D~|)D~�)D|)D�)D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D��HD��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��HD��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D�HD�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D¾D��D�>D�~DþD��D�>D�~DľD��D�>D�~DžD��D�>D�~DƾD��D�>D�~DǾD��D�>D�~DȾD��D�>D�~DɾD��D�>D�~DʾD��D�>D�~D˾D��D�>D�~D̾D��D�:�D�~D;D��D�>D�~DξD��D�>D�~DϾD��D�>D�~DоD��D�>D�~DѾD��D�>D�~DҾD��D�>D�~DӾD��D�>D�~DԾD��D�>D�~DվD��D�>D�~D־D��D�>DׁHD׾D��D�>D�~DؾD��D�>D�~DپD���D�>D�~DھD��D�>D�~D۾D��D�>D�~DܾD��D�>D�~DݾD��D�>D�~D޾D��D�>D�~D߾D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D��D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�J�D�d{11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��jA��jA��wA���A���A�A�A���A�A�ȴA�ȴA�ȴA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�ƨA�ƨA��jA���A���A���A��A���A���A��+A��uA��PA�$�A��
A��^A���A��A��+A��;A�-A��A��wA��A���A�ĜA���A��HA��+A��A��\A�A�A��;A�jA�?}A���A��uA��A���A�ȴA�^5A�jA��hA��PA��A�$�A���A�ffA�-A��A��AƨA}�-A|-AyXAw�Aw?}Av�jAu�#AuK�Au"�AtĜAs�wAp�yAm�Aj �Ag�^Af�uAe�TAeG�Ad��AdffAb9XAa�mAa�wAa��Aa��Aa��Aa�7Aax�A`�\A^��A^z�A^E�A]�hA\�A[��A[�-AZ��AY��AX9XAV�+AUt�AT��AT1AS��AS|�AS?}AR�AQO�AO��AO+ANbAL��AK�;AK\)AJ�AJ�AJ��AJ�AI`BAH�HAG�AF�HAE�AD��ADM�AC�ACXAB��ABr�AB=qAA��A@r�A?��A>VA=�hA=l�A=XA<��A;��A9��A8-A7�;A7"�A6�A6ZA5�A5l�A4�/A4 �A3
=A29XA1+A0JA/�TA/dZA.�A-&�A,�9A,n�A,1'A+�mA+�7A+33A*�!A*JA)�PA'�^A%�;A%x�A%G�A$ĜA$r�A#��A#S�A"��A!x�A �A bNA 5?A��AhsAO�A^5A$�A�A��A`BA/A��A��A�DAbA��AXA�A�uAVA"�A�/A�A�Al�A �A�A�HA�A�jA1'A��A�A	;dA/A��AffA�AS�A��A�jAE�AdZA��A�A�A$�A�A�wA+A ��A Z@��y@�1'@�O�@��D@�;d@�^5@�F@��@�bN@�\)@��@웦@�\)@�R@��#@��
@�\@�A�@�;d@⟾@��@��/@ߍP@ޗ�@���@�=q@�O�@أ�@׾w@�
=@�ȴ@�n�@��T@ՙ�@�`B@�%@ԓu@� �@ӕ�@҇+@�;d@�7L@�A�@��
@˝�@�n�@���@ȋD@�t�@��T@�1'@��@���@�b@��@�~�@��h@��@�(�@��w@��P@�ȴ@��^@���@��m@�"�@�7L@�G�@���@�;d@���@���@���@�^5@�5?@��@��@���@���@�/@��`@��D@� �@���@�K�@��@��!@�-@���@�I�@��F@�+@�
=@���@��R@���@�ff@���@�&�@���@�\)@���@�V@�{@��@���@���@��@�/@���@�j@��@�K�@�"�@��@�@��H@���@�~�@�E�@���@��7@�7L@�V@���@�bN@��F@�
=@��!@���@��\@�ff@���@���@�x�@�G�@��@�7L@�?}@�/@���@��@�Q�@��@�33@�~�@��@�Ĝ@��D@�j@�Z@�Z@�Q�@�A�@�1'@��@��P@�@���@��@�7L@���@��/@��@���@��D@�1'@��@� �@�9X@��m@�;d@�"�@��@��+@���@�7L@�%@��@��j@���@��u@�z�@�b@���@���@���@��@�l�@�
=@��@���@���@���@�5?@�@���@�hs@�?}@��@�%@��@��`@��`@��`@��/@��j@�z�@�r�@�I�@�@|�@\)@+@~V@}p�@|�D@z=q@yX@xĜ@xr�@w�@w�P@w�@vV@u�-@u�@u�@u�@u?}@t�@u?}@r~�@r^5@t�@t�j@tz�@tZ@t1@st�@r�@r~�@rM�@r=q@q��@q%@pĜ@pbN@o�;@n�y@m@m`B@m�@l(�@k��@mO�@lj@kƨ@ko@j�H@j�!@i�@h��@hQ�@i��@hr�@g+@g�@g�@g
=@f��@g+@fff@e��@e�@eO�@e/@d�@d�/@d�/@d��@d1@cƨ@c��@c��@c��@c��@cS�@cS�@c33@b�\@a�#@a7L@`�`@a�^@a��@ax�@`�`@`Q�@`1'@`1'@`1'@`A�@`Q�@`  @_�w@_�@`bN@`�u@`�u@`��@`bN@`r�@`  @_
=@^5?@]�T@]@\��@\j@\j@[��@Z�@Y�#@Yhs@Y&�@X��@X��@Y�#@Y��@Y�@X��@X��@X1'@W��@WK�@Vȴ@Vv�@VV@VE�@VE�@VE�@VE�@VE�@V5?@V5?@V{@U�@U�T@U?}@UV@T�/@T��@T�/@U/@U?}@TZ@S��@S33@R�\@Rn�@Rn�@RM�@RM�@RM�@R=q@R=q@R-@Q�#@Q�7@Q�@O��@Nff@M�h@L��@L��@LZ@L1@KC�@J�H@J��@J��@J=q@JJ@Jn�@J�\@J^5@I�@I&�@H�@H1'@G��@Gl�@G
=@F��@Fȴ@F��@F�+@Fv�@Fff@FV@FE�@F@EV@D1@C�F@Ct�@B~�@B�@A��@A��@@�`@@�u@@r�@@b@?�@?�@>��@>��@>ȴ@>v�@>V@>E�@=�T@=�h@=`B@=�@<�@<��@<j@<1@;�@;33@:�@:�!@:�!@:��@:�!@:n�@:^5@:�@9G�@8Ĝ@8�@8r�@8A�@81'@7��@7|�@7K�@7;d@7�@7�@7
=@6��@6�y@6{@5�@5�T@5�-@5�h@5O�@4��@4�j@4�D@4I�@4(�@3��@3@2�\@2~�@2=q@2�@1��@1X@1G�@17L@1�@0��@0Ĝ@0bN@0b@/��@/+@.ȴ@.V@-�@-�-@,��@,��@,�@,I�@+�
@+C�@+"�@+o@+@+@*��@*�!@*�!@*�!@*�!@*��@*�\@*�\@*M�@)�7@(��@(r�@(bN@(b@'�w@'\)@&�y@&��@&v�@&ff@&V@&5?@&$�@&@%�h@%?}@%?}@%?}@%�@$�D@$9X@#��@#ƨ@#��@#�@#t�@#33@#o@"��@"^5@!��@!�#@!�^@!��@!7L@ ��@ �u@ r�@ bN@ bN@ Q�@ Q�@ Q�@ Q�@ Q�@ A�@  �@ b@�;@��@|�@+@�@��@��@v�@E�@$�@��@�-@�h@p�@/@�/@�D@z�@j@Z@9X@�@1@�
@�@C�@o@�H@n�@��@��@��@x�@hs@X@hs@hs@G�@�@��@�u@A�@b@��@;d@+@ȴ@E�@5?@5?@{@�T@�T@��@�h@p�@O�@O�@p�@`B@��@�/@��@�@z�@Z@I�@�@�
@�F@��@t�@33@�@��@��@^5@��@�#@�#@��@x�@&�@%@�`@��@��@��@��@�u@r�@bN@A�@ �@  @�w@|�@l�@;d@��@�@��@�+@ff@V@@�@�T@��@��@@�h@p�@?}@�@��@�j@�D@Z@I�@1@�m@ƨ@�F@�@dZ@S�@S�@S�@S�@C�@33@"�@"�@"�@"�@"�@"�@@
��@
~�@
-@	��@	�#@	�^@	��@	��@	��@	�7@	x�@	hs@	7L@��@�9@A�@ �@b@�;@�P@l�@K�@+@�@
=@��@�y@ȴ@�+@ff@V@5?@@��@�h@`B@?}@�@V@�@j@�@�m@�m@��@��@��@�m@ƨ@��@�@S�@33@�@��@��@~�@=q@=q11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��jA��jA��wA���A���A�A�A���A�A�ȴA�ȴA�ȴA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�ƨA�ƨA��jA���A���A���A��A���A���A��+A��uA��PA�$�A��
A��^A���A��A��+A��;A�-A��A��wA��A���A�ĜA���A��HA��+A��A��\A�A�A��;A�jA�?}A���A��uA��A���A�ȴA�^5A�jA��hA��PA��A�$�A���A�ffA�-A��A��AƨA}�-A|-AyXAw�Aw?}Av�jAu�#AuK�Au"�AtĜAs�wAp�yAm�Aj �Ag�^Af�uAe�TAeG�Ad��AdffAb9XAa�mAa�wAa��Aa��Aa��Aa�7Aax�A`�\A^��A^z�A^E�A]�hA\�A[��A[�-AZ��AY��AX9XAV�+AUt�AT��AT1AS��AS|�AS?}AR�AQO�AO��AO+ANbAL��AK�;AK\)AJ�AJ�AJ��AJ�AI`BAH�HAG�AF�HAE�AD��ADM�AC�ACXAB��ABr�AB=qAA��A@r�A?��A>VA=�hA=l�A=XA<��A;��A9��A8-A7�;A7"�A6�A6ZA5�A5l�A4�/A4 �A3
=A29XA1+A0JA/�TA/dZA.�A-&�A,�9A,n�A,1'A+�mA+�7A+33A*�!A*JA)�PA'�^A%�;A%x�A%G�A$ĜA$r�A#��A#S�A"��A!x�A �A bNA 5?A��AhsAO�A^5A$�A�A��A`BA/A��A��A�DAbA��AXA�A�uAVA"�A�/A�A�Al�A �A�A�HA�A�jA1'A��A�A	;dA/A��AffA�AS�A��A�jAE�AdZA��A�A�A$�A�A�wA+A ��A Z@��y@�1'@�O�@��D@�;d@�^5@�F@��@�bN@�\)@��@웦@�\)@�R@��#@��
@�\@�A�@�;d@⟾@��@��/@ߍP@ޗ�@���@�=q@�O�@أ�@׾w@�
=@�ȴ@�n�@��T@ՙ�@�`B@�%@ԓu@� �@ӕ�@҇+@�;d@�7L@�A�@��
@˝�@�n�@���@ȋD@�t�@��T@�1'@��@���@�b@��@�~�@��h@��@�(�@��w@��P@�ȴ@��^@���@��m@�"�@�7L@�G�@���@�;d@���@���@���@�^5@�5?@��@��@���@���@�/@��`@��D@� �@���@�K�@��@��!@�-@���@�I�@��F@�+@�
=@���@��R@���@�ff@���@�&�@���@�\)@���@�V@�{@��@���@���@��@�/@���@�j@��@�K�@�"�@��@�@��H@���@�~�@�E�@���@��7@�7L@�V@���@�bN@��F@�
=@��!@���@��\@�ff@���@���@�x�@�G�@��@�7L@�?}@�/@���@��@�Q�@��@�33@�~�@��@�Ĝ@��D@�j@�Z@�Z@�Q�@�A�@�1'@��@��P@�@���@��@�7L@���@��/@��@���@��D@�1'@��@� �@�9X@��m@�;d@�"�@��@��+@���@�7L@�%@��@��j@���@��u@�z�@�b@���@���@���@��@�l�@�
=@��@���@���@���@�5?@�@���@�hs@�?}@��@�%@��@��`@��`@��`@��/@��j@�z�@�r�@�I�@�@|�@\)@+@~V@}p�@|�D@z=q@yX@xĜ@xr�@w�@w�P@w�@vV@u�-@u�@u�@u�@u?}@t�@u?}G�O�G�O�@t�@t�j@tz�@tZ@t1@st�@r�@r~�@rM�@r=q@q��@q%@pĜ@pbN@o�;@n�y@m@m`B@m�G�O�G�O�@mO�@lj@kƨ@ko@j�H@j�!@i�@h��@hQ�G�O�@hr�@g+@g�@g�@g
=@f��@g+@fff@e��@e�@eO�@e/@d�@d�/@d�/@d��@d1@cƨ@c��@c��@c��@c��@cS�@cS�@c33@b�\@a�#@a7L@`�`@a�^@a��@ax�@`�`@`Q�@`1'@`1'@`1'@`A�@`Q�@`  @_�w@_�@`bN@`�u@`�u@`��@`bN@`r�@`  @_
=@^5?@]�T@]@\��@\j@\j@[��@Z�@Y�#@Yhs@Y&�@X��@X��@Y�#@Y��@Y�@X��@X��@X1'@W��@WK�@Vȴ@Vv�@VV@VE�@VE�@VE�@VE�@VE�@V5?@V5?@V{@U�@U�T@U?}@UV@T�/@T��@T�/@U/@U?}@TZ@S��@S33@R�\@Rn�@Rn�@RM�@RM�@RM�@R=q@R=q@R-@Q�#@Q�7@Q�@O��@Nff@M�h@L��@L��@LZ@L1@KC�@J�H@J��@J��@J=q@JJ@Jn�@J�\@J^5@I�@I&�@H�@H1'@G��@Gl�@G
=@F��@Fȴ@F��@F�+@Fv�@Fff@FV@FE�@F@EV@D1@C�F@Ct�@B~�@B�@A��@A��@@�`@@�u@@r�@@b@?�@?�@>��@>��@>ȴ@>v�@>V@>E�@=�T@=�h@=`B@=�@<�@<��@<j@<1@;�@;33@:�@:�!@:�!@:��@:�!@:n�@:^5@:�@9G�@8Ĝ@8�@8r�@8A�@81'@7��@7|�@7K�@7;d@7�@7�@7
=@6��@6�y@6{@5�@5�T@5�-@5�h@5O�@4��@4�j@4�D@4I�@4(�@3��@3@2�\@2~�@2=q@2�@1��@1X@1G�@17L@1�@0��@0Ĝ@0bN@0b@/��@/+@.ȴ@.V@-�@-�-@,��@,��@,�@,I�@+�
@+C�@+"�@+o@+@+@*��@*�!@*�!@*�!@*�!@*��@*�\@*�\@*M�@)�7@(��@(r�@(bN@(b@'�w@'\)@&�y@&��@&v�@&ff@&V@&5?@&$�@&@%�h@%?}@%?}@%?}@%�@$�D@$9X@#��@#ƨ@#��@#�@#t�@#33@#o@"��@"^5@!��@!�#@!�^@!��@!7L@ ��@ �u@ r�@ bN@ bN@ Q�@ Q�@ Q�@ Q�@ Q�@ A�@  �@ b@�;@��@|�@+@�@��@��@v�@E�@$�@��@�-@�h@p�@/@�/@�D@z�@j@Z@9X@�@1@�
@�@C�@o@�H@n�@��@��@��@x�@hs@X@hs@hs@G�@�@��@�u@A�@b@��@;d@+@ȴ@E�@5?@5?@{@�T@�T@��@�h@p�@O�@O�@p�@`B@��@�/@��@�@z�@Z@I�@�@�
@�F@��@t�@33@�@��@��@^5@��@�#@�#@��@x�@&�@%@�`@��@��@��@��@�u@r�@bN@A�@ �@  @�w@|�@l�@;d@��@�@��@�+@ff@V@@�@�T@��@��@@�h@p�@?}@�@��@�j@�D@Z@I�@1@�m@ƨ@�F@�@dZ@S�@S�@S�@S�@C�@33@"�@"�@"�@"�@"�@"�@@
��@
~�@
-@	��@	�#@	�^@	��@	��@	��@	�7@	x�@	hs@	7L@��@�9@A�@ �@b@�;@�P@l�@K�@+@�@
=@��@�y@ȴ@�+@ff@V@5?@@��@�h@`B@?}@�@V@�@j@�@�m@�m@��@��@��@�m@ƨ@��@�@S�@33@�@��@��@~�@=q@=q11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441111111111111111111441111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B$�B$�B$�B$�B$�B$�B$�B$�B$�B$�B$�B$�B$�B$�B%�B%�B%�B$�B$�B$�B%�B$�B$�B$�B$�B$�B#�B#�B"�B �B �B �B �B �B�B�B�B�BhB�B��BXB�B��B�qB��B��B�Bq�Bu�BhsB]/BF�B+B-B(�B&�B!�B�BbBDB	7BB
�B
�B
�B
�/B
��B
B
�RB
�3B
�B
�B
��B
��B
��B
�hB
�B
t�B
ffB
]/B
_;B
[#B
W
B
P�B
O�B
I�B
?}B
$�B
bB	��B	�B	�B	�B	�mB	�`B	�BB	��B	�B	�
B	�B	�B	��B	��B	��B	ǮB	�XB	�wB	�qB	�FB	�B	�B	�B	��B	��B	�VB	�1B	�B	�B	�B	�B	� B	{�B	u�B	jB	_;B	cTB	ZB	VB	O�B	P�B	P�B	P�B	M�B	F�B	?}B	=qB	33B	,B	-B	'�B	)�B	,B	(�B	$�B	$�B	 �B	�B	VB	
=B		7B	DB	\B	PB	B��B�B�NB�B�B�B�B�mB�ZB�BB�/B��B��B��B��B��B��BB��BĜBŢBĜB��B�}B�jB�XB�-B�B��B��B��B��B��B��B��B��B��B�\B�uB��B��B�hB�=B�B�B�JB�DB�7B�=B�7B�7B�1B�%B�B�B�B}�Bw�BjBgmBp�BiyBbNB`BB[#BcTB_;B\)BYBYBW
BP�BE�B?}BO�BS�BQ�BP�BR�BS�BP�BM�BN�BS�BR�BP�BO�BO�BK�BH�BH�BA�B9XB7LB>wB:^B8RB0!B2-B33B8RB9XB<jB;dB<jB:^B33B33B/B49B7LB49B49B1'B1'B.B/B7LB8RB8RB8RB;dB:^B:^B;dB;dB:^B9XB8RB6FB2-B(�B0!B:^B>wB>wB:^B7LB:^B7LB2-B2-B2-B8RB9XB2-B0!B?}BC�BD�BE�BF�BC�BA�BB�BC�BD�B?}B9XBK�BYB[#B\)B\)B\)B\)B\)B\)B\)B\)B[#B]/B]/B_;BaHBaHBdZBl�Bo�Bq�Bx�B~�B�B�1B�1B�7B�7B�1B�+B�%B�JB�JB�PB�hB�hB�bB�1B�VB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�!B�-B�FB�^B�jB�qB��BƨBǮB��B��B��B��B��B��B�B�B�B�#B�#B�NB�NB�B�B�B�B�B�B�B�B�B�B��B��B��B��B	B	B	B	B	B	%B	+B		7B	
=B	JB	uB	uB	oB	bB	{B	�B	�B	�B	�B	�B	�B	{B	�B	�B	�B	�B	�B	�B	�B	 �B	#�B	'�B	(�B	,B	.B	/B	0!B	0!B	2-B	33B	33B	49B	49B	49B	49B	5?B	8RB	8RB	7LB	<jB	=qB	<jB	;dB	=qB	A�B	@�B	K�B	M�B	O�B	O�B	R�B	T�B	VB	YB	]/B	_;B	`BB	cTB	ffB	gmB	ffB	l�B	v�B	x�B	y�B	{�B	|�B	|�B	~�B	�B	�B	�%B	�%B	�%B	�+B	�7B	�7B	�1B	�7B	�JB	�bB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�3B	�9B	�FB	�RB	�RB	�RB	�LB	�FB	�^B	�qB	�wB	��B	B	B	ÖB	ŢB	ŢB	ƨB	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�B	�B	�B	�B	�)B	�/B	�5B	�5B	�5B	�5B	�5B	�/B	�)B	�)B	�5B	�HB	�BB	�HB	�NB	�HB	�BB	�BB	�BB	�HB	�NB	�TB	�fB	�yB	�sB	�yB	�yB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
  B	��B	��B
B
B
B
B
B
B
B
%B
%B
%B
%B
%B
B
B
B
1B
1B
+B

=B
JB
JB
JB
\B
bB
bB
hB
hB
hB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
"�B
"�B
#�B
#�B
#�B
#�B
"�B
"�B
$�B
%�B
%�B
%�B
%�B
%�B
&�B
&�B
&�B
&�B
%�B
%�B
'�B
(�B
(�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
)�B
)�B
+B
+B
,B
,B
-B
/B
0!B
0!B
1'B
2-B
1'B
1'B
1'B
33B
49B
49B
49B
49B
5?B
6FB
6FB
6FB
6FB
6FB
6FB
5?B
49B
5?B
8RB
8RB
7LB
7LB
7LB
8RB
8RB
8RB
9XB
9XB
9XB
9XB
9XB
9XB
:^B
<jB
<jB
;dB
:^B
;dB
<jB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
>wB
?}B
?}B
?}B
?}B
?}B
@�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
C�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
F�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
K�B
L�B
L�B
M�B
M�B
M�B
M�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
N�B
M�B
M�B
O�B
P�B
O�B
O�B
P�B
P�B
O�B
O�B
P�B
P�B
Q�B
Q�B
P�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
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
XB
XB
XB
XB
XB
YB
YB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
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
]/B
]/B
]/B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
_;B
`BB
`BB
`BB
`BB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
`BB
`BB
`BB
aHB
bNB
bNB
bNB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
e`B
e`B
e`B
e`B
e`B
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
gmB
hsB
iyB
jB
jB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
jB
jB
k�B
k�B
k�B
l�B
l�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B$�B$�B$�B$�B$�B$�B$�B$�B$�B$�B$�B$�B$�B$�B%�B%�B%�B$�B$�B$�B%�B$�B$�B$�B$�B$�B#�B#�B"�B �B �B �B �B �B�B�B�B�B,B�`B��B^jB)yBٴB�{B��B�B�XBvFBw�Bk6B`vBKB0B/OB*�B'�B#BB�BdB
#B{B
�B
�B
��B
�pB
�B
��B
��B
��B
�/B
��B
��B
��B
��B
�@B
��B
v�B
i_B
^�B
`'B
[�B
W�B
Q�B
PHB
J�B
A�B
(�B
�B	�qB	�`B	�B	�wB	�>B	�B	�bB	�B	ևB	�?B	�B	�B	�2B	�&B	�\B	�B	�JB	��B	��B	�fB	��B	��B	�wB	�B	�)B	��B	�#B	��B	�B	��B	�[B	��B	|jB	v�B	l�B	a-B	d&B	[�B	W�B	Q B	Q�B	QhB	QNB	NVB	GzB	@�B	>BB	4�B	-�B	.IB	)DB	*�B	,�B	)�B	%�B	%`B	!HB	�B	B	�B	
�B	0B	�B	�B	YB�]B�B�tB�!B�B�"B�B�$B�,B�-B�OBևB�9B�HB�6B�TBοB�3BªB�9B�%B�B�B�B�B�*B�MB�IB�B��B�`B�XB��B�hB��B��B��B�B�FB�B��B�:B�^B�{B�3B��B��B��B��B��B��B��B��B��B��B��B~�Bx�Bm)BiBq'Bj�BdBb4B\�Bd&B`vB]�BZ�BZBW�BRTBH1BBBP�BT{BR�BQ�BSuBT�BQ�BN�BO�BT,BSuBQhBPHBP}BL�BI�BIlBCB;dB9$B?.B;dB9>B1�B3�B4nB9$B:xB=qB<PB=B;0B4�B4TB0�B4�B7�B5B4�B2-B2B/�B0�B8B9	B9	B8�B;�B:�B:�B;�B;�B:�B9�B8�B6�B3�B+B1�B:�B>�B>�B;JB8RB:�B8RB3�B3�B3�B9>B:DB3�B2aB@BDBE9BFBF�BDgBB[BCaBDgBE�BAUB;�BL�BYKB[qB\]B\]B\]B\xB\]B\]B\xB\xB[�B]~B]�B_�Ba�Ba�Bd�Bl�Bp;Br|ByXB}B��B�KB��B�lB�lB��B��B��B��B�PB��B��B��B�NB��B��B��B��B�	B�B�'B�HB�&B��B�B�B�B�>B�>B�KB��B�]B�iB�iB�}B��B��B��B�xB��B��B��B��B��B��B�"B��B�B�FB�MB�9BؓBٴBیB��B��B� B��B��B��B��B��B��B��B�B�-B�3B�?B�LB�lB�BB	B	B	3B	�B	SB	?B	EB		RB	
�B	�B	�B	�B	�B	 B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B		B	�B	 �B	#�B	(>B	)_B	,WB	.IB	/OB	0UB	0UB	2aB	3MB	3MB	4TB	4TB	4nB	4nB	5tB	8lB	8�B	7�B	<�B	=�B	<�B	;�B	=�B	BB	AUB	LB	NB	P.B	P.B	S&B	UMB	VSB	YKB	]dB	_pB	`vB	cTB	f�B	gmB	f�G�O�G�O�B	x�B	y�B	|B	}<B	}<B	.B	�AB	�MB	�YB	�tB	�tB	�zB	�lB	��B	��B	��B	��B	�}B	�hG�O�G�O�B	��B	�B	�B	��B	�&B	� B	�@B	�B	��G�O�B	�`B	��B	��B	�=B	�CB	�/B	�iB	�[B	�MB	�TB	�`B	��B	�lB	�lB	�fB	��B	�xB	��B	�wB	��B	ªB	ªB	��B	żB	��B	��B	��B	��B	��B	��B	�2B	�SB	�SB	�?B	�B	�B	�B	�+B	�1B	�QB	�B	�B	�OB	�OB	�5B	�OB	�jB	�~B	ܒB	ܒB	�jB	�bB	�B	�|B	�B	�B	�B	��B	�vB	�bB	�hB	�TB	�B	�B	��B	�B	�B	��B	��B	�B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	��G�O�B	�B	�B	�B	��B	��B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�$B	�	B	�B	�`B	�2B	�*B	�$B	�B	��B	�*B	�$B	�B	�B	�B	�B	��G�O�G�O�B
 B
 4B	�cB	�cB
AB
AB
GB
3B
9B
9B
9B
%B
%B
YB
?B
?B
SB
�B
mB
KB
fB
zB

XB
dB
~B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
B
�B
�B
 �B
 �B
 �B
 �B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
"�B
# B
$�B
%�B
&B
&B
&B
&B
'B
'B
'B
'B
&B
&B
($B
)B
)*B
)B
)*B
)B
*B
*B
*B
*0B
*0B
*KB
+6B
+6B
,WB
,WB
-CB
/OB
0UB
0UB
1AB
2aB
1[B
1[B
1[B
3MB
49B
49B
49B
4nB
5?B
6FB
6FB
6`B
6FB
6`B
6`B
5tB
4�B
5�B
8�B
8�B
7fB
7�B
7�B
8lB
8lB
8�B
9XB
9rB
9rB
9rB
9�B
9�B
:�B
<�B
<jB
;B
:�B
;�B
<�B
=�B
=�B
=�B
=�B
=�B
=�B
=�B
=�B
>�B
?�B
?�B
?�B
?�B
?�B
@�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
C�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
F�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
K�B
L�B
MB
M�B
M�B
M�B
M�B
MB
MB
MB
L�B
L�B
MB
MB
MB
N�B
N"B
NB
O�B
P�B
O�B
PB
P�B
P�B
O�B
PB
Q B
P�B
Q�B
RB
Q4B
SB
SB
SB
S&B
TB
T,B
TB
T,B
TB
T�B
U2B
UB
UB
V9B
VB
UB
U2B
V9B
W
B
W?B
X+B
XEB
X+B
X+B
X+B
YB
YB
Z7B
ZB
Z7B
ZB
ZQB
Z7B
Z7B
ZQB
Z7B
[#B
[=B
[=B
[=B
[=B
\CB
\]B
\CB
\CB
]IB
]/B
]/B
]/B
]IB
]IB
]IB
^OB
^OB
^OB
^jB
^OB
_VB
_VB
_pB
_pB
`vB
`\B
`vB
`vB
abB
aHB
aHB
aHB
aHB
abB
aHB
aHB
abB
abB
abB
abB
`vB
`\B
`\B
abB
bhB
bhB
b�B
cTB
cnB
cTB
cTB
cnB
cnB
cnB
cnB
c�B
c�B
e`B
ezB
ezB
ezB
ezB
f�B
f�B
ffB
ffB
f�B
f�B
f�B
f�B
g�B
g�B
g�B
g�B
g�B
g�B
g�B
h�B
h�B
h�B
h�B
g�B
h�B
i�B
jB
jB
i�B
i�B
i�B
i�B
i�B
i�B
i�B
i�B
j�B
j�B
k�B
k�B
k�B
l�B
l�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441111111111111111111441111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111144111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<>�<#�
<,1<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.06(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201712100035492017121000354920171210003549201806221234282018062212342820180622123428201804050430372018040504303720180405043037  JA  ARFMdecpA19c                                                                20171206093518  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20171206003520  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20171206003522  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20171206003523  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20171206003524  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20171206003524  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20171206003524  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20171206003524  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20171206003524  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20171206003524                      G�O�G�O�G�O�                JA  ARUP                                                                        20171206005549                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20171206153232  CV  JULD            G�O�G�O�F�՜                JM  ARGQJMQC2.0                                                                 20171206153232  CV  JULD_LOCATION   G�O�G�O�F�թ                JM  ARSQJMQC2.0                                                                 20171207000000  CF  PSAL_ADJUSTED_QCDp� D�@ G�O�                JM  ARSQJMQC2.0                                                                 20171207000000  CF  TEMP_ADJUSTED_QCDp� D�@ G�O�                JM  ARCAJMQC2.0                                                                 20171209153549  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20171209153549  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404193037  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622033428  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115121517                      G�O�G�O�G�O�                