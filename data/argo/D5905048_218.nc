CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-03-09T00:35:28Z creation;2018-03-09T00:35:32Z conversion to V3.1;2019-12-19T07:43:37Z update;     
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
_FillValue                 �  I(   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `|   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �x   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �\   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �X   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �<   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �T   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �p   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �p   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �p   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �p   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �    HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �@   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �P   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �T   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �d   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �h   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �l   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �pArgo profile    3.1 1.2 19500101000000  20180309003528  20200116221515  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0577_218                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @�Q��# 1   @�Q�q��@4[W>�6�d=���l�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�  A   A   AA��A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CC�fCF  CH  CJ�CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT�fDUfDU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D��3D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�G�@�{@�{A
=A@��A_
=A
=A��A��A��A��AυA߅A�A��BBBBB'B/B7B?BGBOBWB_BgBoBwBB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB�{B��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HC�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�
CE�CG�CJ
=CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC�C��RC��RC��C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RD |)D �)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�D|)D�)D|)D�)D|)D�)D	|)D	�)D
|)D
�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D��D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D |)D �)D!|)D!�)D"|)D"�)D#|)D#�)D$|)D$�)D%|)D%�)D&|)D&�)D'|)D'�)D(|)D(�)D)|)D)�)D*|)D*�)D+|)D+�)D,|)D,�)D-|)D-�)D.|)D.�)D/|)D/�)D0|)D0�)D1|)D1�)D2|)D2�)D3|)D3�)D4|)D4�)D5|)D5�)D6|)D6�)D7|)D7�)D8|)D8�)D9|)D9�)D:|)D:�)D;|)D;�)D<|)D<�)D=|)D=�)D>|)D>�)D?|)D?�)D@|)D@�)DA|)DA�)DB|)DB�)DC|)DC�)DD|)DD�)DE|)DE�)DF|)DF�)DG|)DG�)DH|)DH�)DI|)DI�)DJ|)DJ�)DK|)DK�)DL|)DL�)DM|)DM�)DN|)DN�)DO|)DO�)DP|)DP�)DQ|)DQ�)DR|)DR�)DS|)DS�)DT��DU�DU|)DU�)DV|)DV�)DW|)DW�)DX|)DX�)DY|)DY�)DZ|)DZ�)D[|)D[�)D\|)D\�)D]|)D]�)D^|)D^�)D_|)D_�)D`|)D`�)Da|)Da�)Db|)Db�)Dc|)Dc�)Dd|)Dd�)De|)De�)Df|)Df�)Dg|)Dg�)Dh|)Dh�)Di|)Di�)Dj|)Dj�)Dk|)Dk�)Dl|)Dl�)Dm|)Dm�)Dn|)Dn�)Do|)Do�)Dp|)Dp�)Dq|)Dq�)Dr|)Dr�)Ds|)Ds�)Dt|)Dt�)Du|)Du�)Dv|)Dv�)Dw|)Dw�)Dx|)Dx�)Dy|)Dy�)Dz|)Dz�)D{|)D{�)D||)D|�)D}|)D}�)D~|)D~�)D|)D�)D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�:�D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D���D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D¾D��D�>D�~DþD��D�>D�~DľD��D�>D�~DžD��D�>D�~DƾD��D�>D�~DǾD��D�>D�~DȾD��D�>D�~DɾD��D�>D�~DʾD��D�>D�~D˾D��D�>D�~D̾D��D�>D�~D;D��D�>D�~DξD��D�>D�~DϾD��D�>D�~DоD��D�>D�~DѾD��D�>D�~DҾD��D�>D�~DӾD��D�>D�~DԾD��D�>D�~DվD��D�>D�~D־D��D�>D�~D׾D��D�>D�~DؾD��D�>D�~DپD��D�>D�~DھD��D�>D�~D۾D��D�>D�~DܾD��D�>D�~DݾD��D�>D�~D޾D��D�>D�~D߾D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D��D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D��HD��HD��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A� �A��A� �A� �A� �A��A� �A�"�A�"�A�"�A�$�A�$�A�$�A�$�A�&�A� �A��A� �A� �A���A�^5AŃA�=qA�33A�"�A�VA�A�JA�Aě�A�O�A�&�A�"�A��HA��HA�ƨAüjAüjA�A�Q�A���A�XA�9XA�{A���A�+A�"�A��PA�1A�p�A��/A�-A�^5A�ĜA�JA���A�33A��#A�VA��7A�z�A� �A�oA�"�A�bNA��;A�jA��A�$�A�VA��A��\A��TA��A�hsA�=qA�;dA��7A�r�A��-A�^5A�O�A�E�A�A�A�/A��RA�\)A��RA�XA��A�?}A� �A��-A�VA�p�A�z�A��A�A��A�VA��RA�ȴA�A�K�A��A��PA�v�A�A��A�1A�JA�x�A��-A�VA|�uAwhsAt�As�7ArbNArM�Aq��Ap��Ap1Am��AjE�Ai�PAhr�AedZAdA�Ac�
Ac��Ac|�Ab�jA]��AYXAY�AY%AX��AX��AX��AXr�AX �AWG�AU��AS+AR(�AO�AM��ALr�AK"�AJ5?AHĜAG��AF��AD�jAC��AB��A@�!A>1'A<�A;oA:�\A9��A8��A5"�A3VA0�A/\)A,M�A*��A)
=A(JA&Q�A#hsA Q�A`BA1'A�PA�A�A�A�TA��AM�AbNA�/A�PA��A�
AI�AAl�A
=A~�A��Ax�A33A/A%A
�DA	C�AbNAdZA��AK�A��A33A��A n�A E�A $�@���@���@�t�@��@�@���@�;d@�!@�-@�Ĝ@�dZ@�x�@�@��H@���@�-@���@ᙚ@߮@�G�@�A�@�"�@ڰ!@ڗ�@�V@�z�@��H@�-@ԃ@��m@�K�@�33@�=q@Ѓ@�;d@�ȴ@·+@�J@�@ͩ�@́@�`B@̣�@�V@ə�@�hs@�z�@ǅ@�E�@�{@���@���@��/@���@�^5@�M�@�$�@��7@�V@���@���@��@��@��7@�G�@�/@��`@�b@�C�@��\@�v�@�n�@�n�@�ff@�{@�hs@���@�1@���@�"�@�ff@��@�@���@�V@���@���@�(�@��;@��@��@���@���@���@��!@��!@�n�@�$�@���@��^@�x�@�?}@�%@���@��@�b@���@�\)@���@�^5@���@�x�@�X@�&�@��7@�V@��j@��u@�&�@��@���@���@��\@�^5@�v�@�"�@�{@�V@��D@�A�@�1@��;@���@�|�@�t�@���@���@�I�@��F@�@���@���@�M�@�J@�`B@�?}@�&�@�V@�O�@��h@��-@�x�@��@�  @���@��F@���@���@��H@���@��#@��@�?}@���@��!@���@�ff@�v�@��+@�@�G�@�%@��@��j@�ƨ@��@�dZ@�dZ@�S�@�C�@��@���@��!@���@�5?@��@��@���@�`B@�7L@�&�@�/@�&�@��@���@�j@�A�@�9X@��@���@��+@�ff@�=q@��@��@���@� �@��@�b@���@��@�l�@��@��y@���@���@�v�@�ff@�=q@���@��#@���@��^@�&�@�Ĝ@�j@�Z@��@��m@��w@��@���@�K�@�;d@�"�@��H@���@�M�@�J@��#@���@��-@��h@�hs@�G�@�7L@��@��@��j@�r�@�Z@��@�r�@�bN@�Q�@�1@��@��@��@���@��F@���@���@�dZ@�33@�dZ@�@�ff@���@���@��h@�O�@��@�Ĝ@��j@��u@��@���@�;d@�@�o@�ȴ@�ff@�M�@�=q@�{@�@��@�O�@�7L@��@���@���@��u@�Z@�9X@���@�t�@�\)@�K�@�;d@��@��@���@��\@�v�@�^5@�E�@�{@��T@�hs@��`@��j@���@���@�j@�I�@�b@� �@��@�@\)@~�@~�@}�@}��@}@}p�@}�@|��@{�
@{S�@z��@z�!@yx�@x��@x1'@w�w@wl�@v�y@v�+@v��@v�R@v�y@w
=@v$�@up�@u?}@u�@uV@u�@t��@t(�@sS�@so@r�H@r�!@rn�@r=q@r-@r�@q��@qX@q%@pĜ@pr�@pQ�@pA�@pA�@pA�@pbN@o��@n�R@n��@n�+@nv�@n5?@m�@m@mO�@l��@l�@l��@l��@l(�@k��@j�H@jn�@i�#@i��@ix�@iX@i�@h�`@hĜ@h�u@h�@h�@h�@h�@hr�@hQ�@g�;@g�P@gK�@g�@f�y@f�R@f$�@f{@e�@e�@e`B@d�j@d��@d(�@cC�@b�!@a��@a��@a��@a��@a��@a��@aX@`r�@_��@_\)@_;d@^��@^�+@^{@]@]�-@]�@]/@\�@\�D@\(�@[�
@[S�@Z��@Z�\@Zn�@Z=q@Z�@Yx�@X�`@XĜ@X��@X �@X  @W�@W�w@W�@V�+@VV@U@T��@T�@S��@Sƨ@S��@SS�@So@R�@R��@R��@R��@R�\@R-@Q�7@Q&�@Q%@Q%@P��@P��@P�u@PbN@P �@O�w@O\)@O+@O
=@N��@N�@N�@Nȴ@N�R@Nv�@N{@M��@M@M��@M?}@L��@L�j@L��@LI�@L(�@L�@K�m@K�F@K��@K��@K��@K�@K�@KS�@K"�@J��@J^5@I�@I��@IX@H��@Hb@G�w@G�P@G\)@G�@F�R@F5?@Ep�@D�/@Dz�@D(�@C�
@C33@B��@BM�@BM�@B�@A��@Ahs@Ahs@Ahs@Ahs@Ahs@Ahs@Ahs@Ax�@A&�@@�`@@Ĝ@@�9@@��@@�@@bN@@A�@?�@?l�@>�y@>V@=�T@=p�@=`B@=`B@=`B@=?}@=?}@=�@=V@<�j@<I�@;��@;�
@;ƨ@;��@;�@:�@:�!@:�\@:�\@:^5@9&�@81'@7�w@7K�@7K�@7K�@7;d@7�@6�y@6�+@65?@5��@5p�@5`B@5V@4j@4I�@3ƨ@3S�@3"�@2�@2��@2��@2�!@2��@2~�@2-@1��@1hs@17L@1%@0��@0��@0�u@0r�@0A�@/�@/|�@/K�@/�@.�@.�+@.ff@.E�@.$�@.@-�@-�@-�T@-��@,�/@,I�@+�F@+C�@*��@*�\@*~�@*�@)�@)��@)��@)��@)��@)�7@)7L@)&�@(��@( �@'��@'K�@'�@&ȴ@&�R@&5?@%`B@$�/@$z�@$�@#��@#�
@#�F@#��@#�@#dZ@#33@#"�@#o@"�H@"n�@"M�@!��@!x�@!hs@!X@!G�@!7L@!7L@!�@!%@ ��@ ��@ �`@ ��@ Ĝ@ ��@ �@ r�@ bN@ Q�@ A�@ 1'@ 1'@ b@�@�P@+@ȴ@E�@{@�-@�@`B@O�@?}@�@�@�@��@z�@Z@�@�
@t�@o@�@��@��@=q@��@G�@��@�9@r�@A�@b@b@  @  @b@�@�w@�@�@�P@|�@l�@\)@�@�y@�R@ff@�T@@�-@�-@��@��@�-@��@O�@�@�@V@�@��@��@�j@9X@1@1@�m@�
@�F@t�@C�@C�@@�!@��@~�@n�@M�@=q@�@��@��@�7@x�@x�@X@�@�`@��@1'@  @�;@�;@�;@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A� �A��A� �A� �A� �A��A� �A�"�A�"�A�"�A�$�A�$�A�$�A�$�A�&�A� �A��A� �A� �A���A�^5AŃA�=qA�33A�"�A�VA�A�JA�Aě�A�O�A�&�A�"�A��HA��HA�ƨAüjAüjA�A�Q�A���A�XA�9XA�{A���A�+A�"�A��PA�1A�p�A��/A�-A�^5A�ĜA�JA���A�33A��#A�VA��7A�z�A� �A�oA�"�A�bNA��;A�jA��A�$�A�VA��A��\A��TA��A�hsA�=qA�;dA��7A�r�A��-A�^5A�O�A�E�A�A�A�/A��RA�\)A��RA�XA��A�?}A� �A��-A�VA�p�A�z�A��A�A��A�VA��RA�ȴA�A�K�A��A��PA�v�A�A��A�1A�JA�x�A��-A�VA|�uAwhsAt�As�7ArbNArM�Aq��Ap��Ap1Am��AjE�Ai�PAhr�AedZAdA�Ac�
Ac��Ac|�Ab�jA]��AYXAY�AY%AX��AX��AX��AXr�AX �AWG�AU��AS+AR(�AO�AM��ALr�AK"�AJ5?AHĜAG��AF��AD�jAC��AB��A@�!A>1'A<�A;oA:�\A9��A8��A5"�A3VA0�A/\)A,M�A*��A)
=A(JA&Q�A#hsA Q�A`BA1'A�PA�A�A�A�TA��AM�AbNA�/A�PA��A�
AI�AAl�A
=A~�A��Ax�A33A/A%A
�DA	C�AbNAdZA��AK�A��A33A��A n�A E�A $�@���@���@�t�@��@�@���@�;d@�!@�-@�Ĝ@�dZ@�x�@�@��H@���@�-@���@ᙚ@߮@�G�@�A�@�"�@ڰ!@ڗ�@�V@�z�@��H@�-@ԃ@��m@�K�@�33@�=q@Ѓ@�;d@�ȴ@·+@�J@�@ͩ�@́@�`B@̣�@�V@ə�@�hs@�z�@ǅ@�E�@�{@���@���@��/@���@�^5@�M�@�$�@��7@�V@���@���@��@��@��7@�G�@�/@��`@�b@�C�@��\@�v�@�n�@�n�@�ff@�{@�hs@���@�1@���@�"�@�ff@��@�@���@�V@���@���@�(�@��;@��@��@���@���@���@��!@��!@�n�@�$�@���@��^@�x�@�?}@�%@���@��@�b@���@�\)@���@�^5@���@�x�@�X@�&�@��7@�V@��j@��u@�&�@��@���@���@��\@�^5@�v�@�"�@�{@�V@��D@�A�@�1@��;@���@�|�@�t�@���@���@�I�@��F@�@���@���@�M�@�J@�`B@�?}@�&�@�V@�O�@��h@��-@�x�@��@�  @���@��F@���@���@��H@���@��#@��@�?}@���@��!@���@�ff@�v�@��+@�@�G�@�%@��@��j@�ƨ@��@�dZ@�dZ@�S�@�C�@��@���@��!@���@�5?@��@��@���@�`B@�7L@�&�@�/@�&�@��@���@�j@�A�@�9X@��@���@��+@�ff@�=q@��@��@���@� �@��@�b@���@��@�l�@��@��y@���@���@�v�@�ff@�=q@���@��#@���@��^@�&�@�Ĝ@�j@�Z@��@��m@��w@��@���@�K�@�;d@�"�@��H@���@�M�@�J@��#@���@��-@��h@�hs@�G�@�7L@��@��@��j@�r�@�Z@��@�r�@�bN@�Q�@�1@��@��@��@���@��F@���@���@�dZ@�33@�dZ@�@�ff@���@���@��h@�O�@��@�Ĝ@��j@��u@��@���@�;d@�@�o@�ȴ@�ff@�M�@�=q@�{@�@��@�O�@�7L@��@���@���@��u@�Z@�9X@���@�t�@�\)@�K�@�;d@��@��@���@��\@�v�@�^5@�E�@�{@��T@�hs@��`@��j@���@���@�j@�I�@�b@� �@��@�@\)@~�@~�@}�@}��@}@}p�@}�@|��@{�
@{S�@z��@z�!@yx�@x��@x1'@w�w@wl�@v�y@v�+@v��@v�R@v�y@w
=@v$�@up�@u?}@u�@uV@u�@t��@t(�@sS�@so@r�H@r�!@rn�@r=q@r-@r�@q��@qX@q%@pĜ@pr�@pQ�@pA�@pA�@pA�@pbN@o��@n�R@n��@n�+@nv�@n5?@m�@m@mO�@l��@l�@l��@l��@l(�@k��@j�H@jn�@i�#@i��@ix�@iX@i�@h�`@hĜ@h�u@h�@h�@h�@h�@hr�@hQ�@g�;@g�P@gK�@g�@f�y@f�R@f$�@f{@e�@e�@e`B@d�j@d��@d(�@cC�@b�!@a��@a��@a��@a��@a��@a��@aX@`r�@_��@_\)@_;d@^��@^�+@^{@]@]�-@]�@]/@\�@\�D@\(�@[�
@[S�@Z��@Z�\@Zn�@Z=q@Z�@Yx�@X�`@XĜ@X��@X �@X  @W�@W�w@W�@V�+@VV@U@T��@T�@S��@Sƨ@S��@SS�@So@R�@R��@R��@R��@R�\@R-@Q�7@Q&�@Q%@Q%@P��@P��@P�u@PbN@P �@O�w@O\)@O+@O
=@N��@N�@N�@Nȴ@N�R@Nv�@N{@M��@M@M��@M?}@L��@L�j@L��@LI�@L(�@L�@K�m@K�F@K��@K��@K��@K�@K�@KS�@K"�@J��@J^5@I�@I��@IX@H��@Hb@G�w@G�P@G\)@G�@F�R@F5?@Ep�@D�/@Dz�@D(�@C�
@C33@B��@BM�@BM�@B�@A��@Ahs@Ahs@Ahs@Ahs@Ahs@Ahs@Ahs@Ax�@A&�@@�`@@Ĝ@@�9@@��@@�@@bN@@A�@?�@?l�@>�y@>V@=�T@=p�@=`B@=`B@=`B@=?}@=?}@=�@=V@<�j@<I�@;��@;�
@;ƨ@;��@;�@:�@:�!@:�\@:�\@:^5@9&�@81'@7�w@7K�@7K�@7K�@7;d@7�@6�y@6�+@65?@5��@5p�@5`B@5V@4j@4I�@3ƨ@3S�@3"�@2�@2��@2��@2�!@2��@2~�@2-@1��@1hs@17L@1%@0��@0��@0�u@0r�@0A�@/�@/|�@/K�@/�@.�@.�+@.ff@.E�@.$�@.@-�@-�@-�T@-��@,�/@,I�@+�F@+C�@*��@*�\@*~�@*�@)�@)��@)��@)��@)��@)�7@)7L@)&�@(��@( �@'��@'K�@'�@&ȴ@&�R@&5?@%`B@$�/@$z�@$�@#��@#�
@#�F@#��@#�@#dZ@#33@#"�@#o@"�H@"n�@"M�@!��@!x�@!hs@!X@!G�@!7L@!7L@!�@!%@ ��@ ��@ �`@ ��@ Ĝ@ ��@ �@ r�@ bN@ Q�@ A�@ 1'@ 1'@ b@�@�P@+@ȴ@E�@{@�-@�@`B@O�@?}@�@�@�@��@z�@Z@�@�
@t�@o@�@��@��@=q@��@G�@��@�9@r�@A�@b@b@  @  @b@�@�w@�@�@�P@|�@l�@\)@�@�y@�R@ff@�T@@�-@�-@��@��@�-@��@O�@�@�@V@�@��@��@�j@9X@1@1@�m@�
@�F@t�@C�@C�@@�!@��@~�@n�@M�@=q@�@��@��@�7@x�@x�@X@�@�`@��@1'@  @�;@�;@�;@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
$�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
&�B
&�B
'�B
-B
0!B
/B
.B
F�B
��BDB@�BI�B\)Bw�B�PB��B�LB�RBŢB��B�)B�NB�TB�NB�TB�B��BBuB �B�BDB+B+B5?B0!B1'B@�B<jB5?B1'B+B(�B&�B!�B�B�B�B-B:^BB�BH�BL�BG�B/B"�B2-B�B�B)�B�B�BVB1B	7B��B�B��B%BB  B��B�B��B�B�B��B�qB�B�!B�B�uBw�Bn�B�B{�BbNBVBVBH�B;dB%�B
��B
�BB
�wB
��B
q�B
VB
R�B
=qB
%�B
PB	�mB	�B	��B	��B	��B	�B	�`B	�)B	ƨB	��B	�LB	��B	�{B	�uB	��B	��B	�VB	w�B	F�B	/B	aHB	dZB	bNB	_;B	[#B	W
B	N�B	?}B	-B	�B	�B	%B	1B	B	B��B��B�B�mB�
B�/B�BÖB�9B�XB�qB��B�XB�B�PB�bB�+B�VBx�B}�B~�Bx�Bo�BS�BXBiyBz�Bu�Bn�BaHBS�BT�B\)BffB_;BZBYB^5BVBM�BdZBaHBaHBaHB[#BffBe`BgmBaHBYBN�BP�BO�BP�BH�BE�BR�BJ�BL�B_;B^5BXBN�BM�BYBQ�BE�BL�B`BB_;B[#BT�BVBT�BYBN�BP�BP�BQ�BR�BT�BaHBdZBjBm�BiyB`BB`BBm�BhsBq�Br�Bt�Bo�Bk�Bs�Bz�B}�B|�B~�B� B~�B|�Bv�Bo�B�B�%B�B�%B�1B��B��B�oB�VB�JB��B��B��B��B��B��B��B��B��B��B�!B�'B�B�B�!B�LB��B��BBBB��BŢBŢB��B��B��B�B�B�B�B�;B�BB�;B�HB�HB�TB�mB�B��B��B��B��B��B��B��B��B��B	B	B	+B	+B		7B		7B	PB	{B	�B	�B	�B	�B	$�B	%�B	'�B	-B	2-B	33B	33B	-B	,B	5?B	8RB	>wB	9XB	6FB	;dB	A�B	D�B	G�B	I�B	K�B	P�B	YB	]/B	]/B	[#B	YB	^5B	aHB	`BB	cTB	e`B	iyB	k�B	n�B	q�B	u�B	v�B	v�B	w�B	~�B	�B	�B	�+B	�B	�B	�B	�JB	�VB	�VB	�DB	�%B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�-B	�9B	�3B	�3B	�?B	�RB	�LB	�LB	�^B	�jB	�qB	�wB	�qB	�}B	�}B	ĜB	ȴB	ǮB	ƨB	ȴB	��B	��B	ǮB	��B	��B	��B	��B	�
B	��B	�
B	�B	�B	�B	�B	�B	�)B	�;B	�BB	�BB	�NB	�TB	�NB	�;B	�BB	�NB	�mB	�mB	�sB	�sB	�yB	�yB	�yB	�B	�B	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B	��B	��B
B
B
B
B
B
B
B	��B	��B
B
+B
	7B
1B
+B

=B
DB
DB
	7B

=B
DB
PB
PB
PB
JB
JB
PB
VB
JB
bB
oB
oB
uB
oB
oB
uB
�B
�B
{B
{B
uB
uB
oB
bB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
!�B
!�B
"�B
"�B
"�B
!�B
!�B
"�B
#�B
#�B
$�B
%�B
%�B
%�B
$�B
#�B
"�B
%�B
%�B
%�B
$�B
$�B
%�B
%�B
%�B
&�B
%�B
%�B
#�B
#�B
#�B
%�B
%�B
'�B
'�B
(�B
'�B
(�B
(�B
(�B
)�B
)�B
+B
)�B
)�B
)�B
)�B
)�B
+B
+B
+B
+B
)�B
,B
+B
+B
+B
+B
-B
-B
+B
.B
-B
0!B
0!B
1'B
1'B
0!B
/B
-B
.B
0!B
1'B
1'B
0!B
1'B
1'B
2-B
2-B
1'B
2-B
1'B
2-B
2-B
2-B
33B
49B
5?B
5?B
5?B
33B
33B
6FB
6FB
5?B
7LB
7LB
6FB
5?B
5?B
7LB
6FB
5?B
6FB
:^B
:^B
:^B
:^B
:^B
;dB
;dB
;dB
;dB
;dB
:^B
9XB
;dB
<jB
=qB
=qB
<jB
=qB
<jB
<jB
<jB
<jB
>wB
>wB
>wB
>wB
?}B
>wB
>wB
=qB
=qB
>wB
?}B
>wB
=qB
>wB
?}B
?}B
?}B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
@�B
@�B
>wB
@�B
@�B
@�B
@�B
@�B
@�B
B�B
C�B
C�B
B�B
B�B
B�B
A�B
C�B
D�B
D�B
D�B
D�B
E�B
G�B
H�B
G�B
G�B
H�B
I�B
J�B
J�B
I�B
J�B
I�B
I�B
H�B
H�B
I�B
J�B
J�B
I�B
I�B
I�B
H�B
G�B
I�B
I�B
J�B
K�B
M�B
N�B
M�B
M�B
M�B
M�B
M�B
L�B
L�B
M�B
N�B
N�B
N�B
N�B
M�B
N�B
O�B
O�B
M�B
J�B
L�B
P�B
P�B
S�B
S�B
R�B
R�B
Q�B
Q�B
R�B
R�B
R�B
S�B
R�B
Q�B
S�B
S�B
S�B
T�B
VB
VB
W
B
W
B
VB
VB
VB
T�B
W
B
W
B
XB
XB
XB
W
B
XB
XB
XB
W
B
YB
YB
YB
YB
ZB
[#B
[#B
[#B
[#B
[#B
ZB
YB
XB
YB
ZB
[#B
\)B
]/B
]/B
]/B
^5B
^5B
^5B
_;B
_;B
^5B
^5B
^5B
]/B
\)B
]/B
_;B
_;B
_;B
`BB
^5B
]/B
`BB
`BB
bNB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
dZB
dZB
cTB
bNB
dZB
cTB
cTB
e`B
ffB
ffB
ffB
ffB
ffB
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
ffB
ffB
e`B
e`B
ffB
ffB
gmB
gmB
hsB
iyB
iyB
iyB
iyB
iyB
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
jB
jB
iyB
jB
l�B
l�B
m�B
m�B
n�B
o�B
n�B
o�B
o�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
n�B
n�B
n�B
n�B
n�B
n�B
p�B
q�B
q�B
q�B
q�B
q�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
p�B
q�B
r�B
r�B
r�B
r�B
q�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
s�B
t�B
t�B
s�B
u�B
v�B
v�B
v�B
w�B
w�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
$�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
&�B
&�B
($B
-B
0!B
/OB
.�B
H�B
�]BxB@iBI�B\]Bw�B�PB�$B��B�	B��B�BܬB�NB�B�hB�B��B��B9BFB!B~B"B,�B-B6`B1vB2�BA�B=�B6�B2|B,�B*B'�B"�B#B�BOB/�B<�BD�BJrBNBIlB2�B&�B4TB"NBB+�B �BpB B
rBB��B��B��B?BMB 4B�RB�"BԯB�eB��B� B�.B�cB�AB�WB�B{�BqvB�AB|�BezBW�BXBJ�B=�B)�B
�B
�B
ªB
� B
xB
Z�B
V�B
A�B
)�B
TB	�]B	�ZB	�rB	��B	�B	�B	�B	ݲB	ɠB	��B	�RB	��B	��B	��B	�/B	�+B	�(B	zB	L�B	3�B	a�B	dtB	bhB	_�B	[qB	WsB	O�B	@�B	/iB	QB	VB		lB	
XB	�B	�B	 OB��B��B�_B�eBޞB��BƎB�fB��B��B�uB��B�OB��B�@B�=B��B|�B�4B� Bz�BrGBXB[�Bj�B|PBv�Bp;Bc�BV9BV�B]�Bg�Ba�B\)BZ�B_;BW�BO�Bd�Bb4BbBbB\)Bf�Be�Bg�Ba�BZBP�BRTBQhBRTBJ�BG�BS�BL�BN<B_VB^�BX�BPbBO\BY�BS@BH1BNpB`�B_�B\CBVSBWYBVSBZBP�BRoBR�BS�BTaBV�Ba�BeBj�Bm�BjBa�Ba|Bn/BiyBrBsMBu%BpoBl�Bt�B{0B~BB}VB.B�4B.B}VBw�BqAB�oB�tB��B��B�B��B��B��B�BB��B�)B�B�B�\B�-B�bB�pB�pB�hB�eB�UB�[B��B��B��B��B��B��BªB��B��B�'B�B�?B�0B�4B�vB�9B�EB�yBٴBߊB�BߤB�B�B�B�B�B��B��B��B�B��B�B�B�(B�HB	;B	[B	_B	�B		�B		�B	�B	�B	�B	�B	�B	�B	$�B	&2B	($B	-CB	2B	3�B	3�B	-�B	,�B	5ZB	8lB	>]B	:*B	7B	;�B	A�B	D�B	G�B	I�B	K�B	QB	YB	\�B	]IB	[�B	Y�B	^�B	a|B	`�B	c�B	e�B	i�B	k�B	n�B	q�B	u�B	v�B	wB	x�B	HB	�-B	�3B	�+B	�mB	��B	��B	�JB	��B	��B	�0B	�+B	��B	��B	��B	��B	�CB	�B	��B	�$B	�mB	��B	�KB	�"B	�B	�/B	�/B	�]B	�GB	�TB	�hB	��B	�tB	��B	��B	��B	�xB	��B	�qB	��B	��B	��B	��B	ĶB	��B	�B	�+B	�B	��B	��B	�1B	�)B	�B	�HB	�B	�$B	�MB	�?B	�+B	�9B	�EB	�7B	�QB	�CB	�VB	�\B	�vB	�hB	�B	�B	ߤB	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	��B	�B	�B	�B	�B	�.B	�B
B
oB	��B	�qB
B
3B
AB
[B
AB
-B
;B	�cB	�}B
GB
_B
	7B
�B
zB

rB
^B
xB
	�B

�B
xB
jB
jB
jB
~B
~B
�B
�B
�B
}B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
B
�B
�B
�B
�B
�B
B
B
 �B
!�B
!�B
!�B
"�B
"�B
#B
!�B
!�B
"�B
#�B
#�B
$�B
%�B
%�B
%�B
$�B
$&B
# B
%�B
%�B
&B
$�B
$�B
&B
&2B
%�B
&�B
%�B
%�B
$B
$&B
$@B
&B
&2B
(
B
(
B
)*B
(
B
)*B
)*B
)B
)�B
*B
+B
)�B
*B
*B
*0B
*B
+6B
+6B
+6B
+6B
*0B
,B
+B
+6B
+6B
+QB
-CB
-CB
+kB
.IB
-CB
0UB
0UB
1'B
1'B
0;B
/OB
-]B
.IB
0;B
1AB
1AB
0UB
1[B
1AB
2aB
2GB
1vB
2GB
1[B
2aB
2|B
2aB
3hB
4nB
5ZB
5ZB
5ZB
3hB
3hB
6`B
6`B
5�B
7�B
7fB
6`B
5tB
5tB
7fB
6zB
5�B
6�B
:�B
:xB
:xB
:xB
:xB
;B
;�B
;�B
;dB
;B
:�B
9�B
;B
<�B
=qB
=�B
<�B
=�B
<�B
<�B
<�B
<�B
>�B
>�B
>�B
>�B
?}B
>�B
>�B
=�B
=�B
>�B
?}B
>�B
=�B
>�B
?�B
?�B
?�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
@�B
@�B
>�B
@�B
@�B
@�B
@�B
@�B
@�B
B�B
C�B
C�B
B�B
B�B
B�B
A�B
C�B
D�B
D�B
D�B
D�B
E�B
G�B
H�B
G�B
G�B
H�B
I�B
J�B
J�B
I�B
J�B
I�B
I�B
H�B
H�B
I�B
J�B
J�B
I�B
I�B
I�B
H�B
G�B
J	B
J	B
J�B
K�B
M�B
N�B
M�B
M�B
M�B
M�B
M�B
L�B
MB
NB
N�B
N�B
OB
N�B
N"B
N�B
O�B
PB
NB
KDB
M6B
Q4B
QB
S�B
S�B
SB
SB
RB
R B
SB
S@B
SB
TB
S&B
R:B
T,B
TFB
TFB
UB
VB
VB
W
B
W$B
V9B
VB
VB
U2B
W?B
W$B
X+B
X+B
XEB
W$B
XEB
X+B
X+B
W?B
YKB
YKB
YKB
Y1B
ZQB
[WB
[=B
[=B
[=B
[#B
Z7B
YKB
X_B
YeB
ZQB
[WB
\]B
]dB
]dB
]IB
^OB
^OB
^jB
_VB
_;B
^OB
^jB
^OB
]dB
\xB
]~B
_VB
_pB
_pB
`\B
^jB
]~B
`�B
`vB
bhB
cnB
c�B
cnB
cTB
c�B
cnB
cnB
dZB
d�B
c�B
b�B
dtB
c�B
c�B
ezB
ffB
f�B
ffB
ffB
f�B
ffB
f�B
ffB
ffB
f�B
ffB
f�B
f�B
gmB
gmB
g�B
gmB
g�B
gmB
f�B
f�B
e�B
e�B
f�B
f�B
g�B
g�B
h�B
iyB
i�B
iyB
i�B
i�B
i�B
iyB
i�B
i�B
i�B
i�B
i�B
i�B
j�B
j�B
j�B
j�B
i�B
j�B
l�B
l�B
m�B
m�B
n�B
o�B
n�B
o�B
o�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
n�B
n�B
n�B
n�B
n�B
n�B
p�B
q�B
q�B
q�B
q�B
q�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
p�B
q�B
r�B
r�B
r�B
r�B
q�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
s�B
t�B
t�B
s�B
u�B
v�B
v�B
v�B
w�B
w�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.06(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201803130050362018031300503620180313005036201806221327222018062213272220180622132722201804050731142018040507311420180405073114  JA  ARFMdecpA19c                                                                20180309093519  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180309003528  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180309003530  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180309003531  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180309003531  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180309003531  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180309003531  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180309003531  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180309003532  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180309003532                      G�O�G�O�G�O�                JA  ARUP                                                                        20180309005529                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180312154839  CV  JULD            G�O�G�O�F�                JM  ARCAJMQC2.0                                                                 20180312155036  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180312155036  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404223114  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622042722  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116221515                      G�O�G�O�G�O�                