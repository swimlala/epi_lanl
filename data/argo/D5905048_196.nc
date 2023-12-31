CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-01-02T00:35:31Z creation;2018-01-02T00:35:35Z conversion to V3.1;2019-12-19T07:48:41Z update;     
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
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \|   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `\   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �<   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �|   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �\   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �\   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �l   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �p   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �t   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �x   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �|   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20180102003531  20200116221515  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0577_196                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @�Av� 1   @�Av�l @4_��o �dva|�Q1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @@  @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5�fD6fD6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DL��DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D�|�D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@<(�@|(�@�{@�{A
=A?
=A_
=A
=A��A��A��A��AυA߅A�A��BBBBB'B/B7B?BGBOBWB_BgBoBwBB��HB��HB��HB��HB��HB��HB��HB��HB��B��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HC�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RD |)D �)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D	|)D	�)D
|)D
�)D|)D�)D|)D��D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D |)D �)D!|)D!�)D"|)D"�)D#|)D#�)D$|)D$�)D%|)D%�)D&|)D&�)D'|)D'�)D(|)D(�)D)|)D)�)D*|)D*�)D+|)D+�)D,|)D,�)D-|)D-�)D.|)D.�)D/|)D/�)D0|)D0�)D1|)D1�)D2|)D2�)D3|)D3�)D4|)D4�)D5��D6�D6|)D6�)D7|)D7�)D8|)D8�)D9|)D9�)D:|)D:�)D;|)D;�)D<|)D<�)D=|)D=�)D>|)D>�)D?|)D?�)D@|)D@�)DA|)DA�)DB|)DB�)DC|)DC�)DD|)DD�)DE|)DE�)DF|)DF�)DG|)DG�)DH|)DH�)DI|)DI�)DJ|)DJ�)DK|)DK�)DL|)DL��DM|)DM�)DN|)DN�)DO|)DO�)DP|)DP�)DQ|)DQ�)DR|)DR�)DS|)DS�)DT|)DT�)DU|)DU�)DV|)DV�)DW|)DW�)DX|)DX�)DY|)DY�)DZ|)DZ�)D[|)D[�)D\|)D\�)D]|)D]�)D^|)D^�)D_|)D_�)D`|)D`�)Da|)Da�)Db|)Db�)Dc|)Dc�)Dd|)Dd�)De|)De�)Df|)Df�)Dg|)Dg�)Dh|)Dh�)Di|)Di�)Dj|)Dj�)Dk|)Dk�)Dl|)Dl�)Dm|)Dm�)Dn|)Dn�)Do|)Do�)Dp|)Dp�)Dq|)Dq�)Dr|)Dr�)Ds|)Ds�)Dt|)Dt�)Du|)Du�)Dv|)Dv�)Dw|)Dw�)Dx|)Dx�)Dy|)Dy�)Dz|)Dz�)D{|)D{�)D||)D|�)D}|)D}�)D~|)D~�)D|)D�)D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�:�D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D���D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�:�D�z�D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D¾D��D�>D�~DþD��D�>D�~DľD��D�>D�~DžD��D�>D�~DƾD��D�>D�~DǾD��D�>D�~DȾD��D�>D�~DɾD��D�>D�~DʾD��D�>D�~D˾D��D�>D�~D̾D��D�>D�~D;D��D�>D�~DξD��D�>D�~DϾD��D�>D�~DоD��D�>D�~DѾD��D�>D�~DҾD��D�>D�~DӾD��D�>D�~DԾD��D�>D�~DվD��D�>D�~D־D��D�>D�z�D׾D��D�>D�~DؾD��D�>D�~DپD��D�>D�~DھD��D�>D�~D۾D��D�>D�~DܾD��D�>D�~DݾD��D�>D�~D޾D��D�>D�~D߾D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D��D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D��D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D���D��H11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111AʸRAʺ^AʼjAʸRAʸRAʸRAʸRAʺ^AʼjAʾwA�A���AʾwAʼjAʾwA���A�A�Aʺ^AʾwA�A���A���A��
A��yA��yA��A�%A�
=A��A�E�A�bNA�~�AˍPA˗�A˟�Aˣ�AˬA˴9A�7LA�M�A��A���Aę�A� �A��TAöFA��#A�-Aĕ�AăA�(�A��DA�v�A�VA���A�~�A��PA��
A���A���A��A�(�A��#A�K�A��^A�t�A�=qA�&�A� �A��
A�33A��
A���A��A��yA��/A��A�ȴA���A�M�A���A��A��A���A��A���A��RA��PA���A��A��A��A��A�A�9XA��A�9XA���A�A��7A���A�E�A�;dA��yA�7LA��\A��#A��9A��HA�E�A�^5A��9A}`BA{�Ax5?Av^5As�TAsXAtAt~�At1Ar9XAo�#AkdZAg��Af(�Ae��Ae�Adn�Ab�+A`bNA^�A\��A\JA[AXffAV��AT=qAR��AP�\AN�AL9XAI��AG��AE;dAC�^AB�AB�A@5?A=VA<=qA;S�A9�wA8ZA7G�A5+A3�mA2^5A0��A/��A.bNA.�A-�A-C�A,ZA*��A*�+A*Q�A*I�A)��A(^5A%hsA#�mA"��A!�#A �9A�7A�A�jA �A&�A��A�A�#A�9A��Al�A�HAbNA��A�^A��A�uA�^A?}A��A
ZA	7LAr�A{A�A�A�+AbNA5?AhsA~�A1'A�A�A��A�!A��A �DA 1@�K�@�=q@�&�@�ƨ@�n�@�?}@��@���@�hs@�C�@��@�Q�@�5?@� �@�o@���@�dZ@�+@旍@��T@�S�@���@�Q�@��@�n�@��@۶F@�ff@��@�x�@��@���@؛�@���@��@��#@���@�1'@�C�@�v�@�p�@мj@�9X@�1@��
@�S�@��@���@�E�@Ͳ-@��@�1'@�1@�;d@���@ȓu@�r�@�A�@Ɨ�@ŉ7@�7L@ģ�@þw@�l�@���@�@�5?@���@��`@�r�@�9X@��@���@�S�@���@�O�@�Ĝ@���@�Z@���@���@�@��-@�O�@��`@�j@��F@�|�@�C�@�@���@�n�@�n�@���@��/@��@�bN@�ƨ@��@�t�@��@���@�^5@���@��h@�hs@�%@�z�@�1@�C�@��@��@�V@���@��@�Z@�1'@� �@��@�
=@�ff@�=q@�-@�J@��-@��`@�j@��@�z�@��@�(�@���@�33@��y@�5?@�hs@�G�@���@��@���@�r�@�b@��@��y@��y@�\)@���@��;@�b@���@�|�@���@�V@���@�%@�I�@��@��@���@���@���@�t�@���@��+@�M�@�@��@�%@�Ĝ@��u@�z�@�j@�Z@�I�@�bN@�bN@�r�@��D@�z�@�1'@��w@��@�K�@�o@��@�o@���@��+@�ff@�=q@�E�@�$�@���@��h@�O�@���@��j@��u@�z�@�bN@�Q�@�A�@�9X@�1@���@�;d@��@��@��!@�ff@��@��^@���@�x�@�X@�7L@�%@�z�@�1'@� �@�b@�1@��@��;@��;@���@��w@��P@�|�@�S�@�"�@���@�~�@��@���@�&�@���@��@��D@�I�@�b@���@�dZ@�K�@�;d@�"�@��@���@�ff@�-@�J@���@�p�@�?}@�%@���@��9@�j@�Q�@�1'@���@��@�33@�
=@���@�^5@�{@��^@��h@�7L@��@�%@�Ĝ@�Z@� �@�1@���@��@��;@�ƨ@��@���@�t�@�K�@�33@�33@�"�@�
=@���@��@���@�v�@�{@�@���@�x�@��@��/@��@��u@��u@��@�Z@�b@��m@��;@��m@���@��P@�\)@��!@���@�=q@�{@���@��@���@���@�7L@��`@��@��D@�Z@�  @l�@;d@
=@~�@~$�@~@}�-@|�j@|�@{�
@{��@{t�@z��@z=q@y��@y��@yX@y�@xA�@w�P@v��@v�+@u��@u?}@t�@t�@tZ@t9X@s�
@s��@sdZ@r^5@q�7@q7L@q�@p�`@pĜ@p�u@o|�@o
=@n��@n�y@n�@n��@n�+@nff@n5?@m��@m`B@l�/@lz�@lj@l1@k��@k33@j��@j~�@j�@i��@i��@i��@ix�@i�@h�u@hbN@h1'@g��@g��@gl�@f�R@f{@e��@e/@d�/@d��@d1@c"�@c@b��@bM�@a��@ahs@a7L@`��@`�`@`�9@`r�@`Q�@`Q�@`1'@_�;@_
=@^�@^ff@]�-@\��@\�j@\Z@[��@[t�@[t�@[t�@[dZ@[dZ@[S�@["�@Z�@ZM�@Y�#@Y�7@YX@X�`@XA�@W��@Wl�@WK�@W�@V��@Vv�@V{@U�h@U/@UV@T�@T�@Tz�@T9X@S�
@SdZ@SS�@S"�@R�\@RJ@Q�#@Q�#@Q��@Q�^@Q�^@Q��@Q�7@Qhs@P��@P�`@P�`@PĜ@P�@PbN@PQ�@PA�@P �@O��@O;d@O
=@N�y@N�@N�R@N��@N�+@Nff@M�@M�T@M��@Mp�@M/@L�@Lj@L1@K�F@KC�@J�@J�H@Jn�@JJ@I��@I�^@I�7@H��@H��@H��@H��@H�u@Hr�@H  @G��@G�P@Gl�@GK�@F��@F�R@E�@EO�@EV@D�/@D�j@D�@D�@C�F@CS�@B��@BM�@BJ@A��@Ax�@A&�@@�9@@ �@?l�@>��@>ff@>@=�@=�T@=��@=O�@<�/@<j@;��@;��@;33@:~�@9��@9��@9X@9X@9G�@8��@8�u@8A�@8  @7��@7��@7l�@7;d@7�@7
=@6ȴ@6v�@6E�@5��@5?}@4�@4��@4z�@4I�@4(�@41@3��@3"�@2��@2~�@2=q@2�@2J@1�@1��@1hs@1G�@1&�@1%@0Ĝ@0��@0�u@0Q�@/�@/K�@/
=@.�y@.ȴ@.�+@.E�@.{@-�T@-��@-��@-�h@-p�@-p�@-O�@-?}@-�@,�@,9X@+��@+"�@*�H@*��@*�\@*n�@*�@)�7@)hs@)G�@(��@(��@(��@(��@(bN@(Q�@( �@(b@'��@'|�@';d@&ȴ@&��@&�+@&ff@&$�@&@%@%@%�-@%�@%V@$�/@$��@$�D@$I�@$�@#�
@#�F@#C�@#o@#@#@"�@"��@"-@!�#@!��@!�^@!%@ ��@ �u@ 1'@ b@�P@
=@�R@v�@E�@$�@{@�h@`B@�@V@��@�/@z�@�@�@�@�@1@�
@t�@C�@�H@��@�\@^5@n�@n�@^5@^5@�@�@��@��@��@��@x�@x�@hs@hs@&�@��@Ĝ@�u@�@r�@bN@�@�@��@v�@ff@V@E�@E�@5?@{@�@��@@��@p�@O�@�@��@�/@�@j@(�@��@��@�m@�@S�@"�@o@�@�!@�\@n�@=q@J@��@�@�#@��@��@��@hs@hs@X@G�@7L@�@%@%@%@%@�`@Ĝ@��@r�@Q�@b@�@l�@\)@K�@;d@�@�@�R@��@v�@V@ff11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111AʸRAʺ^AʼjAʸRAʸRAʸRAʸRAʺ^AʼjAʾwA�A���AʾwAʼjAʾwA���A�A�Aʺ^AʾwA�A���A���A��
A��yA��yA��A�%A�
=A��A�E�A�bNA�~�AˍPA˗�A˟�Aˣ�AˬA˴9A�7LA�M�A��A���Aę�A� �A��TAöFA��#A�-Aĕ�AăA�(�A��DA�v�A�VA���A�~�A��PA��
A���A���A��A�(�A��#A�K�A��^A�t�A�=qA�&�A� �A��
A�33A��
A���A��A��yA��/A��A�ȴA���A�M�A���A��A��A���A��A���A��RA��PA���A��A��A��A��A�A�9XA��A�9XA���A�A��7A���A�E�A�;dA��yA�7LA��\A��#A��9A��HA�E�A�^5A��9A}`BA{�Ax5?Av^5As�TAsXAtAt~�At1Ar9XAo�#AkdZAg��Af(�Ae��Ae�Adn�Ab�+A`bNA^�A\��A\JA[AXffAV��AT=qAR��AP�\AN�AL9XAI��AG��AE;dAC�^AB�AB�A@5?A=VA<=qA;S�A9�wA8ZA7G�A5+A3�mA2^5A0��A/��A.bNA.�A-�A-C�A,ZA*��A*�+A*Q�A*I�A)��A(^5A%hsA#�mA"��A!�#A �9A�7A�A�jA �A&�A��A�A�#A�9A��Al�A�HAbNA��A�^A��A�uA�^A?}A��A
ZA	7LAr�A{A�A�A�+AbNA5?AhsA~�A1'A�A�A��A�!A��A �DA 1@�K�@�=q@�&�@�ƨ@�n�@�?}@��@���@�hs@�C�@��@�Q�@�5?@� �@�o@���@�dZ@�+@旍@��T@�S�@���@�Q�@��@�n�@��@۶F@�ff@��@�x�@��@���@؛�@���@��@��#@���@�1'@�C�@�v�@�p�@мj@�9X@�1@��
@�S�@��@���@�E�@Ͳ-@��@�1'@�1@�;d@���@ȓu@�r�@�A�@Ɨ�@ŉ7@�7L@ģ�@þw@�l�@���@�@�5?@���@��`@�r�@�9X@��@���@�S�@���@�O�@�Ĝ@���@�Z@���@���@�@��-@�O�@��`@�j@��F@�|�@�C�@�@���@�n�@�n�@���@��/@��@�bN@�ƨ@��@�t�@��@���@�^5@���@��h@�hs@�%@�z�@�1@�C�@��@��@�V@���@��@�Z@�1'@� �@��@�
=@�ff@�=q@�-@�J@��-@��`@�j@��@�z�@��@�(�@���@�33@��y@�5?@�hs@�G�@���@��@���@�r�@�b@��@��y@��y@�\)@���@��;@�b@���@�|�@���@�V@���@�%@�I�@��@��@���@���@���@�t�@���@��+@�M�@�@��@�%@�Ĝ@��u@�z�@�j@�Z@�I�@�bN@�bN@�r�@��D@�z�@�1'@��w@��@�K�@�o@��@�o@���@��+@�ff@�=q@�E�@�$�@���@��h@�O�@���@��j@��u@�z�@�bN@�Q�@�A�@�9X@�1@���@�;d@��@��@��!@�ff@��@��^@���@�x�@�X@�7L@�%@�z�@�1'@� �@�b@�1@��@��;@��;@���@��w@��P@�|�@�S�@�"�@���@�~�@��@���@�&�@���@��@��D@�I�@�b@���@�dZ@�K�@�;d@�"�@��@���@�ff@�-@�J@���@�p�@�?}@�%@���@��9@�j@�Q�@�1'@���@��@�33@�
=@���@�^5@�{@��^@��h@�7L@��@�%@�Ĝ@�Z@� �@�1@���@��@��;@�ƨ@��@���@�t�@�K�@�33@�33@�"�@�
=@���@��@���@�v�@�{@�@���@�x�@��@��/@��@��u@��u@��@�Z@�b@��m@��;@��m@���@��P@�\)@��!@���@�=q@�{@���@��@���@���@�7L@��`@��@��D@�Z@�  @l�@;d@
=@~�@~$�@~@}�-@|�j@|�@{�
@{��@{t�@z��@z=q@y��@y��@yX@y�@xA�@w�P@v��@v�+@u��@u?}@t�@t�@tZ@t9X@s�
@s��@sdZ@r^5@q�7@q7L@q�@p�`@pĜ@p�u@o|�@o
=@n��@n�y@n�@n��@n�+@nff@n5?@m��@m`B@l�/@lz�@lj@l1@k��@k33@j��@j~�@j�@i��@i��@i��@ix�@i�@h�u@hbN@h1'@g��@g��@gl�@f�R@f{@e��@e/@d�/@d��@d1@c"�@c@b��@bM�@a��@ahs@a7L@`��@`�`@`�9@`r�@`Q�@`Q�@`1'@_�;@_
=@^�@^ff@]�-@\��@\�j@\Z@[��@[t�@[t�@[t�@[dZ@[dZ@[S�@["�@Z�@ZM�@Y�#@Y�7@YX@X�`@XA�@W��@Wl�@WK�@W�@V��@Vv�@V{@U�h@U/@UV@T�@T�@Tz�@T9X@S�
@SdZ@SS�@S"�@R�\@RJ@Q�#@Q�#@Q��@Q�^@Q�^@Q��@Q�7@Qhs@P��@P�`@P�`@PĜ@P�@PbN@PQ�@PA�@P �@O��@O;d@O
=@N�y@N�@N�R@N��@N�+@Nff@M�@M�T@M��@Mp�@M/@L�@Lj@L1@K�F@KC�@J�@J�H@Jn�@JJ@I��@I�^@I�7@H��@H��@H��@H��@H�u@Hr�@H  @G��@G�P@Gl�@GK�@F��@F�R@E�@EO�@EV@D�/@D�j@D�@D�@C�F@CS�@B��@BM�@BJ@A��@Ax�@A&�@@�9@@ �@?l�@>��@>ff@>@=�@=�T@=��@=O�@<�/@<j@;��@;��@;33@:~�@9��@9��@9X@9X@9G�@8��@8�u@8A�@8  @7��@7��@7l�@7;d@7�@7
=@6ȴ@6v�@6E�@5��@5?}@4�@4��@4z�@4I�@4(�@41@3��@3"�@2��@2~�@2=q@2�@2J@1�@1��@1hs@1G�@1&�@1%@0Ĝ@0��@0�u@0Q�@/�@/K�@/
=@.�y@.ȴ@.�+@.E�@.{@-�T@-��@-��@-�h@-p�@-p�@-O�@-?}@-�@,�@,9X@+��@+"�@*�H@*��@*�\@*n�@*�@)�7@)hs@)G�@(��@(��@(��@(��@(bN@(Q�@( �@(b@'��@'|�@';d@&ȴ@&��@&�+@&ff@&$�@&@%@%@%�-@%�@%V@$�/@$��@$�D@$I�@$�@#�
@#�F@#C�@#o@#@#@"�@"��@"-@!�#@!��@!�^@!%@ ��@ �u@ 1'@ b@�P@
=@�R@v�@E�@$�@{@�h@`B@�@V@��@�/@z�@�@�@�@�@1@�
@t�@C�@�H@��@�\@^5@n�@n�@^5@^5@�@�@��@��@��@��@x�@x�@hs@hs@&�@��@Ĝ@�u@�@r�@bN@�@�@��@v�@ff@V@E�@E�@5?@{@�@��@@��@p�@O�@�@��@�/@�@j@(�@��@��@�m@�@S�@"�@o@�@�!@�\@n�@=q@J@��@�@�#@��@��@��@hs@hs@X@G�@7L@�@%@%@%@%@�`@Ĝ@��@r�@Q�@b@�@l�@\)@K�@;d@�@�@�R@��@v�@V@ff11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�B�B�B�B�B�B �B!�B!�B"�B"�B!�B �B �B �B!�B �B �B �B!�B#�B&�B'�B+B0!B0!B6FB:^B=qBD�BS�B]/BgmBk�Bo�Bu�Bx�B�PB��B��B�B�mB�HB�B��B��BB�B�B �B��B��B�}B�/B�B�`B��B��B��B��B+B�B%�B!�B!�B&�B$�B�BVB�BoB��B�B��B�B�ZB�5B�
BƨB��BÖB�wB�B��B�=B�DBz�Bk�B[#BXB<jBXBdZBk�Bu�B�7B��B�hB� B~�BcTBH�BhB�B�B
��B
�ZB
�qB
�\B
E�B	�B	�qB	��B	��B	��B	��B	�oB	��B	ǮB	�`B	�BB	��B	�9B	�{B	n�B	�B	��B	��B	�JB	u�B	jB	^5B	_;B	bNB	ZB	@�B	?}B	,B	(�B	�B	PB��B�B�sB�/B�#B�/B�
BƨB�BÖB�wB�!B�B��B��B��B��B�hB�oB�PB��B��B�oB�JB�%B�\B�bB�VB�Bm�BXBgmBjBn�BgmBffB^5BaHBjBbNBe`BR�BZBT�BP�BI�BW
BVBN�BM�BT�BT�BQ�BT�BN�B:^BH�BM�BS�BJ�BZB[#BZBW
BO�BL�BVBW
BS�BN�BT�BL�BJ�BT�BW
BS�BT�BO�BQ�BW
BP�BP�BZBS�BP�B]/BS�BP�BYBVBW
BcTB_;BZBQ�BO�BR�BS�BZBT�BS�BT�B_;B]/B`BBaHB_;B\)B\)B]/BbNBdZBffBgmBiyBn�Bp�Bu�Bt�Bs�Bw�Bx�Bw�Bv�Bx�B{�B�B}�Bz�B� B�DB�1B�B�1B�bB�bB�bB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�9B�XB�^B�dB�qB�wBŢBƨBǮBȴB��B��BȴB��B��B�
B�B�)B�5B�/B�5B�TB�TB�fB�B�B�B�B�B��B��B��B��B	B	B	
=B	DB		7B	1B	hB	�B	#�B	"�B	 �B	 �B	'�B	,B	5?B	>wB	:^B	;dB	<jB	>wB	=qB	@�B	G�B	H�B	N�B	R�B	S�B	R�B	S�B	T�B	]/B	dZB	l�B	m�B	p�B	n�B	p�B	q�B	t�B	w�B	t�B	|�B	�B	�+B	�DB	�\B	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�'B	�FB	�jB	�dB	�^B	�jB	�wB	�}B	B	ÖB	ÖB	B	ŢB	ƨB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�#B	�#B	�#B	�)B	�#B	�B	�5B	�HB	�HB	�NB	�HB	�NB	�TB	�TB	�NB	�NB	�TB	�TB	�TB	�TB	�ZB	�`B	�mB	�ZB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
%B
%B
B
B
1B
+B
%B
%B
+B
+B
	7B

=B

=B

=B

=B
JB
VB
VB
PB
DB
JB

=B
\B
\B
bB
hB
hB
bB
bB
VB
bB
hB
oB
hB
hB
oB
{B
{B
{B
uB
{B
{B
oB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
#�B
#�B
#�B
#�B
#�B
#�B
"�B
"�B
"�B
"�B
#�B
$�B
#�B
$�B
$�B
$�B
%�B
%�B
&�B
'�B
'�B
&�B
&�B
&�B
'�B
'�B
&�B
'�B
'�B
&�B
&�B
(�B
(�B
)�B
)�B
)�B
(�B
-B
,B
,B
-B
-B
.B
/B
/B
/B
/B
/B
0!B
/B
.B
-B
/B
/B
.B
/B
1'B
1'B
0!B
33B
49B
49B
49B
49B
33B
33B
2-B
1'B
2-B
33B
33B
2-B
2-B
49B
49B
5?B
5?B
49B
6FB
5?B
5?B
7LB
7LB
8RB
7LB
8RB
7LB
7LB
7LB
9XB
8RB
8RB
8RB
;dB
;dB
;dB
;dB
<jB
;dB
;dB
;dB
:^B
<jB
<jB
<jB
;dB
<jB
<jB
<jB
<jB
;dB
;dB
=qB
>wB
>wB
>wB
>wB
>wB
=qB
=qB
>wB
>wB
>wB
>wB
>wB
>wB
>wB
?}B
?}B
@�B
A�B
@�B
A�B
B�B
B�B
B�B
A�B
B�B
D�B
D�B
D�B
C�B
B�B
D�B
D�B
D�B
D�B
C�B
C�B
B�B
C�B
E�B
F�B
F�B
F�B
D�B
E�B
E�B
E�B
F�B
H�B
H�B
H�B
H�B
G�B
G�B
G�B
I�B
I�B
K�B
L�B
L�B
K�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
O�B
O�B
P�B
P�B
O�B
O�B
O�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
R�B
Q�B
Q�B
Q�B
P�B
Q�B
R�B
T�B
T�B
T�B
T�B
T�B
S�B
S�B
T�B
T�B
VB
W
B
W
B
W
B
W
B
W
B
W
B
W
B
W
B
W
B
W
B
W
B
W
B
VB
XB
YB
YB
YB
YB
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
YB
XB
YB
YB
[#B
\)B
\)B
\)B
[#B
ZB
[#B
[#B
ZB
[#B
\)B
[#B
[#B
]/B
\)B
]/B
[#B
\)B
\)B
\)B
^5B
^5B
^5B
]/B
^5B
^5B
`BB
_;B
_;B
^5B
_;B
_;B
_;B
_;B
`BB
`BB
aHB
`BB
aHB
bNB
bNB
bNB
aHB
`BB
aHB
bNB
bNB
`BB
bNB
bNB
aHB
bNB
aHB
aHB
bNB
cTB
cTB
dZB
dZB
cTB
dZB
dZB
e`B
ffB
ffB
e`B
ffB
hsB
hsB
hsB
hsB
gmB
ffB
gmB
gmB
hsB
hsB
hsB
jB
jB
jB
jB
jB
k�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
k�B
k�B
l�B
l�B
l�B
l�B
k�B
jB
k�B
l�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
o�B
o�B
p�B
p�B
q�B
q�B
p�B
q�B
q�B
r�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
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
s�B
s�B
s�B
s�B
t�B
u�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
v�B
v�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�B�B�B�B�B�B �B!�B!�B"�B"�B!�B �B �B �B!�B �B �B �B!�B#�B&�B'�B*�B0!B0!B6+B:DB=<BDMBS�B\�BgRBkkBo�Bu�By>B��B��B��B��B�B�nB�]B�+B�	B�B�B�B!�B��BΥB��BߊB�9B��B��B �B�$B��B	RBB&�B#B"�B'�B%�B�BbB�BaB�]B�B��B�)B�B�vB�B��B��B�mB�OB��B�OB�dB��B}<BnB]dBY�B@4BX_Bd@BkQButB�B�:B�FB��B�iBe�BL�BB�B!�B-B
��B
�-B
�mB
M�B	��B	ðB	��B	��B	�=B	��B	��B	�fB	�+B	�B	�bB	�{B	��B	��B	sB	��B	�1B	�YB	��B	x�B	m)B	`�B	`�B	cTB	[�B	C�B	A�B	/iB	+B	�B	B�.B��B�B�'B��B�jBؓB�RB��BĶB��B�aB��B��B�IB��B��B�[B�B��B�B�_B�@B��B��B��B��B��B�3Bp!B[�Bi_Bl"Bo�Bi*Bh>B`BBb�BkQBc�Bf�BUMB[�BV�BS&BK�BW�BW
BQ BO\BVBU�BS&BU�BPHB=VBJ#BN�BT�BLJBZQB[qBZ�BW�BQBNBV�BW�BT�BO�BU�BNVBLBU�BW�BT�BVBQBSBXBRTBR BZ�BU�BRoB^BU�BR�BZBWsBX+Bc�B_�B[	BS�BQ BS�BT�BZ�BVBT�BVB_�B]�B`�Ba|B_�B\�B\�B^Bb�BeBg8Bh$Bj0Bo5BqBu�BuBt9BxBy>BxRBwLByrB|�B�[B~�B|B��B��B��B�AB��B��B��B�B��B�
B��B�B�B�WB�B�B�B�B�bB�bB��B�QB�iB��B��B��B��B��B��B��B��B�B��B��B��B�B��B�B�lB�~B�FB�YB�mB�]B�OB�~BޞB�B��B��B�B�B�B�B�MB�B�B�dB�}B	uB	�B	
XB	xB		�B	�B	�B	�B	#�B	#B	!-B	!bB	(>B	,=B	5?B	>wB	:�B	;�B	<�B	>�B	>(B	A B	G�B	IB	OB	S&B	T,B	SuB	TaB	UgB	]/B	d&B	lqB	mwB	p�B	o B	qB	r-B	u%B	xRB	utB	}qB	�GB	�_B	�^B	�vB	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	�B	�B	��B	��B	��B	��B	�"B	�)B	�IB	�]B	�cB	�UB	�[B	�zB	�jB	�B	��B	��B	��B	��B	ªB	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	�B	�0B	�B	�,B	�B	�FB	�gB	�SB	�eB	�=B	�=B	�=B	�CB	�WB	چB	�jB	�bB	�bB	�hB	�|B	�hB	�nB	�nB	�hB	�hB	�nB	�B	�B	�B	��B	��B	�B	��B	�B	�B	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	��B	��B	��B	�+B	�	B	�$B	�B	�B	�B	�(B	�"B	�6B	�B
;B
'B
'B
AB
'B
'B
'B
-B
3B
9B
?B
?B
?B
?B
?B
?B
SB
mB
fB
_B
tB
tB
_B
_B
	RB

=B

XB

rB

rB
dB
pB
VB
jB
xB
~B

�B
�B
�B
}B
�B
�B
}B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
#B
B
�B
 �B
 �B
 �B
 �B
B
!�B
#�B
#�B
#�B
#�B
#�B
#�B
"�B
#B
# B
#B
$B
$�B
$&B
%,B
%B
%,B
&B
&B
'B
($B
(
B
'B
'B
'B
(
B
(
B
'8B
($B
($B
'8B
'8B
)DB
)*B
*B
*KB
*0B
)_B
-B
,=B
,WB
-)B
-CB
.IB
/5B
/5B
/5B
/5B
/5B
0!B
/5B
.cB
-wB
/5B
/OB
.}B
/OB
1[B
1[B
0oB
33B
49B
4TB
49B
4TB
3hB
3hB
2aB
1[B
2aB
3MB
3hB
2aB
2|B
4nB
4nB
5tB
5tB
4nB
6`B
5tB
5tB
7�B
7fB
8lB
7�B
8lB
7fB
7�B
7�B
9XB
8lB
8�B
8�B
;�B
;B
;dB
;B
<�B
;�B
;�B
;B
:�B
<jB
<jB
<�B
;B
<�B
<�B
<�B
<�B
;�B
;�B
=�B
>�B
>wB
>�B
>�B
>�B
=�B
=�B
>�B
>�B
>�B
>�B
>�B
>�B
>�B
?�B
?�B
@�B
A�B
@�B
A�B
B�B
B�B
B�B
A�B
B�B
D�B
D�B
D�B
C�B
B�B
D�B
D�B
D�B
D�B
C�B
C�B
B�B
C�B
E�B
F�B
F�B
F�B
D�B
E�B
E�B
E�B
F�B
H�B
H�B
H�B
H�B
G�B
G�B
G�B
J	B
J	B
K�B
L�B
L�B
K�B
J�B
KB
KB
LB
LB
K�B
LB
L0B
PB
PB
P�B
QB
PB
O�B
O�B
QB
Q B
RB
RB
RB
RB
R�B
RB
R B
RB
Q4B
R:B
S@B
UB
UB
U2B
U2B
U2B
T,B
T,B
UB
UB
VB
W$B
W$B
W$B
W?B
W$B
W$B
W$B
W?B
W$B
W?B
W$B
W$B
VSB
X+B
Y1B
Y1B
Y1B
YKB
Y1B
Y1B
Z7B
Z7B
ZQB
Z7B
Z7B
ZB
Z7B
Z7B
Z7B
YKB
X_B
YeB
YeB
[=B
\CB
\CB
\CB
[WB
ZQB
[WB
[=B
ZQB
[WB
\)B
[=B
[=B
]/B
\CB
]dB
[WB
\]B
\CB
\xB
^OB
^OB
^jB
]IB
^OB
^OB
`BB
_pB
_VB
^jB
_VB
_VB
_pB
_VB
`\B
`\B
a|B
`vB
abB
bhB
bNB
bhB
abB
`�B
abB
bhB
bhB
`vB
b�B
bhB
a�B
bhB
a|B
a|B
b�B
cnB
c�B
dtB
d�B
c�B
dtB
dtB
ezB
ffB
f�B
e�B
f�B
hsB
h�B
h�B
h�B
g�B
f�B
g�B
g�B
h�B
h�B
h�B
j�B
jB
j�B
jB
j�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
k�B
k�B
l�B
l�B
l�B
l�B
k�B
j�B
k�B
l�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
o�B
o�B
p�B
p�B
q�B
q�B
p�B
q�B
q�B
r�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
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
s�B
s�B
s�B
s�B
t�B
u�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
v�B
v�11111111111111111111111111111111111111111111111113311111111111111111111111111111111111111111111111111111111111111111111131111111111111111111111111111111111111111111111111311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.06(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201801060033202018010600332020180106003320201806221324182018062213241820180622132418201804050727322018040507273220180405072732  JA  ARFMdecpA19c                                                                20180102093515  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180102003531  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180102003533  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180102003534  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180102003534  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180102003534  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180102003534  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180102003534  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180102003535  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180102003535                      G�O�G�O�G�O�                JA  ARUP                                                                        20180102005506                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180102153608  CV  JULD            G�O�G�O�F��                JM  ARSQJMQC2.0                                                                 20180105000000  CF  PSAL_ADJUSTED_QCB�  C�  G�O�                JM  ARCAJMQC2.0                                                                 20180105153320  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180105153320  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404222732  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622042418  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116221515                      G�O�G�O�G�O�                