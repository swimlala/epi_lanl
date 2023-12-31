CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-05-08T00:35:40Z creation;2018-05-08T00:35:45Z conversion to V3.1;2019-12-19T07:38:51Z update;     
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
_FillValue                 �  ID   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M0   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  pl   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tX   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �(   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �d   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ޜ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �    HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �$   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �(   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �,   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �l   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �|   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20180508003540  20200116221516  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0577_238                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @�`��O 1   @�`���-�@4�)^�	�dK��҈�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4�C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D
��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�3D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�z�@�{@�{A
=A?
=A_
=A
=A��A��A��A��AυA߅A�A��BBBBB'B/B7B?BGBOBWB_BgBoBwBB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB�{B��HB��HB��HB��HB��HB��HB��HC�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C4
=C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��RC��RC��RC��RC�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RD |)D �)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D	|)D	�)D
|)D
��D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D |)D �)D!|)D!�)D"|)D"�)D#|)D#�)D$|)D$�)D%|)D%�)D&|)D&�)D'|)D'�)D(|)D(�)D)|)D)�)D*|)D*�)D+|)D+�)D,|)D,�)D-|)D-�)D.|)D.�)D/|)D/�)D0|)D0�)D1|)D1�)D2|)D2�)D3|)D3�)D4|)D4�)D5|)D5�)D6|)D6�)D7|)D7�)D8|)D8�)D9|)D9�)D:|)D:�)D;|)D;�)D<|)D<�)D=|)D=�)D>|)D>�)D?|)D?�)D@|)D@�)DA|)DA�)DB|)DB�)DC|)DC�)DD|)DD�)DE|)DE�)DF|)DF�)DG|)DG�)DH|)DH�)DI|)DI�)DJ|)DJ�)DK|)DK�)DL|)DL�)DM|)DM�)DN|)DN�)DO|)DO�)DP|)DP�)DQ|)DQ�)DR|)DR�)DS|)DS�)DT|)DT�)DU|)DU�)DV|)DV�)DW|)DW�)DX|)DX�)DY|)DY�)DZ|)DZ�)D[|)D[�)D\|)D\�)D]|)D]�)D^|)D^�)D_|)D_�)D`|)D`�)Da|)Da�)Db|)Db�)Dc|)Dc�)Dd|)Dd�)De|)De�)Df|)Df�)Dg|)Dg�)Dh|)Dh�)Di|)Di�)Dj|)Dj�)Dk|)Dk�)Dl|)Dl�)Dm|)Dm�)Dn|)Dn�)Do|)Do�)Dp|)Dp�)Dq|)Dq�)Dr|)Dr�)Ds|)Ds�)Dt|)Dt�)Du|)Du�)Dv|)Dv�)Dw|)Dw�)Dx|)Dx�)Dy|)Dy�)Dz|)Dz�)D{|)D{�)D||)D|�)D}|)D}�)D~|)D~�)D|)D�)D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��HD��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D¾D��D�>D�~DþD��D�>D�~DľD��D�>D�~DžD��D�>D�~DƾD��D�>D�~DǾD��D�>D�~DȾD��D�>D�~DɾD��D�>D�~DʾD��D�>D�~D˾D��D�>D�~D̾D��D�>D�~D;D��D�>D�~DξD��D�>D�~DϾD��D�>D�~DоD��D�>D�~DѾD��D�>D�~DҾD��D�>D�~DӾD��D�>D�~DԾD��D�>D�~DվD��D�>D�~D־D��D�>D�~D׾D��D�>D�~DؾD��D�>D�~DپD��D�>D�~DھD��D�>D�~D۾D��D�>D�~DܾD��D�>D�~DݾD��D�>D�~D޾D��D�>D�~D߾D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�HD�D��D�>D�~D�D��D�>D�~D�D�HD�>D�~D�D��D�>D�~D��D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D��HD��{111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�&�A�&�A�(�A�&�A�"�A� �A�"�A�$�A�"�A�"�A�"�A�"�A�"�A�$�A�&�A�&�A�&�A�&�A�&�A�&�A��A��A�ȴA���A�1'A��mA���A��^A��A�A�A���A��/A��-A�S�A��RA�VA�$�A��A��DA�C�A�
=A��FA�^5A���A���A���A�A�A�JA���A�
=A��A�+A��A�z�A�(�A���A�^5A�A��PA��9A�-A�%A���A���A�p�A��9A�=qA�33A��;A���A��/A�n�A�XA��HA��A��A���A�A�A���A�O�A���A�"�A��A�^5A� �A��A�5?A��^A���A�`BA��uA�dZA��TA�ĜA���A�~�A��A�n�A�(�A���A��A�-A��-A�+A�+A�x�A��A�t�A�XA�9XA��+A�G�A�1A�7LA���A�A�A~�A| �Ay33AxM�AvbAr��Ap�\An{Ak�
Aj��AhbNAe��AdQ�AcAa�hA_\)A^�uA]33A[��A[`BAZ��AY��AW�TAV-AU
=AS��ASG�ARv�AQ�-APĜAOVAN(�AL5?AJ-AG&�AFjAC��A@A�A>�A<��A;��A:9XA9hsA7�A5A5S�A4�A4n�A3�A2��A2�jA21'A0��A0bNA.1'A,9XA*n�A(��A(JA'��A'oA%hsA#�;A#
=A!�A �`A�FA?}A��A��A=qA�A�TA\)A��A�A�A�Ax�AoA\)A��A�A7LA��AA�TA7LA��AK�A�A;dA
VA��A�#A�AC�AbA�A`BA ��A ĜA =q@��R@��@��@�-@��D@�"�@��!@�^5@���@�K�@�/@�ƨ@�C�@�ȴ@��^@�@��@�j@�!@�@�z�@� �@�o@�v�@�%@�X@�ƨ@�\)@�5?@�(�@�\)@֟�@թ�@�r�@��@�bN@θR@�^5@�X@˥�@ˍP@�=q@�^5@�v�@ɡ�@��@�|�@�C�@�b@���@��m@ǍP@�"�@�ff@�O�@�G�@�?}@�1'@��m@���@�+@���@�-@�$�@�{@�p�@�&�@� �@���@�@�7L@�%@�Ĝ@���@��@�ȴ@�&�@�hs@���@��/@���@��@�;d@��@��@���@�r�@�bN@�1@�"�@���@�@���@�x�@��@�p�@�O�@�&�@�Z@���@�+@�~�@�%@�I�@�K�@��@��@�{@��#@��7@���@��@��w@��\@�M�@�@��T@���@�`B@�?}@�/@�V@��@���@�z�@���@�@���@�-@���@���@�x�@�X@�G�@�7L@�%@���@�j@�9X@���@��@�C�@�33@��@��@���@�~�@�ff@�M�@�M�@�=q@��T@��^@��7@�x�@�O�@�&�@�p�@��T@��T@��`@��D@��j@�V@�V@�7L@�hs@�x�@��7@��@�O�@�r�@���@�|�@�;d@�^5@��@���@��@���@�O�@��^@�{@���@�G�@�9X@�9X@���@�K�@��!@�5?@�=q@�M�@��@�{@���@��h@��^@��-@�?}@�V@��`@��D@�j@�A�@��@��m@��m@���@��F@��@�K�@�K�@�\)@�dZ@�\)@�K�@�;d@�@���@���@��+@�=q@�{@��@���@�hs@�/@���@��#@��@�X@��D@�(�@� �@��@�ƨ@�t�@��@�5?@�@��@�G�@���@��@�z�@�r�@�Q�@�b@��m@��;@��
@�ƨ@��@�+@��!@�~�@�ff@�{@���@��#@�@��@���@��u@�A�@�9X@�b@���@��@��;@��;@��F@�33@���@���@���@�v�@�J@�J@��T@��^@��@�hs@�&�@���@���@�j@�9X@�1@��w@���@�\)@��@��\@�5?@���@���@��7@�`B@�V@��9@�r�@�A�@� �@�  @�@|�@;d@
=@~�y@~ȴ@~��@~E�@}�T@}@}�h@}`B@}?}@|��@|�@|1@|1@|1@|1@{��@{ƨ@{o@z�!@zM�@y�@xĜ@w|�@vff@v{@u��@t�j@s��@sC�@s33@s"�@s@r��@q�^@q�@pĜ@pA�@o�w@oK�@n�@m�h@mV@l��@l�@lj@k��@kS�@j~�@i�@i�#@i��@i��@i�@h�9@h �@g�@g�@gl�@gK�@g;d@g+@f�y@f��@fv�@f5?@e��@eO�@d�D@c��@c�m@cƨ@c��@ct�@c�F@c@b�!@b�\@b~�@b^5@bM�@a��@a�7@aX@a&�@`�@`A�@`�u@`�@_|�@^ȴ@^ff@]��@\��@\j@[��@[ƨ@[t�@[o@Z��@Z�!@Z�!@Yx�@X��@XĜ@X��@Xb@W��@W�P@W�P@W|�@Wl�@W;d@W
=@V�y@V�+@V5?@U��@UO�@T��@T��@Tz�@Tj@TZ@T(�@S��@S�
@S�F@SdZ@R�@R~�@RM�@R=q@RJ@Q��@Q��@Q�@P�u@P1'@O|�@O\)@O+@N��@Nȴ@Nv�@M�T@M�@MO�@M?}@M/@MV@MV@MV@L��@L�@Lz�@LZ@K��@K�
@K�
@K�
@Kt�@J�@J��@JM�@JJ@I��@I��@Ihs@I7L@I&�@I�@HĜ@HbN@G��@G+@F�R@F��@F�R@F��@F�+@F�+@Fff@E�@E�-@E��@E�@EO�@E�@EV@D�@D�D@DI�@D(�@C��@C�
@C��@B�H@B�\@B^5@B=q@A�#@AX@@�`@@Ĝ@@r�@@bN@@b@?�@?;d@?�@>�@>�R@>��@>�+@>v�@>E�@>{@=�-@=p�@<��@<�@<z�@<j@<9X@;��@;��@;C�@;o@:�@:M�@9��@9hs@9hs@97L@8��@8Ĝ@8�@8r�@81'@7\)@7
=@6�@6��@6E�@5@5�h@5`B@5?}@4��@4�@3�
@3t�@3@2�!@2�@2J@1��@1�#@1��@17L@0��@/��@/�@/��@/�@/K�@.��@.�y@.�y@.�R@.ff@-�T@-p�@-/@-V@-V@,�/@,j@+�
@+��@+�@+t�@+S�@+dZ@+S�@+"�@*��@*M�@)��@)�#@)��@)�^@)��@)�7@)x�@)&�@(��@(Ĝ@(bN@(A�@(1'@'�w@'��@'|�@'
=@&��@&5?@&{@%�@%�T@%��@%@%��@%�h@%�@%O�@%/@%V@$�@$Z@$(�@#�m@#�F@#�@#S�@#S�@#C�@#C�@"�@"�!@"�\@"~�@"^5@"-@"�@!��@!�@!�#@!�#@!�^@!�^@!��@!�7@!G�@ ��@ ��@ ��@ bN@  �@�@�P@\)@\)@K�@;d@;d@��@�R@V@�@@�-@�@`B@V@�/@��@z�@Z@�@�
@ƨ@�@S�@C�@33@33@33@"�@"�@�H@n�@�@��@�@��@�^@�^@��@x�@X@&�@�`@��@r�@ �@�@�;@�@\)@
=@��@ff@5?@�@@�h@�h@�@`B@�@�@��@j@I�@9X@�@�m@�
@ƨ@�F@��@��@t�@C�@"�@�H@n�@=q@J@��@�@��@�^@��@��@X@7L@��@bN@ �@b@  @�@�;@�P@l�@;d@
=@��@��@�@v�@ff@5?@�@@@�h@O�@?}@/@/@V@��@�D@9X@(�@�@��@t�@S�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�&�A�&�A�(�A�&�A�"�A� �A�"�A�$�A�"�A�"�A�"�A�"�A�"�A�$�A�&�A�&�A�&�A�&�A�&�A�&�A��A��A�ȴA���A�1'A��mA���A��^A��A�A�A���A��/A��-A�S�A��RA�VA�$�A��A��DA�C�A�
=A��FA�^5A���A���A���A�A�A�JA���A�
=A��A�+A��A�z�A�(�A���A�^5A�A��PA��9A�-A�%A���A���A�p�A��9A�=qA�33A��;A���A��/A�n�A�XA��HA��A��A���A�A�A���A�O�A���A�"�A��A�^5A� �A��A�5?A��^A���A�`BA��uA�dZA��TA�ĜA���A�~�A��A�n�A�(�A���A��A�-A��-A�+A�+A�x�A��A�t�A�XA�9XA��+A�G�A�1A�7LA���A�A�A~�A| �Ay33AxM�AvbAr��Ap�\An{Ak�
Aj��AhbNAe��AdQ�AcAa�hA_\)A^�uA]33A[��A[`BAZ��AY��AW�TAV-AU
=AS��ASG�ARv�AQ�-APĜAOVAN(�AL5?AJ-AG&�AFjAC��A@A�A>�A<��A;��A:9XA9hsA7�A5A5S�A4�A4n�A3�A2��A2�jA21'A0��A0bNA.1'A,9XA*n�A(��A(JA'��A'oA%hsA#�;A#
=A!�A �`A�FA?}A��A��A=qA�A�TA\)A��A�A�A�Ax�AoA\)A��A�A7LA��AA�TA7LA��AK�A�A;dA
VA��A�#A�AC�AbA�A`BA ��A ĜA =q@��R@��@��@�-@��D@�"�@��!@�^5@���@�K�@�/@�ƨ@�C�@�ȴ@��^@�@��@�j@�!@�@�z�@� �@�o@�v�@�%@�X@�ƨ@�\)@�5?@�(�@�\)@֟�@թ�@�r�@��@�bN@θR@�^5@�X@˥�@ˍP@�=q@�^5@�v�@ɡ�@��@�|�@�C�@�b@���@��m@ǍP@�"�@�ff@�O�@�G�@�?}@�1'@��m@���@�+@���@�-@�$�@�{@�p�@�&�@� �@���@�@�7L@�%@�Ĝ@���@��@�ȴ@�&�@�hs@���@��/@���@��@�;d@��@��@���@�r�@�bN@�1@�"�@���@�@���@�x�@��@�p�@�O�@�&�@�Z@���@�+@�~�@�%@�I�@�K�@��@��@�{@��#@��7@���@��@��w@��\@�M�@�@��T@���@�`B@�?}@�/@�V@��@���@�z�@���@�@���@�-@���@���@�x�@�X@�G�@�7L@�%@���@�j@�9X@���@��@�C�@�33@��@��@���@�~�@�ff@�M�@�M�@�=q@��T@��^@��7@�x�@�O�@�&�@�p�@��T@��T@��`@��D@��j@�V@�V@�7L@�hs@�x�@��7@��@�O�@�r�@���@�|�@�;d@�^5@��@���@��@���@�O�@��^@�{@���@�G�@�9X@�9X@���@�K�@��!@�5?@�=q@�M�@��@�{@���@��h@��^@��-@�?}@�V@��`@��D@�j@�A�@��@��m@��m@���@��F@��@�K�@�K�@�\)@�dZ@�\)@�K�@�;d@�@���@���@��+@�=q@�{@��@���@�hs@�/@���@��#@��@�X@��D@�(�@� �@��@�ƨ@�t�@��@�5?@�@��@�G�@���@��@�z�@�r�@�Q�@�b@��m@��;@��
@�ƨ@��@�+@��!@�~�@�ff@�{@���@��#@�@��@���@��u@�A�@�9X@�b@���@��@��;@��;@��F@�33@���@���@���@�v�@�J@�J@��T@��^@��@�hs@�&�@���@���@�j@�9X@�1@��w@���@�\)@��@��\@�5?@���@���@��7@�`B@�V@��9@�r�@�A�@� �@�  @�@|�@;d@
=@~�y@~ȴ@~��@~E�@}�T@}@}�h@}`B@}?}@|��@|�@|1@|1@|1@|1@{��@{ƨ@{o@z�!@zM�@y�@xĜ@w|�@vff@v{@u��@t�j@s��@sC�@s33@s"�@s@r��@q�^@q�@pĜ@pA�@o�w@oK�@n�@m�h@mV@l��@l�@lj@k��@kS�@j~�@i�@i�#@i��@i��@i�@h�9@h �@g�@g�@gl�@gK�@g;d@g+@f�y@f��@fv�@f5?@e��@eO�@d�D@c��@c�m@cƨ@c��@ct�@c�F@c@b�!@b�\@b~�@b^5@bM�@a��@a�7@aX@a&�@`�@`A�@`�u@`�@_|�@^ȴ@^ff@]��@\��@\j@[��@[ƨ@[t�@[o@Z��@Z�!@Z�!@Yx�@X��@XĜ@X��@Xb@W��@W�P@W�P@W|�@Wl�@W;d@W
=@V�y@V�+@V5?@U��@UO�@T��@T��@Tz�@Tj@TZ@T(�@S��@S�
@S�F@SdZ@R�@R~�@RM�@R=q@RJ@Q��@Q��@Q�@P�u@P1'@O|�@O\)@O+@N��@Nȴ@Nv�@M�T@M�@MO�@M?}@M/@MV@MV@MV@L��@L�@Lz�@LZ@K��@K�
@K�
@K�
@Kt�@J�@J��@JM�@JJ@I��@I��@Ihs@I7L@I&�@I�@HĜ@HbN@G��@G+@F�R@F��@F�R@F��@F�+@F�+@Fff@E�@E�-@E��@E�@EO�@E�@EV@D�@D�D@DI�@D(�@C��@C�
@C��@B�H@B�\@B^5@B=q@A�#@AX@@�`@@Ĝ@@r�@@bN@@b@?�@?;d@?�@>�@>�R@>��@>�+@>v�@>E�@>{@=�-@=p�@<��@<�@<z�@<j@<9X@;��@;��@;C�@;o@:�@:M�@9��@9hs@9hs@97L@8��@8Ĝ@8�@8r�@81'@7\)@7
=@6�@6��@6E�@5@5�h@5`B@5?}@4��@4�@3�
@3t�@3@2�!@2�@2J@1��@1�#@1��@17L@0��@/��@/�@/��@/�@/K�@.��@.�y@.�y@.�R@.ff@-�T@-p�@-/@-V@-V@,�/@,j@+�
@+��@+�@+t�@+S�@+dZ@+S�@+"�@*��@*M�@)��@)�#@)��@)�^@)��@)�7@)x�@)&�@(��@(Ĝ@(bN@(A�@(1'@'�w@'��@'|�@'
=@&��@&5?@&{@%�@%�T@%��@%@%��@%�h@%�@%O�@%/@%V@$�@$Z@$(�@#�m@#�F@#�@#S�@#S�@#C�@#C�@"�@"�!@"�\@"~�@"^5@"-@"�@!��@!�@!�#@!�#@!�^@!�^@!��@!�7@!G�@ ��@ ��@ ��@ bN@  �@�@�P@\)@\)@K�@;d@;d@��@�R@V@�@@�-@�@`B@V@�/@��@z�@Z@�@�
@ƨ@�@S�@C�@33@33@33@"�@"�@�H@n�@�@��@�@��@�^@�^@��@x�@X@&�@�`@��@r�@ �@�@�;@�@\)@
=@��@ff@5?@�@@�h@�h@�@`B@�@�@��@j@I�@9X@�@�m@�
@ƨ@�F@��@��@t�@C�@"�@�H@n�@=q@J@��@�@��@�^@��@��@X@7L@��@bN@ �@b@  @�@�;@�P@l�@;d@
=@��@��@�@v�@ff@5?@�@@@�h@O�@?}@/@/@V@��@�D@9X@(�@�@��@t�@S�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
q�B
q�B
q�B
q�B
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
r�B
r�B
s�B
q�B
q�B
u�B
�B�LB�HB�B(�B)�B'�B)�B-B2-B0!B.B2-BC�BO�BP�BT�BXB`BB^5B_;BdZBn�Br�Bt�B|�B~�B|�B�7B�uB�hB�PB�+B�%B�JB�1B�B~�B� B�B�B�B}�Bu�BbNBYBM�BK�BL�B@�B.B(�BB�B�HB�jB�}B�B��B�\BffB;dB0!B)�B.B&�B(�B!�B�BVB�B&�B,B+B�B�BhB
��BB
��B
�`B
��B
��B
��B
�{B
�=B
q�B
ffB
ffB
dZB
T�B
5?B
?}B
6FB
5?B
)�B
\B	��B	��B	�mB	��B	��B	�^B	�B	�B	��B	�B	�JB	�+B	v�B	cTB	l�B	aHB	]/B	^5B	VB	M�B	A�B	7LB	:^B	2-B	49B	.B	%�B	�B	VB	DB��B�B�
B�BŢB��B�qB�LB�FB�3B�FB��B��B�!B�'B�B��B��B��B��B��B��B�%B|�B{�B|�B�%B�%B�Br�Bm�Bv�Bq�Bp�Bo�Bx�By�Bx�Bv�Bv�Br�BiyB_;BW
BcTBbNBbNBbNBS�BYBdZBbNBZB^5B[#B[#BW
BXBYBQ�BT�BO�BN�BK�BP�BP�BP�BO�B\)B_;BZBXBS�BYB^5BZB^5BbNBaHB[#B[#BXB_;BgmBgmBaHBaHB[#B`BBcTBiyBk�Bm�Bn�Bn�BjB`BBn�By�Bt�Bx�B~�B�B~�B}�B}�Bw�B�B�PB�=B�1B��B��B��B��B��B��B��B��B�LB�XB�dB�^B�XB�RB�?B�wB�}B�jBĜBǮBŢBɺBɺB��B��B��B��B��B��B��B��B�B�B�B�B�)B�5B�B�B�B�B��B�B�B�B�B��B��B��B�B��B��B	B	1B	DB	DB	
=B	DB		7B	PB	hB	hB	\B	�B	�B	'�B	(�B	+B	33B	2-B	1'B	49B	6FB	7LB	D�B	F�B	K�B	L�B	N�B	Q�B	VB	ZB	ZB	[#B	]/B	\)B	_;B	dZB	ffB	iyB	m�B	o�B	p�B	q�B	q�B	q�B	r�B	v�B	x�B	{�B	|�B	~�B	�B	�B	�B	�B	�%B	�+B	�+B	�1B	�1B	�+B	�DB	�JB	�\B	�\B	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�!B	�B	�B	��B	�-B	�!B	�B	�B	�!B	�9B	�FB	�FB	�jB	�}B	�wB	�}B	�dB	B	B	B	B	B	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�B	�B	�B	�B	�B	�)B	�;B	�HB	�ZB	�ZB	�`B	�ZB	�`B	�fB	�mB	�fB	�mB	�mB	�mB	�fB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B	��B
B
B
%B
B
+B
	7B
	7B
1B
+B
DB
DB
PB
VB
\B
\B
\B
\B
VB
PB
PB
bB
bB
\B
VB
oB
hB
hB
oB
uB
oB
oB
uB
{B
{B
{B
{B
�B
{B
{B
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
 �B
!�B
!�B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
"�B
"�B
"�B
!�B
 �B
!�B
#�B
#�B
#�B
#�B
#�B
!�B
%�B
%�B
&�B
%�B
$�B
$�B
%�B
&�B
(�B
(�B
(�B
&�B
&�B
&�B
'�B
(�B
)�B
)�B
+B
+B
+B
)�B
)�B
)�B
(�B
(�B
'�B
(�B
)�B
)�B
)�B
)�B
,B
+B
,B
-B
-B
,B
,B
+B
+B
,B
+B
+B
,B
.B
-B
)�B
)�B
)�B
(�B
)�B
,B
,B
-B
,B
,B
,B
,B
,B
+B
.B
0!B
1'B
0!B
0!B
33B
33B
33B
33B
2-B
2-B
33B
2-B
2-B
33B
5?B
6FB
7LB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
8RB
8RB
9XB
:^B
;dB
:^B
:^B
:^B
9XB
:^B
:^B
9XB
<jB
<jB
<jB
<jB
<jB
;dB
<jB
=qB
>wB
>wB
>wB
?}B
?}B
>wB
>wB
=qB
?}B
>wB
?}B
@�B
?}B
>wB
>wB
?}B
?}B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
@�B
?}B
?}B
@�B
A�B
B�B
B�B
B�B
C�B
C�B
C�B
B�B
C�B
D�B
D�B
D�B
D�B
E�B
D�B
C�B
E�B
E�B
E�B
E�B
D�B
C�B
E�B
F�B
F�B
E�B
E�B
F�B
H�B
G�B
H�B
G�B
H�B
H�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
I�B
J�B
J�B
J�B
K�B
L�B
K�B
K�B
K�B
K�B
L�B
L�B
K�B
J�B
K�B
M�B
M�B
L�B
N�B
M�B
M�B
N�B
L�B
O�B
P�B
P�B
O�B
O�B
Q�B
Q�B
P�B
O�B
P�B
R�B
Q�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
Q�B
Q�B
O�B
R�B
T�B
T�B
T�B
T�B
T�B
T�B
S�B
S�B
R�B
R�B
S�B
T�B
T�B
S�B
S�B
R�B
T�B
W
B
W
B
XB
YB
ZB
ZB
ZB
[#B
[#B
\)B
]/B
]/B
]/B
]/B
]/B
\)B
\)B
]/B
\)B
^5B
^5B
]/B
^5B
^5B
]/B
]/B
^5B
_;B
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
aHB
aHB
bNB
bNB
bNB
cTB
cTB
bNB
bNB
bNB
cTB
cTB
cTB
cTB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
cTB
dZB
dZB
dZB
dZB
dZB
dZB
e`B
ffB
ffB
ffB
ffB
e`B
e`B
e`B
e`B
ffB
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
iyB
iyB
iyB
jB
jB
jB
jB
jB
iyB
iyB
hsB
jB
k�B
k�B
k�B
l�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
l�B
m�B
l�B
l�B
l�B
l�B
m�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
n�B
o�B
o�B
p�B
p�B
p�B
p�B
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
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
r�B
s�B
r�B
r�B
s�B
t�B
t�B
t�B
t�B
s�B
t�B
t�B
u�B
u�B
u�B
u�B
t�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
v�B
v�B
v�B
x�B
x�B
w�B
w�B
y�B
z�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
q�B
q�B
q�B
q�B
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
r�B
r�B
s�B
q�B
r�B
yrB
��B��B��B�B)*B*KB(sB*�B-�B2|B0�B/B3�BDMBPHBQ�BU�BX�B`�B_B`Be,Bo Bs3Bu�B}qB�B~wB�=B�B� B�<B��B�EB��B�B�MB��B�B��B��B��B~�Bw�BezB[�BP�BN�BN�BC�B0�B*�B	RB��B�B� B�;B��B��B��BkQBA B2�B,qB/�B(
B)_B"�B�B�B�B'RB,qB+�B!�B�B{B
�BtB
�	B
�mB
ѝB
�B
�@B
��B
��B
tTB
h�B
h
B
e�B
XEB
9>B
@�B
7�B
6B
+�B
�B	�0B	�wB	�B	��B	�gB	�<B	��B	��B	��B	�_B	��B	��B	yrB	fB	m�B	c B	^�B	_B	W$B	OBB	C�B	9rB	;�B	3�B	5B	/OB	'8B	 'B	�B	�B��B�cBڠB��B�lB�/B�cB�rB�B�B��B�sB��B��B��B��B��B��B�sB��B�KB��B�B�B~]B~�B��B��B�ABuBo�BxBsMBr-BqByrBz^ByrBwfBwLBsMBj�Ba|BYKBd�Bc�BcTBcnBVSBZ�Bd�Bc:B[�B_�B\�B\CBX�BY1BZ�BS�BVmBQ�BP�BM�BR�BR�BR�BQ�B\�B_�B[=BYeBU�BZB^�B[=B_!Bb�Ba�B\CB\xBY�B`'Bg�Bg�Bb4BbhB\�BabBdtBjKBl=BnBoiBoiBk�Bb�Bo�BzxBu�Bz*B�B��B�BB.By�B�B��B�)B�RB��B�QB��B� B�tB�vB��B�0B��B�rB��B��B��B��B��B��B��B�"B��B��B�%B�	B�#B�B�B�VB�\B̘BˬBϑB�gB�KB�_B�_B��B��B�!B�kB��B�CB��B�?B�B�}B�3B�9B�B�B�+B�nB�JB�wB	SB	fB	^B	^B	
rB	�B		�B	�B	�B	 B	}B	=B	 vB	($B	)DB	+�B	3�B	2|B	1�B	4�B	6�B	8B	D�B	F�B	K�B	MB	OB	R B	V9B	ZQB	ZkB	[WB	]~B	\�B	_�B	d�B	f�B	i�B	m�B	o�B	p�B	q�B	q�B	q�B	r�B	v�B	y$B	|B	}<B	.B	�'B	�-B	�GB	�gB	�?B	�EB	�EB	�fB	�KB	�zB	�^B	��B	�vB	��B	��B	�MB	�kB	��B	�+B	��B	��B	��B	�B	��B	�B	�/B	�!B	�;B	�cB	��B	��B	�GB	��B	��B	�]B	�;B	�9B	�zB	�`B	�PB	�cB	��B	��B	�B	ªB	�B	��B	�B	��B	ƨB	ɺB	��B	��B	��B	�B	��B	�B	�TB	�,B	�,B	�@B	�B	�MB	�$B	�$B	�1B	�1B	�1B	�KB	�7B	�CB	�;B	�HB	�ZB	�tB	�zB	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�OB	�B	�B	�B	�B	��B	��B	��B	�B	�B	�B	�GB	��B	��B	�?B	�B	�B	�B	��B	�B	�<B
'B
B
GB
-B
;B	�}B
UB
9B
?B
SB
EB
	RB
	RB
�B
�B
xB
�B
jB
�B
vB
vB
vB
vB
�B
�B
�B
}B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
B
 B
 �B
!�B
!�B
 �B
 �B
�B
!B
B
 B
B
/B
)B
�B
�B
�B
B
/B
 �B
"�B
#B
"�B
"B
!HB
!�B
#�B
$B
$B
$B
$&B
"4B
&B
&B
'B
&B
%,B
%,B
&LB
'B
)B
)B
)B
'B
'B
'8B
(
B
)*B
*B
*B
+6B
+B
+6B
*0B
*B
*0B
)*B
)*B
(>B
)DB
)�B
*0B
*B
*B
,B
+6B
,"B
-)B
-)B
,"B
,"B
+6B
+6B
,=B
+B
+6B
,"B
.B
-)B
*eB
*eB
*0B
)DB
*eB
,"B
,WB
-)B
,=B
,"B
,=B
,=B
,=B
+�B
.cB
0UB
1AB
0UB
0oB
33B
33B
33B
33B
2GB
2GB
3hB
2aB
2|B
3hB
5ZB
6`B
7fB
9XB
9XB
9�B
9rB
9rB
9rB
9rB
8�B
8�B
9�B
:xB
;�B
:xB
:xB
:xB
9�B
:�B
:�B
9�B
<�B
<�B
<�B
<�B
<�B
;�B
<�B
=�B
>�B
>wB
>�B
?}B
?}B
>�B
>�B
=�B
?�B
>�B
?�B
@�B
?�B
>�B
>�B
?�B
?�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
@�B
?�B
?�B
@�B
A�B
B�B
B�B
B�B
C�B
C�B
C�B
B�B
C�B
D�B
D�B
D�B
D�B
E�B
D�B
C�B
E�B
E�B
E�B
E�B
D�B
C�B
E�B
F�B
F�B
E�B
E�B
F�B
H�B
G�B
H�B
G�B
H�B
H�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
J	B
J�B
J�B
J�B
K�B
L�B
K�B
K�B
K�B
K�B
MB
MB
K�B
J�B
K�B
M�B
M�B
L�B
N�B
NB
M�B
OB
MB
PB
Q B
Q B
PB
P.B
RB
RB
Q B
PB
QB
SB
R B
S&B
SB
S@B
S�B
T,B
TB
TB
R B
R B
PHB
SB
U2B
U2B
UB
UB
T�B
T�B
TB
TB
S&B
S&B
T,B
UB
T�B
TB
T,B
S@B
U2B
W
B
W?B
X+B
Y1B
ZQB
Z7B
ZQB
[WB
[=B
\CB
]/B
]/B
]dB
]IB
]IB
\CB
\]B
]IB
\]B
^5B
^jB
]dB
^OB
^jB
]dB
]dB
^OB
_VB
`BB
`BB
`\B
`BB
`vB
`BB
`vB
`\B
`\B
`vB
`vB
`\B
abB
abB
bhB
bhB
bhB
cnB
cnB
bhB
bhB
b�B
cnB
cnB
cnB
cnB
dZB
d�B
dZB
dtB
dZB
dtB
dZB
dtB
d�B
dtB
cnB
dtB
d�B
d�B
d�B
dtB
dtB
ezB
ffB
ffB
f�B
ffB
ezB
ezB
ezB
ezB
f�B
gmB
g�B
g�B
g�B
g�B
g�B
h�B
h�B
h�B
h�B
iyB
i�B
i�B
jB
jB
j�B
jB
jB
i�B
i�B
h�B
j�B
k�B
k�B
k�B
l�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
l�B
m�B
l�B
l�B
l�B
l�B
m�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
n�B
o�B
o�B
p�B
p�B
p�B
p�B
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
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
r�B
s�B
r�B
r�B
s�B
t�B
t�B
t�B
t�B
s�B
t�B
t�B
u�B
u�B
u�B
u�B
t�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
v�B
v�B
v�B
x�B
x�B
w�B
xB
y�B
z�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.06(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201805120037122018051200371220180512003712201806221330132018062213301320180622133013201806042132362018060421323620180604213236  JA  ARFMdecpA19c                                                                20180508093518  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180508003540  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180508003543  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180508003544  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180508003545  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180508003545  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180508003545  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180508003545  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180508003545  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180508003545                      G�O�G�O�G�O�                JA  ARUP                                                                        20180508005705                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180508153731  CV  JULD            G�O�G�O�F��                JM  ARGQJMQC2.0                                                                 20180508153731  CV  JULD_LOCATION   G�O�G�O�F��                JM  ARGQJMQC2.0                                                                 20180508153731  CV  LATITUDE        G�O�G�O�A�M�                JM  ARCAJMQC2.0                                                                 20180511153712  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180511153712  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180604123236  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622043013  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116221516                      G�O�G�O�G�O�                