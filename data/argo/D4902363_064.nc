CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2016-12-05T00:35:18Z creation;2016-12-05T00:35:21Z conversion to V3.1;2019-12-19T08:23:54Z update;     
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
resolution        =���   axis      Z        `  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     `  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \4   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  `   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ol   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  sD   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  �|   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     `  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  �L   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  ˬ   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ۜ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20161205003518  20200115111516  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               @A   JA  I2_0576_064                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @��6 � 1   @��6�@y�@:���n/�d�@N���1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�  A   A   AA��A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fDfD� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#fD#�fD$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*�fD+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dqy�Dq��Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׃3D�� D�  D�C3D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��3D�  D�@ D� D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�L�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�G�@�{@�{A
=A@��A_
=A
=A��A��A��A��AυA߅A�A��BBBBB'B/B7B?BGBOBWB_BgBoBwBB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HC�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RD |)D �)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D	|)D	�)D
|)D
�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D��D�D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D |)D �)D!|)D!�)D"|)D#�D#��D#�)D$|)D$�)D%|)D%�)D&|)D&�)D'|)D'�)D(|)D(�)D)|)D)�)D*��D*�)D+|)D+�)D,|)D,�)D-|)D-�)D.|)D.�)D/|)D/�)D0|)D0�)D1|)D1�)D2|)D2�)D3|)D3�)D4|)D4�)D5|)D5�)D6|)D6�)D7|)D7�)D8|)D8�)D9|)D9�)D:|)D:�)D;|)D;�)D<|)D<�)D=|)D=�)D>|)D>�)D?|)D?�)D@|)D@�)DA|)DA�)DB|)DB�)DC|)DC�)DD|)DD�)DE|)DE�)DF|)DF�)DG|)DG�)DH|)DH�)DI|)DI�)DJ|)DJ�)DK|)DK�)DL|)DL�)DM|)DM�)DN|)DN�)DO|)DO�)DP|)DP�)DQ|)DQ�)DR|)DR�)DS|)DS�)DT|)DT�)DU|)DU�)DV|)DV�)DW|)DW�)DX|)DX�)DY|)DY�)DZ|)DZ�)D[|)D[�)D\|)D\�)D]|)D]�)D^|)D^�)D_|)D_�)D`|)D`�)Da|)Da�)Db|)Db�)Dc|)Dc�)Dd|)Dd�)De|)De�)Df|)Df�)Dg|)Dg�)Dh|)Dh�)Di|)Di�)Dj|)Dj�)Dk|)Dk�)Dl|)Dl�)Dm|)Dm�)Dn|)Dn�)Do|)Do�)Dp|)Dp�)Dqu�Dq��Dr|)Dr�)Ds|)Ds�)Dt|)Dt�)Du|)Du�)Dv|)Dv�)Dw|)Dw�)Dx|)Dx�)Dy|)Dy�)Dz|)Dz�)D{|)D{�)D||)D|�)D}|)D}�)D~|)D~�)D|)D�)D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D�HD�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D���D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D¾D��D�>D�~DþD��D�>D�~DľD��D�>D�~DžD��D�>D�~DƾD��D�>D�~DǾD��D�>D�~DȾD��D�>D�~DɾD��D�>D�~DʾD��D�>D�~D˾D��D�>D�~D̾D��D�>D�~D;D��D�>D�~DξD��D�>D�~DϾD��D�>D�~DоD��D�>D�~DѾD��D�>D�~DҾD��D�>D�~DӾD��D�>D�~DԾD��D�>D�~DվD��D�>D�~D־D��D�>DׁHD׾D��D�AHD�~DؾD��D�>D�~DپD��D�>D�~DھD��D�>D�~D۾D��D�>D�~DܾD��D�>D�~DݾD��D�>D�~D޾D��D�>D�~D߾D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D��HD��D�>D�~D�D�HD�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D��D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D��D��D�>D�~D��D��D�J�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��`A��mA��TA��yA��A��A��A��A��A��A��A��`A��A��A��A���A���A���A���A���A���A���A���A���A���A���A�  A�  A�A�A�A�A�A�A�A�A�A�%A�%A�A�A���A��A��`A��/A�A�`BA��mA�M�A�ffA�l�A�33A��jA��A�/A�bNA��`A���A��mA�x�A��#A��/A��7A���A�G�A�ZA�-A�ZA��!A�VA�\)A��A��A�
=A�?}A�v�A�/A�O�A���A��A�n�A��A�/A�`BA�+A���A���A���A��;A��RA��-A��A�bA�33A���A��yA�ffA�VA�ĜA���A��A�t�A�^5A��mA�hsA�VA���A��7A��A�S�A��hA���A��A�^5A��#A��7A�7A}��A|ffA{��Az��AwdZAv9XAu��At�9As�ArbAp=qAoS�An��Am�Akx�Akp�AkG�Ai��AihsAhbNAgp�Ae�wAd�DAc%Ab�!Ab-Aa�A_�
A^-A]�
A]?}AY�PAW�hAUS�ATM�AS/AQ��AP�+AO��AO/AN�DAN �AL�uAJbNAJ�AI�AIhsAG�;AES�AE%ADr�AC�AB�AB�\ABA�AB  AAl�AA&�A@ȴA@VA?�wA?"�A>ffA;�TA9��A9`BA8�A81'A7�mA7�A81A7\)A5VA4��A3S�A0��A0E�A/�A/�A.�A-t�A,�A,M�A,bA+��A+K�A*�A*�DA)�PA(�A(Q�A&�yA%O�A$�\A"��A!�hA!�Al�A�uAt�A�
A+AffA$�A�A~�A(�A��A��A��A�PAȴA�PA�!AO�A1'A�
A�A��A33A��Az�A��AM�A+A
�9A
(�A	�
A	p�A	33A�A��A��AdZAl�Ax�A|�A�7A�7A��A�7A��A-AXA�A ��@�|�@���@���@�E�@��7@�/@��@�+@��@�@�9@���@���@�@��H@�r�@��T@�@噚@��@�9X@�l�@�V@��T@ᙚ@�/@���@��@ߍP@�~�@�Ĝ@ٺ^@��y@�M�@�@�S�@�@���@�@�K�@ũ�@�33@�-@��u@�|�@�ȴ@�^5@�{@���@�p�@�Z@��P@��+@�@���@�9X@��F@�C�@��@��R@�1'@��@��9@��w@��@�M�@��@��@���@���@�j@� �@��;@���@��@�@���@��@�M�@��@�Z@�  @��@���@���@���@�9X@��@���@��F@�dZ@��@�v�@���@��-@��@���@��@���@�r�@�Q�@�1'@�b@��@���@�V@��#@�?}@�V@��@���@��D@�Z@� �@��w@��P@�l�@�C�@���@�5?@��@��@���@��D@�z�@�A�@��w@�;d@���@���@�V@�=q@��#@�?}@��@��@�Ĝ@�bN@��@��m@��m@��;@��w@�"�@���@��@�@�v�@���@�|�@�ƨ@��F@���@�33@���@�V@�@��9@�r�@�1@���@�K�@�o@��@��H@���@�=q@�J@���@��@�x�@�?}@�&�@�V@��/@��@���@���@��u@�r�@� �@���@��
@�|�@���@��@���@��R@���@�^5@���@��h@��h@�p�@�G�@���@���@��j@���@��u@�Z@�1'@|�@�@�@�w@l�@
=@~�y@|�@{��@{S�@{�m@|��@{�F@z��@z^5@z�\@z��@y��@x��@x�`@xĜ@xĜ@x��@xĜ@xĜ@x�@xbN@xb@w�@w�;@w�P@w|�@v�R@uO�@t�/@t�D@s�m@s��@s33@s@so@r��@r-@q��@q7L@pĜ@p��@p�u@pQ�@o��@o|�@ol�@o\)@o;d@o
=@n�@n�+@n@l��@l�@k��@ko@j��@i�@i&�@i&�@i%@h��@h�u@hbN@hQ�@h1'@h  @g�@g��@g\)@g\)@fȴ@fff@fV@fV@f5?@f$�@f{@f{@f{@e@e��@e@e�h@e`B@d��@dj@dj@dI�@d�@d1@c��@cƨ@c��@c��@ct�@cdZ@c"�@a�@a��@ahs@aG�@`�9@`Q�@`b@_�;@_�@_��@_|�@_l�@_;d@^�@^{@]�@]`B@]O�@]/@]V@\�@\�/@\�D@\z�@\j@\j@\Z@\(�@[�
@[ƨ@[��@[C�@Z��@ZM�@Y��@Yhs@X��@X�u@XQ�@X �@W�@W;d@V�y@V�@VE�@U�@UO�@V@U�h@T��@T1@S�m@R�@Rn�@Q��@Q�^@QX@PĜ@PA�@P1'@P  @O�w@O��@O\)@Nȴ@N5?@M�h@M/@L��@L�@L�/@L�@L9X@K��@K�F@K"�@Jn�@I&�@H�@HA�@H �@G�@G��@Gl�@F��@FV@F{@E��@E`B@EO�@E�@Dj@D�@C��@C�
@CdZ@C@B�!@B��@B�\@Bn�@B^5@B^5@BM�@B=q@B-@A��@A�#@A�#@A��@A��@Ahs@A%@@r�@?��@>�@>v�@>E�@>E�@>E�@>5?@>@=��@=�@=`B@=?}@<�@<�/@<�j@<I�@<�@<1@;��@;�m@;�
@;�
@;ƨ@;��@;dZ@;@:��@:M�@:-@9�7@9G�@9&�@9%@8��@8�@8Q�@8 �@7��@7�@7�P@7�@6��@6�y@6�@6ȴ@6�R@6��@6$�@5��@5@5�h@5p�@5O�@5�@4�/@4��@4��@4z�@4(�@3ƨ@3��@3�@3"�@3o@2��@2�\@2n�@2�@1�#@1��@1�^@1hs@1G�@17L@0��@0�@0r�@0bN@0Q�@0b@/��@/l�@/;d@/+@/+@.�@.V@.$�@-�h@-/@,��@,j@,I�@,�@+ƨ@+t�@+S�@+33@*�H@*�!@*~�@*M�@*=q@*=q@*-@*J@)�7@(��@(�@(r�@(Q�@(A�@(A�@(1'@(  @'�w@'�@&v�@&V@&E�@&5?@&{@%��@%�-@%�h@%`B@%V@$�j@$�D@$z�@$j@$Z@$I�@$9X@$(�@#��@#ƨ@#��@#t�@#dZ@#C�@"��@"n�@"�@"J@!�#@!�7@ ��@ A�@  �@ b@ b@ b@��@
=@��@@@��@�h@�h@p�@?}@�/@�@j@1@�m@�
@�
@ƨ@��@t�@dZ@S�@"�@@��@M�@-@�@��@x�@7L@&�@�@%@��@1'@  @�;@��@��@l�@+@
=@��@�R@��@�+@�+@�+@v�@v�@E�@@��@�h@?}@�@�D@I�@9X@1@�m@ƨ@�F@�F@�F@��@�@dZ@S�@33@�@��@~�@n�@-@��@��@x�@x�@G�@7L@&�@&�@�@��@r�@b@�@��@�w@�@l�@+@��@�y@�@ȴ@�R@��@5?@�@V@��@z�@(�@�
@�F@�@dZ@S�@S�@33@
�@
�H@
~�@
�@	��@	�7@	x�@	7L@	&�@	&�@	&�@	�@	%@	%@��@�`@��@Ĝ@��@bN@�@�@��@|�@;d@
=@�y@�@�R@�R@��@ff@5?@�T@111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��`A��mA��TA��yA��A��A��A��A��A��A��A��`A��A��A��A���A���A���A���A���A���A���A���A���A���A���A�  A�  A�A�A�A�A�A�A�A�A�A�%A�%A�A�A���A��A��`A��/A�A�`BA��mA�M�A�ffA�l�A�33A��jA��A�/A�bNA��`A���A��mA�x�A��#A��/A��7A���A�G�A�ZA�-A�ZA��!A�VA�\)A��A��A�
=A�?}A�v�A�/A�O�A���A��A�n�A��A�/A�`BA�+A���A���A���A��;A��RA��-A��A�bA�33A���A��yA�ffA�VA�ĜA���A��A�t�A�^5A��mA�hsA�VA���A��7A��A�S�A��hA���A��A�^5A��#A��7A�7A}��A|ffA{��Az��AwdZAv9XAu��At�9As�ArbAp=qAoS�An��Am�Akx�Akp�AkG�Ai��AihsAhbNAgp�Ae�wAd�DAc%Ab�!Ab-Aa�A_�
A^-A]�
A]?}AY�PAW�hAUS�ATM�AS/AQ��AP�+AO��AO/AN�DAN �AL�uAJbNAJ�AI�AIhsAG�;AES�AE%ADr�AC�AB�AB�\ABA�AB  AAl�AA&�A@ȴA@VA?�wA?"�A>ffA;�TA9��A9`BA8�A81'A7�mA7�A81A7\)A5VA4��A3S�A0��A0E�A/�A/�A.�A-t�A,�A,M�A,bA+��A+K�A*�A*�DA)�PA(�A(Q�A&�yA%O�A$�\A"��A!�hA!�Al�A�uAt�A�
A+AffA$�A�A~�A(�A��A��A��A�PAȴA�PA�!AO�A1'A�
A�A��A33A��Az�A��AM�A+A
�9A
(�A	�
A	p�A	33A�A��A��AdZAl�Ax�A|�A�7A�7A��A�7A��A-AXA�A ��@�|�@���@���@�E�@��7@�/@��@�+@��@�@�9@���@���@�@��H@�r�@��T@�@噚@��@�9X@�l�@�V@��T@ᙚ@�/@���@��@ߍP@�~�@�Ĝ@ٺ^@��y@�M�@�@�S�@�@���@�@�K�@ũ�@�33@�-@��u@�|�@�ȴ@�^5@�{@���@�p�@�Z@��P@��+@�@���@�9X@��F@�C�@��@��R@�1'@��@��9@��w@��@�M�@��@��@���@���@�j@� �@��;@���@��@�@���@��@�M�@��@�Z@�  @��@���@���@���@�9X@��@���@��F@�dZ@��@�v�@���@��-@��@���@��@���@�r�@�Q�@�1'@�b@��@���@�V@��#@�?}@�V@��@���@��D@�Z@� �@��w@��P@�l�@�C�@���@�5?@��@��@���@��D@�z�@�A�@��w@�;d@���@���@�V@�=q@��#@�?}@��@��@�Ĝ@�bN@��@��m@��m@��;@��w@�"�@���@��@�@�v�@���@�|�@�ƨ@��F@���@�33@���@�V@�@��9@�r�@�1@���@�K�@�o@��@��H@���@�=q@�J@���@��@�x�@�?}@�&�@�V@��/@��@���@���@��u@�r�@� �@���@��
@�|�@���@��@���@��R@���@�^5@���@��h@��h@�p�@�G�@���@���@��j@���@��u@�Z@�1'@|�@�@�@�w@l�@
=@~�y@|�@{��@{S�@{�m@|��@{�F@z��@z^5@z�\@z��@y��@x��@x�`@xĜ@xĜ@x��@xĜ@xĜ@x�@xbN@xb@w�@w�;@w�P@w|�@v�R@uO�@t�/@t�D@s�m@s��@s33@s@so@r��@r-@q��@q7L@pĜ@p��@p�u@pQ�@o��@o|�@ol�@o\)@o;d@o
=@n�@n�+@n@l��@l�@k��@ko@j��@i�@i&�@i&�@i%@h��@h�u@hbN@hQ�@h1'@h  @g�@g��@g\)@g\)@fȴ@fff@fV@fV@f5?@f$�@f{@f{@f{@e@e��@e@e�h@e`B@d��@dj@dj@dI�@d�@d1@c��@cƨ@c��@c��@ct�@cdZ@c"�@a�@a��@ahs@aG�@`�9@`Q�@`b@_�;@_�@_��@_|�@_l�@_;d@^�@^{@]�@]`B@]O�@]/@]V@\�@\�/@\�D@\z�@\j@\j@\Z@\(�@[�
@[ƨ@[��@[C�@Z��@ZM�@Y��@Yhs@X��@X�u@XQ�@X �@W�@W;d@V�y@V�@VE�@U�@UO�@V@U�h@T��@T1@S�m@R�@Rn�@Q��@Q�^@QX@PĜ@PA�@P1'@P  @O�w@O��@O\)@Nȴ@N5?@M�h@M/@L��@L�@L�/@L�@L9X@K��@K�F@K"�@Jn�@I&�@H�@HA�@H �@G�@G��@Gl�@F��@FV@F{@E��@E`B@EO�@E�@Dj@D�@C��@C�
@CdZ@C@B�!@B��@B�\@Bn�@B^5@B^5@BM�@B=q@B-@A��@A�#@A�#@A��@A��@Ahs@A%@@r�@?��@>�@>v�@>E�@>E�@>E�@>5?@>@=��@=�@=`B@=?}@<�@<�/@<�j@<I�@<�@<1@;��@;�m@;�
@;�
@;ƨ@;��@;dZ@;@:��@:M�@:-@9�7@9G�@9&�@9%@8��@8�@8Q�@8 �@7��@7�@7�P@7�@6��@6�y@6�@6ȴ@6�R@6��@6$�@5��@5@5�h@5p�@5O�@5�@4�/@4��@4��@4z�@4(�@3ƨ@3��@3�@3"�@3o@2��@2�\@2n�@2�@1�#@1��@1�^@1hs@1G�@17L@0��@0�@0r�@0bN@0Q�@0b@/��@/l�@/;d@/+@/+@.�@.V@.$�@-�h@-/@,��@,j@,I�@,�@+ƨ@+t�@+S�@+33@*�H@*�!@*~�@*M�@*=q@*=q@*-@*J@)�7@(��@(�@(r�@(Q�@(A�@(A�@(1'@(  @'�w@'�@&v�@&V@&E�@&5?@&{@%��@%�-@%�h@%`B@%V@$�j@$�D@$z�@$j@$Z@$I�@$9X@$(�@#��@#ƨ@#��@#t�@#dZ@#C�@"��@"n�@"�@"J@!�#@!�7@ ��@ A�@  �@ b@ b@ b@��@
=@��@@@��@�h@�h@p�@?}@�/@�@j@1@�m@�
@�
@ƨ@��@t�@dZ@S�@"�@@��@M�@-@�@��@x�@7L@&�@�@%@��@1'@  @�;@��@��@l�@+@
=@��@�R@��@�+@�+@�+@v�@v�@E�@@��@�h@?}@�@�D@I�@9X@1@�m@ƨ@�F@�F@�F@��@�@dZ@S�@33@�@��@~�@n�@-@��@��@x�@x�@G�@7L@&�@&�@�@��@r�@b@�@��@�w@�@l�@+@��@�y@�@ȴ@�R@��@5?@�@V@��@z�@(�@�
@�F@�@dZ@S�@S�@33@
�@
�H@
~�@
�@	��@	�7@	x�@	7L@	&�@	&�@	&�@	�@	%@	%@��@�`@��@Ĝ@��@bN@�@�@��@|�@;d@
=@�y@�@�R@�R@��@ff@5?@�T@111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��BB�B�B�B�B�B�B�B�ZB�B�B��B��B��BB�-B�!B�B��B��B��B�{B�=B�1B}�Bx�Br�Bl�BffB]/BP�BI�BB�B<jB6FB,B �B	7BBB�yB�B��B��B��BȴBÖB�FB�B��B��B��B�bB�%BjBcTBR�BK�BC�B=qB8RB'�B�B{B	7B
��B
�B
�)B
�XB
��B
�DB
|�B
p�B
l�B
dZB
H�B
:^B
8RB
2-B
(�B
�B
oB
bB
{B
1B	��B	��B	��B	��B	�B	�sB	�5B	��B	ÖB	�9B	�!B	�B	��B	��B	�oB	�bB	�hB	x�B	iyB	W
B	L�B	G�B	?}B	9XB	7LB	6FB	33B	1'B	-B	$�B	 �B	�B	�B	�B		7B		7B	DB		7B	%B	B	B	B��B��B��B��B��B�B�B�B�#B�
B��B��B��B��B��B��BɺBǮB��B�3B�?B�dB�dB�^B�?B�3B�'B�!B�B�B�B��B��B��B��B��B�oB�bB�=B�+B�%B|�By�Bx�Bs�Br�Bo�Bn�Bo�Bn�Bo�Bp�Bl�Bk�BjBiyBe`BaHB]/BYBW
BW
BW
BT�BR�BP�BM�BM�BF�BF�BI�BI�BH�BG�BF�BE�BH�BJ�BL�BN�BP�BQ�BQ�BVBW
BW
BT�BQ�BL�BM�BI�BH�BF�BA�B?}B?}B?}B;dB8RB6FB;dB@�BB�BD�BH�BC�B;dB5?B6FB7LBB�BC�BE�BE�BE�BD�BD�BC�BB�BA�B<jB6FB/B&�B�B!�B'�B"�B"�B%�B�B�B�B�B�B�B�B�B�B�B�B �B �B!�B#�B%�B)�B+B,B,B0!B.B/B5?B9XB:^B<jB=qBA�BB�BD�BF�BG�BJ�BL�BL�BM�BO�BT�BZB[#B\)B`BBcTBhsBm�Bp�Bq�Bq�Bs�Bt�Bv�Bx�Bz�B|�B�B�B�B�B�%B�+B�1B�1B�7B�=B�JB�\B�oB��B��B��B��B��B��B��B��B��B��B��B��B�B�B�-B�9B�?B�FB�XB�dB�jB�}B��BBĜBȴBɺB��B��B��B�B�
B�B�B�B�5B�5B�BB�NB�`B�sB�B��B��B��B��B��B��B��B��B��B��B��B��B	B	B	B	B	1B	DB	DB	DB	JB	VB	\B	bB	uB	�B	�B	�B	�B	�B	�B	�B	�B	!�B	%�B	&�B	&�B	'�B	+B	/B	1'B	49B	5?B	6FB	6FB	8RB	8RB	9XB	;dB	>wB	A�B	B�B	H�B	I�B	N�B	Q�B	Q�B	P�B	P�B	Q�B	S�B	S�B	W
B	[#B	\)B	^5B	aHB	dZB	e`B	hsB	jB	jB	jB	k�B	m�B	p�B	r�B	r�B	s�B	v�B	x�B	x�B	{�B	� B	�B	�B	�B	�B	�B	�B	�+B	�7B	�DB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�'B	�9B	�FB	�FB	�^B	�dB	�jB	�jB	�jB	�jB	�jB	�jB	�jB	�jB	�wB	�wB	�wB	�}B	�}B	��B	��B	��B	B	ŢB	ǮB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�#B	�)B	�)B	�)B	�/B	�/B	�5B	�HB	�TB	�TB	�TB	�ZB	�ZB	�ZB	�`B	�fB	�fB	�fB	�fB	�fB	�mB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
%B
%B
+B
1B

=B

=B

=B

=B
DB
DB
DB
JB
JB
PB
VB
VB
VB
\B
\B
bB
bB
hB
oB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
!�B
"�B
#�B
$�B
$�B
$�B
$�B
$�B
%�B
%�B
&�B
&�B
&�B
'�B
'�B
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
+B
+B
,B
,B
-B
-B
.B
.B
.B
/B
/B
/B
0!B
0!B
0!B
1'B
1'B
1'B
1'B
1'B
1'B
1'B
2-B
2-B
33B
33B
33B
33B
49B
49B
49B
49B
5?B
5?B
6FB
6FB
6FB
7LB
7LB
7LB
7LB
8RB
8RB
8RB
8RB
8RB
9XB
9XB
9XB
:^B
:^B
:^B
:^B
:^B
:^B
;dB
;dB
<jB
<jB
<jB
<jB
=qB
=qB
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
B�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
D�B
E�B
E�B
E�B
E�B
F�B
E�B
E�B
F�B
F�B
G�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
N�B
N�B
O�B
P�B
P�B
P�B
P�B
P�B
P�B
Q�B
R�B
S�B
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
VB
VB
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
XB
XB
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
[#B
[#B
[#B
[#B
[#B
[#B
\)B
[#B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
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
bNB
bNB
bNB
bNB
bNB
cTB
cTB
cTB
cTB
cTB
cTB
dZB
e`B
e`B
e`B
e`B
e`B
e`B
e`B
ffB
ffB
ffB
gmB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
jB
jB
jB
jB
jB
jB
jB
jB
jB
jB
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B�B��B
�B��B�MB�vB��B�'B�B��B�B��BچB֡B�TB�pB�B��B��B��B�0B�2B�kB�B��B�=B�Bz�Bt�Bn}BhsB_�BR�BKxBD3B>B8�B/OB#�B
rB+BB�B�qB�HB�B�xB��B�SB��B��B�B��B��B�&B��Bl=Be`BT,BMBD�B>�B:�B)yBVBmBDB
�wB
��B
ߊB
�PB
��B
��B
~wB
q�B
nIB
g�B
J	B
;JB
9�B
3�B
*�B
�B
�B
�B
B
	�B	�*B	�rB	�PB	��B	��B	��B	�'B	уB	�B	��B	�B	��B	��B	��B	�uB	�:B	�MB	{dB	k�B	X�B	NpB	I�B	@�B	:^B	88B	72B	49B	33B	/5B	%zB	!|B	 �B	�B	�B		�B	
#B	dB		�B	�B	�B	�B	�B��B��B��B��B��B�TB�B�B��B��B��B�.B�B�bB�TB׍B��BɺB�B�B��B��B�jB��B�B�B��B��B��B��B��B�QB�$B��B��B��B��B��B��B�fB�B~wB{�Bz�Bt�Bs�BpUBo�Bp�Bo�Bq[Bq�Bl�BlBk�BkBf�BcB^�BY�BWYBWYBW�BU�BTBRTBO�BO(BGzBG_BJ=BJXBI7BHKBG_BF�BIBJ�BL�BN�BQ BRoBS@BW�BX+BXEBV�BTBM�BO\BJ�BJ#BH1BB'B@ B@4B@�B=B:*B7B;B@�BCGBFBJXBESB<�B7B7LB7�BCaBDMBF%BE�BF%BEBEBD3BC�BCGB>�B8�B2B(�BB#B)_B$�B$�B'B \B�B�B�B7BBeB]B!B�B�B!�B!�B"�B$@B&fB*B+�B,�B-�B1�B.�B0B5�B9�B:�B<�B=�BBBB�BEBF�BH1BK�BM�BN<BNpBP�BU�BZ�B[�B\�B`�Bd&BiDBnBp�Bq�BrBtBu?Bw2By>B{JB}VB�;B�GB�3B�mB�tB�zB��B��B��B�B��B��B��B��B��B��B��B��B��B��B��B�-B�@B�mB�eB��B�}B�|B�nB��B��B��B��B��B��B��B��B�B�B��B��B�"B�@B�9B�$B�+B�KBچB�jB�OB�\B�B�B�>B�B��B��B�>B�PB�JB�xB��B�VB�qB�cB�.B�.B	 B	UB	[B	mB	fB	^B	xB	�B	~B	pB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	B	"B	%�B	'B	'B	($B	+QB	/iB	1vB	4TB	5�B	6zB	6zB	8lB	8�B	9rB	;�B	>�B	A�B	B�B	H�B	I�B	N�B	R B	R B	QNB	Q�B	R:B	TB	S�B	W$B	[qB	\�B	^jB	aHB	d�B	e�B	h�B	j�B	j�B	j�B	k�B	m�B	p�B	r�B	r�B	s�B	v�B	x�B	y	B	|6B	�OB	��B	�GB	�SB	�mB	�SB	�mB	�EB	�RB	�xB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�,B	�_B	�6B	�kB	�]B	�CB	�iB	�vB	�9B	�`B	�zB	�xB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�wB	��B	��B	��B	��B	��B	ªB	żB	ǮB	��B	��B	�B	��B	��B	� B	�B	�B	�B	�B	�B	�B	�B	�B	�&B	�[B	�B	�,B	�B	�2B	�EB	�KB	�7B	�WB	�CB	�CB	�]B	�IB	�dB	ބB	�|B	�nB	�nB	�nB	�tB	�tB	�B	�B	�B	�fB	�fB	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�	B	�B	�PB	��B	��B	�BB	�HB	�BB	�HB
uB
MB
YB
YB
_B
fB

�B

rB

XB

XB
^B
xB
xB
�B
~B
jB
pB
pB
pB
vB
�B
}B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
 B
"B
#:B
#�B
$�B
$�B
$�B
$�B
%B
%�B
%�B
'B
'B
'B
(
B
(
B
(>B
)B
(�B
)B
(�B
(�B
*B
*B
*0B
*0B
*KB
+B
+6B
,"B
,=B
-)B
-CB
.IB
./B
./B
/5B
/5B
/5B
0;B
0;B
0UB
1AB
1'B
1AB
1AB
1[B
1AB
1[B
2GB
2GB
3hB
3hB
3hB
3MB
4nB
4TB
4nB
4nB
5tB
5ZB
6`B
6`B
6zB
7�B
7fB
7fB
7fB
8lB
8�B
8lB
8lB
8lB
9�B
9rB
9�B
:�B
:xB
:xB
:�B
:xB
:�B
;�B
;�B
<jB
<�B
<�B
<�B
=�B
=�B
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
B�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
D�B
E�B
E�B
E�B
E�B
F�B
E�B
E�B
F�B
F�B
G�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
MB
L�B
L�B
MB
M�B
NB
M�B
NB
OB
O(B
PB
Q B
P�B
P�B
Q B
QB
QNB
R B
S@B
TB
T,B
S�B
S�B
TB
T,B
UB
UB
UB
UB
VB
VB
VB
VB
VB
W$B
W
B
W$B
W$B
W$B
W?B
W?B
X+B
X+B
XEB
Y1B
YKB
YB
Y1B
Y1B
YKB
Z7B
Z7B
Z7B
Z7B
[WB
[WB
[WB
[=B
[WB
[=B
\CB
[#B
\CB
\)B
\)B
\CB
\]B
\]B
\CB
\CB
]IB
]IB
]dB
^jB
^jB
^OB
^OB
_;B
_VB
_;B
_;B
_VB
_;B
_pB
_VB
_pB
_VB
_pB
`vB
`\B
`\B
`vB
a|B
abB
aHB
a|B
abB
aHB
bNB
b�B
b�B
bhB
bhB
cnB
c�B
cTB
cnB
cnB
c�B
dtB
e`B
e`B
e`B
ezB
e�B
e�B
e�B
f�B
f�B
f�B
g�B
g�B
g�B
g�B
h�B
hsB
h�B
h�B
h�B
h�B
h�B
i�B
i�B
j�B
j�B
j�B
j�B
jB
jB
jB
jB
j�B
j�B
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.06(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201612090036212016120900362120161209003621201806221217482018062212174820180622121748201804050410522018040504105220180405041052  JA  ARFMdecpA19c                                                                20161205093504  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20161205003518  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20161205003519  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20161205003519  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20161205003520  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20161205003520  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20161205003520  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20161205003520  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20161205003520  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20161205003521                      G�O�G�O�G�O�                JA  ARUP                                                                        20161205013425                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20161205153333  CV  JULD            G�O�G�O�F���                JM  ARCAJMQC2.0                                                                 20161208153621  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20161208153621  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404191052  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622031748  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115111516                      G�O�G�O�G�O�                