CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-03-12T00:35:35Z creation;2018-03-12T00:35:39Z conversion to V3.1;2019-12-19T07:43:23Z update;     
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
_FillValue                 �  IL   PRES_ADJUSTED            
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
resolution        :�o     �  tp   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �X   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �D   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  Τ   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �T   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �d   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �h   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �l   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �p   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �t   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20180312003535  20200116221515  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0577_219                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @�R�q� 1   @�R�DDD�@4(>BZ�c�d>��m\�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @333@�  @�33A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>�C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DFy�DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D��3D�� D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@/\)@|(�@�G�@�{A
=A?
=A_
=A
=A��A��A��A��AυA߅A�A��BBBBB'B/B7B?BGBOBWB_BgBoBwBB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��B��HB��HB��HB��HB��HB��HB��HB��HB�{B��HB��HB��HB��HB��HB��HB��HC�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C>
=C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RD |)D �)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D	|)D	�)D
|)D
�)D|)D�)D|)D�)D|)D�)D|)D��D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D |)D �)D!|)D!�)D"|)D"�)D#|)D#�)D$|)D$�)D%|)D%�)D&|)D&�)D'|)D'�)D(|)D(�)D)|)D)�)D*|)D*�)D+|)D+�)D,|)D,�)D-|)D-�)D.|)D.�)D/|)D/�)D0|)D0�)D1|)D1�)D2|)D2�)D3|)D3�)D4|)D4�)D5|)D5�)D6|)D6�)D7|)D7�)D8|)D8�)D9|)D9�)D:|)D:�)D;|)D;�)D<|)D<�)D=|)D=�)D>|)D>�)D?|)D?�)D@|)D@�)DA|)DA�)DB|)DB�)DC|)DC�)DD|)DD�)DE|)DE�)DFu�DF�)DG|)DG�)DH|)DH�)DI|)DI�)DJ|)DJ�)DK|)DK�)DL|)DL�)DM|)DM�)DN|)DN�)DO|)DO�)DP|)DP�)DQ|)DQ�)DR|)DR�)DS|)DS�)DT|)DT�)DU|)DU�)DV|)DV�)DW|)DW�)DX|)DX�)DY|)DY�)DZ|)DZ�)D[|)D[�)D\|)D\�)D]|)D]�)D^|)D^�)D_|)D_�)D`|)D`�)Da|)Da�)Db|)Db�)Dc|)Dc�)Dd|)Dd�)De|)De�)Df|)Df�)Dg|)Dg�)Dh|)Dh�)Di|)Di�)Dj|)Dj�)Dk|)Dk�)Dl|)Dl�)Dm|)Dm�)Dn|)Dn�)Do|)Do�)Dp|)Dp�)Dq|)Dq�)Dr|)Dr�)Ds|)Ds�)Dt|)Dt�)Du|)Du�)Dv|)Dv�)Dw|)Dw�)Dx|)Dx�)Dy|)Dy�)Dz|)Dz�)D{|)D{�)D||)D|�)D}|)D}�)D~|)D~�)D|)D�)D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�:�D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D¾D��D�>D�~DþD��D�>D�~DľD��D�>D�~DžD��D�>D�~DƾD��D�>D�~DǾD��D�>D�~DȾD��D�>D�~DɾD��D�>D�~DʾD��D�>D�~D˾D��D�>D�~D̾D��D�>D�~D;D��D�>D�~DξD��D�>D�~DϾD��D�>D�~DоD��D�>D�~DѾD��D�>D�~DҾD��D�>D�~DӾD��D�>D�~DԾD��D�>D�~DվD��D�>D�~D־D��D�>D�~D׾D��D�>D�~DؾD��D�>D�~DپD��D�>D�~DھD��D�>D�~D۾D��D�>D�~DܾD��D�>D�~DݾD��D�>D�~D޾D��D�>D�~D߾D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D��D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�AHD��HD��D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�x�Ać+AąAāAăAąAć+Ać+Aĉ7Aď\AčPAđhAđhAđhAď\Aď\Aď\AđhAđhAģ�A���A�A�A��HA��
A���A�Aħ�AđhAčPAċDAāA�M�AÙ�A�O�A��TA¼jA�AhA�/A��A���A���A�M�A���A� �A�z�A�=qA�+A� �A���A�G�A�|�A�=qA��A�?}A���A�VA�I�A��`A���A���A��;A�(�A�1'A�A�9XA��wA��A�ƨA��#A�bNA�"�A�~�A���A��A�G�A�|�A��TA� �A��FA���A�7LA���A���A���A�ffA��yA��A��FA�C�A�ȴA�E�A��-A�bNA�dZA���A�-A��!A��;A�%A��FA�E�A���A�bA��A��7A�^A|~�A{�Ax��Avn�Au\)At�At1Ar(�AnĜAlJAhĜAh1Ag�Af�9Af�uAfE�Ae�
AdA�A^ĜA\�AZ�AY�TAX�9AV�/AU�AT�AT=qAS&�ARA�AQ?}AP(�AO�AN{AJA�AIG�AG��ABȴAB1'AA��AAdZA@�DA=&�A;��A:�\A8r�A8  A7S�A7
=A6�DA5�A533A4�+A3��A2��A1�7A0-A-��A*�+A)dZA(��A(ĜA(E�A&�A$��A#�A#C�A ��A �A�FA/AȴA�^A9XA��A�#A��AE�A�#AC�A��A�-A��A��A��A^5A�A?}A��A1AA�TA	��A�jAr�A�^A�/AZA;dA&�A�AdZA"�A ��A �/A ȴA �A �\A A�@���@���@�x�@�z�@��w@�"�@��y@��+@�J@��@�Z@�`B@��@��@�V@��@��@�1@�S�@�n�@���@�@�F@��@�1@�@���@��@�|�@އ+@�|�@�V@���@�?}@ج@�9X@���@�@Ӿw@���@�`B@�Q�@�;d@��H@Η�@�M�@���@�`B@�?}@�V@���@̛�@�z�@�A�@�C�@�n�@�X@Ǖ�@�-@��@�J@�Ĝ@Å@��y@�@�b@��F@���@�=q@��@��@���@���@���@���@��u@���@���@�l�@�1'@��@�"�@���@��h@���@���@�V@�+@��T@��h@��@�b@�l�@�-@�p�@�%@���@��@�t�@�
=@��@���@�X@���@�I�@��@�1@���@�\)@�C�@�"�@��@���@�M�@�@�`B@��@��u@�bN@�  @���@�\)@�o@��@��R@�~�@�M�@��T@�p�@�?}@�G�@��`@�r�@��@�K�@�+@�ƨ@���@�ƨ@��w@��w@��@���@�l�@�+@�o@��@�~�@�$�@���@��@��#@���@��h@�Ĝ@��@��j@���@��/@�bN@��@���@�$�@�`B@�?}@��9@�b@�;d@��H@���@�n�@�E�@�=q@��@��#@�`B@��-@��^@���@���@���@���@�@�x�@�/@���@�Z@�A�@�1@��m@��@���@�K�@�@��y@��!@�v�@�E�@���@��#@��#@�@�hs@���@��/@�r�@��
@���@��@���@�@��!@���@���@��\@�E�@�{@��@�@��#@�@���@�p�@��@���@��D@�b@�ƨ@��w@��w@�ƨ@���@�1@��@���@�C�@��R@�v�@��@��T@���@�hs@�hs@�hs@�`B@�`B@�`B@�`B@�X@�`B@�`B@�hs@�p�@�p�@�x�@�p�@�`B@�p�@�x�@�hs@�&�@��@���@�j@�1@��
@���@��@�l�@�K�@�;d@��@�@�
=@��@�
=@��@��R@�n�@�-@��@���@��#@��T@��@��T@���@��h@�p�@�O�@�O�@�?}@��@�V@���@��`@��/@���@��9@��u@��@�z�@�Q�@�9X@�(�@�  @��m@��w@�S�@�@��\@�~�@�ff@�M�@���@��7@�X@�?}@�/@�&�@��@�%@��`@��j@���@��D@��@��D@�bN@�(�@�(�@� �@��@���@�ƨ@��w@�|�@�\)@�33@��R@���@���@��\@�E�@�{@�J@��#@��^@��@�?}@���@��@���@�z�@�Q�@�1'@��@�@��@~��@}O�@|�D@|�@{��@z�@z�\@z-@y�@yG�@xĜ@x�@xbN@xA�@x  @w�;@w|�@v��@v{@u�-@u?}@t��@t9X@sƨ@r��@q��@qG�@q&�@p��@pb@n��@n��@nv�@n5?@m@m`B@l�/@l��@l(�@k"�@j�\@i��@i�^@i��@iG�@h��@h�u@hQ�@h �@hb@h  @h  @g�@g��@g�@g\)@g;d@g
=@f�@fȴ@fv�@e�@d��@dZ@c�
@b��@b^5@a�^@a�@`Ĝ@_�@_�P@_
=@^�R@^5?@]O�@\��@\�j@\j@\�@\�@[�
@[dZ@Z�H@Z�\@Z-@Y��@Y�@Y�@Y��@X�`@X1'@W�@W��@WK�@W+@V�@V$�@U�-@U�h@U`B@U/@U�@T�/@T�D@TZ@T(�@S�m@SdZ@R�!@Rn�@Q�@Qhs@Q7L@P��@P�u@PbN@PA�@O�@O�@O\)@N��@NE�@M�@M�@MV@L�@K�m@KdZ@J�\@I��@IX@H��@H�u@H�@H�@Hr�@HQ�@H �@H �@Hb@G�@G�@F�y@F�R@Fff@E�T@E�h@E`B@EV@DI�@Cƨ@C�@C33@Co@B�@B�H@B��@B��@B-@A�^@A�@@��@@Q�@@Q�@@Q�@@A�@@1'@@  @?�@?\)@>��@>�+@>@=`B@=?}@=/@<�/@<�@<��@<�D@<z�@<I�@;��@;t�@;dZ@:�@:�\@:^5@:-@9��@9�@8r�@7�w@7|�@7K�@7+@7�@6�y@6��@6E�@5��@5p�@5`B@5O�@5�@4��@4�D@4�@3"�@2��@2�\@2^5@2=q@2-@1�@1hs@17L@1%@0�9@0�u@0r�@0bN@0Q�@0A�@01'@/�@/�;@/|�@/+@.�@.ȴ@.ȴ@.ȴ@.ȴ@.ȴ@.ȴ@.ȴ@.��@.�+@.ff@.5?@-�h@,��@,�D@,Z@+�m@+��@+S�@+o@*��@*�\@*~�@*=q@)�#@)x�@)&�@)%@(��@(�u@( �@( �@(b@'�;@'�w@'�@'��@'l�@'\)@'\)@';d@'�@&��@&�@&v�@&5?@&{@%�T@%@%�-@%�h@%p�@%O�@%V@$�/@$�/@$�@$�D@$I�@$�@#�
@#��@#�@#dZ@#S�@#S�@#@"��@"��@"�!@"��@"�\@"^5@"�@!��@!�7@!�@ ��@ Ĝ@ ��@  �@��@\)@;d@+@+@
=@��@ȴ@�R@v�@ff@$�@��@��@�@/@�@�D@(�@��@�H@M�@�@��@x�@X@G�@7L@&�@�@%@��@��@�u@r�@Q�@Q�@A�@A�@ �@  @�;@�w@l�@K�@+@�@�@�@
=@��@�y@�@ȴ@ȴ@v�@5?@$�@{@�@��@��@�@p�@`B@V@��@�D@Z@I�@1@�
@ƨ@t�@S�@C�@C�@��@n�@M�@M�@-@J@�@�@�#@�^@x�@hs@X@&�@�@��@��@�@Q�@A�@  @�;@��@��@�@\)@;d@��@ȴ@��@�+@V@$�@�@��@��@��@��@��@�T@�T11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�x�Ać+AąAāAăAąAć+Ać+Aĉ7Aď\AčPAđhAđhAđhAď\Aď\Aď\AđhAđhAģ�A���A�A�A��HA��
A���A�Aħ�AđhAčPAċDAāA�M�AÙ�A�O�A��TA¼jA�AhA�/A��A���A���A�M�A���A� �A�z�A�=qA�+A� �A���A�G�A�|�A�=qA��A�?}A���A�VA�I�A��`A���A���A��;A�(�A�1'A�A�9XA��wA��A�ƨA��#A�bNA�"�A�~�A���A��A�G�A�|�A��TA� �A��FA���A�7LA���A���A���A�ffA��yA��A��FA�C�A�ȴA�E�A��-A�bNA�dZA���A�-A��!A��;A�%A��FA�E�A���A�bA��A��7A�^A|~�A{�Ax��Avn�Au\)At�At1Ar(�AnĜAlJAhĜAh1Ag�Af�9Af�uAfE�Ae�
AdA�A^ĜA\�AZ�AY�TAX�9AV�/AU�AT�AT=qAS&�ARA�AQ?}AP(�AO�AN{AJA�AIG�AG��ABȴAB1'AA��AAdZA@�DA=&�A;��A:�\A8r�A8  A7S�A7
=A6�DA5�A533A4�+A3��A2��A1�7A0-A-��A*�+A)dZA(��A(ĜA(E�A&�A$��A#�A#C�A ��A �A�FA/AȴA�^A9XA��A�#A��AE�A�#AC�A��A�-A��A��A��A^5A�A?}A��A1AA�TA	��A�jAr�A�^A�/AZA;dA&�A�AdZA"�A ��A �/A ȴA �A �\A A�@���@���@�x�@�z�@��w@�"�@��y@��+@�J@��@�Z@�`B@��@��@�V@��@��@�1@�S�@�n�@���@�@�F@��@�1@�@���@��@�|�@އ+@�|�@�V@���@�?}@ج@�9X@���@�@Ӿw@���@�`B@�Q�@�;d@��H@Η�@�M�@���@�`B@�?}@�V@���@̛�@�z�@�A�@�C�@�n�@�X@Ǖ�@�-@��@�J@�Ĝ@Å@��y@�@�b@��F@���@�=q@��@��@���@���@���@���@��u@���@���@�l�@�1'@��@�"�@���@��h@���@���@�V@�+@��T@��h@��@�b@�l�@�-@�p�@�%@���@��@�t�@�
=@��@���@�X@���@�I�@��@�1@���@�\)@�C�@�"�@��@���@�M�@�@�`B@��@��u@�bN@�  @���@�\)@�o@��@��R@�~�@�M�@��T@�p�@�?}@�G�@��`@�r�@��@�K�@�+@�ƨ@���@�ƨ@��w@��w@��@���@�l�@�+@�o@��@�~�@�$�@���@��@��#@���@��h@�Ĝ@��@��j@���@��/@�bN@��@���@�$�@�`B@�?}@��9@�b@�;d@��H@���@�n�@�E�@�=q@��@��#@�`B@��-@��^@���@���@���@���@�@�x�@�/@���@�Z@�A�@�1@��m@��@���@�K�@�@��y@��!@�v�@�E�@���@��#@��#@�@�hs@���@��/@�r�@��
@���@��@���@�@��!@���@���@��\@�E�@�{@��@�@��#@�@���@�p�@��@���@��D@�b@�ƨ@��w@��w@�ƨ@���@�1@��@���@�C�@��R@�v�@��@��T@���@�hs@�hs@�hs@�`B@�`B@�`B@�`B@�X@�`B@�`B@�hs@�p�@�p�@�x�@�p�@�`B@�p�@�x�@�hs@�&�@��@���@�j@�1@��
@���@��@�l�@�K�@�;d@��@�@�
=@��@�
=@��@��R@�n�@�-@��@���@��#@��T@��@��T@���@��h@�p�@�O�@�O�@�?}@��@�V@���@��`@��/@���@��9@��u@��@�z�@�Q�@�9X@�(�@�  @��m@��w@�S�@�@��\@�~�@�ff@�M�@���@��7@�X@�?}@�/@�&�@��@�%@��`@��j@���@��D@��@��D@�bN@�(�@�(�@� �@��@���@�ƨ@��w@�|�@�\)@�33@��R@���@���@��\@�E�@�{@�J@��#@��^@��@�?}@���@��@���@�z�@�Q�@�1'@��@�@��@~��@}O�@|�D@|�@{��@z�@z�\@z-@y�@yG�@xĜ@x�@xbN@xA�@x  @w�;@w|�@v��@v{@u�-@u?}@t��@t9X@sƨ@r��@q��@qG�@q&�@p��@pb@n��@n��@nv�@n5?@m@m`B@l�/@l��@l(�@k"�@j�\@i��@i�^@i��@iG�@h��@h�u@hQ�@h �@hb@h  @h  @g�@g��@g�@g\)@g;d@g
=@f�@fȴ@fv�@e�@d��@dZ@c�
@b��@b^5@a�^@a�@`Ĝ@_�@_�P@_
=@^�R@^5?@]O�@\��@\�j@\j@\�@\�@[�
@[dZ@Z�H@Z�\@Z-@Y��@Y�@Y�@Y��@X�`@X1'@W�@W��@WK�@W+@V�@V$�@U�-@U�h@U`B@U/@U�@T�/@T�D@TZ@T(�@S�m@SdZ@R�!@Rn�@Q�@Qhs@Q7L@P��@P�u@PbN@PA�@O�@O�@O\)@N��@NE�@M�@M�@MV@L�@K�m@KdZ@J�\@I��@IX@H��@H�u@H�@H�@Hr�@HQ�@H �@H �@Hb@G�@G�@F�y@F�R@Fff@E�T@E�h@E`B@EV@DI�@Cƨ@C�@C33@Co@B�@B�H@B��@B��@B-@A�^@A�@@��@@Q�@@Q�@@Q�@@A�@@1'@@  @?�@?\)@>��@>�+@>@=`B@=?}@=/@<�/@<�@<��@<�D@<z�@<I�@;��@;t�@;dZ@:�@:�\@:^5@:-@9��@9�@8r�@7�w@7|�@7K�@7+@7�@6�y@6��@6E�@5��@5p�@5`B@5O�@5�@4��@4�D@4�@3"�@2��@2�\@2^5@2=q@2-@1�@1hs@17L@1%@0�9@0�u@0r�@0bN@0Q�@0A�@01'@/�@/�;@/|�@/+@.�@.ȴ@.ȴ@.ȴ@.ȴ@.ȴ@.ȴ@.ȴ@.��@.�+@.ff@.5?@-�h@,��@,�D@,Z@+�m@+��@+S�@+o@*��@*�\@*~�@*=q@)�#@)x�@)&�@)%@(��@(�u@( �@( �@(b@'�;@'�w@'�@'��@'l�@'\)@'\)@';d@'�@&��@&�@&v�@&5?@&{@%�T@%@%�-@%�h@%p�@%O�@%V@$�/@$�/@$�@$�D@$I�@$�@#�
@#��@#�@#dZ@#S�@#S�@#@"��@"��@"�!@"��@"�\@"^5@"�@!��@!�7@!�@ ��@ Ĝ@ ��@  �@��@\)@;d@+@+@
=@��@ȴ@�R@v�@ff@$�@��@��@�@/@�@�D@(�@��@�H@M�@�@��@x�@X@G�@7L@&�@�@%@��@��@�u@r�@Q�@Q�@A�@A�@ �@  @�;@�w@l�@K�@+@�@�@�@
=@��@�y@�@ȴ@ȴ@v�@5?@$�@{@�@��@��@�@p�@`B@V@��@�D@Z@I�@1@�
@ƨ@t�@S�@C�@C�@��@n�@M�@M�@-@J@�@�@�#@�^@x�@hs@X@&�@�@��@��@�@Q�@A�@  @�;@��@��@�@\)@;d@��@ȴ@��@�+@V@$�@�@��@��@��@��@��@�T@�T11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
�JB
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�DB
�DB
�PB
�uB
�3B
��Bk�B��B�-BÖBÖBĜB��B��B��B��B��B�;BhB!�B>wB@�BA�B:^B7LB5?B5?B5?B33B1'BP�Bm�Br�Bp�BhsBdZB^5BS�BiyB_;BP�BQ�B[#Be`BcTBN�BQ�BYBO�B2-B<jB=qB/B�B  B�B!�B�BPB�B�fB�B�B�HBŢB�-B�B��B{�B�DB~�BP�BI�B8RB"�BoB%BBB
�mB
�qB
ĜB
��B
�B
�DB
y�B
�B
aHB
XB
Q�B
%�B
DB
VB
�B
1B	��B	��B	��B	�B	�)B	�-B	�^B	��B	�9B	�B	�B	�B	��B	��B	~�B	H�B	Q�B	ZB	W
B	M�B	A�B	B�B	>wB	<jB	49B	.B	'�B	�B	{B	1B�ZB�B�ZB�^B�TB�TB�#B��B�B�RB�^B�B�jB�XB�XB�?B�B��B��B��B��B�7B� Bo�BdZB|�B�1B�%B|�Bn�BgmBn�Bq�BcTBr�By�Bu�Bu�Bk�Be`BhsBl�BiyBr�Br�Bl�Bk�B]/BP�BZB]/BVB_;B_;B`BB^5BZB]/BK�BXB^5BW
BQ�BQ�BI�B=qBN�B]/BaHBcTBcTBcTBbNB`BB]/BYBXBW
BZB\)B^5B`BB]/B[#BW
BQ�BF�BQ�B\)B^5B_;BZBYB[#BZBZBXBT�BP�BL�BJ�BE�BR�BT�B[#BQ�BcTBjBiyBiyBjBiyBcTBZBdZBr�Br�Bt�B|�B}�B}�B}�B�B�B�B�B�B�B�B|�B}�B~�B}�B�7B��B��B��B��B��B��B�uB��B��B��B�!B�?B�?B�?B�3B�B�RB�^B�XB��B�
B�
B�/B�/B�HB�5B�`B�B�5B�#B�NB�HB�;B�sB�B��B��B	B��B	B		7B		7B	bB	{B	�B	�B	!�B	"�B	#�B	$�B	)�B	)�B	)�B	,B	/B	1'B	49B	7LB	:^B	?}B	@�B	D�B	E�B	L�B	O�B	P�B	Q�B	R�B	Q�B	T�B	[#B	]/B	[#B	[#B	^5B	]/B	e`B	p�B	t�B	t�B	x�B	� B	�B	�%B	�%B	�1B	�=B	�7B	�=B	�VB	�uB	��B	��B	��B	��B	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�?B	�LB	�XB	�XB	�dB	�jB	�jB	�dB	�jB	�dB	�dB	�wB	�wB	��B	��B	ÖB	B	ĜB	ǮB	ȴB	ɺB	��B	ɺB	��B	��B	��B	��B	��B	��B	��B	�B	�#B	�#B	�#B	�B	�#B	�BB	�BB	�BB	�;B	�BB	�NB	�TB	�ZB	�TB	�TB	�NB	�NB	�HB	�BB	�HB	�ZB	�sB	�yB	�B	�B	�B	�B	�B	�B	�sB	�B	�B	�B	�B	�B	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
%B
%B
B
B
B
B
%B
+B
+B
+B

=B

=B
	7B
	7B
	7B
DB
DB
PB
JB
JB
JB
JB
PB
PB
PB
PB
JB
PB
PB
JB
PB
PB
JB
JB
DB
	7B
DB
DB
\B
VB
PB
DB
JB
VB
bB
hB
hB
hB
hB
hB
hB
hB
hB
oB
oB
hB
hB
{B
�B
�B
�B
�B
�B
�B
�B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
"�B
#�B
"�B
 �B
!�B
&�B
&�B
%�B
$�B
$�B
'�B
(�B
(�B
'�B
'�B
'�B
(�B
'�B
&�B
(�B
)�B
,B
,B
+B
+B
-B
-B
-B
.B
.B
.B
.B
.B
-B
-B
.B
.B
-B
-B
,B
+B
'�B
-B
,B
)�B
.B
-B
-B
/B
.B
0!B
0!B
1'B
0!B
/B
33B
33B
33B
49B
5?B
49B
33B
33B
49B
5?B
6FB
6FB
6FB
49B
33B
49B
6FB
6FB
6FB
7LB
6FB
5?B
7LB
9XB
9XB
9XB
9XB
9XB
8RB
9XB
9XB
9XB
8RB
8RB
:^B
9XB
:^B
<jB
<jB
;dB
<jB
=qB
<jB
<jB
<jB
;dB
=qB
>wB
=qB
=qB
>wB
<jB
>wB
=qB
>wB
A�B
A�B
B�B
D�B
D�B
D�B
C�B
C�B
D�B
C�B
B�B
B�B
C�B
D�B
C�B
C�B
D�B
D�B
D�B
C�B
D�B
F�B
F�B
G�B
H�B
H�B
H�B
G�B
F�B
F�B
F�B
H�B
I�B
K�B
K�B
K�B
J�B
J�B
I�B
I�B
I�B
J�B
J�B
J�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
M�B
M�B
N�B
N�B
N�B
O�B
O�B
N�B
M�B
N�B
N�B
Q�B
R�B
R�B
R�B
R�B
Q�B
Q�B
Q�B
S�B
T�B
S�B
S�B
R�B
S�B
R�B
P�B
S�B
VB
VB
W
B
VB
VB
T�B
W
B
W
B
W
B
XB
XB
YB
YB
YB
XB
XB
XB
XB
XB
YB
ZB
[#B
[#B
[#B
[#B
[#B
ZB
ZB
ZB
ZB
YB
XB
XB
ZB
[#B
[#B
[#B
\)B
\)B
\)B
^5B
]/B
\)B
\)B
\)B
^5B
^5B
^5B
^5B
^5B
`BB
`BB
`BB
`BB
aHB
aHB
`BB
aHB
aHB
aHB
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
bNB
bNB
bNB
bNB
cTB
bNB
bNB
bNB
cTB
cTB
cTB
dZB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
e`B
dZB
dZB
e`B
cTB
dZB
e`B
ffB
e`B
dZB
ffB
ffB
gmB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
gmB
hsB
hsB
hsB
hsB
gmB
hsB
gmB
gmB
iyB
jB
k�B
l�B
m�B
m�B
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
n�B
n�B
n�B
m�B
n�B
n�B
m�B
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
n�B
o�B
p�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
o�B
o�B
p�B
p�B
q�B
p�B
q�B
q�B
q�B
r�B
r�B
r�B
q�B
q�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
s�B
s�B
t�B
t�B
s�B
t�B
t�B
t�B
t�B
t�B
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
x�B
x�B
x�B
x�B
x�B
x�B
x�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
�B
�XB
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�DB
�DB
�PB
�uB
�hB
�BkQB��B�aB�{B��B��B��B�B�B�:BҽB��B�B"�B>�B@�BA�B;dB8�B6�B6B6+B49B2�BRBm�Br�BqBi�BezB`'BV�Bj�Ba-BTFBT�B]/BffBd�BR�BTB[	BR�B6�B>(B>�B1�BBB�B"�BKB�B��B��B�nB�"B�TB�B��B�CB��B�B�dB��BVBL0B;�B&LB�B	RB�BuB
�B
��B
��B
�B
��B
�\B
|�B
�aB
dtB
ZB
S�B
*�B
�B
�B
B

�B	��B	�]B	�B	��B	��B	��B	��B	�pB	��B	�5B	��B	�]B	��B	��B	��B	NpB	T�B	[�B	XyB	O�B	C�B	C�B	?�B	=qB	5�B	/OB	)_B	 BB	B	
XB��B�IB�B��B��B��B�)BϫB�B�*B�B�wB�<B�*B��B��B� B�B��B��B�
B�DB�ABr�BhXB~(B��B��B~Bp�Bi�BpBsBffBs�BzxBv�Bv�Bm)Bg�BjKBm�BkBshBshBmwBlWB_;BS[B[�B^�BW�B_�B`BBaB_pB[�B_;BN�BY1B^�BX+BS&BR�BK�B@4BPHB]�Ba�Bc�Bc�Bc�Bb�B`�B]�BY�BX�BW�BZ�B\�B^�B`vB]�B[�BW�BSBH�BS&B\�B^�B_�BZ�BY�B[�BZ�BZ�BX�BU�BR:BNVBL~BG�BTFBVSB\)BS�BdBj�Bi�BjBkBjBdtB\xBezBsMBs�ButB}"B~BB~BB~]B�UB�GB�MB�aB�gB�gB�uB}�B~�B� BHB�	B��B��B��B�yB�7B�eB��B�>B��B�B�UB�ZB�tB�ZB�hB��B��B��B��B�pB֡B�YB�BݲB��B��B�zB�BߊB�B�B��B�'B��B�kB�`B�BB	oB��B	�B		�B		�B	�B	�B	+B	B	!�B	"�B	$B	%,B	*0B	*KB	*KB	,WB	/iB	1�B	4�B	7�B	:�B	?�B	@�B	D�B	E�B	MB	O�B	QB	R B	S&B	RTB	UMB	[WB	]IB	[qB	[�B	^�B	]�B	ezB	poB	t�B	t�B	x�B	� B	�GB	�YB	�YB	�fB	�XB	�lB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�yB	�B	�HB	�bB	�B	�NB	�BB	�HB	�2B	�>B	�$B	�
B	�B	�DB	�*B	�=B	�B	�LB	�XB	�rB	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ðB	��B	��B	��B	��B	��B	��B	�	B	��B	�B	�B	�{B	�:B	�MB	�MB	�mB	�=B	�=B	�WB	چB	�WB	�BB	�\B	�\B	ߊB	�\B	�hB	�TB	�tB	�B	�B	�B	�B	�|B	��B	�B	�B	�B	�yB	�B	�qB	�B	�B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	�0B	�B	�B	�B	�*B	�B	�B
 B
;B
;B
'B
GB
3B
%B
?B
SB
9B
SB
mB
YB
EB
_B
_B

=B

=B
	lB
	RB
	lB
^B
^B
PB
dB
dB
dB
~B
jB
jB
�B
jB
~B
jB
jB
dB
jB
jB
dB
dB
�B
	�B
xB
�B
vB
�B
�B
�B
�B
�B
}B
�B
�B
�B
�B
�B
�B
�B
�B
oB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
!�B
#B
#B
#B
$B
#B
!HB
"4B
'B
'B
&B
%FB
%`B
(
B
)B
)*B
($B
($B
(>B
)DB
($B
'mB
)DB
*0B
,"B
,"B
+6B
+QB
-)B
-)B
-)B
.B
.B
.B
./B
./B
-)B
-)B
./B
.IB
-)B
-)B
,=B
+6B
(�B
-)B
,WB
*B
.IB
-]B
-CB
/OB
.cB
0UB
0UB
1vB
0oB
/iB
3hB
3MB
3MB
4TB
5ZB
4nB
3hB
3hB
4nB
5ZB
6`B
6`B
6`B
4nB
3�B
4nB
6`B
6zB
6`B
7�B
6zB
5�B
7�B
9rB
9�B
9�B
9�B
9rB
8lB
9�B
9�B
9�B
8�B
8�B
:�B
9�B
:�B
<�B
<�B
;�B
<�B
=�B
<�B
<�B
<�B
;�B
=�B
>�B
=�B
=�B
>�B
<�B
>�B
=�B
>�B
A�B
A�B
B�B
D�B
D�B
D�B
C�B
C�B
D�B
C�B
B�B
B�B
C�B
D�B
C�B
C�B
D�B
D�B
D�B
C�B
D�B
F�B
F�B
G�B
H�B
H�B
H�B
G�B
F�B
F�B
F�B
IB
I�B
K�B
K�B
K�B
J�B
J�B
I�B
I�B
J	B
KB
KB
KB
M�B
M�B
M�B
M�B
N�B
N�B
N�B
OB
NB
N"B
N�B
O(B
O(B
PB
PB
OB
NB
O(B
O(B
RB
S&B
S&B
SB
SB
R B
R:B
R B
T,B
T�B
TB
T,B
S&B
T,B
S&B
Q4B
TB
VB
VB
W?B
VB
V9B
UMB
W$B
W$B
W$B
X+B
XEB
Y1B
YB
Y1B
X+B
X+B
XEB
X+B
XEB
YKB
ZB
[#B
[#B
[#B
[=B
[#B
ZB
Z7B
Z7B
ZQB
Y1B
X_B
XEB
ZQB
[WB
[qB
[WB
\CB
\]B
\xB
^5B
]IB
\]B
\]B
\xB
^jB
^OB
^OB
^OB
^jB
`\B
`\B
`\B
`\B
aHB
abB
`\B
aHB
abB
a|B
`\B
`\B
`vB
`\B
`\B
a|B
abB
bNB
bNB
b�B
bhB
bhB
b�B
bhB
cTB
bhB
bhB
b�B
cnB
cnB
cnB
dtB
dtB
dtB
dZB
dtB
dtB
e`B
e`B
e`B
e`B
d�B
dtB
ezB
c�B
d�B
e�B
ffB
ezB
d�B
f�B
f�B
g�B
hsB
hsB
h�B
h�B
h�B
h�B
h�B
h�B
h�B
g�B
h�B
h�B
h�B
h�B
g�B
h�B
g�B
g�B
i�B
j�B
k�B
l�B
m�B
m�B
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
n�B
n�B
n�B
m�B
n�B
n�B
m�B
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
n�B
o�B
p�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
o�B
o�B
p�B
p�B
q�B
p�B
q�B
q�B
q�B
r�B
r�B
r�B
q�B
q�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
s�B
s�B
t�B
t�B
s�B
t�B
t�B
t�B
t�B
t�B
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
x�B
x�B
x�B
x�B
x�B
x�B
x�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.06(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201803160034552018031600345520180316003455201806221327302018062213273020180622132730201804050731242018040507312420180405073124  JA  ARFMdecpA19c                                                                20180312093526  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180312003535  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180312003537  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180312003538  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180312003538  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180312003538  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180312003538  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180312003538  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180312003539  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180312003539                      G�O�G�O�G�O�                JA  ARUP                                                                        20180312005543                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180312154955  CV  JULD            G�O�G�O�F�                JM  ARCAJMQC2.0                                                                 20180315153455  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180315153455  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404223124  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622042730  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116221515                      G�O�G�O�G�O�                