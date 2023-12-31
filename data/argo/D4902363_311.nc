CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-12-16T00:36:21Z creation;2018-12-16T00:36:27Z conversion to V3.1;2019-12-19T07:25:37Z update;     
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
_FillValue                    �pArgo profile    3.1 1.2 19500101000000  20181216003621  20200115131518  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL              7A   JA  I2_0576_311                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @ؘuӠm 1   @ؘv����@9Q�����d5O�M1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� DvfDv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�<�DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�G�@�{@�{A
=A?
=A_
=A
=A��A��A��A��AυA߅A�A��BBBBB'B/B7B?BGBOBWB_BgBoBwBB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HC�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RD |)D �)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D	|)D	�)D
|)D
�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D |)D �)D!|)D!�)D"|)D"�)D#|)D#�)D$|)D$�)D%|)D%�)D&|)D&�)D'|)D'�)D(|)D(�)D)|)D)�)D*|)D*�)D+|)D+�)D,|)D,�)D-|)D-�)D.|)D.�)D/|)D/�)D0|)D0�)D1|)D1�)D2|)D2�)D3|)D3�)D4|)D4�)D5|)D5�)D6|)D6�)D7|)D7�)D8|)D8�)D9|)D9�)D:|)D:�)D;|)D;�)D<|)D<�)D=|)D=�)D>|)D>�)D?|)D?�)D@|)D@�)DA|)DA�)DB|)DB�)DC|)DC�)DD|)DD�)DE|)DE�)DF|)DF�)DG|)DG�)DH|)DH�)DI|)DI�)DJ|)DJ�)DK|)DK�)DL|)DL�)DM|)DM�)DN|)DN�)DO|)DO�)DP|)DP�)DQ|)DQ�)DR|)DR�)DS|)DS�)DT|)DT�)DU|)DU�)DV|)DV�)DW|)DW�)DX|)DX�)DY|)DY�)DZ|)DZ�)D[|)D[�)D\|)D\�)D]|)D]�)D^|)D^�)D_|)D_�)D`|)D`�)Da|)Da�)Db|)Db�)Dc|)Dc�)Dd|)Dd�)De|)De�)Df|)Df�)Dg|)Dg�)Dh|)Dh�)Di|)Di�)Dj|)Dj�)Dk|)Dk�)Dl|)Dl�)Dm|)Dm�)Dn|)Dn�)Do|)Do�)Dp|)Dp�)Dq|)Dq�)Dr|)Dr�)Ds|)Ds�)Dt|)Dt�)Du|)Dv�Dv|)Dv�)Dw|)Dw�)Dx|)Dx�)Dy|)Dy�)Dz|)Dz�)D{|)D{�)D||)D|�)D}|)D}�)D~|)D~�)D|)D�)D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D�HD�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D¾D��D�>D�~DþD��D�>D�~DľD��D�>D�~DžD��D�>D�~DƾD��D�>D�~DǾD��D�>D�~DȾD��D�>D�~DɾD��D�>D�~DʾD��D�>D�~D˾D��D�>D�~D̾D��D�>D�~D;D��D�>D�~DξD��D�>D�~DϾD��D�>D�~DоD��D�>D�~DѾD��D�>D�~DҾD��D�>D�~DӾD��D�>D�~DԾD��D�:�D�~DվD��D�>D�~D־D��D�>D�~D׾D��D�>D�~DؾD��D�>D�~DپD��D�>D�~DھD��D�>D�~D۾D��D�>D�~DܾD��D�>D�~DݾD��D�>D�~D޾D��D�>D�~D߾D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D��D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��HD��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�hsA�`BA�\)A�`BA�^5A�^5A�ZA�\)A�\)A�XA�VA�VA�S�A�S�A�S�A�S�A�VA�VA�XA�XA�ZA�^5A�`BA�`BA�bNA�bNA�bNA�`BA�`BA�`BA�XA�M�A�I�A�I�A�G�A�C�A�=qA�JA�/A��A��yA�ZA�M�A��7A���A��A���A�hsA�E�A�K�A�%A�K�A���A��/A�/A��\A�v�A�Q�A��
A�JA�bNA�;dA�VA�Q�A�1A�C�A��PA��#A��wA��!A��A���A��+A�bNA�M�A�7LA��A��A���A��A��HA�/A��`A�"�A��A��A�ffA��A��PA��FA�(�A�A�1'A��A���A�G�A���A�1A�O�A��mA���A��A��A���A��FA��uA�=qA�ĜA��+A��\A}�A|{A{��A{t�A{/AzjAy�Ax��AxbAv��At~�As�Arn�Ar9XArAq�mAq�;Aq��Aq��Aq��Aq&�AoƨAoVAl��Akt�Aj �Ai��AiG�Ai%Ag�TAf=qAb��A`A_�-A^�uA]��A\��A[|�AYAW�;AU�
AT=qASp�AQ33AO��AN��ANI�AL�+AJ9XAH��AHv�AG|�AF��ADn�AC�AA�FA@�A?�A?|�A?�A>ZA<�A;�^A:bA933A7��A5��A4�A4bA2�DA1�wA0A�A/VA.�A-��A,�`A+��A+�A*ȴA*�A)A)��A)33A(��A'�TA%�;A$�`A#�7A"ĜA"1'A!��A!�-A!�A!33A ��A 1'A�^A��A�mA��A�DAVA�A�-AM�AA�A\)An�AA�A�#A��A�A��A�A�+A�PA�DAn�A9XA��A��A  A/A
JAAVA�RAZA��A�
A�^A�\A��A��A?}A ��A �@���@�&�@�33@�v�@���@��j@�z�@�1'@�1@�t�@�~�@��^@�bN@�K�@�J@�bN@�R@��T@홚@�G�@��@�j@睲@�+@��T@��@�K�@�Ĝ@���@�/@ܴ9@�I�@���@�+@ٺ^@�hs@�X@�?}@��@���@��`@���@�Ĝ@؛�@�b@�C�@�M�@թ�@���@�j@��@ӍP@�ȴ@Ь@�V@̛�@�ȴ@��#@�o@���@��m@�{@�9X@���@�\)@�x�@�(�@��y@�@�G�@�O�@��@��9@�Z@�ƨ@�33@�M�@�%@��P@��@�p�@�7L@�&�@�%@���@��D@� �@�@� �@��;@��F@�"�@��@��@��@��h@��j@��F@���@��@�V@��@�A�@�(�@�b@��m@��
@�ƨ@��@�33@�M�@��@�X@�G�@�O�@�/@�&�@���@���@��@��/@��@��@��@���@��9@��@���@��D@��@�z�@�Q�@�  @�\)@�ff@��h@��`@��m@��@��-@��h@���@��;@��@���@��@��@��@��@�x�@�?}@��j@�1@�S�@�ȴ@��+@�^5@�=q@�-@�{@�J@��@��@���@���@�@���@�`B@�/@��@�%@���@���@��@���@��j@��@�A�@��F@�dZ@�K�@�+@�o@��H@��+@��@��@��#@���@���@���@���@�@���@��@�z�@���@��H@��@�X@�%@��@�(�@�  @��F@���@��P@�l�@�C�@�+@�@��y@��!@��\@�v�@�^5@�E�@��@�@��/@��u@�r�@�1'@�1@�@�w@K�@~5?@}O�@|�j@|j@|j@|j@|Z@|I�@|Z@|Z@|I�@|�@{�@{o@z�H@z�\@y�^@x��@v��@u��@u/@t��@tz�@tI�@tZ@tj@t�D@t�@t�/@t��@s��@r��@q�7@p��@o�;@o\)@nȴ@m�@m/@l�/@l��@l�@l�D@l9X@l�@k�
@j�@h�u@g�P@g
=@f�@g+@g
=@f��@f�R@fff@f5?@e�@e@e�T@e��@e?}@d��@dI�@cS�@b��@a&�@a%@`��@a%@a7L@a%@`�u@` �@_�;@_�w@_��@_l�@_�@^�R@^�+@^E�@]�-@]�@]O�@\��@\��@\I�@[�F@[�@[t�@["�@Z�@Y�^@Y&�@Xr�@Xr�@XbN@XbN@XA�@XA�@X  @W�;@W��@W|�@W\)@W+@W
=@W
=@V��@V�@V�+@V$�@U/@T�/@T�j@T�j@T�j@T�j@T�j@T�@T�D@TZ@S�@S33@R��@R~�@R^5@R�@RJ@Q��@Q7L@P��@PQ�@O�@OK�@N�@N��@NE�@N@M�T@M��@MO�@L��@L��@L��@L��@L��@L��@L��@L�j@L��@L�@K�F@K�F@Kƨ@Kƨ@K��@K��@K�@KS�@K"�@K@J��@JM�@HbN@G�@G�@G�@G
=@G�@F�y@Fȴ@F�+@FV@F5?@F5?@F5?@F@E�@E�T@E@D��@D1@C"�@B^5@Ax�@@�9@@1'@?+@>E�@>$�@=�-@=�@=O�@=V@<z�@<9X@;ƨ@;��@;�@;33@:�!@9��@9�7@8r�@81'@8 �@7�@7��@7�@6E�@4�j@3@2�!@2^5@2M�@1��@1�^@1�^@1�7@1��@1�7@1hs@1X@1&�@1%@0��@0�u@0�@0�@0r�@0r�@0r�@0bN@0Q�@0Q�@01'@01'@01'@0 �@0  @/��@/|�@/\)@/\)@/+@/
=@/
=@/
=@/
=@/
=@.�y@.�@.�@.�R@.��@.��@.��@.�+@.E�@.$�@-�@-@-�h@-`B@-/@,�j@,1@+ƨ@+��@+�@+C�@+33@+"�@+"�@+"�@+o@+o@+@*�@*�H@*�H@*�H@*��@*��@*��@*��@*��@*��@*��@*��@*��@*��@*��@*��@*��@*�H@*��@*n�@*J@)�^@)&�@)&�@)�@(��@(�`@(��@(��@(r�@(Q�@( �@(  @'�;@'�w@'�@'��@'�P@'�@%�h@$�@$I�@#ƨ@#��@#dZ@#C�@#33@#@"��@"��@"~�@"M�@"�@!�#@!�7@!hs@!hs@!X@!G�@!7L@ ��@ r�@�@�P@;d@�+@��@�h@/@��@j@I�@�@�H@��@�\@n�@=q@��@�^@�7@&�@�@%@�`@�9@��@Q�@�;@��@�@�P@|�@;d@v�@@@@@@@�T@@p�@`B@?}@V@�@��@j@Z@Z@I�@I�@�@�\@=q@J@��@�@�@�#@��@��@�^@��@��@��@��@��@hs@G�@&�@�`@bN@bN@1'@  @  @��@�w@��@|�@l�@
=@��@ff@E�@$�@�@�T@��@O�@��@�j@j@(�@1@��@�
@ƨ@�F@�F@��@t�@S�@o@
��@
��@
�\@
n�@
�@	�@	��@	��@	7L@	%@��@��@�`@�9@�@r�@bN@bN@bN@bN@Q�@Q�@Q�@A�@b@�;@��@��@�w@�w@��@��@�P@|�@\)@\)@K�@;d@+@
=@�@��@v�@V@5?@��@�h@`B@/@��@�@�@�@�@�@��@�@�@��@��@�D@(�@�@�@1@�m@��@��@�@S�@33@"�@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�hsA�`BA�\)A�`BA�^5A�^5A�ZA�\)A�\)A�XA�VA�VA�S�A�S�A�S�A�S�A�VA�VA�XA�XA�ZA�^5A�`BA�`BA�bNA�bNA�bNA�`BA�`BA�`BA�XA�M�A�I�A�I�A�G�A�C�A�=qA�JA�/A��A��yA�ZA�M�A��7A���A��A���A�hsA�E�A�K�A�%A�K�A���A��/A�/A��\A�v�A�Q�A��
A�JA�bNA�;dA�VA�Q�A�1A�C�A��PA��#A��wA��!A��A���A��+A�bNA�M�A�7LA��A��A���A��A��HA�/A��`A�"�A��A��A�ffA��A��PA��FA�(�A�A�1'A��A���A�G�A���A�1A�O�A��mA���A��A��A���A��FA��uA�=qA�ĜA��+A��\A}�A|{A{��A{t�A{/AzjAy�Ax��AxbAv��At~�As�Arn�Ar9XArAq�mAq�;Aq��Aq��Aq��Aq&�AoƨAoVAl��Akt�Aj �Ai��AiG�Ai%Ag�TAf=qAb��A`A_�-A^�uA]��A\��A[|�AYAW�;AU�
AT=qASp�AQ33AO��AN��ANI�AL�+AJ9XAH��AHv�AG|�AF��ADn�AC�AA�FA@�A?�A?|�A?�A>ZA<�A;�^A:bA933A7��A5��A4�A4bA2�DA1�wA0A�A/VA.�A-��A,�`A+��A+�A*ȴA*�A)A)��A)33A(��A'�TA%�;A$�`A#�7A"ĜA"1'A!��A!�-A!�A!33A ��A 1'A�^A��A�mA��A�DAVA�A�-AM�AA�A\)An�AA�A�#A��A�A��A�A�+A�PA�DAn�A9XA��A��A  A/A
JAAVA�RAZA��A�
A�^A�\A��A��A?}A ��A �@���@�&�@�33@�v�@���@��j@�z�@�1'@�1@�t�@�~�@��^@�bN@�K�@�J@�bN@�R@��T@홚@�G�@��@�j@睲@�+@��T@��@�K�@�Ĝ@���@�/@ܴ9@�I�@���@�+@ٺ^@�hs@�X@�?}@��@���@��`@���@�Ĝ@؛�@�b@�C�@�M�@թ�@���@�j@��@ӍP@�ȴ@Ь@�V@̛�@�ȴ@��#@�o@���@��m@�{@�9X@���@�\)@�x�@�(�@��y@�@�G�@�O�@��@��9@�Z@�ƨ@�33@�M�@�%@��P@��@�p�@�7L@�&�@�%@���@��D@� �@�@� �@��;@��F@�"�@��@��@��@��h@��j@��F@���@��@�V@��@�A�@�(�@�b@��m@��
@�ƨ@��@�33@�M�@��@�X@�G�@�O�@�/@�&�@���@���@��@��/@��@��@��@���@��9@��@���@��D@��@�z�@�Q�@�  @�\)@�ff@��h@��`@��m@��@��-@��h@���@��;@��@���@��@��@��@��@�x�@�?}@��j@�1@�S�@�ȴ@��+@�^5@�=q@�-@�{@�J@��@��@���@���@�@���@�`B@�/@��@�%@���@���@��@���@��j@��@�A�@��F@�dZ@�K�@�+@�o@��H@��+@��@��@��#@���@���@���@���@�@���@��@�z�@���@��H@��@�X@�%@��@�(�@�  @��F@���@��P@�l�@�C�@�+@�@��y@��!@��\@�v�@�^5@�E�@��@�@��/@��u@�r�@�1'@�1@�@�w@K�@~5?@}O�@|�j@|j@|j@|j@|Z@|I�@|Z@|Z@|I�@|�@{�@{o@z�H@z�\@y�^@x��@v��@u��@u/@t��@tz�@tI�@tZ@tj@t�D@t�@t�/@t��@s��@r��@q�7@p��@o�;@o\)@nȴ@m�@m/@l�/@l��@l�@l�D@l9X@l�@k�
@j�@h�u@g�P@g
=@f�@g+@g
=@f��@f�R@fff@f5?@e�@e@e�T@e��@e?}@d��@dI�@cS�@b��@a&�@a%@`��@a%@a7L@a%@`�u@` �@_�;@_�w@_��@_l�@_�@^�R@^�+@^E�@]�-@]�@]O�@\��@\��@\I�@[�F@[�@[t�@["�@Z�@Y�^@Y&�@Xr�@Xr�@XbN@XbN@XA�@XA�@X  @W�;@W��@W|�@W\)@W+@W
=@W
=@V��@V�@V�+@V$�@U/@T�/@T�j@T�j@T�j@T�j@T�j@T�@T�D@TZ@S�@S33@R��@R~�@R^5@R�@RJ@Q��@Q7L@P��@PQ�@O�@OK�@N�@N��@NE�@N@M�T@M��@MO�@L��@L��@L��@L��@L��@L��@L��@L�j@L��@L�@K�F@K�F@Kƨ@Kƨ@K��@K��@K�@KS�@K"�@K@J��@JM�@HbN@G�@G�@G�@G
=@G�@F�y@Fȴ@F�+@FV@F5?@F5?@F5?@F@E�@E�T@E@D��@D1@C"�@B^5@Ax�@@�9@@1'@?+@>E�@>$�@=�-@=�@=O�@=V@<z�@<9X@;ƨ@;��@;�@;33@:�!@9��@9�7@8r�@81'@8 �@7�@7��@7�@6E�@4�j@3@2�!@2^5@2M�@1��@1�^@1�^@1�7@1��@1�7@1hs@1X@1&�@1%@0��@0�u@0�@0�@0r�@0r�@0r�@0bN@0Q�@0Q�@01'@01'@01'@0 �@0  @/��@/|�@/\)@/\)@/+@/
=@/
=@/
=@/
=@/
=@.�y@.�@.�@.�R@.��@.��@.��@.�+@.E�@.$�@-�@-@-�h@-`B@-/@,�j@,1@+ƨ@+��@+�@+C�@+33@+"�@+"�@+"�@+o@+o@+@*�@*�H@*�H@*�H@*��@*��@*��@*��@*��@*��@*��@*��@*��@*��@*��@*��@*��@*�H@*��@*n�@*J@)�^@)&�@)&�@)�@(��@(�`@(��@(��@(r�@(Q�@( �@(  @'�;@'�w@'�@'��@'�P@'�@%�h@$�@$I�@#ƨ@#��@#dZ@#C�@#33@#@"��@"��@"~�@"M�@"�@!�#@!�7@!hs@!hs@!X@!G�@!7L@ ��@ r�@�@�P@;d@�+@��@�h@/@��@j@I�@�@�H@��@�\@n�@=q@��@�^@�7@&�@�@%@�`@�9@��@Q�@�;@��@�@�P@|�@;d@v�@@@@@@@�T@@p�@`B@?}@V@�@��@j@Z@Z@I�@I�@�@�\@=q@J@��@�@�@�#@��@��@�^@��@��@��@��@��@hs@G�@&�@�`@bN@bN@1'@  @  @��@�w@��@|�@l�@
=@��@ff@E�@$�@�@�T@��@O�@��@�j@j@(�@1@��@�
@ƨ@�F@�F@��@t�@S�@o@
��@
��@
�\@
n�@
�@	�@	��@	��@	7L@	%@��@��@�`@�9@�@r�@bN@bN@bN@bN@Q�@Q�@Q�@A�@b@�;@��@��@�w@�w@��@��@�P@|�@\)@\)@K�@;d@+@
=@�@��@v�@V@5?@��@�h@`B@/@��@�@�@�@�@�@��@�@�@��@��@�D@(�@�@�@1@�m@��@��@�@S�@33@"�@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BdZBe`BffBe`Be`Be`Be`Be`BdZBdZBdZBdZBdZBdZBdZBe`Be`Be`Be`BffBhsBiyBiyBiyBiyBjBjBl�Bl�Bl�Bp�Bt�Bu�Bt�Bs�Bo�BffBI�B�B?}BB��B>wB8RB6FBO�BI�BH�BS�BbNBT�B33BM�B^5BYBhsB_;BK�B8RB33B$�B�B�BVBB+B�BB�B��B�B��B��B��B��B��B�B�DB�Bt�BffBF�B"�BK�BG�B@�B@�B7LB&�B�B �BPB
��B
�B
�B
�XB
�XB
��B
�B
VB
cTB
t�B
�+B
�B
y�B
k�B
]/B
T�B
5?B
oB
�B
33B
.B
&�B
�B
hB	��B	�B	�)B	��B	�B	�B	�B	�B	��B	��B	�B	�B	�yB	�B	��B	�}B	��B	��B	��B	�B	�B	��B	�\B	m�B	Q�B	F�B	� B	l�B	ffB	VB	H�B	7LB	#�B	�B	�B	&�B	B	VB	bB	hB��B�HB�B��B�B�#BƨB��B��B��B�B�B��BŢB�9B�9B��B�B��B��B��B��B�bB��B�bB�bB��B��B�{B�PB��B��B��B��B��B�\B�7Bw�Be`Bp�Bk�Bw�B|�B�B�B� B|�Bu�Bq�BgmBQ�BF�B]/BffBhsBaHBW
B@�B33BC�BG�BM�BS�BM�B?}B<jBF�B<jB>wB6FB33BE�BA�B8RB)�B)�B#�B�BoB&�B49B/B�B,B7LB(�B(�B7LB33B-B%�B#�B%�B!�B/B1'B0!B8RB6FB5?B1'B+B)�B$�B%�B$�B!�B&�B1'B7LB33B%�B�B0!B9XB1'B/B)�B"�B/B49BD�BD�BD�B@�B>wBN�BR�BR�BQ�BR�BR�BQ�BO�BM�BG�BF�BD�BI�BI�BJ�BJ�BF�B>wB1'B.B7LB6FBB�B7LB8RBO�BM�BL�B[#BW
BI�BR�B^5BdZBl�Bu�B|�B�B�B� B{�Bv�Bl�Bl�Be`Br�Bu�Bw�Bu�Bs�Bp�BjB_;BdZB~�B}�Bw�Bs�Bs�Bm�Be`B}�B�B�B�=B�VB��B��B��B��B��B��B��B��B��B��B��B��B�-B�3B�-B�-B�-B�9B�9B�3B�3B�FB�FB�FB�FB�?B�?B�9B�?B�3B�'B�B�B�B�!B�9B�9B�3B��B��B��BɺB��B�BB�;B�TB�B�B�yB�mB�fB�yB�B��B��B	B	B	B	B	%B	%B	+B	%B	+B	+B	%B	%B		7B	DB	PB	PB	PB	PB	JB	JB	JB	DB	JB	oB	�B	�B	�B	�B	�B	�B	!�B	$�B	%�B	&�B	%�B	%�B	#�B	 �B	�B	!�B	!�B	(�B	0!B	;dB	D�B	G�B	L�B	R�B	S�B	YB	ZB	ZB	[#B	^5B	_;B	bNB	bNB	dZB	e`B	ffB	gmB	gmB	gmB	gmB	o�B	t�B	t�B	x�B	z�B	z�B	y�B	y�B	� B	�%B	�JB	�bB	�bB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�{B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�'B	�3B	�FB	�?B	�FB	�LB	�dB	�}B	�wB	�qB	�wB	��B	�wB	�jB	�^B	ÖB	ŢB	ȴB	��B	��B	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�#B	�)B	�/B	�;B	�5B	�/B	�5B	�NB	�TB	�TB	�TB	�TB	�ZB	�`B	�fB	�`B	�sB	�yB	�sB	�yB	�sB	�yB	�B	�B	�B	�sB	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
1B

=B

=B
DB
JB
DB
JB
JB
\B
VB
bB
bB
bB
\B
\B
VB
PB
VB
oB
oB
hB
hB
hB
hB
bB
bB
bB
\B
JB
1B
bB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
uB
uB
{B
�B
�B
�B
�B
�B
�B
"�B
!�B
"�B
#�B
"�B
!�B
#�B
#�B
$�B
$�B
#�B
!�B
!�B
"�B
!�B
'�B
)�B
'�B
%�B
$�B
"�B
!�B
"�B
/B
0!B
2-B
1'B
2-B
33B
33B
49B
33B
33B
33B
33B
33B
33B
33B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
49B
49B
49B
49B
5?B
6FB
5?B
6FB
7LB
7LB
7LB
7LB
6FB
7LB
7LB
6FB
7LB
7LB
7LB
7LB
6FB
7LB
6FB
7LB
7LB
6FB
6FB
5?B
5?B
9XB
:^B
:^B
:^B
<jB
<jB
=qB
<jB
<jB
<jB
<jB
<jB
<jB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
<jB
;dB
:^B
:^B
;dB
;dB
?}B
?}B
?}B
?}B
@�B
@�B
?}B
A�B
B�B
B�B
C�B
C�B
C�B
B�B
A�B
>wB
:^B
?}B
E�B
E�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
I�B
J�B
J�B
K�B
L�B
L�B
K�B
K�B
I�B
H�B
H�B
J�B
J�B
I�B
J�B
M�B
M�B
M�B
O�B
P�B
M�B
P�B
S�B
T�B
T�B
S�B
S�B
T�B
T�B
VB
W
B
XB
W
B
W
B
W
B
W
B
VB
YB
YB
YB
YB
W
B
VB
YB
\)B
]/B
\)B
\)B
\)B
[#B
[#B
[#B
\)B
\)B
\)B
\)B
[#B
]/B
^5B
^5B
]/B
[#B
XB
XB
^5B
`BB
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
aHB
aHB
aHB
aHB
`BB
dZB
cTB
cTB
dZB
cTB
dZB
dZB
dZB
dZB
bNB
bNB
e`B
ffB
ffB
e`B
ffB
e`B
cTB
dZB
ffB
ffB
gmB
iyB
iyB
iyB
iyB
jB
jB
iyB
iyB
iyB
iyB
iyB
jB
jB
jB
jB
jB
jB
k�B
jB
l�B
n�B
n�B
m�B
m�B
m�B
n�B
o�B
o�B
o�B
o�B
n�B
o�B
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
o�B
o�B
o�B
n�B
n�B
n�B
o�B
o�B
o�B
n�B
o�B
p�B
p�B
q�B
r�B
s�B
s�B
r�B
r�B
r�B
r�B
s�B
s�B
r�B
r�B
q�B
s�B
t�B
s�B
r�B
r�B
t�B
s�B
s�B
t�B
t�B
t�B
t�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Bd�Be`BffBezBe`Be`Be`Be`BdZBdZBdZBdZBdZBdZBdZBe`Be`Be`Be`BffBhsBiyBiyBiyBiyBjBjBl�Bl�Bl�Bp�Bt�Bu�Bt�Bs�BpBg�BL�B%�BB'B
XB��BA�B<�B:^BR BL�BKDBU�Bc:BW$B6�BO�B_�BZQBh�B_�BM6B:DB5%B'�B=B�BBB�B��B�_B��B��B�9B҉B�NB�.B�B�B�oB��B�9Bv�BhXBI�B&�BL�BIBA�BA�B8lB(�B!-B!�B�B
�DB
�B
�B
��B
�0B
�B
��B
Z�B
e�B
u�B
�+B
�oB
zxB
l�B
^jB
VB
8B
B
]B
3hB
.cB
'mB
�B
:B	�]B	��B	�B	�VB	ٚB	�B	�B	��B	��B	��B	��B	��B	��B	�1B	�uB	�B	�zB	��B	�HB	��B	��B	��B	�4B	pUB	VB	I�B	�B	nB	g�B	W�B	JXB	9�B	&fB	"NB	�B	'�B	B	B	�B	�B�8B�&B�=B�B��B��B�RB̈́BϑB�NB��BּBԯB��B�FB��B�"B��B��B��B��B�&B��B��B�TB��B�dB��B��B��B�EB�)B��B�
B��B�B��By�Bg�Bq�Bm]Bx�B}�B�uB�oB�iB}�Bv�Br|Bh�BT�BH�B^jBf�Bh�Ba�BXEBB�B5�BD�BH�BNVBT,BNVB@�B=qBGEB=�B?HB7�B4nBE�BBB9$B+QB+QB%FB!bB�B($B4�B0!B�B-B7�B*�B*B7�B3�B-�B'8B%B'B# B/�B1�B0�B8�B6�B5�B1�B+�B*�B%�B&�B%�B# B(
B1�B7�B3�B'8B �B0�B9�B2-B0!B+QB$�B0oB5ZBD�BEBEBAUB?}BN�BSBS@BR BSBSBRBPBN"BHKBGEBESBJXBJXBK)BK)BGEB?cB2�B/�B8�B7�BCaB9XB9�BP�BOBBN"B[qBW�BK)BS�B_!Bd�BmBu�B}<B�uB�oB��B|�Bw�Bm�Bm�Bf�Br�Bu�Bw�Bu�BtBq'Bk6B`�Be�B~�B~BBx�Bt�BtnBn�BgB~�B��B�B��B��B��B�B��B��B�B�B�B��B�5B�dB�XB�eB�GB�hB�aB�aB�aB�TB�TB�MB�hB�`B�`B�zB�`B�ZB�ZB�TB�ZB�MB�vB�}B��B��B��B��B�%B��B��B�:B�jB��B��B�vB߾B�B�B�B�B�B�B�0B�!B�2B�(B	;B	GB	9B	9B	?B	YB	EB	YB	EB	EB	YB	YB		lB	^B	jB	jB	jB	�B	dB	~B	~B	�B	�B	�B	�B	�B	�B	�B	�B	�B	"B	$�B	&B	&�B	%�B	&B	#�B	!B	5B	"4B	"�B	)�B	0�B	;�B	D�B	HB	MB	S@B	TFB	YKB	Z7B	Z7B	[=B	^OB	_�B	b�B	b�B	dtB	ezB	f�B	g�B	g�B	g�B	h$B	o�B	t�B	t�B	x�B	{B	z�B	z*B	zDB	�OB	�YB	�dB	�bB	�}B	��B	�uB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�2B	��B	�B	�B	��B	�$B	�B	�B	�B	�/B	�/B	�)B	�kB	��B	��B	��B	��B	�zB	��B	��B	��B	�B	�}B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	� B	�B	�,B	�@B	�[B	�SB	�oB	�#B	�CB	�/B	�;B	�OB	�~B	ބB	�hB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�+B	�B	�B	��B	�B	��B	��B	�B	�B	�B	�0B	�B
;B
'B
MB
MB
MB
GB
GB
SB
gB
SB
mB
�B

XB

rB
xB
dB
xB
dB
dB
\B
�B
bB
}B
bB
\B
�B
pB
�B
pB
oB
�B
hB
�B
�B
�B
�B
�B
}B
vB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
B
�B
"�B
!�B
#B
#�B
"�B
!�B
#�B
$B
%B
%B
$B
!�B
"B
# B
"4B
($B
)�B
($B
&B
%,B
#:B
"hB
#nB
/5B
0;B
2-B
1[B
2GB
33B
3MB
4TB
3hB
3MB
3MB
3hB
3hB
3hB
3hB
5ZB
5?B
5?B
5?B
5?B
5ZB
5?B
5?B
5ZB
5?B
5?B
4nB
4TB
4TB
4TB
5ZB
6FB
5ZB
6`B
7fB
7LB
7LB
7LB
6zB
7fB
7LB
6`B
7LB
7LB
7fB
7�B
6`B
7�B
6`B
7�B
7�B
6zB
6zB
5tB
5�B
9�B
:xB
:xB
:�B
<�B
<jB
=qB
<jB
<jB
<jB
<jB
<�B
<jB
=qB
=qB
=�B
=qB
=qB
=qB
=qB
=�B
=qB
=qB
=qB
=qB
=�B
=qB
=qB
<jB
;B
:xB
:�B
;B
;�B
?}B
?�B
?�B
?�B
@�B
@�B
?�B
A�B
B�B
B�B
C�B
C�B
C�B
B�B
A�B
>�B
:�B
?�B
E�B
E�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
I�B
J�B
J�B
K�B
L�B
L�B
K�B
K�B
I�B
IB
H�B
J�B
KB
J	B
J�B
M�B
NB
NB
O�B
QB
N<B
QB
TB
T�B
UB
TB
TB
U2B
UB
VB
W
B
XB
W$B
W$B
W$B
W$B
V9B
Y1B
Y1B
YKB
YKB
WYB
VmB
Y1B
\CB
]IB
\)B
\)B
\)B
[=B
[=B
[WB
\CB
\CB
\]B
\]B
[=B
]IB
^OB
^5B
]/B
[=B
XyB
X_B
^jB
`vB
aHB
bhB
bNB
bNB
bhB
bNB
bNB
b�B
cnB
bhB
bhB
bNB
a|B
a|B
a|B
abB
`vB
dtB
c�B
c�B
dtB
c�B
dZB
dtB
dtB
d�B
b�B
b�B
ezB
f�B
f�B
e�B
ffB
ezB
c�B
dtB
f�B
f�B
g�B
iyB
iyB
i�B
iyB
jB
jB
i�B
i�B
i�B
i�B
i�B
jB
j�B
j�B
j�B
j�B
j�B
k�B
j�B
l�B
n�B
n�B
m�B
m�B
m�B
n�B
o�B
o�B
o�B
o�B
n�B
o�B
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
o�B
o�B
o�B
n�B
n�B
n�B
o�B
o�B
o�B
n�B
o�B
p�B
p�B
q�B
r�B
s�B
s�B
r�B
r�B
r�B
r�B
s�B
s�B
r�B
r�B
q�B
s�B
t�B
s�B
r�B
r�B
t�B
s�B
s�B
t�B
t�B
t�B
t�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.06(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201812200033292018122000332920181220003329201812200200162018122002001620181220020016201812210024242018122100242420181221002424  JA  ARFMdecpA19c                                                                20181216093619  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20181216003621  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20181216003625  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20181216003625  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20181216003626  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20181216003626  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20181216003626  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20181216003626  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20181216003626  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20181216003627                      G�O�G�O�G�O�                JA  ARUP                                                                        20181216005638                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20181216153702  CV  JULD            G�O�G�O�F�ï                JM  ARCAJMQC2.0                                                                 20181219153329  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20181219153329  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20181219170016  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20181220152424  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115131518                      G�O�G�O�G�O�                