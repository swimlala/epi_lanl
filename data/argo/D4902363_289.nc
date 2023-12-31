CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-10-11T00:35:16Z creation;2018-10-11T00:35:21Z conversion to V3.1;2019-12-19T07:30:44Z update;     
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
resolution        =���   axis      Z        d  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I    PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     d  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \@   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     d  `   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     d  s\   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     d  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     d  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �@   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     d  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     d  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     d  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �H   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �X   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �\   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �`   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �d   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �h   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181011003516  20200115131517  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL              !A   JA  I2_0576_289                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @؇�,/ 1   @؇�+��@9sݗ�+k�d8����1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @y��@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(ffB0  B8  B@  BH  BP  BX  B`  Bh  BpffBx  B��B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C�fC  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH�CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0�fD1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׃3D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�P D�` 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @u@�{@�{A
=A?
=A_
=A
=A��A��A��A��AυA߅A�A��BBBBB((�B/B7B?BGBOBWB_BgBp(�BwB\)B��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HBӮB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HC�
C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CH
=CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��C��RC��RC��RC��RC��RD |)D �)D|)D�)D|)D�)D|)D�)Du�D�)D|)D�)D|)D�)D|)D�)D|)D�)D	|)D	�)D
|)D
�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D |)D �)D!|)D!�)D"|)D"�)D#|)D#�)D$|)D$�)D%|)D%�)D&|)D&�)D'|)D'�)D(|)D(�)D)|)D)�)D*|)D*�)D+|)D+�)D,|)D,�)D-|)D-�)D.|)D.�)D/|)D/�)D0��D0�)D1|)D1�)D2|)D2�)D3|)D3�)D4|)D4�)D5|)D5�)D6|)D6�)D7|)D7�)D8|)D8�)D9|)D9�)D:|)D:�)D;|)D;�)D<|)D<�)D=|)D=�)D>|)D>�)D?|)D?�)D@|)D@�)DA|)DA�)DB|)DB�)DC|)DC�)DD|)DD�)DE|)DE�)DF|)DF�)DG|)DG�)DH|)DH�)DI|)DI�)DJ|)DJ�)DK|)DK�)DL|)DL�)DM|)DM�)DN|)DN�)DO|)DO�)DP|)DP�)DQ|)DQ�)DR|)DR�)DS|)DS�)DT|)DT�)DU|)DU�)DV|)DV�)DW|)DW�)DX|)DX�)DY|)DY�)DZ|)DZ�)D[|)D[�)D\|)D\�)D]|)D]�)D^|)D^�)D_|)D_�)D`|)D`�)Da|)Da�)Db|)Db�)Dc|)Dc�)Dd|)Dd�)De|)De�)Df|)Df�)Dg|)Dg�)Dh|)Dh�)Di|)Di�)Dj|)Dj�)Dk|)Dk�)Dl|)Dl�)Dm|)Dm�)Dn|)Dn�)Do|)Do�)Dp|)Dp�)Dq|)Dq�)Dr|)Dr�)Ds|)Ds�)Dt|)Dt�)Du|)Du�)Dv|)Dv�)Dw|)Dw�)Dx|)Dx�)Dy|)Dy�)Dz|)Dz�)D{|)D{�)D||)D|�)D}|)D}�)D~|)D~�)D|)D�)D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D¾D��D�>D�~DþD��D�>D�~DľD��D�>D�~DžD��D�>D�~DƾD��D�>D�~DǾD��D�>D�~DȾD��D�>D�~DɾD��D�>D�~DʾD��D�>D�~D˾D��D�>D�~D̾D��D�>D�~D;D��D�>D�~DξD��D�>D�~DϾD��D�>D�~DоD��D�>D�~DѾD��D�>D�~DҾD��D�>D�~DӾD��D�>D�~DԾD��D�>D�~DվD��D�>D�~D־D��D�>DׁHD׾D��D�>D�~DؾD��D�>D�~DپD��D�>D�~DھD��D�>D�~D۾D��D�>D�~DܾD��D�>D�~DݾD��D�>D�~D޾D��D�>D�~D߾D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D��HD��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D��D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D��D��D�>D�~D��D��D�ND�^1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�bNA�bNA�bNA�^5A�\)A�VA�ZA�S�A�S�A�O�A�K�A�K�A�I�A�I�A�G�A�G�A�E�A�=qA�1'A� �A���A���A�I�A��#Aϝ�A�t�A�33AΟ�A�+A��A���AÍPA���A�
=A�n�A���A��A���A�ffA�"�A��A���A�C�A�A��RA��HA�I�A�1'A��A�E�A� �A��A�z�A�C�A�7LA�A���A�A� �A���A�hsA��yA�ZA�t�A�XA�G�A�ȴA��A���A���A�oA���A��A�1'A��A�  A��A�;dA��wA���A�v�A�=qA��A��\A��#A�VA��mA�1'A���A���A��A�/A���A�O�AC�A}�-A|�/A|I�A{hsAz�`Ay�mAyAx�!Aw��Aw\)Av��At��As�^Aq�Ap�9Ao��An�Am�FAlM�Ak�Aj��Aj^5AiAh(�Ag"�AfQ�Ae"�Ad�RAd(�Acl�Ab�\Aa�Aa�AaS�A`��A_S�A]ƨA\ĜA[�AZ�jAYt�AX�AX�DAW�AVbNAT��AR��AQ7LAPAO�AO"�ANn�AM�FAL��AK�AK�AJ�RAJQ�AIl�AH-AG�wAF�HAFn�AE��AD�RAC�#AC��AC+ABv�A@�\A??}A>�A>Q�A=�
A=VA<{A:=qA9%A8~�A7oA6��A6jA6M�A61A5G�A4�jA4ffA41'A2�A2VA1�mA0�A0E�A/ƨA/
=A.r�A-��A-�A,��A,  A*��A)ƨA(ĜA(-A'�A'ƨA'%A%��A$��A#��A#��A#K�A"=qA �9A�-A|�AA�+AQ�A1A�PA��AȴA��AM�A��A �A��A��Ax�A�yAQ�A��A��A1'A��A7LA�AbA�#AS�A�A�jAE�A;dAVA��AE�A
�+A	O�AȴAI�A �AƨA��A�7AhsA"�A�A~�A�#A��A�AA�A Z@��@��@���@�dZ@���@�V@�@�v�@�hs@���@�  @��y@�=q@�Q�@�@�$�@��@�x�@� �@�^5@�j@��@݁@�b@���@��T@�&�@�+@Ձ@�Z@��m@��@���@�/@�\)@�J@̬@��
@�;d@�/@�z�@�Z@�Z@� �@��;@ǍP@Ƨ�@š�@���@��@�@�O�@�z�@�-@���@��
@�|�@�o@�~�@�V@�ƨ@���@��+@�G�@�1@�"�@��@�@�x�@��@��`@��@�G�@��F@�C�@�5?@���@��@�Q�@�(�@�v�@�hs@���@�z�@�j@�9X@��P@���@�M�@�5?@��@��^@�V@���@���@�33@��@���@�V@�@�Q�@��H@��R@�ff@��#@��@���@��@�A�@�1'@� �@���@���@�33@�o@�@�ȴ@��+@�V@�-@���@�V@�Q�@��;@�ƨ@��@���@��P@��@�~�@��^@��`@�A�@��@���@��F@��P@�@��y@��@��\@��T@�&�@���@�1'@��
@�|�@�;d@�"�@���@��\@�^5@�=q@��@��@���@�X@��j@�z�@�bN@�A�@��;@��w@��@�\)@�o@��+@�{@��@��@��#@�V@��`@�Ĝ@��9@�z�@�I�@�b@�b@�1@��@���@��;@�z�@�bN@�9X@�1'@��@��;@���@��y@�n�@�~�@��+@�@���@���@�`B@�G�@�O�@�G�@�%@�r�@�(�@��@��@;d@~�@�@K�@~�y@|�D@{t�@z�@y7L@xĜ@xr�@w�;@w
=@v��@v$�@vv�@y7L@x�`@yX@x  @uV@u�@uO�@u`B@u�@u��@u�T@vȴ@v5?@u��@u?}@tz�@st�@r�\@r�\@r=q@q�@qx�@qG�@q7L@qG�@qG�@q&�@q&�@qX@qX@qG�@p��@p�@pb@o�;@ol�@o�w@pQ�@pĜ@qG�@q��@q��@q&�@p�`@p �@nV@o�@nff@n{@m�T@m��@l�D@k�F@j^5@i�@h�u@g�@h  @gl�@gK�@e�@f�+@g��@g�;@g;d@fȴ@f@e�@e�T@e��@e`B@e�@d�@d��@d�@b��@b�!@b��@bn�@bJ@a�^@aX@a&�@`��@`��@`Ĝ@`  @_�@^�R@^ȴ@^��@]�-@]�-@]��@]��@]/@\�j@\z�@[@Z~�@Z~�@ZM�@Y�^@X��@X��@X�u@XA�@W�@W\)@W;d@V�y@V$�@U��@Up�@Tz�@T�@Tj@TZ@Tj@T�@T�@T1@S��@T�@T(�@S��@R��@R-@Q�7@QG�@Q�@P�9@P  @P  @O�P@N�y@N��@NE�@N{@N@M�@N{@MO�@Kƨ@J�H@Jn�@J^5@Jn�@Jn�@I��@I7L@IG�@IG�@I%@H�@H �@G��@G��@GK�@G�@G
=@F�y@Fȴ@F��@Fv�@FV@E�@E�@EO�@E?}@E�@E�@D��@D��@D�@D�j@DZ@C�@C33@Co@B�H@B��@B��@B��@B^5@BM�@A��@A��@A��@AX@A&�@A&�@A&�@A�@@�`@@�u@@Q�@@A�@@A�@@A�@@ �@?�@?��@?K�@?
=@>�y@>ff@>5?@>$�@=�@=�@<I�@;�
@;��@;dZ@;S�@;S�@;o@:��@:��@:�\@:~�@9��@9�7@9x�@9hs@9hs@9G�@9&�@9�@9�@9%@8��@8��@8��@8Ĝ@8�u@8�@8�@8�@8r�@8Q�@8b@7�w@7�@6�y@6�@6ȴ@6ȴ@6�R@6��@6��@6v�@6ff@65?@5p�@4�/@4�D@4j@3�m@3��@3dZ@3@2��@2�@1�#@1��@1��@1��@1�7@1hs@1X@1�@0�9@0bN@0Q�@0b@0  @/�@/��@/�P@/\)@/+@.�y@.��@.ff@.5?@-�@-��@-��@-�@-`B@-O�@-?}@-�@,�@,�j@,�@,��@,��@,�D@,z�@,j@,I�@,I�@,9X@,1@+t�@+"�@*��@*��@*�@)��@)hs@)�@(Q�@'�@'��@'|�@'\)@&�@&��@&5?@%@%�-@%�h@$�@$�@$Z@$1@#�F@#dZ@#@"��@"�\@"=q@!��@!X@!&�@ ��@ �@ r�@ r�@ Q�@ A�@ 1'@ b@   @�;@��@�P@K�@�R@$�@@�-@��@�@`B@O�@?}@V@�j@z�@�D@�D@Z@�@�F@��@S�@o@�H@��@��@n�@=q@��@��@��@X@&�@%@Ĝ@�u@�@bN@1'@�;@�@�P@+@�@�+@V@E�@�@�-@�@?}@��@�/@��@Z@9X@��@�F@33@@@@�@��@��@~�@M�@�@��@��@x�@7L@%@Ĝ@�u@r�@A�@1'@b@�;@�w@;d@�y@�R@��@�+@V@E�@5?@�@��@@O�@�@V@�/@�j@z�@I�@I�@I�@9X@�@ƨ@t�@S�@C�@"�@o@@
��@
�\@
n�@
-@
�@	��@	x�@	hs@	G�@	7L@	&�@	&�@	�@	�@��@�`@Ĝ@��@r�@ �@�;@|�@|�@K�@;d@;d@;d@�@ȴ@��@�+@V@@�@�T@@�h@?}@��@�@�@��@��@�j1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�bNA�bNA�bNA�^5A�\)A�VA�ZA�S�A�S�A�O�A�K�A�K�A�I�A�I�A�G�A�G�A�E�A�=qA�1'A� �A���A���A�I�A��#Aϝ�A�t�A�33AΟ�A�+A��A���AÍPA���A�
=A�n�A���A��A���A�ffA�"�A��A���A�C�A�A��RA��HA�I�A�1'A��A�E�A� �A��A�z�A�C�A�7LA�A���A�A� �A���A�hsA��yA�ZA�t�A�XA�G�A�ȴA��A���A���A�oA���A��A�1'A��A�  A��A�;dA��wA���A�v�A�=qA��A��\A��#A�VA��mA�1'A���A���A��A�/A���A�O�AC�A}�-A|�/A|I�A{hsAz�`Ay�mAyAx�!Aw��Aw\)Av��At��As�^Aq�Ap�9Ao��An�Am�FAlM�Ak�Aj��Aj^5AiAh(�Ag"�AfQ�Ae"�Ad�RAd(�Acl�Ab�\Aa�Aa�AaS�A`��A_S�A]ƨA\ĜA[�AZ�jAYt�AX�AX�DAW�AVbNAT��AR��AQ7LAPAO�AO"�ANn�AM�FAL��AK�AK�AJ�RAJQ�AIl�AH-AG�wAF�HAFn�AE��AD�RAC�#AC��AC+ABv�A@�\A??}A>�A>Q�A=�
A=VA<{A:=qA9%A8~�A7oA6��A6jA6M�A61A5G�A4�jA4ffA41'A2�A2VA1�mA0�A0E�A/ƨA/
=A.r�A-��A-�A,��A,  A*��A)ƨA(ĜA(-A'�A'ƨA'%A%��A$��A#��A#��A#K�A"=qA �9A�-A|�AA�+AQ�A1A�PA��AȴA��AM�A��A �A��A��Ax�A�yAQ�A��A��A1'A��A7LA�AbA�#AS�A�A�jAE�A;dAVA��AE�A
�+A	O�AȴAI�A �AƨA��A�7AhsA"�A�A~�A�#A��A�AA�A Z@��@��@���@�dZ@���@�V@�@�v�@�hs@���@�  @��y@�=q@�Q�@�@�$�@��@�x�@� �@�^5@�j@��@݁@�b@���@��T@�&�@�+@Ձ@�Z@��m@��@���@�/@�\)@�J@̬@��
@�;d@�/@�z�@�Z@�Z@� �@��;@ǍP@Ƨ�@š�@���@��@�@�O�@�z�@�-@���@��
@�|�@�o@�~�@�V@�ƨ@���@��+@�G�@�1@�"�@��@�@�x�@��@��`@��@�G�@��F@�C�@�5?@���@��@�Q�@�(�@�v�@�hs@���@�z�@�j@�9X@��P@���@�M�@�5?@��@��^@�V@���@���@�33@��@���@�V@�@�Q�@��H@��R@�ff@��#@��@���@��@�A�@�1'@� �@���@���@�33@�o@�@�ȴ@��+@�V@�-@���@�V@�Q�@��;@�ƨ@��@���@��P@��@�~�@��^@��`@�A�@��@���@��F@��P@�@��y@��@��\@��T@�&�@���@�1'@��
@�|�@�;d@�"�@���@��\@�^5@�=q@��@��@���@�X@��j@�z�@�bN@�A�@��;@��w@��@�\)@�o@��+@�{@��@��@��#@�V@��`@�Ĝ@��9@�z�@�I�@�b@�b@�1@��@���@��;@�z�@�bN@�9X@�1'@��@��;@���@��y@�n�@�~�@��+@�@���@���@�`B@�G�@�O�@�G�@�%@�r�@�(�@��@��@;d@~�@�@K�@~�y@|�D@{t�@z�@y7L@xĜ@xr�@w�;@w
=@v��@v$�@vv�@y7L@x�`@yX@x  @uV@u�@uO�@u`B@u�@u��@u�T@vȴ@v5?@u��@u?}@tz�@st�@r�\@r�\@r=q@q�@qx�@qG�@q7L@qG�@qG�@q&�@q&�@qX@qX@qG�@p��@p�@pb@o�;@ol�@o�w@pQ�@pĜ@qG�@q��@q��@q&�@p�`@p �@nV@o�@nff@n{@m�T@m��@l�D@k�F@j^5@i�@h�u@g�@h  @gl�@gK�@e�@f�+@g��@g�;@g;d@fȴ@f@e�@e�T@e��@e`B@e�@d�@d��@d�@b��@b�!@b��@bn�@bJ@a�^@aX@a&�@`��@`��@`Ĝ@`  @_�@^�R@^ȴ@^��@]�-@]�-@]��@]��@]/@\�j@\z�@[@Z~�@Z~�@ZM�@Y�^@X��@X��@X�u@XA�@W�@W\)@W;d@V�y@V$�@U��@Up�@Tz�@T�@Tj@TZ@Tj@T�@T�@T1@S��@T�@T(�@S��@R��@R-@Q�7@QG�@Q�@P�9@P  @P  @O�P@N�y@N��@NE�@N{@N@M�@N{@MO�@Kƨ@J�H@Jn�@J^5@Jn�@Jn�@I��@I7L@IG�@IG�@I%@H�@H �@G��@G��@GK�@G�@G
=@F�y@Fȴ@F��@Fv�@FV@E�@E�@EO�@E?}@E�@E�@D��@D��@D�@D�j@DZ@C�@C33@Co@B�H@B��@B��@B��@B^5@BM�@A��@A��@A��@AX@A&�@A&�@A&�@A�@@�`@@�u@@Q�@@A�@@A�@@A�@@ �@?�@?��@?K�@?
=@>�y@>ff@>5?@>$�@=�@=�@<I�@;�
@;��@;dZ@;S�@;S�@;o@:��@:��@:�\@:~�@9��@9�7@9x�@9hs@9hs@9G�@9&�@9�@9�@9%@8��@8��@8��@8Ĝ@8�u@8�@8�@8�@8r�@8Q�@8b@7�w@7�@6�y@6�@6ȴ@6ȴ@6�R@6��@6��@6v�@6ff@65?@5p�@4�/@4�D@4j@3�m@3��@3dZ@3@2��@2�@1�#@1��@1��@1��@1�7@1hs@1X@1�@0�9@0bN@0Q�@0b@0  @/�@/��@/�P@/\)@/+@.�y@.��@.ff@.5?@-�@-��@-��@-�@-`B@-O�@-?}@-�@,�@,�j@,�@,��@,��@,�D@,z�@,j@,I�@,I�@,9X@,1@+t�@+"�@*��@*��@*�@)��@)hs@)�@(Q�@'�@'��@'|�@'\)@&�@&��@&5?@%@%�-@%�h@$�@$�@$Z@$1@#�F@#dZ@#@"��@"�\@"=q@!��@!X@!&�@ ��@ �@ r�@ r�@ Q�@ A�@ 1'@ b@   @�;@��@�P@K�@�R@$�@@�-@��@�@`B@O�@?}@V@�j@z�@�D@�D@Z@�@�F@��@S�@o@�H@��@��@n�@=q@��@��@��@X@&�@%@Ĝ@�u@�@bN@1'@�;@�@�P@+@�@�+@V@E�@�@�-@�@?}@��@�/@��@Z@9X@��@�F@33@@@@�@��@��@~�@M�@�@��@��@x�@7L@%@Ĝ@�u@r�@A�@1'@b@�;@�w@;d@�y@�R@��@�+@V@E�@5?@�@��@@O�@�@V@�/@�j@z�@I�@I�@I�@9X@�@ƨ@t�@S�@C�@"�@o@@
��@
�\@
n�@
-@
�@	��@	x�@	hs@	G�@	7L@	&�@	&�@	�@	�@��@�`@Ĝ@��@r�@ �@�;@|�@|�@K�@;d@;d@;d@�@ȴ@��@�+@V@@�@�T@@�h@?}@��@�@�@��@��@�j1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BŢB�/B�B�B�B�B�5B�BG�BA�B2-Bw�B�VB��B��Bw�B�%B��B��B�uB�B�7B�bB�B{�BVBI�BQ�BC�B?}B1'B<jB8RB#�B��B�B�B�
B�)B��B��B�-B��B�\Bm�BW
BZBO�BO�BS�B7LBB�B{BbB	7BBBB
��B
��B
�B
�ZB
�;B
��B
ÖB
��B
��B
�VB
�bB
�PB
�=B
�B
t�B
r�B
r�B
iyB
ffB
`BB
ZB
ZB
P�B
J�B
B�B
1'B
(�B
�B
�B
�B
{B
DB
B	��B
B	��B	�B	�B	�ZB	�NB	�#B	�BB	�)B	�
B	��B	��B	��B	��B	ȴB	�^B	�3B	�B	��B	��B	��B	��B	��B	�oB	~�B	u�B	cTB	ffB	cTB	ffB	_;B	YB	R�B	J�B	L�B	G�B	G�B	E�B	>wB	5?B	8RB	-B	/B	&�B	�B	�B	�B	�B	
=B��B�B	B��B��B�B�BĜBĜB��B��B��B��B��B��BB��BĜBÖB�3B�dB�jB�3B�3B�?B�B�B�B��B��B��B��B�bB�bB��B��B��B�JB�B�B}�B�B� Bs�BjBs�B�B}�B}�B�B}�Bz�Bw�B}�B{�Bt�BjBXB`BBW
B^5BcTB[#BYBQ�BXBZBS�BQ�BS�BXBR�BP�BR�BH�B@�B;dB?}B2-B%�B6FB=qB@�BE�BC�BD�BD�BB�B>wB;dB5?B-B"�B�BDBB�B\BVB�B"�B�B\B�B�BBoB�B�B�B�B �B!�B!�B�BbBPB�BbB{B�B�B�B\BoB�B �B�B�B�B�B�B�B �B#�B�B(�B0!B1'B.B,B)�B#�B!�B �B%�B$�B+B(�B�B-B2-B8RB9XB6FB2-B33B>wB8RB5?B:^B>wB@�BG�BH�BG�BF�B=qB7LBD�BQ�BP�BR�B]/BaHB_;BYBcTBl�Bp�Bs�Br�Bo�Bp�Br�Bx�Bv�Bv�Bu�Bw�Bv�B}�B~�B~�B}�Bz�Bs�Bz�B�bB�\B�VB�oB�uB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�3B�3B�3B�-B�B�B�-B�RB�wBŢBɺBɺBɺBȴB��B��B��B��B��B�
B�)B�BB�NB�`B�sB�mB�B�B�B�B�B�B�B�B��B��B��B��B��B��B	  B	  B	B	B	JB	PB	DB		7B	uB	�B	�B	�B	�B	�B	�B	!�B	#�B	(�B	/B	;dB	6FB	6FB	7LB	5?B	6FB	5?B	2-B	2-B	9XB	;dB	:^B	=qB	B�B	D�B	H�B	L�B	L�B	L�B	N�B	VB	[#B	ZB	]/B	]/B	dZB	e`B	dZB	^5B	cTB	e`B	jB	o�B	q�B	r�B	u�B	y�B	�B	�+B	�{B	�bB	�hB	�PB	�B	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�'B	�3B	�9B	�RB	�jB	�wB	��B	ÖB	ÖB	ÖB	ÖB	��B	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�BB	�BB	�5B	�BB	�BB	�`B	�`B	�`B	�fB	�fB	�fB	�fB	�TB	�NB	�yB	�B	�yB	�yB	�B	�B	�B	�B	�B	�B	�yB	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B	��B
  B
B
  B
B
B
B
+B
	7B
%B
B
	7B

=B
VB
VB
PB
PB
bB
\B
\B
oB
oB
{B
{B
{B
uB
VB

=B
hB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
 �B
 �B
!�B
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
"�B
#�B
$�B
$�B
$�B
%�B
'�B
'�B
&�B
&�B
%�B
&�B
'�B
(�B
'�B
&�B
&�B
%�B
$�B
%�B
&�B
%�B
&�B
'�B
%�B
$�B
!�B
)�B
,B
-B
.B
.B
.B
.B
/B
/B
/B
-B
/B
2-B
33B
49B
33B
33B
49B
49B
49B
49B
49B
49B
49B
33B
49B
49B
49B
33B
33B
2-B
2-B
1'B
5?B
6FB
6FB
6FB
6FB
6FB
6FB
5?B
5?B
49B
2-B
49B
6FB
7LB
5?B
6FB
7LB
6FB
7LB
7LB
9XB
;dB
;dB
;dB
;dB
;dB
;dB
:^B
:^B
;dB
<jB
<jB
=qB
=qB
=qB
=qB
>wB
>wB
>wB
>wB
?}B
@�B
@�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
A�B
@�B
A�B
B�B
B�B
A�B
B�B
C�B
C�B
B�B
D�B
F�B
G�B
F�B
E�B
G�B
F�B
G�B
I�B
I�B
G�B
I�B
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
K�B
K�B
K�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
L�B
K�B
K�B
J�B
J�B
L�B
O�B
O�B
O�B
O�B
O�B
O�B
N�B
N�B
P�B
Q�B
Q�B
P�B
P�B
Q�B
R�B
R�B
R�B
S�B
T�B
T�B
T�B
T�B
T�B
T�B
VB
VB
W
B
W
B
W
B
XB
YB
XB
XB
W
B
YB
YB
XB
XB
YB
[#B
[#B
ZB
[#B
[#B
[#B
[#B
]/B
\)B
\)B
]/B
]/B
^5B
]/B
`BB
aHB
aHB
`BB
`BB
`BB
`BB
`BB
`BB
aHB
aHB
`BB
aHB
bNB
aHB
cTB
cTB
cTB
dZB
cTB
cTB
cTB
bNB
cTB
dZB
e`B
e`B
dZB
ffB
ffB
e`B
ffB
ffB
e`B
gmB
gmB
gmB
hsB
gmB
hsB
iyB
iyB
iyB
hsB
gmB
hsB
jB
jB
jB
jB
jB
iyB
k�B
k�B
jB
k�B
k�B
jB
m�B
m�B
n�B
n�B
n�B
n�B
n�B
m�B
m�B
m�B
m�B
m�B
l�B
m�B
m�B
p�B
o�B
p�B
p�B
p�B
o�B
o�B
o�B
p�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
r�B
r�B
r�B
s�B
s�B
t�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�@B��B��B��B��B�B�3B��B��B�&B�BM�BK)B=�B}�B�uB�OB��B~�B��B��B�B��B��B�dB��B�+B}�BZ�BMPBT�BFtBBAB3�B<�B8�B%�BB��BܬBٴB�B�&B�'B��B��B�BqABZ�B\�BR�BQhBU2B:�B�B�B�BNB
=B9B�B�B
��B
��B
�B
��B
��B
�[B
ňB
�>B
�B
�B
��B
��B
�DB
��B
vzB
s�B
s�B
jB
g8B
abB
[#B
Z�B
Q�B
K�B
C�B
3hB
*�B
!B
�B
�B
�B
�B
�B	�]B
�B	��B	�OB	�B	�B	�nB	�xB	��B	�B	�+B	��B	ԯB	҉B	уB	��B	�B	�B	��B	�6B	�fB	�/B	��B	�B	��B	�B	w�B	fLB	h
B	d�B	f�B	`B	ZB	T,B	LB	M�B	H�B	HfB	FtB	?�B	6�B	8�B	./B	/�B	(
B	�B	�B	OB	YB	�B�B�?B	�B��B��B�BٴB��B�?BЗB�-B�JB�NB�.B�^B�{B�AB�9B�B��B�6B�"B��B�B�B�;B��B�B�
B��B��B�B�B��B�EB�B�B��B��B�9BHB��B��BuZBl�BuB��B~�B~�B�oB~�B{�Bx�B~BB|PButBk�BZQBa-BYB_!Bd&B\)BZBS@BX�BZ�BT�BR�BT�BXyBS�BQ�BS�BI�BBB<�B@�B4TB(>B7�B>(BA BE�BD3BD�BD�BB�B>�B;�B5�B./B$&B 'B�B�B
BhB.B�B#�B�B�B 'B�BBuB]B�B�BkB!|B"NB"NB�B�B�BSB�BgBmBWB_B�B�BkB!HBxBkBVB�B�B�B!|B$tB/B)yB0UB1[B.cB,WB*B$�B"�B!�B&�B%�B+�B)�B�B-�B2�B8�B9�B6�B33B4B>�B9>B6FB;dB?HBA;BG�BIBH1BG+B>�B9	BE�BRTBQ�BS�B]�Ba|B_�BZQBdBl�Bp�Bs�BsBp!Bq'Bs3By	BwBwBvzBx8Bw�B~]BcBHB~]B{�BuB|B�}B��B��B��B��B��B��B��B��B�	B��B�B��B� B� B�B�B�B�hB�nB�mB�}B�MB�MB�MB�aB��B��B��B�	B��B��B��B�	B��B�7B��B�B�6B�^B�}B�sB�xB�B�B�B�B�B�B��B�B��B��B��B�B�B�B�B�0B�DB�.B�.B	 4B	 OB	oB	mB	dB	jB	xB		�B	�B	�B	�B	�B	�B	�B	�B	!�B	#�B	)*B	/B	;B	6`B	6zB	7fB	5tB	6zB	5�B	2�B	2�B	9XB	;B	:�B	=�B	B�B	D�B	H�B	L�B	L�B	M6B	O\B	V9B	[=B	ZQB	]dB	]dB	dZB	ezB	d�B	_B	c�B	e�B	j�B	o�B	q�B	r�B	vB	zB	� B	�B	��B	��B	��B	�B	�B	�TB	�yB	��B	��B	��B	��B	�~B	��B	��B	�	B	��B	��B	��B	��B	��B	�B	�,B	��B	��B	�
B	��B	�B	�B	�"B	�=B	�CB	�IB	�OB	�vB	�MB	�TB	�8B	�6B	�wB	�oB	�{B	ðB	��B	��B	��B	�B	̳B	�0B	��B	��B	��B	�JB	�VB	�VB	�<B	�4B	� B	�B	�&B	�B	�TB	��B	��B	�BB	ބB	��B	�vB	�`B	�zB	�B	�B	�B	�B	�B	�B	��B	�yB	�B	�B	��B	�B	�B	��B	��B	�B	�B	��B	�B	�B	��B	��B	��B	�B	�B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	�B	�$B	�0B	�B	�DB
  B	�B
 B
 B
 B
B
3B
3B
+B
	RB
tB
mB
	�B

rB
pB
�B
�B
�B
}B
�B
�B
�B
�B
�B
�B
{B
�B
�B

�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
 �B
 �B
!�B
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
"�B
#�B
$�B
$�B
$�B
%�B
(
B
'�B
'B
'B
%�B
'B
'�B
)B
(
B
'B
'B
%�B
%,B
&B
'B
&2B
'B
(
B
&B
%B
"NB
*0B
,"B
-CB
.B
./B
./B
./B
/5B
/5B
/OB
-]B
/5B
2-B
33B
49B
3MB
3MB
49B
4TB
49B
4TB
4TB
49B
49B
3hB
49B
49B
4TB
3hB
3MB
2GB
2GB
1vB
5ZB
6`B
6FB
6`B
6FB
6`B
6FB
5ZB
5ZB
4TB
2|B
4�B
6zB
7�B
5tB
6`B
7fB
6zB
7�B
7�B
9�B
;dB
;dB
;B
;B
;B
;�B
:xB
:xB
;B
<jB
<�B
=qB
=�B
=�B
=�B
>�B
>�B
>�B
>�B
?�B
@�B
@�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
A�B
@�B
A�B
B�B
B�B
A�B
B�B
C�B
C�B
B�B
D�B
F�B
G�B
F�B
E�B
G�B
F�B
G�B
I�B
I�B
G�B
I�B
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
K�B
K�B
K�B
M�B
M�B
NB
M�B
M�B
M�B
M�B
NB
L�B
K�B
K�B
J�B
KB
MB
O�B
O�B
O�B
PB
O�B
O�B
N�B
N�B
Q B
RB
RB
QB
QB
RB
SB
S&B
SB
T,B
UB
U2B
UB
UB
UB
U2B
VB
VB
W$B
W$B
W$B
XEB
Y1B
X+B
XEB
WYB
Y1B
Y1B
XEB
XEB
YKB
[WB
[WB
ZQB
[WB
[WB
[WB
[=B
]IB
\CB
\]B
]IB
]dB
^OB
]dB
`\B
aHB
abB
`vB
`\B
`vB
`vB
`\B
`\B
a|B
abB
`\B
a|B
bhB
abB
c�B
c�B
cnB
dtB
c�B
c�B
cnB
b�B
c�B
dtB
ezB
ezB
dtB
ffB
ffB
ezB
f�B
f�B
e�B
g�B
g�B
g�B
h�B
g�B
h�B
iyB
iyB
iyB
h�B
g�B
h�B
jB
jB
j�B
jB
j�B
i�B
k�B
k�B
j�B
k�B
k�B
j�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
m�B
m�B
m�B
m�B
m�B
l�B
m�B
m�B
p�B
o�B
p�B
p�B
p�B
o�B
o�B
o�B
p�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
r�B
r�B
r�B
s�B
s�B
t�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<5��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.06(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201810150036442018101500364420181015003644201810150200172018101502001720181015020017201810160018512018101600185120181016001851  JA  ARFMdecpA19c                                                                20181011093515  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20181011003516  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20181011003520  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20181011003520  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20181011003521  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20181011003521  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20181011003521  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20181011003521  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20181011003521  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20181011003521                      G�O�G�O�G�O�                JA  ARUP                                                                        20181011005531                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20181011153453  CV  JULD            G�O�G�O�F�?�                JM  ARCAJMQC2.0                                                                 20181014153644  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20181014153644  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20181014170017  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20181015151851  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115131517                      G�O�G�O�G�O�                