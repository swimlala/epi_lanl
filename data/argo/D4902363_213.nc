CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-02-26T00:37:40Z creation;2018-02-26T00:37:44Z conversion to V3.1;2019-12-19T07:48:33Z update;     
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
resolution        =���   axis      Z        p  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     p  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \X   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     p  `4   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     p  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     p  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �<   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     p  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     p  �d   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     p  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     p  �D   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ۴   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �D   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �D   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �D   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �D   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �$   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �(   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �8   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �<   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �@   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �DArgo profile    3.1 1.2 19500101000000  20180226003740  20200115121517  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0576_213                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�N��]L 1   @�N�����@:g�K]�d�dfu��!�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  @���A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B ffB(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D4��D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�<�Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׃3D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�C3D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߃3D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�|�D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�3D�6f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�z�@�{@��HA
=A?
=A_
=A
=A��A��A��A��AυA߅A�A��BBBB (�B'B/B7B?BGBOBWB_BgBoBwBB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HC�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RD |)D �)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D	|)D	�)D
|)D
�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D |)D �)D!|)D!�)D"|)D"�)D#|)D#�)D$|)D$�)D%|)D%�)D&|)D&�)D'|)D'�)D(|)D(�)D)|)D)�)D*|)D*�)D+|)D+�)D,|)D,�)D-|)D-�)D.|)D.�)D/|)D/�)D0|)D0�)D1|)D1�)D2|)D2�)D3|)D3�)D4|)D4��D5|)D5�)D6|)D6�)D7|)D7�)D8|)D8�)D9|)D9�)D:|)D:�)D;|)D;�)D<|)D<�)D=|)D=�)D>|)D>�)D?|)D?�)D@|)D@�)DA|)DA�)DB|)DB�)DC|)DC�)DD|)DD�)DE|)DE�)DF|)DF�)DG|)DG�)DH|)DH�)DI|)DI�)DJ|)DJ�)DK|)DK�)DL|)DL�)DM|)DM�)DN|)DN�)DO|)DO�)DP|)DP�)DQ|)DQ�)DR|)DR�)DS|)DS�)DT|)DT�)DU|)DU�)DV|)DV�)DW|)DW�)DX|)DX�)DY|)DY�)DZ|)DZ�)D[|)D[�)D\|)D\�)D]|)D]�)D^|)D^�)D_|)D_�)D`|)D`�)Da|)Da�)Db|)Db�)Dc|)Dc�)Dd|)Dd�)De|)De�)Df|)Df�)Dg|)Dg�)Dh|)Dh�)Di|)Di�)Dj|)Dj�)Dk|)Dk�)Dl|)Dl�)Dm|)Dm�)Dn|)Dn�)Do|)Do�)Dp|)Dp�)Dq|)Dq�)Dr|)Dr�)Ds|)Ds�)Dt|)Dt�)Du|)Du�)Dv|)Dv�)Dw|)Dw�)Dx|)Dx�)Dy|)Dy�)Dz|)Dz�)D{|)D{�)D||)D|�)D}|)D}�)D~|)D~�)D|)D�)D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�z�D��D��D�>D�~D��HD��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D��HD��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��HD��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D¾D��D�>D�~DþD��D�>D�~DľD��D�>D�~DžD��D�>D�~DƾD��D�>D�~DǾD��D�>D�~DȾD��D�>D�~DɾD��D�:�D�~DʾD��D�>D�~D˾D��D�>D�~D̾D��D�>D�~D;D��D�>D�~DξD��D�>D�~DϾD��D�>D�~DоD��D�>D�~DѾD��D�>D�~DҾD��D�>D�~DӾD��D�>D�~DԾD��D�>D�~DվD��D�>D�~D־D��D�>DׁHD׾D��D�>D�~DؾD��D�>D�~DپD��D�>D�~DھD��D�>D�~D۾D��D�>D�~DܾD��D�AHD�~DݾD��D�>D�~D޾D��D�>D߁HD߾D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�z�D��D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��HD�HD�4{1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��A���A���A��uA�p�A�ZA�33A��A�JA���A���A��RA��FA�x�A�7LA��A�bA�A�`BA�1A��/A��FA�z�A�Q�A�I�A�E�A�-A�$�A�{A�JA�1A�A�%A�%A���A��A��;A��A���A��A�n�A�;dA��A���A�K�A�C�A��A���A���A��PA�+A�M�A�?}A��
A�hsA��A��-A�jA�hsA�7LA�\)A�ƨA��A�jA��;A�bNA�A���A�dZA�=qA��A��A�oA��HA��A�1'A��A��A�A��
A��-A�ZA�7LA��7A��/A�?}A��RA�`BA�VA�K�A��/A�A\)A{�Ay�^Ax-Aw`BAwp�Aw33Av1At��Ar��Aqx�Ap�!Ap$�Ao��An�\AmdZAl�jAl(�Ai;dAf��Ae��Ad�RAb��A`A^~�A\�/AZ�/AY��AX��AX�AWS�AV�uAUC�ASC�ARn�AR  AQ�PAQt�AQS�AQ33AQG�AP�jAN��AM�mAL�RAK��AJ�RAJ(�AI�wAI7LAH~�AFȴAE��AD�`AD^5AC�AA%A?�A>�A>A�A=ƨA=�A<�\A;�A9��A9�A8ĜA8ZA6��A5�A4�jA4-A3��A2��A0�`A.�HA.1'A.{A-��A-�#A-��A-\)A-"�A,jA+�TA+G�A+oA*�9A*(�A)��A)�A(jA'��A't�A&��A&JA%ƨA%��A%dZA#�7A"��A"JA ��A ^5AƨA"�A~�A�AG�A�!A5?AbA�mA�^A�7A;dAv�A�A�yA�jAr�A��A?}A��AJA��A(�A�PAp�A5?At�An�A�A��A+A
9XA	�A	&�A�AbNAC�An�AƨA`BA�A�A�uAZA�wAG�A�A5?A�#A�^A�7A�A z�A �@��F@�$�@��/@�|�@���@��!@��T@���@���@�@�Ĝ@�@�r�@�@�K�@�+@�!@�^5@�7L@�A�@�t�@��@��T@�V@�K�@�v�@���@Ձ@Ӿw@�@҇+@�%@ϝ�@�`B@�z�@˕�@�V@ə�@ȼj@��@ǶF@�;d@�{@��
@���@�j@�33@���@�ff@�^5@�$�@���@�p�@��@��/@�Z@�;d@��T@�Q�@�K�@��@�b@���@�5?@�x�@��9@���@��@��@�+@�@�&�@���@���@��@�j@��@�@�x�@�/@�&�@��`@��j@���@���@�Ĝ@��@�
=@��@�@���@��@�G�@�&�@�V@���@�z�@�+@�v�@�^5@�V@�E�@�-@��@�p�@�7L@���@��@��@���@��!@�^5@�$�@�{@���@�p�@��@��D@��
@��P@�;d@��@���@��@��^@�X@�%@���@���@�b@���@���@���@�ƨ@���@��P@�|�@�t�@�S�@�"�@��@��!@�ff@���@�/@�&�@�%@���@���@�A�@��
@��@���@��@��@���@��\@�M�@��@�@���@�x�@�`B@�&�@���@�Ĝ@�z�@�Q�@�1'@�  @���@�t�@��H@���@��!@��\@��@��y@��@�;d@�K�@���@�5?@�$�@�@��T@���@�X@���@���@��j@���@�Z@�I�@�ƨ@�|�@���@�v�@�ff@�V@�M�@�n�@�@���@��D@��@�@l�@~ȴ@~E�@}�@}`B@}O�@}O�@}?}@}/@}�@|�@{�F@z�@x �@w�@w�;@w�;@w�@w��@w�P@v��@v$�@u�@u?}@u�@t�@t��@t�@tz�@t�@s��@s�
@s33@sC�@sdZ@sdZ@s"�@r�@r��@r�\@r-@q��@q&�@p��@p�@o�@o\)@o
=@o
=@o+@o|�@o�@o|�@m�@l��@lz�@lI�@l1@kƨ@k�F@kS�@j^5@j~�@j��@j�@i�^@iG�@h�@hr�@hA�@h  @g�;@g��@g��@g|�@gl�@g+@g
=@g
=@g
=@f��@f�+@f{@e��@ep�@e`B@dz�@d1@c�F@c�@cC�@co@b�@b��@b^5@a��@aG�@a&�@`��@` �@_�@_�@_�w@_�w@_�P@_+@^ff@]`B@\��@\z�@\I�@\1@[C�@Z��@Z~�@Y�#@Y�7@Y7L@X��@XĜ@X1'@W��@W�@W�P@Wl�@WK�@V�y@V��@VE�@V$�@V{@U�T@U��@UV@T�j@T��@T��@T�D@TZ@T9X@S�
@S��@S@R��@Q��@Q��@P�`@PĜ@P�9@PĜ@P�9@P�u@P�@Pr�@P �@O
=@N��@Nȴ@Nff@N@M�T@M@MO�@L��@L�j@LI�@K�F@K33@J��@J��@J�\@J~�@J^5@I��@I��@I�7@Ihs@I&�@H��@H��@H�9@H�@G�;@Gl�@F�+@E�@E�@D��@D��@DZ@C��@CS�@C33@Co@B�@B�!@B=q@A�@A��@@��@@�u@@�@@r�@@b@?��@?�P@?|�@?K�@?
=@>�@>�R@>��@>��@>V@>@=@=O�@=�@<��@<Z@<I�@;��@;ƨ@;��@;t�@;C�@:�!@:M�@9��@8��@8Q�@7�@7|�@7K�@7;d@7
=@6ȴ@6�+@6E�@6@5��@5�-@5��@5�-@5��@5�h@5O�@5V@4�@4z�@4j@4Z@4Z@49X@3��@3�
@3ƨ@3�
@3dZ@3o@2��@2-@2�@1�^@1G�@1�@1%@0�`@0��@0Q�@/�w@/�P@/\)@/
=@.�y@.�+@.$�@.$�@.@-�T@-`B@-�@,��@,�@,��@,�@,��@,�D@,�D@,j@,9X@+��@+�m@+ƨ@+C�@*�\@*M�@*-@*�@*J@)�^@)x�@)hs@)X@)7L@)&�@(��@(�9@(�9@(�@'��@'+@'�@'�@'
=@&�@&�R@&V@&@%�-@%�@$�/@$��@$j@$Z@$I�@$9X@$9X@$9X@$(�@#��@#33@"�H@"��@"��@"��@"��@"��@"�\@"^5@"^5@"M�@"-@"J@!��@!7L@!%@ ��@ �u@ bN@ 1'@ b@   @�;@�@l�@+@
=@�@ȴ@��@ff@�T@@�-@�@O�@�@�/@�j@�D@j@I�@I�@(�@1@��@�
@�F@��@�@t�@dZ@S�@"�@��@^5@J@�@��@��@�7@x�@%@��@�u@�u@�@�@r�@Q�@ �@  @�@�;@�P@\)@;d@�@�y@��@v�@ff@E�@$�@{@�@@�h@p�@O�@/@�@��@�j@j@9X@�@1@�
@��@�@C�@33@o@��@n�@J@�#@�^@x�@G�@&�@%@��@Ĝ@��@bN@1'@ �@�@�;@�@\)@�@�y@��@v�@5?@�T@��@��@9X@��@�
@�F@�F@��@��@�@t�@t�@o@
�H@
�!@
~�@
n�@
^5@
�@	�@	��@	�^@	��@	��@	��@	�7@	hs@	G�@	7L@	X@	G�@	�@��@��@Q�@Q�@A�@1'@b@�@�;@�@��@|�@�@�@ȴ@ȴ@��@�+@ff@5?@�@�h@`B@?}@�@�@V@��@��@�@�/@�/@�@�D@z�@j@j@Z@Z1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��A���A���A��uA�p�A�ZA�33A��A�JA���A���A��RA��FA�x�A�7LA��A�bA�A�`BA�1A��/A��FA�z�A�Q�A�I�A�E�A�-A�$�A�{A�JA�1A�A�%A�%A���A��A��;A��A���A��A�n�A�;dA��A���A�K�A�C�A��A���A���A��PA�+A�M�A�?}A��
A�hsA��A��-A�jA�hsA�7LA�\)A�ƨA��A�jA��;A�bNA�A���A�dZA�=qA��A��A�oA��HA��A�1'A��A��A�A��
A��-A�ZA�7LA��7A��/A�?}A��RA�`BA�VA�K�A��/A�A\)A{�Ay�^Ax-Aw`BAwp�Aw33Av1At��Ar��Aqx�Ap�!Ap$�Ao��An�\AmdZAl�jAl(�Ai;dAf��Ae��Ad�RAb��A`A^~�A\�/AZ�/AY��AX��AX�AWS�AV�uAUC�ASC�ARn�AR  AQ�PAQt�AQS�AQ33AQG�AP�jAN��AM�mAL�RAK��AJ�RAJ(�AI�wAI7LAH~�AFȴAE��AD�`AD^5AC�AA%A?�A>�A>A�A=ƨA=�A<�\A;�A9��A9�A8ĜA8ZA6��A5�A4�jA4-A3��A2��A0�`A.�HA.1'A.{A-��A-�#A-��A-\)A-"�A,jA+�TA+G�A+oA*�9A*(�A)��A)�A(jA'��A't�A&��A&JA%ƨA%��A%dZA#�7A"��A"JA ��A ^5AƨA"�A~�A�AG�A�!A5?AbA�mA�^A�7A;dAv�A�A�yA�jAr�A��A?}A��AJA��A(�A�PAp�A5?At�An�A�A��A+A
9XA	�A	&�A�AbNAC�An�AƨA`BA�A�A�uAZA�wAG�A�A5?A�#A�^A�7A�A z�A �@��F@�$�@��/@�|�@���@��!@��T@���@���@�@�Ĝ@�@�r�@�@�K�@�+@�!@�^5@�7L@�A�@�t�@��@��T@�V@�K�@�v�@���@Ձ@Ӿw@�@҇+@�%@ϝ�@�`B@�z�@˕�@�V@ə�@ȼj@��@ǶF@�;d@�{@��
@���@�j@�33@���@�ff@�^5@�$�@���@�p�@��@��/@�Z@�;d@��T@�Q�@�K�@��@�b@���@�5?@�x�@��9@���@��@��@�+@�@�&�@���@���@��@�j@��@�@�x�@�/@�&�@��`@��j@���@���@�Ĝ@��@�
=@��@�@���@��@�G�@�&�@�V@���@�z�@�+@�v�@�^5@�V@�E�@�-@��@�p�@�7L@���@��@��@���@��!@�^5@�$�@�{@���@�p�@��@��D@��
@��P@�;d@��@���@��@��^@�X@�%@���@���@�b@���@���@���@�ƨ@���@��P@�|�@�t�@�S�@�"�@��@��!@�ff@���@�/@�&�@�%@���@���@�A�@��
@��@���@��@��@���@��\@�M�@��@�@���@�x�@�`B@�&�@���@�Ĝ@�z�@�Q�@�1'@�  @���@�t�@��H@���@��!@��\@��@��y@��@�;d@�K�@���@�5?@�$�@�@��T@���@�X@���@���@��j@���@�Z@�I�@�ƨ@�|�@���@�v�@�ff@�V@�M�@�n�@�@���@��D@��@�@l�@~ȴ@~E�@}�@}`B@}O�@}O�@}?}@}/@}�@|�@{�F@z�@x �@w�@w�;@w�;@w�@w��@w�P@v��@v$�@u�@u?}@u�@t�@t��@t�@tz�@t�@s��@s�
@s33@sC�@sdZ@sdZ@s"�@r�@r��@r�\@r-@q��@q&�@p��@p�@o�@o\)@o
=@o
=@o+@o|�@o�@o|�@m�@l��@lz�@lI�@l1@kƨ@k�F@kS�@j^5@j~�@j��@j�@i�^@iG�@h�@hr�@hA�@h  @g�;@g��@g��@g|�@gl�@g+@g
=@g
=@g
=@f��@f�+@f{@e��@ep�@e`B@dz�@d1@c�F@c�@cC�@co@b�@b��@b^5@a��@aG�@a&�@`��@` �@_�@_�@_�w@_�w@_�P@_+@^ff@]`B@\��@\z�@\I�@\1@[C�@Z��@Z~�@Y�#@Y�7@Y7L@X��@XĜ@X1'@W��@W�@W�P@Wl�@WK�@V�y@V��@VE�@V$�@V{@U�T@U��@UV@T�j@T��@T��@T�D@TZ@T9X@S�
@S��@S@R��@Q��@Q��@P�`@PĜ@P�9@PĜ@P�9@P�u@P�@Pr�@P �@O
=@N��@Nȴ@Nff@N@M�T@M@MO�@L��@L�j@LI�@K�F@K33@J��@J��@J�\@J~�@J^5@I��@I��@I�7@Ihs@I&�@H��@H��@H�9@H�@G�;@Gl�@F�+@E�@E�@D��@D��@DZ@C��@CS�@C33@Co@B�@B�!@B=q@A�@A��@@��@@�u@@�@@r�@@b@?��@?�P@?|�@?K�@?
=@>�@>�R@>��@>��@>V@>@=@=O�@=�@<��@<Z@<I�@;��@;ƨ@;��@;t�@;C�@:�!@:M�@9��@8��@8Q�@7�@7|�@7K�@7;d@7
=@6ȴ@6�+@6E�@6@5��@5�-@5��@5�-@5��@5�h@5O�@5V@4�@4z�@4j@4Z@4Z@49X@3��@3�
@3ƨ@3�
@3dZ@3o@2��@2-@2�@1�^@1G�@1�@1%@0�`@0��@0Q�@/�w@/�P@/\)@/
=@.�y@.�+@.$�@.$�@.@-�T@-`B@-�@,��@,�@,��@,�@,��@,�D@,�D@,j@,9X@+��@+�m@+ƨ@+C�@*�\@*M�@*-@*�@*J@)�^@)x�@)hs@)X@)7L@)&�@(��@(�9@(�9@(�@'��@'+@'�@'�@'
=@&�@&�R@&V@&@%�-@%�@$�/@$��@$j@$Z@$I�@$9X@$9X@$9X@$(�@#��@#33@"�H@"��@"��@"��@"��@"��@"�\@"^5@"^5@"M�@"-@"J@!��@!7L@!%@ ��@ �u@ bN@ 1'@ b@   @�;@�@l�@+@
=@�@ȴ@��@ff@�T@@�-@�@O�@�@�/@�j@�D@j@I�@I�@(�@1@��@�
@�F@��@�@t�@dZ@S�@"�@��@^5@J@�@��@��@�7@x�@%@��@�u@�u@�@�@r�@Q�@ �@  @�@�;@�P@\)@;d@�@�y@��@v�@ff@E�@$�@{@�@@�h@p�@O�@/@�@��@�j@j@9X@�@1@�
@��@�@C�@33@o@��@n�@J@�#@�^@x�@G�@&�@%@��@Ĝ@��@bN@1'@ �@�@�;@�@\)@�@�y@��@v�@5?@�T@��@��@9X@��@�
@�F@�F@��@��@�@t�@t�@o@
�H@
�!@
~�@
n�@
^5@
�@	�@	��@	�^@	��@	��@	��@	�7@	hs@	G�@	7L@	X@	G�@	�@��@��@Q�@Q�@A�@1'@b@�@�;@�@��@|�@�@�@ȴ@ȴ@��@�+@ff@5?@�@�h@`B@?}@�@�@V@��@��@�@�/@�/@�@�D@z�@j@j@Z@Z1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B��B��B�B�B�B�B�B�B�yB�mB�mB�B�sB�ZB�BǮBɺBB��BŢBȴBȴB��B��B��B��B��B��B��B��B��B�B�B��B��B��B��B��BɺBB�wB�dB�B��B�B��B��B��B��B�DBbNB<jB��BÖB�XB��B��B��Bq�B�+B�BcTBF�B#�B%BB�B&�B%�B&�B&�B#�B�BVBB
�B
�)B
�B
�yB
�HB
��B
�B
�DB
��B
��B
��B
�oB
��B
�\B
x�B
W
B
>wB
�B
�B
(�B
(�B
7LB
.B
�B
\B	��B	��B
B
B
  B	�B	�`B	�HB	��B	�B	��B	��B	��B	�=B	m�B	{�B	s�B	e`B	e`B	`BB	aHB	YB	P�B	A�B	.B	?}B	D�B	E�B	N�B	O�B	N�B	M�B	F�B	2-B	6FB	.B	-B	+B	,B	1'B	$�B	 �B	
=B	\B	\B	PB��B�B�B��B��B��B�B�B�#B��B�;B�HB�B��BŢBBǮBĜB�?B��B��B�9B�wB�wB�dB�RB�9B�-B��B��B��B�B��B��B��B��B�{B��B�oB�PB�bB�{B�oB�+Bm�Bv�B}�Bv�Bx�B{�Bt�Bu�Bu�Br�Br�Bu�B{�Bz�Bx�Bu�Bp�BgmB`BBhsBm�BiyBaHBaHB[#BT�BK�BK�BD�B49B2-BA�B7LB5?B1'BB�B<jB?}BA�BB�B9XB/B1'B8RB;dB=qB>wB9XB9XB1'B33B33B0!B49B8RB49B.B)�B+B)�B�B�B�BhB\B#�B�B�B{B1B��B\BB�B&�B!�B�B�B{B{B��BB�BVBoBDB��BPB�B�BuBbBVB�B�B�B �B�B�B!�B�BhBJBbB�B#�B,B1'B33B1'B/B/B.B.B)�B"�B"�B!�B)�B&�B �B.B:^B8RB;dBC�BA�B<jB6FB7LBB�BJ�BI�BG�BF�B<jBC�BL�BQ�BS�BO�BL�BA�BF�BR�BVBW
B[#BiyBk�Bk�Bk�Bl�Bl�Bk�BffBcTBm�Bw�By�Bx�Bw�Bu�Bt�Bw�Bu�Bu�Bw�B�%B�+B�%B�1B�=B�1B�+B�1B�1B�7B�hB�uB��B��B��B��B��B��B��B��B��B��B��B�-B�dB�qB�wB�}B��B�}B��B��B��B��B�}BǮB��B��B��B��B��B��B��B��B��B��B��B�B�B�#B�;B�HB�HB�NB�NB�TB�ZB�`B�sB�B�B�B�B�B��B��B��B	%B	VB	oB	uB	oB	hB	hB	�B	�B	 �B	"�B	�B	!�B	+B	/B	-B	-B	.B	+B	-B	.B	/B	8RB	:^B	:^B	;dB	9XB	@�B	;dB	I�B	N�B	N�B	N�B	Q�B	R�B	ZB	[#B	]/B	]/B	]/B	]/B	^5B	[#B	YB	\)B	k�B	m�B	m�B	m�B	m�B	m�B	jB	k�B	n�B	t�B	v�B	v�B	w�B	y�B	y�B	y�B	}�B	~�B	}�B	�1B	�JB	�\B	�oB	�{B	�{B	��B	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�9B	�?B	�?B	�^B	�^B	�jB	�qB	�wB	�wB	��B	��B	��B	ÖB	ĜB	ĜB	ÖB	��B	ĜB	ƨB	ȴB	ɺB	ƨB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�
B	�B	�)B	�/B	�#B	�B	�B	�
B	�
B	�/B	�BB	�HB	�BB	�;B	�HB	�ZB	�TB	�fB	�sB	�sB	�yB	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B	��B	��B
  B
B
B
B
B
B
B
B
B
B
1B
	7B
	7B
1B
+B
1B
	7B
	7B
	7B
	7B

=B

=B
	7B
+B
+B
+B
	7B

=B

=B
PB
PB
JB
VB
bB
hB
hB
hB
bB
oB
oB
bB
oB
�B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
!�B
"�B
#�B
#�B
"�B
#�B
#�B
$�B
%�B
%�B
'�B
'�B
'�B
&�B
&�B
'�B
'�B
)�B
+B
+B
+B
+B
)�B
+B
,B
,B
)�B
)�B
+B
)�B
.B
-B
,B
/B
0!B
0!B
/B
-B
-B
0!B
1'B
1'B
2-B
1'B
2-B
49B
49B
33B
2-B
49B
5?B
5?B
6FB
6FB
6FB
7LB
7LB
6FB
6FB
5?B
6FB
6FB
49B
5?B
8RB
9XB
:^B
:^B
9XB
9XB
<jB
;dB
;dB
;dB
;dB
:^B
<jB
:^B
8RB
:^B
>wB
>wB
>wB
=qB
=qB
<jB
<jB
=qB
<jB
?}B
@�B
A�B
B�B
B�B
B�B
B�B
B�B
A�B
?}B
@�B
B�B
D�B
D�B
E�B
E�B
D�B
D�B
D�B
E�B
E�B
D�B
D�B
D�B
B�B
E�B
F�B
F�B
G�B
G�B
H�B
H�B
H�B
H�B
G�B
H�B
I�B
I�B
I�B
I�B
H�B
H�B
J�B
K�B
J�B
J�B
J�B
L�B
L�B
L�B
L�B
M�B
N�B
M�B
M�B
N�B
M�B
N�B
N�B
N�B
O�B
O�B
N�B
N�B
M�B
M�B
O�B
Q�B
Q�B
Q�B
Q�B
R�B
Q�B
Q�B
S�B
T�B
S�B
T�B
T�B
S�B
S�B
T�B
T�B
T�B
S�B
T�B
VB
VB
VB
VB
W
B
XB
XB
XB
XB
XB
XB
XB
YB
YB
YB
YB
YB
YB
YB
ZB
ZB
[#B
ZB
ZB
[#B
[#B
\)B
[#B
ZB
ZB
ZB
\)B
\)B
\)B
]/B
]/B
^5B
^5B
]/B
^5B
^5B
^5B
_;B
_;B
_;B
^5B
^5B
^5B
_;B
^5B
_;B
_;B
^5B
^5B
\)B
^5B
`BB
aHB
aHB
bNB
bNB
bNB
bNB
bNB
bNB
aHB
aHB
cTB
cTB
dZB
dZB
cTB
dZB
e`B
e`B
e`B
ffB
e`B
e`B
e`B
e`B
ffB
gmB
gmB
ffB
ffB
ffB
ffB
hsB
hsB
gmB
gmB
gmB
gmB
gmB
gmB
ffB
ffB
gmB
iyB
iyB
iyB
hsB
hsB
gmB
hsB
hsB
jB
jB
k�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
k�B
l�B
l�B
m�B
m�B
m�B
m�B
n�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B��B��B�B�B��B��B��B�B�B�B��B�B�*B�,B�QB�B�XB�{B�[B�B�B�7B�B��B��B�B�&B�B� B��B��B�B�B�2B�B�B� B�B�#B�-B��B��B�B��B�"B�fB�dB�/B�B��Bd�B?�B�B��B�PB��B�$B�CBv+B�B��Be�BJ=B'�B^BB \B'RB&2B'B'B$B]B�B?B
�UB
��B
��B
��B
��B
�.B
��B
��B
�B
��B
��B
�&B
��B
��B
zDB
Y�B
A�B
 �B
"NB
*B
)�B
72B
.�B
+B
 B
 iB	�B
�B
�B
 �B	�B	��B	�NB	ԕB	��B	��B	�kB	��B	��B	q'B	}�B	u�B	g�B	gB	a|B	bB	Z7B	R B	CaB	0�B	@iB	E9B	F?B	N�B	P.B	OB	N"B	G�B	4�B	7�B	/�B	.IB	,WB	,�B	1�B	%�B	!�B	�B	�B	}B	VB��B�)B�OB��B��B�zB�B�B�B��B��B��B�	B̳B�+B�BȀB�mB�B�NB��B��B��B��B��B��B��B��B��B��B��B�kB��B��B�~B�WB��B�EB�uB��B�B��B��B�BpBw�B~�Bx8By�B|�Bu�Bv�Bv�Bs�Bs�Bv`B|6B{0By$Bv+Bq[Bh�Ba�Bi*Bm�Bj0BbNBbB[�BVSBMPBMBFB6�B3�BB�B8�B7B3BC{B=�B@iBB'BCB:xB0�B2aB9$B<B=�B>�B9�B9�B2B3�B3�B1'B4�B8�B4�B.�B*�B+�B*�B�B�B�B&B�B$@BjBWBgB	�B��B�BgBB'B"NB BBeBgBMB B�B
B�B@B�B BVBIB=B�B�B�B]BxB�B!HBpBVB"BdB�B�B�B�B$�B,qB1[B3MB1vB/�B/iB.}B.cB*�B#�B$B# B*�B'�B"4B/B:�B9	B<BC�BA�B<�B72B88BB�BJ�BI�BH1BG+B=�BD3BM6BR BT,BP.BM6BB�BG�BS�BV�BW�B[�Bi�Bk�Bk�Bk�Bl�Bl�Bk�BgBdZBnBxBy�Bx�BxBvBu%BxBv`BvzBx�B�?B�zB��B��B�XB��B�zB��B��B��B��B��B��B��B��B�	B�B�B�B�4B�qB��B�B�|B��B��B��B��B��B��B��B��B��B��B�B��B��B�B�B�"B�"B�(B�B�,B�&B�NB�FB�_B�eB�qB�pB�bB�|B�hB�B�B�B��B��B�B��B��B��B�B�B�B�.B	%B	<B	TB	�B	�B	�B	�B	�B	�B	 �B	#B	 'B	"4B	+6B	/B	-CB	-]B	.IB	+�B	-]B	.}B	/�B	8RB	:xB	:xB	;B	9�B	@�B	<6B	J	B	OB	OB	O(B	R B	S[B	Z7B	[=B	]/B	]dB	]IB	]IB	^jB	[�B	Y�B	\�B	k�B	m�B	m�B	m�B	m�B	m�B	j�B	k�B	o B	t�B	v�B	v�B	xB	y�B	y�B	z*B	~B	B	~BB	�1B	�JB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�'B	�IB	� B	�B	�$B	�$B	�
B	�
B	�B	�FB	�B	�B	�UB	��B	�tB	��B	�xB	��B	��B	��B	��B	��B	��B	��B	��B	ðB	ĶB	ĜB	ðB	��B	��B	��B	��B	��B	�+B	�B	�B	�B	��B	�B	�B	�B	�B	�NB	�,B	�B	�MB	�YB	�KB	�CB	�/B	�=B	�7B	�_B	�sB	׍B	�~B	�\B	�bB	��B	ߤB	�|B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	��B	��B	�B	��B	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�B	�B	�8B
 4B	�B	�.B
 4B
AB
AB
;B
'B
-B
AB
UB
MB
SB
fB
	7B
	RB
KB
zB
fB
	lB
	RB
	RB
	lB

XB

XB
	RB
zB
zB
�B
	�B

�B

�B
jB
�B
�B
pB
}B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
!�B
"�B
#�B
#�B
"�B
$B
#�B
%B
%�B
%�B
(
B
'�B
'�B
'B
'B
($B
($B
*0B
+B
+B
+B
+B
*B
+6B
,"B
,B
*KB
*0B
+QB
*KB
./B
-CB
,WB
/5B
0;B
0;B
/5B
-CB
-CB
0UB
1AB
1AB
2GB
1[B
2aB
49B
4TB
3hB
2aB
4TB
5tB
5tB
6`B
6`B
6FB
7fB
7LB
6`B
6`B
5ZB
6`B
6`B
4�B
5tB
8lB
9�B
:xB
:xB
9rB
9rB
<jB
;B
;B
;B
;B
:xB
<jB
:�B
8�B
:�B
>�B
>wB
>�B
=�B
=�B
<�B
<�B
=�B
<�B
?�B
@�B
A�B
B�B
B�B
B�B
B�B
B�B
A�B
?�B
@�B
B�B
D�B
D�B
E�B
E�B
D�B
D�B
D�B
E�B
E�B
D�B
D�B
D�B
B�B
E�B
F�B
F�B
G�B
G�B
H�B
H�B
H�B
H�B
G�B
H�B
I�B
I�B
I�B
I�B
H�B
H�B
J�B
K�B
J�B
J�B
J�B
L�B
L�B
MB
MB
NB
N�B
NB
M�B
N�B
NB
N�B
N�B
N�B
O�B
O�B
OB
N�B
NB
N"B
O�B
Q�B
R B
RB
RB
SB
R B
R B
TB
T�B
TB
T�B
T�B
T,B
TB
T�B
T�B
UB
TB
U2B
VB
V9B
VB
VB
W$B
XB
XEB
X+B
XB
X+B
X+B
X+B
Y1B
Y1B
YKB
YKB
Y1B
YKB
Y1B
ZQB
Z7B
[=B
Z7B
Z7B
[=B
[WB
\)B
[WB
Z7B
ZkB
ZQB
\CB
\CB
\CB
]dB
]dB
^OB
^OB
]IB
^jB
^OB
^jB
_VB
_VB
_;B
^OB
^OB
^OB
_VB
^OB
_pB
_pB
^�B
^OB
\xB
^�B
`vB
abB
a|B
bhB
bhB
bNB
bhB
bNB
bhB
abB
abB
cnB
cnB
dtB
dZB
cnB
d�B
e�B
ezB
e`B
ffB
e`B
ezB
ezB
e�B
f�B
gmB
g�B
f�B
f�B
f�B
f�B
hsB
hsB
g�B
g�B
g�B
g�B
g�B
gmB
f�B
f�B
g�B
i�B
iyB
i�B
h�B
h�B
g�B
h�B
h�B
j�B
j�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
k�B
l�B
l�B
m�B
m�B
m�B
m�B
n�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.06(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201803010033202018030100332020180301003320201806221238122018062212381220180622123812201804050435072018040504350720180405043507  JA  ARFMdecpA19c                                                                20180226093528  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180226003740  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180226003742  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180226003742  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180226003743  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180226003743  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180226003743  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180226003743  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180226003744  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180226003744                      G�O�G�O�G�O�                JA  ARUP                                                                        20180226005658                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180226154511  CV  JULD            G�O�G�O�F�w�                JM  ARCAJMQC2.0                                                                 20180228153320  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180228153320  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404193507  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622033812  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115121517                      G�O�G�O�G�O�                