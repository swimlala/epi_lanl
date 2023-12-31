CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-04-14T00:35:23Z creation;2018-04-14T00:35:29Z conversion to V3.1;2019-12-19T07:44:51Z update;     
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
_FillValue                 �  I8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M    PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p@   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t(   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �H   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �0   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �P   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ݈   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20180414003523  20200115121518  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0576_229                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�Z�%[g 1   @�Z���J @:�6���dl^5?|�1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @���@���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A���A�  B ffBffB  B  B   B(  B0  B8  B@  BH  BP  BXffB_��Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dly�Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�<�D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�3D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��H@�{A
=A?
=A_
=A
=A��A��A��A��AυA�Q�A�B (�B(�BBBB'B/B7B?BGBOBX(�B_\)BgBoBwBB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HC�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC�C��RC��RC��RC��RC��RC��RC��RC�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RD |)D �)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D	|)D	�)D
|)D
�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D |)D �)D!|)D!�)D"|)D"�)D#|)D#�)D$|)D$�)D%|)D%�)D&|)D&�)D'|)D'�)D(|)D(�)D)|)D)�)D*|)D*�)D+|)D+�)D,|)D,�)D-|)D-�)D.|)D.�)D/|)D/�)D0|)D0�)D1|)D1�)D2|)D2�)D3|)D3�)D4|)D4�)D5|)D5�)D6|)D6�)D7|)D7�)D8|)D8�)D9|)D9�)D:|)D:�)D;|)D;�)D<|)D<�)D=|)D=�)D>|)D>�)D?|)D?�)D@|)D@�)DA|)DA�)DB|)DB�)DC|)DC�)DD|)DD�)DE|)DE�)DF|)DF�)DG|)DG�)DH|)DH�)DI|)DI�)DJ|)DJ�)DK|)DK�)DL|)DL�)DM|)DM�)DN|)DN�)DO|)DO�)DP|)DP�)DQ|)DQ�)DR|)DR�)DS|)DS�)DT|)DT�)DU|)DU�)DV|)DV�)DW|)DW�)DX|)DX�)DY|)DY�)DZ|)DZ�)D[|)D[�)D\|)D\�)D]|)D]�)D^|)D^�)D_|)D_�)D`|)D`�)Da|)Da�)Db|)Db�)Dc|)Dc�)Dd|)Dd�)De|)De�)Df|)Df�)Dg|)Dg�)Dh|)Dh�)Di|)Di�)Dj|)Dj�)Dk|)Dk�)Dlu�Dl�)Dm|)Dm�)Dn|)Dn�)Do|)Do�)Dp|)Dp�)Dq|)Dq�)Dr|)Dr�)Ds|)Ds�)Dt|)Dt�)Du|)Du�)Dv|)Dv�)Dw|)Dw�)Dx|)Dx�)Dy|)Dy�)Dz|)Dz�)D{|)D{�)D||)D|�)D}|)D}�)D~|)D~�)D|)D�)D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�:�D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D��HD��D��D�>D�~D��D��D�>D�~D¾D��D�>D�~DþD��D�>D�~DľD��D�>D�~DžD��D�>D�~DƾD��D�>D�~DǾD��D�>D�~DȾD��D�>D�~DɾD��D�>D�~DʾD��D�>D�~D˾D��D�>D�~D̾D��D�>D�~D;D��D�>D�~DξD��D�>D�~DϾD��D�>D�~DоD��D�>D�~DѾD��D�>D�~DҾD��D�>D�~DӾD��D�>D�~DԾD��D�>D�~DվD��D�>D�~D־D��D�:�D�~D׾D��D�>D�~DؾD��D�>D�~DپD�HD�>D�~DھD��D�>D�~D۾D��D�>D�~DܾD��D�>D�~DݾD��D�>D�~D޾D��D�>D�~D߾D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D��D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D��D��D�>D�~D��D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��HD���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�-A�/A�1'A�1'A�1'A�33A�33A�33A�5?A�5?A�5?A�5?A�7LA�7LA�7LA�9XA�9XA�9XA�;dA�;dA�;dA�;dA�=qA�?}A�?}A�?}A�?}A�A�A�A�A�=qA�?}A�7LA��TA�$�A�bA��yA�jA���A��HA�bNA���A��hA�$�A�1'A���A��hA��A�ƨA�z�A�Q�A�`BA��jA���A��+A���A��7A���A��/A���A�l�A�"�A�dZA�ƨA��/A�I�A�ƨA�$�A���A�A�A��HA�ffA�I�A���A��TA�`BA��TA���A�5?A���A��A�1'A��A~��A}
=A{��Az�DAy�-Ay%Aw+AuAt�Aq�mAp{Ao&�Ann�Al��Ak�Aj�uAiK�Ag7LAf=qAeAc7LA`ĜA_S�A^�\A]�^A\��A[XAZM�AYdZAXA�AWdZAV��AUl�ATv�ASt�AR�jAR$�AQl�AP��AO��AN��AN �AM��AL�`ALbAJ�DAI��AI&�AG�FAF�DAF=qAE��AEG�ADI�ABQ�AA�7AAS�AA/A@��A?�
A?�A>9XA<E�A;XA;
=A:�A:^5A9��A9t�A8�DA8E�A7�#A6��A5XA4�`A3��A2bNA1�A0��A0ffA/dZA/A.��A.�uA.z�A-�wA-+A-�A,^5A+|�A)�A(ĜA(n�A($�A'��A'�A%��A%C�A%A$��A#hsA!VA {A�7A��A+A�RA�+A��AbNA�wA��A5?A�7A&�A��AjA��AA�AZA;dA��A�AQ�A\)A�A��A~�A�Al�A33A�/AE�AAhsA
�A	p�A	�A�\A�AA�A%AZA�TAx�AoA�HAA v�@�l�@�-@�V@���@��u@�j@�b@���@���@���@�t�@�O�@��
@�p�@�bN@�ƨ@�j@�hs@��@�%@�Ĝ@�w@�;d@�=q@�/@��;@�V@�Z@ߕ�@��y@���@�v�@�(�@���@��@�;d@ָR@�-@ղ-@�X@ԃ@���@�@Ь@Ѓ@��@�@̼j@���@�S�@ʟ�@�5?@���@�X@���@�|�@�ff@�%@�9X@�1@�  @��m@�|�@�"�@�ff@���@��m@���@�33@��y@�-@�X@��@�&�@��;@�33@���@�`B@��@�I�@��w@�dZ@�"�@�ȴ@�ff@��@�O�@��D@���@�
=@���@�Ĝ@� �@��m@�ƨ@���@��@��+@���@�dZ@��^@�Z@���@�C�@�33@�"�@�o@��H@��R@��+@�=q@��@���@�G�@��/@�j@�  @���@���@��@�hs@�  @��!@�E�@�J@���@��@�p�@�hs@�O�@���@��@�b@�o@���@�=q@�$�@�J@�J@���@��7@�x�@�&�@�z�@�Q�@��@��
@�|�@�"�@��@�ȴ@�ff@��@�`B@�Ĝ@�1@��@�l�@�\)@�\)@�\)@�\)@�K�@�33@���@�E�@���@�1'@��@�1@��m@��;@��m@���@���@�  @��@��P@�;d@���@���@�-@��@�r�@�Z@�Q�@�I�@� �@�  @���@��m@��
@���@��w@��P@�S�@�
=@��R@��\@�~�@�^5@�=q@�{@���@��@���@��-@��7@�hs@�?}@�%@���@�z�@�bN@�9X@�(�@� �@��@�|�@��@��y@�ff@�V@�E�@��#@�O�@��@�1'@�@;d@~V@}��@}�-@}��@}@}��@}�h@}`B@}V@|�/@|��@|j@|9X@{S�@z��@z�\@z-@y�^@yhs@y&�@y%@x�u@w��@w�@vȴ@v��@vff@v5?@u@u/@t��@t��@t�@sC�@r~�@r-@q�^@qhs@qX@q7L@q�@pr�@p1'@pQ�@oK�@n{@m?}@m/@m/@l�@l�D@l�@k��@kƨ@kdZ@j�H@j��@j��@jM�@j�@i��@i��@i�^@i��@ix�@iX@i%@h�9@h �@gl�@f��@fff@fE�@eV@d9X@c��@cƨ@c��@c�@cC�@bM�@a��@a�@a�#@a�@a��@a�^@`��@`1'@_�P@_l�@_K�@_
=@^�R@^�+@^5?@]��@]�h@]�@\��@\I�@[�
@[dZ@[33@Z�@Z�!@ZM�@Y�#@Yx�@Y�@XĜ@X1'@W��@W�@W��@Wl�@W\)@WK�@V��@VE�@V@Up�@U/@UV@T��@T�@T��@T(�@S��@S�m@S�F@R�H@RM�@R-@Q��@Q��@QX@Q7L@Q%@PĜ@P�@P  @O|�@O�@O�@O
=@N�y@N��@N�+@Nff@M�T@Mp�@MV@Lj@L(�@L�@L1@K�m@K��@KS�@J�\@I�#@I�^@I��@IX@I%@H��@HQ�@G�;@G|�@F�@F�+@F@Ep�@E`B@E?}@D�@D��@D�@D�@D��@D�D@D�D@DI�@D9X@D1@C�
@C��@B��@Bn�@B-@A��@Ahs@A&�@@��@@Ĝ@@Q�@?��@?\)@>��@>E�@>@=�@=�T@=��@=��@=��@=`B@<�/@<�D@<�D@<�D@<z�@<z�@<z�@<Z@<9X@<1@;��@;dZ@;S�@;C�@;C�@;C�@;33@;"�@;@:��@:~�@:-@9��@9x�@9hs@9G�@97L@9&�@9%@8�u@8Q�@7�;@7+@6�y@6�R@6��@6��@6v�@5�T@5?}@4��@4�/@4��@4��@4��@4�@4j@4j@4Z@4�@3�
@3��@3�@3t�@3C�@3@2�@2�H@2�!@2=q@2-@1��@1��@1�^@1��@1x�@1&�@0Q�@/�@/��@/�P@/K�@.��@.ȴ@.�R@.��@.��@.v�@.v�@.v�@.ff@.E�@-�T@-�-@-p�@,��@,�@,Z@+��@+dZ@+33@+"�@+o@*�@*��@*n�@*=q@*J@*J@)�@)��@)��@)��@)x�@)G�@)7L@)%@(�`@(��@(��@(��@(Ĝ@(bN@( �@'�@'��@'�w@'��@'K�@&�+@&V@&V@&5?@&@%�-@%�@%�@%p�@%O�@%/@$��@$�/@$�j@$��@$z�@$j@$I�@#��@#�F@#dZ@"�\@!��@!x�@!7L@!%@ ��@ ��@ ��@ r�@  �@   @�;@K�@�y@�@��@v�@E�@@�T@��@�@�@`B@?}@/@�@z�@I�@��@�@t�@dZ@S�@@��@^5@M�@=q@-@-@�@J@��@��@X@�@�`@�`@��@�9@�@A�@�w@\)@
=@��@
=@��@�y@�R@�+@ff@5?@5?@$�@�T@@��@V@z�@(�@��@�
@��@�@33@�H@��@�\@^5@M�@-@�@��@�^@�7@�@�u@A�@ �@�@�w@l�@�@��@{@@@�-@�-@��@p�@O�@O�@?}@�@�j@I�@�@ƨ@�F@��@33@
��@
��@
^5@
-@
�@
J@
J@	��@
J@
J@	��@	�@	��@	�7@	X@	7L@��@�@Q�@A�@1'@1'@ �@ �@ �@ �@b@��@�P@l�@K�@�@�R@�+@v�@V@E�@$�@@�@��@�@?}@V@��@�j@�j@�@��@j@I�@�@�m@ƨ@t�@�@��@n�@M�@=q@-@�@�#@��@��@x�@x�@X@7L@%@ �9@ �@ bN@ bN@ bN111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�-A�/A�1'A�1'A�1'A�33A�33A�33A�5?A�5?A�5?A�5?A�7LA�7LA�7LA�9XA�9XA�9XA�;dA�;dA�;dA�;dA�=qA�?}A�?}A�?}A�?}A�A�A�A�A�=qA�?}A�7LA��TA�$�A�bA��yA�jA���A��HA�bNA���A��hA�$�A�1'A���A��hA��A�ƨA�z�A�Q�A�`BA��jA���A��+A���A��7A���A��/A���A�l�A�"�A�dZA�ƨA��/A�I�A�ƨA�$�A���A�A�A��HA�ffA�I�A���A��TA�`BA��TA���A�5?A���A��A�1'A��A~��A}
=A{��Az�DAy�-Ay%Aw+AuAt�Aq�mAp{Ao&�Ann�Al��Ak�Aj�uAiK�Ag7LAf=qAeAc7LA`ĜA_S�A^�\A]�^A\��A[XAZM�AYdZAXA�AWdZAV��AUl�ATv�ASt�AR�jAR$�AQl�AP��AO��AN��AN �AM��AL�`ALbAJ�DAI��AI&�AG�FAF�DAF=qAE��AEG�ADI�ABQ�AA�7AAS�AA/A@��A?�
A?�A>9XA<E�A;XA;
=A:�A:^5A9��A9t�A8�DA8E�A7�#A6��A5XA4�`A3��A2bNA1�A0��A0ffA/dZA/A.��A.�uA.z�A-�wA-+A-�A,^5A+|�A)�A(ĜA(n�A($�A'��A'�A%��A%C�A%A$��A#hsA!VA {A�7A��A+A�RA�+A��AbNA�wA��A5?A�7A&�A��AjA��AA�AZA;dA��A�AQ�A\)A�A��A~�A�Al�A33A�/AE�AAhsA
�A	p�A	�A�\A�AA�A%AZA�TAx�AoA�HAA v�@�l�@�-@�V@���@��u@�j@�b@���@���@���@�t�@�O�@��
@�p�@�bN@�ƨ@�j@�hs@��@�%@�Ĝ@�w@�;d@�=q@�/@��;@�V@�Z@ߕ�@��y@���@�v�@�(�@���@��@�;d@ָR@�-@ղ-@�X@ԃ@���@�@Ь@Ѓ@��@�@̼j@���@�S�@ʟ�@�5?@���@�X@���@�|�@�ff@�%@�9X@�1@�  @��m@�|�@�"�@�ff@���@��m@���@�33@��y@�-@�X@��@�&�@��;@�33@���@�`B@��@�I�@��w@�dZ@�"�@�ȴ@�ff@��@�O�@��D@���@�
=@���@�Ĝ@� �@��m@�ƨ@���@��@��+@���@�dZ@��^@�Z@���@�C�@�33@�"�@�o@��H@��R@��+@�=q@��@���@�G�@��/@�j@�  @���@���@��@�hs@�  @��!@�E�@�J@���@��@�p�@�hs@�O�@���@��@�b@�o@���@�=q@�$�@�J@�J@���@��7@�x�@�&�@�z�@�Q�@��@��
@�|�@�"�@��@�ȴ@�ff@��@�`B@�Ĝ@�1@��@�l�@�\)@�\)@�\)@�\)@�K�@�33@���@�E�@���@�1'@��@�1@��m@��;@��m@���@���@�  @��@��P@�;d@���@���@�-@��@�r�@�Z@�Q�@�I�@� �@�  @���@��m@��
@���@��w@��P@�S�@�
=@��R@��\@�~�@�^5@�=q@�{@���@��@���@��-@��7@�hs@�?}@�%@���@�z�@�bN@�9X@�(�@� �@��@�|�@��@��y@�ff@�V@�E�@��#@�O�@��@�1'@�@;d@~V@}��@}�-@}��@}@}��@}�h@}`B@}V@|�/@|��@|j@|9X@{S�@z��@z�\@z-@y�^@yhs@y&�@y%@x�u@w��@w�@vȴ@v��@vff@v5?@u@u/@t��@t��@t�@sC�@r~�@r-@q�^@qhs@qX@q7L@q�@pr�@p1'@pQ�@oK�@n{@m?}@m/@m/@l�@l�D@l�@k��@kƨ@kdZ@j�H@j��@j��@jM�@j�@i��@i��@i�^@i��@ix�@iX@i%@h�9@h �@gl�@f��@fff@fE�@eV@d9X@c��@cƨ@c��@c�@cC�@bM�@a��@a�@a�#@a�@a��@a�^@`��@`1'@_�P@_l�@_K�@_
=@^�R@^�+@^5?@]��@]�h@]�@\��@\I�@[�
@[dZ@[33@Z�@Z�!@ZM�@Y�#@Yx�@Y�@XĜ@X1'@W��@W�@W��@Wl�@W\)@WK�@V��@VE�@V@Up�@U/@UV@T��@T�@T��@T(�@S��@S�m@S�F@R�H@RM�@R-@Q��@Q��@QX@Q7L@Q%@PĜ@P�@P  @O|�@O�@O�@O
=@N�y@N��@N�+@Nff@M�T@Mp�@MV@Lj@L(�@L�@L1@K�m@K��@KS�@J�\@I�#@I�^@I��@IX@I%@H��@HQ�@G�;@G|�@F�@F�+@F@Ep�@E`B@E?}@D�@D��@D�@D�@D��@D�D@D�D@DI�@D9X@D1@C�
@C��@B��@Bn�@B-@A��@Ahs@A&�@@��@@Ĝ@@Q�@?��@?\)@>��@>E�@>@=�@=�T@=��@=��@=��@=`B@<�/@<�D@<�D@<�D@<z�@<z�@<z�@<Z@<9X@<1@;��@;dZ@;S�@;C�@;C�@;C�@;33@;"�@;@:��@:~�@:-@9��@9x�@9hs@9G�@97L@9&�@9%@8�u@8Q�@7�;@7+@6�y@6�R@6��@6��@6v�@5�T@5?}@4��@4�/@4��@4��@4��@4�@4j@4j@4Z@4�@3�
@3��@3�@3t�@3C�@3@2�@2�H@2�!@2=q@2-@1��@1��@1�^@1��@1x�@1&�@0Q�@/�@/��@/�P@/K�@.��@.ȴ@.�R@.��@.��@.v�@.v�@.v�@.ff@.E�@-�T@-�-@-p�@,��@,�@,Z@+��@+dZ@+33@+"�@+o@*�@*��@*n�@*=q@*J@*J@)�@)��@)��@)��@)x�@)G�@)7L@)%@(�`@(��@(��@(��@(Ĝ@(bN@( �@'�@'��@'�w@'��@'K�@&�+@&V@&V@&5?@&@%�-@%�@%�@%p�@%O�@%/@$��@$�/@$�j@$��@$z�@$j@$I�@#��@#�F@#dZ@"�\@!��@!x�@!7L@!%@ ��@ ��@ ��@ r�@  �@   @�;@K�@�y@�@��@v�@E�@@�T@��@�@�@`B@?}@/@�@z�@I�@��@�@t�@dZ@S�@@��@^5@M�@=q@-@-@�@J@��@��@X@�@�`@�`@��@�9@�@A�@�w@\)@
=@��@
=@��@�y@�R@�+@ff@5?@5?@$�@�T@@��@V@z�@(�@��@�
@��@�@33@�H@��@�\@^5@M�@-@�@��@�^@�7@�@�u@A�@ �@�@�w@l�@�@��@{@@@�-@�-@��@p�@O�@O�@?}@�@�j@I�@�@ƨ@�F@��@33@
��@
��@
^5@
-@
�@
J@
J@	��@
J@
J@	��@	�@	��@	�7@	X@	7L@��@�@Q�@A�@1'@1'@ �@ �@ �@ �@b@��@�P@l�@K�@�@�R@�+@v�@V@E�@$�@@�@��@�@?}@V@��@�j@�j@�@��@j@I�@�@�m@ƨ@t�@�@��@n�@M�@=q@-@�@�#@��@��@x�@x�@X@7L@%@ �9@ �@ bN@ bN@ bN111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�B�B��B��B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��BɺB�dB��B�+Bq�B]/Be`BbNBaHB\)BR�BE�B49B0!B1'B�BDB�5BǮB�1B�\B�}B�RB��B�JBbNB!�B �B	7B.B �B�B
��BBB
��B
��B
��B
�B
�B
�B
�NB
��B
ŢB
�dB
��B
��B
��B
}�B
hsB
s�B
l�B
S�B
S�B
M�B
H�B
?}B
.B
�B
�B
1B
B
%B
B	��B	�B	�B	�TB	��B	��B	��B	�LB	��B	��B	��B	��B	��B	�B	�B	}�B	w�B	r�B	n�B	bNB	aHB	[#B	]/B	YB	XB	N�B	I�B	D�B	G�B	H�B	>wB	7LB	(�B	)�B	$�B	�B	uB	�B	�B	hB	B��B	B	
=B	1B	B��B��B�yB�BB�ZB�B�B�mB�NB�HB�B�5B��BƨB�3BƨB�dB�B�jB�'B�qB�B�^B�dB�dB�RB�B��B�B��B��B�+B�DB��B��B�uB�PB�B�B�7B~�Bk�BM�BgmBq�Be`B]/Bl�BjB]/BO�B^5BXBYB[#B_;B_;BYBR�BR�BVBN�B>wB1'BD�BF�BD�BL�BO�BO�BJ�BC�BJ�BF�B?}B@�B9XB(�B49B8RB1'B"�B�B!�B+B0!B/B,B+B�BuB#�B)�B,B49B33B2-B/B-B&�B �B�B�B�B�B�B�B	7BDB&�B'�B#�B�B�B�BuBDBhB	7B�B�B!�B�BbB{B�B�B"�B#�B"�B"�B�B�B�B�B$�B �B�B�B$�B%�B&�B(�B(�B'�B%�B �B"�B$�B,B2-B49B2-B/B.B+B'�B-B49B33B33B/B-B'�B �B2-B9XB:^B>wBD�BA�BG�BI�BO�BS�BS�BS�BS�BR�BQ�BT�BP�BZB]/BbNBbNB`BB]/BT�BO�BT�BZBaHBk�Bq�Bt�Bt�Bt�Br�Bs�Br�Br�Br�Br�Br�Bq�Br�Bs�Bs�Bq�Bq�Bv�Bs�Bw�B�%B�7B�7B�=B�PB�PB�JB�7B�=B�7B�=B�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B��B��B�B�B�'B�XB�jB�}B��B��B��B��B��B�}BĜBǮBŢB�
B�B�B�/B�HB�NB�TB�TB�NB�HB�`B�yB�yB�B�B��B	B	B	B	B	B	B	B	B	B	B	B	+B		7B	JB	oB	{B	{B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	!�B	"�B	&�B	(�B	(�B	+B	+B	(�B	%�B	)�B	1'B	33B	:^B	:^B	8RB	9XB	>wB	;dB	?}B	C�B	C�B	G�B	L�B	P�B	T�B	VB	XB	\)B	bNB	dZB	ffB	gmB	gmB	gmB	m�B	q�B	s�B	u�B	y�B	}�B	� B	�B	~�B	�B	�%B	�=B	�JB	�JB	�JB	�VB	�hB	�uB	�oB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�-B	�'B	�3B	�-B	�-B	�FB	�LB	�XB	�jB	�jB	�}B	��B	B	B	B	ÖB	ĜB	ÖB	ĜB	ĜB	ǮB	ȴB	ĜB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	�B	�B	�)B	�;B	�BB	�;B	�;B	�BB	�BB	�HB	�NB	�TB	�HB	�NB	�ZB	�`B	�mB	�mB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
  B
  B
B
B
  B
  B
B
B
B
%B
%B
B
B
B
B
%B

=B

=B
	7B
	7B

=B

=B
	7B

=B

=B
PB
PB
PB
oB
hB
hB
oB
uB
uB
uB
uB
uB
oB
uB
uB
oB
hB
bB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
 �B
 �B
 �B
 �B
!�B
"�B
#�B
#�B
#�B
#�B
"�B
"�B
"�B
"�B
"�B
"�B
#�B
%�B
&�B
&�B
&�B
%�B
%�B
%�B
%�B
%�B
(�B
)�B
)�B
)�B
)�B
(�B
(�B
,B
-B
.B
.B
-B
-B
-B
.B
.B
-B
.B
.B
/B
/B
/B
.B
0!B
0!B
/B
/B
1'B
1'B
1'B
1'B
1'B
1'B
0!B
.B
2-B
33B
33B
49B
49B
5?B
6FB
6FB
6FB
6FB
7LB
7LB
6FB
5?B
5?B
5?B
6FB
5?B
7LB
5?B
5?B
9XB
:^B
;dB
;dB
:^B
:^B
:^B
;dB
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
>wB
>wB
>wB
>wB
>wB
=qB
>wB
>wB
?}B
?}B
?}B
>wB
=qB
@�B
A�B
A�B
A�B
A�B
B�B
C�B
C�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
B�B
B�B
B�B
A�B
B�B
E�B
F�B
F�B
H�B
G�B
G�B
G�B
G�B
H�B
H�B
F�B
H�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
K�B
K�B
L�B
M�B
M�B
O�B
O�B
O�B
N�B
M�B
O�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
O�B
O�B
P�B
Q�B
R�B
R�B
Q�B
P�B
Q�B
P�B
Q�B
R�B
T�B
VB
VB
VB
T�B
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
VB
W
B
YB
YB
YB
YB
YB
YB
ZB
ZB
[#B
[#B
[#B
\)B
[#B
[#B
[#B
ZB
ZB
\)B
]/B
^5B
^5B
]/B
]/B
^5B
^5B
`BB
aHB
aHB
aHB
aHB
aHB
aHB
bNB
aHB
aHB
`BB
`BB
cTB
cTB
dZB
dZB
cTB
cTB
e`B
dZB
ffB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
ffB
ffB
ffB
ffB
ffB
hsB
iyB
iyB
jB
jB
jB
jB
jB
jB
iyB
iyB
jB
jB
jB
jB
k�B
l�B
l�B
m�B
m�B
n�B
n�B
m�B
m�B
m�B
m�B
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
n�B
m�B
m�B
n�B
o�B
o�B
p�B
o�B
o�B
p�B
p�B
p�B
q�B
p�B
p�B
p�B
p�B
q�B
r�B
r�B
r�B
s�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�B�B��B��B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B�B� B�#B��G�O�B��Bt�B`vBg8Bc�BbhB]/BS�BF�B6+B1B1�B!BPG�O�B�B��B�:B��B�$G�O�B�BG�O�B&�B%zB�B/B"�BB 4BmB�B �B
��B
��B
�B
�B
�AB
��B
өB
�B
��B
��B
�ZB
��G�O�B
k�B
t�B
m�B
VB
U�B
O(B
I�B
@�B
0UB
�B
�B

�B
-B
EB
AB	��B	�oB	�B	�B	�PB	�NB	̘B	��B	��B	��B	�B	��B	��B	�+B	�gB	.B	yXB	s�B	o�B	c�B	b�B	\�B	^B	ZB	YB	PB	J�B	E�B	H�B	IlB	?�B	8�B	*�B	+B	%�B	eB	�B	VB	kB	TB	�B�$B	�B	
�B	�B	�B��B��B�B�B�`B��B��B�$B�B�4B�KBޞB��B�1B�?B�EB��B�B��B��B��B��B��B��B��B��B�/B��B��G�O�B�
B�7B��B�B�)B�FB�<B�{B�3B��B�4G�O�BQ Bh�Br|Bf�B_!BmBkB^�BQ�B_BYeBZ7B\)B_�B_�BY�BT,BS�BV�BO�B@OB2�BE�BG�BE�BMjBPHBPHBKxBD�BKDBGEB@iBA;B:xB*�B5B9	B2-B$@B�B#:B+�B0�B/�B,�B+�B!B2B$�B*�B,�B4nB3�B2|B/�B-�B'�B"B �BB�BB�B�G�O�B�B'B($B$@B�B)B9B,B~B�B
�BB]B"BOBB�B�B�B#TB$ZB#nB#nB�B�B�BpB%FB!|B+B�B%zB&fB'mB)_B)_B(�B&�B!�B#�B%�B,�B2aB4TB2aB/�B.�B+�B)B-�B4�B3�B3�B/�B-�B)*B"�B3B9�B;0B>�BEBBABHBJ#BP.BTFBT{BT�BT�BS�BR�BU�BQ�BZ�B]�Bb�Bb�B`�B]~BU�BQBVB[qBbNBl"Bq�Bt�Bt�Bt�Br�BtBr�Br�Br�BsBr�BrBs3BtBt9Br|BraBwfBt�Bx�B�YB�lB��B��B�jB�jB��B��B��B��B�B��B��B��B��B��B��B�B��B�B�B� B� B�@B�,B�2B�>B�6B�eB�eB��B��B��B��B��B��B��B��B��B��B��B�B�B�1B��B�
B�KB�7B�IB�HB�NB�nB�B�B�B�B��B��B�"B�qB�FB	 B	'B	AB	;B	'B	3B	3B	3B	SB	SB	mB	_B		�B	~B	�B	�B	�B	�B	�B	�B	�B	�B	�B	B	�B	 �B	!�B	#:B	'B	)B	)*B	+6B	+6B	)*B	&�B	*eB	1[B	3�B	:^B	:�B	8�B	9�B	>�B	<B	?�B	C�B	DB	G�B	L�B	Q B	T�B	V9B	XEB	\CB	bhB	d�B	f�B	g�B	g�B	g�B	m�B	q�B	s�B	vB	zB	~B	�B	�UB	cB	�uB	�?B	�rB	�~B	�~B	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�,B	�$B	�DG�O�B	�LB	�>B	�B	�)B	�]B	�;B	�aB	�AB	�MB	�|B	�aB	�FB	�fB	��B	��B	��B	��B	��B	ªB	ªB	ªB	��B	��B	��B	��B	��B	��B	�G�O�B	�B	��B	��B	�B	��B	�B	�B	� B	��B	��B	�B	�9B	�MB	�SB	�eB	�]B	�VB	�\B	�VB	�VB	�\B	�vB	�|B	�hB	�G�O�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	�B	��B	��B	�B	�B	�B	�B	�B	��B	� B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�BB	�B
 B
 B
 4B
 B
 B
 B
 OB
 OB
AB
AB
SB
%B
?B
9B
9B
mB
�B
tB

XB

rB
	RB
	lB

rB

XB
	lB

�B

rB
jB
�B
�B
�B
�B
�B
�B
�B
uB
�B
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
 �B
 �B
 �B
 �B
!�B
"�B
#�B
#�B
#�B
#�B
"�B
#B
#B
#B
"�B
# B
$B
%�B
'B
'B
'B
&B
&B
%�B
&2B
&LB
)*B
*B
*B
*B
*B
)*B
)DB
,"B
-CB
.B
.B
-)B
-)B
-CB
.B
.IB
-)B
.IB
./B
/5B
/5B
/OB
./B
0!B
0UB
/5B
/OB
1AB
1AB
1AB
1AB
1AB
1[B
0UG�O�B
2GB
3MB
3MB
4TB
4nB
5tB
6FB
6FB
6FB
6zB
7fB
7LB
6`B
5ZB
5ZB
5ZB
6`B
5tB
7fG�O�B
5�B
9�B
:xB
;dB
;B
:xB
:xB
:xB
;B
<�B
<jB
<�B
<�B
<�B
=qB
=�B
=�B
=�B
=�B
>wB
>wB
>�B
>wB
>�B
=�B
>�B
>�B
?�B
?�B
?�B
>�B
=�B
@�B
A�B
A�B
A�B
A�B
B�B
C�B
C�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
B�B
B�B
B�B
A�B
B�B
E�B
F�B
F�B
H�B
G�B
G�B
G�B
G�B
H�B
H�G�O�B
H�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
L�B
L�B
MB
L�B
L�B
K�B
LB
L�B
M�B
NB
O�B
O�B
O�B
OB
NB
O�B
Q B
P�B
Q B
P�B
P�B
QB
Q B
P.B
O�B
Q B
RB
SB
SB
R B
QB
R B
QB
RB
S&B
T�B
VB
VB
VB
U2B
UB
VB
V9B
W
B
W$B
VB
V9B
VB
UMB
VSB
W?B
YKB
YKB
Y1B
Y1B
Y1B
YKB
Z7B
ZQB
[=B
[#B
[WB
\CB
[WB
[WB
[WB
ZkB
ZkB
\]B
]IB
^OB
^OB
]dB
]~B
^jB
^�B
`vB
aHB
aHB
aHB
abB
abB
abB
bNB
abB
a|B
`vB
`vB
c�B
c�B
dZB
dtB
c�B
cnB
e`B
d�B
f�B
gmB
gmB
gmB
gmB
gmB
gmB
gmB
g�B
g�B
f�B
f�B
f�B
f�B
f�B
h�B
iyB
i�B
jB
jB
j�B
jB
jB
j�B
i�B
i�B
j�B
j�B
j�B
j�B
k�B
l�B
l�B
m�B
m�B
n�B
n�B
m�B
m�B
m�B
m�B
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
n�B
m�B
m�B
n�B
o�B
o�B
p�B
o�B
o�B
p�B
p�B
p�B
q�B
p�B
p�B
p�B
p�B
q�B
r�B
r�B
r�B
s�111111111111111111111111111111114111111111111114111114141111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111141111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111411111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111411111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�<#�
<#�
<#�
<#�
<#�
G�O�<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.06(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201804180033422018041800334220180418003342201806221240272018062212402720180622124027201804271405522018042714055220180427140552  JA  ARFMdecpA19c                                                                20180414093517  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180414003523  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180414003526  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180414003526  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180414003527  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180414003527  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180414003527  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180414003527  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180414003529  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180414003529                      G�O�G�O�G�O�                JA  ARUP                                                                        20180414005634                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180414153455  CV  JULD            G�O�G�O�F�ת                JM  ARSQJMQC2.0                                                                 20180416000000  CF  PSAL_ADJUSTED_QCB�  D�� G�O�                JM  ARCAJMQC2.0                                                                 20180417153342  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180417153342  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180427050552  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622034027  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115121518                      G�O�G�O�G�O�                