CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       ~2018-11-16T00:36:31Z creation;2018-11-16T00:36:36Z conversion to V3.1;2019-12-19T07:24:05Z update;2021-11-12T04:16:43Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7(   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  70   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7p   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     84   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8T   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8t   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8x   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
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
_FillValue                 �  I`   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ML   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tt   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �D   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �0   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ΀   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  `  �(   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ވ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �,   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �<   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �@   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �P   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �T   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �X   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �\Argo profile    3.1 1.2 195001010000002420181116003631  20211116021501  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL              .A   JA  I2_0577_302                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @ؐ󴷏�1   @ؐ��`�@4w�@���du��U�=1ry GPS     A avBragBd [Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @��@�  @�  A   A   A@  A`  A�  A�  A���A�  A�  A�  A�  A�  B ffBffB��B��B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C�fC  C  C  C   C"  C$  C&  C(  C*  C,�C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\�C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�3D�@ Dʀ D�� D���D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
=@@|(�@�{@�{A
=A?
=A_
=A
=A��A�Q�A��A��AυA߅A�B (�B(�B\)B\)BB'B/B7B?BGBOBWB_BgBoBwBB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HC�C�C�C�C	�C�C�C�C�C�C�C�
C�C�C�C�C!�C#�C%�C'�C)�C,
=C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C\
=C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RD |)D �)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D	|)D	�)D
|)D
�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D |)D �)D!|)D!�)D"|)D"�)D#|)D#�)D$|)D$�)D%|)D%�)D&|)D&�)D'|)D'�)D(|)D(�)D)|)D)�)D*|)D*�)D+|)D+�)D,|)D,�)D-|)D-�)D.|)D.�)D/|)D/�)D0|)D0�)D1|)D1�)D2|)D2�)D3|)D3�)D4|)D4�)D5|)D5�)D6|)D6�)D7|)D7�)D8|)D8�)D9|)D9�)D:|)D:�)D;|)D;�)D<|)D<�)D=|)D=�)D>|)D>�)D?|)D?�)D@|)D@�)DA|)DA�)DB|)DB�)DC|)DC�)DD|)DD�)DE|)DE�)DF|)DF�)DG|)DG�)DH|)DH�)DI|)DI�)DJ|)DJ�)DK|)DK�)DL|)DL�)DM|)DM�)DN|)DN�)DO|)DO�)DP|)DP�)DQ|)DQ�)DR|)DR�)DS|)DS�)DT|)DT�)DU|)DU�)DV|)DV�)DW|)DW�)DX|)DX�)DY|)DY�)DZ|)DZ�)D[|)D[�)D\|)D\�)D]|)D]�)D^|)D^�)D_|)D_�)D`|)D`�)Da|)Da�)Db|)Db�)Dc|)Dc�)Dd|)Dd�)De|)De�)Df|)Df�)Dg|)Dg�)Dh|)Dh�)Di|)Di�)Dj|)Dj�)Dk|)Dk�)Dl|)Dl�)Dm|)Dm�)Dn|)Dn�)Do|)Do�)Dp|)Dp�)Dq|)Dq�)Dr|)Dr�)Ds|)Ds�)Dt|)Dt�)Du|)Du�)Dv|)Dv�)Dw|)Dw�)Dx|)Dx�)Dy|)Dy�)Dz|)Dz�)D{|)D{�)D||)D|�)D}|)D}�)D~|)D~�)D|)D�)D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D���D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��HD��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D¾D��D�>D�~DþD��D�>D�~DľD��D�>D�~DžD��D�>D�~DƾD��D�>D�~DǾD��D�>D�~DȾD��D�>D�~DɾD�HD�>D�~DʾD���D�>D�~D˾D��D�>D�~D̾D��D�>D�~D;D��D�>D�~DξD��D�>D�~DϾD��D�>D�~DоD��D�>D�~DѾD��D�>D�~DҾD��D�>D�~DӾD��D�>D�~DԾD��D�>D�~DվD��D�>D�~D־D��D�>D�~D׾D��D�>D�~DؾD��D�>D�~DپD��D�>D�~DھD��D�>D�~D۾D��D�>D�~DܾD��D�>D�~DݾD��D�>D�~D޾D��D�>D�~D߾D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D��D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D��{111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111ƨAպ^AվwA�ƨA�ƨA�ȴA�ƨA�ƨA�ĜA�ȴA�ƨA�ȴA�ƨA���A���A���AնFAՇ+AҺ^A�%Aʩ�A�K�A�ĜA���A��A��yA�ZA���A���A�A�VA�r�A��mA��A��wA��DA�S�A�&�A�VA���A�;dA��7A�M�A��RA�bA��FA�oA���A��#A��A���A�VA�1'A��-A���A��A���A��A��A�9XA�bA��!A���A�Q�A��A�(�A���A�
=A��yA��A�/A�M�A�%A��RA�dZA��A��!A�v�A���A��A���A�oA���A��RA�"�A�ȴA��DA�+A��PA��A�?}A�v�A�(�A�ĜA��A�Azn�Awt�Avn�AuG�Aq�;Al~�Ai?}AedZAdI�Ac7LAbAaA_��A^9XA[�TAZA�AY��AX��AW;dAUoAR{AQ`BAO��AN�AN1AM`BALz�AJ��AH�`AH��AGƨAF�jAE�ABjAAA?�A>{A<��A;�FA:~�A8ffA6�uA3��A2 �A1O�A0�HA0(�A.ĜA-33A*�\A(��A'A&v�A%��A$��A#��A �RA�
A?}AVA��AM�A�FA�A�RA�A�A(�A��AhsA�A��A�A��AXAA��A�9A�uAz�A�
A�A��A��AI�AVA=qA��A�wA|�AK�A^5A�A�-A;dAJA7LAhsA�-A�PA�A�yA  AoA�FA
5?A
Q�A	x�AM�A��A�AA�A9XA�7Ar�AM�A�AXA   @��u@�~�@�@��@� �@�@�n�@��@���@�ƨ@�n�@�G�@�!@�@�h@�Q�@��@�7@�z�@�n�@�Ĝ@���@ڧ�@�{@�X@�Ĝ@� �@�o@֏\@�5?@�x�@Լj@�t�@�33@�J@Ѓ@��@ϥ�@��@�-@�X@��@�z�@�Q�@��H@��T@ǥ�@�V@Ų-@�hs@�G�@���@�l�@�@���@�(�@���@�"�@��R@���@�ff@���@��@��@�bN@�33@���@��@�  @�  @�|�@�o@���@���@�ff@�{@�`B@���@���@��@��@��@��m@��m@��w@���@�J@�O�@���@�bN@�  @��@�;d@��R@�`B@��9@���@��9@�z�@��@��!@���@�X@��h@�@��-@�r�@�z�@�1@��
@��@�o@��y@���@��!@��\@�5?@�J@��T@�7L@�V@�%@��`@���@�?}@��@��D@�Z@�A�@�  @���@�C�@�+@�o@��y@���@���@�~�@�=q@��^@�O�@��@�z�@��F@�t�@�l�@�dZ@�dZ@�
=@���@��\@�^5@��7@��`@��j@��@���@��D@�9X@��F@��@���@���@�ff@��@�{@�@�@��@��T@��#@���@���@�@�x�@���@��j@��@��D@�Q�@�ƨ@��@��@�|�@�\)@�+@�ȴ@�M�@��@�J@��T@���@�p�@�/@�%@��@���@���@�(�@��@��
@��@��@�;d@�o@�"�@��H@���@��\@�-@���@���@��7@��7@�p�@�O�@���@��@�A�@��@�|�@�+@�@��H@���@���@�^5@�$�@�J@���@�p�@�G�@�/@��@��@��`@�Z@��m@��@���@�|�@��@���@�{@�J@��h@�%@�r�@�A�@�b@��m@�S�@�C�@�C�@�+@��y@���@�v�@�n�@��T@�p�@�?}@�?}@�V@�r�@�Q�@��;@�K�@�o@�
=@���@���@��\@�v�@�ff@�{@��@���@���@�G�@��@���@���@��9@���@��D@��@�z�@�j@�I�@�  @~��@~�R@~�@~�@~�R@~ȴ@~��@~$�@}��@}O�@|j@|1@{��@{dZ@{S�@{33@{C�@{"�@{@z�H@z��@z��@z�!@z~�@z�@y��@y�^@yhs@y�@y%@x��@xĜ@x1'@w��@wl�@w�@vV@u�-@u`B@uV@t�D@tI�@st�@sC�@s33@s@r-@q��@q�@qx�@pr�@pb@o��@o|�@o�@n��@n��@nv�@nV@n5?@m��@m�@lj@k�
@k�@kdZ@k@jM�@i�#@i�^@i��@ihs@i%@h�9@hQ�@h �@h  @h  @h  @g�;@g�;@g�w@gl�@g\)@g;d@f�y@fv�@fE�@e��@e�h@eV@d�@d1@c�m@c�F@b��@a�7@a&�@`��@`r�@`  @_��@_K�@_
=@^V@]�-@]�h@]�@]�@]p�@]`B@]/@\�@\�@\I�@\1@[ƨ@[t�@[C�@[@Z��@Z��@Zn�@ZM�@Z-@Y�@Y��@YX@X�`@X��@XbN@X  @W�@W;d@W
=@Vȴ@V�+@V{@U@Up�@U`B@U?}@T�@TZ@T9X@T(�@T�@S�F@SdZ@SS�@S33@So@R�H@R��@R��@R�!@RM�@Q��@Qhs@Q7L@Q%@P�u@PbN@PQ�@Pb@O�;@O\)@O�@N�@Nȴ@N��@N@M��@M@M�h@L��@L�@Kt�@Ko@J=q@I�7@I%@H�9@H�@HA�@G�@G�;@G��@G�w@G+@Fff@E��@E��@E`B@EV@D�@Dj@D(�@D1@C�
@C��@C�@CS�@Co@B�H@B��@B�!@B��@B~�@B=q@B�@A��@A�#@A�^@Ahs@A7L@@��@@�u@@A�@@b@?�;@?|�@>�R@>�+@>ff@>E�@>$�@=��@=�@<��@<�@<��@<�@;t�@;"�@:��@:J@9�^@9�7@9X@9G�@9&�@8�`@8r�@8  @7�;@7�w@7��@7\)@7+@6��@6��@6V@6{@5�@5�T@5@5`B@4��@4�/@4��@4I�@4�@3��@3t�@3S�@3S�@333@3@2�@2�H@2��@2M�@1x�@1G�@1G�@17L@1&�@1�@0��@0�9@0�@0A�@/�;@/�@/|�@/l�@/\)@/�@/
=@.�y@.��@.E�@-��@-��@-`B@-/@-�@-V@,�@,�/@,��@,��@,j@,I�@,(�@+��@+�F@+��@+S�@+o@*��@*n�@*M�@)�^@)x�@)&�@)�@)�@(��@(�u@(r�@(Q�@(Q�@(b@'��@'�P@';d@&��@&�R@&�+@%�@%�h@%`B@$��@$�D@#��@#�F@#�@#o@"��@"~�@"n�@"n�@"J@!�#@!��@!X@!7L@!�@ �`@ r�@ bN@ b@�@�w@l�@\)@+@ff@�@�@`B@`B@`B@O�@/@��@��@z�@�F@dZ@"�@��@��@=q@J@�@�@�#@��@�7@x�@hs@&�@�`@��@��@Ĝ@�9@Q�@1'@b@b@�@�w@�@�P@l�@�@��@ȴ@��@�+@E�@{@�@��@p�@��@�j@�j@�@z�@z�@I�@�@1@�m@�
@�@t�@"�@��@~�@M�@��@��@X@G�@&�@%@��@Ĝ@�9@��@r�@Q�@1'@ �@b@b@�@�@�P@|�@l�@K�@�@��@�y@��@E�@��@��@`B@/@�@�j@��@�D@Z@Z@I�@��@�m@�m@�
@��@��@dZ@33@"�@
�@
�H@
�!@
~�@
n�@
M�@
�@	��@	�#@	��@	hs@	X@	X@	G�@	G�@	7L@	7L@	�@	%@��@�`@�9@�u@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111ƨAպ^AվwA�ƨA�ƨA�ȴA�ƨA�ƨA�ĜA�ȴA�ƨA�ȴA�ƨA���A���A���AնFAՇ+AҺ^A�%Aʩ�A�K�A�ĜA���A��A��yA�ZA���A���A�A�VA�r�A��mA��A��wA��DA�S�A�&�A�VA���A�;dA��7A�M�A��RA�bA��FA�oA���A��#A��A���A�VA�1'A��-A���A��A���A��A��A�9XA�bA��!A���A�Q�A��A�(�A���A�
=A��yA��A�/A�M�A�%A��RA�dZA��A��!A�v�A���A��A���A�oA���A��RA�"�A�ȴA��DA�+A��PA��A�?}A�v�A�(�A�ĜA��A�Azn�Awt�Avn�AuG�Aq�;Al~�Ai?}AedZAdI�Ac7LAbAaA_��A^9XA[�TAZA�AY��AX��AW;dAUoAR{AQ`BAO��AN�AN1AM`BALz�AJ��AH�`AH��AGƨAF�jAE�ABjAAA?�A>{A<��A;�FA:~�A8ffA6�uA3��A2 �A1O�A0�HA0(�A.ĜA-33A*�\A(��A'A&v�A%��A$��A#��A �RA�
A?}AVA��AM�A�FA�A�RA�A�A(�A��AhsA�A��A�A��AXAA��A�9A�uAz�A�
A�A��A��AI�AVA=qA��A�wA|�AK�A^5A�A�-A;dAJA7LAhsA�-A�PA�A�yA  AoA�FA
5?A
Q�A	x�AM�A��A�AA�G�O�G�O�Ar�AM�A�AXA   @��u@�~�@�@��@� �@�@�n�@��@���@�ƨ@�n�@�G�@�!@�@�h@�Q�@��@�7@�z�@�n�@�Ĝ@���@ڧ�@�{@�X@�Ĝ@� �@�o@֏\@�5?@�x�@Լj@�t�@�33@�J@Ѓ@��@ϥ�@��@�-@�X@��@�z�@�Q�@��H@��T@ǥ�@�V@Ų-@�hs@�G�@���@�l�@�@���@�(�@���@�"�@��R@���@�ff@���@��@��@�bN@�33@���@��@�  @�  @�|�@�o@���@���@�ff@�{@�`B@���@���@��@��@��@��m@��m@��w@���@�J@�O�@���@�bN@�  @��@�;d@��R@�`B@��9@���@��9@�z�@��@��!@���@�X@��h@�@��-@�r�@�z�@�1@��
@��@�o@��y@���@��!@��\@�5?@�J@��T@�7L@�V@�%@��`@���@�?}@��@��D@�Z@�A�@�  @���@�C�@�+@�o@��y@���@���@�~�@�=q@��^@�O�@��@�z�@��F@�t�@�l�@�dZ@�dZ@�
=@���@��\@�^5@��7@��`@��j@��@���@��D@�9X@��F@��@���@���@�ff@��@�{@�@�@��@��T@��#@���@���@�@�x�@���@��j@��@��D@�Q�@�ƨ@��@��@�|�@�\)@�+@�ȴ@�M�@��@�J@��T@���@�p�@�/@�%@��@���@���@�(�@��@��
@��@��@�;d@�o@�"�@��H@���@��\@�-@���@���@��7@��7@�p�@�O�@���@��@�A�@��@�|�@�+@�@��H@���@���@�^5@�$�@�J@���@�p�@�G�@�/@��@��@��`@�Z@��m@��@���@�|�@��@���@�{@�J@��h@�%@�r�@�A�@�b@��m@�S�@�C�@�C�@�+@��y@���@�v�@�n�@��T@�p�@�?}@�?}@�V@�r�@�Q�@��;@�K�@�o@�
=@���@���@��\@�v�@�ff@�{@��@���@���@�G�@��@���@���@��9@���@��D@��@�z�@�j@�I�@�  @~��@~�R@~�@~�@~�R@~ȴ@~��@~$�@}��@}O�@|j@|1@{��@{dZ@{S�@{33@{C�@{"�@{@z�H@z��@z��@z�!@z~�@z�@y��@y�^@yhs@y�@y%@x��@xĜ@x1'@w��@wl�@w�@vV@u�-@u`B@uV@t�D@tI�@st�@sC�@s33@s@r-@q��@q�@qx�@pr�@pb@o��@o|�@o�@n��@n��@nv�@nV@n5?@m��@m�@lj@k�
@k�@kdZ@k@jM�@i�#@i�^@i��@ihs@i%@h�9@hQ�@h �@h  @h  @h  @g�;@g�;@g�w@gl�@g\)@g;d@f�y@fv�@fE�@e��@e�h@eV@d�@d1@c�m@c�F@b��@a�7@a&�@`��@`r�@`  @_��@_K�@_
=@^V@]�-@]�h@]�@]�@]p�@]`B@]/@\�@\�@\I�@\1@[ƨ@[t�@[C�@[@Z��@Z��@Zn�@ZM�@Z-@Y�@Y��@YX@X�`@X��@XbN@X  @W�@W;d@W
=@Vȴ@V�+@V{@U@Up�@U`B@U?}@T�@TZ@T9X@T(�@T�@S�F@SdZ@SS�@S33@So@R�H@R��@R��@R�!@RM�@Q��@Qhs@Q7L@Q%@P�u@PbN@PQ�@Pb@O�;@O\)@O�@N�@Nȴ@N��@N@M��@M@M�h@L��@L�@Kt�@Ko@J=q@I�7@I%@H�9@H�@HA�@G�@G�;@G��@G�w@G+@Fff@E��@E��@E`B@EV@D�@Dj@D(�@D1@C�
@C��@C�@CS�@Co@B�H@B��@B�!@B��@B~�@B=q@B�@A��@A�#@A�^@Ahs@A7L@@��@@�u@@A�@@b@?�;@?|�@>�R@>�+@>ff@>E�@>$�@=��@=�@<��@<�@<��@<�@;t�@;"�@:��@:J@9�^@9�7@9X@9G�@9&�@8�`@8r�@8  @7�;@7�w@7��@7\)@7+@6��@6��@6V@6{@5�@5�T@5@5`B@4��@4�/@4��@4I�@4�@3��@3t�@3S�@3S�@333@3@2�@2�H@2��@2M�@1x�@1G�@1G�@17L@1&�@1�@0��@0�9@0�@0A�@/�;@/�@/|�@/l�@/\)@/�@/
=@.�y@.��@.E�@-��@-��@-`B@-/@-�@-V@,�@,�/@,��@,��@,j@,I�@,(�@+��@+�F@+��@+S�@+o@*��@*n�@*M�@)�^@)x�@)&�@)�@)�@(��@(�u@(r�@(Q�@(Q�@(b@'��@'�P@';d@&��@&�R@&�+@%�@%�h@%`B@$��@$�D@#��@#�F@#�@#o@"��@"~�@"n�@"n�@"J@!�#@!��@!X@!7L@!�@ �`@ r�@ bN@ b@�@�w@l�@\)@+@ff@�@�@`B@`B@`B@O�@/@��@��@z�@�F@dZ@"�@��@��@=q@J@�@�@�#@��@�7@x�@hs@&�@�`@��@��@Ĝ@�9@Q�@1'@b@b@�@�w@�@�P@l�@�@��@ȴ@��@�+@E�@{@�@��@p�@��@�j@�j@�@z�@z�@I�@�@1@�m@�
@�@t�@"�@��@~�@M�@��@��@X@G�@&�@%@��@Ĝ@�9@��@r�@Q�@1'@ �@b@b@�@�@�P@|�@l�@K�@�@��@�y@��@E�@��@��@`B@/@�@�j@��@�D@Z@Z@I�@��@�m@�m@�
@��@��@dZ@33@"�@
�@
�H@
�!@
~�@
n�@
M�@
�@	��@	�#@	��@	hs@	X@	X@	G�@	G�@	7L@	7L@	�@	%@��@�`@�9@�u@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�BJBO�B�B��B��BǮB��B�5B1B��B�/B�NB#�B.B49B-B"�B-B-B9XBE�B@�B>wB+B0!B(�BPB��B��BB��B  BB��B�BŢB��B�B��B�}B��B��B��B�VB�B]/BH�BC�Bs�Bw�BiyBq�BbNBM�BaHBS�B=qBDB
ƨB
��B
p�B
M�B
w�B
l�B
dZB
]/B
ffB
bNB
ZB
M�B
9XB
G�B
<jB
9XB
0!B	��B	�B	ǮB	ǮB	��B	�FB	�B	A�B	I�B	.B	9XB	0!B	%�B	"�B	-B	(�B	�B	�B	(�B	�B��B�yB�5B	  B��B	B	B��B��B�`B�B��B�B�;B�/B�RBB��B�XBĜB�wB�^B��B�{B�B�=B��B��B�=Bn�BbNBH�BXBhsB]/BiyBW
BM�B2-B_;BiyBp�BjB[#BL�B/BR�BQ�BR�B\)B`BBgmBcTB_;B\)Be`BYB_;BbNBbNB`BB]/BVBQ�B^5B\)BW
BF�BO�B`BB`BB_;B]/BVB_;Bn�B{�B��B�?B�FB�3B�-B�?B��B��B�\B{�Bx�B�Bv�BD�BN�Bu�B�uB��B�
B�BƨB�wB�'B��B�\B��B�VB��B��B��B��B��B��B�{B�%BiyBr�B�oB�oB�+B�B�B~�Bv�B~�B�B��B��B�{B��B��B��B��B��B��B��B��B�B��B��B�-B�}B�qB�jB��BȴBǮBȴBĜBÖBƨB��B�BB�ZB�mB�sB�NB�B�yB�HB��B��B��B��B��B��B��B��B��B��B�B��B	%B	PB	hB	�B	�B	�B	�B	�B	�B	�B	(�B	+B	,B	.B	2-B	49B	0!B	&�B	,B	-B	2-B	5?B	6FB	6FB	5?B	2-B	.B	33B	C�B	@�B	=qB	8RB	/B	8RB	?}B	L�B	N�B	K�B	@�B	H�B	O�B	N�B	Q�B	R�B	VB	W
B	VB	VB	S�B	W
B	YB	W
B	aHB	dZB	e`B	gmB	k�B	m�B	k�B	r�B	t�B	t�B	u�B	x�B	}�B	~�B	}�B	� B	� B	�B	~�B	}�B	�B	�B	�B	�B	�PB	�hB	�oB	��B	��B	��B	��B	��B	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�-B	�3B	�3B	�3B	�9B	�9B	�9B	�9B	�3B	�'B	�!B	�^B	�^B	�XB	�RB	�LB	�qB	��B	��B	�}B	�wB	�jB	�}B	ÖB	ŢB	ŢB	ĜB	ŢB	ƨB	ɺB	��B	��B	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�5B	�HB	�BB	�5B	�)B	�5B	�5B	�BB	�BB	�ZB	�mB	�sB	�yB	�yB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	�B	�B	��B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	�B	�B	��B	��B	�B	�B	��B	��B	��B	�B	��B	��B	��B	��B
  B	��B	��B
B
  B
B
  B
B
B
B
B
B
B
B
%B
+B
+B
+B
1B
%B
B
B
B
1B
JB
JB
JB
JB

=B
	7B
+B
1B
+B

=B
DB
VB
\B
\B
hB
hB
hB
oB
uB
uB
uB
uB
hB
uB
uB
uB
{B
�B
�B
{B
oB
{B
�B
{B
oB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
"�B
/B
�B
�B
�B
�B
�B
�B
 �B
"�B
 �B
�B
"�B
$�B
%�B
$�B
#�B
$�B
$�B
&�B
(�B
(�B
)�B
)�B
)�B
)�B
(�B
)�B
)�B
(�B
'�B
)�B
(�B
(�B
'�B
(�B
'�B
+B
(�B
$�B
%�B
,B
-B
-B
-B
.B
-B
-B
-B
.B
2-B
33B
33B
33B
2-B
1'B
1'B
1'B
0!B
1'B
1'B
1'B
2-B
2-B
33B
49B
33B
33B
33B
2-B
2-B
2-B
1'B
33B
33B
2-B
33B
33B
5?B
49B
49B
49B
49B
6FB
7LB
7LB
5?B
5?B
8RB
9XB
8RB
7LB
7LB
:^B
9XB
9XB
9XB
:^B
:^B
8RB
6FB
5?B
:^B
9XB
9XB
8RB
;dB
;dB
:^B
:^B
9XB
;dB
<jB
<jB
<jB
:^B
<jB
<jB
;dB
8RB
7LB
9XB
;dB
9XB
<jB
=qB
@�B
A�B
A�B
A�B
C�B
B�B
A�B
>wB
>wB
@�B
C�B
C�B
C�B
D�B
C�B
D�B
F�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
H�B
G�B
G�B
G�B
H�B
x�B
YB
u�B
dZB
bNB
hsB
G�B
G�B
H�B
H�B
G�B
F�B
J�B
K�B
K�B
J�B
I�B
H�B
K�B
K�B
I�B
H�B
H�B
K�B
J�B
K�B
M�B
N�B
N�B
O�B
N�B
M�B
M�B
M�B
P�B
P�B
P�B
O�B
O�B
P�B
O�B
O�B
P�B
Q�B
Q�B
P�B
P�B
O�B
R�B
Q�B
P�B
R�B
Q�B
S�B
T�B
T�B
T�B
S�B
T�B
S�B
R�B
Q�B
P�B
T�B
XB
W
B
W
B
W
B
VB
T�B
T�B
T�B
T�B
W
B
W
B
XB
XB
W
B
XB
W
B
VB
VB
VB
XB
YB
YB
ZB
ZB
ZB
ZB
ZB
ZB
YB
ZB
ZB
ZB
YB
ZB
YB
YB
YB
YB
ZB
YB
[#B
\)B
]/B
]/B
\)B
\)B
]/B
]/B
]/B
\)B
\)B
\)B
\)B
\)B
]/B
]/B
[#B
]/B
^5B
]/B
]/B
]/B
`BB
`BB
_;B
`BB
bNB
cTB
cTB
aHB
bNB
bNB
bNB
cTB
cTB
cTB
bNB
dZB
cTB
jB
x�B
l�B
{�B
p�B
x�B
� B
x�B
��B
t�B
hsB
hsB
gmB
gmB
ffB
e`B
dZB
gmB
hsB
hsB
iyB
hsB
jB
k�B
k�B
k�B
jB
l�B
l�B
k�B
jB
k�B
l�B
m�B
l�B
l�B
k�B
l�B
l�B
m�B
l�B
l�B
m�B
m�B
l�B
k�B
m�B
l�B
m�B
m�B
l�B
m�B
m�B
l�B
m�B
l�B
n�B
p�B
o�B
o�B
o�B
o�B
o�B
p�B
o�B
o�B
n�B
o�B
n�B
m�B
o�B
o�B
o�B
o�B
q�B
r�B
r�B
r�B
r�B
s�B
r�B
s�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
r�B
s�B
t�B
t�B
s�B
s�B
r�B
x�B
r�B
~�B
x�B
�B
�1B
�%B
��B
�7B
u�B
v�B
v�B
w�B
v�B
u�B
w�B
w�B
w�B
v�B
w�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
x�B
w�B
w�B
x�B
w�B
w�B
w�B
y�B
�B
�B
�JB
�1B
�DB
��B
�uB
�\B
�uB
�PB
}�B
z�B
{�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
��B
�*B
�B,BW�B�3B�4B��B�B�KB�B	�B�B��B�B$�B/OB5%B/OB%�B/�B/�B;BF�BBB@OB-�B1�B*�B�B�cB��B�B��BB�B��B�OBɺB�B�	B�bB�;B��B�B��B�HB�B`�BLJBF?Bs�Bx�BlBs�BfBPbBbBT�B>�B(B
��B
��B
v�B
R�B
yrB
m�B
f2B
^�B
gB
cB
[=B
OvB
;�B
H�B
>(B
:xB
1�B
�B	��B	�B	��B	�JB	��B	��B	HKB	MjB	2GB	:�B	1�B	'�B	$tB	.�B	+B	QB	�B	)�B	5B	 �B�qB��B	 B��B	B	%B	 B�>B�BںB�B��B��B��B��BāBյB��B�%B�B�PB��B�$B��B�JB��B�_B�xBp�Bd�BLBZ7Bi�B^�BjBX�BO�B5�B`BBjKBp�Bj�B\CBN"B1�BS�BS[BTB]B`�Bg�Bc�B_�B\�Be�BZB_�Bb�Bb�B`�B]�BW
BR�B^�B\�BW�BHKBP�B`�B`�B_�B]�BWYB_�Bo B|6B��B��B�B�B�|B��B�
B�	B��B}�Bz�B��Bx�BH�BQ�Bv�B�,B��G�O�G�O�B��B�HB�aB��B��B�5B��B��B��B��B��B�`B��B��B��Bl�BtnB��B��B�KB�3B�B�BxRB�B�AB��B�B�B�B�B�SB�TB�yB��B��B��B�kB��B��B��B��B��B�"B�'B�B�1B�BżBĜB�1BҽB�B��B�B��B�nB��B�KB�B�FB�DB�JB�.B�PB�XB�^B�qB�}B��B��B��B	�B	�B	�B	�B	�B	�B	�B	�B	=B	!B	)B	+B	,qB	.�B	2|B	4TB	0�B	'�B	,qB	-�B	2�B	5�B	6�B	6�B	5�B	2�B	/ B	3�B	C�B	@�B	=�B	8�B	0!B	8�B	@ B	L�B	N�B	L0B	AUB	H�B	P.B	O(B	R:B	S@B	V9B	W$B	V9B	VSB	TFB	W?B	YeB	W�B	abB	dtB	e�B	gmB	kkB	m�B	k�B	r�B	t�B	u%B	v+B	y$B	~(B	B	~BB	�B	�4B	� B	cB	~]B	�UB	�gB	��B	��B	�jB	��B	��B	��B	��B	��B	��B	��B	�,B	�B	��B	��B	��B	��B	�B	�;B	�NB	�0B	�eB	�WB	�IB	�-B	�MB	�MB	�MB	�TB	�nB	�nB	�TB	�MB	�vB	��B	�^B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ðB	żB	��B	��B	��B	��B	��B	��B	��B	�	B	�#B	�B	�B	�4B	�B	�B	�B	��B	�,B	�:B	�B	�YB	�EB	�QB	�OB	�bB	�\B	ބB	ܒB	ޞB	ބB	��B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	�B	�B	��B	��B	��B	��B	� B	��B	��B	��B	��B	�B	� B	��B	�'B	��B	�B	��B	��B	��B	�B	��B	��B	��B	�B	��B	��B	��B	�'B	��B	��B	�	B	�B	�3B	�	B	�8B	�2B	�B
 4B	�.B	�(B
B
 B
;B
 4B
GB
GB
GB
UB
3B
9B
9B
?B
EB
EB
EB
1B
?B
MB
{B
{B
KB
JB
JB
dB
dB

XB
	�B
_B
�B
zB

XB
xB
pB
\B
�B
hB
�B
�B
�B
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B

B
�B
�B
�B
�B
�B
"�G�O�B
�B
�B
�B
	B
�B
�B
 �B
"�B
 �B
 B
#B
$�B
&B
$�B
$&B
%B
%B
'B
(�B
(�B
)�B
*B
)�B
*0B
)B
*0B
*B
)B
(>B
*B
)*B
)*B
($B
)DB
(>B
+B
)*B
%`B
&fB
,=B
-CB
-]B
-]B
.IB
-CB
-CB
-]B
.IB
2-B
33B
33B
33B
2aB
1AB
1AB
1AB
0UB
1AB
1AB
1AB
2GB
2GB
3hB
4TB
3MB
3hB
3MB
2GB
2GB
2GB
1[B
3MB
3MB
2aB
3MB
3hB
5tB
4TB
4nB
4�B
4TB
6zB
7fB
7fB
5tB
5�B
8RB
9XB
8lB
7�B
7fB
:xB
9rB
9�B
9rB
:^B
:^B
8lB
6zB
5�B
:^B
9rB
9rB
8�B
;B
;B
:xB
:xB
9�B
;B
<�B
<�B
<�B
:�B
<�B
<�B
;�B
8�B
7�B
9�B
;�B
9�B
<�B
=�B
@�B
A�B
A�B
A�B
C�B
B�B
A�B
>�B
>�B
@�B
C�B
C�B
C�B
D�B
C�B
D�B
F�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
H�B
G�B
G�B
G�B
H�G�O�G�O�G�O�G�O�G�O�G�O�B
H1B
G�B
H�B
H�B
G�B
F�B
J�B
K�B
K�B
J�B
J	B
IB
K�B
K�B
I�B
H�B
IB
K�B
J�B
LB
NB
N�B
N�B
O�B
N�B
M�B
N"B
N"B
P�B
Q B
Q B
O�B
PB
Q B
PB
O�B
Q B
RB
RB
Q B
Q B
P.B
SB
R B
Q4B
S&B
R B
TB
T�B
T�B
UB
T,B
T�B
TB
SB
R B
Q4B
UB
XB
W$B
W
B
W
B
VB
U2B
UB
U2B
U2B
W$B
W$B
X+B
X+B
W$B
X+B
W$B
V9B
VSB
V9B
X+B
Y1B
Y1B
Z7B
ZB
ZQB
ZB
Z7B
Z7B
YKB
Z7B
Z7B
ZQB
Y1B
Z7B
YKB
YKB
Y1B
Y1B
Z7B
YeB
[WB
\CB
]/B
]/B
\CB
\]B
]IB
]dB
]/B
\CB
\]B
\CB
\CB
\]B
]IB
]IB
[WB
]IB
^jB
]~B
]dB
]dB
`vB
`vB
_�B
`vB
b�B
cTB
cnB
a|B
bhB
bhB
b�B
cnB
c�B
c�B
b�B
dtB
c�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
h�B
h�B
g�B
g�B
f�B
e�B
d�B
g�B
h�B
h�B
i�B
h�B
j�B
k�B
k�B
k�B
j�B
l�B
l�B
k�B
j�B
k�B
l�B
m�B
l�B
l�B
k�B
l�B
l�B
m�B
l�B
l�B
m�B
m�B
l�B
k�B
m�B
l�B
m�B
m�B
l�B
m�B
m�B
l�B
m�B
l�B
n�B
p�B
o�B
o�B
o�B
o�B
o�B
p�B
o�B
o�B
n�B
o�B
n�B
m�B
o�B
o�B
o�B
o�B
q�B
r�B
r�B
r�B
r�B
s�B
r�B
s�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
r�B
s�B
t�B
t�B
s�B
s�B
r�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
v+B
v�B
v�B
w�B
v�B
u�B
w�B
w�B
w�B
v�B
w�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
x�B
w�B
w�B
x�B
xB
w�B
w�B
zG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
z�B
{�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144444411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444444444411111111111111111111111111111111111111111111111111111111111111111111111111111111111111444444444111111111111111111111111114444444444411��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�G�O�G�O�G�O�G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from this cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    SP=0.06(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201811170035222018111700352220181117003522202111120416442021111204164420211112041644JA  ARFMdecpA19c                                                                20181116093622O�IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20181116003631O�IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20181116003635O�IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20181116003635O�QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20181116003636O�QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20181116003636O�QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20181116003636O�QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcp2.8e                                                                20181116003636O�QCF$                G�O�G�O�G�O�            4000JA  ARGQaqcp2.8e                                                                20181116003636O�QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20181116003636O�QCF$                G�O�G�O�G�O�            4000JA  ARGQrqcpt16c                                                                20181116003636O�QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20181116003636O�                    G�O�G�O�G�O�                JA  ARUP                                                                        20181116005610O�                    G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20181116153510O�CV  JULD            G�O�G�O�Fć�                JM  ARCAJMQC2.0                                                                 20181116153522O�CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20181116153522O�CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20181117000000  CF  PSAL_ADJUSTED_QCC�  C�  G�O�                JM  ARSQJMQC2.0                                                                 20181117000000  CF  TEMP_ADJUSTED_QCC�  C�  G�O�                JM  ARCAJMTM1.0                                                                 20181204170025O�CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20191219072406O�IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116231517                      G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20210204000000  CF  PSAL_ADJUSTED_QCD�� D�  G�O�                JA  ARDU                                                                        20211116021501                      G�O�G�O�G�O�                