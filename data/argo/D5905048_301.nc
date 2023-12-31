CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-11-13T00:36:47Z creation;2018-11-13T00:36:52Z conversion to V3.1;2019-12-19T07:24:19Z update;     
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181113003647  20200116231517  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL              -A   JA  I2_0577_301                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @ؐ63��1   @ؐ7�8�@4o�͞���ds�M;1   GPS     A   B   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @33@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A���A�  A�33B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B��B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DRfDR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�3D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�C3D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�|�D�� D�  D�@ D�3D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�ɚD��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@\)@|(�@�{@�{A
=A?
=A_
=A
=A��A��A��A��A�Q�A߅A�RA��BBBBB'B/B7B?BGBOBWB_BgBoBwB\)B��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB�B��HB��HB��HB��HB��HB��HB��HC�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RD |)D �)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D	|)D	�)D
|)D
�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D |)D �)D!|)D!�)D"|)D"�)D#|)D#�)D$|)D$�)D%|)D%�)D&|)D&�)D'|)D'�)D(|)D(�)D)|)D)�)D*|)D*�)D+|)D+�)D,|)D,�)D-|)D-�)D.|)D.�)D/|)D/�)D0|)D0�)D1|)D1�)D2|)D2�)D3|)D3�)D4|)D4�)D5|)D5�)D6|)D6�)D7|)D7�)D8|)D8�)D9|)D9�)D:|)D:�)D;|)D;�)D<|)D<�)D=|)D=�)D>|)D>�)D?|)D?�)D@|)D@�)DA|)DA�)DB|)DB�)DC|)DC�)DD|)DD�)DE|)DE�)DF|)DF�)DG|)DG�)DH|)DH�)DI|)DI�)DJ|)DJ�)DK|)DK�)DL|)DL�)DM|)DM�)DN|)DN�)DO|)DO�)DP|)DP�)DQ|)DR�DR|)DR�)DS|)DS�)DT|)DT�)DU|)DU�)DV|)DV�)DW|)DW�)DX|)DX�)DY|)DY�)DZ|)DZ�)D[|)D[�)D\|)D\�)D]|)D]�)D^|)D^�)D_|)D_�)D`|)D`�)Da|)Da�)Db|)Db�)Dc|)Dc�)Dd|)Dd�)De|)De�)Df|)Df�)Dg|)Dg�)Dh|)Dh�)Di|)Di�)Dj|)Dj�)Dk|)Dk�)Dl|)Dl�)Dm|)Dm�)Dn|)Dn�)Do|)Do�)Dp|)Dp�)Dq|)Dq�)Dr|)Dr�)Ds|)Ds�)Dt|)Dt�)Du|)Du�)Dv|)Dv�)Dw|)Dw�)Dx|)Dx�)Dy|)Dy�)Dz|)Dz�)D{|)D{�)D||)D|�)D}|)D}�)D~|)D~�)D|)D�)D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D�HD�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�:�D�~D��D��D�>D�~D��D��D�>D�~D��D���D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�z�D��D��D�>D�~D��D��D�>D�~D��D��D�AHD�~D��D�HD�>D�~D��D��D�>D�~D¾D��D�>D�~DþD��D�>D�~DľD��D�>D�~DžD��D�>D�~DƾD��D�>D�~DǾD��D�>D�~DȾD��D�>D�~DɾD��D�>D�~DʾD��D�>D�~D˾D��D�>D�~D̾D��D�>D�~D;D��D�>D�~DξD��D�>D�~DϾD��D�>D�~DоD��D�>D�~DѾD��D�>D�~DҾD��D�>D�~DӾD��D�>D�~DԾD��D�>D�~DվD��D�>D�~D־D��D�>D�~D׾D��D�>D�~DؾD��D�>D�~DپD��D�>D�~DھD��D�>D�~D۾D��D�>D�~DܾD��D�AHD�~DݾD��D�>D�~D޾D��D�>D�~D߾D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�z�D�D��D�>D�HD�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D��D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D��HD�ǮD��H11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�ȴA���A�ƨA�ĜA�ƨA�ȴA�ƨA�ĜA�ĜA�A���AռjA���Aհ!A��/A�&�AυAΗ�A��TA�O�A�dZA�VA���A�VA��A�1A�r�A�K�A��9A�ȴA���A�=qA��9A�C�A�$�A�ffA���A�{A��TA��HA��hA�oA���A��^A�v�A�l�A�=qA�33A��A�5?A���A�{A�(�A�A�9XA�\)A��+A�I�A�t�A�~�A�A�Q�A�t�A�`BA���A��A�jA��A�-A���A�p�A�XA�5?A�5?A���A��A���A��yA�dZA��A��RA{x�Av�Au�hAup�AuO�At�!AqS�AnI�AmG�Al��AlI�Ak��Aj�yAj(�Ah~�Ac�Ab��Aa�wAa
=A^�+A]7LA[�PAY\)AWAV��AT�!AQ��AM��AKp�AJ��AJ�RAJ^5AJJAI�FAH=qAE�;AE�AC�wAB  AAp�AA+AA
=A@bNA?XA=�A:��A9��A8E�A7hsA6��A6(�A5�#A5�A5XA3A0�RA.��A-
=A+hsA)��A)A(jA(A�A(A'�A&jA&bA%�7A#�A"jA!XA �DA JAt�Az�A$�Al�A�mAoA�!A�^AM�A��A&�A��A(�A�;A��A�DA|�A�RAn�A�AE�A�FAQ�AA
ffA	��A	dZA	��A	�A(�A?}AZAȴA�A�PA ��@��
@�-@�/@��j@��D@���@�@��-@�j@�@��-@��/@�/@��@���@�v�@�O�@���@��
@�33@�@�@�$�@�E�@�@�A�@��@�@�C�@�-@��@�33@�Ĝ@��;@���@߾w@�v�@���@�dZ@���@�5?@�hs@�(�@�K�@��#@�/@��@��/@��`@ԛ�@�33@�{@љ�@щ7@ёh@щ7@�O�@�7L@Ͼw@�V@�$�@��@�I�@˕�@�"�@�@���@��m@��@���@�%@�bN@��m@�K�@¸R@�V@�7L@�|�@�ȴ@�v�@�=q@�J@���@��j@�1@��@�n�@�`B@�bN@�(�@�  @���@�
=@�ȴ@���@��\@�M�@��^@�/@��`@��j@��u@�bN@�I�@�(�@��
@���@��#@���@��h@��7@�x�@���@�j@���@�t�@��F@���@�C�@��@��9@�33@��y@���@�5?@���@��7@�`B@�7L@���@��@�z�@�bN@�I�@��w@�33@���@���@�$�@�@���@��@�X@�&�@��/@��u@�Z@�A�@�(�@�b@�1@�  @��m@��m@��
@���@���@���@���@�K�@�33@���@��!@��@�G�@��j@��D@�bN@�A�@� �@� �@��@�1@���@�l�@���@��H@�ȴ@��!@���@��\@�=q@��T@���@���@���@��-@�hs@�/@��@��j@�A�@�(�@��@�b@���@���@��@�dZ@�"�@��@���@�^5@�5?@�$�@�{@�@���@��7@��@�r�@�9X@��@��@��@��@���@�M�@��@��h@�p�@�&�@��`@�Ĝ@�j@�I�@�b@���@��
@�dZ@��H@��\@�V@�{@�@��@���@��@���@��@�hs@�G�@�V@���@�  @��;@��;@��@�K�@�~�@��@��^@�x�@�p�@�?}@�`B@�p�@�O�@��@�Ĝ@���@�bN@�I�@��m@�t�@�\)@�;d@�+@��y@���@��+@�ff@�=q@�$�@��#@���@��h@��@�O�@�V@���@��`@��/@��@��@�t�@�C�@���@��R@�^5@��@��@��h@�hs@�?}@���@��@�z�@�Q�@�I�@�9X@��m@��@��P@�l�@�S�@�o@��!@��\@���@���@�{@��@�/@���@��/@�Ĝ@��u@�j@�Q�@�(�@��@�;@~�R@~E�@~5?@~$�@}�T@}�@}O�@}�@}�@}V@}V@|��@|��@|�/@|��@|�@z^5@y�^@yX@y&�@x�9@xA�@w��@w|�@w;d@w+@w+@w+@v�y@vV@v@u�@u�T@u�-@u�h@u�h@u�h@uO�@t��@t�@sC�@rn�@q��@q%@pr�@pb@oK�@n�y@n�+@n$�@m@m�h@l�/@l�@l��@lz�@lI�@k�m@k��@kS�@k"�@j�\@j=q@i��@i&�@h�@hA�@g�P@g
=@fV@e�@e��@e��@d��@d�@c��@c�
@c�@c33@co@b�!@b�@a��@aG�@`�`@`  @_�w@_l�@_K�@_;d@^��@^��@^ff@^{@]��@]?}@\��@\j@\9X@[ƨ@[�@[t�@[o@Z�@Z��@Z��@Z~�@Z~�@ZM�@Y�#@Yhs@X��@X�@XQ�@W�;@W��@W
=@V��@V{@U�T@U�T@U�T@U�T@U��@U`B@T�/@TZ@S��@S�F@SS�@S"�@R��@R=q@Q�#@Qx�@Q�@P�`@P�u@P1'@O�@OK�@Nȴ@N��@Nv�@NV@N5?@M�T@M�h@M/@L�@L��@L�D@K��@K��@K@J��@J�\@I��@Ihs@I&�@H�`@H��@HbN@G�@G|�@G\)@G;d@G+@F�y@F�R@F��@F@E@E`B@E�@D�j@Dz�@D9X@C�
@C�@CdZ@CC�@Co@B�H@B��@B~�@B^5@BJ@A��@AX@A%@@�`@@�9@@A�@@ �@?�@?��@?;d@>�y@>��@>ff@>V@>E�@=�@=�h@=`B@=/@<�/@<�j@<j@<�@;��@;dZ@;dZ@:�@:�\@:J@9�#@9��@9��@9��@9�7@9hs@9�@8Ĝ@8�9@8r�@8  @7�;@7��@7�w@7|�@7;d@6�y@6�R@6��@6V@5�@5p�@5O�@5?}@5/@5V@4��@4�@4�/@4��@4�@3��@3dZ@3S�@3S�@3"�@2��@2�!@2��@2�\@2~�@2-@1�@1�#@1��@1x�@17L@0�`@0�u@0bN@0b@/�;@/�P@/l�@/+@.��@.ȴ@.�+@.5?@-�@-�h@-p�@-�@,�/@,�D@,9X@+�
@+�@+S�@+33@*�H@*�\@*M�@*�@)�#@)��@)��@)�7@)�@(Q�@'�@'�P@'K�@'
=@&�y@&ȴ@&��@&{@%@%�@%/@$�j@$z�@$I�@#�m@#S�@"��@"��@"M�@"=q@"-@!��@!��@!�7@!X@!G�@!&�@!%@ �`@ Ĝ@ �9@ �u@ bN@ b@�@�@�P@;d@;d@;d@K�@K�@;d@;d@+@+@+@K�@K�@;d@+@�y@ff@{@�T@@��@O�@�/@z�@Z@(�@1@�m@�F@"�@�!@n�@�@�@��@��@�^@��@X@�@�9@r�@bN@A�@1'@�;@�@�P@K�@�@ȴ@��@v�@{@�-@/@�@�/@�j@�D@j@I�@(�@�
@o@��@��@��@�\@�\@�\@�\@~�@n�@^5@^5@M�@�#@hs@�@�`@bN@1'@ �@b@�@�P@\)@;d@+@�@
=@�y@�y@�@�R@��@��@v�@ff@ff@V@E�@E�@5?@{@�T@�h@�@��@�@��@�D@�D@z�@j@I�@9X@(�@(�@��@�m@ƨ@ƨ@ƨ@�F@��@o@
~�@
^5@
=q@	��@	�@	�#@	�^@	��@	hs@	&�@	%@�`@Ĝ@�u@r�@r�@r�@r�11111111111111111111111111111111111111111111111111111111111111111111111111141114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�ȴA���A�ƨA�ĜA�ƨA�ȴA�ƨA�ĜA�ĜA�A���AռjA���Aհ!A��/A�&�AυAΗ�A��TA�O�A�dZA�VA���A�VA��A�1A�r�A�K�A��9A�ȴA���A�=qA��9A�C�A�$�A�ffA���A�{A��TA��HA��hA�oA���A��^A�v�A�l�A�=qA�33A��A�5?A���A�{A�(�A�A�9XA�\)A��+A�I�A�t�A�~�A�A�Q�A�t�A�`BA���A��A�jA��A�-A���A�p�A�XA�5?A�5?A���G�O�G�O�A��yA�dZG�O�G�O�A{x�Av�Au�hAup�AuO�At�!AqS�AnI�AmG�Al��AlI�Ak��Aj�yAj(�Ah~�Ac�Ab��Aa�wAa
=A^�+A]7LA[�PAY\)AWAV��AT�!AQ��AM��AKp�AJ��AJ�RAJ^5AJJAI�FAH=qAE�;AE�AC�wAB  AAp�AA+AA
=A@bNA?XA=�A:��A9��A8E�A7hsA6��A6(�A5�#A5�A5XA3A0�RA.��A-
=A+hsA)��A)A(jA(A�A(A'�A&jA&bA%�7A#�A"jA!XA �DA JAt�Az�A$�Al�A�mAoA�!A�^AM�A��A&�A��A(�A�;A��A�DA|�A�RAn�A�AE�A�FAQ�AA
ffA	��A	dZA	��A	�A(�A?}AZAȴA�A�PA ��@��
@�-@�/@��j@��D@���@�@��-@�j@�@��-@��/@�/@��@���@�v�@�O�@���@��
@�33@�@�G�O�G�O�@�@�A�@��@�@�C�@�-@��@�33@�Ĝ@��;@���@߾w@�v�@���@�dZ@���@�5?@�hs@�(�@�K�@��#@�/@��@��/@��`@ԛ�@�33@�{@љ�@щ7@ёh@щ7@�O�@�7L@Ͼw@�V@�$�@��@�I�@˕�@�"�@�@���@��m@��@���@�%@�bN@��m@�K�@¸R@�V@�7L@�|�@�ȴ@�v�@�=q@�J@���@��j@�1@��@�n�@�`B@�bN@�(�@�  @���@�
=@�ȴ@���@��\@�M�@��^@�/@��`@��j@��u@�bN@�I�@�(�@��
@���@��#@���@��h@��7@�x�@���@�j@���@�t�@��F@���@�C�@��@��9@�33@��y@���@�5?@���@��7@�`B@�7L@���@��@�z�@�bN@�I�@��w@�33@���@���@�$�@�@���@��@�X@�&�@��/@��u@�Z@�A�@�(�@�b@�1@�  @��m@��m@��
@���@���@���@���@�K�@�33@���@��!@��@�G�@��j@��D@�bN@�A�@� �@� �@��@�1@���@�l�@���@��H@�ȴ@��!@���@��\@�=q@��T@���@���@���@��-@�hs@�/@��@��j@�A�@�(�@��@�b@���@���@��@�dZ@�"�@��@���@�^5@�5?@�$�@�{@�@���@��7@��@�r�@�9X@��@��@��@��@���@�M�@��@��h@�p�@�&�@��`@�Ĝ@�j@�I�@�b@���@��
@�dZ@��H@��\@�V@�{@�@��@���@��@���@��@�hs@�G�@�V@���@�  @��;@��;@��@�K�@�~�@��@��^@�x�@�p�@�?}@�`B@�p�@�O�@��@�Ĝ@���@�bN@�I�@��m@�t�@�\)@�;d@�+@��y@���@��+@�ff@�=q@�$�@��#@���@��h@��@�O�@�V@���@��`@��/@��@��@�t�@�C�@���@��R@�^5@��@��@��h@�hs@�?}@���@��@�z�@�Q�@�I�@�9X@��m@��@��P@�l�@�S�@�o@��!@��\@���@���@�{@��@�/@���@��/@�Ĝ@��u@�j@�Q�@�(�@��@�;@~�R@~E�@~5?@~$�@}�T@}�@}O�@}�@}�@}V@}V@|��@|��@|�/@|��@|�@z^5@y�^@yX@y&�@x�9@xA�@w��@w|�@w;d@w+@w+@w+@v�y@vV@v@u�@u�T@u�-@u�h@u�h@u�h@uO�@t��@t�@sC�@rn�@q��@q%@pr�@pb@oK�@n�y@n�+@n$�@m@m�h@l�/@l�@l��@lz�@lI�@k�m@k��@kS�@k"�@j�\@j=q@i��@i&�@h�@hA�@g�P@g
=@fV@e�@e��@e��@d��@d�@c��@c�
@c�@c33@co@b�!@b�@a��@aG�@`�`@`  @_�w@_l�@_K�@_;d@^��@^��@^ff@^{@]��@]?}@\��@\j@\9X@[ƨ@[�@[t�@[o@Z�@Z��@Z��@Z~�@Z~�@ZM�@Y�#@Yhs@X��@X�@XQ�@W�;@W��@W
=@V��@V{@U�T@U�T@U�T@U�T@U��@U`B@T�/@TZ@S��@S�F@SS�@S"�@R��@R=q@Q�#@Qx�@Q�@P�`@P�u@P1'@O�@OK�@Nȴ@N��@Nv�@NV@N5?@M�T@M�h@M/@L�@L��@L�D@K��@K��@K@J��@J�\@I��@Ihs@I&�@H�`@H��@HbN@G�@G|�@G\)@G;d@G+@F�y@F�R@F��@F@E@E`B@E�@D�j@Dz�@D9X@C�
@C�@CdZ@CC�@Co@B�H@B��@B~�@B^5@BJ@A��@AX@A%@@�`@@�9@@A�@@ �@?�@?��@?;d@>�y@>��@>ff@>V@>E�@=�@=�h@=`B@=/@<�/@<�j@<j@<�@;��@;dZ@;dZ@:�@:�\@:J@9�#@9��@9��@9��@9�7@9hs@9�@8Ĝ@8�9@8r�@8  @7�;@7��@7�w@7|�@7;d@6�y@6�R@6��@6V@5�@5p�@5O�@5?}@5/@5V@4��@4�@4�/@4��@4�@3��@3dZ@3S�@3S�@3"�@2��@2�!@2��@2�\@2~�@2-@1�@1�#@1��@1x�@17L@0�`@0�u@0bN@0b@/�;@/�P@/l�@/+@.��@.ȴ@.�+@.5?@-�@-�h@-p�@-�@,�/@,�D@,9X@+�
@+�@+S�@+33@*�H@*�\@*M�@*�@)�#@)��@)��@)�7@)�@(Q�@'�@'�P@'K�@'
=@&�y@&ȴ@&��@&{@%@%�@%/@$�j@$z�@$I�@#�m@#S�@"��@"��@"M�@"=q@"-@!��@!��@!�7@!X@!G�@!&�@!%@ �`@ Ĝ@ �9@ �u@ bN@ b@�@�@�P@;d@;d@;d@K�@K�@;d@;d@+@+@+@K�@K�@;d@+@�y@ff@{@�T@@��@O�@�/@z�@Z@(�@1@�m@�F@"�@�!@n�@�@�@��@��@�^@��@X@�@�9@r�@bN@A�@1'@�;@�@�P@K�@�@ȴ@��@v�@{@�-@/@�@�/@�j@�D@j@I�@(�@�
@o@��@��@��@�\@�\@�\@�\@~�@n�@^5@^5@M�@�#@hs@�@�`@bN@1'@ �@b@�@�P@\)@;d@+@�@
=@�y@�y@�@�R@��@��@v�@ff@ff@V@E�@E�@5?@{@�T@�h@�@��@�@��@�D@�D@z�@j@I�@9X@(�@(�@��@�m@ƨ@ƨ@ƨ@�F@��@o@
~�@
^5@
=q@	��@	�@	�#@	�^@	��@	hs@	&�@	%@�`@Ĝ@�u@r�@r�@r�@r�11111111111111111111111111111111111111111111111111111111111111111111111111144114411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B%�B%�B%�B&�B&�B&�B&�B&�B&�B&�B'�B(�B"�BbB
��B^5Bo�B7LB?}B�B�)B\B9XB�BE�BVBD�B\B
=B;dB/BI�BQ�B[#BG�B;dB0!B%�B1'BE�B8RB�B{BB+B�BhB�B��B�!B��B�+B�B�=Bs�BN�BE�BL�B<jBA�B%�B1B
�5B
�\B
{�B
�B
ĜB
�'B
�9B
��B
��B
~�B
z�B
XB
`BB
T�B
{B	�yB	�B	�mB	�uB	�DB	��B	��B	ÖB	�B	~�B	t�B	��B	��B	��B	�hB	�B	w�B	W
B	(�B	E�B	S�B	I�B	-B	'�B	�B	B	B��B�;B�qB�'B��B�B��B�B�`B�B��B��B��B�LB�!B��B��BɺB�XB��B�hB�1B��B��B��B��B�B��B��B��Bx�BQ�BgmBl�Bp�BiyB�%B�B�PB�%Bx�Bt�Bw�Bn�BW
BW
Be`BhsBm�BiyB]/BhsBXBI�BR�BXBI�B@�BVB]/B`BB^5B`BB\)BG�BA�BB�BG�B$�B5?B>wB7LB49BH�BI�BM�Bk�B\)BP�BF�BE�B49B.B-B@�B33B:^B?}BB�BB�B7LB.B>wB>wB7LB9XBD�BK�B6FB!�B&�B<jBJ�BM�BYB~�B�VB�uB�jB�oB��B�+B�+B�1B�B�7B�Br�B~�B|�Bo�B�B�B� B�oB�bB�bB�=B�oB�hB��B��B��B��B��B��B��B�'B�LB�dB�dB�XB�XB�!B�!B��B��BŢB��B��B��B��B��B��B��B��B�B�/B�B�#B�)B��B�B�B�B��B��B�B�B��B��B��B��B��B	+B	
=B		7B	%B	JB	uB	�B	�B	�B	�B	 �B	$�B	&�B	&�B	&�B	$�B	"�B	�B	'�B	1'B	49B	33B	1'B	/B	,B	&�B	=qB	=qB	8RB	2-B	&�B	(�B	%�B	49B	49B	6FB	6FB	;dB	>wB	A�B	@�B	G�B	H�B	G�B	E�B	A�B	C�B	K�B	K�B	J�B	R�B	ZB	^5B	_;B	`BB	`BB	bNB	ffB	iyB	k�B	l�B	n�B	o�B	o�B	s�B	w�B	y�B	|�B	~�B	� B	}�B	�B	~�B	}�B	w�B	|�B	� B	�+B	�1B	�7B	�=B	�JB	�JB	�DB	�=B	�7B	�DB	�hB	�uB	�uB	��B	�uB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�!B	�B	�B	�B	�B	�!B	�FB	�FB	�9B	�?B	�jB	�qB	�wB	�}B	��B	ŢB	ŢB	ƨB	ȴB	ǮB	��B	��B	��B	��B	ȴB	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�B	�/B	�/B	�#B	�B	�B	�HB	�NB	�BB	�/B	�#B	�/B	�TB	�ZB	�mB	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B	��B
  B
B
B
B
B
B
B
B
B
B
  B
B
B
1B
B	��B	��B
B
B
+B
+B
%B
+B
1B
+B
%B
	7B
B
	7B
JB
DB
DB

=B
PB
PB
\B
\B
\B
\B
VB
PB
DB
1B
%B
DB
\B
hB
oB
oB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
oB
uB
�B
�B
�B
�B
�B
�B
�B
�B
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
$�B
$�B
"�B
$�B
"�B
#�B
#�B
%�B
$�B
%�B
'�B
'�B
,B
)�B
'�B
)�B
(�B
-B
,B
,B
-B
+B
)�B
+B
,B
,B
)�B
0!B
/B
1'B
0!B
/B
/B
0!B
/B
/B
.B
/B
0!B
2-B
1'B
2-B
33B
2-B
49B
49B
33B
49B
49B
2-B
0!B
0!B
1'B
49B
49B
33B
33B
2-B
49B
49B
7LB
9XB
8RB
8RB
6FB
49B
33B
49B
5?B
7LB
6FB
7LB
6FB
6FB
6FB
7LB
7LB
9XB
8RB
8RB
7LB
9XB
9XB
<jB
=qB
<jB
<jB
;dB
;dB
;dB
<jB
<jB
<jB
:^B
;dB
;dB
>wB
>wB
<jB
=qB
@�B
@�B
A�B
?}B
?}B
@�B
C�B
C�B
C�B
B�B
B�B
C�B
@�B
B�B
B�B
C�B
C�B
D�B
D�B
D�B
E�B
F�B
F�B
F�B
F�B
F�B
G�B
F�B
E�B
F�B
F�B
G�B
G�B
H�B
F�B
H�B
H�B
G�B
G�B
H�B
I�B
J�B
K�B
K�B
I�B
I�B
K�B
K�B
J�B
K�B
J�B
J�B
J�B
L�B
M�B
K�B
K�B
K�B
N�B
N�B
O�B
O�B
O�B
N�B
M�B
M�B
O�B
N�B
N�B
P�B
Q�B
P�B
O�B
N�B
O�B
P�B
P�B
O�B
O�B
O�B
R�B
S�B
S�B
S�B
S�B
S�B
R�B
Q�B
O�B
P�B
S�B
VB
T�B
S�B
S�B
VB
VB
VB
T�B
S�B
T�B
VB
VB
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
XB
XB
XB
W
B
W
B
W
B
XB
XB
YB
XB
YB
XB
YB
YB
ZB
ZB
[#B
YB
ZB
[#B
[#B
[#B
\)B
\)B
[#B
YB
W
B
[#B
\)B
^5B
^5B
_;B
_;B
^5B
]/B
^5B
_;B
_;B
_;B
`BB
`BB
_;B
^5B
_;B
bNB
bNB
dZB
cTB
cTB
bNB
dZB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
dZB
cTB
cTB
cTB
ffB
e`B
e`B
gmB
hsB
hsB
gmB
gmB
hsB
hsB
hsB
hsB
hsB
gmB
ffB
ffB
dZB
cTB
e`B
ffB
ffB
ffB
e`B
e`B
ffB
hsB
hsB
hsB
hsB
ffB
e`B
ffB
hsB
iyB
jB
jB
k�B
k�B
jB
iyB
hsB
iyB
jB
k�B
k�B
k�B
jB
k�B
k�B
k�B
jB
jB
k�B
k�B
iyB
jB
jB
l�B
n�B
n�B
m�B
n�B
m�B
m�B
l�B
jB
o�B
q�B
q�B
q�B
r�B
q�B
q�B
q�B
q�B
q�B
p�B
o�B
m�B
m�B
o�B
o�B
o�B
q�B
s�B
r�B
r�B
q�B
s�B
s�B
t�B
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
u�B
u�B
u�B
t�B
s�B
r�B
r�B
r�B
t�B
v�B
v�B
v�B
w�B
v�B
w�B
v�B
w�B
w�B
w�B
v�B
v�B
v�B
w�B
w�B
v�B
u�B
s�B
s�B
w�B
w�B
w�B
x�B
x�B
w�B
w�B
w�B
w�B
x�B
x�B
y�B
x�B
y�B
z�B
z�B
z�B
{�11111111111111111111111111111111111111111111111111111111111111111111111111141114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B%�B%�B%�B&�B&�B&�B&�B&�B&�B'B(
B)DB#�B�B 4Ba|Br�B@4BIB��B�\B�B;dB)BG�BW�BG�BgB�B=qB2|BJ�BR�B[�BIlB<�B2B(sB33BF?B9�BWBYB�B	B�B B��B�.B�B�hB�)B��B�BvFBR�BHKBN�B>�BB�B'�B
�B
��B
�$B
�B
��B
�%B
��B
�ZB
�DB
�#B
��B
}VB
[WB
a�G�O�G�O�B	��B	�G�O�G�O�B	�HB	ªB	�B	�B	��B	��B	xB	�mB	�kB	�=B	�TB	�MB	y$B	Y�B	-�B	GEB	T�B	KB	0!B	)�B	�B	�B	B��B�hB��B��B�@B�GB�%B�B��B�#B��B�B�uB�$B��B�jB�VB�#B��B��B�{B�B�BB�kB��B��B��B�yB�nB�eB{dBVBjBn�Br�Bk�B��B�B��B��BzBu�BxlBo�BYeBX�Bf�Bi�BnIBjeB^jBiBYeBK�BT,BX�BKDBBuBV�B^B`�B_!B`�B\�BIRBC-BC�BH�B(
B7B?}B9XB6BI�BJ�BN�Bk6B]IBRoBH1BGB6�B0UB/BA;B4�B;�B@BB�BB�B8B/iB>�B?cB8lB:DBE9BK�B7�B#�B(�B="BK)BN�BY1B}�B��B�&G�O�G�O�B�$B��B�KB�RB�MB�#B�MBtnB�B}�Bq�B��B�9B�;B��B� B�4B�DB�@B�oB�!B�B�B�>B�_B��B��B�[B�fB�dB��B��B��B�[B�'B��B�UB�YB�bB�{B͹BΥB��BѝB��BՁB֡BݘBںBۦBܒB��B�?B��B�B�B�B�3B�aB�fB�JB��B��B��B	_B	
rB		�B	�B	~B	�B	�B	�B	+B	#B	 �B	%B	'8B	'8B	'B	%B	#:B	�B	(�B	1AB	4nB	3hB	1[B	/�B	,�B	'�B	=B	=VB	8�B	2�B	(
B	)�B	&�B	4�B	4�B	6�B	6�B	;�B	>�B	A�B	AB	G�B	H�B	G�B	E�B	BB	DB	LB	LB	K^B	S[B	ZQB	^jB	_pB	`vB	`�B	b�B	f�B	i�B	k�B	l�B	n�B	o�B	o�B	s�B	w�B	zB	}B	~�B	�4B	~BB	�;B	HB	~]B	xlB	}qB	��B	�_B	�KB	�RB	�XB	�JB	�dB	�^B	�rB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	��B	�'B	��B	�B	�B	�2B	�B	�B	�>B	�B	�$B	�DB	�6B	�/B	�;B	�UB	�5B	�CB	�WB	��B	��B	�zB	�zB	��B	��B	��B	��B	��B	��B	��B	żB	��B	��B	��B	�B	��B	��B	��B	��B	�B	�)B	�(B	�4B	�FB	�$B	�EB	�B	�KB	�KB	�KB	�IB	�IB	�WB	�B	ٚB	�bB	�hB	�vB	ݲB	��B	ݘB	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	�B	��B	��B	� B	��B	��B	��B	��B	��B	�B	�B	��B	�	B	�B	��B	��B	��B	�	B	�B	�B	��B	�B	�B	�?B	�B	��B	�*B	�0B	�0B	�"B	�(B	�<B	�B
 4B	�.B
 4B
UB
-B
MB
-B
UB
UB
-B
-B
'B
 4B
;B
-B
B
3B	�}B	�]B
AB
SB
EB
EB
tB
_B
KB
_B
tB
	7B
�B
	RB
~B
xB
^B

rB
jB
jB
\B
vB
\B
vB
VB
jB
xB
�B
�B
xB
vB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
!�B
#B
$B
#�B
$B
#�B
%B
$�B
#B
$�B
# B
$B
$&B
%�B
%FB
&B
($B
(>B
,"B
*0B
(>B
*0B
)DB
-B
,=B
,"B
-CB
+QB
*KB
+6B
,=B
,=B
*B
0!B
/5B
1AB
0UB
/5B
/OB
0;B
/OB
/5B
.IB
/OB
0UB
2GB
1vB
2aB
3MB
2|B
49B
4nB
3MB
4nB
49B
2GB
0UB
0UB
1[B
4TB
4TB
3�B
3MB
2aB
4TB
4�B
7fB
9XB
8RB
8RB
6`B
4nB
3hB
4�B
5tB
7fB
6zB
7fB
6zB
6�B
6�B
7�B
7�B
9�B
8lB
8�B
7�B
9�B
9�B
<�B
=�B
<�B
<�B
;B
;B
;�B
<�B
<�B
<�B
:�B
;�B
;�B
>�B
>�B
<�B
=�B
@�B
@�B
A�B
?�B
?�B
@�B
C�B
C�B
C�B
B�B
B�B
C�B
@�B
B�B
B�B
C�B
C�B
D�B
D�B
D�B
E�B
F�B
F�B
F�B
F�B
F�B
G�B
F�B
E�B
F�B
F�B
G�B
G�B
H�B
F�B
H�B
H�B
G�B
G�B
H�B
I�B
J�B
K�B
K�B
I�B
I�B
K�B
K�B
J�B
K�B
J�B
J�B
KB
L�B
NB
K�B
K�B
K�B
N�B
N�B
O�B
O�B
O�B
OB
M�B
NB
O�B
N�B
N�B
QB
RB
Q B
O�B
N�B
PB
Q B
Q B
O�B
PB
PB
R�B
S�B
TB
TB
S�B
S�B
SB
RB
P.B
QB
TB
VB
T�B
TB
TB
VB
VB
VB
UB
T,B
UB
VB
V9B
U2B
UB
UB
UB
V9B
V9B
W$B
W$B
X+B
X+B
X+B
W$B
W$B
W$B
X+B
X+B
Y1B
XEB
Y1B
X+B
Y1B
YeB
ZQB
Z7B
[=B
YeB
Z7B
[=B
[=B
[=B
\CB
\)B
[=B
YKB
WsB
[=B
\]B
^OB
^OB
_VB
_pB
^OB
]dB
^OB
_VB
_VB
_pB
`\B
`\B
_pB
^�B
_�B
b�B
b�B
dtB
c�B
cnB
bhB
dZB
d�B
dtB
dtB
dtB
d�B
e�B
e`B
d�B
cnB
cnB
c�B
ffB
ezB
e�B
g�B
hsB
hsB
gmB
gmB
hsB
hsB
h�B
h�B
hsB
gmB
f�B
f�B
d�B
c�B
e�B
f�B
f�B
f�B
e�B
e�B
f�B
h�B
h�B
h�B
h�B
f�B
e�B
f�B
h�B
i�B
j�B
j�B
k�B
k�B
j�B
i�B
h�B
i�B
j�B
k�B
k�B
k�B
j�B
k�B
k�B
k�B
j�B
j�B
k�B
k�B
i�B
j�B
j�B
l�B
n�B
n�B
m�B
n�B
m�B
m�B
l�B
j�B
o�B
q�B
q�B
q�B
r�B
q�B
q�B
q�B
q�B
q�B
p�B
o�B
m�B
m�B
o�B
o�B
o�B
q�B
s�B
r�B
r�B
q�B
s�B
s�B
t�B
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
u�B
u�B
u�B
t�B
s�B
r�B
r�B
r�B
t�B
v�B
v�B
v�B
w�B
v�B
w�B
v�B
w�B
w�B
w�B
v�B
v�B
v�B
w�B
w�B
v�B
u�B
s�B
s�B
w�B
w�B
w�B
x�B
x�B
w�B
xB
w�B
w�B
y	B
x�B
y�B
x�B
y�B
z�B
z�B
z�B
{�11111111111111111111111111111111111111111111111111111111111111111111111111144114411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�;o;oG�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.06(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201811170035532018111700355320181117003553201811170200252018111702002520181117020025201811180037102018111800371020181118003710  JA  ARFMdecpA19c                                                                20181113093636  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20181113003647  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20181113003651  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20181113003651  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20181113003652  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20181113003652  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20181113003652  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcp2.8e                                                                20181113003652  QCF$                G�O�G�O�G�O�            4000JA  ARGQaqcp2.8e                                                                20181113003652  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20181113003652  QCF$                G�O�G�O�G�O�            4000JA  ARGQrqcpt16c                                                                20181113003652  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20181113003652                      G�O�G�O�G�O�                JA  ARUP                                                                        20181113005618                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20181113153519  CV  JULD            G�O�G�O�Fā�                JM  ARGQJMQC2.0                                                                 20181113153519  CV  JULD_LOCATION   G�O�G�O�Fā�                JM  ARGQJMQC2.0                                                                 20181113153519  CV  LATITUDE        G�O�G�O�A��                JM  ARSQJMQC2.0                                                                 20181114000000  CF  PSAL_ADJUSTED_QCC  C�  G�O�                JM  ARSQJMQC2.0                                                                 20181114000000  CF  TEMP_ADJUSTED_QCC  C�  G�O�                JM  ARCAJMQC2.0                                                                 20181116153553  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20181116153553  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20181116170025  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20181117153710  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116231517                      G�O�G�O�G�O�                