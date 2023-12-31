CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2017-08-02T00:35:37Z creation;2017-08-02T00:35:48Z conversion to V3.1;2019-12-19T08:01:04Z update;     
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
_FillValue                 �  I<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M$   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  pL   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t4   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �\   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �D   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �l   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ݬ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �<   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �<   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �<   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �<   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �    HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �0   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �4   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �8   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �<Argo profile    3.1 1.2 19500101000000  20170802003537  20200116221516  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0577_145                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @�4}��1   @�58� @4 A�7K��d��c�A 1   GPS     A   B   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  BxffB�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fDfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�<�D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�)�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�G�@�{@�{A
=A?
=A_
=A
=A��A��A��A��AυA߅A�A��BBBBB'B/B7B?BGBOBWB_BgBoBx(�BB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HC�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC�C�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RD |)D �)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D	|)D	�)D
|)D
�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D��D�D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D |)D �)D!|)D!�)D"|)D"�)D#|)D#�)D$|)D$�)D%|)D%�)D&|)D&�)D'|)D'�)D(|)D(�)D)|)D)�)D*|)D*�)D+|)D+�)D,|)D,�)D-|)D-�)D.|)D.�)D/|)D/�)D0|)D0�)D1|)D1�)D2|)D2�)D3|)D3�)D4|)D4�)D5|)D5�)D6|)D6�)D7|)D7�)D8|)D8�)D9|)D9�)D:|)D:�)D;|)D;�)D<|)D<�)D=|)D=�)D>|)D>�)D?|)D?�)D@|)D@�)DA|)DA�)DB|)DB�)DC|)DC�)DD|)DD�)DE|)DE�)DF|)DF�)DG|)DG�)DH|)DH�)DI|)DI�)DJ|)DJ�)DK|)DK�)DL|)DL�)DM|)DM�)DN|)DN�)DO|)DO�)DP|)DP�)DQ|)DQ�)DR|)DR�)DS|)DS�)DT|)DT�)DU|)DU�)DV|)DV�)DW|)DW�)DX|)DX�)DY|)DY�)DZ|)DZ�)D[|)D[�)D\|)D\�)D]|)D]�)D^|)D^�)D_|)D_�)D`|)D`�)Da|)Da�)Db|)Db�)Dc|)Dc�)Dd|)Dd�)De|)De�)Df|)Df�)Dg|)Dg�)Dh|)Dh�)Di|)Di�)Dj|)Dj�)Dk|)Dk�)Dl|)Dl�)Dm|)Dm�)Dn|)Dn�)Do|)Do�)Dp|)Dp�)Dq|)Dq�)Dr|)Dr�)Ds|)Ds�)Dt|)Dt�)Du|)Du�)Dv|)Dv�)Dw|)Dw�)Dx|)Dx�)Dy|)Dy�)Dz|)Dz�)D{|)D{�)D||)D|�)D}|)D}�)D~|)D~�)D|)D�)D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D���D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D¾D��D�>D�~DþD��D�>D�~DľD��D�>D�~DžD��D�>D�~DƾD��D�>D�~DǾD��D�>D�~DȾD��D�>D�~DɾD��D�>D�~DʾD��D�>D�~D˾D��D�>D�~D̾D��D�>D�~D;D��D�>D�~DξD��D�>D�~DϾD��D�>D�~DоD��D�>D�~DѾD��D�>D�~DҾD��D�>D�~DӾD��D�>D�~DԾD��D�>D�~DվD��D�>D�~D־D��D�>D�~D׾D��D�>D�~DؾD��D�>D�~DپD��D�>D�~DھD��D�>D�~D۾D��D�>D�~DܾD��D�>D�~DݾD��D�>D�~D޾D��D�>D�~D߾D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�:�D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D��D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D�HD�'�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A۶FA۴9A۴9A۰!AۮA۩�AۮAۮAۮAۧ�Aۧ�Aۧ�A۝�AۋDA�^5A�33A�r�A��AυA·+AͰ!A��Aˡ�A�5?A�n�A�(�A��mA�A��A��A�l�A�v�A�&�A���A��mA�bNA���A�5?A���A�O�A���A�z�A�jA�A�K�A�ĜA���A��A�?}A��A���A�VA�z�A��\A���A�`BA��A��A���A��
A�I�A��hA��`A�=qA��A�^5A�$�A���A�%A�l�A��;A�K�A��DA���A�;dA�`BA��hA�1A��+A�G�A��A�{A���A���A�VA�&�A��\A�ZA��-A�&�A��A�C�A�ĜA�E�A��A���A�7LA���A�n�A��
A�l�A���A�(�A��-A��A%A|�A{?}AzȴAzjAy�Ax~�Avv�Ar�9Ao�^An��Am�Ak�AhVAf��AedZAd�HAd�Ad��Adv�Ac�Ac\)AbbNA`��A_�A^�A^JA]&�A\�uA[
=AY+AXZAW��AV�HAU�AU�-AUx�AU+AT��AT=qASO�AQ��AP�AN��AL��AKx�AH�AD��AA�A>VA<�/A:9XA8ffA7\)A5�A3`BA2ȴA2I�A1��A0ffA0E�A01A/��A/x�A/hsA.��A-�A-+A+��A)�mA)
=A(~�A'��A&jA%C�A#S�A!�A!hsA ��A��A&�AhsAVA��A{A�9At�A��A��A��A�!AJA�AVAoA�#A�An�Ap�A
�/A	�
A��A�FA�A�RA=qA�\AdZAoA��AbNA �Al�@�t�@��^@�^5@� �@��@�S�@�@���@�^@�hs@�u@�+@�$�@��-@�u@�t�@��y@�{@�`B@�=q@�t�@�9@���@ޏ\@�Q�@ڇ+@�t�@�/@�A�@�1'@�9X@�Q�@Դ9@Ӆ@�M�@�A�@�o@��@���@�j@�1'@˶F@�K�@ʰ!@�$�@�O�@�I�@�\)@Ƈ+@�^5@�ff@�~�@�-@��@�&�@�O�@���@�j@��;@�x�@��w@��R@�V@�M�@�5?@�5?@�x�@�  @�o@�$�@�V@�Ĝ@��D@�bN@�(�@��@�\)@�"�@��H@���@��\@�M�@���@��@�r�@�j@�1'@�1@�K�@���@�5?@�{@��#@��-@���@�/@��@���@��@�7L@�x�@��-@���@�&�@��F@��@��@��!@�M�@���@��j@��@�ƨ@��P@�dZ@�;d@��H@�v�@�5?@�{@��^@�&�@�Ĝ@���@�1@��
@���@�"�@���@�ȴ@��\@�-@�@�x�@�G�@��@��@���@��`@���@�z�@�b@��
@��@�dZ@�dZ@�S�@�C�@�"�@��y@��R@���@�n�@��#@��^@��^@���@��7@�&�@���@��@��/@���@���@�Z@�1'@�1@�ƨ@���@�dZ@�S�@�K�@��@��y@���@�v�@�M�@��@��#@���@��@�/@�V@���@��u@�j@�Z@� �@��;@��@�l�@�"�@���@���@�ff@���@��@��@��@�Ĝ@��@���@���@��@��@��@�t�@�t�@�l�@�\)@�\)@�K�@���@���@�ff@��@�@��#@��^@���@��7@�p�@�`B@�O�@�/@���@���@���@�Z@��
@�t�@��@��!@�~�@�M�@�$�@��#@�@���@�G�@��9@��@�bN@�Z@�Q�@�I�@�(�@��;@��@�dZ@�33@�
=@��@��y@��@�ȴ@�ȴ@���@��+@�V@��@��@��-@�p�@�&�@��@��j@���@�r�@�(�@�1@��
@��P@�\)@�
=@��@���@���@��!@�v�@�5?@�J@���@�X@��@��`@��j@���@�9X@��w@���@��P@��@�t�@�l�@���@�^5@�{@��T@�x�@�O�@�&�@��@��9@�Z@�1'@�  @��;@��w@���@�\)@�C�@��@�@�ȴ@��+@�v�@�ff@�^5@��@��@�@���@��@�O�@�V@���@��@�Z@�(�@��@��@~ȴ@}�@}�-@}O�@|��@{�
@{C�@{33@z�@z�\@z^5@z�@y��@yx�@yG�@x��@xbN@xQ�@w�@wl�@w;d@v�@v��@v{@u�T@u��@t�/@tz�@tZ@tI�@t�@s��@s@r�@q�^@q�7@qx�@qhs@q7L@p�9@pA�@pQ�@p  @o��@o�@o�P@ol�@o;d@o
=@n�@n��@n�+@n�+@nff@nE�@m�@l�@l�D@l�D@lI�@k�
@kC�@j��@ihs@h��@hĜ@h�@hQ�@h  @g�@g�@g��@g�@f�R@f@e@e�-@eO�@eV@d�j@d�D@d(�@d�@c��@c�
@c�@ct�@b�@b��@b��@b�!@b-@a�#@a��@a7L@`�9@`Q�@_�@_|�@_\)@_;d@_�@^�@^��@^v�@]�@]?}@]�@]V@\�@\�j@\�@\z�@[��@[�F@[�@Z�H@Z~�@Z-@Y��@YX@Y7L@X��@X �@W+@V�R@Vff@V5?@U?}@T�/@T�/@T�/@T��@T��@Tj@TI�@T(�@T�@SdZ@R��@RM�@Q��@QX@Q%@P�u@PA�@P  @O�@O+@N��@N{@M@M`B@L�j@LZ@L(�@L1@K�
@K��@K@J��@JJ@I��@IG�@I&�@Hr�@H1'@H  @G�w@G��@G|�@GK�@F�@F�+@Fff@E�@E`B@E?}@E�@D��@D��@D�@D�/@DI�@C�
@C��@C�@Ct�@CC�@Co@B��@B��@Bn�@BJ@A��@A��@A%@@r�@@A�@?�@?�@?�P@?�P@?\)@>�y@>�+@>E�@=�@=p�@=?}@=�@<�@<j@;��@;�F@;��@;��@;S�@;"�@:�@:�!@:^5@9��@9��@9hs@9�@8�`@8��@8�u@81'@8b@8  @7�@7�w@7|�@7K�@6�@6ȴ@6��@6v�@5�@5��@5@5��@5?}@4��@4�/@4��@4�@4j@4�@4�@4�@41@3��@3t�@3"�@2��@2~�@2^5@2M�@2=q@1��@1&�@0�`@0��@0Ĝ@0Q�@/�;@/�w@/�@/��@/�P@/;d@.�@.��@.�+@.E�@-�@-p�@-�@,�/@,��@,j@,Z@,(�@+�
@+t�@+C�@+@*��@*��@*n�@*=q@)�#@)��@)hs@)�@(�`@(�9@(r�@(b@'�P@'\)@';d@&��@&�+@&ff@&E�@&$�@%�@%@%�@%?}@$�j@$�D@$z�@$I�@#�
@#dZ@#C�@#C�@#"�@"�H@"��@"��@"��@"�!@"�\@"M�@"J@!��@!�^@!x�@!hs@!X@!7L@!�@!�@ ��@ ��@ Ĝ@ ��@ �@ bN@ A�@�@|�@l�@;d@+@
=@�@��@E�@$�@{@{@@�@�T@@�h@`B@?}@/@V@�@�j@j@(�@�m@�F@��@��@�@�@S�@o@�@�H@��@~�@n�@^5@-@�@�#@��@��@��@��@�^@X@��@Ĝ@�9@�9@�@Q�@A�@A�@1'@�;@�@\)@�@�R@�+@E�@�@��@�-@��@`B@V@�@z�@9X@(�@�m@�F@��@�@"�@�H@��@n�@M�@J@�#@��@��@��@�9@�@bN@b@�;@��@�@l�@;d@;d@
=@ȴ@�R@��@��@v�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A۶FA۴9A۴9A۰!AۮA۩�AۮAۮAۮAۧ�Aۧ�Aۧ�A۝�AۋDA�^5A�33A�r�A��AυA·+AͰ!A��Aˡ�A�5?A�n�A�(�A��mA�A��A��A�l�A�v�A�&�A���A��mA�bNA���A�5?A���A�O�A���A�z�A�jA�A�K�A�ĜA���A��A�?}A��A���A�VA�z�A��\A���A�`BA��A��A���A��
A�I�A��hA��`A�=qA��A�^5A�$�A���A�%A�l�A��;A�K�A��DA���A�;dA�`BA��hA�1A��+A�G�A��A�{A���A���A�VA�&�A��\A�ZA��-A�&�A��A�C�A�ĜA�E�A��A���A�7LA���A�n�A��
A�l�A���A�(�A��-A��A%A|�A{?}AzȴAzjAy�Ax~�Avv�Ar�9Ao�^An��Am�Ak�AhVAf��AedZAd�HAd�Ad��Adv�Ac�Ac\)AbbNA`��A_�A^�A^JA]&�A\�uA[
=AY+AXZAW��AV�HAU�AU�-AUx�AU+AT��AT=qASO�AQ��AP�AN��AL��AKx�AH�AD��AA�A>VA<�/A:9XA8ffA7\)A5�A3`BA2ȴA2I�A1��A0ffA0E�A01A/��A/x�A/hsA.��A-�A-+A+��A)�mA)
=A(~�A'��A&jA%C�A#S�A!�A!hsA ��A��A&�AhsAVA��A{A�9At�A��A��A��A�!AJA�AVAoA�#A�An�Ap�A
�/A	�
A��A�FA�A�RA=qA�\AdZAoA��AbNA �Al�@�t�@��^@�^5@� �@��@�S�@�@���@�^@�hs@�u@�+@�$�@��-@�u@�t�@��y@�{@�`B@�=q@�t�@�9@���@ޏ\@�Q�@ڇ+@�t�@�/@�A�@�1'@�9X@�Q�@Դ9@Ӆ@�M�@�A�@�o@��@���@�j@�1'@˶F@�K�@ʰ!@�$�@�O�@�I�@�\)@Ƈ+@�^5@�ff@�~�@�-@��@�&�@�O�@���@�j@��;@�x�@��w@��R@�V@�M�@�5?@�5?@�x�@�  @�o@�$�@�V@�Ĝ@��D@�bN@�(�@��@�\)@�"�@��H@���@��\@�M�@���@��@�r�@�j@�1'@�1@�K�@���@�5?@�{@��#@��-@���@�/@��@���@��@�7L@�x�@��-@���@�&�@��F@��@��@��!@�M�@���@��j@��@�ƨ@��P@�dZ@�;d@��H@�v�@�5?@�{@��^@�&�@�Ĝ@���@�1@��
@���@�"�@���@�ȴ@��\@�-@�@�x�@�G�@��@��@���@��`@���@�z�@�b@��
@��@�dZ@�dZ@�S�@�C�@�"�@��y@��R@���@�n�@��#@��^@��^@���@��7@�&�@���@��@��/@���@���@�Z@�1'@�1@�ƨ@���@�dZ@�S�@�K�@��@��y@���@�v�@�M�@��@��#@���@��@�/@�V@���@��u@�j@�Z@� �@��;@��@�l�@�"�@���@���@�ff@���@��@��@��@�Ĝ@��@���@���@��@��@��@�t�@�t�@�l�@�\)@�\)@�K�@���@���@�ff@��@�@��#@��^@���@��7@�p�@�`B@�O�@�/@���@���@���@�Z@��
@�t�@��@��!@�~�@�M�@�$�@��#@�@���@�G�@��9@��@�bN@�Z@�Q�@�I�@�(�@��;@��@�dZ@�33@�
=@��@��y@��@�ȴ@�ȴ@���@��+@�V@��@��@��-@�p�@�&�@��@��j@���@�r�@�(�@�1@��
@��P@�\)@�
=@��@���@���@��!@�v�@�5?@�J@���@�X@��@��`@��j@���@�9X@��w@���@��P@��@�t�@�l�@���@�^5@�{@��T@�x�@�O�@�&�@��@��9@�Z@�1'@�  @��;@��w@���@�\)@�C�@��@�@�ȴ@��+@�v�@�ff@�^5@��@��@�@���@��@�O�@�V@���@��@�Z@�(�@��@��@~ȴ@}�@}�-@}O�@|��@{�
@{C�@{33@z�@z�\@z^5@z�@y��@yx�@yG�@x��@xbN@xQ�@w�@wl�@w;d@v�@v��@v{@u�T@u��@t�/@tz�@tZ@tI�@t�@s��@s@r�@q�^@q�7@qx�@qhs@q7L@p�9@pA�@pQ�@p  @o��@o�@o�P@ol�@o;d@o
=@n�@n��@n�+@n�+@nff@nE�@m�@l�@l�D@l�D@lI�@k�
@kC�@j��@ihs@h��@hĜ@h�@hQ�@h  @g�@g�@g��@g�@f�R@f@e@e�-@eO�@eV@d�j@d�D@d(�@d�@c��@c�
@c�@ct�@b�@b��@b��@b�!@b-@a�#@a��@a7L@`�9@`Q�@_�@_|�@_\)@_;d@_�@^�@^��@^v�@]�@]?}@]�@]V@\�@\�j@\�@\z�@[��@[�F@[�@Z�H@Z~�@Z-@Y��@YX@Y7L@X��@X �@W+@V�R@Vff@V5?@U?}@T�/@T�/@T�/@T��@T��@Tj@TI�@T(�@T�@SdZ@R��@RM�@Q��@QX@Q%@P�u@PA�@P  @O�@O+@N��@N{@M@M`B@L�j@LZ@L(�@L1@K�
@K��@K@J��@JJ@I��@IG�@I&�@Hr�@H1'@H  @G�w@G��@G|�@GK�@F�@F�+@Fff@E�@E`B@E?}@E�@D��@D��@D�@D�/@DI�@C�
@C��@C�@Ct�@CC�@Co@B��@B��@Bn�@BJ@A��@A��@A%@@r�@@A�@?�@?�@?�P@?�P@?\)@>�y@>�+@>E�@=�@=p�@=?}@=�@<�@<j@;��@;�F@;��@;��@;S�@;"�@:�@:�!@:^5@9��@9��@9hs@9�@8�`@8��@8�u@81'@8b@8  @7�@7�w@7|�@7K�@6�@6ȴ@6��@6v�@5�@5��@5@5��@5?}@4��@4�/@4��@4�@4j@4�@4�@4�@41@3��@3t�@3"�@2��@2~�@2^5@2M�@2=q@1��@1&�@0�`@0��@0Ĝ@0Q�@/�;@/�w@/�@/��@/�P@/;d@.�@.��@.�+@.E�@-�@-p�@-�@,�/@,��@,j@,Z@,(�@+�
@+t�@+C�@+@*��@*��@*n�@*=q@)�#@)��@)hs@)�@(�`@(�9@(r�@(b@'�P@'\)@';d@&��@&�+@&ff@&E�@&$�@%�@%@%�@%?}@$�j@$�D@$z�@$I�@#�
@#dZ@#C�@#C�@#"�@"�H@"��@"��@"��@"�!@"�\@"M�@"J@!��@!�^@!x�@!hs@!X@!7L@!�@!�@ ��@ ��@ Ĝ@ ��@ �@ bN@ A�@�@|�@l�@;d@+@
=@�@��@E�@$�@{@{@@�@�T@@�h@`B@?}@/@V@�@�j@j@(�@�m@�F@��@��@�@�@S�@o@�@�H@��@~�@n�@^5@-@�@�#@��@��@��@��@�^@X@��@Ĝ@�9@�9@�@Q�@A�@A�@1'@�;@�@\)@�@�R@�+@E�@�@��@�-@��@`B@V@�@z�@9X@(�@�m@�F@��@�@"�@�H@��@n�@M�@J@�#@��@��@��@�9@�@bN@b@�;@��@�@l�@;d@;d@
=@ȴ@�R@��@��@v�1111111111111311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�B�B�B�B�B�B�B�B�B�B�BuBJB��B��B'�B>wB?}BW
B\)BN�BaHB_;BE�B8RBcTBR�Bu�Bs�B�B�B�hB��B��B��B��B��B��B��B�JB�\B�hB�PB�oB�Bx�BhsBbNBO�BA�B/B'�B'�B&�B �BoB	7BBoB!�B-B0!B0!B)�B-B(�B �B�BPB%B��B�B�B�'B��B��B�JB�B}�Bs�B[#BF�B9XB<jB'�B�B�BVB
�B
�B
�;B
�)B
��B
�?B
�B
��B
��B
�VB
�1B
�B
x�B
dZB
l�B
p�B
^5B
YB
XB
[#B
W
B
M�B
9XB
&�B	��B	�B	�B	�;B	ɺB	�FB	�dB	�XB	��B	��B	��B	��B	ŢB	��B	�FB	�B	��B	��B	��B	�=B	}�B	p�B	gmB	n�B	m�B	jB	gmB	n�B	m�B	jB	l�B	n�B	k�B	bNB	`BB	YB	H�B	8RB	�B�B�B�B�'B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�DB�bB�B}�B�B�7B�Bz�Bv�Bn�Bz�B�B�Bx�B}�Bo�Bo�Bk�Bm�BiyBgmBw�B}�B� Bz�Bp�B`BBW
BT�BW
BF�BN�BP�BT�BP�BK�BO�BR�BW
BP�BM�BO�BW
BXBXB^5BW
BE�BM�BA�BJ�BD�BVB]/BdZBm�Bm�Bk�Bl�Bq�Bx�B|�B{�B�B~�B�1B�%B�By�B�Bz�Bw�Bu�Bm�Bm�Bu�B{�B~�B~�B~�B}�B}�B|�B}�B~�B{�B�B�B�B�B�B�B�B�B�1B�VB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�!B�B�B�-B�XB�XBBŢBǮBȴBɺB��B�B�B�/B�;B�;B�;B�ZB�B�B��B��B�B�B�B��B��B��B��B��B	+B	PB	hB	uB	{B	�B	�B	{B	�B	�B	%�B	$�B	#�B	%�B	(�B	.B	5?B	7LB	8RB	9XB	9XB	:^B	<jB	=qB	<jB	=qB	B�B	F�B	E�B	L�B	M�B	P�B	W
B	\)B	\)B	]/B	aHB	gmB	jB	l�B	o�B	n�B	o�B	o�B	o�B	r�B	w�B	z�B	|�B	�B	�B	�%B	�+B	�1B	�7B	�=B	�JB	�PB	�oB	�{B	�{B	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�B	�!B	�'B	�?B	�?B	�9B	�FB	�^B	�^B	�dB	�qB	�wB	�qB	�wB	��B	��B	B	ĜB	ĜB	ĜB	ŢB	ɺB	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�
B	�B	�B	�B	��B	�B	�B	�B	�#B	�#B	�)B	�/B	�/B	�/B	�5B	�5B	�/B	�5B	�;B	�;B	�5B	�5B	�BB	�HB	�ZB	�`B	�fB	�mB	�mB	�yB	�sB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
  B
B
B
B
B
B
%B
+B
+B
+B
+B

=B
DB
DB
DB

=B
	7B
	7B
JB
PB
JB
VB
VB
VB
VB
VB
\B
bB
hB
hB
hB
hB
oB
oB
oB
oB
uB
{B
{B
{B
uB
{B
{B
{B
{B
{B
{B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
#�B
#�B
#�B
"�B
"�B
"�B
%�B
&�B
&�B
&�B
&�B
%�B
$�B
'�B
'�B
(�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
+B
+B
+B
)�B
(�B
&�B
)�B
+B
)�B
(�B
(�B
'�B
&�B
)�B
,B
,B
,B
,B
-B
-B
,B
+B
,B
,B
.B
.B
.B
.B
/B
/B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
1'B
1'B
1'B
0!B
1'B
1'B
1'B
1'B
2-B
2-B
33B
49B
49B
49B
49B
49B
49B
33B
33B
6FB
6FB
6FB
6FB
6FB
6FB
5?B
6FB
6FB
6FB
7LB
7LB
7LB
7LB
8RB
7LB
6FB
6FB
7LB
9XB
8RB
7LB
9XB
;dB
;dB
;dB
:^B
:^B
:^B
:^B
9XB
8RB
8RB
9XB
:^B
:^B
;dB
;dB
<jB
<jB
<jB
<jB
<jB
<jB
=qB
=qB
=qB
>wB
?}B
?}B
?}B
?}B
>wB
?}B
>wB
@�B
B�B
C�B
B�B
C�B
D�B
D�B
E�B
E�B
D�B
D�B
D�B
E�B
E�B
E�B
G�B
G�B
G�B
G�B
G�B
F�B
E�B
F�B
G�B
H�B
H�B
G�B
G�B
G�B
H�B
H�B
G�B
G�B
G�B
G�B
G�B
I�B
I�B
J�B
J�B
K�B
J�B
I�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
J�B
K�B
L�B
M�B
M�B
M�B
M�B
N�B
N�B
M�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
P�B
R�B
R�B
R�B
Q�B
R�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
S�B
R�B
S�B
T�B
T�B
VB
VB
T�B
S�B
T�B
W
B
W
B
W
B
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
ZB
[#B
[#B
\)B
\)B
\)B
[#B
\)B
\)B
]/B
]/B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
_;B
_;B
_;B
_;B
_;B
`BB
aHB
aHB
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
cTB
cTB
cTB
cTB
dZB
e`B
e`B
dZB
e`B
e`B
e`B
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
ffB
ffB
ffB
ffB
gmB
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
hsB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
jB
jB
iyB
iyB
iyB
jB
jB
k�B
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
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
l�B
l�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
r�B
q�B
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
t�B
s�B
s�B
t�B
u�B
u�B
u�B
u�B
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
w�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�B�B�B�B�B�B�B�B�B�B�B�BjB��B{B-�BCaBDBYB^OBQ�BbhBa-BJXB>�BfBW�BxBwfB��B�B�B�B�ZB�B�tB��B�8B�hB��B�TB��B�HB�aB�EBz�Bj�Bd&BR�BEB2�B+B)�B(>B"4BFB^B�B&B#:B.�B1�B1vB+6B-�B)�B!�B�B�B�B��B�wB�WB�9B��B�_B��B�?B~�Bt�B]�BI�B;dB=qB*eB!B �BbB
�B
�[B
��B
ݘB
ԯB
��B
� B
��B
�WB
��B
��B
�tB
{0B
g�B
m�B
r�B
a�B
[�B
Y�B
[�B
W�B
N�B
;�B
)�B
�B	��B	�5B	�HB	�~B	�rB	�<B	��B	�;B	��B	�NB	�6B	�tB	�uB	��B	��B	�zB	��B	�mB	�^B	.B	r�B	i�B	o�B	n}B	k�B	h�B	n�B	m�B	kB	mCB	o�B	mB	dtB	bhB	Z�B	KxB	:�B	�B��B�B�hB��B�B�&B��B�jB��B��B��B��B��B�>B�mB�hB�@B�'B��B��B�NB�3B�4B�9B�	B�uB|�Bx�Bq'B|�B��B�BzxB~�Bq�Bq[BmwBn�BkkBh�BxRB~BB�4B{Bq�Bb�BY�BV�BYBI�BPHBR:BVBR�BMjBQBS�BW�BR BO�BQNBW�BX�BX�B^�BX_BG�BOBBDBLdBF�BV�B]�Bd�Bm�Bm�BlqBm�Br|ByXB}�B|�B��B�B�RB�KB��B{�B��B|By�Bw2Bo�Bn�BvFB|B~�BB.B~�B~�B~]B~�B�B|�B�[B�mB�{B�{B��B��B��B�B��B��B��B��B��B�B�kB��B�
B�mB�B��B��B��B��B�DB�"B�IB�oB��B�/B��B��B�DB��B��B��B�B�#B�TB�SB�KB�dBߊBߤB��B��B��B��B��B��B�MB�-B�B��B��B�	B�6B�HB	EB	jB	NB	[B	{B	�B	�B	2B	mB	!B	%�B	%B	$ZB	&�B	)�B	.�B	5�B	7�B	8�B	9�B	9�B	:�B	<�B	=�B	<�B	=�B	B�B	F�B	F?B	MB	N"B	QNB	W?B	\CB	\xB	]�B	a�B	g�B	j�B	l�B	o�B	n�B	o�B	o�B	pB	r�B	xB	{B	}<B	�'B	�MB	�?B	�_B	�fB	�lB	�rB	�~B	��B	��B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	��B	�>B	�B	�6B	�KB	�=B	�CB	�OB	�oB	�vB	�?B	�tB	��B	��B	�xB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�B	�B	�VB	� B	�B	�
B	�B	�B	�B	�
B	�B	�B	�9B	�FB	�9B	�SB	�EB	�WB	�=B	�CB	�IB	�dB	�dB	�OB	�OB	�dB	�jB	�pB	�pB	ބB	ޞB	��B	��B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�$B	��B	�B	�*B	�B	�B	�6B	�B	�"B	�B
 4B
 B
 B
 4B
;B
aB
aB
SB
SB
YB
_B
_B
zB
zB

XB
DB
xB
^B

XB
	�B
	�B
�B
�B
�B
pB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!-B
"�B
#�B
$B
#�B
# B
# B
# B
&B
'B
'B
'B
'B
&B
%B
'�B
($B
)*B
*B
*0B
*B
*B
*B
*B
*B
+6B
+B
+6B
*B
)*B
'mB
*B
+B
*B
)*B
)*B
(>B
'RB
*KB
,"B
,"B
,"B
,"B
-B
-CB
,WB
+QB
,WB
,=B
.IB
.IB
./B
.IB
/5B
/OB
0;B
0;B
0;B
0;B
0UB
0;B
0oB
1[B
1AB
1AB
0UB
1[B
1AB
1[B
1vB
2aB
2aB
3MB
4TB
4TB
4TB
4nB
4TB
4nB
3hB
3hB
6FB
6zB
6`B
6`B
6`B
6zB
5tB
6`B
6zB
6zB
7�B
7�B
7�B
7fB
8�B
7�B
6�B
6�B
7�B
9rB
8�B
7�B
9�B
;dB
;dB
;�B
:�B
:�B
:�B
:xB
9rB
8�B
8�B
9�B
:�B
:�B
;�B
;�B
<�B
<�B
<�B
<�B
<�B
<�B
=�B
=�B
=�B
>�B
?�B
?�B
?�B
?�B
>�B
?�B
>�B
@�B
B�B
C�B
B�B
C�B
D�B
D�B
E�B
E�B
D�B
D�B
D�B
E�B
E�B
E�B
G�B
G�B
G�B
G�B
G�B
F�B
E�B
F�B
G�B
H�B
H�B
G�B
G�B
G�B
H�B
H�B
G�B
G�B
G�B
G�B
G�B
I�B
I�B
J�B
J�B
K�B
J�B
I�B
J�B
J�B
J�B
KB
K�B
K�B
K�B
KB
K�B
L�B
M�B
M�B
NB
M�B
N�B
N�B
M�B
OB
N�B
N�B
N�B
PB
PB
O�B
O�B
Q B
P�B
QB
Q B
Q B
Q B
QB
Q�B
RB
R B
QB
R�B
S&B
SB
R B
S&B
TB
TB
TB
TB
TB
T�B
T�B
U2B
T,B
S&B
TB
UB
UB
VB
VB
UB
T,B
U2B
W?B
W$B
W$B
VSB
W?B
X+B
X+B
XB
XEB
X+B
XEB
X+B
Y1B
Y1B
Y1B
YeB
Z7B
[=B
[=B
\]B
\]B
\CB
[qB
\CB
\CB
]IB
]dB
]dB
]IB
]IB
]IB
^OB
^OB
^OB
_VB
_VB
_VB
_pB
_pB
`\B
a|B
abB
`vB
a|B
bhB
bhB
b�B
b�B
b�B
bhB
b�B
c�B
c�B
cnB
c�B
c�B
dtB
e`B
ezB
d�B
e`B
ezB
e`B
ezB
e�B
ezB
ezB
e�B
ezB
ezB
f�B
ffB
f�B
f�B
ffB
f�B
f�B
g�B
g�B
g�B
g�B
g�B
g�B
g�B
hsB
h�B
h�B
h�B
h�B
h�B
h�B
iyB
iyB
i�B
i�B
iyB
iyB
i�B
i�B
i�B
i�B
jB
j�B
i�B
i�B
i�B
j�B
j�B
k�B
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
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
l�B
l�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
r�B
q�B
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
t�B
s�B
tB
t�B
u�B
u�B
u�B
u�B
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
xB
w�1111111111111311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.06(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201708060041442017080600414420170806004144201806221317062018062213170620180622131706201804050719152018040507191520180405071915  JA  ARFMdecpA19c                                                                20170802093512  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20170802003537  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20170802003541  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20170802003542  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20170802003543  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20170802003543  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20170802003544  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20170802003544  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20170802003547  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20170802003548                      G�O�G�O�G�O�                JA  ARUP                                                                        20170802010014                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20170802153312  CV  JULD            G�O�G�O�F�٤                JM  ARGQJMQC2.0                                                                 20170802153312  CV  JULD_LOCATION   G�O�G�O�F�ٺ                JM  ARGQJMQC2.0                                                                 20170802153312  CV  LATITUDE        G�O�G�O�A��                JM  ARGQJMQC2.0                                                                 20170802153312  CV  LONGITUDE       G�O�G�O��$~�                JM  ARSQJMQC2.0                                                                 20170803000000  CF  PSAL_ADJUSTED_QCA�  A�  G�O�                JM  ARSQJMQC2.0                                                                 20170803000000  CF  TEMP_ADJUSTED_QCA�  A�  G�O�                JM  ARCAJMQC2.0                                                                 20170805154144  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20170805154144  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404221915  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622041706  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116221516                      G�O�G�O�G�O�                