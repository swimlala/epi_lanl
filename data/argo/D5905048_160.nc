CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2017-09-16T00:35:16Z creation;2017-09-16T00:35:19Z conversion to V3.1;2019-12-19T07:57:27Z update;     
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
_FillValue                    �<Argo profile    3.1 1.2 19500101000000  20170916003516  20200116221515  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0577_160                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @�&u�> 1   @�&u��� @4���{���d�!�.H�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�  A��A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CO�fCQ�fCT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� DZ��D[� D\  D\� D]  D]� D^  D^�fD_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�|�D�� D�3D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�3D�@ Dƀ D�� D���D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D���D�<�D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�3D�@ 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�G�@�{A ��A
=A?
=A_
=A
=A��A��A��A��AυA߅A�A��BBBBB'B/B7B?BGBOBWB_BgBoBwBB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HC�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�
CQ�
CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RD |)D �)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D	|)D	�)D
|)D
�)D��D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D |)D �)D!|)D!�)D"|)D"�)D#|)D#�)D$|)D$�)D%|)D%�)D&|)D&�)D'|)D'�)D(|)D(�)D)|)D)�)D*|)D*�)D+|)D+�)D,|)D,�)D-|)D-�)D.|)D.�)D/|)D/�)D0|)D0�)D1|)D1�)D2|)D2�)D3|)D3�)D4|)D4�)D5|)D5�)D6|)D6�)D7|)D7�)D8|)D8�)D9|)D9�)D:|)D:�)D;|)D;�)D<|)D<�)D=|)D=�)D>|)D>�)D?|)D?�)D@|)D@�)DA|)DA�)DB|)DB�)DC|)DC�)DD|)DD�)DE|)DE�)DF|)DF�)DG|)DG�)DH|)DH�)DI|)DI�)DJ|)DJ�)DK|)DK�)DL|)DL�)DM|)DM�)DN|)DN�)DO|)DO�)DP|)DP�)DQ|)DQ�)DR|)DR�)DS|)DS�)DT|)DT�)DU|)DU�)DV|)DV�)DW|)DW�)DX|)DX�)DY|)DY�)DZ|)DZ��D[|)D[�)D\|)D\�)D]|)D]�)D^��D^�)D_|)D_�)D`|)D`�)Da|)Da�)Db|)Db�)Dc|)Dc�)Dd|)Dd�)De|)De�)Df|)Df�)Dg|)Dg�)Dh|)Dh�)Di|)Di�)Dj|)Dj�)Dk|)Dk�)Dl|)Dl�)Dm|)Dm�)Dn|)Dn�)Do|)Do�)Dp|)Dp�)Dq|)Dq�)Dr|)Dr�)Ds|)Ds�)Dt|)Dt�)Du|)Du�)Dv|)Dv�)Dw|)Dw�)Dx|)Dx�)Dy|)Dy�)Dz|)Dz�)D{|)D{�)D||)D|�)D}|)D}�)D~|)D~�)D|)D�)D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D�HD�>D�~D��D��D�>D�~D���D��D�>D�z�D��D�HD�AHD�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�z�D���D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�z�D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D¾D��D�>D�~DþD��D�>D�~DľD��D�>D�~DžD�HD�>D�~DƾD���D�>D�~DǾD��D�>D�~DȾD��D�>D�~DɾD��D�>D�~DʾD��D�>D�~D˾D��D�>D�~D̾D��D�>D�~D;D��D�>D�~DξD��D�>D�~DϾD��D�>D�~DоD��D�>D�~DѾD��D�>D�~DҾD��D�>D�~DӾD��D�>D�~DԾD��D�>D�~DվD��D�>D�~D־D��D�>D�~D׾D��D�>D�~DؾD��D�>D�~DپD��D�>D�~DھD��D�>D�~D۾D��D�>D�~DܾD��D�>D�~DݾD��D�>D�~D޾D��D�>D�~D߾D���D�:�D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D��D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��HD�HD�>1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�wA��A�ȴA��A�XA�VA╁A�r�A�n�A�`BA��A�jA�hA�ZA�ȴA�ffA��A߸RA�K�AޅAݧ�A�bNA���A��/A܅Aܝ�A�E�A�ȴA��HAۏ\Aں^A؅A֏\A�C�A�A�x�A���A��A��A΃A͏\A��A�-A���Aʰ!A�1A��A�r�AƼjA���A���A�1A���A��A��A��PA�ffA���A�;dA���A�x�A��A�%A�A��A���A��7A���A���A��A�9XA�G�A���A�dZA���A�z�A���A�+A���A�JA�v�A��mA���A�p�A�A�"�A�;dA��PA�A�A�|�A�bA�1A��A�G�A��A��uA���A�-A��\A���A�t�A�1A�A�%A��A�v�A�?}A�=qA|�`A{��Az~�Ay�7Ay/AxJAvA�Ar��Ap�Ao�AnA�Al��Ak&�AjbNAi�7Ag��Af��AfM�Ac�TAb{A`ZA_?}A^��A^1'A]t�AYS�AR�DAPA�AJbNAG�
AFjAE��ADz�AB(�A=�A;��A:�A9�^A89XA7;dA6��A6�!A5�FA3A2�/A1��A0=qA.�\A-��A,�A+�
A+�PA+p�A+\)A+K�A+�A*�`A*�A(��A(��A(VA'|�A%x�A#��A"I�A �yAA&�A�A �A��AC�A��A  A7LA��A�A�HA��AA��A�A�A��AȴAz�A=qAA�^AoAƨA��AA�-AK�A+A�yA�\A{A��AXA33AhsAhsA
��A	�hA��A�A�A�A�yAz�A�A�HA-AbNAJA�-A�A �9A ffA r�A Z@��F@�@�V@�r�@�1@ꟾ@��`@���@ް!@݉7@ܬ@ۮ@ٲ-@� �@�|�@֗�@�hs@�Z@� �@�n�@�%@�r�@�n�@�@ͩ�@�X@�V@�j@˶F@�+@�o@���@��y@��@�x�@�G�@ȃ@��;@�;d@�5?@���@��
@Õ�@�C�@��y@�E�@��@���@���@���@��D@��@�5?@��^@��^@��/@��w@�v�@��7@��@�bN@��@�|�@�S�@�l�@��@��@�n�@���@���@���@���@��h@��h@�%@�K�@�5?@���@�p�@�7L@��/@�Q�@� �@��m@��@�|�@�S�@�;d@�;d@�C�@�;d@�+@�@��+@�^5@�V@�V@�n�@��!@��@��!@��\@�-@��#@��T@��T@���@�7L@���@�r�@�|�@��@��y@�ȴ@�{@���@��-@�p�@�/@���@��@��@��w@��P@�t�@�l�@�dZ@�K�@��@�ȴ@�^5@�@��^@�7L@��@���@��9@�j@�1@��@�;d@���@��+@�$�@��#@�hs@�7L@��`@���@���@��P@�33@��y@��@�=q@�@���@�G�@��@�%@�%@��@���@��9@�1'@��@��;@��w@��@��@��P@�\)@�;d@���@�$�@��@��^@���@�hs@�X@�&�@���@�Ĝ@���@�9X@�  @��;@�ƨ@�|�@�dZ@�C�@���@���@��\@�^5@�{@��@���@�`B@�V@�Ĝ@���@���@�Q�@���@��@�K�@��R@�{@���@�x�@�O�@��@���@���@���@�bN@�(�@�1@��w@��@��@��R@��\@�{@��@�7L@�&�@�V@��/@���@�r�@�1@��@���@�t�@�
=@��@��R@���@���@�E�@�{@�J@�@��T@���@�G�@��j@�I�@�(�@���@�ƨ@�|�@�K�@�"�@��@�
=@��@�ȴ@��R@�v�@�=q@�-@�$�@��@���@�@��h@�x�@�X@�?}@���@���@��j@���@� �@�1@�1@���@��F@��@�+@��H@��\@�V@�=q@�-@��@��@��^@�`B@��@���@�z�@�I�@��@��@�@��@l�@�@~��@~{@}�T@}@}��@}�@}p�@}`B@}`B@}`B@}�@|��@|(�@{��@{@z�\@z-@y��@yX@y&�@x�`@x��@xĜ@x�u@x �@w�P@vȴ@v��@vv�@v$�@u�@u`B@uO�@u?}@u/@uV@t��@t�@s��@s�
@s�F@sdZ@so@r�\@r�@q��@qhs@qX@p�`@p �@o�@o�P@o;d@o+@o
=@n��@n@n{@m��@m�@mO�@l�@l��@l(�@k�m@k33@j~�@j�@i��@ihs@h�@hA�@h  @g��@f�@fff@f{@e��@e�h@eO�@d�/@d�D@dI�@d(�@cƨ@ct�@cC�@co@b��@b��@b�!@b-@a%@`A�@_�w@_�w@_l�@_;d@_+@_�@_
=@^��@^ȴ@]�@]�h@\�/@\9X@[��@Z�@ZM�@Y��@Yx�@Yhs@Y7L@X��@XQ�@X  @W|�@V�y@V�+@VE�@V@U�@T�@TZ@T(�@S��@S�@SS�@So@S@R��@R��@RM�@Q�^@Qx�@P�9@P�@P1'@P1'@Pb@Pb@P  @O�;@O��@O��@O;d@N�y@N$�@Mp�@L��@L�j@K�m@KC�@J�!@J��@J�\@J~�@J^5@JM�@I��@I��@I�7@I%@Hr�@H �@H �@H �@H �@H �@H �@H  @G��@G|�@G�@F�@F�R@Fv�@FV@F$�@E�T@E�h@E?}@D��@D��@D�@D��@Dz�@D(�@C�m@C�m@C��@CS�@C33@B�H@B�!@B�\@B~�@Bn�@B^5@B^5@B^5@B=q@A�@A�#@A��@A��@Ax�@Ahs@AG�@A�@@��@@A�@@  @?�w@?\)@>��@>�@>v�@>$�@=�@=@=�@=O�@<�/@<Z@<1@;�@;33@:��@:n�@9��@97L@8�u@81'@7�P@7;d@7
=@6��@6$�@5�@5��@5��@5�h@5�h@5�@5�@5�@5p�@5/@4�@4z�@4j@4I�@49X@4(�@41@3t�@3"�@2��@2�!@2-@1�#@1�^@1��@1��@1G�@1&�@0��@0�9@0bN@0Q�@/�w@/l�@/+@/
=@.�y@.ȴ@.�R@.��@.v�@.E�@.$�@-�T@-�-@-�h@-p�@-O�@-?}@-�@-�@,�j@,I�@,�@+��@+�
@+�F@+��@+33@*�!@*^5@*-@)�@)��@)�7@)x�@)x�@)hs@)&�@(��@(�`@(�9@(�@(1'@'�;@'�@'l�@'K�@'+@&��@&�@&�R@&�R@&��@&��@&��@&ff@&@%@%��@%`B@%V@$�@$��@$Z@$�@#�
@#C�@#o@"��@"�@!�^@!��@!G�@!%@ Ĝ@ r�@ bN@ Q�@ 1'@��@|�@K�@+@
=@��@�y@�@ȴ@�+@v�@ff@{@�T@��@?}@�@��@��@�@��@��@z�@I�@9X@��@�
@�F@��@t�@"�@�H@��@��@^5@=q@=q@�@J@�#@�^@��@��@hs@7L@%@��@��@Ĝ@Ĝ@�@�;@��@
=@�R@5?@$�@�h@�@��@��@�D@I�@1@�m@�
@ƨ@��@S�@33@@��@�\@n�@M�@�#@��@x�@G�@��@�`@��@��@  @�w@�P@|�@\)@K�@K�@�@�@��@�+@ff@V@$�@@��@�-@��@�h@`B@�@�@��@�j@��@j@Z@9X@1@�
@�F@��@t�@331111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�wA��A�ȴA��A�XA�VA╁A�r�A�n�A�`BA��A�jA�hA�ZA�ȴA�ffA��A߸RA�K�AޅAݧ�A�bNA���A��/A܅Aܝ�A�E�A�ȴA��HAۏ\Aں^A؅A֏\A�C�A�A�x�A���A��A��A΃A͏\A��A�-A���Aʰ!A�1A��A�r�AƼjA���A���A�1A���A��A��A��PA�ffA���A�;dA���A�x�A��A�%A�A��A���A��7A���A���A��A�9XA�G�A���A�dZA���A�z�A���A�+A���A�JA�v�A��mA���A�p�A�A�"�A�;dA��PA�A�A�|�A�bA�1A��A�G�A��A��uA���A�-A��\A���A�t�A�1A�A�%A��A�v�A�?}A�=qA|�`A{��Az~�Ay�7Ay/AxJAvA�Ar��Ap�Ao�AnA�Al��Ak&�AjbNAi�7Ag��Af��AfM�Ac�TAb{A`ZA_?}A^��A^1'A]t�AYS�AR�DAPA�AJbNAG�
AFjAE��ADz�AB(�A=�A;��A:�A9�^A89XA7;dA6��A6�!A5�FA3A2�/A1��A0=qA.�\A-��A,�A+�
A+�PA+p�A+\)A+K�A+�A*�`A*�A(��A(��A(VA'|�A%x�A#��A"I�A �yAA&�A�A �A��AC�A��A  A7LA��A�A�HA��AA��A�A�A��AȴAz�A=qAA�^AoAƨA��AA�-AK�A+A�yA�\A{A��AXA33AhsAhsA
��A	�hA��A�A�A�A�yAz�A�A�HA-AbNAJA�-A�A �9A ffA r�A Z@��F@�@�V@�r�@�1@ꟾ@��`@���@ް!@݉7@ܬ@ۮ@ٲ-@� �@�|�@֗�@�hs@�Z@� �@�n�@�%@�r�@�n�@�@ͩ�@�X@�V@�j@˶F@�+@�o@���@��y@��@�x�@�G�@ȃ@��;@�;d@�5?@���@��
@Õ�@�C�@��y@�E�@��@���@���@���@��D@��@�5?@��^@��^@��/@��w@�v�@��7@��@�bN@��@�|�@�S�@�l�@��@��@�n�@���@���@���@���@��h@��h@�%@�K�@�5?@���@�p�@�7L@��/@�Q�@� �@��m@��@�|�@�S�@�;d@�;d@�C�@�;d@�+@�@��+@�^5@�V@�V@�n�@��!@��@��!@��\@�-@��#@��T@��T@���@�7L@���@�r�@�|�@��@��y@�ȴ@�{@���@��-@�p�@�/@���@��@��@��w@��P@�t�@�l�@�dZ@�K�@��@�ȴ@�^5@�@��^@�7L@��@���@��9@�j@�1@��@�;d@���@��+@�$�@��#@�hs@�7L@��`@���@���@��P@�33@��y@��@�=q@�@���@�G�@��@�%@�%@��@���@��9@�1'@��@��;@��w@��@��@��P@�\)@�;d@���@�$�@��@��^@���@�hs@�X@�&�@���@�Ĝ@���@�9X@�  @��;@�ƨ@�|�@�dZ@�C�@���@���@��\@�^5@�{@��@���@�`B@�V@�Ĝ@���@���@�Q�@���@��@�K�@��R@�{@���@�x�@�O�@��@���@���@���@�bN@�(�@�1@��w@��@��@��R@��\@�{@��@�7L@�&�@�V@��/@���@�r�@�1@��@���@�t�@�
=@��@��R@���@���@�E�@�{@�J@�@��T@���@�G�@��j@�I�@�(�@���@�ƨ@�|�@�K�@�"�@��@�
=@��@�ȴ@��R@�v�@�=q@�-@�$�@��@���@�@��h@�x�@�X@�?}@���@���@��j@���@� �@�1@�1@���@��F@��@�+@��H@��\@�V@�=q@�-@��@��@��^@�`B@��@���@�z�@�I�@��@��@�@��@l�@�@~��@~{@}�T@}@}��@}�@}p�@}`B@}`B@}`B@}�@|��@|(�@{��@{@z�\@z-@y��@yX@y&�@x�`@x��@xĜ@x�u@x �@w�P@vȴ@v��@vv�@v$�@u�@u`B@uO�@u?}@u/@uV@t��@t�@s��@s�
@s�F@sdZ@so@r�\@r�@q��@qhs@qX@p�`@p �@o�@o�P@o;d@o+@o
=@n��@n@n{@m��@m�@mO�@l�@l��@l(�@k�m@k33@j~�@j�@i��@ihs@h�@hA�@h  @g��@f�@fff@f{@e��@e�h@eO�@d�/@d�D@dI�@d(�@cƨ@ct�@cC�@co@b��@b��@b�!@b-@a%@`A�@_�w@_�w@_l�@_;d@_+@_�@_
=@^��@^ȴ@]�@]�h@\�/@\9X@[��@Z�@ZM�@Y��@Yx�@Yhs@Y7L@X��@XQ�@X  @W|�@V�y@V�+@VE�@V@U�@T�@TZ@T(�@S��@S�@SS�@So@S@R��@R��@RM�@Q�^@Qx�@P�9@P�@P1'@P1'@Pb@Pb@P  @O�;@O��@O��@O;d@N�y@N$�@Mp�@L��@L�j@K�m@KC�@J�!@J��@J�\@J~�@J^5@JM�@I��@I��@I�7@I%@Hr�@H �@H �@H �@H �@H �@H �@H  @G��@G|�@G�@F�@F�R@Fv�@FV@F$�@E�T@E�h@E?}@D��@D��@D�@D��@Dz�@D(�@C�m@C�m@C��@CS�@C33@B�H@B�!@B�\@B~�@Bn�@B^5@B^5@B^5@B=q@A�@A�#@A��@A��@Ax�@Ahs@AG�@A�@@��@@A�@@  @?�w@?\)@>��@>�@>v�@>$�@=�@=@=�@=O�@<�/@<Z@<1@;�@;33@:��@:n�@9��@97L@8�u@81'@7�P@7;d@7
=@6��@6$�@5�@5��@5��@5�h@5�h@5�@5�@5�@5p�@5/@4�@4z�@4j@4I�@49X@4(�@41@3t�@3"�@2��@2�!@2-@1�#@1�^@1��@1��@1G�@1&�@0��@0�9@0bN@0Q�@/�w@/l�@/+@/
=@.�y@.ȴ@.�R@.��@.v�@.E�@.$�@-�T@-�-@-�h@-p�@-O�@-?}@-�@-�@,�j@,I�@,�@+��@+�
@+�F@+��@+33@*�!@*^5@*-@)�@)��@)�7@)x�@)x�@)hs@)&�@(��@(�`@(�9@(�@(1'@'�;@'�@'l�@'K�@'+@&��@&�@&�R@&�R@&��@&��@&��@&ff@&@%@%��@%`B@%V@$�@$��@$Z@$�@#�
@#C�@#o@"��@"�@!�^@!��@!G�@!%@ Ĝ@ r�@ bN@ Q�@ 1'@��@|�@K�@+@
=@��@�y@�@ȴ@�+@v�@ff@{@�T@��@?}@�@��@��@�@��@��@z�@I�@9X@��@�
@�F@��@t�@"�@�H@��@��@^5@=q@=q@�@J@�#@�^@��@��@hs@7L@%@��@��@Ĝ@Ĝ@�@�;@��@
=@�R@5?@$�@�h@�@��@��@�D@I�@1@�m@�
@ƨ@��@S�@33@@��@�\@n�@M�@�#@��@x�@G�@��@�`@��@��@  @�w@�P@|�@\)@K�@K�@�@�@��@�+@ff@V@$�@@��@�-@��@�h@`B@�@�@��@�j@��@j@Z@9X@1@�
@�F@��@t�@331111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
�B
�B
�B
�fB
�TB
�)B
�)B
�)B
�)B
�
B
��B
��B
ƨB
�qB
�9B
�!B
�B
��B
��B
��B
��B
�hB
�3B
��B
��B(�BL�BB�BQ�BF�B6FB#�B�B7LBS�Bo�BgmB~�B�oBÖB�BBB�B�BVB7LBR�BL�B33B  B�B7LBK�B�DB�uB��B��B�uBz�BZBJB�NB�BB��BB  B��B��BiyBP�Bt�B�By�B�B�wB��B�B}�B�BB��B�LB�B��Bp�BcTBP�B:^B.B�B
��B
�B
�/B
��B
�^B
��B
��B
��B
�B
ǮB
ǮB
�!B
��B
�JB
{�B
gmB
VB
M�B
6FB
8RB
1'B
,B
%�B
�B
+B	�B	�fB	�NB	��B	��B	�jB	�dB	�?B	��B	��B	��B	�VB	�B	}�B	x�B	r�B	jB	YB	49B	B�B�B�B�5B�5B��BÖB��B�!B�?B�?B�3B�3B�XB�?B�B��B��B��B��B�oB��B��B��B��B��B��B��B��B��B��B�VB�{B�\B�1B~�B}�B|�Bz�B}�B|�B~�B{�B�B�%B�%B�B~�B}�Bu�Bs�BhsBiyBiyBk�Bo�Bk�Bp�Bo�Bo�Bm�Bk�BffB^5BbNBffBn�Bx�B{�B{�B{�By�Bx�B~�B~�B�B�B�B�DB�uB��B��B��B��B�hB�VB�7Bw�B~�B�PB�\B�oB�hB�oB��B��B��B�uB�7B�1B�%Bx�BiyBdZBgmBk�Bo�Bz�B|�B}�B�B�B�B}�B�B{�Bz�B|�Bw�B�B�%B�7B�DB�DB�PB�oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B��B�B�!B�?B�^B�XB�^B�}BɺB��B��B��B��B��B��B�B�/B�;B�`B�mB�B�B�B�B��B��B��B��B��B��B�B��B��B��B��B��B	  B	B	%B	+B	
=B	PB	bB	�B	�B	�B	�B	�B	�B	$�B	%�B	'�B	+B	2-B	5?B	5?B	7LB	7LB	9XB	>wB	D�B	F�B	G�B	H�B	I�B	K�B	S�B	W
B	YB	XB	]/B	^5B	_;B	`BB	`BB	`BB	cTB	ffB	hsB	jB	l�B	l�B	l�B	l�B	o�B	q�B	t�B	u�B	v�B	z�B	}�B	~�B	}�B	~�B	�B	�B	�1B	�7B	�PB	�hB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�3B	�9B	�?B	�?B	�LB	�^B	�^B	�wB	�wB	��B	��B	��B	��B	��B	��B	��B	��B	ƨB	ȴB	��B	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�B	�B	�B	�)B	�5B	�;B	�5B	�5B	�;B	�;B	�;B	�NB	�ZB	�mB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
  B
  B
B
B
B
B
%B
+B
1B
	7B
	7B
	7B
	7B
	7B
	7B
1B
	7B

=B

=B
	7B
	7B
	7B
DB
JB
JB
DB
JB
JB
JB
DB
PB
VB
VB
PB
PB
JB
PB
VB
VB
\B
bB
\B
\B
VB
VB
VB
VB
\B
hB
oB
oB
{B
{B
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
!�B
!�B
!�B
!�B
"�B
#�B
$�B
#�B
#�B
$�B
%�B
$�B
#�B
$�B
%�B
%�B
$�B
&�B
&�B
'�B
'�B
'�B
&�B
'�B
(�B
(�B
(�B
(�B
)�B
+B
+B
+B
+B
)�B
'�B
)�B
+B
.B
.B
/B
0!B
/B
/B
/B
.B
.B
/B
.B
.B
.B
-B
.B
/B
1'B
1'B
1'B
0!B
1'B
1'B
2-B
2-B
33B
49B
49B
49B
49B
5?B
7LB
7LB
7LB
8RB
8RB
8RB
8RB
7LB
7LB
7LB
8RB
8RB
:^B
:^B
<jB
<jB
<jB
<jB
<jB
<jB
<jB
;dB
<jB
;dB
;dB
=qB
=qB
<jB
=qB
>wB
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
?}B
@�B
B�B
C�B
C�B
C�B
C�B
B�B
B�B
A�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
E�B
D�B
E�B
E�B
E�B
E�B
G�B
F�B
F�B
G�B
G�B
G�B
H�B
H�B
H�B
I�B
I�B
I�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
H�B
I�B
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
L�B
L�B
M�B
M�B
M�B
M�B
M�B
N�B
O�B
O�B
P�B
Q�B
Q�B
Q�B
R�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
S�B
S�B
R�B
T�B
T�B
T�B
VB
VB
T�B
T�B
T�B
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
XB
YB
XB
YB
YB
ZB
ZB
ZB
[#B
ZB
ZB
ZB
[#B
ZB
[#B
[#B
[#B
\)B
\)B
\)B
\)B
[#B
[#B
\)B
]/B
]/B
]/B
]/B
\)B
\)B
]/B
^5B
^5B
_;B
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
`BB
`BB
aHB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
aHB
aHB
aHB
aHB
bNB
bNB
aHB
bNB
bNB
bNB
cTB
cTB
bNB
cTB
cTB
cTB
dZB
e`B
e`B
dZB
e`B
e`B
ffB
ffB
e`B
e`B
e`B
ffB
gmB
hsB
hsB
hsB
hsB
hsB
gmB
hsB
hsB
gmB
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
jB
iyB
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
l�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
l�B
l�B
k�B
l�B
l�B
m�B
m�B
n�B
m�B
n�B
p�B
o�B
o�B
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
r�B
r�B
q�B
s�B
s�B
s�B
s�B
t�B
t�B
s�B
s�B
t�B
u�B
u�B
u�B
v�B
v�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
z�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
��B
��B
��B
�B
��B
��B
�]B
�CB
�]B
רB
ѝB
�0B
�EB
��B
�B
��B
��B
�B
�zB
�#B
�$B
��B
�hB
��B
�HB'�BM�BDMBR�BI7B:�B(
B �B9�BUMBp�Bi_B��B��BŢB�B�B#B�B�B9XBT�BO�B9XB[BkB8RBM�B�PB��B��B��B��BcBaHB�B�RB�FB˒B�BB�,B��Bl�BS&BvB��B|6B� B�OB�TB�oB}�B��BևB�B��B�Bt9Bd�BR�B;�B/5B�B;B
�-B
ߊB
�SB
�B
�0B
��B
�B
�B
��B
��B
��B
��B
��B
~�B
j�B
YB
P}B
:B
9�B
2�B
-)B
&�B
xB
	�B	�B	��B	�B	�B	�~B	��B	��B	��B	��B	�B	�-B	�4B	�SB	�B	zDB	s�B	k�B	[qB	9�B		�B��B�\B�B�BߊB�MB�+B�fB��B��B��B�B�nB��B�+B��B� B�,B�NB��B��B��B��B��B�B��B��B��B�B�WB��B��B��B�bB��B��B�OB~�B|�BHB}�B�B|�B��B��B��B�GB�4B~�BwfBu?BkBkBkBlqBp!BlqBqBp;Bp!BnBl=Bg�B`Bc�BgmBo5ByrB|6B|jB|�Bz�By�BcB.B�B��B�MB��B�,B��B��B��B��B�:B��B�DB{0B�B��B��B��B�oB�B��B�5B��B�gB��B�xB��B|�BmCBf�Bh�BlqBpoB{�B~]B~�B��B��B��B~�B��B}<B{�B}�Bx�B�uB�tB��B��B��B��B��B��B��B��B�]B�!B� B�\B�\B�bB��B��B�bB�0B�KB�kB��B�]B�oB�tB��B�*B�0B�iB�	B�BΥB͹B��B�pB�{B�yB�~BߤB�zB�mB�B�B� B��B�B�B�B�B�B�fB��B�tB�PB�<B�.B�]B	 �B	mB	tB	zB	
rB	jB	}B	�B	�B	�B	�B	�B	!B	$�B	%�B	'�B	+B	2B	5?B	5tB	7�B	7�B	9�B	>�B	D�B	F�B	HB	IB	J=B	L�B	TFB	WYB	YKB	X�B	]~B	^�B	_pB	`vB	`�B	`�B	c�B	f�B	h�B	j�B	l�B	l�B	l�B	l�B	o�B	q�B	uB	vB	wLB	{B	~B	.B	~BB	HB	�uB	��B	�fB	��B	��B	��B	��B	��B	��B	��B	�7B	�B	�B	�B	�$B	�XB	�QB	�qB	�iB	�hB	�TB	�?B	�tB	�fB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	��B	��B	��B	��B	��B	��B	�B	�B	�B	� B	� B	�4B	�&B	�&B	�&B	�FB	�9B	�?B	�?B	�+B	�EB	�eB	چB	�xB	�OB	�VB	ބB	ޞB	ߊB	ߊB	߾B	��B	��B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	�B	��B	��B	�B	�B	�B	��B	��B	��B	�	B	�B	�B	�DB	�B	�B	�"B	�PB	�B	�B	�B	�.B	�HB
;B
'B
'B
;B
;B
 OB
 iB
oB
MB
SB
SB
YB
_B
fB
	RB
	RB
	lB
	lB
	RB
	�B
fB
	RB

=B

XB
	RB
	lB
	�B
^B
~B
~B
xB
dB
dB
dB
�B
�B
VB
�B
�B
�B
�B
�B
�B
�B
vB
�B
vB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
B
�B
�B
�B
B
�B
�B
�B
 �B
 �B
 B
�B
"�B
!�B
!�B
!�B
"B
# B
$B
%B
$&B
$&B
%B
%�B
%B
$&B
%B
%�B
&B
%,B
'8B
'B
(
B
(
B
($B
'B
(
B
)B
)B
)*B
)*B
*B
+6B
+6B
+B
+B
*KB
(sB
*KB
+6B
./B
./B
/OB
0!B
/5B
/5B
/OB
.cB
.}B
/OB
.cB
.cB
.IB
-wB
.cB
/OB
1[B
1AB
1AB
0oB
1AB
1[B
2aB
2aB
3hB
4nB
4TB
4nB
4nB
5�B
7�B
7�B
7�B
8�B
8�B
8lB
8lB
7fB
7�B
7�B
8lB
8�B
:�B
:�B
<jB
<�B
<jB
<jB
<�B
<�B
<�B
;�B
<�B
;�B
;�B
=�B
=�B
<�B
=�B
>�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
?�B
@�B
B�B
C�B
C�B
C�B
C�B
B�B
B�B
A�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
E�B
D�B
E�B
E�B
E�B
E�B
G�B
F�B
F�B
G�B
G�B
G�B
H�B
H�B
H�B
I�B
I�B
I�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
H�B
I�B
J�B
J�B
KB
J�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
MB
K�B
L�B
MB
M�B
N"B
N"B
NB
N"B
OB
PB
PB
QB
R B
R:B
R B
S&B
TB
T,B
T�B
T�B
UB
T�B
T�B
TB
TB
S&B
U2B
UB
UB
VB
V9B
U2B
UMB
UB
V9B
VB
V9B
W$B
X+B
X+B
X+B
XEB
X+B
X+B
X+B
X+B
Y1B
XEB
YKB
Y1B
ZQB
Z7B
Z7B
[#B
Z7B
Z7B
ZQB
[=B
Z7B
[=B
[=B
[WB
\]B
\)B
\]B
\CB
[WB
[WB
\CB
]dB
]IB
]IB
]dB
\]B
\]B
]IB
^OB
^OB
_VB
^OB
_;B
_;B
_VB
_pB
_pB
_pB
_VB
_VB
_VB
_pB
`\B
`\B
a|B
abB
abB
abB
a|B
bhB
bhB
bNB
abB
a|B
a|B
abB
b�B
bhB
abB
bhB
b�B
bhB
c�B
cnB
b�B
c�B
c�B
c�B
dtB
ezB
e�B
dtB
ezB
e�B
ffB
f�B
ezB
e�B
ezB
f�B
g�B
h�B
h�B
hsB
h�B
hsB
g�B
h�B
h�B
g�B
h�B
h�B
h�B
i�B
i�B
i�B
i�B
iyB
iyB
i�B
i�B
jB
i�B
j�B
j�B
j�B
j�B
j�B
j�B
k�B
k�B
k�B
k�B
l�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
l�B
l�B
k�B
l�B
l�B
m�B
m�B
n�B
m�B
n�B
p�B
o�B
o�B
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
r�B
r�B
q�B
s�B
s�B
s�B
s�B
t�B
t�B
s�B
s�B
t�B
u�B
u�B
u�B
v�B
v�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
x�B
y	B
y	B
y�B
y�B
y�B
z�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.06(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201709250057452017092500574520170925005745201806221319142018062213191420180622131914201804050721422018040507214220180405072142  JA  ARFMdecpA19c                                                                20170916093506  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20170916003516  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20170916003518  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20170916003518  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20170916003519  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20170916003519  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20170916003519  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20170916003519  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20170916003519  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20170916003519                      G�O�G�O�G�O�                JA  ARUP                                                                        20170916005553                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20170916153453  CV  JULD            G�O�G�O�F�3�                JM  ARCAJMQC2.0                                                                 20170924155745  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20170924155745  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404222142  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622041914  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116221515                      G�O�G�O�G�O�                