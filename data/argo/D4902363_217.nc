CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-03-09T00:35:21Z creation;2018-03-09T00:35:25Z conversion to V3.1;2019-12-19T07:47:34Z update;     
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
_FillValue                    �<Argo profile    3.1 1.2 19500101000000  20180309003521  20200115121518  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0576_217                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�Q��'�1   @�Q��9 @:52a|��dg��[1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A���B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DGy�DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D}��D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�3D�C3D�|�D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�<�D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�|�D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�fD�6f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@|(�@�{@�{A
=A?
=A_
=A
=A��A��A��A��AυA߅A�Q�A��BBBBB'B/B7B?BGBOBWB_BgBoBwBB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB�{B��HB��HB��HB��HB��HB��HB��HB��HB��HC�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RD |)D �)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D	|)D	�)D
|)D
�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D |)D �)D!|)D!�)D"|)D"�)D#|)D#�)D$|)D$�)D%|)D%�)D&|)D&�)D'|)D'�)D(|)D(�)D)|)D)�)D*|)D*�)D+|)D+�)D,|)D,�)D-|)D-�)D.|)D.�)D/|)D/�)D0|)D0�)D1|)D1�)D2|)D2�)D3|)D3�)D4|)D4�)D5|)D5�)D6|)D6�)D7|)D7�)D8|)D8�)D9|)D9�)D:|)D:�)D;|)D;�)D<|)D<�)D=|)D=�)D>|)D>�)D?|)D?�)D@|)D@�)DA|)DA�)DB|)DB�)DC|)DC�)DD|)DD�)DE|)DE�)DF|)DF�)DGu�DG�)DH|)DH�)DI|)DI�)DJ|)DJ�)DK|)DK�)DL|)DL�)DM|)DM�)DN|)DN�)DO|)DO�)DP|)DP�)DQ|)DQ�)DR|)DR�)DS|)DS�)DT|)DT�)DU|)DU�)DV|)DV�)DW|)DW�)DX|)DX�)DY|)DY�)DZ|)DZ�)D[|)D[�)D\|)D\�)D]|)D]�)D^|)D^�)D_|)D_�)D`|)D`�)Da|)Da�)Db|)Db�)Dc|)Dc�)Dd|)Dd�)De|)De�)Df|)Df�)Dg|)Dg�)Dh|)Dh�)Di|)Di�)Dj|)Dj�)Dk|)Dk�)Dl|)Dl�)Dm|)Dm�)Dn|)Dn�)Do|)Do�)Dp|)Dp�)Dq|)Dq�)Dr|)Dr�)Ds|)Ds�)Dt|)Dt�)Du|)Du�)Dv|)Dv�)Dw|)Dw�)Dx|)Dx�)Dy|)Dy�)Dz|)Dz�)D{|)D{�)D||)D|�)D}|)D}��D~|)D~�)D|)D�)D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D��HD��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D�HD�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D¾D��D�>D�~DþD��D�>D�~DľD��D�>D�~DžD��D�>D�~DƾD��D�>D�~DǾD��D�>D�~DȾD��D�>D�~DɾD��D�>D�~DʾD��D�>D�~D˾D��D�>D�~D̾D��D�>D�~D;D��D�>D�~DξD��D�>D�~DϾD��D�>D�~DоD��D�>D�~DѾD��D�>D�~DҾD��D�>D�~DӾD��D�>D�~DԾD��D�>D�~DվD��D�>D�~D־D��D�>D�~D׾D�HD�AHD�z�DؾD��D�>D�~DپD��D�>D�~DھD��D�>D�~D۾D��D�>D�~DܾD��D�:�D�~DݾD��D�>D�~D޾D��D�>D�~D߾D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�z�D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D��D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D�{D�4{1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�E�A�E�A�E�A�E�A�E�A�E�A�E�A�C�A�C�A�C�A�E�A�G�A�C�A�C�A�G�A�I�A�I�A�A�A�=qA�9XA�;dA�;dA�A�A�?}A�A�A�A�A�G�A�G�A�I�A�E�A�K�A�K�A��A��yA��/A���A��A�A�-A���A�;dA���A�jA�jA�K�A�{A��A�I�A�5?A� �A��A��DA��jA��hA��A���A��A���A��A��;A�"�A���A���A�hsA�M�A��A�XA�1'A�ĜA�E�A�`BA�`BA�A�A��A�A��yA��7A��A�`BA�bA��wA�r�A�%A���A�t�A��A��A|$�Az�uAy��Aw�hAr�Ap��ApA�ApJAo�-Ann�Am%Al��Ak�-Aj�Ai%Afr�Ad�\Ac`BAb�9Aa;dA`�\A`�\A`~�A`r�A_C�A^Q�A]XA\ZAZ1'AX=qAW7LAV�+AUG�AT~�AT  ASARbNAQ��AQVAP�RAP�DAPI�AO��AO%AM�;AM��AL��AK33AJVAIƨAH�AG�AF��AF�AE�wAE�hAD�ADbNAD�ACp�AC%AB�uAA+A@M�A?S�A>�/A>��A>{A=x�A<��A;hsA;"�A;VA:�A:�A8ȴA7�A7"�A7A6�/A6��A5�#A5&�A4{A37LA2��A2ffA1�A1/A/l�A/G�A//A.�yA.-A-��A,�A+|�A*1A(v�A&9XA$�+A#t�A"��A!��A ��A��AXA%A�AM�A�A�jAffA5?A�A��A��A�^A��A�AoA�!A�AA��A�A�AZA��A�mA��AĜAjA��A��A
v�A	��A��A9XA�;A��A|�A7LA~�A1'A�A�A��A�9A��A+A ��A ZA �@��@���@�|�@�E�@�  @�@�(�@��-@� �@�"�@�x�@�A�@�C�@�?}@��
@�-@��`@��;@�~�@�7@�9@�F@�/@�z�@�33@ܴ9@ڸR@���@��@�S�@�/@�  @ӶF@ӥ�@�
=@�hs@϶F@�C�@��@Χ�@�V@��@�&�@�1@�@�{@��#@�7L@���@�o@���@�~�@��@°!@���@���@�Z@�1'@��
@�\)@�~�@�~�@�{@�Ĝ@�|�@��`@��P@��H@�v�@��@��j@��9@�Q�@��F@�@�v�@�V@��@��^@���@��;@���@�|�@�S�@���@��@���@��D@�Q�@���@��@�O�@�z�@��w@�\)@���@��^@��h@�`B@�7L@�%@���@��@�1'@�b@���@���@���@��P@�t�@�C�@�
=@��@�ff@�V@��7@��j@�  @��w@�K�@�
=@��@�{@�hs@�?}@��`@��
@�o@���@�v�@�v�@�M�@���@�x�@�?}@��@���@��`@�r�@���@�ff@��/@���@�
=@��R@�n�@�^5@�-@�$�@��@���@�Q�@�1'@�(�@���@�33@��H@���@�=q@�@���@�Z@�(�@�  @��
@��w@��@�+@�~�@�@��@�?}@��9@��j@�Ĝ@��@�A�@��@���@�C�@�
=@�ȴ@���@��!@���@�"�@�@��h@�x�@���@�Ĝ@�A�@��@�  @�ƨ@���@�33@��@��y@���@��@���@�$�@��-@���@��-@��-@���@��h@���@��h@�x�@�p�@�X@��@��@��9@�r�@�I�@�1@
=@~��@~�+@~ff@~{@}V@|9X@{�@{33@{"�@z�H@z�\@y�7@y�@x��@x�9@xQ�@w��@w�w@w�@w��@wl�@w\)@w\)@w\)@w;d@v�R@vE�@v5?@v$�@v@u�-@t�/@r�H@rJ@q�@q��@q��@q7L@p��@p��@p�`@p �@o|�@oK�@o;d@n��@nv�@nV@nE�@n5?@n5?@n5?@n$�@n{@m�@m��@l�/@lj@k��@k�F@k��@k@j��@j^5@j=q@i��@i�^@i��@i��@ihs@h��@hbN@h1'@h  @g�;@g��@g��@gl�@f�@fv�@e�@dz�@d(�@c�m@c��@ct�@cS�@co@b�\@a��@a7L@`��@`r�@_|�@^�y@^�+@^V@]@]p�@]p�@]?}@\��@[�m@[33@Z�H@Z�!@Zn�@Z=q@Y��@Yhs@X��@X�`@X�9@X �@W|�@W
=@W
=@V�y@V�+@U�@U@U��@T�D@T1@Sƨ@SS�@R��@R��@R~�@RM�@R�@Q�@P�9@P1'@O;d@N��@Nv�@NE�@N{@M�@L�@L�@L(�@K"�@J��@J~�@JM�@JJ@I�^@I�7@Ihs@IX@IG�@I7L@H�`@HQ�@H �@G��@G�P@G\)@F�@F�+@F5?@E�@E@E��@E�h@E`B@E/@D�@D�j@D��@D�D@Dz�@DZ@D(�@C�F@C�@C@B^5@B=q@B-@BJ@A�@A��@A��@AX@@��@@�9@@�u@@�@@bN@?�;@?��@?�P@?l�@?+@>�@>��@>$�@=�h@=O�@<�@<Z@<I�@<I�@<(�@;��@;�
@;ƨ@;t�@;33@:^5@9�^@9�^@9��@9hs@9�@8Ĝ@8�@8 �@7��@7�P@7l�@7\)@7K�@6�@6v�@6ff@6$�@5��@5�-@5p�@5V@4��@4�D@4z�@49X@3��@3t�@3@2�!@2n�@2^5@2M�@2-@2J@1�@1�@1�#@1�^@1X@1&�@1&�@1�@0��@0�u@0bN@0 �@/�P@/l�@/;d@/�@.��@.�y@.�R@.ff@.E�@.5?@.$�@.@-@-�-@-�h@-p�@-?}@,��@,��@,�@,�@+dZ@+dZ@+dZ@+dZ@+t�@+dZ@+S�@+S�@+S�@+o@*�@*��@*=q@)��@)�7@)%@(��@(Ĝ@(�u@(bN@( �@'��@&�y@&E�@&@%@%��@%`B@%`B@%?}@$�/@$�j@$j@$9X@$1@#��@#��@#��@#��@"�@"��@"�H@"��@"~�@!�#@!��@!��@!��@!��@!�7@!G�@ ��@ �9@ �u@ r�@ A�@   @�@�@��@��@��@+@ȴ@v�@V@$�@@��@?}@V@�@�@�j@�D@j@Z@Z@I�@1@ƨ@ƨ@�F@��@��@�@t�@33@@��@~�@M�@��@�^@��@�7@x�@x�@x�@7L@�@%@��@Ĝ@�@  @�w@�@�@�@��@l�@K�@+@
=@��@��@ff@V@5?@@��@@��@��@�h@�@��@z�@�@1@1@ƨ@S�@"�@@��@�\@~�@~�@�\@~�@~�@^5@=q@J@�^@7L@hs@hs@hs@&�@Ĝ@�@1'@Q�@bN@  @�@�w@�P@|�@\)@K�@��@�R@��@��@ff@5?@{@{@@@�@�T@@��@p�@��@j@(�@��@�
@��@dZ@"�@
�H@
��@
�!@
�\@
M�@	��@	��@	�7@	X@	G�@	7L@	�@��@Ĝ@��@�u@bN@1'@�;@�;@��@�w@��@K�@
=@��@V@�T@�-@�h@p�@O�@V@��@�j@�@�D@�D@z�@Z@I�@1@ƨ@��@t�@t�@dZ@dZ@C�@o@@�H@�H@�H@��@~�@M�@M�@M�@-@�#@��@x�@G�@&�@%@ �`@ Ĝ@ bN@ 1'@ A�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�E�A�E�A�E�A�E�A�E�A�E�A�E�A�C�A�C�A�C�A�E�A�G�A�C�A�C�A�G�A�I�A�I�A�A�A�=qA�9XA�;dA�;dA�A�A�?}A�A�A�A�A�G�A�G�A�I�A�E�A�K�A�K�A��A��yA��/A���A��A�A�-A���A�;dA���A�jA�jA�K�A�{A��A�I�A�5?A� �A��A��DA��jA��hA��A���A��A���A��A��;A�"�A���A���A�hsA�M�A��A�XA�1'A�ĜA�E�A�`BA�`BA�A�A��A�A��yA��7A��A�`BA�bA��wA�r�A�%A���A�t�A��A��A|$�Az�uAy��Aw�hAr�Ap��ApA�ApJAo�-Ann�Am%Al��Ak�-Aj�Ai%Afr�Ad�\Ac`BAb�9Aa;dA`�\A`�\A`~�A`r�A_C�A^Q�A]XA\ZAZ1'AX=qAW7LAV�+AUG�AT~�AT  ASARbNAQ��AQVAP�RAP�DAPI�AO��AO%AM�;AM��AL��AK33AJVAIƨAH�AG�AF��AF�AE�wAE�hAD�ADbNAD�ACp�AC%AB�uAA+A@M�A?S�A>�/A>��A>{A=x�A<��A;hsA;"�A;VA:�A:�A8ȴA7�A7"�A7A6�/A6��A5�#A5&�A4{A37LA2��A2ffA1�A1/A/l�A/G�A//A.�yA.-A-��A,�A+|�A*1A(v�A&9XA$�+A#t�A"��A!��A ��A��AXA%A�AM�A�A�jAffA5?A�A��A��A�^A��A�AoA�!A�AA��A�A�AZA��A�mA��AĜAjA��A��A
v�A	��A��A9XA�;A��A|�A7LA~�A1'A�A�A��A�9A��A+A ��A ZA �@��@���@�|�@�E�@�  @�@�(�@��-@� �@�"�@�x�@�A�@�C�@�?}@��
@�-@��`@��;@�~�@�7@�9@�F@�/@�z�@�33@ܴ9@ڸR@���@��@�S�@�/@�  @ӶF@ӥ�@�
=@�hs@϶F@�C�@��@Χ�@�V@��@�&�@�1@�@�{@��#@�7L@���@�o@���@�~�@��@°!@���@���@�Z@�1'@��
@�\)@�~�@�~�@�{@�Ĝ@�|�@��`@��P@��H@�v�@��@��j@��9@�Q�@��F@�@�v�@�V@��@��^@���@��;@���@�|�@�S�@���@��@���@��D@�Q�@���@��@�O�@�z�@��w@�\)@���@��^@��h@�`B@�7L@�%@���@��@�1'@�b@���@���@���@��P@�t�@�C�@�
=@��@�ff@�V@��7@��j@�  @��w@�K�@�
=@��@�{@�hs@�?}@��`@��
@�o@���@�v�@�v�@�M�@���@�x�@�?}@��@���@��`@�r�@���@�ff@��/@���@�
=@��R@�n�@�^5@�-@�$�@��@���@�Q�@�1'@�(�@���@�33@��H@���@�=q@�@���@�Z@�(�@�  @��
@��w@��@�+@�~�@�@��@�?}@��9@��j@�Ĝ@��@�A�@��@���@�C�@�
=@�ȴ@���@��!@���@�"�@�@��h@�x�@���@�Ĝ@�A�@��@�  @�ƨ@���@�33@��@��y@���@��@���@�$�@��-@���@��-@��-@���@��h@���@��h@�x�@�p�@�X@��@��@��9@�r�@�I�@�1@
=@~��@~�+@~ff@~{@}V@|9X@{�@{33@{"�@z�H@z�\@y�7@y�@x��@x�9@xQ�@w��@w�w@w�@w��@wl�@w\)@w\)@w\)@w;d@v�R@vE�@v5?@v$�@v@u�-@t�/@r�H@rJ@q�@q��@q��@q7L@p��@p��@p�`@p �@o|�@oK�@o;d@n��@nv�@nV@nE�@n5?@n5?@n5?@n$�@n{@m�@m��@l�/@lj@k��@k�F@k��@k@j��@j^5@j=q@i��@i�^@i��@i��@ihs@h��@hbN@h1'@h  @g�;@g��@g��@gl�@f�@fv�@e�@dz�@d(�@c�m@c��@ct�@cS�@co@b�\@a��@a7L@`��@`r�@_|�@^�y@^�+@^V@]@]p�@]p�@]?}@\��@[�m@[33@Z�H@Z�!@Zn�@Z=q@Y��@Yhs@X��@X�`@X�9@X �@W|�@W
=@W
=@V�y@V�+@U�@U@U��@T�D@T1@Sƨ@SS�@R��@R��@R~�@RM�@R�@Q�@P�9@P1'@O;d@N��@Nv�@NE�@N{@M�@L�@L�@L(�@K"�@J��@J~�@JM�@JJ@I�^@I�7@Ihs@IX@IG�@I7L@H�`@HQ�@H �@G��@G�P@G\)@F�@F�+@F5?@E�@E@E��@E�h@E`B@E/@D�@D�j@D��@D�D@Dz�@DZ@D(�@C�F@C�@C@B^5@B=q@B-@BJ@A�@A��@A��@AX@@��@@�9@@�u@@�@@bN@?�;@?��@?�P@?l�@?+@>�@>��@>$�@=�h@=O�@<�@<Z@<I�@<I�@<(�@;��@;�
@;ƨ@;t�@;33@:^5@9�^@9�^@9��@9hs@9�@8Ĝ@8�@8 �@7��@7�P@7l�@7\)@7K�@6�@6v�@6ff@6$�@5��@5�-@5p�@5V@4��@4�D@4z�@49X@3��@3t�@3@2�!@2n�@2^5@2M�@2-@2J@1�@1�@1�#@1�^@1X@1&�@1&�@1�@0��@0�u@0bN@0 �@/�P@/l�@/;d@/�@.��@.�y@.�R@.ff@.E�@.5?@.$�@.@-@-�-@-�h@-p�@-?}@,��@,��@,�@,�@+dZ@+dZ@+dZ@+dZ@+t�@+dZ@+S�@+S�@+S�@+o@*�@*��@*=q@)��@)�7@)%@(��@(Ĝ@(�u@(bN@( �@'��@&�y@&E�@&@%@%��@%`B@%`B@%?}@$�/@$�j@$j@$9X@$1@#��@#��@#��@#��@"�@"��@"�H@"��@"~�@!�#@!��@!��@!��@!��@!�7@!G�@ ��@ �9@ �u@ r�@ A�@   @�@�@��@��@��@+@ȴ@v�@V@$�@@��@?}@V@�@�@�j@�D@j@Z@Z@I�@1@ƨ@ƨ@�F@��@��@�@t�@33@@��@~�@M�@��@�^@��@�7@x�@x�@x�@7L@�@%@��@Ĝ@�@  @�w@�@�@�@��@l�@K�@+@
=@��@��@ff@V@5?@@��@@��@��@�h@�@��@z�@�@1@1@ƨ@S�@"�@@��@�\@~�@~�@�\@~�@~�@^5@=q@J@�^@7L@hs@hs@hs@&�@Ĝ@�@1'@Q�@bN@  @�@�w@�P@|�@\)@K�@��@�R@��@��@ff@5?@{@{@@@�@�T@@��@p�@��@j@(�@��@�
@��@dZ@"�@
�H@
��@
�!@
�\@
M�@	��@	��@	�7@	X@	G�@	7L@	�@��@Ĝ@��@�u@bN@1'@�;@�;@��@�w@��@K�@
=@��@V@�T@�-@�h@p�@O�@V@��@�j@�@�D@�D@z�@Z@I�@1@ƨ@��@t�@t�@dZ@dZ@C�@o@@�H@�H@�H@��@~�@M�@M�@M�@-@�#@��@x�@G�@&�@%@ �`@ Ĝ@ bN@ 1'@ A�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B� Bz�Bz�B�B�By�B}�B�+B�}B�jB��B�1Bz�B�PB��B�\B�\B�bB�=B� Bm�BP�B;dB'�B�B�`BDB��B�B��B�jB��Bm�B!�B�BhB
�BB
��B
�B
�B
��B
�XB
��B
��B
��B
��B
|�B
�oB
��B
�uB
�VB
�%B
|�B
{�B
hsB
D�B
 �B
(�B
.B
+B	�;B	�mB
B
B	��B	�B	�/B	�B	�B	��B	�jB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�%B	x�B	t�B	hsB	O�B	M�B	S�B	VB	N�B	M�B	P�B	F�B	E�B	F�B	?}B	C�B	C�B	>wB	6FB	0!B	!�B	-B	�B	�B	�B	�B	hB	bB	
=B	{B	{B	{B	VB	
=B	VB	+B	%B	B�B�B��B��B��B��B�B�B�HB�B�B�B�TB��B�B�B�TB�;B�#B��BǮBŢB��BǮBÖB�dB�XB��B�}B�wB�LB�B��B��B�=B� B{�BiyBp�By�B{�Bw�Bq�Bp�B� B|�Bz�Bw�Bo�Bp�Bv�Bx�Bt�Bo�BffBVB]/BW
B_;BaHBaHBVBN�BK�BA�BJ�B:^B@�BA�BK�BF�B>wB)�B�B0!B1'B;dB>wB>wB>wB:^B/B6FB0!B'�B�B�B#�B+B0!B.B2-B33B2-B,B �B�BhBuBPBuB�BbB�BuB	7BPBJBVBbB\B\BhB
=B��BPB+B��BBhBJB%BBuB�B�B�BPBPB"�B$�B$�B"�B�B�B�B�B�B!�B!�B�B �B%�B"�B�BJB �B'�B+B/B-B+B'�B.B(�B�B�B�B'�B1'B5?B1'B=qB@�B=qB:^B<jB=qBB�BB�B@�B?}B?}BI�BI�BG�BB�B=qBN�BP�BN�BJ�BH�BC�BP�BT�BXBW
BS�BaHB`BB`BB`BBaHB`BBaHBdZBdZBe`BgmBgmBffBffBffBgmBffBjBhsBiyBl�Bs�Br�Bt�Bs�Bo�Bs�By�Bw�Bv�B|�B�B�DB�VB�VB�PB�{B��B��B��B��B��B�{B��B��B��B��B�B�B�3B�-B�3B�-B�B��B�jB�qB�^B�RB��B��B��BÖBŢB��B��B�
B�B�
B�B��B��B�B�5B�TB�ZB�B�B�B�B�B�B�B��B��B��B��B��B��B��B	B	+B	B		7B	
=B	uB	�B	�B	�B	�B	#�B	+B	-B	2-B	1'B	0!B	:^B	?}B	A�B	B�B	B�B	B�B	D�B	E�B	E�B	H�B	H�B	I�B	L�B	N�B	P�B	S�B	VB	VB	\)B	^5B	^5B	]/B	[#B	^5B	aHB	ffB	hsB	gmB	hsB	gmB	n�B	r�B	t�B	t�B	w�B	{�B	|�B	{�B	z�B	{�B	|�B	|�B	|�B	~�B	�B	�%B	�%B	�B	�B	� B	}�B	�B	�JB	�PB	�PB	�PB	�bB	�hB	�hB	�bB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�9B	�?B	�FB	�RB	�RB	�RB	�RB	�jB	��B	B	ĜB	ĜB	ĜB	ŢB	ĜB	ƨB	ƨB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�)B	�;B	�BB	�;B	�NB	�ZB	�NB	�HB	�BB	�ZB	�mB	�yB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B	��B	��B	��B
B
B	��B
B
+B
+B
+B
+B
1B
1B
	7B
1B
1B
1B
1B

=B
DB
PB
JB
DB
PB
VB
VB
bB
bB
hB
bB
hB
hB
hB
oB
oB
uB
oB
oB
hB
uB
oB
oB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
!�B
!�B
 �B
 �B
!�B
!�B
"�B
#�B
$�B
$�B
$�B
"�B
#�B
%�B
%�B
%�B
&�B
&�B
&�B
'�B
(�B
(�B
(�B
(�B
'�B
'�B
)�B
+B
,B
,B
-B
-B
.B
.B
.B
.B
-B
.B
/B
/B
.B
-B
.B
.B
-B
0!B
0!B
1'B
1'B
1'B
2-B
1'B
33B
33B
33B
33B
2-B
33B
33B
33B
33B
33B
49B
33B
2-B
33B
7LB
9XB
9XB
:^B
;dB
;dB
<jB
<jB
;dB
;dB
:^B
:^B
:^B
:^B
9XB
<jB
<jB
<jB
<jB
;dB
:^B
9XB
<jB
>wB
>wB
@�B
@�B
@�B
@�B
?}B
@�B
@�B
A�B
A�B
B�B
C�B
B�B
A�B
?}B
B�B
C�B
C�B
B�B
A�B
F�B
F�B
G�B
G�B
F�B
F�B
E�B
F�B
F�B
F�B
F�B
F�B
H�B
H�B
H�B
G�B
F�B
E�B
F�B
G�B
I�B
H�B
I�B
J�B
I�B
K�B
L�B
M�B
L�B
M�B
M�B
N�B
N�B
M�B
M�B
M�B
N�B
N�B
N�B
O�B
N�B
N�B
M�B
M�B
M�B
M�B
N�B
N�B
O�B
P�B
P�B
Q�B
Q�B
Q�B
P�B
P�B
Q�B
P�B
P�B
O�B
O�B
Q�B
S�B
S�B
T�B
S�B
R�B
S�B
S�B
S�B
S�B
R�B
T�B
VB
T�B
T�B
VB
W
B
VB
VB
T�B
R�B
R�B
T�B
T�B
VB
VB
T�B
S�B
W
B
W
B
W
B
XB
YB
ZB
[#B
[#B
[#B
[#B
ZB
ZB
YB
YB
\)B
\)B
]/B
[#B
[#B
]/B
]/B
_;B
_;B
]/B
_;B
_;B
_;B
_;B
_;B
_;B
^5B
^5B
_;B
`BB
_;B
_;B
`BB
`BB
`BB
`BB
`BB
_;B
_;B
^5B
]/B
[#B
^5B
`BB
`BB
aHB
`BB
aHB
aHB
aHB
cTB
cTB
cTB
bNB
bNB
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
e`B
e`B
ffB
gmB
gmB
gmB
ffB
e`B
ffB
ffB
gmB
ffB
hsB
iyB
iyB
iyB
iyB
iyB
jB
jB
jB
k�B
jB
jB
k�B
jB
jB
l�B
k�B
m�B
l�B
l�B
l�B
k�B
m�B
m�B
m�B
m�B
l�B
l�B
m�B
n�B
m�B
m�B
l�B
m�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
p�B
q�B
r�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�-B�B�'B�B� B�B�'B�B�B�B�B�B�B�B�B�B� B� B�B�B�B�B�'B�B�B�B�B�B�B� B�4B{JB{dB� B�UBz�BcB�lB�iB�qB�eB�)B}B��B�=B��B��B��B��B��Bn�BS@B>wB+�B�TB��B�B��B��B�~B��B�=Bq�B(�B#nB�B
�@B
��B
�B
�qB
�(B
��B
�MB
�NB
B
��B
��B
��B
�QB
�FB
�(B
�+B
}�B
|�B
jB
G�B
$�B
*�B
/�B

XB	�ZB	��B
�B
�B	��B	�B	�B	�B	ۦB	ðB	�(B	��B	��B	�&B	��B	�)B	�?B	��B	��B	��B	��B	z*B	v+B	i�B	R�B	PB	UMB	W$B	PbB	N�B	Q�B	G�B	F�B	G_B	@iB	C�B	C�B	?B	72B	1AB	#nB	-�B	!-B	EB	�B	�B	�B	�B	�B	MB	B	�B	(B	
�B	�B	B	�B	�B�B��B��B�B�qB��B�B�B�B��B��B�B�BյB�QB��B�BߊBۦB� B��B�BªB�1B�MB��B�^B�B��B��B�B�B��B��B�dB�'B~(Bl�Br�B{JB}"By	BsMBq�B��B}qB{Bx�Bp�Bq�BwfBy$BuZBpoBg�BW�B^jBXyB`Ba�Ba�BW?BPBMBCGBK�B<PBA�BB�BLBG_B?�B,=B"hB1'B2�B<B>�B>�B>�B:�B0;B6�B0�B)DBeB5B%,B+�B0�B.�B2�B3�B2|B,�B!�BB�B�B�B{BQB�BSBaB
�BVBjBBBBHBB B)B �B�BKB��BMB�BB_B�B,B�BBYB�BpB# B%,B%,B#:B BB]BkBBIB"4B"�B�B!bB&LB#nB�B"B!bB(�B+kB/OB-wB+�B(�B.IB)yB�B �BQB(�B1�B5�B2B=�B@�B=�B:�B=B=�BB�BB�BAB@4B@4BI�BI�BHBCaB>�BO(BQBO(BK^BI�BD�BQ�BU�BX�BW�BT�BabB`vB`vB`vBa�B`�Ba�Bd�Bd�Be�Bg�Bg�Bf�Bf�Bf�Bg�Bf�Bj�BiDBj0Bm)BtBsBuBtBp;Bt9BzBxRBw�B}qB�gB�xB�pB��B��B��B��B��B��B��B�B��B�WB��B�yB�8B�CB�iB�MB�|B�MB�aB��B��B�jB��B��B��B��B��B�B�B�YB�.B�2B�?B�EB�?B�mB�gB՛BچBޞB�B��B�B��B� B� B� B�B��B��B�B�B��B�B�.B��B	MB	EB	�B		�B	
�B	�B	�B	�B	�B	B	$&B	+B	-B	2-B	1vB	0�B	:�B	?�B	A�B	B�B	B�B	B�B	D�B	E�B	E�B	H�B	H�B	I�B	MB	OB	QB	T,B	V9B	VmB	\CB	^jB	^jB	]dB	[�B	^�B	a�B	f�B	h�B	g�B	h�B	g�B	n�B	r�B	t�B	uB	xB	|B	}B	|B	z�B	|B	|�B	}"B	}B	.B	�GB	�%B	�?B	�9B	�GB	��B	~�B	�mB	�dB	�jB	�jB	��B	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�:B	�B	�>B	�B	�B	�6B	�IB	�AB	�TB	�ZB	�`B	��B	�lB	��B	��B	��B	��B	ªB	ĶB	ĶB	ĶB	żB	��B	��B	�B	�B	� B	�B	�B	�B	�B	�,B	�@B	�FB	�?B	�_B	�QB	�yB	�]B	�pB	�vB	�pB	�hB	�ZB	�hB	�|B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	��B	�B	�B	�B	�B	��B	��B	��B	��B	�!B	��B	��B	��B	��B	��B	�	B	�	B	�	B	�B	�?B	�$B	�*B	�6B
 4B
 B	�B	�.B	�HB
;B
;B	�}B
MB
_B
_B
_B
EB
KB
KB
	7B
fB
KB
fB
fB

rB
xB
jB
dB
xB
jB
�B
pB
}B
}B
�B
}B
�B
�B
�B
�B
oB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
B
"�B
!�B
!�B
 �B
 �B
!�B
!�B
"�B
#�B
%B
$�B
$�B
#B
$B
%�B
%�B
&B
'B
'B
'B
(
B
)*B
)B
)*B
)B
($B
($B
*B
+6B
,B
,"B
-CB
-CB
./B
./B
./B
.IB
-CB
./B
/5B
/5B
./B
-]B
./B
./B
-CB
0!B
0;B
1[B
1AB
1AB
2GB
1AB
3MB
3MB
3MB
3MB
2aB
33B
3hB
3hB
3MB
3MB
49B
3hB
2aB
3hB
7LB
9XB
9XB
:^B
;dB
;dB
<�B
<jB
;B
;B
:�B
:�B
:xB
:�B
9�B
<�B
<�B
<�B
<�B
;B
:�B
9�B
<�B
>�B
>�B
@�B
@�B
@�B
@�B
?�B
@�B
@�B
A�B
A�B
B�B
C�B
B�B
A�B
?�B
B�B
C�B
C�B
B�B
A�B
F�B
F�B
G�B
G�B
F�B
F�B
E�B
F�B
F�B
F�B
F�B
F�B
H�B
H�B
H�B
G�B
F�B
E�B
F�B
G�B
I�B
H�B
I�B
J�B
J	B
K�B
L�B
M�B
L�B
M�B
NB
N�B
N�B
M�B
NB
NB
N�B
N�B
N�B
O�B
N�B
OB
NB
M�B
M�B
M�B
N�B
N�B
PB
Q B
P�B
RB
Q�B
Q�B
Q B
QB
RB
Q B
Q B
O�B
P.B
RB
S�B
TB
T�B
TB
SB
TB
TB
TB
T,B
S&B
UB
VB
U2B
UB
VB
W
B
V9B
VB
UB
S&B
S&B
UB
U2B
VB
VB
UB
T,B
W$B
W$B
W?B
X+B
YB
ZB
[#B
[#B
[#B
[WB
Z7B
Z7B
Y1B
YKB
\)B
\CB
]/B
[WB
[WB
]IB
]dB
_VB
_;B
]dB
_VB
_VB
_VB
_;B
_pB
_VB
^jB
^OB
_VB
`BB
_VB
_VB
`\B
`BB
`BB
`BB
`BB
_VB
_pB
^OB
]dB
[qB
^OB
`\B
`\B
a|B
`\B
a|B
abB
a|B
cTB
cnB
cnB
b�B
bhB
cnB
d�B
e�B
ezB
e`B
ezB
ezB
e�B
e�B
f�B
e�B
e�B
f�B
g�B
g�B
gmB
f�B
e�B
f�B
f�B
g�B
f�B
h�B
i�B
i�B
i�B
i�B
i�B
j�B
jB
j�B
k�B
jB
j�B
k�B
j�B
j�B
l�B
k�B
m�B
l�B
l�B
l�B
k�B
m�B
m�B
m�B
m�B
l�B
l�B
m�B
n�B
m�B
m�B
l�B
m�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
p�B
q�B
r�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.06(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201803130048172018031300481720180313004817201806221238452018062212384520180622123845201804050435472018040504354720180405043547  JA  ARFMdecpA19c                                                                20180309093520  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180309003521  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180309003523  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180309003524  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180309003524  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180309003524  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180309003525  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180309003525  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180309003525  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180309003525                      G�O�G�O�G�O�                JA  ARUP                                                                        20180309005529                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180312154624  CV  JULD            G�O�G�O�F�                JM  ARCAJMQC2.0                                                                 20180312154817  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180312154817  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404193547  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622033845  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115121518                      G�O�G�O�G�O�                