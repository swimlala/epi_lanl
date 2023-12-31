CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2016-12-20T00:35:27Z creation;2016-12-20T00:35:30Z conversion to V3.1;2019-12-19T08:22:46Z update;     
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
_FillValue                    �<Argo profile    3.1 1.2 19500101000000  20161220003527  20200115111518  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               EA   JA  I2_0576_069                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @����eC 1   @���:� @:ش9Xb�d���vȴ1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @9��@y��@�  A   A!��A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  Dy�D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DA��DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DVfDV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh�fDi  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�C3D̀ D�� D���D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�	�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@5@u@�{@�{A ��A?
=A_
=A
=A��A��A��A��AυA߅A�A��BBBBB'B/B7B?BGBOBWB_BgBoBwBB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HC�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RD |)D �)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D	|)D	�)D
|)D
�)D|)D�)D|)D�)Du�D�)Du�D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D |)D �)D!|)D!�)D"|)D"�)D#|)D#�)D$|)D$�)D%|)D%�)D&|)D&�)D'|)D'�)D(|)D(�)D)|)D)�)D*|)D*�)D+|)D+�)D,|)D,�)D-|)D-�)D.|)D.�)D/|)D/�)D0|)D0�)D1|)D1�)D2|)D2�)D3|)D3�)D4|)D4�)D5|)D5�)D6|)D6�)D7|)D7�)D8|)D8�)D9|)D9�)D:|)D:�)D;|)D;�)D<|)D<�)D=|)D=�)D>|)D>�)D?|)D?�)D@|)D@�)DA|)DA��DB|)DB�)DC|)DC�)DD|)DD�)DE|)DE�)DF|)DF�)DG|)DG�)DH|)DH�)DI|)DI�)DJ|)DJ�)DK|)DK�)DL|)DL�)DM|)DM�)DN|)DN�)DO|)DO�)DP|)DP�)DQ|)DQ�)DR|)DR�)DS|)DS�)DT|)DT�)DU|)DV�DV|)DV�)DW|)DW�)DX|)DX�)DY|)DY�)DZ|)DZ�)D[|)D[�)D\|)D\�)D]|)D]�)D^|)D^�)D_|)D_�)D`|)D`�)Da|)Da�)Db|)Db�)Dc|)Dc�)Dd|)Dd�)De|)De�)Df|)Df�)Dg|)Dg�)Dh��Dh�)Di|)Di�)Dj|)Dj�)Dk|)Dk�)Dl|)Dl�)Dm|)Dm�)Dn|)Dn�)Do|)Do�)Dp|)Dp�)Dq|)Dq�)Dr|)Dr�)Ds|)Ds�)Dt|)Dt�)Du|)Du�)Dv|)Dv�)Dw|)Dw�)Dx|)Dx�)Dy|)Dy�)Dz|)Dz�)D{|)D{�)D||)D|�)D}|)D}�)D~|)D~�)D|)D�)D�>D�z�D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D�HD�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�:�D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D¾D��D�>D�~DþD��D�>D�~DľD��D�>D�~DžD��D�>D�~DƾD��D�>D�~DǾD��D�>D�~DȾD��D�>D�~DɾD��D�>D�~DʾD��D�>D�~D˾D��D�>D�~D̾D��D�AHD�~D;D���D�>D�~DξD��D�>D�~DϾD��D�>D�~DоD��D�>D�~DѾD��D�>D�~DҾD��D�>D�~DӾD��D�>D�~DԾD��D�>D�~DվD��D�>D�~D־D��D�>D�~D׾D��D�>D�~DؾD��D�>D�~DپD��D�>D�~DھD��D�>D�~D۾D��D�>D�~DܾD��D�>D�~DݾD��D�>D�~D޾D��D�>D�~D߾D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D��D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�AHD�~D��D��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�ffA�ffA�hsA�ffA�hsA�hsA�hsA�jA�jA�jA�l�A�n�A�n�A�n�A�p�A�r�A�r�A�t�A�t�A�t�A�v�A�v�A�x�A�z�A�z�A�z�A�z�A�|�A�~�A�~�A�~�A�~�A�z�A�|�A�|�A�G�A���A��FA���A��A�ȴA�=qA�C�A���A�/A���A��#A�r�A��A�-A�|�A�ZA��A�t�A�9XA�C�A�;dA���A��+A���A�O�A�n�A� �A��A�XA�dZA�bA�=qA���A�?}A��A��A��A�+A���A���A��#A���A�VA�~�A�M�A��^A�v�A��^A��A�hsA��/A�A�^5A���A�$�A��A���A�hsA�I�A�-A�-A���A�hsA���A�+A~^5A}p�A|�!A{�AzĜAy�TAyhsAv�uAtA�ArbNAq|�Ap�RAoXAm�TAml�Am\)Al�HAlE�AjȴAj{Ai\)Ah1'AgK�AfȴAe��Ac��Act�Ab��Aa�Aa"�A_�;A^ZA]l�A\~�AZ�jAZ=qAY�#AY"�AV�HAVv�AU�TASx�AP�yAOl�ANr�AM�-AMC�AKS�AIC�AG��AF�AE�TAC��AB�\AB^5AB5?AA�;A@�A@��A@��A@�+A@ZA?��A?��A?;dA=�-A=/A<�/A;"�A9�A9p�A8�\A8{A7�TA7�-A7l�A6��A5�wA3A2��A2Q�A2  A1��A1��A1x�A1�A0��A0r�A/�A/&�A.bNA-�PA+�;A+��A*ĜA*VA)�-A(ZA'�A'x�A';dA&��A%��A$��A$-A#��A#%A!�^A �+A�A�A�yAr�A�mA�yA�Ax�A�jA��A��A$�A��A+A�9A�hAA��A�-A+A�AM�A�A�hA
=A��AC�A1'A��A
ȴA
bA	��A�A�^A�A��A1'A�A9XA�
A�#A�A33A��A�AG�A �+A (�@�C�@�hs@�(�@�K�@�ȴ@��\@�Z@���@�P@�V@�x�@��@�l�@���@�dZ@�h@�9@�j@�(�@�@�&�@�I�@��@�7@�r�@ާ�@�V@�E�@��@�V@ۅ@�^5@�?}@أ�@�j@ׅ@�X@��m@�t�@�M�@�bN@Ώ\@���@���@�ƨ@ʧ�@�?}@�A�@�|�@�ȴ@�5?@��@�x�@�E�@���@�Q�@�  @��@��@���@�O�@���@��m@�x�@���@�bN@�t�@��^@���@��T@�?}@�Ĝ@��9@���@��@�{@�z�@�S�@�\)@�ƨ@���@���@��@���@�$�@��j@���@���@��
@�J@�hs@�&�@���@��/@���@�I�@���@��P@��y@�@���@��@���@�dZ@�S�@�+@�o@���@���@�^5@���@���@��@�ƨ@�33@�
=@��H@��!@��\@��\@���@�n�@���@�G�@���@�j@�b@��@��w@���@��@�l�@�33@��@��R@��!@�ȴ@��@�ȴ@��@�p�@�X@�X@�X@�V@���@�Ĝ@��j@�z�@�I�@��;@��F@��F@���@���@��P@�C�@���@�v�@�X@�Z@���@�ff@�O�@��@�%@�j@��w@���@���@�t�@�dZ@�\)@�K�@�5?@��T@��7@�hs@�`B@��@���@���@�%@��@�9X@�\)@�C�@�C�@�K�@��y@�~�@�=q@�$�@�=q@��@��T@�?}@���@��j@�r�@�1@�@�@~��@~�+@~5?@}��@}p�@|��@|I�@|(�@{�@z�!@z=q@y�#@y��@yG�@y&�@x��@x�`@xĜ@x�@x  @w
=@w
=@w
=@v�y@v�R@vV@v$�@u�T@u�h@up�@u/@u�@u?}@u?}@uO�@uO�@uV@u/@u�@u?}@uV@t�@t�@s�
@s��@s�F@sƨ@sƨ@s�m@t1@t1@s��@sS�@r�H@sƨ@sS�@r�@q�^@q��@q%@q%@p��@p�@pA�@o�@o�@o�@o�P@oK�@n��@n$�@m@mp�@m`B@l�j@k�F@kC�@j�@j�@j�H@j~�@j�@i��@i��@ihs@ihs@iG�@i�@h�@g�@gl�@gK�@f�@e?}@d��@dj@dI�@cƨ@co@b-@ax�@`�`@`Ĝ@`�@_�w@_+@^��@^v�@^V@^@]�T@]��@]��@]�@]O�@\��@\�D@\Z@\I�@[��@Z�@Z-@Y�7@Yx�@Yhs@Yhs@YX@YX@YX@Y7L@XA�@W�P@W
=@Vff@T(�@So@R��@Q��@Q7L@P  @Ol�@O+@N�R@N�+@NV@N5?@M�T@Mp�@L��@L��@MV@M`B@M��@N�y@OK�@Ol�@Nȴ@N{@NE�@N{@M/@L��@LI�@K��@Jn�@I��@I�^@I�@H�@H  @Gl�@F��@F�+@Fff@FE�@F{@F{@F{@F{@F$�@F$�@F{@F@F@E�@E�@E�@E�@F@F@F{@F{@F{@F{@F{@E�@Ep�@E�@D�@D�D@C��@C�@CC�@B�@B��@B��@Bn�@B=q@BJ@A�7@A&�@@�9@@ �@?�;@?��@?K�@>ȴ@=`B@<��@<�@<z�@<j@<j@<9X@;t�@:��@:�\@:-@9�^@9G�@8�`@8��@8 �@7�;@7��@7�P@7\)@7K�@7
=@6��@6v�@6V@6$�@6@5@5�@5`B@5/@5�@4��@4��@4�@4�@4�/@4��@4�D@4j@4I�@3��@3�
@3�F@3��@3��@3dZ@3C�@3o@3@2�H@2�\@1��@0�`@01'@/�@/��@/l�@/
=@.ȴ@.��@.ff@.5?@-��@-�@-?}@,�@,�D@,9X@,(�@,�@,1@+�m@+�F@+dZ@+33@+"�@+@*�@*��@*�!@*~�@*J@)��@)��@)��@)��@)�7@)hs@)&�@(��@(r�@(bN@(A�@'�w@'K�@'+@&�@&ff@&@%�@%�-@%��@%�h@$�/@$�D@$I�@#��@#dZ@#33@#@"n�@"-@!�@!��@!��@!��@!x�@!hs@!X@!G�@!7L@!�@ ��@ Ĝ@ ��@ �u@ r�@��@��@�R@��@��@v�@ff@V@E�@V@V@@�T@��@@�-@�-@��@�h@�@�@p�@`B@/@�j@z�@Z@I�@�m@��@o@��@��@�\@n�@^5@J@�#@��@x�@hs@hs@hs@X@G�@G�@&�@�@�@�`@Ĝ@�u@bN@b@�w@��@\)@
=@v�@�h@?}@�@V@��@�@�@��@�@�D@Z@��@�
@�F@S�@33@"�@@��@=q@�@x�@X@7L@7L@�@�9@�@r�@A�@�;@|�@l�@l�@\)@�@�y@ȴ@ff@@�@�T@@�h@?}@�@�@j@Z@9X@(�@��@�
@�
@ƨ@t�@@@@
�@
�H@
�!@
^5@	�7@	x�@	x�@	x�@	x�@	x�@	x�@	x�@	x�@	hs@1'@K�@
=@�y@�@ȴ@�R@��@��@��@�+@�+@�+@�+@v�@V@5?@@��@@@�-@��@�h@`B@��@j@9X@�@�
@��@��@�@t�@t�@C�@o@@@�@��@�\@~�@=q@=q@=q@-@J@�@X@7L@�@ �`@ Ĝ@ r�@ 1'@ b@   ?��;?��w?�\)?��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�ffA�ffA�hsA�ffA�hsA�hsA�hsA�jA�jA�jA�l�A�n�A�n�A�n�A�p�A�r�A�r�A�t�A�t�A�t�A�v�A�v�A�x�A�z�A�z�A�z�A�z�A�|�A�~�A�~�A�~�A�~�A�z�A�|�A�|�A�G�A���A��FA���A��A�ȴA�=qA�C�A���A�/A���A��#A�r�A��A�-A�|�A�ZA��A�t�A�9XA�C�A�;dA���A��+A���A�O�A�n�A� �A��A�XA�dZA�bA�=qA���A�?}A��A��A��A�+A���A���A��#A���A�VA�~�A�M�A��^A�v�A��^A��A�hsA��/A�A�^5A���A�$�A��A���A�hsA�I�A�-A�-A���A�hsA���A�+A~^5A}p�A|�!A{�AzĜAy�TAyhsAv�uAtA�ArbNAq|�Ap�RAoXAm�TAml�Am\)Al�HAlE�AjȴAj{Ai\)Ah1'AgK�AfȴAe��Ac��Act�Ab��Aa�Aa"�A_�;A^ZA]l�A\~�AZ�jAZ=qAY�#AY"�AV�HAVv�AU�TASx�AP�yAOl�ANr�AM�-AMC�AKS�AIC�AG��AF�AE�TAC��AB�\AB^5AB5?AA�;A@�A@��A@��A@�+A@ZA?��A?��A?;dA=�-A=/A<�/A;"�A9�A9p�A8�\A8{A7�TA7�-A7l�A6��A5�wA3A2��A2Q�A2  A1��A1��A1x�A1�A0��A0r�A/�A/&�A.bNA-�PA+�;A+��A*ĜA*VA)�-A(ZA'�A'x�A';dA&��A%��A$��A$-A#��A#%A!�^A �+A�A�A�yAr�A�mA�yA�Ax�A�jA��A��A$�A��A+A�9A�hAA��A�-A+A�AM�A�A�hA
=A��AC�A1'A��A
ȴA
bA	��A�A�^A�A��A1'A�A9XA�
A�#A�A33A��A�AG�A �+A (�@�C�@�hs@�(�@�K�@�ȴ@��\@�Z@���@�P@�V@�x�@��@�l�@���@�dZ@�h@�9@�j@�(�@�@�&�@�I�@��@�7@�r�@ާ�@�V@�E�@��@�V@ۅ@�^5@�?}@أ�@�j@ׅ@�X@��m@�t�@�M�@�bN@Ώ\@���@���@�ƨ@ʧ�@�?}@�A�@�|�@�ȴ@�5?@��@�x�@�E�@���@�Q�@�  @��@��@���@�O�@���@��m@�x�@���@�bN@�t�@��^@���@��T@�?}@�Ĝ@��9@���@��@�{@�z�@�S�@�\)@�ƨ@���@���@��@���@�$�@��j@���@���@��
@�J@�hs@�&�@���@��/@���@�I�@���@��P@��y@�@���@��@���@�dZ@�S�@�+@�o@���@���@�^5@���@���@��@�ƨ@�33@�
=@��H@��!@��\@��\@���@�n�@���@�G�@���@�j@�b@��@��w@���@��@�l�@�33@��@��R@��!@�ȴ@��@�ȴ@��@�p�@�X@�X@�X@�V@���@�Ĝ@��j@�z�@�I�@��;@��F@��F@���@���@��P@�C�@���@�v�@�X@�Z@���@�ff@�O�@��@�%@�j@��w@���@���@�t�@�dZ@�\)@�K�@�5?@��T@��7@�hs@�`B@��@���@���@�%@��@�9X@�\)@�C�@�C�@�K�@��y@�~�@�=q@�$�@�=q@��@��T@�?}@���@��j@�r�@�1@�@�@~��@~�+@~5?@}��@}p�@|��@|I�@|(�@{�@z�!@z=q@y�#@y��@yG�@y&�@x��@x�`@xĜ@x�@x  @w
=@w
=@w
=@v�y@v�R@vV@v$�@u�T@u�h@up�@u/@u�@u?}@u?}@uO�@uO�@uV@u/@u�@u?}@uV@t�@t�@s�
@s��@s�F@sƨ@sƨ@s�m@t1@t1@s��@sS�@r�H@sƨ@sS�@r�@q�^@q��@q%@q%@p��@p�@pA�@o�@o�@o�@o�P@oK�@n��@n$�@m@mp�@m`B@l�j@k�F@kC�@j�@j�@j�H@j~�@j�@i��@i��@ihs@ihs@iG�@i�@h�@g�@gl�@gK�@f�@e?}@d��@dj@dI�@cƨ@co@b-@ax�@`�`@`Ĝ@`�@_�w@_+@^��@^v�@^V@^@]�T@]��@]��@]�@]O�@\��@\�D@\Z@\I�@[��@Z�@Z-@Y�7@Yx�@Yhs@Yhs@YX@YX@YX@Y7L@XA�@W�P@W
=@Vff@T(�@So@R��@Q��@Q7L@P  @Ol�@O+@N�R@N�+@NV@N5?@M�T@Mp�@L��@L��@MV@M`B@M��@N�y@OK�@Ol�@Nȴ@N{@NE�@N{@M/@L��@LI�@K��@Jn�@I��@I�^@I�@H�@H  @Gl�@F��@F�+@Fff@FE�@F{@F{@F{@F{@F$�@F$�@F{@F@F@E�@E�@E�@E�@F@F@F{@F{@F{@F{@F{@E�@Ep�@E�@D�@D�D@C��@C�@CC�@B�@B��@B��@Bn�@B=q@BJ@A�7@A&�@@�9@@ �@?�;@?��@?K�@>ȴ@=`B@<��@<�@<z�@<j@<j@<9X@;t�@:��@:�\@:-@9�^@9G�@8�`@8��@8 �@7�;@7��@7�P@7\)@7K�@7
=@6��@6v�@6V@6$�@6@5@5�@5`B@5/@5�@4��@4��@4�@4�@4�/@4��@4�D@4j@4I�@3��@3�
@3�F@3��@3��@3dZ@3C�@3o@3@2�H@2�\@1��@0�`@01'@/�@/��@/l�@/
=@.ȴ@.��@.ff@.5?@-��@-�@-?}@,�@,�D@,9X@,(�@,�@,1@+�m@+�F@+dZ@+33@+"�@+@*�@*��@*�!@*~�@*J@)��@)��@)��@)��@)�7@)hs@)&�@(��@(r�@(bN@(A�@'�w@'K�@'+@&�@&ff@&@%�@%�-@%��@%�h@$�/@$�D@$I�@#��@#dZ@#33@#@"n�@"-@!�@!��@!��@!��@!x�@!hs@!X@!G�@!7L@!�@ ��@ Ĝ@ ��@ �u@ r�@��@��@�R@��@��@v�@ff@V@E�@V@V@@�T@��@@�-@�-@��@�h@�@�@p�@`B@/@�j@z�@Z@I�@�m@��@o@��@��@�\@n�@^5@J@�#@��@x�@hs@hs@hs@X@G�@G�@&�@�@�@�`@Ĝ@�u@bN@b@�w@��@\)@
=@v�@�h@?}@�@V@��@�@�@��@�@�D@Z@��@�
@�F@S�@33@"�@@��@=q@�@x�@X@7L@7L@�@�9@�@r�@A�@�;@|�@l�@l�@\)@�@�y@ȴ@ff@@�@�T@@�h@?}@�@�@j@Z@9X@(�@��@�
@�
@ƨ@t�@@@@
�@
�H@
�!@
^5@	�7@	x�@	x�@	x�@	x�@	x�@	x�@	x�@	x�@	hs@1'@K�@
=@�y@�@ȴ@�R@��@��@��@�+@�+@�+@�+@v�@V@5?@@��@@@�-@��@�h@`B@��@j@9X@�@�
@��@��@�@t�@t�@C�@o@@@�@��@�\@~�@=q@=q@=q@-@J@�@X@7L@�@ �`@ Ĝ@ r�@ 1'@ b@   ?��;?��w?�\)?��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B7LB7LB7LB7LB7LB7LB7LB7LB7LB7LB7LB8RB7LB7LB8RB8RB8RB8RB8RB8RB8RB8RB8RB9XB8RB8RB8RB8RB8RB8RB8RB8RB8RB8RB8RB7LB5?B33B2-B'�B�B�B\B%B��B��B�B�B�B�mB�TB�;B��B��B��BȴB�wB�B��B�Bx�BjBR�BO�B@�B2-B,B#�B  B�HB�dB�!B��B��B�hB�=B}�Bw�Bm�BW
BS�BP�B9XB2-B �B�B
��B
�B
�;B
�B
ȴB
�}B
��B
ɺB
ɺB
ǮB
ǮB
ŢB
�qB
�?B
�B
��B
��B
�oB
�7B
�B
x�B
r�B
bNB
P�B
>wB
9XB
1'B
.B
"�B
�B
�B
�B
�B
\B
	7B
%B	��B	��B	��B	�B	�ZB	�5B	�B	��B	��B	ȴB	B	�^B	�FB	��B	��B	��B	��B	�\B	�=B	�+B	x�B	ffB	YB	VB	N�B	I�B	D�B	8RB	.B	+B	$�B	�B	{B	uB	oB	oB	VB	JB	DB	DB	DB	JB	VB	VB	JB		7B	+B	+B��B��B��B��B�B�B�B�B�B�NB�/B�#B�B�B�B�
B�B��B��B��B��B��BȴB��B�}B�wB�XB�RB�3B�B�B�B�B��B��B��B��B��B��B�{B�bB�=B�+B�B�B� B{�By�Bw�Bu�Bq�Bo�Bn�Bl�Bk�BjBhsBgmBe`BbNBbNBaHB^5B^5B\)BZBYBS�BR�BP�BL�BL�BI�BG�BF�BE�BD�BB�BA�B?}B?}B?}B>wB=qB=qB;dB:^B9XB8RB7LB5?B49B33B2-B2-B/B0!B/B.B.B.B.B.B-B-B,B,B,B.B-B/B.B0!B0!B/B/B/B.B0!B0!B2-B2-B2-B33B1'B-B.B-B-B+B+B+B+B+B)�B)�B)�B+B,B+B-B6FB:^B<jB<jB>wB@�BA�BA�BA�BC�BH�BH�BI�BK�BK�BA�B=qB=qBB�BL�BS�B_;B`BB]/B\)BbNBgmBl�Bk�Bm�Bl�Bp�Bn�Bo�Br�Bs�Bw�Bx�By�By�Bz�By�Bz�By�By�B|�B�B�B�+B�+B�1B�7B�=B�DB�JB�VB�hB�oB�uB��B��B��B��B��B��B��B��B��B��B��B��B�B�B�-B�3B�FB�LB�RB�RB�^B�qB��BÖBĜBĜBȴB��B�B�B�B�B�
B�
B�B�B�5B�BB�`B�mB�mB�mB�mB�sB�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B	B	JB	hB	oB	{B	�B	�B	�B	�B	�B	�B	�B	"�B	%�B	(�B	-B	1'B	9XB	<jB	@�B	B�B	C�B	F�B	H�B	I�B	J�B	K�B	K�B	K�B	K�B	K�B	L�B	M�B	N�B	Q�B	R�B	S�B	XB	^5B	bNB	dZB	ffB	gmB	hsB	iyB	jB	k�B	m�B	p�B	x�B	}�B	�B	�B	�B	�%B	�%B	�7B	�=B	�DB	�DB	�DB	�JB	�JB	�JB	�JB	�PB	�bB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�FB	�LB	�dB	�wB	��B	B	ÖB	ŢB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�#B	�#B	�#B	�#B	�/B	�5B	�5B	�5B	�;B	�BB	�TB	�fB	�mB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
+B
+B
%B
+B
DB
DB
DB
DB
JB
JB
DB

=B

=B
	7B
	7B
1B
	7B
	7B
DB
JB
VB
\B
\B
hB
hB
oB
uB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 �B
!�B
"�B
"�B
#�B
#�B
$�B
$�B
%�B
'�B
(�B
(�B
(�B
(�B
(�B
(�B
+B
,B
,B
,B
-B
-B
.B
.B
/B
/B
0!B
0!B
0!B
0!B
1'B
2-B
2-B
2-B
2-B
33B
33B
33B
49B
49B
49B
49B
49B
49B
49B
49B
5?B
5?B
5?B
5?B
6FB
6FB
6FB
6FB
6FB
7LB
7LB
7LB
7LB
7LB
8RB
8RB
:^B
;dB
;dB
<jB
<jB
<jB
<jB
=qB
=qB
=qB
>wB
>wB
>wB
?}B
?}B
@�B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
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
D�B
D�B
E�B
E�B
E�B
F�B
F�B
F�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
I�B
J�B
J�B
J�B
K�B
K�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
O�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
S�B
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
XB
YB
YB
YB
YB
YB
YB
YB
YB
ZB
ZB
ZB
ZB
[#B
\)B
]/B
]/B
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
`BB
`BB
`BB
`BB
aHB
aHB
aHB
aHB
aHB
aHB
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
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
dZB
dZB
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
hsB
hsB
hsB
iyB
iyB
iyB
jB
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
k�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
o�B
o�B
o�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
s�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B7LB7LB7LB7LB7LB7LB7LB7LB7LB7LB7LB8RB7LB7LB8RB8RB8RB8RB8RB8RB8RB8RB8RB9rB8RB8RB8RB8RB8lB8RB8RB8RB8lB8�B8�B88B6zB5�B6+B*0BxBB�B�B�^B�FB��B�B�wB�B��B�@B�SB�B�6B˒BB�vB��B��B{BmBT�BR�BB�B3�B/B)*B�B�B�BB��B��B�7B�uB�6B��B~(BoOBXBU�BSuB;JB49B"�B�B
�B
�WB
�bB
��B
ɠB
��B
�pB
�	B
��B
��B
ȀB
�B
�B
��B
�B
��B
��B
��B
�rB
�GB
zDB
u�B
d�B
R�B
?�B
:xB
2�B
/�B
#TB
B
~B
�B
$B
bB

XB
zB
 B	��B	�FB	�;B	�,B	�;B	ۦB	��B	�vB	ʌB	��B	��B	��B	��B	��B	�B	��B	�bB	��B	�=B	{�B	hXB	ZkB	W
B	PB	L0B	F�B	9�B	/�B	,�B	'B	 'B	�B	�B	B	uB	�B	~B	�B	�B	�B	�B	BB	�B	6B	
#B		RB	�B��B��B�lB�%B�B�MB�B�oB��B�BݲBۦB�kB�eB�yB׍B֡B՛B��B�&B�B�0BʌB�'B��B�HB�xB��B�B��B��B�B�]B�DB��B��B�B��B�QB�9B�oB�DB��B�B�gB�;B|�Bz�By$Bw2Br|BpoBo�BmwBl�BkQBiDBh�BfBb�Bc Ba�B^�B_B]/B[�BZkBT�BT,BRBM�BN<BJ�BHKBG�BF�BFBC�BBB?�B@ B@iB?}B>�B>wB<jB;B:DB9�B8B5�B4�B3�B3�B3�B0�B1B/�B.�B/5B/OB/�B/5B-�B-wB,qB,�B-�B.�B./B0B/ B1AB0�B/iB/�B/�B/5B0�B0�B2�B2�B3B4�B2-B-�B/B.cB./B+�B+�B+�B,B,B*�B*�B*�B+�B,�B,"B/ B7LB:�B<�B<�B?BA;BBBB'BB�BEBIRBIlBJ�BM6BMjBBAB>B=�BB�BMBT�B`vBaHB]�B\CBb�BhXBm]BlWBn�Bm)Bq[Bn�BpBs�Bt�Bx8By	Bz*BzB{Bz^B{JBzDBzxB}�B�oB��B��B�zB�KB��B��B��B��B��B��B��B�,B�mB�B��B��B��B��B��B��B�B�fB�RB�eB�WB�iB�aB�hB�zB�fB��B��B��B��B��BðBĶB�B�RB�[B�B�B�B�mB�?B�$B�EB�_B�jB��B�B�B�B�B�B��B�B�GB�B�B�aB��B�OB��B��B�'B�'B��B��B��B��B��B�>B��B	gB	~B	�B	�B	�B	�B	�B	�B	�B	7B	)B	�B	"�B	&B	)DB	-]B	1[B	9rB	<�B	@�B	B�B	C�B	F�B	H�B	J	B	KB	LB	K�B	LB	K�B	LB	MB	NB	O(B	RB	S&B	TFB	X_B	^jB	bhB	d�B	f�B	g�B	h�B	i�B	j�B	k�B	m�B	p�B	x�B	}�B	� B	�-B	�MB	�YB	�YB	�RB	�XB	�xB	�DB	�^B	�JB	�JB	�dB	�dB	�jB	�bB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�-B	�B	��B	��B	��B	��B	�B	��B	��B	ªB	��B	��B	��B	��B	��B	��B	��B	�B	�"B	�B	� B	�B	�4B	�NB	�:B	� B	�B	�B	�@B	�,B	�2B	�2B	�B	��B	�B	�B	�MB	�_B	�WB	�=B	یB	��B	�~B	�jB	�jB	ބB	ߊB	��B	�B	�B	�B	�B	��B	�B	�B	��B	�B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	�%B	�+B	�B	�B	��B	��B	��B	��B	��B	�B	�B	�0B	�JB	�*B	�XB	��B	�+B	��B	��B	��B	�B	��B	�B	��B	�B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B
B
SB
_B
zB
%B
_B
�B
xB
xB
�B
�B
~B
�B

�B

�B
	lB
	�B
�B
	RB
	RB
xB
~B
VB
\B
vB
�B
�B
�B
�B
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!B
!�B
#B
#B
#�B
#�B
%B
%FB
&LB
(
B
)B
)*B
(�B
)B
)*B
)DB
+QB
,=B
,=B
,WB
-CB
-CB
./B
.cB
/5B
/5B
0;B
0UB
0UB
0;B
1[B
2GB
2GB
2GB
2GB
3MB
3MB
3hB
4TB
49B
4nB
49B
4TB
49B
4TB
4TB
5ZB
5ZB
5ZB
5ZB
6zB
6zB
6FB
6`B
6zB
7fB
7�B
7�B
7fB
7�B
8�B
8�B
:�B
;B
;B
<�B
<�B
<�B
<�B
=�B
=�B
=�B
>�B
>�B
>�B
?�B
?�B
@�B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
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
D�B
D�B
E�B
E�B
E�B
F�B
F�B
F�B
G�B
G�B
H�B
H�B
H�B
H�B
IB
I�B
J�B
J�B
J�B
K�B
K�B
MB
MB
M�B
NB
M�B
M�B
NB
M�B
N�B
N�B
N�B
OB
OB
N�B
N�B
N�B
OB
PHB
QNB
R B
Q�B
RB
RB
RB
Q�B
RB
Q�B
RB
S&B
SB
R�B
SB
R�B
SB
SB
SB
SB
R�B
R�B
SB
T,B
TFB
T,B
TB
T,B
T,B
UB
UMB
V9B
VB
W
B
W?B
W$B
W?B
XEB
XEB
X+B
X+B
XB
XB
XB
XB
XB
Y1B
Y1B
YB
Y1B
Y1B
Y1B
Y1B
Y1B
ZQB
Z7B
Z7B
ZQB
[WB
\xB
]IB
]IB
]IB
]/B
]/B
]/B
]IB
]IB
^jB
^OB
^OB
_pB
_VB
_VB
_VB
`\B
`\B
`\B
`vB
a|B
abB
a|B
abB
aHB
abB
bhB
bhB
bhB
bhB
bhB
bhB
cTB
cnB
c�B
cnB
c�B
c�B
cnB
cnB
cTB
cnB
cnB
c�B
c�B
cnB
c�B
cnB
dZB
dtB
dtB
dtB
dtB
dtB
d�B
dtB
ezB
e`B
e`B
e`B
e�B
ezB
e�B
e�B
ffB
ffB
ffB
ffB
f�B
f�B
f�B
f�B
f�B
f�B
g�B
g�B
g�B
g�B
gmB
gmB
g�B
gmB
gmB
g�B
gmB
gmB
gmB
gmB
h�B
h�B
h�B
h�B
hsB
hsB
hsB
h�B
h�B
h�B
i�B
i�B
i�B
j�B
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
k�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
o�B
o�B
o�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
s�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.06(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201612240033332016122400333320161224003333201806221218312018062212183120180622121831201804050411392018040504113920180405041139  JA  ARFMdecpA19c                                                                20161220093506  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20161220003527  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20161220003528  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20161220003528  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20161220003529  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20161220003529  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20161220003529  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20161220003529  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20161220003530  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20161220003530                      G�O�G�O�G�O�                JA  ARUP                                                                        20161220015600                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20161220153514  CV  JULD            G�O�G�O�F��                JM  ARCAJMQC2.0                                                                 20161223153333  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20161223153333  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404191139  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622031831  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115111518                      G�O�G�O�G�O�                