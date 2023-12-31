CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:19:20Z creation;2022-06-04T19:19:20Z conversion to V3.1      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8<   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8@   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8H   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8L   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8T   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8\   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8d   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8h   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8p   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8t   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8x   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8|   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9|   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I@   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M0   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �P   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �@   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �`   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �<   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �@   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �D   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �H   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �L   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604191920  20220610151508  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               2A   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @�#З�&1   @�#-!�@/,1&�y�cWƧ1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   AffA@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�33B�ffB�  B�  B�  B�  B�  B�  B�33B���B�33B���B�  B�  B�33B�33B���B���B�  B�  B�  B�  B�  B�  B�  B�ffB�  B�  B���B�  C   C  C  C�fC  C
  C  C  C  C  C  C  C  C  C  C  C �C"  C#�fC%�fC(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch�Cj�Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$fD$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ�fD[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�@k�@�@�AG�A:�HAZ�HAz�HA�p�A�p�A�p�A�p�A�p�A�p�A�p�A�p�B�RB�RB�RB�RB&�RB.�RB6�RB>�RBF�RBN�RBV�RB^�RBf�RBn�RBv�RB~�RB�\)B��\B�B�\)B�\)B�\)B�\)B�\)B�\)B��\B���B��\B�(�B�\)B�\)B��\BÏ\B�(�B�(�B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�B�\)B�\)B�(�B�\)B�\)C�C�C�zC�C	�C�C�C�C�C�C�C�C�C�C�CǮC!�C#�zC%�zC'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�CgǮCiǮCk�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C���C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
D k�D �Dk�D�Dk�D�Dk�D�Dk�D�Dk�D�Dk�D�Dk�D�Dk�D�D	k�D	�D
k�D
�Dk�D�Dk�D�Dk�D�Dk�D�Dk�D�Dk�D�Dk�D�Dk�D�Dk�D�Dk�D�Dk�D�Dk�D�Dk�D�Dk�D�Dk�D�Dk�D�Dk�D�Dk�D�Dk�D�Dk�D�Dk�D�D k�D �D!k�D!�D"k�D"�D#k�D#��D$k�D$�D%k�D%�D&k�D&�D'k�D'�D(k�D(�D)k�D)�D*k�D*�D+k�D+�D,k�D,�D-k�D-�D.k�D.�D/k�D/�D0k�D0�D1k�D1�D2k�D2�D3k�D3�D4k�D4�D5k�D5�D6k�D6�D7k�D7�D8k�D8�D9k�D9�D:k�D:�D;k�D;�D<k�D<�D=k�D=�D>k�D>�D?k�D?�D@k�D@�DAk�DA�DBk�DB�DCk�DC�DDk�DD�DEk�DE�DFk�DF�DGk�DG�DHk�DH�DIk�DI�DJk�DJ�DKk�DK�DLk�DL�DMk�DM�DNk�DN�DOk�DO�DPk�DP�DQk�DQ�DRk�DR�DSk�DS�DTk�DT�DUk�DU�DVk�DV�DWk�DW�DXk�DX�DYk�DY�DZq�DZ�D[k�D[�D\k�D\�D]k�D]�D^k�D^�D_k�D_�D`k�D`�Dak�Da�Dbk�Db�Dck�Dc�Ddk�Dd�Dek�De�Dfk�Df�Dgk�Dg�Dhk�Dh�Dik�Di�Djk�Dj�Dkk�Dk�Dlk�Dl�Dmk�Dm�Dnk�Dn�Dok�Do�Dpk�Dp�Dqk�Dq�Drk�Dr�Dsk�Ds�Dtk�Dt�Duk�Du�Dvk�Dv�Dwk�Dw�Dxk�Dx�Dyk�Dy�Dzk�Dz�D{k�D{�D|k�D|�D}k�D}�D~k�D~�Dk�D�D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�8�D�x�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�Dµ�D���D�5�D�u�Dõ�D���D�5�D�u�Dĵ�D���D�5�D�u�Dŵ�D���D�5�D�u�DƵ�D���D�5�D�u�Dǵ�D���D�5�D�u�Dȵ�D���D�5�D�u�Dɵ�D���D�5�D�u�Dʵ�D���D�5�D�u�D˵�D���D�5�D�u�D̵�D���D�5�D�u�D͵�D���D�5�D�u�Dε�D���D�5�D�u�Dϵ�D���D�5�D�u�Dе�D���D�5�D�u�Dѵ�D���D�5�D�u�Dҵ�D���D�5�D�u�Dӵ�D���D�5�D�u�DԵ�D���D�5�D�u�Dյ�D���D�5�D�u�Dֵ�D���D�5�D�u�D׵�D���D�5�D�u�Dص�D���D�5�D�u�Dٵ�D���D�5�D�u�Dڵ�D���D�5�D�u�D۵�D���D�5�D�u�Dܵ�D���D�5�D�u�Dݵ�D���D�5�D�u�D޵�D���D�5�D�u�Dߵ�D���D�5�D�u�D��D���D�5�D�u�D��D���D�5�D�u�D��D���D�5�D�u�D��D���D�5�D�u�D��D���D�5�D�u�D��D���D�5�D�u�D��D���D�5�D�u�D��D���D�5�D�u�D��D���D�5�D�u�D��D���D�5�D�u�D��D���D�5�D�u�D��D���D�5�D�u�D��D���D�5�D�u�D���D���D�5�D�u�D��D���D�5�D�u�D��D���D�5�D�u�D��D���D�5�D�u�D��D���D�5�D�u�D��D���D�5�D�u�D��D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A̳3A̳�A̴9A̽qA̿HA���A̸�A̮Ą�A̦LA̟VA̟�ǍA�y>A�`vA�X�A�V�A�T�A�Q�A�RTA�QA�M�A�K)A�J�A�J#A�I�A�HA�FA�>A�;�A�8A�1[A�-A�֡AȼjA�ZA�W
A�k�A��A��RA�49A���A��A���A���A��hA�gA��A���A�B[A���A��6A���A���A���A�h>A�a�A�r�A���A�.A���A�B�A��A�XEA��tA��{A�h�A��jA��A��tA�4A��MA�fA�w2A��6A�A���A�.A�A��SA~[WA{��A{AwT�Aq��An�gAe_�A]	A[*�AW�rAU�OAT�gATJ�AQ/�AO}VALhsAH��AF>�AB� AA�AA��A@�A=�A<=A:$A8�A8/�A5�8A4u%A2��A0��A/�A--A,[�A+��A+A*�@A*jA*4A)��A&��A&v�A&?A%)�A!�VA �Al�A
=A�A��A4A��A�4A��A�A/�AM�A iAkQA�FA�A/A��A��A��A~�A�A�A+ARTA�A��AP�A�A��A�A�}A��Aw2A�oA��A��A��A�fA8AA�A�SA�AA;dA \A�A�SA�KA�A@AZA�A�A�A[WAD�A�A�A)�A�tA��A#:A�AیA|A
��A��AJA�A5?A�A�8AA��A��A�3A{JA�5A{@���@�G�@�y>@��@��K@��;@�~(@��e@�%@��x@���@���@�:�@��R@�?@�2a@��7@�S&@�F@�#:A F@���@��7@�k�@��@�.I@� i@�҉@�@��@�@�M@�x@��@�c@�~(@�GE@�@�*�@��j@��@꒣@�s�@��#@�]d@��@�O@���@�R@�IR@�k�@��@�6@�_�@��@�hs@��@�|�@뢜@�֡@�!@�@�"�@��@�W�@�$@���@�5?@�i�@��@�W?@���@��@�PH@�bN@�q�@��@�@�4�@ޚ�@ݻ0@�"h@�_p@�[�@�E9@�Ĝ@؃�@�\�@�5?@� �@�qv@�Y@��@���@ցo@�6�@�]�@Ԛ@�z�@�g8@�+k@���@ӯ�@Ӆ�@ҽ<@�	@�@�f�@��@�[�@�=�@��,@�1'@��y@�_@�33@��@�xl@�2�@� �@��@���@��@�_p@���@Ƈ+@��&@�.I@�͟@Ľ<@�5?@Ì~@ÄM@�\�@��s@¶�@�@�_@��;@�y�@���@��h@�_�@��@�ԕ@�j@�ی@�c�@�!�@��@�)_@��<@��@�j@�-@��@�X�@�ȴ@�;@���@��@�z�@�C-@���@�Mj@��`@��r@�@�a@�P�@�K�@�͟@�Ta@��@�ԕ@���@���@�b�@�Dg@�"�@��P@��H@���@�J@���@���@�|�@�Vm@�RT@��h@��@���@�!-@���@�~�@�C�@�  @��@�O�@�+@��O@�bN@�A�@�$@��@�M@�-@�M@�-�@��h@�%F@��`@�Ɇ@���@��1@��1@��_@�-@���@�qv@�2a@�*0@��@�1'@��g@���@�S&@�
=@���@�PH@��@��W@�,�@��@�q@��@��[@���@�hs@�>�@��@�Q@��r@��^@���@��$@�j�@��	@�u%@��d@�p�@�8�@�q@��`@���@�2�@�G@���@���@�F@���@�j@��@���@�;d@��P@��}@�ff@��@���@��@��4@�J�@��@���@�	l@���@�N�@�"h@��@��6@�g�@�4�@��@�ȴ@��@�� @��@���@��4@�X�@�7L@��"@���@��@��L@���@�}V@�`�@�H@�G@�ԕ@�e�@�.I@��@��f@�_�@�=q@�u@�x@�Q�@�H�@�8@���@�ѷ@�z�@�YK@�(�@�{@��@��t@�j�@�!�@��A@�Ta@���@��3@��@���@�l�@�tT@�7@� �@��@��@���@�˒@��K@��*@�~�@�Z�@�@O@�&@�o@��|@�|�@�1�@���@�6z@��M@���@���@��m@�rG@� i@�~(@�GE@���@��[@�o�@��@��!@��F@�h
@�9X@��@���@�_p@�0�@���@�u%@�[�@�H�@��@��@���@�a@�C�@�"�@�(@��8@���@�YK@��@o�@~^5@}�j@}�X@}?}@|�@|[�@{�r@{��@z�@z($@y��@y��@yF@y%@x�@x`�@w�Q@wj�@wC@v~�@v@�@v8�@vu@u�@u�@tZ@sخ@so�@s i@r}V@rkQ@rC�@r&�@q�@q��@p�@pN�@p�@p  @o��@o�4@oF�@o!-@nߤ@nl�@m�D@m��@m��@m��@m��@m�n@m�"@mzx@m�@l��@l�j@l��@lPH@l�@k�@k]�@j�"@j��@jff@jO@i�d@i^�@i@h��@h�@g�
@g�@f�H@f�B@f��@f:*@ezx@eIR@d�@dh�@d6@d7@d  @c� @c��@cy�@co@a�"@a;@`��@`m�@`�@_��@^�R@^$�@]�@]	l@\�o@\(�@[�@[!-@Z�X@Z �@Y�@Y=�@X�@XĜ@X��@XC-@W��@W�q@Wt�@V�6@Vl�@U�9@US&@T�@T�$@T�@S�k@S��@SiD@S4�@SC@S�@R�c@R�M@R��@R��@Rl�@R4@Q��@Qw2@P��@P~(@P  @O�@Oj�@O,�@N�M@N�m@N��@N{�@N;�@M4@L��@L(�@L�@K�A@K��@K��@K��@K��@K�	@KE9@J�8@J�8@J{�@JJ@I�@I��@I��@IS&@Hѷ@H�o@Hoi@HH@Gƨ@G�@Fd�@F�@E��@E��@E�@E��@E&�@D*�@C�$@C{J@C/�@B��@B��@B�\@BJ�@B4@A�T@A��@AF@A0�@@�|@@l"@@6@?�K@?��@?>�@?Y@>��@>�}@>��@>!�@=��@=a�@=*0@=@<��@<��@<m�@<I�@;�@;�f@;X�@;�@:z@:$�@9��@9o @8�@7��@7)_@6�y@6��@6ff@6&�@5�>@5��@5c�@50�@5�@4�@4�?@4��@4C-@4-�@44n@47@3��@3!-@3 i@2��@2}V@1��@0��@0Ĝ@0��@0[�@06@0	�@/�@/��@/RT@/O@/F�@/A�@/=@/�@.��@.�!@.:*@. �@-�T@-�X@-�@-`B@-(�@,�@,�e@,��@,�@,_@,:�@+��@+��@+E9@+o@*��@*��@*5?@)��@)��@)�M@)c�@)@(��@(`�@(/�@(	�@'˒@'��@'_p@'8@&��@&z@&$�@%�T@%�@%c�@%J�@% \@$�@$��@$m�@$<�@$@#�6@#�a@#��@#E9@#+@"_�@!��@!�@!�d@!�"@![W@!7L@ �@ �O@ �@ Q�@ 6@�@Z�@"�@@�H@�@�@��@GE@��@�z@��@|@o @rG@f�@?}@#�@��@7@�
@�@�$@O@�"@�x@u%@u%@xl@GE@�@�N@w2@Vm@@��@S�@:�@7@�Q@�@v`@@O@C@�@��@�@�@[W@+@�v@�@�p@�)@��@�z@��@j@`�@K^@9X@�@��@1�@�@�@��@{�@M�@6�@!�@@ԕ@�d@��@p�@\�@@��@�U@�@�@��@oi@U2@N�@D�@6@1'@,=@�@�W@� @~�@+@Y@�@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A̳3A̳�A̴9A̽qA̿HA���A̸�A̮Ą�A̦LA̟VA̟�ǍA�y>A�`vA�X�A�V�A�T�A�Q�A�RTA�QA�M�A�K)A�J�A�J#A�I�A�HA�FA�>A�;�A�8A�1[A�-A�֡AȼjA�ZA�W
A�k�A��A��RA�49A���A��A���A���A��hA�gA��A���A�B[A���A��6A���A���A���A�h>A�a�A�r�A���A�.A���A�B�A��A�XEA��tA��{A�h�A��jA��A��tA�4A��MA�fA�w2A��6A�A���A�.A�A��SA~[WA{��A{AwT�Aq��An�gAe_�A]	A[*�AW�rAU�OAT�gATJ�AQ/�AO}VALhsAH��AF>�AB� AA�AA��A@�A=�A<=A:$A8�A8/�A5�8A4u%A2��A0��A/�A--A,[�A+��A+A*�@A*jA*4A)��A&��A&v�A&?A%)�A!�VA �Al�A
=A�A��A4A��A�4A��A�A/�AM�A iAkQA�FA�A/A��A��A��A~�A�A�A+ARTA�A��AP�A�A��A�A�}A��Aw2A�oA��A��A��A�fA8AA�A�SA�AA;dA \A�A�SA�KA�A@AZA�A�A�A[WAD�A�A�A)�A�tA��A#:A�AیA|A
��A��AJA�A5?A�A�8AA��A��A�3A{JA�5A{@���@�G�@�y>@��@��K@��;@�~(@��e@�%@��x@���@���@�:�@��R@�?@�2a@��7@�S&@�F@�#:A F@���@��7@�k�@��@�.I@� i@�҉@�@��@�@�M@�x@��@�c@�~(@�GE@�@�*�@��j@��@꒣@�s�@��#@�]d@��@�O@���@�R@�IR@�k�@��@�6@�_�@��@�hs@��@�|�@뢜@�֡@�!@�@�"�@��@�W�@�$@���@�5?@�i�@��@�W?@���@��@�PH@�bN@�q�@��@�@�4�@ޚ�@ݻ0@�"h@�_p@�[�@�E9@�Ĝ@؃�@�\�@�5?@� �@�qv@�Y@��@���@ցo@�6�@�]�@Ԛ@�z�@�g8@�+k@���@ӯ�@Ӆ�@ҽ<@�	@�@�f�@��@�[�@�=�@��,@�1'@��y@�_@�33@��@�xl@�2�@� �@��@���@��@�_p@���@Ƈ+@��&@�.I@�͟@Ľ<@�5?@Ì~@ÄM@�\�@��s@¶�@�@�_@��;@�y�@���@��h@�_�@��@�ԕ@�j@�ی@�c�@�!�@��@�)_@��<@��@�j@�-@��@�X�@�ȴ@�;@���@��@�z�@�C-@���@�Mj@��`@��r@�@�a@�P�@�K�@�͟@�Ta@��@�ԕ@���@���@�b�@�Dg@�"�@��P@��H@���@�J@���@���@�|�@�Vm@�RT@��h@��@���@�!-@���@�~�@�C�@�  @��@�O�@�+@��O@�bN@�A�@�$@��@�M@�-@�M@�-�@��h@�%F@��`@�Ɇ@���@��1@��1@��_@�-@���@�qv@�2a@�*0@��@�1'@��g@���@�S&@�
=@���@�PH@��@��W@�,�@��@�q@��@��[@���@�hs@�>�@��@�Q@��r@��^@���@��$@�j�@��	@�u%@��d@�p�@�8�@�q@��`@���@�2�@�G@���@���@�F@���@�j@��@���@�;d@��P@��}@�ff@��@���@��@��4@�J�@��@���@�	l@���@�N�@�"h@��@��6@�g�@�4�@��@�ȴ@��@�� @��@���@��4@�X�@�7L@��"@���@��@��L@���@�}V@�`�@�H@�G@�ԕ@�e�@�.I@��@��f@�_�@�=q@�u@�x@�Q�@�H�@�8@���@�ѷ@�z�@�YK@�(�@�{@��@��t@�j�@�!�@��A@�Ta@���@��3@��@���@�l�@�tT@�7@� �@��@��@���@�˒@��K@��*@�~�@�Z�@�@O@�&@�o@��|@�|�@�1�@���@�6z@��M@���@���@��m@�rG@� i@�~(@�GE@���@��[@�o�@��@��!@��F@�h
@�9X@��@���@�_p@�0�@���@�u%@�[�@�H�@��@��@���@�a@�C�@�"�@�(@��8@���@�YK@��@o�@~^5@}�j@}�X@}?}@|�@|[�@{�r@{��@z�@z($@y��@y��@yF@y%@x�@x`�@w�Q@wj�@wC@v~�@v@�@v8�@vu@u�@u�@tZ@sخ@so�@s i@r}V@rkQ@rC�@r&�@q�@q��@p�@pN�@p�@p  @o��@o�4@oF�@o!-@nߤ@nl�@m�D@m��@m��@m��@m��@m�n@m�"@mzx@m�@l��@l�j@l��@lPH@l�@k�@k]�@j�"@j��@jff@jO@i�d@i^�@i@h��@h�@g�
@g�@f�H@f�B@f��@f:*@ezx@eIR@d�@dh�@d6@d7@d  @c� @c��@cy�@co@a�"@a;@`��@`m�@`�@_��@^�R@^$�@]�@]	l@\�o@\(�@[�@[!-@Z�X@Z �@Y�@Y=�@X�@XĜ@X��@XC-@W��@W�q@Wt�@V�6@Vl�@U�9@US&@T�@T�$@T�@S�k@S��@SiD@S4�@SC@S�@R�c@R�M@R��@R��@Rl�@R4@Q��@Qw2@P��@P~(@P  @O�@Oj�@O,�@N�M@N�m@N��@N{�@N;�@M4@L��@L(�@L�@K�A@K��@K��@K��@K��@K�	@KE9@J�8@J�8@J{�@JJ@I�@I��@I��@IS&@Hѷ@H�o@Hoi@HH@Gƨ@G�@Fd�@F�@E��@E��@E�@E��@E&�@D*�@C�$@C{J@C/�@B��@B��@B�\@BJ�@B4@A�T@A��@AF@A0�@@�|@@l"@@6@?�K@?��@?>�@?Y@>��@>�}@>��@>!�@=��@=a�@=*0@=@<��@<��@<m�@<I�@;�@;�f@;X�@;�@:z@:$�@9��@9o @8�@7��@7)_@6�y@6��@6ff@6&�@5�>@5��@5c�@50�@5�@4�@4�?@4��@4C-@4-�@44n@47@3��@3!-@3 i@2��@2}V@1��@0��@0Ĝ@0��@0[�@06@0	�@/�@/��@/RT@/O@/F�@/A�@/=@/�@.��@.�!@.:*@. �@-�T@-�X@-�@-`B@-(�@,�@,�e@,��@,�@,_@,:�@+��@+��@+E9@+o@*��@*��@*5?@)��@)��@)�M@)c�@)@(��@(`�@(/�@(	�@'˒@'��@'_p@'8@&��@&z@&$�@%�T@%�@%c�@%J�@% \@$�@$��@$m�@$<�@$@#�6@#�a@#��@#E9@#+@"_�@!��@!�@!�d@!�"@![W@!7L@ �@ �O@ �@ Q�@ 6@�@Z�@"�@@�H@�@�@��@GE@��@�z@��@|@o @rG@f�@?}@#�@��@7@�
@�@�$@O@�"@�x@u%@u%@xl@GE@�@�N@w2@Vm@@��@S�@:�@7@�Q@�@v`@@O@C@�@��@�@�@[W@+@�v@�@�p@�)@��@�z@��@j@`�@K^@9X@�@��@1�@�@�@��@{�@M�@6�@!�@@ԕ@�d@��@p�@\�@@��@�U@�@�@��@oi@U2@N�@D�@6@1'@,=@�@�W@� @~�@+@Y@�@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	1B	KB	eB	1B	KB	KB	1B	�B	�B	�B	_B	_B	+B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	$B	�B	B	MB	B	�B	uB��B	�B	"�B	5tB	\�B	y	B	�B	��B	��B
v�B
��B
�;B�B0�B9�BRoBMBCGBF?BS�BwB��B��B�?B��B��B�mB�B��B�B�YB�Bs�B{JB�KB��Bs�BK�B�B
�B
�fB
��B
|B
\�B
"hB	��B	��B	�\B	�1B	��B	{JB	:�B	B	2B	�B	EB	B	*�B	.cB	!�B	�B	tB	3B	jB	�B	 B��B�dB�}B	YB	�B	 B	B	"�B	%B	!�B	VB	�B	"4B	 �B	�B	$B	aB	}B	~B	�B	�B	 �B��B�'B̈́B�B�KBʦB�YB��B��B��B��BуB��B�B��B	{B	�B	�B	%`B	="B	QNB	ZB	k6B	��B	��B	�oB	�NB	׍B	�bB	��B	��B	��B	�*B	�B	�B	ބB	ݘB	��B	��B	ݲB	��B	ٚB	�NB	�B	�_B	��B	��B	��B	�B	�SB	�GB	�{B	�JB	��B	��B	��B	�1B	��B	��B	ѝB	�DB	�=B	�RB	̈́B	��B	�B	ЗB	��B	��B	��B	��B	�B	�XB	�XB	�+B	�B	�B	͟B	ŢB	��B	��B	�B	��B	��B	��B	��B	�RB	�B	��B	��B	��B	�$B	��B	��B	��B	��B	�aB	��B	��B	żB	چB	�B	�dB	�`B	��B	�.B	��B	�hB	��B	�B	�B	� B	�_B	��B	�QB	��B	�6B	�sB	��B	��B	��B	��B	�6B	�-B	ݘB	� B	�qB	�oB	��B	�B	�B
�B
�B
pB
dB
xB
�B

�B

�B
�B
�B
xB

�B
	�B
�B
�B
�B
�B	�cB	��B	��B	�WB	��B	�$B	�B	�UB	�-B	��B	�nB	�9B	�hB	�AB	�5B	�B	�zB	�B	��B	�IB	ܒB	�CB	��B	��B	�=B	��B	�~B	ݘB	ݘB	�B	ބB	��B	�NB	�nB	�ZB	�B	�kB	�B	�!B	�/B	�/B	��B	�)B	�B	��B	�B	�eB	�yB	�0B	��B	�6B	�qB	�WB	�=B	�/B	�/B	�B	�=B	��B	�B	�)B	��B	�}B	�B	��B	�B	�B	��B	�B	�B	��B	��B	�B	��B	��B	��B	��B	�?B	��B	�LB	�zB	��B	��B	��B	�FB	��B	��B	��B	�B	��B	��B	��B	�B	�lB	�RB	�>B	�	B	�*B	�xB	�xB	�0B	��B	�JB	�JB	�JB	�JB	��B	�dB	�jB	��B	�B	��B	��B	�qB	��B	�VB	�qB	��B	��B	��B	�B	�cB	�HB	��B	��B	�B	�wB	��B	��B	�B	�^B	�^B	�dB	�B	�6B	��B	��B	�B	��B	�BB	��B
 B
�B
B
�B
 �B
 �B
 �B
�B
B
�B
�B
�B
�B
'B
�B
�B
 OB	��B
 �B
[B
�B
gB
9B
�B
B
B
�B
tB
B
�B
�B
�B
�B
B
%B
tB
tB
YB
YB
tB
�B
�B
1B
�B
�B
fB
�B
	�B

#B

#B

#B

rB

�B
DB
xB
�B
�B
B
"B
�B
jB
"B
pB
�B
vB
�B
HB
�B
�B
�B
B
oB
 B
:B
�B
�B
&B
�B
�B
[B
�B
MB
B
�B
�B
�B
�B
�B
�B
�B
�B

B
YB
+B
_B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
B
�B
/B
/B
B
B
B
�B
�B
dB
B
5B
jB
�B
�B
�B
�B
VB
 BB
 �B
 �B
 �B
 �B
!HB
#B
#nB
#�B
$@B
$�B
$�B
$@B
%,B
%�B
&�B
'�B
'�B
(
B
(�B
)DB
)yB
)*B
)B
)�B
*B
*B
*�B
+B
+6B
,B
,�B
,�B
,�B
-CB
-�B
-�B
.B
.IB
.IB
./B
./B
.�B
.�B
.IB
.�B
.�B
/OB
/�B
/�B
0B
0�B
1'B
1AB
1AB
1�B
2GB
2GB
2GB
2GB
2|B
2|B
33B
3�B
3�B
4nB
4�B
4�B
4�B
4�B
5tB
5�B
6FB
6�B
6�B
7LB
7LB
7fB
7fB
7�B
7�B
8lB
9	B
9>B
9$B
9>B
9�B
9�B
9�B
:xB
:�B
:�B
:�B
:�B
:�B
:�B
:�B
:�B
:�B
;JB
;JB
;JB
;dB
;�B
<6B
<jB
<�B
<�B
<�B
<�B
=B
=<B
=�B
=�B
=�B
=�B
>BB
>�B
>�B
?B
?B
?cB
@B
@B
@OB
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
B[B
B[B
BuB
B�B
B�B
CB
C�B
C�B
D3B
D�B
D�B
EB
ESB
E�B
E�B
FtB
F�B
F�B
GB
G+B
GEB
GzB
G�B
G�B
G�B
H�B
H�B
I7B
I�B
I�B
J	B
J�B
KB
J�B
KB
K^B
KxB
K�B
K�B
KxB
K�B
K�B
K�B
LB
L0B
L~B
L�B
MB
MjB
M�B
M�B
M�B
M�B
N"B
NB
N"B
NB
N�B
O(B
O�B
O�B
O�B
O�B
O�B
P.B
P.B
P.B
PbB
PHB
P�B
QNB
Q�B
R B
R:B
RTB
R�B
R�B
RoB
RTB
RoB
S@B
S�B
S�B
S�B
S�B
S�B
S�B
TFB
TaB
T�B
UB
T�B
UMB
U2B
U2B
U�B
U�B
U�B
U�B
VB
VSB
VB
V9B
V�B
V�B
W?B
WsB
W�B
W�B
W�B
W�B
W�B
W�B
XB
XEB
X+B
XEB
XEB
X�B
X�B
X�B
X�B
YKB
Y1B
YeB
Y�B
ZB
ZB
Z7B
ZQB
Z�B
Z�B
Z�B
Z�B
Z�B
[#B
[=B
[qB
[�B
[�B
[�B
\B
\B
\)B
\�B
]�B
]�B
]�B
]~B
]�B
]~B
]�B
]�B
^�B
_!B
_;B
_VB
_�B
_�B
_�B
_�B
`B
`\B
`\B
`\B
`vB
`vB
`�B
`�B
aB
a|B
a|B
a|B
a�B
a�B
a�B
bB
bhB
b�B
c B
c B
cnB
c�B
c�B
c�B
c�B
d&B
dZB
eB
e`B
ezB
e�B
e�B
f2B
f�B
gB
g8B
gRB
gmB
g�B
g�B
g�B
g�B
h
B
h$B
hsB
h�B
iyB
i�B
i�B
i�B
jB
j0B
jeB
jB
j�B
j�B
j�B
j�B
j�B
j�B
k�B
lB
lB
lB
l=B
lqB
lqB
l�B
l�B
mB
m)B
mB
m)B
m�B
m�B
m�B
nB
nB
nB
n/B
ncB
n�B
n�B
n�B
oOB
oOB
oOB
oiB
oiB
o�B
o�B
poB
p�B
p�B
p�B
p�B
qAB
q�B
q�B
q�B
q�B
rB
r-B
rGB
r�B
r�B
r�B
sMB
s�B
s�B
s�B
s�B
tB
tB
tnB
tnB
t�B
t�B
uZB
u�B
u�B
vB
v`B
v`B
v`B
v`B
vzB
v�B
v�B
v�B
v�B
v�B
v�B
wB
w�B
w�B
xB
xRB
xRB
xlB
x�B
x�B
x�B
x�B
x�B
x�B
y>B
y>B
y>B
y�B
y�B
y�B
y�B
zB
zB
zDB
zDB
z^B
zxB
z^B
z^B
z^B
zxB
z�B
z�B
{JB
{B
{�B
{�B
{0111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	1B	KB	eB	1B	KB	KB	1B	�B	�B	�B	_B	_B	+B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	$B	�B	B	MB	B	�B	uB��B	�B	"�B	5tB	\�B	y	B	�B	��B	��B
v�B
��B
�;B�B0�B9�BRoBMBCGBF?BS�BwB��B��B�?B��B��B�mB�B��B�B�YB�Bs�B{JB�KB��Bs�BK�B�B
�B
�fB
��B
|B
\�B
"hB	��B	��B	�\B	�1B	��B	{JB	:�B	B	2B	�B	EB	B	*�B	.cB	!�B	�B	tB	3B	jB	�B	 B��B�dB�}B	YB	�B	 B	B	"�B	%B	!�B	VB	�B	"4B	 �B	�B	$B	aB	}B	~B	�B	�B	 �B��B�'B̈́B�B�KBʦB�YB��B��B��B��BуB��B�B��B	{B	�B	�B	%`B	="B	QNB	ZB	k6B	��B	��B	�oB	�NB	׍B	�bB	��B	��B	��B	�*B	�B	�B	ބB	ݘB	��B	��B	ݲB	��B	ٚB	�NB	�B	�_B	��B	��B	��B	�B	�SB	�GB	�{B	�JB	��B	��B	��B	�1B	��B	��B	ѝB	�DB	�=B	�RB	̈́B	��B	�B	ЗB	��B	��B	��B	��B	�B	�XB	�XB	�+B	�B	�B	͟B	ŢB	��B	��B	�B	��B	��B	��B	��B	�RB	�B	��B	��B	��B	�$B	��B	��B	��B	��B	�aB	��B	��B	żB	چB	�B	�dB	�`B	��B	�.B	��B	�hB	��B	�B	�B	� B	�_B	��B	�QB	��B	�6B	�sB	��B	��B	��B	��B	�6B	�-B	ݘB	� B	�qB	�oB	��B	�B	�B
�B
�B
pB
dB
xB
�B

�B

�B
�B
�B
xB

�B
	�B
�B
�B
�B
�B	�cB	��B	��B	�WB	��B	�$B	�B	�UB	�-B	��B	�nB	�9B	�hB	�AB	�5B	�B	�zB	�B	��B	�IB	ܒB	�CB	��B	��B	�=B	��B	�~B	ݘB	ݘB	�B	ބB	��B	�NB	�nB	�ZB	�B	�kB	�B	�!B	�/B	�/B	��B	�)B	�B	��B	�B	�eB	�yB	�0B	��B	�6B	�qB	�WB	�=B	�/B	�/B	�B	�=B	��B	�B	�)B	��B	�}B	�B	��B	�B	�B	��B	�B	�B	��B	��B	�B	��B	��B	��B	��B	�?B	��B	�LB	�zB	��B	��B	��B	�FB	��B	��B	��B	�B	��B	��B	��B	�B	�lB	�RB	�>B	�	B	�*B	�xB	�xB	�0B	��B	�JB	�JB	�JB	�JB	��B	�dB	�jB	��B	�B	��B	��B	�qB	��B	�VB	�qB	��B	��B	��B	�B	�cB	�HB	��B	��B	�B	�wB	��B	��B	�B	�^B	�^B	�dB	�B	�6B	��B	��B	�B	��B	�BB	��B
 B
�B
B
�B
 �B
 �B
 �B
�B
B
�B
�B
�B
�B
'B
�B
�B
 OB	��B
 �B
[B
�B
gB
9B
�B
B
B
�B
tB
B
�B
�B
�B
�B
B
%B
tB
tB
YB
YB
tB
�B
�B
1B
�B
�B
fB
�B
	�B

#B

#B

#B

rB

�B
DB
xB
�B
�B
B
"B
�B
jB
"B
pB
�B
vB
�B
HB
�B
�B
�B
B
oB
 B
:B
�B
�B
&B
�B
�B
[B
�B
MB
B
�B
�B
�B
�B
�B
�B
�B
�B

B
YB
+B
_B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
B
�B
/B
/B
B
B
B
�B
�B
dB
B
5B
jB
�B
�B
�B
�B
VB
 BB
 �B
 �B
 �B
 �B
!HB
#B
#nB
#�B
$@B
$�B
$�B
$@B
%,B
%�B
&�B
'�B
'�B
(
B
(�B
)DB
)yB
)*B
)B
)�B
*B
*B
*�B
+B
+6B
,B
,�B
,�B
,�B
-CB
-�B
-�B
.B
.IB
.IB
./B
./B
.�B
.�B
.IB
.�B
.�B
/OB
/�B
/�B
0B
0�B
1'B
1AB
1AB
1�B
2GB
2GB
2GB
2GB
2|B
2|B
33B
3�B
3�B
4nB
4�B
4�B
4�B
4�B
5tB
5�B
6FB
6�B
6�B
7LB
7LB
7fB
7fB
7�B
7�B
8lB
9	B
9>B
9$B
9>B
9�B
9�B
9�B
:xB
:�B
:�B
:�B
:�B
:�B
:�B
:�B
:�B
:�B
;JB
;JB
;JB
;dB
;�B
<6B
<jB
<�B
<�B
<�B
<�B
=B
=<B
=�B
=�B
=�B
=�B
>BB
>�B
>�B
?B
?B
?cB
@B
@B
@OB
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
B[B
B[B
BuB
B�B
B�B
CB
C�B
C�B
D3B
D�B
D�B
EB
ESB
E�B
E�B
FtB
F�B
F�B
GB
G+B
GEB
GzB
G�B
G�B
G�B
H�B
H�B
I7B
I�B
I�B
J	B
J�B
KB
J�B
KB
K^B
KxB
K�B
K�B
KxB
K�B
K�B
K�B
LB
L0B
L~B
L�B
MB
MjB
M�B
M�B
M�B
M�B
N"B
NB
N"B
NB
N�B
O(B
O�B
O�B
O�B
O�B
O�B
P.B
P.B
P.B
PbB
PHB
P�B
QNB
Q�B
R B
R:B
RTB
R�B
R�B
RoB
RTB
RoB
S@B
S�B
S�B
S�B
S�B
S�B
S�B
TFB
TaB
T�B
UB
T�B
UMB
U2B
U2B
U�B
U�B
U�B
U�B
VB
VSB
VB
V9B
V�B
V�B
W?B
WsB
W�B
W�B
W�B
W�B
W�B
W�B
XB
XEB
X+B
XEB
XEB
X�B
X�B
X�B
X�B
YKB
Y1B
YeB
Y�B
ZB
ZB
Z7B
ZQB
Z�B
Z�B
Z�B
Z�B
Z�B
[#B
[=B
[qB
[�B
[�B
[�B
\B
\B
\)B
\�B
]�B
]�B
]�B
]~B
]�B
]~B
]�B
]�B
^�B
_!B
_;B
_VB
_�B
_�B
_�B
_�B
`B
`\B
`\B
`\B
`vB
`vB
`�B
`�B
aB
a|B
a|B
a|B
a�B
a�B
a�B
bB
bhB
b�B
c B
c B
cnB
c�B
c�B
c�B
c�B
d&B
dZB
eB
e`B
ezB
e�B
e�B
f2B
f�B
gB
g8B
gRB
gmB
g�B
g�B
g�B
g�B
h
B
h$B
hsB
h�B
iyB
i�B
i�B
i�B
jB
j0B
jeB
jB
j�B
j�B
j�B
j�B
j�B
j�B
k�B
lB
lB
lB
l=B
lqB
lqB
l�B
l�B
mB
m)B
mB
m)B
m�B
m�B
m�B
nB
nB
nB
n/B
ncB
n�B
n�B
n�B
oOB
oOB
oOB
oiB
oiB
o�B
o�B
poB
p�B
p�B
p�B
p�B
qAB
q�B
q�B
q�B
q�B
rB
r-B
rGB
r�B
r�B
r�B
sMB
s�B
s�B
s�B
s�B
tB
tB
tnB
tnB
t�B
t�B
uZB
u�B
u�B
vB
v`B
v`B
v`B
v`B
vzB
v�B
v�B
v�B
v�B
v�B
v�B
wB
w�B
w�B
xB
xRB
xRB
xlB
x�B
x�B
x�B
x�B
x�B
x�B
y>B
y>B
y>B
y�B
y�B
y�B
y�B
zB
zB
zDB
zDB
z^B
zxB
z^B
z^B
z^B
zxB
z�B
z�B
{JB
{B
{�B
{�B
{0111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105237  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604191920  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604191920  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604191920                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605041928  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605041928  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610151508                      G�O�G�O�G�O�                