CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:17:27Z creation;2022-06-04T19:17:27Z conversion to V3.1      
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
_FillValue                 �  I<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M,   PRES_ADJUSTED_QC         
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
_FillValue                 �  �@   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �0   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
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
resolution        :�o     �  �D   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ޼   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �    HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �$   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �(   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �h   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �x   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �|   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604191727  20220610151508  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               'A   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @�p�З�1   @�q��0�@-�������c�C��%1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @33@y��@�  A��A   A@  A^ffA�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�33B�  B�  B�  B���B�  B�  B�  B���B�  B�ffB���B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0�C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�3D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ?�p�@e�@�@���A�HA:�HAYG�Az�HA�p�A�p�A�p�A�p�A�p�A�p�A�p�A�p�B�RB�RB�RB�RB&�RB.�RB6�RB>�RBF�RBN�RBV�RB^�RBf�RBn�RBv�RB~�RB��\B�\)B�\)B�\)B���B�\)B�\)B�\)B�(�B�\)B�B�(�B�\)B�\)B��\B��\B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\B�\)B�\)B�\)B�\)C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/ǮC1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��
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
C��=C��
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
D k�D �Dk�D�Dk�D�Dk�D�Dk�D�Dk�D�Dk�D�Dk�D�Dk�D�D	k�D	�D
k�D
�Dk�D�Dk�D�Dk�D�Dk�D�Dk�D�Dk�D�Dk�D�Dk�D�Dk�D�Dk�D�Dk�D�Dk�D�Dk�D�Dk�D�Dk�D�Dk�D�Dk�D�Dk�D�Dk�D��Dk�D�Dk�D�D k�D �D!k�D!�D"k�D"�D#k�D#�D$k�D$�D%k�D%�D&k�D&�D'k�D'�D(k�D(�D)k�D)�D*k�D*�D+k�D+�D,k�D,�D-k�D-�D.k�D.�D/k�D/�D0k�D0�D1k�D1�D2k�D2�D3k�D3�D4k�D4�D5k�D5�D6k�D6�D7k�D7�D8k�D8�D9k�D9�D:k�D:�D;k�D;�D<k�D<�D=k�D=�D>k�D>�D?k�D?�D@k�D@�DAk�DA�DBk�DB�DCk�DC�DDk�DD�DEk�DE�DFk�DF�DGk�DG�DHk�DH�DIk�DI�DJk�DJ�DKk�DK�DLk�DL�DMk�DM�DNk�DN�DOk�DO�DPk�DP�DQk�DQ�DRk�DR�DSk�DS�DTk�DT�DUk�DU�DVk�DV�DWk�DW�DXk�DX�DYk�DY�DZk�DZ�D[k�D[�D\k�D\�D]k�D]�D^k�D^�D_k�D_�D`k�D`�Dak�Da�Dbk�Db�Dck�Dc�Ddk�Dd�Dek�De�Dfk�Df�Dgk�Dg�Dhk�Dh�Dik�Di�Djk�Dj�Dkk�Dk�Dlk�Dl�Dmk�Dm�Dnk�Dn�Dok�Do�Dpk�Dp�Dqk�Dq�Drk�Dr�Dsk�Ds�Dtk�Dt�Duk�Du�Dvk�Dv�Dwk�Dw�Dxk�Dx�Dyk�Dy�Dzk�Dz�D{k�D{�D|k�D|�D}k�D}�D~k�D~�Dk�D�D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�x�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�Dµ�D���D�5�D�u�Dõ�D���D�5�D�u�Dĵ�D���D�5�D�u�Dŵ�D���D�5�D�u�DƵ�D���D�5�D�u�Dǵ�D���D�5�D�u�Dȵ�D���D�5�D�u�Dɵ�D���D�5�D�u�Dʵ�D���D�5�D�u�D˵�D���D�5�D�u�D̵�D���D�5�D�u�D͵�D���D�5�D�u�Dε�D���D�5�D�u�Dϵ�D���D�5�D�u�Dе�D���D�5�D�u�Dѵ�D���D�5�D�u�Dҵ�D���D�5�D�u�Dӵ�D���D�5�D�u�DԵ�D���D�5�D�u�Dյ�D���D�5�D�u�Dֵ�D���D�5�D�u�D׵�D���D�5�D�u�Dص�D���D�5�D�u�Dٵ�D���D�5�D�u�Dڵ�D���D�5�D�u�D۵�D���D�5�D�u�Dܵ�D���D�5�D�u�Dݵ�D���D�5�D�u�D޵�D���D�5�D�u�Dߵ�D���D�5�D�u�D��D���D�5�D�u�D��D���D�5�D�u�D��D���D�5�D�u�D��D���D�5�D�u�D��D���D�5�D�u�D��D���D�5�D�u�D��D���D�5�D�u�D��D���D�5�D�u�D��D���D�5�D�u�D��D���D�5�D�u�D��D���D�5�D�u�D��D���D�5�D�u�D��D���D�5�D�u�D���D���D�5�D�u�D��D���D�5�D�u�D��D���D�5�D�u�D��D���D�5�D�u�D��D���D�5�D�u�D��D���D�5�D�u�D��D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��AA��vA��A��%A��`A��2A���A���A��fA��A��8A���A��A���A���A��"A� �A�;A���A�AA��A��A�A��A��A�	lA��A�oA��A��A�1A˔�Aʪ�AɛqA��Aȗ$Aȗ�A�B�A�D�Aǅ�AƷ�AĄA�"�A�MA� �A�K)A���A�7LA���A���A�
�A��^A�u�A��A��$A��A�\�A�7�A�3�A��[A�,A�S�A��A��pA�O�A�AA���A�e�A�e�A��!A�VmA���A�P�A�&A�9�A���A�V�A���A�h�A��VA�[#A���A��A���A���A���A�HAy�Aw,�AuZ�Ar�AApݘAn�CAe|�Ad�hAcc�A^�A[ �AXw�AV~�AT�AQz�AOm]AL��AJ)_AH�qAF�DADg8A@یA>w2A;یA:7�A7��A6MjA5U2A4�A2TaA0T�A.�WA,i�A+��A+{A*+A(�_A'�"A&O�A%�oA$��A#��A#;dA"�{A!�PA ɆA n/A�A��A҉A �A��A�zA�FA��AM�A�cA'�AK�A1�A��A>BA�AیA �AZA��Am]A/A�AYA}�A
�oA
��A
MA	҉A	��A	+A�tA��A"hA�Ac�A8AE�AA�A�AA A�Av`A�A@AXAQA�]A� A�jA�KA�AA�A;dA��AbNA�}A�A�:A�A�IA �jA #:@�Ft@��z@��u@��+@��6@��@���@�#�@���@��<@�J@�W�@�@��A@��@�$t@퇔@�@�@��@�o @�p�@�h@�}V@��@��@�O@�a@���@�j�@�F@���@�֡@��@譬@�2a@���@�@�Q�@��@�R@�M@��@��f@�1�@�c@���@߷@�0�@�͟@ޗ�@�p;@��@��Z@��
@۴�@ۂ�@��@�GE@��@�C�@�@؃@��@׉7@�k�@���@�`�@ՖS@��@Դ9@ԃ�@�:�@��)@�7L@ҸR@�E�@��A@Ѧ�@�(�@�6@϶F@��@Ψ�@�:*@��D@�|@��@�:�@��@��s@��@ɀ4@�1�@�n�@��m@��>@�:�@�M�@� �@�
�@�@��;@�
=@�*�@�_p@��`@��c@��5@���@�e@�+�@� �@��*@��U@��@��@�_@���@�j@�;d@���@��b@�I�@���@���@��~@�ی@�xl@�C-@�>B@�GE@�+k@��@��@���@��~@�(@���@�M�@��@���@�;d@���@��@��H@���@���@�0�@���@��<@�7@�p�@���@���@�H@�ƨ@��P@��@��B@��u@�l"@��g@�zx@�c�@�H�@��|@��M@���@�Ft@��r@��X@�0�@��@�u�@�L�@��`@��F@��@��@���@�ں@�8�@�U�@���@���@��_@�m�@�W�@�7@��}@�o @�)_@��@���@���@���@�l"@���@�/@��y@��@�M�@���@��d@��@��t@�e�@��9@��I@��@�M�@�xl@�YK@���@�C�@��@���@�:*@���@�S&@�.I@�6z@�A @�A�@�&@���@�2�@��T@��@���@��.@��D@�v�@�Z�@�C�@�� @���@�W?@�+@��8@���@�u�@�@��t@�c@��@��+@�2�@��W@��m@��3@�RT@�Ɇ@��@��t@�Q�@��@���@�E�@��o@���@�X�@�8@��@��@��,@���@���@�� @�PH@���@���@��a@���@�RT@��@��@���@�g8@�W�@�#:@��@��j@���@��@�p�@�e,@�S&@�@���@���@�	l@�֡@�_@��m@��S@�u�@�8�@��@���@���@��z@�.�@��o@��D@�=@���@�S�@�x@��m@���@�u�@�N<@�'�@��@���@���@�j@�($@���@���@�j@�o@���@���@��z@�s�@�,=@��;@��n@�1�@��@���@��x@�:�@�4n@�1'@�@��@��@��n@�@O@��@��@��@��8@���@���@��'@�PH@�خ@��@��~@�`B@�IR@��@��@���@��@�@��m@�V@�"h@�&@Z�@~��@~#:@}�3@|��@|�)@|��@|2�@{]�@{�@{(@zv�@y�@x��@xD�@xS�@w��@w
=@v^5@u�M@uY�@t��@t�@t�e@t�@t�_@t@s(@r��@rc @r�@r��@r�@q<6@p�e@p@o��@o$t@n��@n �@m��@m`B@m2a@l�p@l,=@l1@k��@k$t@j�X@j�x@j� @j-@g�@g�$@g��@f��@f�b@fp;@e�@e#�@d��@d�e@dC-@c��@cl�@c@bp;@b($@a��@a4@`_@`,=@_�}@_,�@^�<@^��@^Ta@^$�@]@]`B@\�@\�_@\Q�@\'R@[�@[��@[RT@['�@Z�y@Z��@Z�@ZQ@Y�T@Y��@Yx�@Y/@X֡@X��@W��@W�@Wl�@WdZ@WA�@V�b@U��@U��@U?}@U�@T�[@T�O@T�D@Tj@S�g@S��@S��@SiD@R��@Q��@Q��@Q��@Qj@Qc�@Q\�@QDg@Pѷ@Pq@PG@O~�@N��@N��@NTa@M��@Mzx@L�U@L-�@K��@Ks@K�@J��@J�\@Ja|@Ik�@I�@I@H�@H��@H6@G��@G��@GU�@G=@G,�@Fߤ@F�@F5?@E��@E��@E��@E�t@E��@E[W@EV@D��@DtT@D"h@C�@CZ�@C@O@C=@B�L@B.�@B �@A�T@A��@@�[@@�@?��@?O@?o@>��@>��@>��@>� @>s�@>V@=�M@=+@<Ɇ@<�@<_@< �@;�@;�@;U�@:�8@:Ov@9��@9��@9o @9S&@9:�@9+@8�j@8]d@8~@7�F@7��@7g�@6��@6�}@6�x@6�+@6W�@65?@6�@5�o@5��@5@@4�K@4�4@4m�@4>B@3�@3�q@3dZ@2�,@23�@2@1�o@1ϫ@1�@1Y�@1@@0�U@0C-@/�Q@/��@/�:@/�4@.�s@.��@.c @.�@-��@-��@-^�@,�|@,�$@,c�@,@+�}@+a@+
=@*�@*��@*�L@*}V@*Ta@*-@)�#@)rG@)#�@(�`@(l"@(�@'��@'�[@'{J@'RT@&��@&��@&}V@&H�@%�t@%�~@%f�@$�)@$�@$��@$�o@$�@#qv@#�@#�@"��@":*@"	@"�@!��@!�9@!��@!c@!u�@!f�@!Dg@!�@!�@ �	@ �@ �@ ��@ ��@ ~(@ y>@ `�@ �@��@��@��@W?@@�"@��@�<@�L@^5@6�@@�@�3@�M@x�@u�@T�@�@�@�@>B@�&@��@�@�]@��@^5@YK@Ov@�=@(�@�`@g8@M@G@��@�@qv@A�@(@�@\�@�.@�j@o @V@�@�@�9@|�@A�@�@��@�@s@.I@�@��@�@�\@	@�@��@p�@L�@8�@-w@�|@�U@��@��@w�@N�@*�@�@�@�@�{@RT@=@+@ i@ߤ@�'@�A@J�@$�@��@a�@0�@�@�j@��@e�@�]@�a@g�@F�@$t@@
�m@
u%@
-@
�@
O@
�@
�@	�o@	�9@	��@	�7@	k�@	S&@	F@	-w@�	@ی@ѷ@�O@Z@�@��@��@��@��@��@��@X�@6z@C@
=@��@�@�b@d�@	@�@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��AA��vA��A��%A��`A��2A���A���A��fA��A��8A���A��A���A���A��"A� �A�;A���A�AA��A��A�A��A��A�	lA��A�oA��A��A�1A˔�Aʪ�AɛqA��Aȗ$Aȗ�A�B�A�D�Aǅ�AƷ�AĄA�"�A�MA� �A�K)A���A�7LA���A���A�
�A��^A�u�A��A��$A��A�\�A�7�A�3�A��[A�,A�S�A��A��pA�O�A�AA���A�e�A�e�A��!A�VmA���A�P�A�&A�9�A���A�V�A���A�h�A��VA�[#A���A��A���A���A���A�HAy�Aw,�AuZ�Ar�AApݘAn�CAe|�Ad�hAcc�A^�A[ �AXw�AV~�AT�AQz�AOm]AL��AJ)_AH�qAF�DADg8A@یA>w2A;یA:7�A7��A6MjA5U2A4�A2TaA0T�A.�WA,i�A+��A+{A*+A(�_A'�"A&O�A%�oA$��A#��A#;dA"�{A!�PA ɆA n/A�A��A҉A �A��A�zA�FA��AM�A�cA'�AK�A1�A��A>BA�AیA �AZA��Am]A/A�AYA}�A
�oA
��A
MA	҉A	��A	+A�tA��A"hA�Ac�A8AE�AA�A�AA A�Av`A�A@AXAQA�]A� A�jA�KA�AA�A;dA��AbNA�}A�A�:A�A�IA �jA #:@�Ft@��z@��u@��+@��6@��@���@�#�@���@��<@�J@�W�@�@��A@��@�$t@퇔@�@�@��@�o @�p�@�h@�}V@��@��@�O@�a@���@�j�@�F@���@�֡@��@譬@�2a@���@�@�Q�@��@�R@�M@��@��f@�1�@�c@���@߷@�0�@�͟@ޗ�@�p;@��@��Z@��
@۴�@ۂ�@��@�GE@��@�C�@�@؃@��@׉7@�k�@���@�`�@ՖS@��@Դ9@ԃ�@�:�@��)@�7L@ҸR@�E�@��A@Ѧ�@�(�@�6@϶F@��@Ψ�@�:*@��D@�|@��@�:�@��@��s@��@ɀ4@�1�@�n�@��m@��>@�:�@�M�@� �@�
�@�@��;@�
=@�*�@�_p@��`@��c@��5@���@�e@�+�@� �@��*@��U@��@��@�_@���@�j@�;d@���@��b@�I�@���@���@��~@�ی@�xl@�C-@�>B@�GE@�+k@��@��@���@��~@�(@���@�M�@��@���@�;d@���@��@��H@���@���@�0�@���@��<@�7@�p�@���@���@�H@�ƨ@��P@��@��B@��u@�l"@��g@�zx@�c�@�H�@��|@��M@���@�Ft@��r@��X@�0�@��@�u�@�L�@��`@��F@��@��@���@�ں@�8�@�U�@���@���@��_@�m�@�W�@�7@��}@�o @�)_@��@���@���@���@�l"@���@�/@��y@��@�M�@���@��d@��@��t@�e�@��9@��I@��@�M�@�xl@�YK@���@�C�@��@���@�:*@���@�S&@�.I@�6z@�A @�A�@�&@���@�2�@��T@��@���@��.@��D@�v�@�Z�@�C�@�� @���@�W?@�+@��8@���@�u�@�@��t@�c@��@��+@�2�@��W@��m@��3@�RT@�Ɇ@��@��t@�Q�@��@���@�E�@��o@���@�X�@�8@��@��@��,@���@���@�� @�PH@���@���@��a@���@�RT@��@��@���@�g8@�W�@�#:@��@��j@���@��@�p�@�e,@�S&@�@���@���@�	l@�֡@�_@��m@��S@�u�@�8�@��@���@���@��z@�.�@��o@��D@�=@���@�S�@�x@��m@���@�u�@�N<@�'�@��@���@���@�j@�($@���@���@�j@�o@���@���@��z@�s�@�,=@��;@��n@�1�@��@���@��x@�:�@�4n@�1'@�@��@��@��n@�@O@��@��@��@��8@���@���@��'@�PH@�خ@��@��~@�`B@�IR@��@��@���@��@�@��m@�V@�"h@�&@Z�@~��@~#:@}�3@|��@|�)@|��@|2�@{]�@{�@{(@zv�@y�@x��@xD�@xS�@w��@w
=@v^5@u�M@uY�@t��@t�@t�e@t�@t�_@t@s(@r��@rc @r�@r��@r�@q<6@p�e@p@o��@o$t@n��@n �@m��@m`B@m2a@l�p@l,=@l1@k��@k$t@j�X@j�x@j� @j-@g�@g�$@g��@f��@f�b@fp;@e�@e#�@d��@d�e@dC-@c��@cl�@c@bp;@b($@a��@a4@`_@`,=@_�}@_,�@^�<@^��@^Ta@^$�@]@]`B@\�@\�_@\Q�@\'R@[�@[��@[RT@['�@Z�y@Z��@Z�@ZQ@Y�T@Y��@Yx�@Y/@X֡@X��@W��@W�@Wl�@WdZ@WA�@V�b@U��@U��@U?}@U�@T�[@T�O@T�D@Tj@S�g@S��@S��@SiD@R��@Q��@Q��@Q��@Qj@Qc�@Q\�@QDg@Pѷ@Pq@PG@O~�@N��@N��@NTa@M��@Mzx@L�U@L-�@K��@Ks@K�@J��@J�\@Ja|@Ik�@I�@I@H�@H��@H6@G��@G��@GU�@G=@G,�@Fߤ@F�@F5?@E��@E��@E��@E�t@E��@E[W@EV@D��@DtT@D"h@C�@CZ�@C@O@C=@B�L@B.�@B �@A�T@A��@@�[@@�@?��@?O@?o@>��@>��@>��@>� @>s�@>V@=�M@=+@<Ɇ@<�@<_@< �@;�@;�@;U�@:�8@:Ov@9��@9��@9o @9S&@9:�@9+@8�j@8]d@8~@7�F@7��@7g�@6��@6�}@6�x@6�+@6W�@65?@6�@5�o@5��@5@@4�K@4�4@4m�@4>B@3�@3�q@3dZ@2�,@23�@2@1�o@1ϫ@1�@1Y�@1@@0�U@0C-@/�Q@/��@/�:@/�4@.�s@.��@.c @.�@-��@-��@-^�@,�|@,�$@,c�@,@+�}@+a@+
=@*�@*��@*�L@*}V@*Ta@*-@)�#@)rG@)#�@(�`@(l"@(�@'��@'�[@'{J@'RT@&��@&��@&}V@&H�@%�t@%�~@%f�@$�)@$�@$��@$�o@$�@#qv@#�@#�@"��@":*@"	@"�@!��@!�9@!��@!c@!u�@!f�@!Dg@!�@!�@ �	@ �@ �@ ��@ ��@ ~(@ y>@ `�@ �@��@��@��@W?@@�"@��@�<@�L@^5@6�@@�@�3@�M@x�@u�@T�@�@�@�@>B@�&@��@�@�]@��@^5@YK@Ov@�=@(�@�`@g8@M@G@��@�@qv@A�@(@�@\�@�.@�j@o @V@�@�@�9@|�@A�@�@��@�@s@.I@�@��@�@�\@	@�@��@p�@L�@8�@-w@�|@�U@��@��@w�@N�@*�@�@�@�@�{@RT@=@+@ i@ߤ@�'@�A@J�@$�@��@a�@0�@�@�j@��@e�@�]@�a@g�@F�@$t@@
�m@
u%@
-@
�@
O@
�@
�@	�o@	�9@	��@	�7@	k�@	S&@	F@	-w@�	@ی@ѷ@�O@Z@�@��@��@��@��@��@��@X�@6z@C@
=@��@�@�b@d�@	@�@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�B�fB�B�B�B�B�B��B��B��B��B��B�B�B�B�B�B�B��B�LB�fB�fB�fB�B�8B�8B��B�DB�QB�QB�"B�	B	BB	# B	9>B	S�B	q'B
 �B
��B
��B
��B�B�B�B)yBW$B��B��B��B��B��B�B� B��B͟B�pB��B��B��B��B��B�_B��B��B��B�B��B��B��BtnB_�Bd&BU�BAB3hB+�B1AB�B�B
��B
ݘB
��B
��B
}B
^�B
B�B
?�B
�B
 �B	�IB	��B	�.B	��B	bB	T�B	M�B	3�B	�B	oB	
#B	GB	 OB	�B��B�B�B�gBɆB�VB��B��B�|B�%B��B�FB�TB��B�B�$B�5B��B��B�&B��B� B�B��B�B�B��B��B�%B��B��B�nB��B��B�B� B�tB��B�tB��B̘BΥB�4B�uB�uB�:BԕB�B��B�FBևB��B��B�B�	B��B�ZB�fB��B�9B�$B�]B	�B	MB		�B	NB	B	�B	�B	$&B	72B	=�B	CGB	=�B	9	B	7�B	5%B	=VB	]IB	s�B	e�B	`\B	s�B	��B	�9B	�*B	��B	�kB	�OB	��B	��B	�KB	�@B	��B	�]B	�	B	��B	��B	��B	�TB	�4B	��B	��B	�9B	�BB	��B	��B	�0B	��B	��B	�0B	�KB	�=B	��B	�sB	�=B	�pB	��B	��B	��B	ɆB	�XB	��B	�)B	ʦB	��B	��B	��B	��B	�;B	�iB	��B	�.B	�wB	��B	��B	��B	�VB	��B	��B	�B	�iB	��B	�uB	�B	��B	ȚB	��B	��B	�JB	��B	οB	�.B	бB	��B	��B	ҽB	�&B	ӏB	�@B	�B	�FB	�gB	��B	�B	��B	�B	�B	֡B	��B	�YB	רB	��B	�_B	�B	��B	�_B	��B	��B	�$B	��B	��B	ӏB	��B	�}B	�bB	��B	��B	��B	�&B	�mB	׍B	�mB	�?B	��B	�WB	�/B	��B	ߊB	�HB	�B	��B	�B	��B	��B	�B	�HB	�-B	ߊB	�;B	�FB	��B	��B	�B	��B	�B	�_B	�yB	��B	��B	�B	�"B	�/B	��B	��B	�oB	�oB	�oB	�B	��B	�B	�B	�GB	�B	�B	�|B	�-B	�B	�B	�9B	�B	�9B	�nB	��B	��B	�B	�B	�?B	��B	��B	�B	�fB	��B	��B	��B	�LB	��B	�LB	�fB	��B	��B	�DB	�xB	�xB	��B	��B	��B	�JB	��B
 �B	�}B	��B
 4B	��B	��B	��B	�]B	��B	��B	��B	�BB	��B	��B	�.B	��B	��B	��B	��B	�]B	�B	��B	��B	�cB	��B	��B
 B
 �B
 �B
 �B
 �B
 OB
 �B
�B
�B
�B
B
�B
�B
?B
mB
B
�B
�B
B
?B
zB
�B
�B
	B
	�B

�B

�B

rB
B

�B
	�B
	�B
	�B
	�B
	�B

#B
DB
B

rB

XB

�B

�B
^B
�B
B
�B
�B
�B
�B
bB
hB
�B
 B
�B
�B
�B
�B
�B
NB
�B
 B
TB
oB
�B
�B
[B
�B
�B
�B
�B
B
�B
�B
�B
gB
�B
gB
�B
mB
?B
+B
B
yB
_B
�B
�B
�B
B
yB
�B
=B
�B
~B
/B
]B
)B
]B
B
�B
IB
�B
~B
�B
�B
�B
OB
�B
;B
pB
VB
B
�B
!B
B
;B
�B
 B
!B
!�B
!bB
!-B
!�B
!�B
"4B
"4B
"4B
"hB
"�B
#�B
#�B
$�B
%`B
%FB
%�B
%�B
%�B
%�B
%�B
&fB
&�B
'B
'�B
'�B
(>B
(XB
(sB
(XB
(sB
)B
*KB
+6B
,B
,B
+�B
+�B
+�B
,=B
-�B
/�B
/�B
0oB
0oB
0�B
1�B
1�B
2B
1�B
1�B
1�B
1�B
1[B
1AB
1�B
2�B
3�B
4TB
4�B
5?B
5�B
6B
72B
6+B
5�B
6B
6+B
7�B
8B
88B
8�B
8�B
9rB
9�B
:xB
:DB
;B
;B
;B
;�B
;�B
<B
<�B
=qB
=�B
>]B
>�B
?B
?B
?cB
?cB
?B
?.B
?cB
?}B
?�B
?HB
?.B
?.B
>�B
>�B
>�B
>�B
>�B
>�B
>wB
>]B
>�B
>�B
?HB
?�B
@ B
@OB
@4B
@iB
@�B
A;B
AUB
A�B
BAB
B�B
B�B
B�B
B�B
B�B
B�B
CGB
C�B
C�B
C�B
C�B
C�B
DMB
DMB
D�B
D�B
D�B
D�B
ESB
ESB
EmB
E�B
E�B
E�B
FYB
F�B
F�B
F�B
F�B
GzB
G_B
G�B
G�B
G�B
HB
HfB
H�B
HfB
IlB
IlB
IlB
I�B
I�B
JrB
J�B
J�B
J�B
J�B
J�B
J�B
KDB
K�B
K�B
LdB
L�B
L�B
L�B
L�B
M6B
M�B
NpB
N�B
N�B
O(B
OvB
O�B
OvB
P�B
P�B
P�B
P�B
Q B
Q4B
QhB
Q�B
Q�B
Q�B
Q�B
RB
R:B
RoB
R�B
R�B
R�B
R�B
R�B
R�B
R�B
SB
S[B
S[B
S�B
S�B
TB
S�B
T�B
T�B
T�B
T�B
T�B
U�B
VB
V9B
VmB
V�B
V�B
V�B
V�B
V�B
V�B
V�B
W�B
W�B
W�B
X+B
X+B
X_B
X_B
X�B
X�B
X�B
YeB
YB
Y�B
ZB
ZB
ZB
ZB
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
[=B
[qB
[qB
[qB
[�B
[�B
\B
\B
\�B
\�B
\�B
]/B
]/B
]/B
]�B
]~B
]~B
]�B
^�B
^jB
^�B
^�B
^�B
^�B
_B
_VB
_�B
`\B
`�B
`�B
`vB
`�B
`�B
`�B
aB
aHB
abB
a�B
bB
b4B
bhB
b�B
b�B
c B
cnB
c�B
c�B
c�B
c�B
c�B
c�B
dB
dZB
dtB
d�B
eB
eFB
e`B
e�B
e�B
e�B
fB
f�B
ffB
f�B
g8B
g8B
g8B
g�B
g�B
g�B
g�B
h>B
h�B
h�B
h�B
i�B
i�B
i�B
i�B
jB
j0B
jeB
jeB
jB
jeB
j�B
j�B
j�B
j�B
j�B
kB
kB
kB
kQB
k6B
kQB
k�B
k�B
k�B
k�B
l"B
l=B
lWB
lqB
l�B
l�B
m)B
m)B
m]B
m]B
m�B
m�B
m�B
m�B
m�B
m�B
n/B
nIB
n�B
n�B
o B
o�B
o�B
p!B
p;B
p!B
o�B
p�B
q'B
q[B
q�B
rB
rB
raB
r�B
r|B
r�B
r�B
r�B
s�B
s�B
s�B
tnB
t�B
t�B
t�B
t�B
u%B
uZB
u�B
u�B
u�B
v+B
v`B
v�B
v�B
v�B
v�B
wfB
w�B
w�B
xB
xB
xB
xB
xRB
x�B
x�B
x�B
x�B
x�B
y	B
y$B
y	B
y>B
y�B
y�B
y�B
y�B
z*B
z*B
z^B
z�B
z�B
z�B
{dB
{�B
{�B
|6B
|PB
|6B
|�B
|�B
}<B
}�B
}�B
}�B
}�B
~BB
~wB
B
B
B
B
.B
HB
HB
�B
�B
�B
�B
�B
�B
�B
�4B
�4B
�OB
��B
��B
�B
�oB
��B
��B
��B
��B
�B
�'B
�[B
�[B
�uB
��B
��B
�B
�GB
�GB
�{11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�B�fB�B�B�B�B�B��B��B��B��B��B�B�B�B�B�B�B��B�LB�fB�fB�fB�B�8B�8B��B�DB�QB�QB�"B�	B	BB	# B	9>B	S�B	q'B
 �B
��B
��B
��B�B�B�B)yBW$B��B��B��B��B��B�B� B��B͟B�pB��B��B��B��B��B�_B��B��B��B�B��B��B��BtnB_�Bd&BU�BAB3hB+�B1AB�B�B
��B
ݘB
��B
��B
}B
^�B
B�B
?�B
�B
 �B	�IB	��B	�.B	��B	bB	T�B	M�B	3�B	�B	oB	
#B	GB	 OB	�B��B�B�B�gBɆB�VB��B��B�|B�%B��B�FB�TB��B�B�$B�5B��B��B�&B��B� B�B��B�B�B��B��B�%B��B��B�nB��B��B�B� B�tB��B�tB��B̘BΥB�4B�uB�uB�:BԕB�B��B�FBևB��B��B�B�	B��B�ZB�fB��B�9B�$B�]B	�B	MB		�B	NB	B	�B	�B	$&B	72B	=�B	CGB	=�B	9	B	7�B	5%B	=VB	]IB	s�B	e�B	`\B	s�B	��B	�9B	�*B	��B	�kB	�OB	��B	��B	�KB	�@B	��B	�]B	�	B	��B	��B	��B	�TB	�4B	��B	��B	�9B	�BB	��B	��B	�0B	��B	��B	�0B	�KB	�=B	��B	�sB	�=B	�pB	��B	��B	��B	ɆB	�XB	��B	�)B	ʦB	��B	��B	��B	��B	�;B	�iB	��B	�.B	�wB	��B	��B	��B	�VB	��B	��B	�B	�iB	��B	�uB	�B	��B	ȚB	��B	��B	�JB	��B	οB	�.B	бB	��B	��B	ҽB	�&B	ӏB	�@B	�B	�FB	�gB	��B	�B	��B	�B	�B	֡B	��B	�YB	רB	��B	�_B	�B	��B	�_B	��B	��B	�$B	��B	��B	ӏB	��B	�}B	�bB	��B	��B	��B	�&B	�mB	׍B	�mB	�?B	��B	�WB	�/B	��B	ߊB	�HB	�B	��B	�B	��B	��B	�B	�HB	�-B	ߊB	�;B	�FB	��B	��B	�B	��B	�B	�_B	�yB	��B	��B	�B	�"B	�/B	��B	��B	�oB	�oB	�oB	�B	��B	�B	�B	�GB	�B	�B	�|B	�-B	�B	�B	�9B	�B	�9B	�nB	��B	��B	�B	�B	�?B	��B	��B	�B	�fB	��B	��B	��B	�LB	��B	�LB	�fB	��B	��B	�DB	�xB	�xB	��B	��B	��B	�JB	��B
 �B	�}B	��B
 4B	��B	��B	��B	�]B	��B	��B	��B	�BB	��B	��B	�.B	��B	��B	��B	��B	�]B	�B	��B	��B	�cB	��B	��B
 B
 �B
 �B
 �B
 �B
 OB
 �B
�B
�B
�B
B
�B
�B
?B
mB
B
�B
�B
B
?B
zB
�B
�B
	B
	�B

�B

�B

rB
B

�B
	�B
	�B
	�B
	�B
	�B

#B
DB
B

rB

XB

�B

�B
^B
�B
B
�B
�B
�B
�B
bB
hB
�B
 B
�B
�B
�B
�B
�B
NB
�B
 B
TB
oB
�B
�B
[B
�B
�B
�B
�B
B
�B
�B
�B
gB
�B
gB
�B
mB
?B
+B
B
yB
_B
�B
�B
�B
B
yB
�B
=B
�B
~B
/B
]B
)B
]B
B
�B
IB
�B
~B
�B
�B
�B
OB
�B
;B
pB
VB
B
�B
!B
B
;B
�B
 B
!B
!�B
!bB
!-B
!�B
!�B
"4B
"4B
"4B
"hB
"�B
#�B
#�B
$�B
%`B
%FB
%�B
%�B
%�B
%�B
%�B
&fB
&�B
'B
'�B
'�B
(>B
(XB
(sB
(XB
(sB
)B
*KB
+6B
,B
,B
+�B
+�B
+�B
,=B
-�B
/�B
/�B
0oB
0oB
0�B
1�B
1�B
2B
1�B
1�B
1�B
1�B
1[B
1AB
1�B
2�B
3�B
4TB
4�B
5?B
5�B
6B
72B
6+B
5�B
6B
6+B
7�B
8B
88B
8�B
8�B
9rB
9�B
:xB
:DB
;B
;B
;B
;�B
;�B
<B
<�B
=qB
=�B
>]B
>�B
?B
?B
?cB
?cB
?B
?.B
?cB
?}B
?�B
?HB
?.B
?.B
>�B
>�B
>�B
>�B
>�B
>�B
>wB
>]B
>�B
>�B
?HB
?�B
@ B
@OB
@4B
@iB
@�B
A;B
AUB
A�B
BAB
B�B
B�B
B�B
B�B
B�B
B�B
CGB
C�B
C�B
C�B
C�B
C�B
DMB
DMB
D�B
D�B
D�B
D�B
ESB
ESB
EmB
E�B
E�B
E�B
FYB
F�B
F�B
F�B
F�B
GzB
G_B
G�B
G�B
G�B
HB
HfB
H�B
HfB
IlB
IlB
IlB
I�B
I�B
JrB
J�B
J�B
J�B
J�B
J�B
J�B
KDB
K�B
K�B
LdB
L�B
L�B
L�B
L�B
M6B
M�B
NpB
N�B
N�B
O(B
OvB
O�B
OvB
P�B
P�B
P�B
P�B
Q B
Q4B
QhB
Q�B
Q�B
Q�B
Q�B
RB
R:B
RoB
R�B
R�B
R�B
R�B
R�B
R�B
R�B
SB
S[B
S[B
S�B
S�B
TB
S�B
T�B
T�B
T�B
T�B
T�B
U�B
VB
V9B
VmB
V�B
V�B
V�B
V�B
V�B
V�B
V�B
W�B
W�B
W�B
X+B
X+B
X_B
X_B
X�B
X�B
X�B
YeB
YB
Y�B
ZB
ZB
ZB
ZB
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
[=B
[qB
[qB
[qB
[�B
[�B
\B
\B
\�B
\�B
\�B
]/B
]/B
]/B
]�B
]~B
]~B
]�B
^�B
^jB
^�B
^�B
^�B
^�B
_B
_VB
_�B
`\B
`�B
`�B
`vB
`�B
`�B
`�B
aB
aHB
abB
a�B
bB
b4B
bhB
b�B
b�B
c B
cnB
c�B
c�B
c�B
c�B
c�B
c�B
dB
dZB
dtB
d�B
eB
eFB
e`B
e�B
e�B
e�B
fB
f�B
ffB
f�B
g8B
g8B
g8B
g�B
g�B
g�B
g�B
h>B
h�B
h�B
h�B
i�B
i�B
i�B
i�B
jB
j0B
jeB
jeB
jB
jeB
j�B
j�B
j�B
j�B
j�B
kB
kB
kB
kQB
k6B
kQB
k�B
k�B
k�B
k�B
l"B
l=B
lWB
lqB
l�B
l�B
m)B
m)B
m]B
m]B
m�B
m�B
m�B
m�B
m�B
m�B
n/B
nIB
n�B
n�B
o B
o�B
o�B
p!B
p;B
p!B
o�B
p�B
q'B
q[B
q�B
rB
rB
raB
r�B
r|B
r�B
r�B
r�B
s�B
s�B
s�B
tnB
t�B
t�B
t�B
t�B
u%B
uZB
u�B
u�B
u�B
v+B
v`B
v�B
v�B
v�B
v�B
wfB
w�B
w�B
xB
xB
xB
xB
xRB
x�B
x�B
x�B
x�B
x�B
y	B
y$B
y	B
y>B
y�B
y�B
y�B
y�B
z*B
z*B
z^B
z�B
z�B
z�B
{dB
{�B
{�B
|6B
|PB
|6B
|�B
|�B
}<B
}�B
}�B
}�B
}�B
~BB
~wB
B
B
B
B
.B
HB
HB
�B
�B
�B
�B
�B
�B
�B
�4B
�4B
�OB
��B
��B
�B
�oB
��B
��B
��B
��B
�B
�'B
�[B
�[B
�uB
��B
��B
�B
�GB
�GB
�{11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105235  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604191727  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604191727  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604191727                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605041735  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605041735  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610151508                      G�O�G�O�G�O�                