CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2016-06-28T04:58:54Z creation;2016-06-28T04:58:56Z conversion to V3.1;2019-12-19T08:39:50Z update;     
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
_FillValue                 �  IX   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  MH   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �\   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �L   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED            
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
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �h   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �h   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �h   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �h   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �8   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �H   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �L   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �\   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �`   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �d   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �hArgo profile    3.1 1.2 19500101000000  20160628045854  20200115101516  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA  I2_0576_001                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @ת��0��1   @ת�>�� @;��<64�dO��1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DCfDC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]fD]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D~��D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�<�D؀ D�� D�  D�@ Dـ D�� D�  D�C3Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dރ3D��3D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @~�R@�@�A�HA:�HAZ�HAz�HA�p�A�p�A�p�A�p�A�p�A�p�A�p�A�p�B�RB�RB�RB�RB&�RB.�RB6�RB>�RBF�RBN�RBV�RB^�RBf�RBn�RBv�RB~�RB�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��
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
C���C���C��
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
�Dk�D�Dk�D�Dk�D�Dk�D�Dk�D�Dk�D�Dk�D�Dk�D�Dk�D�Dk�D�Dk�D�Dk�D�Dk�D�Dk�D�Dk�D�Dk�D�Dk�D�Dk�D�Dk�D�Dk�D�Dk�D�D k�D �D!k�D!�D"k�D"�D#k�D#�D$k�D$�D%k�D%�D&k�D&�D'k�D'�D(k�D(�D)k�D)�D*k�D*�D+k�D+�D,k�D,�D-k�D-�D.k�D.�D/k�D/�D0k�D0�D1k�D1�D2k�D2�D3k�D3�D4k�D4�D5k�D5�D6k�D6�D7k�D7�D8k�D8�D9k�D9�D:k�D:�D;k�D;�D<k�D<�D=k�D=�D>k�D>�D?k�D?�D@k�D@�DAk�DA�DBk�DB��DCk�DC�DDk�DD�DEk�DE�DFk�DF�DGk�DG�DHk�DH�DIk�DI�DJk�DJ�DKk�DK�DLk�DL�DMk�DM�DNk�DN�DOk�DO�DPk�DP�DQk�DQ�DRk�DR�DSk�DS�DTk�DT�DUk�DU�DVk�DV�DWk�DW�DXk�DX�DYk�DY�DZk�DZ�D[k�D[�D\k�D\��D]k�D]�D^k�D^�D_k�D_�D`k�D`�Dak�Da�Dbk�Db�Dck�Dc�Ddk�Dd�Dek�De�Dfk�Df�Dgk�Dg�Dhk�Dh�Dik�Di�Djk�Dj�Dkk�Dk�Dlk�Dl�Dmk�Dm�Dnk�Dn�Dok�Do�Dpk�Dp�Dqk�Dq�Drk�Dr�Dsk�Ds�Dtk�Dt�Duk�Du�Dvk�Dv�Dwk�Dw�Dxk�Dx�Dyk�Dy�Dzk�Dz�D{k�D{�D|k�D|�D}k�D}�D~k�D~�Dk�D�D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�r�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�Dµ�D���D�5�D�u�Dõ�D���D�5�D�u�Dĵ�D���D�5�D�u�Dŵ�D���D�5�D�u�DƵ�D���D�5�D�u�Dǵ�D���D�5�D�u�Dȵ�D���D�5�D�u�Dɵ�D���D�5�D�u�Dʵ�D���D�5�D�u�D˵�D���D�5�D�u�D̵�D���D�5�D�u�D͵�D���D�5�D�u�Dε�D���D�5�D�u�Dϵ�D���D�5�D�u�Dе�D���D�5�D�u�Dѵ�D���D�5�D�u�Dҵ�D���D�5�D�u�Dӵ�D���D�5�D�u�DԵ�D���D�5�D�u�Dյ�D���D�5�D�u�Dֵ�D���D�5�D�u�D׵�D���D�2�D�u�Dص�D���D�5�D�u�Dٵ�D���D�8�D�u�Dڵ�D���D�5�D�u�D۵�D���D�5�D�u�Dܵ�D���D�5�D�u�Dݵ�D���D�5�D�x�D޸�D���D�5�D�u�Dߵ�D���D�5�D�u�D��D���D�5�D�u�D��D���D�5�D�u�D��D���D�5�D�u�D��D���D�5�D�u�D��D���D�5�D�u�D��D���D�5�D�u�D��D���D�5�D�u�D��D���D�5�D�u�D��D���D�5�D�u�D��D���D�5�D�u�D��D���D�5�D�u�D��D���D�5�D�u�D��D���D�5�D�u�D���D���D�5�D�u�D��D���D�5�D�u�D��D���D�5�D�u�D��D���D�5�D�u�D��D���D�5�D�u�D��D���D�5�D�u�D��D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�x�D���D��\11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A���A�  A�A�A�  A�A�A�A�A�  A�  A�A�  A���A���A���A���A���A���A���A��A��mA��TA��`A��HA���A���A��A�A�A�jA�1A��A��A�M�A��A�(�A�A�  A��A�p�A�bA��uA�I�A�ĜA�jA���A�I�A��A��-A���A�M�A�1A�A��A�K�A��A�A��A��A��HA�t�A�=qA�G�A���A��-A�A��;A��A�O�A���A�r�A�9XA�VA�"�A���A�1'A���A���A�(�A���A�;dA�bNA�~�A��A���A���A��PA�1A��RA�$�A��A���A�O�A�;dA��A�33A�ZA�?}A��A��;A��A�(�A���A�5?A��A"�A}�TA|E�Ax�Av��Av�Au��Au��AtQ�Aq�Aq&�ApI�Ao��AoC�An�`An��Am\)Ak�;Ak?}Aj�DAj=qAj$�Aj$�AiAi�AidZAh�HAh��Ah1'Ag�Ag�Af��Ae�Ad��AdVAb$�A`-A^ZA\��A\{A[dZA[AZ�DAY��AV��AV5?AU�AUl�AT�jASƨAR�AR{AO�
AMXAJ�9AI��AI
=AH��AH~�AH^5AHM�AH5?AH$�AG�AGl�AF-AEoAD~�AC�7AAK�A@�A?�A<1A;�A:bNA9��A8�DA7�
A6z�A4�A4ffA2��A1��A/�TA.��A.z�A,��A+VA*r�A*r�A*n�A(�A'�hA'7LA'VA&�yA&��A&1'A%A%7LA#�A"��A"bA!��A!�A JA��AĜA9XAbAJAA��A+A�A�A~�Av�Av�A�A��A\)AG�A;dAȴA�A$�A�AS�A|�A�`A^5A/A(�A&�AbNA�A/A
�/A
$�A	33A�uA�A��A�A�!A��Av�A�mA ��A bN@�S�@�n�@�{@��T@�p�@��P@�M�@��u@�b@���@�K�@�V@���@�Ĝ@�b@�dZ@�=q@�/@�@��D@�|�@�ff@��@��@�o@�ff@蛦@�M�@�K�@�^@�/@� �@�V@�j@��@�+@�n�@ٲ-@�x�@��@�dZ@��@��@�bN@��@��@Ϯ@�;d@�V@�G�@̬@˥�@ʏ\@�G�@�b@�ff@ũ�@��@ě�@��@�\)@�{@�Ĝ@�z�@��@�+@�@� �@��@�V@���@�o@�V@��^@�`B@��u@��m@��F@��@�@�/@���@�Q�@�  @�K�@��!@�n�@�5?@��@��#@���@�7L@�z�@�K�@�-@�p�@�Q�@�
=@��y@�~�@���@�G�@���@��@���@�z�@���@���@�x�@�/@�%@���@�Ĝ@��j@��9@���@��u@��D@�r�@�I�@�1'@��@��m@��@�;d@���@�v�@�-@�hs@�/@�z�@�1@��@��;@��w@���@�|�@�;d@��@�$�@���@�G�@�r�@�(�@� �@��@�ƨ@�+@�@��H@�ȴ@���@�n�@�$�@��@���@�%@�Ĝ@��u@�9X@�ƨ@��@�K�@�+@���@���@�^5@��@��h@�?}@��@��@���@��u@�I�@� �@��m@�ƨ@���@�\)@���@���@�&�@�j@���@���@�^5@�E�@�{@���@�X@��@��@�j@���@�l�@��@�$�@��T@���@��@�p�@���@�j@�@|�@�@~ff@}�-@}?}@|I�@{o@z��@zM�@y��@y�^@y��@y�7@y%@xr�@xQ�@xA�@w�@wl�@w;d@v�y@vff@u�@t�/@tz�@t9X@s�
@s@r�@r�@r�@r�@r��@r��@r=q@r�@q�@q��@p�9@o�w@oK�@o�@nȴ@nv�@nE�@n$�@n{@m��@m�-@m�h@m?}@l�@l�@l�@k�
@j��@j^5@j=q@j�@jJ@i�@i�#@i��@i�^@i��@i��@ihs@iX@iG�@i7L@i&�@i%@h��@hĜ@h�9@h�u@hbN@h1'@h  @g�w@f�y@e�-@e�h@e�@ep�@d�/@dI�@c��@cƨ@ct�@cC�@co@b�H@b^5@bJ@a�@a&�@`r�@`  @_��@_K�@_+@^��@^ȴ@^��@^v�@^$�@]?}@\�D@\�@[o@Z��@Z�\@Z�\@Zn�@Z�@Y�7@Y&�@X�9@XQ�@X �@X  @W�@W��@W�@W��@W��@W��@W�P@Wl�@Wl�@W\)@W;d@W+@W
=@V�y@V�+@V@U�@T�@T��@Tj@S�
@S��@S�@S�@St�@SC�@So@R�@R��@R��@R�\@RM�@Q�#@Q��@Qx�@QG�@Q�@P�`@P �@O�w@O+@N�y@Nȴ@N�+@Nff@N5?@N@M@M�@Mp�@M`B@M?}@M?}@M�@L�/@L�@L�@L�D@Lj@LI�@L9X@L(�@L�@L1@K��@K�m@Kƨ@K�@J�!@Jn�@J=q@I��@I�@I�@I�^@I��@Ix�@I7L@I&�@H��@Hr�@H �@G�@G|�@G;d@G
=@F�@F��@FE�@F@E��@EV@D�@Dj@C��@C��@C�@C"�@B��@B~�@B^5@BM�@BM�@A�#@@��@@�u@@ �@?��@?;d@>�y@>�@>ȴ@>��@>��@>v�@>ff@>V@>@=�T@=��@=/@=V@<��@<��@<��@<�@<��@<I�@;��@;@:=q@:J@9�@9�7@97L@8�`@8�u@8bN@8  @7��@6��@6V@65?@5�T@5p�@5�@4z�@49X@3��@3��@3t�@3C�@3C�@3"�@2�@2��@2�\@2n�@2�@1G�@0��@0�u@0�@0bN@0bN@/�@/;d@/
=@.��@/
=@/+@/l�@/�P@/|�@/;d@.ȴ@.��@.$�@-�@,��@,��@,��@,�j@,�j@,�j@,�@,��@,Z@,�@+��@+@*�!@*-@)��@)x�@)x�@)X@)x�@)x�@)�^@)&�@(��@(��@(��@(�9@(A�@(b@'�w@'+@&��@'
=@&v�@%�@%�T@%�-@%O�@$z�@$Z@$I�@#��@#�@#C�@#"�@#"�@#o@#@"�@"�@"�@"�@"�@"�H@"��@"��@"�\@!�^@!�@ Ĝ@ r�@ 1'@ b@�;@��@l�@\)@K�@;d@
=@�R@v�@{@�T@@�h@�@p�@�@��@�@1@Z@ƨ@�@~�@M�@=q@J@�#@��@��@x�@hs@hs@hs@G�@�@Ĝ@r�@A�@ �@  @�@��@�P@\)@\)@\)@+@��@ȴ@��@��@��@��@ff@5?@@�@�T@�@/@V@�@�@Z@(�@��@�F@��@�@t�@t�@dZ@S�@C�@o@��@n�@�@��@x�@X@7L@%@Ĝ@��@r�@r�@bN@1'@  @�@�@�y@�y@�y@ȴ@�+@ff@V@$�@�@�T@�T@�T@�T@@��@�h@�h@�h@p�@V@�j@1@�
@ƨ@��@�@�@�@S�@C�@o@
�H@
��@
�!@
~�@
=q@	�@	��@	��@	�7@	x�@	hs@	X@	G�@	7L@	�@��@Ĝ@�@r�@A�@  @�@�P@|�@+@ȴ@��@��@ff@E�@$�@�@��@@�-@�-@��@��@O�@?}@�@��@�/@�@��@�D@j@Z@9X@(�@ƨ@S�@C�@33@@�@�H@�H@��@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A���A�  A�A�A�  A�A�A�A�A�  A�  A�A�  A���A���A���A���A���A���A���A��A��mA��TA��`A��HA���A���A��A�A�A�jA�1A��A��A�M�A��A�(�A�A�  A��A�p�A�bA��uA�I�A�ĜA�jA���A�I�A��A��-A���A�M�A�1A�A��A�K�A��A�A��A��A��HA�t�A�=qA�G�A���A��-A�A��;A��A�O�A���A�r�A�9XA�VA�"�A���A�1'A���A���A�(�A���A�;dA�bNA�~�A��A���A���A��PA�1A��RA�$�A��A���A�O�A�;dA��A�33A�ZA�?}A��A��;A��A�(�A���A�5?A��A"�A}�TA|E�Ax�Av��Av�Au��Au��AtQ�Aq�Aq&�ApI�Ao��AoC�An�`An��Am\)Ak�;Ak?}Aj�DAj=qAj$�Aj$�AiAi�AidZAh�HAh��Ah1'Ag�Ag�Af��Ae�Ad��AdVAb$�A`-A^ZA\��A\{A[dZA[AZ�DAY��AV��AV5?AU�AUl�AT�jASƨAR�AR{AO�
AMXAJ�9AI��AI
=AH��AH~�AH^5AHM�AH5?AH$�AG�AGl�AF-AEoAD~�AC�7AAK�A@�A?�A<1A;�A:bNA9��A8�DA7�
A6z�A4�A4ffA2��A1��A/�TA.��A.z�A,��A+VA*r�A*r�A*n�A(�A'�hA'7LA'VA&�yA&��A&1'A%A%7LA#�A"��A"bA!��A!�A JA��AĜA9XAbAJAA��A+A�A�A~�Av�Av�A�A��A\)AG�A;dAȴA�A$�A�AS�A|�A�`A^5A/A(�A&�AbNA�A/A
�/A
$�A	33A�uA�A��A�A�!A��Av�A�mA ��A bN@�S�@�n�@�{@��T@�p�@��P@�M�@��u@�b@���@�K�@�V@���@�Ĝ@�b@�dZ@�=q@�/@�@��D@�|�@�ff@��@��@�o@�ff@蛦@�M�@�K�@�^@�/@� �@�V@�j@��@�+@�n�@ٲ-@�x�@��@�dZ@��@��@�bN@��@��@Ϯ@�;d@�V@�G�@̬@˥�@ʏ\@�G�@�b@�ff@ũ�@��@ě�@��@�\)@�{@�Ĝ@�z�@��@�+@�@� �@��@�V@���@�o@�V@��^@�`B@��u@��m@��F@��@�@�/@���@�Q�@�  @�K�@��!@�n�@�5?@��@��#@���@�7L@�z�@�K�@�-@�p�@�Q�@�
=@��y@�~�@���@�G�@���@��@���@�z�@���@���@�x�@�/@�%@���@�Ĝ@��j@��9@���@��u@��D@�r�@�I�@�1'@��@��m@��@�;d@���@�v�@�-@�hs@�/@�z�@�1@��@��;@��w@���@�|�@�;d@��@�$�@���@�G�@�r�@�(�@� �@��@�ƨ@�+@�@��H@�ȴ@���@�n�@�$�@��@���@�%@�Ĝ@��u@�9X@�ƨ@��@�K�@�+@���@���@�^5@��@��h@�?}@��@��@���@��u@�I�@� �@��m@�ƨ@���@�\)@���@���@�&�@�j@���@���@�^5@�E�@�{@���@�X@��@��@�j@���@�l�@��@�$�@��T@���@��@�p�@���@�j@�@|�@�@~ff@}�-@}?}@|I�@{o@z��@zM�@y��@y�^@y��@y�7@y%@xr�@xQ�@xA�@w�@wl�@w;d@v�y@vff@u�@t�/@tz�@t9X@s�
@s@r�@r�@r�@r�@r��@r��@r=q@r�@q�@q��@p�9@o�w@oK�@o�@nȴ@nv�@nE�@n$�@n{@m��@m�-@m�h@m?}@l�@l�@l�@k�
@j��@j^5@j=q@j�@jJ@i�@i�#@i��@i�^@i��@i��@ihs@iX@iG�@i7L@i&�@i%@h��@hĜ@h�9@h�u@hbN@h1'@h  @g�w@f�y@e�-@e�h@e�@ep�@d�/@dI�@c��@cƨ@ct�@cC�@co@b�H@b^5@bJ@a�@a&�@`r�@`  @_��@_K�@_+@^��@^ȴ@^��@^v�@^$�@]?}@\�D@\�@[o@Z��@Z�\@Z�\@Zn�@Z�@Y�7@Y&�@X�9@XQ�@X �@X  @W�@W��@W�@W��@W��@W��@W�P@Wl�@Wl�@W\)@W;d@W+@W
=@V�y@V�+@V@U�@T�@T��@Tj@S�
@S��@S�@S�@St�@SC�@So@R�@R��@R��@R�\@RM�@Q�#@Q��@Qx�@QG�@Q�@P�`@P �@O�w@O+@N�y@Nȴ@N�+@Nff@N5?@N@M@M�@Mp�@M`B@M?}@M?}@M�@L�/@L�@L�@L�D@Lj@LI�@L9X@L(�@L�@L1@K��@K�m@Kƨ@K�@J�!@Jn�@J=q@I��@I�@I�@I�^@I��@Ix�@I7L@I&�@H��@Hr�@H �@G�@G|�@G;d@G
=@F�@F��@FE�@F@E��@EV@D�@Dj@C��@C��@C�@C"�@B��@B~�@B^5@BM�@BM�@A�#@@��@@�u@@ �@?��@?;d@>�y@>�@>ȴ@>��@>��@>v�@>ff@>V@>@=�T@=��@=/@=V@<��@<��@<��@<�@<��@<I�@;��@;@:=q@:J@9�@9�7@97L@8�`@8�u@8bN@8  @7��@6��@6V@65?@5�T@5p�@5�@4z�@49X@3��@3��@3t�@3C�@3C�@3"�@2�@2��@2�\@2n�@2�@1G�@0��@0�u@0�@0bN@0bN@/�@/;d@/
=@.��@/
=@/+@/l�@/�P@/|�@/;d@.ȴ@.��@.$�@-�@,��@,��@,��@,�j@,�j@,�j@,�@,��@,Z@,�@+��@+@*�!@*-@)��@)x�@)x�@)X@)x�@)x�@)�^@)&�@(��@(��@(��@(�9@(A�@(b@'�w@'+@&��@'
=@&v�@%�@%�T@%�-@%O�@$z�@$Z@$I�@#��@#�@#C�@#"�@#"�@#o@#@"�@"�@"�@"�@"�@"�H@"��@"��@"�\@!�^@!�@ Ĝ@ r�@ 1'@ b@�;@��@l�@\)@K�@;d@
=@�R@v�@{@�T@@�h@�@p�@�@��@�@1@Z@ƨ@�@~�@M�@=q@J@�#@��@��@x�@hs@hs@hs@G�@�@Ĝ@r�@A�@ �@  @�@��@�P@\)@\)@\)@+@��@ȴ@��@��@��@��@ff@5?@@�@�T@�@/@V@�@�@Z@(�@��@�F@��@�@t�@t�@dZ@S�@C�@o@��@n�@�@��@x�@X@7L@%@Ĝ@��@r�@r�@bN@1'@  @�@�@�y@�y@�y@ȴ@�+@ff@V@$�@�@�T@�T@�T@�T@@��@�h@�h@�h@p�@V@�j@1@�
@ƨ@��@�@�@�@S�@C�@o@
�H@
��@
�!@
~�@
=q@	�@	��@	��@	�7@	x�@	hs@	X@	G�@	7L@	�@��@Ĝ@�@r�@A�@  @�@�P@|�@+@ȴ@��@��@ff@E�@$�@�@��@@�-@�-@��@��@O�@?}@�@��@�/@�@��@�D@j@Z@9X@(�@ƨ@S�@C�@33@@�@�H@�H@��@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BS�BS�BT�BT�BS�BT�BT�BT�BS�BS�BS�BS�BS�BS�BS�BS�BS�BS�BR�BR�BR�BQ�BQ�BQ�BQ�BP�BN�BJ�BB�B;dB,B%�B#�B"�B!�B �B�B�B�B�B�B�B�B{BB��B�B�B�B�B�TB�;B�B��B�^B�?B�-B�B��B��B�uB�\B�B{�B{�By�By�Bu�B`BBT�BI�B49B'�B(�B!�B{B+B�B�`B�)BȴB��B|�Bt�BcTBW
BC�B1'B�BhBPB	7BBB
��B
�B
�mB
�B
��B
ƨB
�B
��B
��B
�\B
�7B
�B
{�B
s�B
[#B
J�B
E�B
B�B
>wB
;dB
'�B
"�B
�B
�B
uB
\B
DB
B	�B	�yB	�fB	�`B	�fB	�sB	�mB	�B	�B	�yB	�yB	�B	�mB	�ZB	�;B	�B	��B	ŢB	�FB	��B	��B	�\B	�JB	�7B	�B	�B	�B	y�B	x�B	x�B	x�B	v�B	p�B	hsB	aHB	N�B	<jB	$�B	�B	�B	oB	bB	\B	\B	VB	PB	JB		7B	  B��B�B�yB�/B��B��B�dB�!B�B��B��B��B��B�hB�\B�=B�B~�B|�B{�B� B}�B�B�DB�bB��B�oB�bB�\B�\B�VB�PB�JB�=B�+B�B�B� B~�B}�Bt�Br�Bp�Bp�Bo�Bo�Bo�Bn�BiyBgmBdZBdZBdZBdZBbNBbNBaHBaHB_;B]/BW
BVBVBO�BM�BK�BI�BE�BC�BA�B@�B>wB=qB<jB;dB8RB7LB6FB33B2-B0!B.B.B-B,B-B,B,B+B+B+B+B)�B'�B'�B'�B&�B%�B&�B%�B&�B%�B&�B%�B%�B%�B%�B$�B%�B#�B#�B#�B$�B#�B"�B"�B"�B"�B �B!�B �B!�B!�B �B!�B"�B!�B%�B$�B$�B&�B(�B(�B(�B(�B(�B(�B+B+B,B.B/B/B/B0!B1'B33B49B5?B6FB7LB8RB:^B;dB?}B@�B@�BA�BA�BA�BC�BC�BD�BE�BF�BG�BH�BH�BI�BJ�BK�BL�BL�BL�BM�BM�BN�BP�BT�BYB[#B_;BaHBaHBcTBdZBe`BgmBhsBo�Br�Bw�Bx�Bz�B{�B|�B}�B}�B}�B}�B}�B~�B~�B~�B� B� B�B�B�B�B�%B�1B�7B�VB�{B��B��B��B��B��B��B��B��B��B��B�B�!B�9B�FB�FB�FB�XB�jB�jB�qB�qB�wB�}B��B��BÖBŢBƨBǮBɺB��B��B��B��B��B��B��B��B�B�B�B�B�B�)B�5B�;B�BB�BB�NB�TB�fB�B�B��B��B��B��B��B��B	  B	B	B	%B	1B	DB	VB	oB	�B	�B	�B	�B	�B	#�B	'�B	,B	1'B	33B	5?B	8RB	:^B	>wB	C�B	D�B	F�B	I�B	I�B	I�B	I�B	J�B	M�B	O�B	P�B	VB	YB	ZB	ZB	\)B	`BB	`BB	bNB	bNB	dZB	jB	k�B	l�B	l�B	l�B	m�B	o�B	q�B	q�B	r�B	t�B	y�B	~�B	� B	�B	�B	�B	�B	�B	�B	�%B	�%B	�%B	�+B	�1B	�=B	�=B	�DB	�VB	�\B	�bB	�bB	�bB	�hB	�hB	�oB	�oB	�oB	�uB	�uB	�{B	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�'B	�-B	�-B	�9B	�9B	�?B	�RB	�XB	�XB	�^B	�jB	�qB	�wB	�}B	�}B	��B	��B	ŢB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�
B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�/B	�5B	�BB	�HB	�NB	�ZB	�`B	�`B	�`B	�`B	�`B	�fB	�fB	�mB	�mB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
%B
%B
+B
+B
1B
1B
	7B

=B
DB
DB
JB
PB
PB
VB
\B
\B
\B
\B
\B
bB
oB
oB
uB
{B
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
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
"�B
"�B
#�B
$�B
%�B
%�B
&�B
&�B
&�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
+B
+B
+B
+B
,B
,B
-B
.B
.B
/B
/B
0!B
1'B
1'B
0!B
1'B
1'B
2-B
33B
5?B
6FB
7LB
8RB
8RB
8RB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
8RB
8RB
7LB
8RB
8RB
9XB
9XB
:^B
:^B
:^B
;dB
<jB
<jB
=qB
=qB
=qB
=qB
=qB
>wB
=qB
=qB
=qB
=qB
>wB
A�B
B�B
A�B
A�B
A�B
A�B
A�B
A�B
B�B
C�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
G�B
H�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
L�B
M�B
M�B
M�B
M�B
N�B
N�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
Q�B
R�B
R�B
S�B
T�B
T�B
T�B
VB
VB
T�B
T�B
VB
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
ZB
[#B
[#B
[#B
[#B
\)B
\)B
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
^5B
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
_;B
_;B
_;B
_;B
`BB
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
cTB
cTB
dZB
dZB
cTB
dZB
dZB
dZB
e`B
e`B
ffB
ffB
gmB
gmB
gmB
gmB
gmB
gmB
hsB
hsB
iyB
iyB
iyB
jB
jB
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
l�B
l�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BTBTBUBUBTBUBUBUBTBTBTBTBTBTBTBTBTBTBSBS&BS&BR BRBR BRoBQ�BP�BM�BF�B@ B.cB&�B$�B#�B#nB!-BB;B�B�B�B�BEB$B�B��B�|B��B�MB��B�@B�vB��B�B��B��B��B�;B�KB��B�{B�4B�B|�B}VBz�B{�BwLBa|BV�BLJB6+B(�B*B#TBB	7B�9B�mB��B��B��B.Bw�Be�BZ7BGB3�BB:BVB	�B�B-B
��B
��B
��B
ٚB
�2B
�=B
�;B
��B
��B
�}B
�XB
�B
~�B
w�B
]IB
K�B
FYB
C�B
@iB
=�B
)B
#�B
xB
KB
,B
HB
�B
�B	�B	�KB	��B	�B	��B	��B	��B	�)B	�)B	�B	�KB	�6B	�$B	�zB	�vB	�sB	�BB	�fB	��B	�,B	�QB	��B	�6B	��B	�%B	��B	��B	z�B	y�B	yXB	zB	xB	rB	jKB	d@B	R B	?cB	&LB	�B	9B	�B	�B	�B	�B	�B	B	jB	
�B	�B�	B�|B�"B��B��B�bB��B�[B�wB��B�:B��B��B��B��B�0B�EB��B~BB~BB��B~�B��B��B�oB�+B�&B��B��B��B�(B�<B�jB��B��B�?B��B�B��B��Bv+Bs�Bq'BqBp!BpoBp�Bp;Bj�Bh>Bd�Bd�Be,BeBb�Bb�Ba�BbhBaB^�BW�BWsBX+BQ BN�BM�BKDBGBD�BBuBA�B?HB>�B=�B<�B9rB8�B7�B5?B3�B1�B/OB/�B-�B-)B-�B,�B,�B+�B,qB,=B,=B*�B(�B(�B(�B'�B&�B'�B&�B'�B&�B'mB&�B&�B'B'B&B&�B$�B%`B%�B&�B$�B#�B#�B$ZB$&B!|B"�B!|B"hB"NB!|B"�B#�B#:B&�B%�B&2B(�B)�B)�B)�B)�B)�B*B,"B,"B-]B.�B/�B/�B/�B0�B2-B49B4�B6B72B8lB9�B;B<�B@�BA BAUBBABBBB[BDMBD3BE�BFtBGEBHKBIBI7BJrBK^BL0BM6BMBM6BNVBNpBO�BQ�BVBZB\CB`'Ba�Ba�BdBeBf2BhsBj0Bp�BtBx�ByXB{JB|6B}VB~BB~BB~(B~(B~(BHBcBcB�iB�iB�oB�oB��B��B��B��B��B��B�2B�)B��B�B�-B�-B�4B�TB�ZB��B�B��B��B��B�zB��B��B��B��B��B��B��B��B��B��B�B�3B�B��B�1B�#B�0B�<B�<B�(B�bB�bB�TB�aB�mB�yB�BچB�kB�xBޞBߤB�B��B�B�&B�mB�5B�[B�tB��B�6B�<B�<B��B	 iB	uB	�B	�B	�B	�B	B	&B	�B		B	B	B	;B	$ZB	(XB	,qB	1�B	3�B	5�B	8�B	:�B	?B	C�B	EB	F�B	J	B	J	B	J	B	J#B	KB	N"B	P.B	QNB	VmB	YKB	Z�B	Z�B	\�B	`�B	`�B	b�B	b�B	d�B	j�B	k�B	l�B	l�B	l�B	m�B	pB	q�B	q�B	sB	u?B	z^B	HB	�OB	�UB	�[B	�aB	�gB	�SB	�mB	�?B	�tB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�OB	�NB	�2B	�8B	�8B	�RB	�kB	�WB	�IB	�iB	�[B	�[B	�|B	��B	��B	�nB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�#B	�B	�B	�(B	�.B	�HB	�NB	�:B	�aB	�2B	�9B	�9B	�9B	�?B	�?B	�+B	�+B	�EB	�_B	�_B	�KB	�KB	�KB	�eB	�eB	�kB	چB	یB	ݘB	ބB	��B	�|B	�B	�B	�B	�zB	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	��B	��B	��B	��B	� B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	��B	�B	�B	�B	�	B	�$B	�$B	�$B	�B	�B	�B	�*B	�B	�B	�0B	�JB	�PB	�"B	�BB	�.B
 4B
 B
 OB
 OB
UB
UB
;B
AB
uB
aB
gB
�B
mB
tB
tB
zB
�B
fB
�B
	�B

�B
xB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
B
!B
�B
�B
 B
 �B
"B
"B
"B
# B
# B
$@B
%,B
&B
&2B
'RB
'RB
'RB
)DB
)*B
)DB
*0B
*0B
*B
*KB
+QB
+QB
+QB
+QB
,WB
,�B
-]B
.cB
.IB
/iB
/iB
0oB
1vB
1vB
0;B
1AB
1AB
2-B
3hB
5tB
6�B
7�B
8�B
8�B
8�B
9�B
9rB
9rB
9�B
9rB
9rB
9�B
9�B
9�B
8�B
8�B
7�B
8�B
8�B
9�B
9�B
:�B
:xB
:�B
;�B
<�B
<�B
=�B
=�B
=�B
=�B
=�B
>�B
=�B
=�B
=�B
=�B
>�B
A�B
B�B
A�B
A�B
A�B
A�B
A�B
A�B
B�B
C�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
GB
G+B
HB
IB
J	B
I�B
J	B
I�B
I�B
KB
J�B
J�B
KB
J�B
LB
K�B
MB
N"B
N"B
NB
NB
OB
O(B
P.B
PHB
O�B
PB
QNB
QNB
Q4B
R B
S&B
S&B
T,B
U2B
UMB
UMB
V9B
VB
UB
UMB
VSB
VSB
V9B
WYB
WYB
WYB
WYB
X_B
X_B
X_B
XEB
X+B
XEB
YeB
YeB
YeB
Y1B
YKB
YKB
YeB
ZQB
ZQB
Z7B
ZkB
ZkB
[WB
[qB
[WB
[WB
\xB
\]B
\xB
]~B
]dB
]~B
]dB
]IB
]IB
]dB
]dB
^�B
^�B
^�B
^�B
_pB
_pB
_�B
_pB
_pB
_pB
_pB
_pB
`\B
`vB
_�B
_�B
_�B
_�B
`�B
`vB
`\B
`vB
`�B
a|B
a|B
a|B
a|B
abB
a|B
bhB
bhB
b�B
c�B
c�B
dtB
d�B
c�B
d�B
d�B
d�B
e�B
e�B
f�B
f�B
g�B
g�B
g�B
g�B
g�B
g�B
h�B
h�B
i�B
i�B
i�B
j�B
j�B
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
l�B
l�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.32(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201605301136142016053011361420160530113614201806221138272018062211382720180622113827201804050400522018040504005220180405040052  JA  ARFMdecpA19c                                                                20160623182202  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20160628045854  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20160628045855  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20160628045855  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20160628045856  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20160628045856  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20160628045856  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20160628045856  QCP$                G�O�G�O�G�O�            FB40JA      jafc1.0                                                                 20160628045856                      G�O�G�O�G�O�                JA  ARUP                                                                        20160628072244                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20160530023456  CV  JULD            G�O�G�O�F�U�                JM  ARCAJMQC2.0                                                                 20160530023614  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20160530023614  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404190052  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622023827  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115101516                      G�O�G�O�G�O�                