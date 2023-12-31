CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:40:26Z creation;2022-06-04T17:40:27Z conversion to V3.1      
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
_FillValue                 �  I4   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M$   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p|   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tl   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �X   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �t   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ޤ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �0   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �4   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �D   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �H   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �L   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20220604174026  20220610131507  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               cA   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @ٝƊ�1   @ٝ(d�@/5\(��cE�S���1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @33@�  @�  A   A   AA��A`  A�  A�  A�33A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BPffBX��B_33Bh  Bp  Bx  B�  B�  B���B���B�  B�  B�  B�ffB�ffB�  B�  B�  B�  B�  B�  B�  B�  B���B�ffB�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B���C  C  C  C  C
  C  C�C  C�fC  C�fC  C  C  C  C   C"  C$  C&  C(  C*  C,  C.�C0  C2  C4  C6  C8  C:  C<�C=��C?�fCB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr�Ct�Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C��C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DSfDS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�0 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ?�p�@k�@�@�A�HA<z�AZ�HAz�HA�p�A���A�p�A�p�A�p�A�p�A�p�A�p�B�RB�RB�RB�RB&�RB.�RB6�RB>�RBF�RBO�BW�B]�Bf�RBn�RBv�RB~�RB�\)B�(�B�(�B�\)B�\)B�\)B�B�B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�(�B�B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B��\B��\B�(�C�C�C�C�C	�C�CǮC�C�zC�C�zC�C�C�C�C�C!�C#�C%�C'�C)�C+�C-ǮC/�C1�C3�C5�C7�C9�C;ǮC=z�C?�zCA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�CqǮCsǮCu�Cw�Cy�C{�C}�C�C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
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
C��
C��
C��
C��
C���C���C���C��
C��
C��
C��=C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
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
D k�D �Dk�D�Dk�D�Dk�D�Dk�D�Dk�D�Dk�D�Dk�D�Dk�D�D	k�D	�D
k�D
�Dk�D�Dk�D�Dk�D�Dk�D�Dk�D�Dk�D�Dk�D�Dk�D�Dk�D�Dk�D�Dk�D�Dk�D�Dk�D�Dk�D�Dk�D�Dk�D�Dk�D�Dk�D�Dk�D�Dk�D�Dk�D�D k�D �D!k�D!�D"k�D"�D#k�D#�D$k�D$�D%k�D%�D&k�D&�D'k�D'�D(k�D(�D)k�D)�D*k�D*�D+k�D+�D,k�D,�D-k�D-�D.k�D.�D/k�D/�D0k�D0�D1k�D1�D2k�D2�D3k�D3�D4k�D4�D5k�D5�D6k�D6�D7k�D7�D8k�D8�D9k�D9�D:k�D:�D;k�D;�D<k�D<�D=k�D=�D>k�D>�D?k�D?�D@k�D@�DAk�DA�DBk�DB�DCk�DC�DDk�DD�DEk�DE�DFk�DF�DGk�DG�DHk�DH�DIk�DI�DJk�DJ�DKk�DK�DLk�DL�DMk�DM�DNk�DN�DOk�DO�DPk�DP�DQk�DQ�DRk�DR��DSk�DS�DTk�DT�DUk�DU�DVk�DV�DWk�DW�DXk�DX�DYk�DY�DZk�DZ�D[k�D[�D\k�D\�D]k�D]�D^k�D^�D_k�D_�D`k�D`�Dak�Da�Dbk�Db�Dck�Dc�Ddk�Dd�Dek�De�Dfk�Df�Dgk�Dg�Dhk�Dh�Dik�Di�Djk�Dj�Dkk�Dk�Dlk�Dl�Dmk�Dm�Dnk�Dn�Dok�Do�Dpk�Dp�Dqk�Dq�Drk�Dr�Dsk�Ds�Dtk�Dt�Duk�Du�Dvk�Dv�Dwk�Dw�Dxk�Dx�Dyk�Dy�Dzk�Dz�D{k�D{�D|k�D|�D}k�D}�D~k�D~�Dk�D�D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�8�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�x�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�Dµ�D���D�5�D�u�Dõ�D���D�5�D�u�Dĵ�D���D�5�D�u�Dŵ�D���D�5�D�u�DƵ�D���D�5�D�u�Dǵ�D���D�5�D�u�Dȵ�D���D�5�D�u�Dɵ�D���D�5�D�u�Dʵ�D���D�5�D�u�D˵�D���D�5�D�u�D̵�D���D�5�D�u�D͵�D���D�5�D�u�Dε�D���D�5�D�u�Dϵ�D���D�5�D�u�Dе�D���D�5�D�u�Dѵ�D���D�5�D�u�Dҵ�D���D�5�D�u�Dӵ�D���D�5�D�u�DԵ�D���D�5�D�u�Dյ�D���D�5�D�u�Dֵ�D���D�5�D�u�D׵�D���D�5�D�u�Dص�D���D�5�D�u�Dٵ�D���D�5�D�u�Dڵ�D���D�5�D�u�D۵�D���D�5�D�u�Dܵ�D���D�5�D�u�Dݵ�D���D�5�D�u�D޵�D���D�5�D�u�Dߵ�D���D�5�D�u�D��D���D�5�D�u�D��D���D�5�D�u�D��D���D�5�D�u�D��D���D�5�D�u�D��D���D�5�D�u�D��D���D�5�D�u�D��D���D�5�D�u�D��D���D�5�D�u�D��D���D�5�D�u�D��D���D�5�D�u�D��D���D�5�D�u�D��D���D�5�D�u�D��D���D�5�D�u�D���D���D�5�D�u�D��D���D�5�D�u�D��D���D�5�D�u�D��D���D�5�D�u�D��D���D�5�D�u�D��D���D�5�D�u�D��D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�u�D���D���D�5�D�r�D���D���D�5�D�u�D���D���D�%�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   AՊ�AՉ�AՉ7AՈ�AՉAՊ=AՊ�AՄ�A�y�A�m�A�s�A�Q�A�.IA��A��|A��pA��?A�ΥA���AԿHAԾwAԼAԸ�AԱ�AԥAԚ7Aԍ�A�{�A��A�уAЫ6A�Z�A��A���Aϔ�A�4A�s�A�,�A��A�}"A�sA��A���A�YA�\�A�IA��[A��/A���A��A���A��=A�Z�A���A�}�A�m)A�IA��oA�A�B�A�f�A��CA��A���A��}A�%A�@�A���A�d&A�A�A���A���A��A���A���A��A�o�A�<�A���A���A��zA�͟A��*A�+A�xA���A��A��A�G�A�#�A��A��ArGA{=�As�An"hAk�SAi��Ag�RAd��A^��A]cA\$tA[xAZ�'AW��AR	�AO�AO($AN�9ANu�AMs�AIX�AFjADx�AC��AB��AB��AB�sAB��AB iA>�cA<K^A;��A:��A:uA9xA7�A6��A5�'A5;�A4�wA3V�A1_A/u%A.�^A-�mA,�mA+��A+�A*�'A*�PA)	lA&6A$�_A$1A$oA#�2A!�FA xA&A�AJ#A MA�uA�jA$�A��A�A!�A��A�AA(�A��Ag8Al"A3�As�A�A�pA�@A{�A��AA�A�A��A{�A33A��A�wA��A_AG�A}�A	�fA�qA�\A��A�FA	��A
��A?�A��AP�A�A
�aA
{JA
jA
qA	��A�9A7�A�!Ad�A|�A�NA�A�
A3�A�&A��A>�A�2Au%A($AOA�`Ai�A ��A ��A -w@���@��@�xl@��3@���@�Y@�^�@�:*@���@���@���@�S�@�A�@��=@��@�]�@�&@�~(@�<6@��K@�$@��@��@���@�l�@���@���@�C�@�:@��@��@�L�@��X@�!@�p�@��@���@啁@���@�^5@�X@�=@�@���@⭬@�1'@�j�@���@�:�@�|@���@ޒ�@�A�@�_@�a@� �@�_p@��@��@��m@�0�@�\�@׮�@��@�}V@�1'@��@��a@�K�@Ԁ�@�?�@�)�@���@��@�~�@�@�#:@��@й�@Є�@�l"@�Xy@Ϡ�@��p@��@��M@�8�@�+�@��o@�+@ɢ�@��@�o@�[�@�M@�m]@�b�@�͟@�w�@��@�rG@�0�@�s�@�ݘ@�x@�+�@��f@���@���@1@�,=@��
@��@��"@�{J@�$t@��K@���@��@���@���@��@��:@�@@��@�H�@�˒@�?}@�;�@���@�C�@��B@�D�@�G@��@���@�/@��@�$@�ƨ@�a�@��f@���@�p;@���@��@���@�M�@��@���@���@�a@�U�@�RT@�O@��@�h�@��.@��V@�{J@��@���@�\�@�M@�_@��r@��@��@�oi@�{J@��'@�e�@�N�@���@���@�E9@���@�d�@��@��T@��g@��?@�,=@���@���@�~�@�!@���@�f�@�k�@�ݘ@��@��@�-@���@��7@��@��"@���@�"h@�ƨ@��4@�A @�2a@���@���@�ff@�	�@���@���@�Vm@���@�Ɇ@��@���@�M�@�ƨ@�x@�"�@��6@�Xy@���@���@�=�@�@@��O@�w�@�6�@��@��3@��@�e�@��@��1@�d�@��n@�X@�=�@��@��	@��U@��@��v@��8@���@�6@��+@��@�j�@�J�@���@�5�@��5@���@�\�@��@�ϫ@��d@��@��9@��6@�}�@�E9@��@���@��@��@���@���@���@�T�@�Y@���@��@��F@�9X@��@��@�c@�:�@���@���@�w�@�@�@��+@��@���@�k�@��@���@�ѷ@��m@���@��@�_�@�$�@���@���@�7L@�!�@��2@��x@�a|@�@���@�[W@�,�@��@�xl@�@�@��@��d@���@���@�S�@�V@���@�bN@�8�@�&�@��@���@���@�y�@�J�@�(�@���@��/@���@���@�r�@��@�خ@���@��[@��=@�y�@�S�@�8�@��@��s@��!@�W�@�#:@��
@�g�@�+�@��@���@���@���@�c�@�@a@~��@~��@~�\@~\�@}��@}�@|(�@{��@{Z�@{J#@{�@z��@z�@z;�@y�#@y<6@x��@w�@w��@w\)@wC@v͟@v�+@v_�@v_@uc�@t�j@tw�@t,=@sX�@r�}@rv�@ra|@r?@ru@qx�@p�v@pm�@p�@o��@o�4@ov`@oRT@o�@n�x@m��@mo @l֡@l_@k�}@kg�@j�@jd�@j8�@i��@iL�@h�@h�@hN�@g�@g�k@gJ#@g�@fs�@eT�@d�|@d�D@c�A@c��@ct�@c�@b�L@bn�@bE�@a�^@aY�@`�@`m�@`�@_��@_F�@_�@_S@^��@^��@^��@^�@]u�@]#�@\�@\x@[��@['�@Z�]@Zff@Z
�@Y�-@Y�@YQ�@X��@XĜ@X_@W��@W� @WRT@W/�@W1�@V�"@V�A@Uϫ@U��@U0�@T�D@T6@S�q@S1�@SO@SX�@SX�@S@O@S@Ri�@Q�>@QS&@Q#�@Q�@P��@P��@P��@P��@PQ�@O�@@O]�@OA�@N��@NJ�@M��@MrG@M�@L�@L7�@K�
@J��@I��@I�X@I?}@H�@H�e@HA�@H �@GJ#@FYK@F�@F	@Ef�@D�v@C�+@C��@Cx@C�@B�B@B{�@B@�@A��@A�"@A7L@@ی@@D�@?��@?�}@?�:@?Z�@>xl@>_�@=�t@=/@=&�@<�j@<!@;��@;X�@;�@:��@:�!@:Z�@:u@9\�@8�@8��@7��@7��@7��@7C�@6�<@6C�@5�@5J�@4��@4�[@4w�@41'@3�@3��@3s@3,�@2��@2��@2n�@2c @2+k@1�@1|@12a@1@1�@0��@04n@/��@/�V@/�	@/Mj@.ߤ@.�L@-�@-=�@-�@,�.@,Xy@,"h@+�@+�P@+4�@*��@*��@*Q@*�@)��@)�-@)�@)��@)�S@)*0@(��@(�@(Xy@(@'�6@'e�@'@&��@&��@&�2@&�H@&��@&c @&�@%zx@%+�@%V@$��@$�@$�e@$N�@$1'@$*�@$'R@$�@$  @#��@#�@#��@#O@#/�@#�@"�@"GE@"
�@!�@!��@!zx@!\�@!L�@!A @!8�@!-w@!@!%@ �P@ �K@ �9@ l"@ %�@˒@�	@F�@�@�@�6@+k@ �@��@��@�@5�@�@�@��@��@��@�z@�@C-@%�@�g@iD@�@n�@6�@�@��@��@��@|@A @��@�@y>@<�@�@��@{J@@O@�@�@�m@�b@u%@J�@��@�d@��@F@�@�@�f@��@��@bN@'R@�W@��@ƨ@��@��@t�@�@�@��@~�@C�@J@�>@�^@��@L�@%@�?@��@~(@[�@%�@�@�}@t�@dZ@K�@�@ȴ@�r@n�@0U@�Z@�@�h@S&@ \@�5@��@��@l"@A�@�@�K@{J@W?@O@33@(@(@ i@
��@
�@
q�@
p;@
ff@
	@	�@	m]@	IR@	@Ɇ@�z@��@��@��@w�@`�@1'@'R@�@�
@�a@��@��@{J@J#@,�@�@�@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   AՊ�AՉ�AՉ7AՈ�AՉAՊ=AՊ�AՄ�A�y�A�m�A�s�A�Q�A�.IA��A��|A��pA��?A�ΥA���AԿHAԾwAԼAԸ�AԱ�AԥAԚ7Aԍ�A�{�A��A�уAЫ6A�Z�A��A���Aϔ�A�4A�s�A�,�A��A�}"A�sA��A���A�YA�\�A�IA��[A��/A���A��A���A��=A�Z�A���A�}�A�m)A�IA��oA�A�B�A�f�A��CA��A���A��}A�%A�@�A���A�d&A�A�A���A���A��A���A���A��A�o�A�<�A���A���A��zA�͟A��*A�+A�xA���A��A��A�G�A�#�A��A��ArGA{=�As�An"hAk�SAi��Ag�RAd��A^��A]cA\$tA[xAZ�'AW��AR	�AO�AO($AN�9ANu�AMs�AIX�AFjADx�AC��AB��AB��AB�sAB��AB iA>�cA<K^A;��A:��A:uA9xA7�A6��A5�'A5;�A4�wA3V�A1_A/u%A.�^A-�mA,�mA+��A+�A*�'A*�PA)	lA&6A$�_A$1A$oA#�2A!�FA xA&A�AJ#A MA�uA�jA$�A��A�A!�A��A�AA(�A��Ag8Al"A3�As�A�A�pA�@A{�A��AA�A�A��A{�A33A��A�wA��A_AG�A}�A	�fA�qA�\A��A�FA	��A
��A?�A��AP�A�A
�aA
{JA
jA
qA	��A�9A7�A�!Ad�A|�A�NA�A�
A3�A�&A��A>�A�2Au%A($AOA�`Ai�A ��A ��A -w@���@��@�xl@��3@���@�Y@�^�@�:*@���@���@���@�S�@�A�@��=@��@�]�@�&@�~(@�<6@��K@�$@��@��@���@�l�@���@���@�C�@�:@��@��@�L�@��X@�!@�p�@��@���@啁@���@�^5@�X@�=@�@���@⭬@�1'@�j�@���@�:�@�|@���@ޒ�@�A�@�_@�a@� �@�_p@��@��@��m@�0�@�\�@׮�@��@�}V@�1'@��@��a@�K�@Ԁ�@�?�@�)�@���@��@�~�@�@�#:@��@й�@Є�@�l"@�Xy@Ϡ�@��p@��@��M@�8�@�+�@��o@�+@ɢ�@��@�o@�[�@�M@�m]@�b�@�͟@�w�@��@�rG@�0�@�s�@�ݘ@�x@�+�@��f@���@���@1@�,=@��
@��@��"@�{J@�$t@��K@���@��@���@���@��@��:@�@@��@�H�@�˒@�?}@�;�@���@�C�@��B@�D�@�G@��@���@�/@��@�$@�ƨ@�a�@��f@���@�p;@���@��@���@�M�@��@���@���@�a@�U�@�RT@�O@��@�h�@��.@��V@�{J@��@���@�\�@�M@�_@��r@��@��@�oi@�{J@��'@�e�@�N�@���@���@�E9@���@�d�@��@��T@��g@��?@�,=@���@���@�~�@�!@���@�f�@�k�@�ݘ@��@��@�-@���@��7@��@��"@���@�"h@�ƨ@��4@�A @�2a@���@���@�ff@�	�@���@���@�Vm@���@�Ɇ@��@���@�M�@�ƨ@�x@�"�@��6@�Xy@���@���@�=�@�@@��O@�w�@�6�@��@��3@��@�e�@��@��1@�d�@��n@�X@�=�@��@��	@��U@��@��v@��8@���@�6@��+@��@�j�@�J�@���@�5�@��5@���@�\�@��@�ϫ@��d@��@��9@��6@�}�@�E9@��@���@��@��@���@���@���@�T�@�Y@���@��@��F@�9X@��@��@�c@�:�@���@���@�w�@�@�@��+@��@���@�k�@��@���@�ѷ@��m@���@��@�_�@�$�@���@���@�7L@�!�@��2@��x@�a|@�@���@�[W@�,�@��@�xl@�@�@��@��d@���@���@�S�@�V@���@�bN@�8�@�&�@��@���@���@�y�@�J�@�(�@���@��/@���@���@�r�@��@�خ@���@��[@��=@�y�@�S�@�8�@��@��s@��!@�W�@�#:@��
@�g�@�+�@��@���@���@���@�c�@�@a@~��@~��@~�\@~\�@}��@}�@|(�@{��@{Z�@{J#@{�@z��@z�@z;�@y�#@y<6@x��@w�@w��@w\)@wC@v͟@v�+@v_�@v_@uc�@t�j@tw�@t,=@sX�@r�}@rv�@ra|@r?@ru@qx�@p�v@pm�@p�@o��@o�4@ov`@oRT@o�@n�x@m��@mo @l֡@l_@k�}@kg�@j�@jd�@j8�@i��@iL�@h�@h�@hN�@g�@g�k@gJ#@g�@fs�@eT�@d�|@d�D@c�A@c��@ct�@c�@b�L@bn�@bE�@a�^@aY�@`�@`m�@`�@_��@_F�@_�@_S@^��@^��@^��@^�@]u�@]#�@\�@\x@[��@['�@Z�]@Zff@Z
�@Y�-@Y�@YQ�@X��@XĜ@X_@W��@W� @WRT@W/�@W1�@V�"@V�A@Uϫ@U��@U0�@T�D@T6@S�q@S1�@SO@SX�@SX�@S@O@S@Ri�@Q�>@QS&@Q#�@Q�@P��@P��@P��@P��@PQ�@O�@@O]�@OA�@N��@NJ�@M��@MrG@M�@L�@L7�@K�
@J��@I��@I�X@I?}@H�@H�e@HA�@H �@GJ#@FYK@F�@F	@Ef�@D�v@C�+@C��@Cx@C�@B�B@B{�@B@�@A��@A�"@A7L@@ی@@D�@?��@?�}@?�:@?Z�@>xl@>_�@=�t@=/@=&�@<�j@<!@;��@;X�@;�@:��@:�!@:Z�@:u@9\�@8�@8��@7��@7��@7��@7C�@6�<@6C�@5�@5J�@4��@4�[@4w�@41'@3�@3��@3s@3,�@2��@2��@2n�@2c @2+k@1�@1|@12a@1@1�@0��@04n@/��@/�V@/�	@/Mj@.ߤ@.�L@-�@-=�@-�@,�.@,Xy@,"h@+�@+�P@+4�@*��@*��@*Q@*�@)��@)�-@)�@)��@)�S@)*0@(��@(�@(Xy@(@'�6@'e�@'@&��@&��@&�2@&�H@&��@&c @&�@%zx@%+�@%V@$��@$�@$�e@$N�@$1'@$*�@$'R@$�@$  @#��@#�@#��@#O@#/�@#�@"�@"GE@"
�@!�@!��@!zx@!\�@!L�@!A @!8�@!-w@!@!%@ �P@ �K@ �9@ l"@ %�@˒@�	@F�@�@�@�6@+k@ �@��@��@�@5�@�@�@��@��@��@�z@�@C-@%�@�g@iD@�@n�@6�@�@��@��@��@|@A @��@�@y>@<�@�@��@{J@@O@�@�@�m@�b@u%@J�@��@�d@��@F@�@�@�f@��@��@bN@'R@�W@��@ƨ@��@��@t�@�@�@��@~�@C�@J@�>@�^@��@L�@%@�?@��@~(@[�@%�@�@�}@t�@dZ@K�@�@ȴ@�r@n�@0U@�Z@�@�h@S&@ \@�5@��@��@l"@A�@�@�K@{J@W?@O@33@(@(@ i@
��@
�@
q�@
p;@
ff@
	@	�@	m]@	IR@	@Ɇ@�z@��@��@��@w�@`�@1'@'R@�@�
@�a@��@��@{J@J#@,�@�@�@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
NB
NpB
NVB
NpB
NpB
N�B
N�B
OvB
PbB
Q�B
QNB
T�B
XyB
bNB
cTB
e,B
e�B
f�B
e�B
ffB
iB
i�B
l�B
qvB
s�B
u�B
{�B
�aBl�Bo�Bp�BsMBxBw�Bu�Br�Bi�BQ�B-]B./B9�B\�Bw�B��B��B��B�	BƎB�$B�BB��B�iB�|B�ZB�B�B�>B��B��B��B��B��B��B׍B��B�(BˬB�?B�OB�-B��B�7B{BX�BN�B@iB6+B4nB+B�B	�B
��B
��B
�7B
��B
�,B
��B
{�B
p�B
c:B
R�B
4�B
�B	�AB	��B	�
B	��B	|PB	q'B	`�B	D�B	;JB	3�B	,B	(�B	�B	B	B	�B	�B	 �B�B	�B	B	#�B	;dB	K�B	X�B	sB	�;B	�SB	j�B	ZB	V�B	PbB	D�B	>]B	TB	UgB	cnB	kQB	lB	y�B	{dB	q'B	l=B	n/B	qAB	{0B	��B	�vB	��B	�QB	��B	�-B	}B	��B	��B	�4B	��B	��B	��B	��B	��B	��B	�dB	��B	��B	�gB	��B	�B	�~B	�oB	� B	��B	��B	��B	��B	�CB	�qB	�B	�QB	��B	��B	��B	�+B	�B	��B	�mB	��B	��B	�6B	�4B	y�B	q'B	lqB	oB	p�B	t�B	�EB	�pB	�iB	�B	��B	ѝB	�vB	��B	ބB	�TB	�-B	��B	یB	ۦB	��B	�zB	��B	�B	�bB	�B	��B	��B	��B	�4B	�4B	�B	�\B	ߊB	��B	��B	�	B	�WB	�B	�_B	��B	ּB	�kB	�CB	�7B	��B	��B	�	B	یB	ٴB	�mB	ՁB	��B	�+B	�]B	��B	ބB	ݘB	ݲB	߾B	��B	�B	�TB	��B	�mB	�B	�fB	�2B	�B	�B	��B	�B	�XB	�B	��B	�DB	�B	�kB	�WB	�CB	�B	�B	�B	�-B	��B	�?B	�B	��B	�B	�ZB	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�CB	�wB	�)B	�B	��B	�B	�B	�B	��B	��B	�B	�_B	�B	�sB	��B	��B	��B	��B	�XB	�B	��B	�qB	��B	��B	�=B	�wB	��B	�B	�B	�B	��B	�-B	�tB	�aB	�GB	�B	�B	��B	�iB	�B	�B	�B	�]B	��B	��B	�5B	�B	�B	��B	��B	�-B	�B	�FB	�zB	��B	��B	�`B	��B	��B	��B	�LB	��B	�FB	��B	��B	��B	�`B	�B	��B	��B	��B	�dB	�B	��B	�BB	��B	��B	��B
 4B
 iB
 iB
 4B
 4B	�cB
 B
;B
�B
AB
uB
�B
�B
�B
�B
-B
{B
�B
GB
�B
�B
�B
gB
�B
�B
gB
�B
�B
B
�B
aB
�B
uB
�B
[B
uB
�B
 B
 4B	��B
 �B	��B	�B	�"B	�B	�JB	�0B	��B	�6B	��B
�B
�B
 iB	��B	�B	��B	�B	�.B	��B
 B
 OB
 �B
 B
[B
-B
B
GB
�B
B
aB
�B
gB
gB
�B
gB
MB
3B
3B
�B
B
mB
B
?B
�B
tB
�B
�B
�B
�B
B
B
B
+B
+B
B
�B
_B
EB
_B
zB
�B

rB
�B
bB
�B
�B
�B
.B
HB
�B
�B
�B
�B
mB
�B
YB
�B
_B
�B
B
�B
�B
=B
=B
qB
B
B
~B
�B
�B
5B
�B
�B
VB
pB
 B
 BB
 BB
 BB
 �B
!B
!bB
!bB
!�B
!�B
!�B
!�B
"4B
#B
#B
"�B
"�B
#B
# B
# B
#:B
#nB
#�B
$@B
$�B
%B
%`B
%`B
%FB
%`B
%B
%zB
%zB
%`B
%,B
%�B
&LB
&�B
&�B
'B
'mB
'�B
(�B
)B
)DB
)�B
*B
*B
+�B
,"B
,WB
,�B
,�B
,�B
,�B
,�B
-�B
-�B
-�B
-�B
.B
.IB
.cB
.�B
/ B
.�B
.�B
/OB
/B
.�B
.�B
.�B
.�B
.�B
/ B
/iB
0;B
0�B
1[B
1�B
1�B
1�B
1�B
2B
2�B
33B
3�B
3�B
3�B
3�B
3�B
4B
4nB
4�B
5?B
5tB
5�B
5�B
6�B
72B
7�B
7�B
7�B
7�B
8B
8�B
8�B
9	B
9�B
:*B
:*B
:*B
:^B
:DB
:^B
:�B
:�B
:�B
;�B
;�B
;�B
<B
<6B
<PB
<�B
<�B
="B
=<B
=qB
=�B
>(B
>(B
>(B
>BB
>�B
>�B
>�B
?B
?HB
?HB
?}B
?}B
@ B
@�B
@�B
@�B
AUB
AUB
AUB
A�B
A�B
BB
BB
B[B
B�B
CB
CGB
CaB
C�B
C�B
C�B
C�B
C�B
C�B
D3B
D�B
D�B
D�B
D�B
E9B
E9B
EmB
EmB
E�B
E�B
E�B
E�B
E�B
F%B
FB
F?B
FtB
F�B
F�B
GEB
G�B
HKB
H�B
IB
H�B
H�B
G�B
HB
HB
H1B
H�B
H�B
IB
IRB
IlB
IB
HfB
H�B
H�B
I�B
J#B
K�B
K�B
LdB
L�B
L�B
MPB
M�B
M�B
N"B
MPB
MB
M�B
N<B
N"B
M�B
M�B
MjB
N<B
NB
NVB
NVB
N�B
N�B
N�B
N�B
OBB
PHB
PbB
QB
Q4B
QB
R�B
RTB
R�B
R�B
R�B
R�B
S&B
S&B
S&B
S�B
S[B
S�B
T,B
TFB
U�B
UgB
U�B
U�B
U�B
VmB
V�B
V�B
W
B
W
B
W?B
WsB
W�B
XB
X�B
YeB
YeB
Y�B
ZB
Z7B
ZkB
[	B
[WB
[�B
\]B
\�B
\�B
]�B
^5B
^OB
^OB
^�B
^�B
_!B
_;B
_pB
_VB
_�B
_�B
`'B
`vB
`vB
`�B
aB
a|B
a�B
bB
a�B
bB
b�B
b�B
c B
b�B
b�B
cB
b�B
cB
c:B
c:B
cTB
c�B
c�B
dB
dB
d@B
dtB
dtB
d�B
d@B
d�B
d�B
d�B
e,B
e`B
ezB
e�B
e�B
f2B
fLB
ffB
ffB
f�B
gB
gRB
g�B
g�B
h
B
g�B
h$B
hXB
h�B
h�B
h�B
h�B
h�B
h�B
h�B
h�B
i*B
iDB
iDB
iyB
i�B
j0B
jeB
jB
j�B
j�B
kB
kB
kB
kB
kB
k6B
kQB
k6B
kQB
k�B
lB
lqB
l�B
l�B
mB
mwB
m�B
m�B
n/B
nIB
ncB
ncB
n�B
n�B
n�B
n�B
o B
oB
oB
oB
oOB
o�B
o�B
p!B
poB
p�B
qAB
qAB
q�B
q�B
q�B
q�B
q�B
r-B
r�B
r�B
r�B
sMB
shB
s�B
s�B
tB
tB
tTB
tnB
t�B
t�B
t�B
u?B
u?B
u�B
u�B
u�B
u�B
v+B
vzB
v�B
v�B
v�B
wfB
wLB
wfB
w�B
w�B
w�B
xlB
xlB
x�B
x�B
x�B
y>B
y�B
y�B
y�B
y�B
z�B
z�B
{B
{B
{JB
{B
{dB
{�B
|6B
|6B
|PB
|�B
}B
}"B
}<B
}VB
}�B
}�B
}�B
~(B
~BB
~wB
~�B
.B
~�B
}B
�B
�B
�B
�4B
�OB
��B
��B
��B
��B
��B
��B
�B
�B
� B
�UB
��B
�'B
�'B
�[B
��B
��B
�B
�-B
�GB
�GB
�GB
��B
��B
��B
�B
�B
�B
�3B
��B
��B
��B
��B
�B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
NB
NpB
NVB
NpB
NpB
N�B
N�B
OvB
PbB
Q�B
QNB
T�B
XyB
bNB
cTB
e,B
e�B
f�B
e�B
ffB
iB
i�B
l�B
qvB
s�B
u�B
{�B
�aBl�Bo�Bp�BsMBxBw�Bu�Br�Bi�BQ�B-]B./B9�B\�Bw�B��B��B��B�	BƎB�$B�BB��B�iB�|B�ZB�B�B�>B��B��B��B��B��B��B׍B��B�(BˬB�?B�OB�-B��B�7B{BX�BN�B@iB6+B4nB+B�B	�B
��B
��B
�7B
��B
�,B
��B
{�B
p�B
c:B
R�B
4�B
�B	�AB	��B	�
B	��B	|PB	q'B	`�B	D�B	;JB	3�B	,B	(�B	�B	B	B	�B	�B	 �B�B	�B	B	#�B	;dB	K�B	X�B	sB	�;B	�SB	j�B	ZB	V�B	PbB	D�B	>]B	TB	UgB	cnB	kQB	lB	y�B	{dB	q'B	l=B	n/B	qAB	{0B	��B	�vB	��B	�QB	��B	�-B	}B	��B	��B	�4B	��B	��B	��B	��B	��B	��B	�dB	��B	��B	�gB	��B	�B	�~B	�oB	� B	��B	��B	��B	��B	�CB	�qB	�B	�QB	��B	��B	��B	�+B	�B	��B	�mB	��B	��B	�6B	�4B	y�B	q'B	lqB	oB	p�B	t�B	�EB	�pB	�iB	�B	��B	ѝB	�vB	��B	ބB	�TB	�-B	��B	یB	ۦB	��B	�zB	��B	�B	�bB	�B	��B	��B	��B	�4B	�4B	�B	�\B	ߊB	��B	��B	�	B	�WB	�B	�_B	��B	ּB	�kB	�CB	�7B	��B	��B	�	B	یB	ٴB	�mB	ՁB	��B	�+B	�]B	��B	ބB	ݘB	ݲB	߾B	��B	�B	�TB	��B	�mB	�B	�fB	�2B	�B	�B	��B	�B	�XB	�B	��B	�DB	�B	�kB	�WB	�CB	�B	�B	�B	�-B	��B	�?B	�B	��B	�B	�ZB	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�CB	�wB	�)B	�B	��B	�B	�B	�B	��B	��B	�B	�_B	�B	�sB	��B	��B	��B	��B	�XB	�B	��B	�qB	��B	��B	�=B	�wB	��B	�B	�B	�B	��B	�-B	�tB	�aB	�GB	�B	�B	��B	�iB	�B	�B	�B	�]B	��B	��B	�5B	�B	�B	��B	��B	�-B	�B	�FB	�zB	��B	��B	�`B	��B	��B	��B	�LB	��B	�FB	��B	��B	��B	�`B	�B	��B	��B	��B	�dB	�B	��B	�BB	��B	��B	��B
 4B
 iB
 iB
 4B
 4B	�cB
 B
;B
�B
AB
uB
�B
�B
�B
�B
-B
{B
�B
GB
�B
�B
�B
gB
�B
�B
gB
�B
�B
B
�B
aB
�B
uB
�B
[B
uB
�B
 B
 4B	��B
 �B	��B	�B	�"B	�B	�JB	�0B	��B	�6B	��B
�B
�B
 iB	��B	�B	��B	�B	�.B	��B
 B
 OB
 �B
 B
[B
-B
B
GB
�B
B
aB
�B
gB
gB
�B
gB
MB
3B
3B
�B
B
mB
B
?B
�B
tB
�B
�B
�B
�B
B
B
B
+B
+B
B
�B
_B
EB
_B
zB
�B

rB
�B
bB
�B
�B
�B
.B
HB
�B
�B
�B
�B
mB
�B
YB
�B
_B
�B
B
�B
�B
=B
=B
qB
B
B
~B
�B
�B
5B
�B
�B
VB
pB
 B
 BB
 BB
 BB
 �B
!B
!bB
!bB
!�B
!�B
!�B
!�B
"4B
#B
#B
"�B
"�B
#B
# B
# B
#:B
#nB
#�B
$@B
$�B
%B
%`B
%`B
%FB
%`B
%B
%zB
%zB
%`B
%,B
%�B
&LB
&�B
&�B
'B
'mB
'�B
(�B
)B
)DB
)�B
*B
*B
+�B
,"B
,WB
,�B
,�B
,�B
,�B
,�B
-�B
-�B
-�B
-�B
.B
.IB
.cB
.�B
/ B
.�B
.�B
/OB
/B
.�B
.�B
.�B
.�B
.�B
/ B
/iB
0;B
0�B
1[B
1�B
1�B
1�B
1�B
2B
2�B
33B
3�B
3�B
3�B
3�B
3�B
4B
4nB
4�B
5?B
5tB
5�B
5�B
6�B
72B
7�B
7�B
7�B
7�B
8B
8�B
8�B
9	B
9�B
:*B
:*B
:*B
:^B
:DB
:^B
:�B
:�B
:�B
;�B
;�B
;�B
<B
<6B
<PB
<�B
<�B
="B
=<B
=qB
=�B
>(B
>(B
>(B
>BB
>�B
>�B
>�B
?B
?HB
?HB
?}B
?}B
@ B
@�B
@�B
@�B
AUB
AUB
AUB
A�B
A�B
BB
BB
B[B
B�B
CB
CGB
CaB
C�B
C�B
C�B
C�B
C�B
C�B
D3B
D�B
D�B
D�B
D�B
E9B
E9B
EmB
EmB
E�B
E�B
E�B
E�B
E�B
F%B
FB
F?B
FtB
F�B
F�B
GEB
G�B
HKB
H�B
IB
H�B
H�B
G�B
HB
HB
H1B
H�B
H�B
IB
IRB
IlB
IB
HfB
H�B
H�B
I�B
J#B
K�B
K�B
LdB
L�B
L�B
MPB
M�B
M�B
N"B
MPB
MB
M�B
N<B
N"B
M�B
M�B
MjB
N<B
NB
NVB
NVB
N�B
N�B
N�B
N�B
OBB
PHB
PbB
QB
Q4B
QB
R�B
RTB
R�B
R�B
R�B
R�B
S&B
S&B
S&B
S�B
S[B
S�B
T,B
TFB
U�B
UgB
U�B
U�B
U�B
VmB
V�B
V�B
W
B
W
B
W?B
WsB
W�B
XB
X�B
YeB
YeB
Y�B
ZB
Z7B
ZkB
[	B
[WB
[�B
\]B
\�B
\�B
]�B
^5B
^OB
^OB
^�B
^�B
_!B
_;B
_pB
_VB
_�B
_�B
`'B
`vB
`vB
`�B
aB
a|B
a�B
bB
a�B
bB
b�B
b�B
c B
b�B
b�B
cB
b�B
cB
c:B
c:B
cTB
c�B
c�B
dB
dB
d@B
dtB
dtB
d�B
d@B
d�B
d�B
d�B
e,B
e`B
ezB
e�B
e�B
f2B
fLB
ffB
ffB
f�B
gB
gRB
g�B
g�B
h
B
g�B
h$B
hXB
h�B
h�B
h�B
h�B
h�B
h�B
h�B
h�B
i*B
iDB
iDB
iyB
i�B
j0B
jeB
jB
j�B
j�B
kB
kB
kB
kB
kB
k6B
kQB
k6B
kQB
k�B
lB
lqB
l�B
l�B
mB
mwB
m�B
m�B
n/B
nIB
ncB
ncB
n�B
n�B
n�B
n�B
o B
oB
oB
oB
oOB
o�B
o�B
p!B
poB
p�B
qAB
qAB
q�B
q�B
q�B
q�B
q�B
r-B
r�B
r�B
r�B
sMB
shB
s�B
s�B
tB
tB
tTB
tnB
t�B
t�B
t�B
u?B
u?B
u�B
u�B
u�B
u�B
v+B
vzB
v�B
v�B
v�B
wfB
wLB
wfB
w�B
w�B
w�B
xlB
xlB
x�B
x�B
x�B
y>B
y�B
y�B
y�B
y�B
z�B
z�B
{B
{B
{JB
{B
{dB
{�B
|6B
|6B
|PB
|�B
}B
}"B
}<B
}VB
}�B
}�B
}�B
~(B
~BB
~wB
~�B
.B
~�B
}B
�B
�B
�B
�4B
�OB
��B
��B
��B
��B
��B
��B
�B
�B
� B
�UB
��B
�'B
�'B
�[B
��B
��B
�B
�-B
�GB
�GB
�GB
��B
��B
��B
�B
�B
�B
�3B
��B
��B
��B
��B
�B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104924  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604174026  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604174027  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604174027                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605024034  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605024034  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610131507                      G�O�G�O�G�O�                