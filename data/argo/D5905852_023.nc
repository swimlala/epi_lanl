CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2019-09-20T09:38:24Z creation;2019-09-20T09:38:25Z conversion to V3.1;2022-08-02T05:12:04Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         BPRIMARY|https://orcid.org/0000-0001-9150-6442|Kanako Sato, JAMSTEC        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7,   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7<   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7@   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7D   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7T   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7d   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7t   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7|   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8,   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    80   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    84   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     88   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8X   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8\   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8`   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8�   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
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
_FillValue                    9    CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        :    PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  :   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]d   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  aT   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  q   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �\   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �L   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �d   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ߬   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �,   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �0   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �4   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �8   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �<   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �|   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20190920093824  20220818081508  5905852                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA  A30_8420_023                    2C  D   APEX                            8420                            2.11.2                          846 @������ 1   @���3�J�@.^($x�c�i�B��1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @33@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�ffB�  B�  B�  B�33B�ffB���B���B�  B�33B�33B�  B�ffB�33BǙ�B�33B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C�C
  C  C�fC  C  C�fC  C  C  C  C  C   C"�C$�C%�fC(  C*  C,  C.�C0  C1�fC3�fC6  C8  C:  C<  C=�fC@  CB  CD  CF  CH  CJ  CL  CN  CP  CR�CT33CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D6��D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D��3D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dփ3D��3D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�i�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�H@�=q@��HAA!��AAAaG�A�z�A��RA���A���A���A�z�A���A���B G�B=qB33B=qB Q�B(Q�B0ffB8Q�B@\)BHp�BPffBX\)B`Q�BhQ�BpffBx\)B�.B�33B�.B�(�B��=B�{B��B�(�B�p�B���B�ǮB���B�#�B�G�B�p�B�{B��{B�ffB��)B�W
B��B��fB��B�.B�#�B�#�B�(�B�#�B�#�B�(�B�8RB�B�C �C�C�C�C!HC
�C�C  C\C\C�C�C�C\C\C\C \C"#�C$+�C&�C({C*�C,)C.!HC0)C2�C4�C6�C8�C:�C<�C>�C@�CB�CD
CF�CH{CJ{CL
CN{CP�CR#�CTB�CV�CX\CZ�C\
C^{C`�Cb{Cd�Cf\Ch�Cj\Cl\Cn{Cp�Cr�Ct�Cv\Cx{Cz�C|�C~�C�C��C�fC�
=C�C�C��C��C�C��C��C��C��C�C��C��C�fC�fC�
=C�C��C�
=C�
=C��C��C�
=C��C��C�{C��C�
=C�
=C��C��C��C�fC�
=C��C��C��C��C�
=C�C��C�
=C��C��C��C�C��C��C��C��C��C�
=C�fC��C��C��C�
=C��C�
=C�fC��C��C�
=C��C��C��C�C��C�fC�C��C��C�
=C�
=C�C��C�fC��C�
=C��C�C��C��C�\C��C��C��C��C�
=C�\C��C�
=C��C�
=C��C��C�
=C�
=C�
=C��C��C��C��C��C��C��C��C�
=C��C��C�
=C��C��C��C��C��C��C��C�
=C�
=C��C�
=C��C��C��D �D ��D{D�DD��D�D��D{D��DD��D{D�fD{D�{DfD�D	�D	�D
�D
��D�D�{D{D�DfD�fD{D�3D�D��D�D�3D�D�DD�D
D�fD�D��D
D��D�D�DD�D�D�fDfD�fDfD�fDD�3D3D�
D�D�{D�D�3D�D�D {D ��D!�D!�RD"{D"�{D#�D#��D$fD$�{D%D%�D&{D&�D'�D'��D({D(�D){D)��D*3D*��D+{D+��D,{D,�D-fD-��D.�D.�3D/�D/��D0{D0��D13D1�3D2�D2�D3�D3��D4�D4��D5D5��D6{D6��D7HD7��D8fD8�fD9D9�D:D:�{D;D;��D<�D<��D=3D=��D>�D>�3D?�D?�D@{D@��DADA�{DB3DB�fDC
DC�{DD{DD��DE{DE�3DF{DF�DGfDG�fDH�DH��DI�DI�DJ{DJ��DKRDK��DLfDL��DM�DM�{DNfDN�DO3DO��DPfDP�{DQ�DQ��DR�DR�DSfDS��DT{DT�DURDU��DV{DV��DW{DW�DXDX�{DY�DY��DZfDZ�fD[{D[�{D\fD\�fD]fD]��D^fD^�fD_D_��D`D`�{DaDa�DbDb�DcDc��DdDd�De�De��DffDf��Dg3Dg�{Dh{Dh�fDifDi��Dj�Dj��Dk�Dk�3Dl3Dl��Dm3Dm�fDnfDn�{Do3Do��Dp�Dp�Dq�Dq�DrDr�DsfDs�
Dt�Dt�{Du{Du��Dv�Dv�{DwDw�{Dx3Dx��Dy3Dy�
Dz�Dz�D{{D{�D|�D|�{D}�D}�RD~D~�DfD��D��D�B�D��3D���D��D�A�D���D��)D��D�A�D���D���D��D�B�D���D���D��D�B�D��3D�D��D�B�D��=D���D��D�A�D���D�D��D�B=D���D���D��D�A�D���D���D��D�B=D���D��=D��D�B�D��3D�D��D�C�D���D�D��D�B�D���D��=D��D�B=D���D���D��D�C3D��HD��HD��D�B=D���D���D�=D�B�D���D���D�=D�C�D���D��3D�=D�C�D���D�ÅD�3D�C3D��3D��=D��D�C�D���D��3D�)D�B�D��=D���D��D�B�D���D���D��D�B=D���D���D��D�C�D��3D�D�=D�B�D���D���D�=D�B=D��=D���D��D�B�D���D��=D�=D�B�D���D���D��D�B=D���D���D�=D�C3D���D���D��D�C3D���D��)D�3D�B=D��HD��HD�=D�B=D���D�ÅD��D�C3D���D���D��D�B�D���D��3D��D�B�D���D��3D��D�B�D��=D��=D�=D�C3D���D���D�HD�B�D��=D��HD��D�B�D���D���D��D�B=D���D���D�=D�B�D���D���D��D�B=D��=D�D�=D�A�D���D�D�3D�B�D��=D��=D��D�B�D��=D���D��D�A�D���D��=D��D�B�D���D�D�=D�A�D���D�D��D�C3D���D���D��D�A�D��3D���D�HD�B�D��3D��3D��D�@�D���D��3D�3D�B�D��=D��=D�=D�B�D���D���D�)D�C3D��3D���D��D�B�D���D�D��D�C�D��3D��3D�3D�A�D���D�ÅD�3D�B�D���D���D�=D�C3D��=D��=D��D�B�D��=D���D��D�C3D���D��)D��D�B=D���D���D�HD�B�D��3D���D��D�C3D�D���D��D�@�DÁHD���D��D�B=Dā�D��=D�=D�B�DŃ3D���D��D�B�DƂ�D���D��D�A�Dǂ�D��3D�3D�B�Dȃ�D���D�3D�B�Dɂ�D��=D��D�B�Dʁ�D���D��D�A�Dˁ�D���D��D�C3D̂�D���D��D�B=D́�D��=D�=D�A�D΁�D���D��D�B�Dς�D�D��D�C�DЂ�D��=D��D�B�Dт�D���D��D�B=D҂�D��=D��D�B�Dӂ=D��=D��D�A�Dԁ�D���D�3D�B�DՁ�D���D� �D�B�DօD���D��D�B=Dׂ=D���D��D�B�D؂�D���D��D�B�Dق�D�D�=D�B�Dڂ�D���D��D�B=Dۂ�D��3D��D�B=D܂=D���D��D�C�D݃3D��=D��D�B�Dނ�D�D��D�A�D߁�D�D�=D�A�D���D��HD�HD�A�D�=D���D�3D�C�D⃅D��=D�=D�B�D��D���D��D�B�D��D�D�=D�C3D�)D�ÅD�3D�B�D�=D��=D��D�B�D��D��3D��D�C�D肏D���D��D�B�D�3D�D��D�B�D��D���D��D�B�D�HD���D��D�B=D�=D��=D�=D�B�D��D��3D��D�B�DD��=D��D�C�DD���D��D�B=D��=D�D��D�AHD�D���D��D�B�D��D�ÅD�=D�B�D�D��=D�=D�B=D�)D���D�=D�A�D���D���D��D�B�D���D�D� �D�A�D���D���D�=D�B�D���D��=D��D�@�D���D���D��D�B�D���D���D��D�C3D�mq1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�A�A�=A�D�A�E�A�DgA�B'A���A��A���A��NA��A���A���A���A�ɆA��tA�zA��A�cA�K)A�D3A�C�A�@�A�YA�7�A�	�A��%A���A߾�Aߏ\A�x�A�V9A�.}A��A�MA��A�b�A�A�	A�.IA�kA�e`A�i�A�ĜA��YA�C�A�%A�|�A� 4A�+A�|PA���A��A�҉A���A��A�ȀA��A��A��A�7�A��zA�VA�jA�A���A�R�A�ӏA���A�$@A��A���A�l�A��tA�{JA� 'A�o A��A{�^Ay�&AwHAtE9Ar�uAo�rAk� Ai�TAh�bAgqA_�AV�!AO��AN^5AMdZAK��AIR�AFsAD�gADm]AB�6AAj�A=�KA= �A;t�A8��A7A4��A1�TA1`�A11�A0ƨA0�	A/�0A.
�A+��A*��A*S�A*�A)ZA(�DA(�A'�uA'%�A&�zA&�A%�)A%hsA%$A$��A#�+A"��A!��A �	A��A�+A��A�nAp;A�A>BAzxA��A�A�~A($A��A<6A�An�A^5AC�A��A1'A�A�3A�A�dAg�A�A�BAg8A�7A��A�fA�.Aj�AW?A�A�6AP�AA�:A7�A��AqA~�A{JA&�A
͟A
��A
��A
>BA	��A	G�AĜAE9A��A"hA�A�qAx�Ah
A[WARTA1�A��A7LA�.A�9AC-A�A	A��AHAȴAA�A��A�@As�A�eAIRA�AIRAP�A ��A �@��rA [WA g�A YKA C@��@��@�@�@���@��$@�?@���@��@��4@���@���@�c @�@�h
@�E9@��@�s�@���@��@�6z@�Z@��@�W?@�@�f�@��E@��@궮@鋬@��@��@�*�@�k�@���@�
�@�*0@�Ɇ@�j@��@��M@�bN@��#@�s@�
=@���@�(�@ߛ=@��P@��@��s@�]d@��D@݃{@���@�x�@�e,@��@�s�@�<�@��>@�x@ؑ @�N�@��@ש*@��U@��@�ѷ@�#:@��@Ӱ�@�zx@���@��@��@�8�@��@�]�@�@@��@�}V@�=q@�A�@�K^@�D�@��+@��@ϵt@�W?@���@΋D@΄�@�q@��@�4�@��X@�l�@˛=@��@�S�@ɍP@�-w@�z�@�;�@�|@��v@��X@�e�@��W@���@�Q�@�ں@��@�rG@�$t@���@���@�^5@�)_@��@��r@�e�@�_�@�@�@�خ@��"@�Y�@�J�@�(�@���@��s@��D@�W�@�,=@���@��Z@��g@��~@�=�@��\@���@���@�j@�C@��@���@�V�@�S�@�?@��A@��4@�4�@��@���@���@�GE@��@���@�N<@�)_@��@���@�k�@�"�@��@���@�q@�O@��;@�l�@�5�@���@���@�PH@�!@��@�c�@�N<@�L�@�4@���@��Y@�D�@��]@���@�n/@�͟@�u�@�+k@��m@��@���@�l�@��@�ȴ@��r@��@���@��$@�dZ@�&@��)@�M@��@���@���@�|�@�T�@�)_@�Ĝ@�s�@�&�@��;@���@�W?@��@���@�W�@��o@�W?@�&�@�ں@�C-@��@��@��M@��B@���@���@��@�B[@��@���@�`B@��@��@�V@���@��0@�}�@�Dg@��@��@��.@�:�@���@���@��V@�zx@�!�@��@��I@�{�@�($@��g@���@���@�F@�8�@�&@��@�YK@�(�@���@��&@��-@���@��k@���@��@�Ĝ@��R@���@���@�|�@�@��M@�+�@���@�ȴ@��o@�R�@�u@��^@�RT@��6@��o@��@�q@�($@���@��@�hs@�8@��|@�҉@�ȴ@��m@��j@��u@�N�@�&�@�  @���@�H�@���@���@���@�|�@�u@��@@�#�@��,@��o@�n�@�w�@�Ft@�e@�x@���@���@�v`@�@O@�+@��@�i�@�($@��@��6@��=@�J#@���@���@���@�n�@�($@���@���@��@��@�l"@�_@��[@���@�\�@�!-@��@��$@�r�@�/�@��.@���@��S@�\�@�@@���@��6@�?�@�x@��w@��[@��=@�B�@��@��<@���@�r�@�Q�@�?�@��@�ݘ@��'@�]�@���@��<@���@� �@�@�@J#@.I@~��@~d�@}��@}-w@|�@|�.@|�@|r�@{�r@z�"@y�@yT�@y�@x�|@x�@w��@ws@w�@v��@v��@v!�@u�@u|@uf�@uDg@u@tr�@s�f@rE�@q�9@q��@q�"@q+�@p�)@p��@pH@o��@n�@nn�@n=q@n�@m��@m��@m�>@m��@m�N@m�n@l�e@k�;@k�	@j�y@jYK@j	@i��@i@h�|@h�@h �@g�0@g;d@g)_@f@�@e�@e@e<6@d�U@du�@d,=@c�@c�@c{J@c+@b�L@b{�@b!�@a�>@a�=@arG@ahs@a7L@`֡@`]d@_��@_��@_O@_C@^�@^�@^i�@^J@]�@]��@]��@\��@\oi@\:�@[��@[�@Z�+@Zq�@Zp;@Zc @Z0U@Y��@Y^�@Y�@X�@XtT@XK^@W��@V��@VJ�@U�Z@U�H@Uu�@U(�@T��@T�U@T�u@Tg8@T1'@Sݘ@S�	@S'�@R�}@Q��@Q�'@QrG@P�@PXy@P�@O��@O��@O4�@OC@N�2@N^5@N@�@N1�@N-@N.�@M��@M=�@L�@Ll"@L(�@K�
@Kv`@K!-@J�}@J��@J��@JZ�@J
�@I�z@I��@I��@I��@Ic�@I[W@I?}@H��@H�)@H|�@G�@G|�@G]�@F�@F=q@F	@E�>@E��@Ex�@EVm@D��@D�Y@Db@C��@CP�@C�@B��@A��@A��@@�j@?�Q@?qv@?Mj@?Y@? i@>��@>�}@=�@=��@=c@=p�@=hs@<�P@<%�@;��@;6z@:��@:R�@:($@9��@8ѷ@7��@7_p@6�@6	@5�@4��@4b@3�;@3�@@3dZ@3.I@2�A@2!�@2 �@1�@1s�@0��@09X@/ƨ@/�w@/��@/"�@.��@.6�@-�T@-��@-8�@,�5@,m�@,1'@,~@,G@+�;@+�	@+W?@+�@*��@*�r@*GE@*0U@*$�@*@)�@)Q�@)0�@(��@(�5@(��@(ѷ@(r�@('R@($@(�@'�6@'�P@'>�@'C@'�@&�@&�m@&��@&a|@&Ov@&6�@&
�@%��@%��@%o @%Vm@%?}@%	l@$�`@$�j@$�@$�u@$w�@$]d@$!@#��@#˒@#~�@#iD@#P�@#1�@#$t@"��@"��@"�A@"h
@"Ta@"?@!@!�n@!�@!��@!��@!s�@!a�@!=�@!@!;@ ��@ ی@ �@ �Y@ ~(@ [�@ D�@ A�@ C-@ D�@ 9X@  �@ ~@ �@�@�@�0@b�@8@�@�2@�@�s@҉@��@_�@C�@0U@4@��@�7@f�@-w@�@�@�@Ɇ@�@H@*�@(�@"h@�6@��@�P@�f@X�@K�@;d@��@J�@8�@_@zx@Y�@Dg@+@��@��@M@�@ƨ@|�@RT@��@��@�x@~�@V@	@�-@}�@e,@B�@#�@�@�@��@��@l"@j@<�@(�@@�V@S�@&@o@�"@��@��@�\@GE@u@�N@�^@�n@m]@hs1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�A�A�=A�D�A�E�A�DgA�B'A���A��A���A��NA��A���A���A���A�ɆA��tA�zA��A�cA�K)A�D3A�C�A�@�A�YA�7�A�	�A��%A���A߾�Aߏ\A�x�A�V9A�.}A��A�MA��A�b�A�A�	A�.IA�kA�e`A�i�A�ĜA��YA�C�A�%A�|�A� 4A�+A�|PA���A��A�҉A���A��A�ȀA��A��A��A�7�A��zA�VA�jA�A���A�R�A�ӏA���A�$@A��A���A�l�A��tA�{JA� 'A�o A��A{�^Ay�&AwHAtE9Ar�uAo�rAk� Ai�TAh�bAgqA_�AV�!AO��AN^5AMdZAK��AIR�AFsAD�gADm]AB�6AAj�A=�KA= �A;t�A8��A7A4��A1�TA1`�A11�A0ƨA0�	A/�0A.
�A+��A*��A*S�A*�A)ZA(�DA(�A'�uA'%�A&�zA&�A%�)A%hsA%$A$��A#�+A"��A!��A �	A��A�+A��A�nAp;A�A>BAzxA��A�A�~A($A��A<6A�An�A^5AC�A��A1'A�A�3A�A�dAg�A�A�BAg8A�7A��A�fA�.Aj�AW?A�A�6AP�AA�:A7�A��AqA~�A{JA&�A
͟A
��A
��A
>BA	��A	G�AĜAE9A��A"hA�A�qAx�Ah
A[WARTA1�A��A7LA�.A�9AC-A�A	A��AHAȴAA�A��A�@As�A�eAIRA�AIRAP�A ��A �@��rA [WA g�A YKA C@��@��@�@�@���@��$@�?@���@��@��4@���@���@�c @�@�h
@�E9@��@�s�@���@��@�6z@�Z@��@�W?@�@�f�@��E@��@궮@鋬@��@��@�*�@�k�@���@�
�@�*0@�Ɇ@�j@��@��M@�bN@��#@�s@�
=@���@�(�@ߛ=@��P@��@��s@�]d@��D@݃{@���@�x�@�e,@��@�s�@�<�@��>@�x@ؑ @�N�@��@ש*@��U@��@�ѷ@�#:@��@Ӱ�@�zx@���@��@��@�8�@��@�]�@�@@��@�}V@�=q@�A�@�K^@�D�@��+@��@ϵt@�W?@���@΋D@΄�@�q@��@�4�@��X@�l�@˛=@��@�S�@ɍP@�-w@�z�@�;�@�|@��v@��X@�e�@��W@���@�Q�@�ں@��@�rG@�$t@���@���@�^5@�)_@��@��r@�e�@�_�@�@�@�خ@��"@�Y�@�J�@�(�@���@��s@��D@�W�@�,=@���@��Z@��g@��~@�=�@��\@���@���@�j@�C@��@���@�V�@�S�@�?@��A@��4@�4�@��@���@���@�GE@��@���@�N<@�)_@��@���@�k�@�"�@��@���@�q@�O@��;@�l�@�5�@���@���@�PH@�!@��@�c�@�N<@�L�@�4@���@��Y@�D�@��]@���@�n/@�͟@�u�@�+k@��m@��@���@�l�@��@�ȴ@��r@��@���@��$@�dZ@�&@��)@�M@��@���@���@�|�@�T�@�)_@�Ĝ@�s�@�&�@��;@���@�W?@��@���@�W�@��o@�W?@�&�@�ں@�C-@��@��@��M@��B@���@���@��@�B[@��@���@�`B@��@��@�V@���@��0@�}�@�Dg@��@��@��.@�:�@���@���@��V@�zx@�!�@��@��I@�{�@�($@��g@���@���@�F@�8�@�&@��@�YK@�(�@���@��&@��-@���@��k@���@��@�Ĝ@��R@���@���@�|�@�@��M@�+�@���@�ȴ@��o@�R�@�u@��^@�RT@��6@��o@��@�q@�($@���@��@�hs@�8@��|@�҉@�ȴ@��m@��j@��u@�N�@�&�@�  @���@�H�@���@���@���@�|�@�u@��@@�#�@��,@��o@�n�@�w�@�Ft@�e@�x@���@���@�v`@�@O@�+@��@�i�@�($@��@��6@��=@�J#@���@���@���@�n�@�($@���@���@��@��@�l"@�_@��[@���@�\�@�!-@��@��$@�r�@�/�@��.@���@��S@�\�@�@@���@��6@�?�@�x@��w@��[@��=@�B�@��@��<@���@�r�@�Q�@�?�@��@�ݘ@��'@�]�@���@��<@���@� �@�@�@J#@.I@~��@~d�@}��@}-w@|�@|�.@|�@|r�@{�r@z�"@y�@yT�@y�@x�|@x�@w��@ws@w�@v��@v��@v!�@u�@u|@uf�@uDg@u@tr�@s�f@rE�@q�9@q��@q�"@q+�@p�)@p��@pH@o��@n�@nn�@n=q@n�@m��@m��@m�>@m��@m�N@m�n@l�e@k�;@k�	@j�y@jYK@j	@i��@i@h�|@h�@h �@g�0@g;d@g)_@f@�@e�@e@e<6@d�U@du�@d,=@c�@c�@c{J@c+@b�L@b{�@b!�@a�>@a�=@arG@ahs@a7L@`֡@`]d@_��@_��@_O@_C@^�@^�@^i�@^J@]�@]��@]��@\��@\oi@\:�@[��@[�@Z�+@Zq�@Zp;@Zc @Z0U@Y��@Y^�@Y�@X�@XtT@XK^@W��@V��@VJ�@U�Z@U�H@Uu�@U(�@T��@T�U@T�u@Tg8@T1'@Sݘ@S�	@S'�@R�}@Q��@Q�'@QrG@P�@PXy@P�@O��@O��@O4�@OC@N�2@N^5@N@�@N1�@N-@N.�@M��@M=�@L�@Ll"@L(�@K�
@Kv`@K!-@J�}@J��@J��@JZ�@J
�@I�z@I��@I��@I��@Ic�@I[W@I?}@H��@H�)@H|�@G�@G|�@G]�@F�@F=q@F	@E�>@E��@Ex�@EVm@D��@D�Y@Db@C��@CP�@C�@B��@A��@A��@@�j@?�Q@?qv@?Mj@?Y@? i@>��@>�}@=�@=��@=c@=p�@=hs@<�P@<%�@;��@;6z@:��@:R�@:($@9��@8ѷ@7��@7_p@6�@6	@5�@4��@4b@3�;@3�@@3dZ@3.I@2�A@2!�@2 �@1�@1s�@0��@09X@/ƨ@/�w@/��@/"�@.��@.6�@-�T@-��@-8�@,�5@,m�@,1'@,~@,G@+�;@+�	@+W?@+�@*��@*�r@*GE@*0U@*$�@*@)�@)Q�@)0�@(��@(�5@(��@(ѷ@(r�@('R@($@(�@'�6@'�P@'>�@'C@'�@&�@&�m@&��@&a|@&Ov@&6�@&
�@%��@%��@%o @%Vm@%?}@%	l@$�`@$�j@$�@$�u@$w�@$]d@$!@#��@#˒@#~�@#iD@#P�@#1�@#$t@"��@"��@"�A@"h
@"Ta@"?@!@!�n@!�@!��@!��@!s�@!a�@!=�@!@!;@ ��@ ی@ �@ �Y@ ~(@ [�@ D�@ A�@ C-@ D�@ 9X@  �@ ~@ �@�@�@�0@b�@8@�@�2@�@�s@҉@��@_�@C�@0U@4@��@�7@f�@-w@�@�@�@Ɇ@�@H@*�@(�@"h@�6@��@�P@�f@X�@K�@;d@��@J�@8�@_@zx@Y�@Dg@+@��@��@M@�@ƨ@|�@RT@��@��@�x@~�@V@	@�-@}�@e,@B�@#�@�@�@��@��@l"@j@<�@(�@@�V@S�@&@o@�"@��@��@�\@GE@u@�N@�^@�n@m]@hs1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B��B��B�8B��B��B�_B҉B	B	�B	oB	�B	�B	�B	MB	B	�B	�B	!B	M�B	j�B	n}B	n�B	q�B	�MB	�hB	�4B	�4B	�nB	��B	��B	�mB	��B	��B	��B	��B	��B	�;B	��B	y�B	w�B	�B	��B	�B	��B	�B	��B	�kB	�_B	�B
4B
3MB
4�B
W�B
s�B
� B
��B
�SB
�4B
��B
�B
��B
�B
��B
��B
��B�B�B �B
��B
ߤB
��B
~wB
S&B
1vB

#B	��B	ӏB	�-B	�OB	B	��B	�FB	��B	�B	��B	zxB	rB	e�B	5�B	�B�wB�B��B�-B��B��B̈́B�vB��B� B�SB�B�%BǮB��B��B�uB��B�bB��B��B	�B	�B	aB	�B	�B	;B	-B	2�B	9�B	B�B	I�B	O�B	Q�B	PHB	U�B	YeB	[�B	V9B	U�B	V�B	[WB	]~B	b4B	gmB	j�B	m]B	m�B	m�B	qAB	zxB	{�B	{0B	�B	��B	��B	��B	�XB	��B	��B	�{B	�sB	��B	�EB	�WB	�WB	��B	��B	��B	��B	��B	��B	�KB	��B	��B	��B	�B	�aB	�FB	��B	�*B	��B	��B	��B	�`B	�tB	��B	��B	��B	��B	��B	��B	��B	��B	�	B	�DB	�B	��B	�6B	�]B	��B	�HB	�HB	��B	��B	�B	�BB	��B	�B	�^B	��B	��B	��B	��B	��B	�(B	�UB	��B	�EB	�EB	�zB	οB	�uB	�KB	�,B	��B	�8B	��B	�B	��B	�FB	�ZB	��B	�B	ߊB	�B	�~B	��B	�#B	�B	�1B	��B	�yB	��B	�	B	ܒB	�~B	��B	߾B	�pB	ޞB	�VB	ߊB	��B	��B	�B	�B	�B	�!B	��B	��B	��B	�B	��B	�)B	ܬB	ܒB	��B	�pB	�\B	�|B	��B	�\B	�-B	�B	ߤB	��B	ޞB	�5B	�B	�dB	�B	޸B	��B	�CB	�]B	�OB	�!B	�hB	�B	��B	�B	�B	�B	�RB	�mB	�B	�2B	�FB	��B	�B	�B	�2B	��B	�RB	�2B	�B	�B	�B	�XB	�B	�B	�B	�B	��B	�]B	�B	�[B	�B	�vB	��B	�UB	�!B	�B	�wB	�]B	�"B	�B	�0B	��B	��B	��B	�KB	��B	�B	��B	�B	�B	�UB	�;B	�B	�}B	��B	��B	�CB	�B	�5B	��B	�IB	��B	��B	��B	��B	�cB	��B	�OB	�OB	�iB	�B	��B	�B	�B	��B	�B	��B	��B	��B	��B	�`B	�2B	�B	�B	��B	�$B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	��B	�"B	�"B	�qB	��B	�VB	��B	��B	�}B
  B
  B
 �B
 �B
B
B
�B
�B
aB
�B
B
MB
�B
B
B
�B
�B
B
B
B
9B
mB
�B
%B
�B
�B
?B
B
�B
zB
fB
�B
�B

rB

#B

#B

rB

	B

�B
xB
�B
�B
~B
�B
�B
B
jB
�B
<B
�B
�B
�B
B
�B
�B
HB
�B
�B
 B
�B
�B
�B
�B
�B
�B
�B
�B
[B
@B
�B
�B
�B
aB
�B
�B
2B
gB
�B
�B
�B
�B
2B
�B
B
�B
9B
�B
�B
�B
�B

B
mB
�B
�B
�B
�B
mB
�B
�B
�B
�B
�B
�B
B
7B
QB
�B
WB
�B
�B
�B
�B
B
B
�B
�B
�B
�B
�B
 BB
 �B
!B
!|B
!|B
"NB
"B
"�B
"�B
"B
"B
"hB
# B
# B
#:B
#TB
#:B
#:B
#nB
#:B
"�B
"�B
$tB
$�B
%FB
&2B
&fB
%�B
%zB
%�B
%�B
&2B
&�B
)_B
)_B
)�B
)�B
)�B
)�B
)�B
*eB
*B
*B
+B
+B
+B
+kB
+�B
,=B
,�B
,�B
,�B
-wB
-)B
-)B
,�B
+B
+B
+�B
,WB
,�B
,�B
-CB
-�B
-�B
.cB
.�B
/�B
0;B
0�B
0�B
0�B
1'B
1�B
1�B
2�B
2GB
2�B
2�B
2�B
3B
2�B
3B
3�B
3�B
3�B
3�B
4nB
4�B
4�B
4�B
5ZB
5�B
5tB
5ZB
5�B
5ZB
5�B
6`B
7B
6�B
6�B
6+B
6+B
6+B
6FB
6B
6+B
6zB
6�B
72B
72B
6�B
6�B
7�B
7�B
8B
8�B
9�B
9�B
:^B
:�B
:�B
:�B
:�B
:�B
;JB
;0B
;JB
;dB
;JB
;0B
;0B
;0B
;JB
;�B
;�B
<B
<B
<PB
<jB
<PB
<jB
<jB
<PB
<B
<�B
=B
=VB
>(B
>�B
>�B
?cB
?�B
?�B
@ B
?�B
@OB
@OB
@4B
@�B
@�B
@�B
A B
A B
A;B
A�B
A�B
A�B
A�B
A�B
A�B
B'B
B�B
B�B
B�B
B�B
B�B
CGB
C�B
C�B
C�B
DMB
DgB
D�B
D�B
D�B
D�B
EmB
E�B
E�B
E�B
E�B
E�B
E�B
F%B
F�B
GB
F�B
F�B
F�B
F�B
GzB
G�B
G�B
G�B
HB
G�B
HfB
H�B
IlB
I7B
I�B
I�B
J#B
J#B
J#B
I�B
J	B
J#B
J#B
JrB
J�B
KB
K�B
K�B
LB
L�B
L�B
MB
L�B
MB
M6B
MPB
M�B
N"B
NB
N"B
N"B
M�B
N�B
N�B
OBB
OBB
O\B
O�B
O�B
PHB
P}B
PbB
P}B
P�B
QB
Q�B
Q�B
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
S&B
SuB
S�B
TB
TB
TFB
T,B
TB
T{B
T�B
T�B
T�B
TFB
TaB
T{B
T�B
T�B
UMB
VB
VB
V9B
VmB
VmB
VmB
V�B
W$B
W$B
WYB
WYB
W$B
WYB
X+B
X_B
X�B
X�B
YKB
Y1B
YB
Z7B
[=B
[�B
\)B
\�B
\�B
]�B
^5B
^OB
^�B
^�B
^�B
_;B
_pB
_�B
_�B
_�B
`BB
`�B
a-B
aB
aB
a�B
a�B
bB
bhB
b�B
b�B
b�B
cnB
c�B
c�B
c�B
c�B
c�B
dB
d&B
dtB
d�B
d�B
d�B
d�B
d�B
eFB
e�B
e�B
e�B
e�B
e�B
e�B
fLB
ffB
ffB
f�B
f�B
f�B
gB
g8B
g8B
g8B
gRB
g�B
g�B
g�B
g�B
g�B
g�B
h$B
hXB
hsB
hsB
h�B
h�B
h�B
h�B
h�B
iB
iB
i_B
iyB
i�B
i�B
i�B
i�B
jB
i�B
j0B
jB
j�B
j�B
j�B
j�B
kkB
k�B
k�B
k�B
k�B
k�B
k�B
l"B
l=B
lWB
lqB
lqB
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
m)B
m)B
mwB
mwB
m�B
mwB
m�B
m�B
nB
n/B
nIB
n/B
n/B
ncB
n�B
n�B
n�B
n�B
oiB
oiB
o�B
o�B
o�B
o�B
o�B
o�B
o�B
p;B
p!B
p;B
p;B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
r|B
r�B
r|B
r�B
r�B
sB
s�B
s�B
s�B
tB
tB
t�B
t�B
t�B
t�B
t�B
uB
u�B
u�B
u�B
u�B
u�B
u�B
u�B
vFB
v`B
vzB
vzB
v�B
v�B
w2B
w�B
w�B
x8B
x8B
x8B
xRB
xRB
x�B
x�B
y	B
y>B
y>B
yrB
y�B
y�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B��B��B�RB�$B�B�eB�B	 �B	�B	oB	B	�B	B	gB	9B	�B	B	!�B	M�B	j�B	n}B	n�B	q�B	��B	��B	�hB	��B	��B	�FB	�B	��B	�sB	�mB	��B	��B	��B	�B	�lB	�3B	� B	�PB	��B	��B	�DB	�B	�B	�jB	��B	�"B
?�B
8B
;�B
_�B
yXB
�&B
��B
��B
�&B
�B
��B
��B
��B
�B
�5B
�^BuBMB�B
��B
�B
ʦB
��B
X_B
8RB
�B	�QB	רB	�zB	��B	��B	�%B	�$B	�5B	��B	��B	|�B	vzB	oOB	?�B	
rB�OB�QB�yB�@B��B��BΥBѝB�MB��B��BāB�7B�=B��B��B�B�B�B��B�B	zB	�B	MB	JB	$B	 \B	-�B	3MB	:�B	CaB	JXB	PHB	RB	P�B	VSB	ZkB	\�B	WYB	W$B	W�B	\�B	^5B	b�B	g�B	kQB	nB	o B	n�B	r|B	{JB	|PB	{�B	��B	�[B	�MB	�B	�rB	�B	��B	�B	��B	�B	�1B	��B	��B	�#B	�WB	��B	�B	��B	��B	��B	��B	�B	�!B	�[B	��B	��B	��B	��B	��B	�>B	��B	�fB	��B	�B	�B	��B	�lB	��B	�DB	�DB	��B	��B	��B	�JB	�0B	��B	�wB	��B	�}B	��B	�}B	�OB	�}B	��B	�BB	�PB	�0B	��B	�XB	�$B	��B	��B	�(B	�oB	��B	ǔB	�zB	�_B	��B	�,B	��B	�,B	֡B	�8B	�B	�B	��B	�2B	�FB	�B	��B	�B	��B	��B	�]B	��B	��B	�B	ٚB	�1B	ٚB	�qB	��B	��B	�'B	�B	��B	�B	�pB	ߤB	��B	�sB	�RB	��B	�nB	�VB	�OB	�dB	�xB	�xB	�]B	ܬB	��B	��B	�~B	��B	�B	��B	�B	��B	�|B	�BB	��B	�-B	޸B	�OB	ބB	��B	ޞB	�pB	�)B	�]B	ܬB	޸B	�VB	�B	�&B	�,B	�B	��B	�
B	�
B	�
B	�B	�B	�`B	��B	��B	��B	�LB	��B	�B	�B	�LB	�B	�B	�B	��B	��B	�B	�B	�)B	�wB	��B	�B	��B	�B	�B	�oB	��B	�;B	��B	��B	��B	��B	�B	�eB	�B	�DB	�B	�WB	��B	��B	�IB	��B	��B	��B	�!B	� B	�CB	�"B	�]B	��B	�B	�B	�B	��B	�B	�B	�B	�B	�/B	�OB	�OB	�iB	�B	�B	�;B	��B	��B	�B	��B	��B	�+B	�+B	��B	��B	�B	�lB	��B	�XB	�B	��B	��B	��B	�B	�*B	��B	�B	�6B	�jB	�B	�VB	�qB	��B	��B	��B	�BB
  B	��B
 B
 4B
 �B
B
;B
AB
�B
�B
�B
B
3B
�B
B
B
B
�B
B
SB
B
B
SB
�B
B
?B
�B
�B
tB
B
�B
�B
�B
�B
	RB

�B

=B

XB

�B

XB
DB
�B
B
�B
�B
B
B
jB
�B
B
VB
�B
�B
(B
\B
�B
.B
�B
 B
B
NB
�B
�B
B
:B
�B
�B
�B
�B
�B
�B
�B
�B
FB
�B
�B
�B
gB
�B
B
�B
B
B
gB
�B
2B
�B
mB
�B
�B
�B
�B
?B
�B
�B
$B
�B
�B
�B
�B

B
�B

B
�B
�B
�B
QB
�B
�B
qB
�B
�B
�B
�B
]B
IB
�B
�B
B
�B
 B
 vB
 �B
!|B
!�B
!�B
"NB
"4B
#B
"�B
"B
"NB
"�B
# B
# B
# B
#TB
#TB
#TB
#�B
#nB
#:B
# B
$�B
$�B
%FB
&fB
&�B
&LB
%�B
%�B
&B
&LB
&�B
)yB
)_B
)�B
)�B
)�B
)�B
)�B
*B
*�B
*�B
+6B
+B
+QB
+�B
+�B
,qB
,�B
,�B
,�B
-�B
-CB
-wB
,�B
+6B
+QB
,=B
,�B
,�B
,�B
-wB
-�B
-�B
.�B
/B
0B
0oB
0�B
0�B
0�B
1[B
1�B
1�B
2�B
2aB
2�B
2�B
3B
3MB
3B
3B
3�B
3�B
3�B
3�B
4�B
4�B
5B
5?B
5�B
5�B
5�B
5�B
5�B
5tB
5�B
6�B
72B
7LB
6�B
6+B
6+B
6+B
6`B
6FB
6zB
6�B
6�B
72B
72B
7B
7LB
7�B
7�B
8B
9	B
9�B
:B
:xB
:�B
:�B
:�B
:�B
;dB
;�B
;dB
;dB
;dB
;B
;JB
;JB
;JB
;�B
;�B
;�B
<B
<6B
<PB
<PB
<PB
<PB
<�B
<�B
<6B
=<B
=<B
=qB
>]B
>�B
>�B
?�B
?�B
?�B
@B
@B
@iB
@iB
@iB
@�B
@�B
@�B
AUB
A;B
A;B
A�B
A�B
A�B
A�B
A�B
A�B
B'B
B�B
B�B
B�B
CB
CB
CaB
C�B
C�B
C�B
DMB
DgB
D�B
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
FYB
F�B
GB
F�B
F�B
F�B
GB
GzB
G�B
G�B
G�B
H1B
H1B
H�B
H�B
IlB
I7B
I�B
J	B
J=B
J=B
J=B
J	B
J	B
J#B
JXB
J�B
J�B
K)B
K�B
K�B
LJB
L�B
MB
MB
MB
MB
M6B
MPB
M�B
N"B
NB
N"B
N<B
NB
N�B
N�B
O\B
O\B
OvB
O�B
O�B
PHB
P}B
PbB
P}B
P�B
QB
Q�B
Q�B
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
S@B
S�B
S�B
T,B
TB
TaB
TFB
TFB
T�B
T�B
T�B
T�B
TFB
T{B
T�B
T�B
UB
U�B
VB
V9B
VSB
V�B
V�B
V�B
V�B
W?B
W?B
WYB
WsB
WYB
W�B
X_B
X�B
X�B
X�B
YKB
YeB
Y�B
ZkB
[qB
[�B
\xB
\�B
\�B
]�B
^OB
^OB
^�B
^�B
^�B
_;B
_pB
_�B
_�B
`B
`\B
`�B
aB
aB
aHB
a�B
a�B
bB
bhB
b�B
b�B
c B
c�B
c�B
c�B
c�B
c�B
c�B
dB
d@B
dtB
d�B
d�B
d�B
d�B
d�B
e`B
e�B
e�B
e�B
e�B
e�B
e�B
ffB
ffB
ffB
f�B
f�B
f�B
gB
g8B
gRB
gRB
gmB
g�B
g�B
g�B
g�B
h
B
h
B
h$B
hsB
hsB
hsB
h�B
h�B
h�B
h�B
h�B
i*B
i*B
i_B
i�B
i�B
i�B
i�B
jB
jB
i�B
jKB
jB
j�B
j�B
j�B
j�B
k�B
kkB
k�B
k�B
k�B
k�B
k�B
l"B
lWB
lWB
lqB
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
mB
m)B
mwB
mwB
m�B
m�B
m�B
m�B
n/B
n/B
n/B
n/B
n/B
ncB
n�B
n�B
o B
n�B
oiB
oiB
o�B
o�B
o�B
o�B
o�B
o�B
o�B
p!B
p!B
pUB
pUB
p�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
r|B
r�B
r�B
r�B
r�B
sMB
s�B
s�B
s�B
tB
tB
t�B
t�B
t�B
t�B
t�B
u%B
u�B
u�B
u�B
u�B
u�B
u�B
vB
vFB
v`B
v`B
vzB
wB
wB
wLB
w�B
xB
x8B
xB
xB
xRB
xRB
x�B
x�B
y	B
y$B
yXB
yrB
y�B
y�3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<(�U<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<G�<:�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.08(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201910010106432019100101064320191001010643202207271132502022072711325020220727113250202207271535242022072715352420220727153524  JA  ARFMdecpA30a                                                                20190920093725  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20190920093824  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20190920093824  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20190920093825  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20190920093825  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20190920093825  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20190920093825                      G�O�G�O�G�O�                JA  ARUP                                                                        20190920095446                      G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20190921000000  CF  PSAL_ADJUSTED_QC@@G�O�                JM  ARCAJMQC2.0                                                                 20190930160643  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20190930160643  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20220727023250  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2021V2                                                       20220727063524  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20220818081508                      G�O�G�O�G�O�                