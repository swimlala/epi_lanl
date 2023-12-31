CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2016-10-20T21:35:16Z creation;2016-10-20T21:35:21Z conversion to V3.1;2019-12-19T08:27:25Z update;     
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
_FillValue                 �  I@   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p`   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tL   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �l   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �D   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �x   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �x   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �x   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �x   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �    HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �H   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �X   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �\   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �l   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �p   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �t   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �xArgo profile    3.1 1.2 19500101000000  20161020213516  20200115111516  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               1A   JA  I2_0576_049                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @���0*z 1   @����O� @:דݗ�+�d�?|�h1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU�fDV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� DgfDg�fDh  Dh� DifDi� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�C3D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�<�D�|�D�� D�  D�@ Dր D�� D�  D�@ D׃3D�� D�3D�@ D؀ D�� D�  D�@ Dـ D�� D���D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�C3D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�FfD�p 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @{�@�(�@�\)A�A;�A[�A{�A��
A��
A��
A��
A��
A��
A��
A��
B�B�B�B�B&�B.�B6�B>�BF�BN�BV�B^�Bf�Bn�Bv�B~�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�Bè�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qD n�D �Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�D	n�D	�D
n�D
�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�D n�D �D!n�D!�D"n�D"�D#n�D#�D$n�D$�D%n�D%�D&n�D&�D'n�D'�D(n�D(�D)n�D)�D*n�D*�D+n�D+�D,n�D,�D-n�D-�D.n�D.�D/n�D/�D0n�D0�D1n�D1�D2n�D2�D3n�D3�D4n�D4�D5n�D5�D6n�D6�D7n�D7�D8n�D8�D9n�D9�D:n�D:�D;n�D;�D<n�D<�D=n�D=�D>n�D>�D?n�D?�D@n�D@�DAn�DA�DBn�DB�DCn�DC�DDn�DD�DEn�DE�DFn�DF�DGn�DG�DHn�DH�DIn�DI�DJn�DJ�DKn�DK�DLn�DL�DMn�DM�DNn�DN�DOn�DO�DPn�DP�DQn�DQ�DRn�DR�DSn�DS�DTn�DT�DUuDU�DVn�DV�DWn�DW�DXn�DX�DYn�DY�DZn�DZ�D[n�D[�D\n�D\�D]n�D]�D^n�D^�D_n�D_�D`n�D`�Dan�Da�Dbn�Db�Dcn�Dc�Ddn�Dd�Den�De�Dfn�Df�DguDg�Dhn�Dh�Din�Di�Djn�Dj�Dkn�Dk�Dln�Dl�Dmn�Dm�Dnn�Dn�Don�Do�Dpn�Dp�Dqn�Dq�Drn�Dr�Dsn�Ds�Dtn�Dt�Dun�Du�Dvn�Dv�Dwn�Dw�Dxn�Dx�Dyn�Dy�Dzn�Dz�D{n�D{�D|n�D|�D}n�D}�D~n�D~�Dn�D�D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�4)D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D���D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D���D�:�D�z�D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D·\D��\D�7\D�w\D÷\D��\D�7\D�w\Dķ\D��\D�7\D�w\Dŷ\D��\D�7\D�w\DƷ\D��\D�7\D�w\DǷ\D��\D�7\D�w\Dȷ\D��\D�7\D�w\Dɷ\D��\D�7\D�w\Dʷ\D��\D�7\D�w\D˷\D��\D�7\D�w\D̷\D��\D�7\D�w\Dͷ\D��\D�7\D�w\Dη\D��\D�7\D�w\DϷ\D��\D�7\D�w\Dз\D��\D�7\D�w\Dѷ\D��\D�7\D�w\Dҷ\D��\D�7\D�w\Dӷ\D��\D�7\D�w\DԷ\D��\D�4)D�t)Dշ\D��\D�7\D�w\Dַ\D��\D�7\D�z�D׷\D���D�7\D�w\Dط\D��\D�7\D�w\Dٷ\D��)D�7\D�w\Dڷ\D��\D�7\D�w\D۷\D��\D�7\D�w\Dܷ\D��\D�7\D�w\Dݷ\D��\D�7\D�w\D޷\D��\D�7\D�w\D߷\D��\D�7\D�w\D�\D��\D�7\D�w\D�\D��\D�7\D�w\D�\D��\D�7\D�w\D�\D��\D�7\D�w\D�\D��\D�7\D�w\D�\D��\D�7\D�w\D�\D��\D�7\D�w\D�\D��\D�7\D�w\D�\D��\D�7\D�w\D�\D��\D�7\D�w\D�\D��\D�7\D�w\D�\D��\D�7\D�w\D�\D��\D�7\D�w\D��\D��\D�7\D�w\D�\D��\D�7\D�w\D�\D��\D�7\D�w\D�\D��\D�:�D�w\D�\D��\D�7\D�w\D�\D��\D�7\D�w\D�\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D���D��\D�=�D�g\11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�I�A�K�A�M�A�M�A�M�A�Q�A�M�A�Q�A�S�A�M�A�O�A�S�A�VA�VA�XA�VA�XA�VA�XA�ZA�ZA�;dA�=qA�=qA��`A�bNA���AǺ^AƬA� �A�A��FA���A��hA��/A�
=A��A��A�=qA��A�(�A��A�ƨA��A�~�A���A�5?A��wA��A��A��A�bNA���A���A��wA�  A��9A�E�A���A� �A��yA���A�v�A�v�A��jA��!A��A��mA��A��;A�1A�bNA��;A�JA���A�"�A�bA���A~�A}��A}C�A|�A|�\AzQ�Ax �AvĜAu�hAs�
ArjAq`BAp�RAodZAnbAlbAk�7AkdZAj�Ai\)Ag��Ae��Ad�DAc�wAb^5A`�DA_�A_7LA^ �A]
=A[�TA[S�A[�AZ��AZffAY�wAX�\AU�AR�yAQ��APĜAO��AM�mAM�hAM�AMK�AL5?AJZAIXAH�RAH^5AH �AGp�AD�yACAC��AC�AC?}AA�A@ȴA@VA@1A?�hA>ZA=;dA<A9|�A8��A7�^A6^5A5��A5�#A5�A5K�A4��A4v�A4bA3��A37LA3
=A2��A2�jA2  A/��A.�9A.$�A-�TA-�-A-��A-��A-hsA,��A*�A)��A)
=A(��A(=qA'��A'7LA&�/A&�!A&�+A&M�A&bA%�#A%��A%�A%?}A$�uA$ZA#��A"�+A!�A 9XA33AM�A��AdZA�`A{AG�A��A�^A�Ap�AA�A�/A�jA^5A�hAz�A�A?}A��A��A��A�A��A  AK�A
��A
�+A	�FAȴAr�A�AC�AffA/A\)AA��Ap�AC�A�A ��@���@���@��@�%@�ȴ@�M�@��@�p�@�Ĝ@�+@�`B@�33@���@��`@�t�@��@�@�hs@�7L@�Ĝ@��@�
=@��@�Q�@�ȴ@�j@�5?@�x�@߾w@�O�@ܼj@��H@��T@�z�@�ƨ@��@�x�@���@��/@ԛ�@� �@��@��@Ӆ@�E�@ЋD@Ώ\@�I�@�n�@��
@�+@��H@�v�@�^5@��#@Ų-@�&�@ċD@�I�@�9X@î@�S�@�33@�@°!@�=q@��7@��@��@���@��D@�l�@�
=@���@�1'@��@���@��@�$�@�hs@���@�A�@���@��;@�V@��@�K�@���@��\@�V@�@���@���@���@�1'@��@�X@� �@���@�C�@�@�ȴ@�ff@�J@��^@�;d@��D@�33@�ȴ@�~�@���@��^@���@���@��@���@�\)@��y@���@��@�%@�I�@�b@��@��@���@�=q@��7@�&�@��/@�Ĝ@���@�bN@���@��w@�|�@�@��@���@��9@���@�z�@�j@�A�@��
@�|�@�+@�v�@�@���@��7@��@�x�@�p�@�O�@�7L@�&�@���@��@�Z@��
@�dZ@�;d@���@��H@���@�{@��@��T@�@���@���@��7@�O�@��`@��D@�j@�I�@��@��;@��F@��@�\)@�
=@���@�M�@�5?@��@��^@��h@�hs@�hs@�O�@��@��@��/@���@�Q�@�@��@�@l�@;d@~�y@~�@~ȴ@~5?@~{@}��@}�h@}`B@}O�@}�@|��@|�j@|�D@|1@{ƨ@{�F@{t�@{o@z~�@z=q@y��@yX@x�`@x��@x�u@xbN@xb@w+@v�R@vE�@u�@u�h@u?}@t��@t�@t�@t�/@t��@t�j@t�@tz�@s"�@q��@qhs@q�@p�`@pĜ@p��@q%@q�@q%@p�u@p1'@o��@o;d@o
=@n�y@n��@n5?@m�T@m`B@lz�@l�@k��@k�m@k��@l1@l�@l(�@l9X@l�D@l�j@l�/@m?}@m�-@m?}@l�@l�j@l�D@lI�@lI�@lz�@l��@l�D@k��@l�@k�
@k33@k@j��@j�\@j�@i��@iG�@h�`@hĜ@hr�@hA�@h �@g�@g��@gl�@g+@f��@f��@f�+@f�+@fE�@e@d��@dI�@d1@c��@c�
@c��@c��@ct�@cC�@co@b�!@b�@b�@a�@`A�@_K�@_�@^��@^�y@^v�@^5?@^5?@^{@]�T@]��@]?}@\�@Z��@Z��@Z�@ZM�@YG�@X�`@XĜ@X�u@Xr�@XA�@V��@VE�@V{@U�h@T��@Tz�@S��@S�F@S��@S�
@S�m@S�
@TZ@T�@T��@T��@T�D@TZ@TI�@S�m@S��@S��@S�@SdZ@SC�@RM�@Qhs@Qx�@Qhs@Q�@P��@PA�@Ol�@Nff@N{@N{@N{@M`B@L��@L��@LI�@L9X@L(�@K�m@Kƨ@K��@Ko@JM�@I�#@I�7@IG�@H�`@H�u@HQ�@H �@Hb@H  @H  @G�P@Fȴ@Fv�@FE�@F5?@F5?@E�@E��@Ep�@E?}@E�@D�/@D�D@Dz�@DZ@DI�@D(�@D�@C��@Cƨ@C�F@C��@B�@B~�@B~�@B=q@BJ@A�@Ax�@A&�@@�`@@Ĝ@@r�@?�;@?�@?|�@?\)@?+@>�y@>��@>5?@=�@=�@=V@=�@=�@<�@<�j@<��@<�D@<z�@<j@<Z@<I�@<9X@<�@;��@;�m@;�
@;ƨ@;��@;33@:��@:n�@:^5@:^5@:n�@:^5@:-@:J@9��@97L@8��@8�u@8�@8r�@8r�@8r�@8bN@8A�@8 �@8  @7��@7�w@7�w@7�w@7|�@6�y@6�R@6�+@6{@5�-@5O�@5�@4�@4�/@4��@4(�@3�m@3�F@3t�@3S�@3S�@3S�@333@3@2�@2�H@2��@2n�@2n�@2n�@2M�@2J@1�7@1x�@1%@01'@/�@/\)@/;d@/+@.��@.ȴ@.��@.��@.ff@.E�@.@-�T@-�T@-�-@-`B@,��@,��@,Z@,(�@+�m@+t�@+@*��@*^5@)��@)�#@)�#@)x�@)7L@)�@(�`@(��@(Ĝ@(�9@(�@(bN@(1'@'��@&��@&��@&5?@%�@%��@%��@%?}@%�@$�/@$�D@$I�@#�@#33@"�H@"�!@"M�@!x�@ �`@ ��@ bN@ Q�@ A�@ 1'@   @��@�w@\)@;d@��@ȴ@v�@5?@@�T@��@�@?}@�@��@��@��@z�@Z@I�@Z@I�@I�@I�@I�@9X@I�@9X@9X@��@33@�H@�\@-@��@x�@hs@X@7L@�@�`@Ĝ@��@�@�@ �@  @�@�;@��@�@��@��@\)@�@ȴ@ȴ@�R@v�@ff@V@E�@$�@@��@p�@`B@/@�@1@ƨ@t�@S�@S�@C�@"�@o@o@o@@@@�@�H@��@��@M�@��@�7@hs@X@G�@7L@%@�`@bN@ �@b@�@�;@�;@�w@�@l�@\)@\)@K�@K�@K�@
=@V@�@�-@��@�h@p�@V@�/@�j@j@(�@�m@��@t�@t�@dZ@C�@@
�!@
�\@
~�@
M�@
M�@
M�@
M�@
-@
�@
J@
J@	�@	X@	%@�`@�9@r�@��@�w@�w@�w@�@�@��@��@��@��@��@�P@|�@|�@\)@K�@
=@ff@E�@5?@5?@{@�T@@�-@�-@��@�h@p�@/@/@/@�@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�I�A�K�A�M�A�M�A�M�A�Q�A�M�A�Q�A�S�A�M�A�O�A�S�A�VA�VA�XA�VA�XA�VA�XA�ZA�ZA�;dA�=qA�=qA��`A�bNA���AǺ^AƬA� �A�A��FA���A��hA��/A�
=A��A��A�=qA��A�(�A��A�ƨA��A�~�A���A�5?A��wA��A��A��A�bNA���A���A��wA�  A��9A�E�A���A� �A��yA���A�v�A�v�A��jA��!A��A��mA��A��;A�1A�bNA��;A�JA���A�"�A�bA���A~�A}��A}C�A|�A|�\AzQ�Ax �AvĜAu�hAs�
ArjAq`BAp�RAodZAnbAlbAk�7AkdZAj�Ai\)Ag��Ae��Ad�DAc�wAb^5A`�DA_�A_7LA^ �A]
=A[�TA[S�A[�AZ��AZffAY�wAX�\AU�AR�yAQ��APĜAO��AM�mAM�hAM�AMK�AL5?AJZAIXAH�RAH^5AH �AGp�AD�yACAC��AC�AC?}AA�A@ȴA@VA@1A?�hA>ZA=;dA<A9|�A8��A7�^A6^5A5��A5�#A5�A5K�A4��A4v�A4bA3��A37LA3
=A2��A2�jA2  A/��A.�9A.$�A-�TA-�-A-��A-��A-hsA,��A*�A)��A)
=A(��A(=qA'��A'7LA&�/A&�!A&�+A&M�A&bA%�#A%��A%�A%?}A$�uA$ZA#��A"�+A!�A 9XA33AM�A��AdZA�`A{AG�A��A�^A�Ap�AA�A�/A�jA^5A�hAz�A�A?}A��A��A��A�A��A  AK�A
��A
�+A	�FAȴAr�A�AC�AffA/A\)AA��Ap�AC�A�A ��@���@���@��@�%@�ȴ@�M�@��@�p�@�Ĝ@�+@�`B@�33@���@��`@�t�@��@�@�hs@�7L@�Ĝ@��@�
=@��@�Q�@�ȴ@�j@�5?@�x�@߾w@�O�@ܼj@��H@��T@�z�@�ƨ@��@�x�@���@��/@ԛ�@� �@��@��@Ӆ@�E�@ЋD@Ώ\@�I�@�n�@��
@�+@��H@�v�@�^5@��#@Ų-@�&�@ċD@�I�@�9X@î@�S�@�33@�@°!@�=q@��7@��@��@���@��D@�l�@�
=@���@�1'@��@���@��@�$�@�hs@���@�A�@���@��;@�V@��@�K�@���@��\@�V@�@���@���@���@�1'@��@�X@� �@���@�C�@�@�ȴ@�ff@�J@��^@�;d@��D@�33@�ȴ@�~�@���@��^@���@���@��@���@�\)@��y@���@��@�%@�I�@�b@��@��@���@�=q@��7@�&�@��/@�Ĝ@���@�bN@���@��w@�|�@�@��@���@��9@���@�z�@�j@�A�@��
@�|�@�+@�v�@�@���@��7@��@�x�@�p�@�O�@�7L@�&�@���@��@�Z@��
@�dZ@�;d@���@��H@���@�{@��@��T@�@���@���@��7@�O�@��`@��D@�j@�I�@��@��;@��F@��@�\)@�
=@���@�M�@�5?@��@��^@��h@�hs@�hs@�O�@��@��@��/@���@�Q�@�@��@�@l�@;d@~�y@~�@~ȴ@~5?@~{@}��@}�h@}`B@}O�@}�@|��@|�j@|�D@|1@{ƨ@{�F@{t�@{o@z~�@z=q@y��@yX@x�`@x��@x�u@xbN@xb@w+@v�R@vE�@u�@u�h@u?}@t��@t�@t�@t�/@t��@t�j@t�@tz�@s"�@q��@qhs@q�@p�`@pĜ@p��@q%@q�@q%@p�u@p1'@o��@o;d@o
=@n�y@n��@n5?@m�T@m`B@lz�@l�@k��@k�m@k��@l1@l�@l(�@l9X@l�D@l�j@l�/@m?}@m�-@m?}@l�@l�j@l�D@lI�@lI�@lz�@l��@l�D@k��@l�@k�
@k33@k@j��@j�\@j�@i��@iG�@h�`@hĜ@hr�@hA�@h �@g�@g��@gl�@g+@f��@f��@f�+@f�+@fE�@e@d��@dI�@d1@c��@c�
@c��@c��@ct�@cC�@co@b�!@b�@b�@a�@`A�@_K�@_�@^��@^�y@^v�@^5?@^5?@^{@]�T@]��@]?}@\�@Z��@Z��@Z�@ZM�@YG�@X�`@XĜ@X�u@Xr�@XA�@V��@VE�@V{@U�h@T��@Tz�@S��@S�F@S��@S�
@S�m@S�
@TZ@T�@T��@T��@T�D@TZ@TI�@S�m@S��@S��@S�@SdZ@SC�@RM�@Qhs@Qx�@Qhs@Q�@P��@PA�@Ol�@Nff@N{@N{@N{@M`B@L��@L��@LI�@L9X@L(�@K�m@Kƨ@K��@Ko@JM�@I�#@I�7@IG�@H�`@H�u@HQ�@H �@Hb@H  @H  @G�P@Fȴ@Fv�@FE�@F5?@F5?@E�@E��@Ep�@E?}@E�@D�/@D�D@Dz�@DZ@DI�@D(�@D�@C��@Cƨ@C�F@C��@B�@B~�@B~�@B=q@BJ@A�@Ax�@A&�@@�`@@Ĝ@@r�@?�;@?�@?|�@?\)@?+@>�y@>��@>5?@=�@=�@=V@=�@=�@<�@<�j@<��@<�D@<z�@<j@<Z@<I�@<9X@<�@;��@;�m@;�
@;ƨ@;��@;33@:��@:n�@:^5@:^5@:n�@:^5@:-@:J@9��@97L@8��@8�u@8�@8r�@8r�@8r�@8bN@8A�@8 �@8  @7��@7�w@7�w@7�w@7|�@6�y@6�R@6�+@6{@5�-@5O�@5�@4�@4�/@4��@4(�@3�m@3�F@3t�@3S�@3S�@3S�@333@3@2�@2�H@2��@2n�@2n�@2n�@2M�@2J@1�7@1x�@1%@01'@/�@/\)@/;d@/+@.��@.ȴ@.��@.��@.ff@.E�@.@-�T@-�T@-�-@-`B@,��@,��@,Z@,(�@+�m@+t�@+@*��@*^5@)��@)�#@)�#@)x�@)7L@)�@(�`@(��@(Ĝ@(�9@(�@(bN@(1'@'��@&��@&��@&5?@%�@%��@%��@%?}@%�@$�/@$�D@$I�@#�@#33@"�H@"�!@"M�@!x�@ �`@ ��@ bN@ Q�@ A�@ 1'@   @��@�w@\)@;d@��@ȴ@v�@5?@@�T@��@�@?}@�@��@��@��@z�@Z@I�@Z@I�@I�@I�@I�@9X@I�@9X@9X@��@33@�H@�\@-@��@x�@hs@X@7L@�@�`@Ĝ@��@�@�@ �@  @�@�;@��@�@��@��@\)@�@ȴ@ȴ@�R@v�@ff@V@E�@$�@@��@p�@`B@/@�@1@ƨ@t�@S�@S�@C�@"�@o@o@o@@@@�@�H@��@��@M�@��@�7@hs@X@G�@7L@%@�`@bN@ �@b@�@�;@�;@�w@�@l�@\)@\)@K�@K�@K�@
=@V@�@�-@��@�h@p�@V@�/@�j@j@(�@�m@��@t�@t�@dZ@C�@@
�!@
�\@
~�@
M�@
M�@
M�@
M�@
-@
�@
J@
J@	�@	X@	%@�`@�9@r�@��@�w@�w@�w@�@�@��@��@��@��@��@�P@|�@|�@\)@K�@
=@ff@E�@5?@5?@{@�T@@�-@�-@��@�h@p�@/@/@/@�@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�JBt�BXBF�BA�B:^B.B%�BPB�B�BÖB�FB�B��B��B�Bo�BbNBJ�B)�B	7B�B��B��B��B�DB{�By�B}�B{�Bv�Bo�BbNBC�B=qB$�B�B�B�BoB
=B  B
��B
��B
��B
�B
�HB
�B
��B
ŢB
�^B
�FB
�B
�B
��B
��B
�\B
�JB
�1B
�B
x�B
cTB
]/B
R�B
E�B
9XB
0!B
)�B
 �B
�B
B	��B	��B	��B	�mB	�B	ɺB	�}B	�XB	�'B	��B	��B	��B	�{B	�VB	�%B	�B	� B	~�B	|�B	w�B	o�B	_;B	J�B	D�B	<jB	8RB	2-B	33B	8RB	:^B	7LB	-B	%�B	 �B	�B	�B	�B	PB	B	B	B	B��B��B��B�B�B�B�fB�TB�#B�
B�B��B��B��B��B��B��B��B��B��B��B��B��BɺBɺB��B�jB�RB�LB�RB�LB�LB�LB�FB�'B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�hB�JB�=B�7B�1B�%B�B� Bx�Bw�Br�Bl�BjBiyBgmBffBdZB`BB^5B]/BXBXBQ�BO�BN�BM�BK�BI�BH�BH�BD�BC�BA�B@�B=qB:^B:^B49B2-B2-B1'B1'B0!B1'B/B,B-B+B)�B)�B(�B'�B&�B$�B#�B �B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B{B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B�B �B �B �B!�B"�B"�B#�B$�B$�B$�B%�B&�B'�B+B-B0!B7LB9XB9XB<jB<jB<jB<jB=qB>wB@�B@�BA�BC�BH�BF�BK�BO�BP�BO�BP�BP�BP�BP�BO�BW
B`BBbNBgmBiyBiyBk�Bk�Bm�Bn�Bn�Bu�Bv�Bw�Bw�By�B|�B~�B�B�+B�DB�VB�hB�uB��B��B��B��B��B��B��B��B�B�B�'B�3B�3B�9B�?B�RB�XB�^B�qBĜB��B��B��B��B��B��B��B�B�B�/B�HB�ZB�`B�fB�fB�mB�yB�yB�yB�B�B�B�B��B��B��B��B��B	  B	B	B	B	B	B	B	B	1B	DB	JB	PB	\B	hB	uB	{B	�B	�B	�B	�B	�B	#�B	$�B	$�B	%�B	&�B	&�B	'�B	)�B	,B	/B	49B	8RB	9XB	9XB	:^B	;dB	=qB	>wB	?}B	B�B	C�B	E�B	H�B	I�B	I�B	J�B	K�B	L�B	L�B	O�B	P�B	P�B	Q�B	S�B	VB	VB	W
B	ZB	[#B	\)B	\)B	]/B	^5B	bNB	cTB	e`B	ffB	gmB	iyB	jB	jB	jB	jB	k�B	k�B	l�B	m�B	r�B	v�B	x�B	{�B	~�B	� B	�B	�B	�B	�B	�%B	�+B	�7B	�7B	�=B	�=B	�=B	�JB	�\B	�bB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�!B	�'B	�3B	�?B	�FB	�FB	�LB	�RB	�XB	�dB	�jB	�qB	�wB	�}B	�}B	��B	��B	��B	B	B	B	ĜB	ǮB	ȴB	ɺB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�)B	�/B	�/B	�)B	�/B	�/B	�/B	�/B	�/B	�/B	�5B	�5B	�5B	�;B	�BB	�HB	�NB	�NB	�TB	�ZB	�fB	�fB	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
+B
+B
1B

=B
JB
JB
PB
JB
\B
\B
\B
bB
bB
bB
hB
hB
hB
oB
uB
{B
{B
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
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
"�B
"�B
#�B
#�B
#�B
#�B
$�B
$�B
%�B
&�B
&�B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
(�B
(�B
(�B
)�B
)�B
+B
+B
+B
+B
+B
+B
+B
+B
,B
-B
-B
-B
.B
.B
.B
.B
.B
.B
.B
/B
/B
/B
/B
/B
0!B
0!B
0!B
1'B
1'B
2-B
2-B
2-B
33B
33B
2-B
49B
33B
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
5?B
5?B
5?B
6FB
6FB
6FB
7LB
8RB
9XB
9XB
9XB
9XB
:^B
:^B
:^B
:^B
:^B
:^B
;dB
;dB
;dB
;dB
;dB
<jB
<jB
=qB
=qB
=qB
>wB
>wB
?}B
?}B
?}B
?}B
@�B
@�B
@�B
A�B
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
D�B
D�B
E�B
E�B
F�B
F�B
F�B
F�B
G�B
H�B
H�B
H�B
H�B
I�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
O�B
P�B
P�B
P�B
O�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
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
T�B
T�B
T�B
VB
VB
VB
VB
VB
VB
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
XB
XB
YB
YB
ZB
YB
YB
YB
[#B
[#B
\)B
\)B
\)B
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
]/B
]/B
^5B
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
aHB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
dZB
e`B
e`B
e`B
e`B
e`B
ffB
ffB
ffB
ffB
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
jB
jB
k�B
k�B
k�B
k�B
k�B
k�B
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
m�B
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
o�B
o�B
o�B
o�B
p�B
p�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B��B�7B�&B|PB[=BH�BD3B=<B1�B/B�B��B�B�+B��B��B�RB�BB�BshBgRBP}B0UB�B�`BөB��B��B�hB�iB|PB.B}�By�Bt�Bh�BGEB@�B&2BxBeB�B�B�B �B
��B
��BB
�+B
�nB
ٴB
�\B
�zB
��B
�fB
��B
��B
�QB
�	B
�.B
�B
�lB
��B
{JB
e,B
^�B
T�B
G_B
:�B
1[B
+�B
"�B
�B
�B	��B	�0B	��B	��B	�]B	�DB	��B	�JB	�MB	��B	��B	�#B	��B	��B	��B	��B	��B	�B	~BB	zB	r�B	b�B	LdB	F%B	>(B	:*B	2�B	3�B	9$B	<B	9rB	.cB	&�B	!|B	�B	)B	KB	�B	�B	�B	B	B��B��B��B��B�hB�OB�B�BܒB�yB׍BԕB�oB�oBѝBѝBбBϫBΊB�pB�JB�0B˒B�xB�dB��B�<B��B��B��B��B�B��B��B��B��B��B��B��B��B��B�:B�:B�TB�TB�NB�HB�-B�|B��B��B�B��B��B��B��B��B�PB�B�XB�lB�_B�GB�oBzDBzBtnBmwBkkBi�BhXBg�Be�BabB_�B_!BY�BYeBSBP�BO�BN�BL�BJ�BJ	BI�BEmBD�BB�BBB?}B<�B<B4�B2�B2�B1�B1�B1�B2B/�B-)B.cB+�B*�B*�B)�B)DB(XB&fB$�B!�B!�B vB�B!BBjB�B�B�B�B�B7BByB�B�B2B�B�B�BKB�BBB�BBB�BBeB�B=BCBjB5BVB \B 'B BB!B BB!HB!|B!bB"NB#:B#nB$@B%FB%`B%`B&�B'�B(�B+�B.B1�B88B9�B:�B=VB<�B=B="B>(B?HBA;BAoBCBE�BJ	BG�BMBPbBQNBPbBQNBQ4BQhBQ�BQNBX�B`�Bc:Bg�Bi�Bi�BlBlBn/BoiBp�Bw�Bw�BxlBx8BzxB}VB�B��B��B��B��B��B�B�9B�WB�\B�4B�:B�fB��B��B��B��B�vB��B��B��B��B��B��B��B�]B�SB�6B�.B�HB�4B�:B҉B�aB֡BخBݲB�B�B�B�B�B�B�B��B��B�B��B�B�3B�+B�B�*B�^B��B	 OB	UB	UB	AB	[B	GB	{B	�B	�B	xB	�B	�B	�B	�B	�B	�B	�B	B	�B	�B	B	$&B	%,B	%,B	&2B	'B	'8B	(XB	*0B	,WB	/�B	4�B	8�B	9�B	9�B	:�B	;�B	=�B	>�B	?�B	B�B	C�B	E�B	H�B	J	B	I�B	J�B	K�B	MB	M6B	PB	QB	QNB	RTB	TFB	VSB	VmB	W�B	ZkB	[WB	\xB	\xB	]~B	^�B	b�B	c�B	e�B	f�B	g�B	i�B	j�B	j�B	j�B	j�B	k�B	k�B	l�B	n/B	s3B	w2B	y	B	|B	.B	�B	�;B	�GB	�MB	��B	�tB	�zB	�lB	�lB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�FB	�B	�$B	�$B	�*B	�=B	�/B	�UB	�UB	�vB	�MB	�tB	��B	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�	B	�#B	�=B	�#B	�B	�B	�B	�:B	�&B	�,B	�,B	�,B	�FB	�mB	�KB	�yB	��B	ܬB	�~B	�~B	�]B	�~B	�dB	�dB	�dB	�dB	�~B	ޞB	޸B	��B	�pB	�vB	��B	��B	�B	�B	�B	�B	��B	�
B	�B	��B	��B	��B	� B	��B	��B	�B	��B	��B	��B	�B	��B	�"B	�"B	�<B	�(B	�(B	�(B	�BB	�(B	�BB	�(B	�BB	�cB
�B
-B
gB
�B
�B
�B
�B

�B
~B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
�B
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
B
B
�B
�B
�B
�B
�B
!-B
!B
!�B
"B
"4B
# B
# B
$B
$B
$B
$B
%,B
%,B
&2B
'8B
'B
'B
'B
'B
(>B
($B
($B
(
B
(
B
($B
(
B
(>B
)DB
)*B
)B
)*B
)*B
)*B
*KB
*eB
+QB
+B
+6B
+B
+6B
+QB
+QB
+QB
,WB
-]B
-]B
-]B
.IB
./B
.IB
.IB
.cB
.IB
.IB
/OB
/5B
/OB
/OB
/OB
0oB
0oB
0oB
1vB
1vB
2aB
2|B
2|B
3hB
3�B
2|B
4nB
3hB
4�B
4nB
4nB
4TB
4nB
4nB
4nB
5ZB
5tB
5tB
5tB
5ZB
5tB
5tB
6�B
6zB
6�B
7�B
8�B
9�B
9�B
9�B
9�B
:�B
:�B
:�B
:�B
:�B
:�B
;�B
;�B
;�B
;�B
;�B
<�B
<�B
=�B
=�B
=�B
>�B
>�B
?�B
?�B
?�B
?�B
@�B
@�B
@�B
A�B
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
D�B
D�B
E�B
E�B
F�B
F�B
F�B
F�B
H1B
H�B
H�B
IB
IB
J=B
K)B
K�B
LB
K�B
K�B
K�B
LB
MB
MB
MB
MB
MB
NB
NB
NB
OB
O(B
OB
O(B
OB
PHB
QB
Q B
Q B
PB
Q B
Q B
QB
Q B
Q B
Q B
Q B
QB
Q B
QB
QB
QNB
QNB
R B
R B
S[B
S[B
T,B
TB
T,B
T,B
T,B
TFB
U2B
UMB
U2B
U2B
U2B
V9B
VB
VB
VB
V9B
VB
W?B
VSB
WYB
XEB
X+B
XEB
X_B
X+B
X+B
X+B
XEB
XEB
YKB
YKB
Z7B
YKB
YeB
YB
[WB
[WB
\CB
\]B
\]B
\]B
\CB
\CB
]dB
]dB
]IB
]IB
]dB
]IB
]dB
]~B
]~B
]dB
^jB
_VB
_VB
_VB
_�B
_pB
`vB
`�B
a�B
abB
a|B
abB
abB
a�B
bhB
b�B
bhB
bhB
b�B
bhB
b�B
b�B
b�B
d�B
e�B
ezB
e�B
e�B
e�B
f�B
f�B
f�B
f�B
g�B
g�B
g�B
h�B
h�B
h�B
h�B
h�B
h�B
h�B
h�B
i�B
i�B
i�B
i�B
i�B
i�B
i�B
i�B
i�B
i�B
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
m�B
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
o�B
o�B
o�B
o�B
p�B
p�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.27(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201610250033542016102500335420161025003354201806221215402018062212154020180622121540201804050408312018040504083120180405040831  JA  ARFMdecpA19c                                                                20161021063505  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20161020213516  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20161020213517  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20161020213517  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20161020213520  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20161020213520  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20161020213520  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20161020213520  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20161020213521  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20161020213521                      G�O�G�O�G�O�                JA  ARUP                                                                        20161020223207                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20161021153722  CV  JULD            G�O�G�O�F���                JM  ARCAJMQC2.0                                                                 20161024153354  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20161024153354  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404190831  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622031540  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115111516                      G�O�G�O�G�O�                