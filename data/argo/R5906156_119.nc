CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2022-07-18T10:01:35Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7(   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7h   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8,   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8<   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8D   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8L   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8T   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8X   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8`   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9`   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9d   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9h   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9l   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M    PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p4   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �T   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �,   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ݼ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220718100135  20220718100135  5906156 Argo PMEL                                                       GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               wA   AO  7912                            2B  A   NAVIS_A                         1020                            170425                          863 @����T�1   @�����@@8��-�c&��`A�1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         wA   A   A   @���@���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D��3D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�I�D�i�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�(�@�(�@�\)A�A;�A[�A{�A��
A��
A��
A��
A��
A��
A��
A��
B�B�B�B�B&�B.�B6�B>�BF�BN�BV�B^�Bf�Bn�Bv�B~�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��>C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qD n�D �Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�D	n�D	�D
n�D
�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�D n�D �D!n�D!�D"n�D"�D#n�D#�D$n�D$�D%n�D%�D&n�D&�D'n�D'�D(n�D(�D)n�D)�D*n�D*�D+n�D+�D,n�D,�D-n�D-�D.n�D.�D/n�D/�D0n�D0�D1n�D1�D2n�D2�D3n�D3�D4n�D4�D5n�D5�D6n�D6�D7n�D7�D8n�D8�D9n�D9�D:n�D:�D;n�D;�D<n�D<�D=n�D=�D>n�D>�D?n�D?�D@n�D@�DAn�DA�DBn�DB�DCn�DC�DDn�DD�DEn�DE�DFn�DF�DGn�DG�DHn�DH�DIn�DI�DJn�DJ�DKn�DK�DLn�DL�DMn�DM�DNn�DN�DOn�DO�DPn�DP�DQn�DQ�DRn�DR�DSn�DS�DTn�DT�DUn�DU�DVn�DV�DWn�DW�DXn�DX�DYn�DY�DZn�DZ�D[n�D[�D\n�D\�D]n�D]�D^n�D^�D_n�D_�D`n�D`�Dan�Da�Dbn�Db�Dcn�Dc�Ddn�Dd�Den�De�Dfn�Df�Dgn�Dg�Dhn�Dh�Din�Di�Djn�Dj�Dkn�Dk�Dln�Dl�Dmn�Dm�Dnn�Dn�Don�Do�Dpn�Dp�Dqn�Dq�Drn�Dr�Dsn�Ds�Dtn�Dt�Dun�Du�Dvn�Dv�Dwn�Dw�Dxn�Dx�Dyn�Dy�Dzn�Dz�D{n�D{�D|n�D|�D}n�D}�D~n�D~�Dn�D�D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D·\D��\D�7\D�w\Dú�D��\D�7\D�w\Dķ\D��\D�7\D�w\Dŷ\D��\D�7\D�w\DƷ\D��\D�7\D�w\DǷ\D��\D�7\D�w\Dȷ\D��\D�7\D�w\Dɷ\D��\D�7\D�w\Dʷ\D��\D�7\D�w\D˷\D��\D�7\D�w\D̷\D��\D�7\D�w\Dͷ\D��\D�7\D�w\Dη\D��\D�7\D�w\DϷ\D��\D�7\D�w\Dз\D��\D�7\D�w\Dѷ\D��\D�7\D�w\Dҷ\D��\D�7\D�w\Dӷ\D��\D�7\D�w\DԷ\D��\D�7\D�w\Dշ\D��\D�7\D�w\Dַ\D��\D�7\D�w\D׷\D��\D�7\D�w\Dط\D��\D�7\D�w\Dٷ\D��\D�7\D�w\Dڷ\D��\D�7\D�w\D۷\D��\D�7\D�w\Dܷ\D��\D�7\D�w\Dݷ\D��\D�7\D�w\D޷\D��\D�7\D�w\D߷\D��\D�7\D�w\D�\D��\D�7\D�t)D�\D��\D�7\D�w\D�\D��\D�7\D�w\D�\D��\D�7\D�w\D�\D��\D�7\D�w\D�\D��\D�7\D�w\D�\D��\D�7\D�w\D�\D��\D�7\D�w\D�\D��\D�7\D�w\D�\D��\D�7\D�w\D�\D��\D�7\D�w\D�\D���D�7\D�w\D�\D��\D�7\D�w\D��\D��\D�7\D�w\D�\D��\D�7\D�w\D�\D��\D�7\D�w\D�\D��\D�7\D�w\D�\D��\D�7\D�w\D�\D��\D�7\D�w\D�\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D���D�@�D�`�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�1'A�33A�C�A�A�A�?}A�=qA�A�A�A�A�A�A�C�A�A�A�C�A�A�A�C�A�E�A�G�A�C�A�=qA�;dA�+A�$�A���AȑhA�v�A�&�A��A�{A�1'A�%A��9A�K�A��;A��A�dZA�"�A��
A���A��A��A��A�ffA���A���A� �A��A��A�ZA�\)A�S�A�G�A�=qA�(�A�
=A��wA��mA�K�A�/A���A���A�=qA��A���A�`BA�$�A���A�\)A��\A�&�A���A���A�VA��yA���A�$�A�%A���A��uA�A�A��jA�/A�ȴA��\A�jA�  A��A��A��A�n�A�oA���A��A��A��jA�\)A��A��A�&�A�(�A��A�`BA�S�A��A�x�A��A�\)A�~�A��PA��
A���A�-A�G�A�~�A��A���A�VA�7LA��9A���A��A��;A�M�A��A�ĜA�+A��uA�l�A�9XA�l�A��A}�A{�hAxr�Aw�Avn�Au"�Aq��Aq��Aqt�Ap�Amt�Ah{Ac��A`�/A_��A^^5A[�AY�AU��ASAQ�AO��AO�AN�RAN(�AM�ALr�AK&�AH��AF$�AA��A?�#A?oA=�hA=
=A<VA;C�A:ȴA9+A7��A4�jA3�
A3p�A2JA0��A0A/`BA-A+��A*Q�A(�A&M�A%|�A#��A"�A"1'A!l�A�A/A��A�AK�A��A �AS�AffAt�AffA|�AJA��A"�A��A1'Ap�AAr�A�AdZAJA;dA=qAO�A
�`A
��A
jA	�^A�+AƨA?}A��A��A�A33AVA�Ax�A�!A�A|�A �/A 1@�{@�(�@���@�o@�{@�  @�~�@���@�ƨ@�\@��@�(�@���@���@�r�@�9X@���@�@�X@�/@�j@�ƨ@�S�@�E�@��@�7@�O�@�u@��@�n�@�-@��@�hs@�Q�@އ+@�`B@ܴ9@�1'@ۥ�@�v�@�/@�z�@���@�33@֗�@��@�X@�1'@�M�@�x�@���@�V@Ͳ-@�?}@�%@̴9@���@ˮ@�
=@�$�@�p�@�+@�{@�/@�x�@�@�Ĝ@�\)@��-@��`@��m@�S�@��R@�`B@�z�@���@���@�S�@���@�=q@���@�x�@���@�9X@��@���@��@�%@�r�@��P@�
=@�v�@���@�%@�S�@��@���@��j@�A�@��@�dZ@�S�@�S�@���@�=q@�O�@��u@��@�1'@��
@���@�S�@��@���@���@�~�@��@��T@�@�x�@�O�@��`@�b@�+@��@�v�@�@��@��#@��#@���@��-@��`@�9X@��w@�"�@��@���@��h@�p�@�G�@�7L@�7L@�7L@�%@���@��;@�dZ@�;d@���@��!@���@�M�@��@�J@��@���@��T@���@��@�`B@�7L@���@�%@�%@�Ĝ@�A�@��F@�C�@��y@��R@��!@���@��+@�ff@�M�@�J@���@��h@�G�@�%@��@�bN@�9X@��@�ƨ@�\)@���@���@���@���@��+@�v�@�V@�M�@�E�@�$�@�{@���@��T@���@��7@�p�@�`B@�%@��j@��D@�r�@�Z@�Z@�I�@��m@���@�l�@�o@��H@���@�v�@�=q@���@��#@���@��^@��7@�`B@��@��9@���@�z�@�9X@� �@��;@��w@���@���@�\)@��@��R@���@�v�@���@��7@�O�@�V@�Ĝ@��@���@��D@��@�I�@�1@��
@���@���@�t�@�C�@��@���@���@�E�@��T@���@���@�@�`B@�V@���@��u@�r�@�j@�bN@�z�@�r�@�Q�@�9X@�b@��@��@\)@;d@
=@~�@~��@~$�@~@}�T@}��@}O�@|��@|�j@|��@|Z@{��@{��@{S�@{33@z��@z^5@y��@yX@yG�@y%@x�@xb@w�@w|�@v��@v��@vff@v5?@u�T@u�@up�@uV@t�@tZ@t9X@t(�@s�
@s�@sdZ@s@r��@rM�@rJ@q��@qX@q&�@q%@p��@pr�@p �@o��@oK�@n�y@n��@n�+@n$�@m@m�h@m`B@mO�@l�@l��@lI�@l1@k��@k"�@j�H@j��@j=q@i�@i��@iX@i%@h�@hb@g�;@g�P@g\)@f�y@fv�@f{@f@e�-@eV@d��@d�@d9X@cƨ@c��@cS�@c"�@b��@b�!@b^5@a�^@ax�@a�@`Ĝ@`Q�@_�@_��@_�@^ȴ@^�R@^E�@]�T@]p�@]`B@]?}@]?}@\�@\I�@\9X@\1@[�m@[ƨ@[C�@Z��@Z^5@Z-@Y�7@Y&�@X��@XA�@W�w@W|�@V��@Vȴ@VV@V{@U�@U�h@T�/@T�@Tz�@T9X@S�
@S33@R�H@R��@R=q@Q��@QG�@P�`@P�u@PbN@P1'@P  @O��@O�P@O\)@N��@N��@NV@N{@M�@M��@M��@Mp�@L�@LZ@K��@K��@KC�@J�@J��@J�\@JM�@J�@I�#@I�^@I�7@Ix�@Ihs@IX@I7L@H��@HA�@G�@G�w@G��@G|�@G\)@GK�@G;d@G+@F�y@Fȴ@F�R@Fff@F@Ep�@E/@D��@D�@Dz�@D9X@C��@C�F@Ct�@C"�@C@B��@B=q@A��@A��@Ax�@A&�@@Ĝ@@�9@@��@@r�@@b@?��@?;d@>��@>��@>��@>v�@>{@=��@=�-@=O�@=/@<�@<�@<z�@<I�@<I�@<(�@;�
@;��@;33@:�H@:�\@:n�@:J@9�@9��@9x�@9%@8�`@8�9@8 �@7��@7
=@6�R@6��@6$�@5�T@5��@5O�@4�@4��@4I�@3��@3�
@3��@3t�@3o@2�!@2=q@1��@1��@1��@1X@1G�@17L@0��@0�9@0r�@01'@/�@/��@/l�@/;d@.�@.�R@.V@.{@-�-@-�h@-�@,�@,��@,Z@,(�@,�@+ƨ@+33@+@*��@*n�@*M�@*=q@*�@)�#@)��@)��@)��@)�7@)X@)7L@)�@(�`@(bN@( �@'�;@'�;@'�;@'�;@'�@'�P@'+@&�y@&�y@&�@&�@&ȴ@&�R@&��@&ff@%�T@%�-@%�h@%p�@%?}@%�@$��@$�/@$��@$��@$�j@$�D@$j@$I�@$�@#�m@#ƨ@#ƨ@#�F@#��@#��@#dZ@#C�@#33@#@"��@"�\@"n�@"J@!�#@!��@!��@!G�@!�@!�@!%@ ��@ �u@ Q�@  �@��@�P@l�@\)@K�@+@��@��@ȴ@��@v�@E�@{@@@�h@p�@p�@p�@p�@`B@�@�@�D@9X@(�@(�@(�@�m@�F@��@33@�!@�\@~�@~�@n�@^5@M�@=q@�@�7@X@7L@�@��@Q�@b@�;@�@\)@;d@
=@�@�R@�+@V@E�@5?@$�@$�@$�@{@@��@O�@V@�/@�j@z�@Z@(�@�m@�F@��@dZ@o@��@�!@~�@=q@�@��@��@�7@hs@G�@��@�`@��@r�@ �@  @�@�@\)@K�@+@
=@�@��@ff@�T@@��@�h@�@O�@/@��@�@��@�@I�@��@ƨ@�F@t�@"�@
�!@
M�@
�@
J@	��@	�@	��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�1'A�33A�C�A�A�A�?}A�=qA�A�A�A�A�A�A�C�A�A�A�C�A�A�A�C�A�E�A�G�A�C�A�=qA�;dA�+A�$�A���AȑhA�v�A�&�A��A�{A�1'A�%A��9A�K�A��;A��A�dZA�"�A��
A���A��A��A��A�ffA���A���A� �A��A��A�ZA�\)A�S�A�G�A�=qA�(�A�
=A��wA��mA�K�A�/A���A���A�=qA��A���A�`BA�$�A���A�\)A��\A�&�A���A���A�VA��yA���A�$�A�%A���A��uA�A�A��jA�/A�ȴA��\A�jA�  A��A��A��A�n�A�oA���A��A��A��jA�\)A��A��A�&�A�(�A��A�`BA�S�A��A�x�A��A�\)A�~�A��PA��
A���A�-A�G�A�~�A��A���A�VA�7LA��9A���A��A��;A�M�A��A�ĜA�+A��uA�l�A�9XA�l�A��A}�A{�hAxr�Aw�Avn�Au"�Aq��Aq��Aqt�Ap�Amt�Ah{Ac��A`�/A_��A^^5A[�AY�AU��ASAQ�AO��AO�AN�RAN(�AM�ALr�AK&�AH��AF$�AA��A?�#A?oA=�hA=
=A<VA;C�A:ȴA9+A7��A4�jA3�
A3p�A2JA0��A0A/`BA-A+��A*Q�A(�A&M�A%|�A#��A"�A"1'A!l�A�A/A��A�AK�A��A �AS�AffAt�AffA|�AJA��A"�A��A1'Ap�AAr�A�AdZAJA;dA=qAO�A
�`A
��A
jA	�^A�+AƨA?}A��A��A�A33AVA�Ax�A�!A�A|�A �/A 1@�{@�(�@���@�o@�{@�  @�~�@���@�ƨ@�\@��@�(�@���@���@�r�@�9X@���@�@�X@�/@�j@�ƨ@�S�@�E�@��@�7@�O�@�u@��@�n�@�-@��@�hs@�Q�@އ+@�`B@ܴ9@�1'@ۥ�@�v�@�/@�z�@���@�33@֗�@��@�X@�1'@�M�@�x�@���@�V@Ͳ-@�?}@�%@̴9@���@ˮ@�
=@�$�@�p�@�+@�{@�/@�x�@�@�Ĝ@�\)@��-@��`@��m@�S�@��R@�`B@�z�@���@���@�S�@���@�=q@���@�x�@���@�9X@��@���@��@�%@�r�@��P@�
=@�v�@���@�%@�S�@��@���@��j@�A�@��@�dZ@�S�@�S�@���@�=q@�O�@��u@��@�1'@��
@���@�S�@��@���@���@�~�@��@��T@�@�x�@�O�@��`@�b@�+@��@�v�@�@��@��#@��#@���@��-@��`@�9X@��w@�"�@��@���@��h@�p�@�G�@�7L@�7L@�7L@�%@���@��;@�dZ@�;d@���@��!@���@�M�@��@�J@��@���@��T@���@��@�`B@�7L@���@�%@�%@�Ĝ@�A�@��F@�C�@��y@��R@��!@���@��+@�ff@�M�@�J@���@��h@�G�@�%@��@�bN@�9X@��@�ƨ@�\)@���@���@���@���@��+@�v�@�V@�M�@�E�@�$�@�{@���@��T@���@��7@�p�@�`B@�%@��j@��D@�r�@�Z@�Z@�I�@��m@���@�l�@�o@��H@���@�v�@�=q@���@��#@���@��^@��7@�`B@��@��9@���@�z�@�9X@� �@��;@��w@���@���@�\)@��@��R@���@�v�@���@��7@�O�@�V@�Ĝ@��@���@��D@��@�I�@�1@��
@���@���@�t�@�C�@��@���@���@�E�@��T@���@���@�@�`B@�V@���@��u@�r�@�j@�bN@�z�@�r�@�Q�@�9X@�b@��@��@\)@;d@
=@~�@~��@~$�@~@}�T@}��@}O�@|��@|�j@|��@|Z@{��@{��@{S�@{33@z��@z^5@y��@yX@yG�@y%@x�@xb@w�@w|�@v��@v��@vff@v5?@u�T@u�@up�@uV@t�@tZ@t9X@t(�@s�
@s�@sdZ@s@r��@rM�@rJ@q��@qX@q&�@q%@p��@pr�@p �@o��@oK�@n�y@n��@n�+@n$�@m@m�h@m`B@mO�@l�@l��@lI�@l1@k��@k"�@j�H@j��@j=q@i�@i��@iX@i%@h�@hb@g�;@g�P@g\)@f�y@fv�@f{@f@e�-@eV@d��@d�@d9X@cƨ@c��@cS�@c"�@b��@b�!@b^5@a�^@ax�@a�@`Ĝ@`Q�@_�@_��@_�@^ȴ@^�R@^E�@]�T@]p�@]`B@]?}@]?}@\�@\I�@\9X@\1@[�m@[ƨ@[C�@Z��@Z^5@Z-@Y�7@Y&�@X��@XA�@W�w@W|�@V��@Vȴ@VV@V{@U�@U�h@T�/@T�@Tz�@T9X@S�
@S33@R�H@R��@R=q@Q��@QG�@P�`@P�u@PbN@P1'@P  @O��@O�P@O\)@N��@N��@NV@N{@M�@M��@M��@Mp�@L�@LZ@K��@K��@KC�@J�@J��@J�\@JM�@J�@I�#@I�^@I�7@Ix�@Ihs@IX@I7L@H��@HA�@G�@G�w@G��@G|�@G\)@GK�@G;d@G+@F�y@Fȴ@F�R@Fff@F@Ep�@E/@D��@D�@Dz�@D9X@C��@C�F@Ct�@C"�@C@B��@B=q@A��@A��@Ax�@A&�@@Ĝ@@�9@@��@@r�@@b@?��@?;d@>��@>��@>��@>v�@>{@=��@=�-@=O�@=/@<�@<�@<z�@<I�@<I�@<(�@;�
@;��@;33@:�H@:�\@:n�@:J@9�@9��@9x�@9%@8�`@8�9@8 �@7��@7
=@6�R@6��@6$�@5�T@5��@5O�@4�@4��@4I�@3��@3�
@3��@3t�@3o@2�!@2=q@1��@1��@1��@1X@1G�@17L@0��@0�9@0r�@01'@/�@/��@/l�@/;d@.�@.�R@.V@.{@-�-@-�h@-�@,�@,��@,Z@,(�@,�@+ƨ@+33@+@*��@*n�@*M�@*=q@*�@)�#@)��@)��@)��@)�7@)X@)7L@)�@(�`@(bN@( �@'�;@'�;@'�;@'�;@'�@'�P@'+@&�y@&�y@&�@&�@&ȴ@&�R@&��@&ff@%�T@%�-@%�h@%p�@%?}@%�@$��@$�/@$��@$��@$�j@$�D@$j@$I�@$�@#�m@#ƨ@#ƨ@#�F@#��@#��@#dZ@#C�@#33@#@"��@"�\@"n�@"J@!�#@!��@!��@!G�@!�@!�@!%@ ��@ �u@ Q�@  �@��@�P@l�@\)@K�@+@��@��@ȴ@��@v�@E�@{@@@�h@p�@p�@p�@p�@`B@�@�@�D@9X@(�@(�@(�@�m@�F@��@33@�!@�\@~�@~�@n�@^5@M�@=q@�@�7@X@7L@�@��@Q�@b@�;@�@\)@;d@
=@�@�R@�+@V@E�@5?@$�@$�@$�@{@@��@O�@V@�/@�j@z�@Z@(�@�m@�F@��@dZ@o@��@�!@~�@=q@�@��@��@�7@hs@G�@��@�`@��@r�@ �@  @�@�@\)@K�@+@
=@�@��@ff@�T@@��@�h@�@O�@/@��@�@��@�@I�@��@ƨ@�F@t�@"�@
�!@
M�@
�@
J@	��@	�@	��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BdZBdZBdZBdZBdZBdZBdZBdZBdZBdZBdZBcTBcTBcTBcTBcTBdZBcTBcTBbNB`BB[#BW
BL�BVB��B��B��B  B  BBB1B	7BDBPB\B{BuBoB{B�B�B�B�B�B�B�B�B�B�B�B�B�B �B�B�B�B�B�B�B�B�B�B�B�B�BhBhBVBJB
=B+BBBBBB��B��B��B�B�B�mB�BȴB�^B�B��B��B�1Bw�Be`B]/BM�B=qB6FB"�B�BoB�BhB��B�ZB��BÖB�?B��B��B�VBz�Bn�B^5BO�B<jB&�B�B\B
��B
�5B
��B
ƨB
�B
��B
�7B
u�B
gmB
K�B
8RB
#�B
�B
B	��B	�B	�B	��B	��B	��B	ĜB	�B	�bB	u�B	^5B	VB	M�B	@�B	.B	�B		7B��B��B�B�B�B�`B�;B�B��B��B�B��B��B��B�uB�uB�PB�DB�%B�By�Br�Bp�Bm�BiyBffBe`B_;B\)BVBS�BM�BL�BK�BI�BH�BG�BI�BG�BG�BE�BF�BE�BE�BF�BE�BF�BE�BE�BF�BF�BG�BF�BI�BH�BG�BH�BF�BF�BF�BE�BD�BD�BC�BB�BB�BB�BC�BA�BF�BE�BE�BD�BE�BD�BD�BG�BK�BJ�BI�BL�BN�BR�BVBVBW
BXBZB[#B[#B\)B\)B^5B^5BaHBaHBbNBbNBbNBcTBcTBcTBcTBdZBcTBffBgmBffBe`BgmBhsBiyBiyBiyBiyBjBl�Bo�Bp�Bq�Br�Bt�Bv�Bx�Bz�B{�B{�B|�B~�B�B�B�B�B�B�B�B�B�B�B� B}�Bz�Bx�Bx�Bv�Bt�Be`BhsBffBdZB`BB`BBaHBcTBffBl�Bn�Bn�Bn�Bn�Bq�Bq�Br�Bt�Bu�Bw�Bz�B~�B�B�B�+B�DB�PB�\B�uB��B��B��B��B��B�B�B�3B�?B�XB�wB�}B��B��B��BŢB��B��B��B�
B�B�#B�;B�TB�fB�yB�B�B�B��B	B	+B		7B	JB	PB	VB	VB	VB	\B	{B	�B	�B	�B	%�B	&�B	+B	/B	1'B	2-B	2-B	5?B	9XB	:^B	<jB	=qB	?}B	@�B	D�B	L�B	P�B	R�B	S�B	S�B	T�B	W
B	\)B	]/B	^5B	`BB	aHB	aHB	bNB	e`B	gmB	ffB	e`B	e`B	hsB	jB	n�B	s�B	v�B	z�B	|�B	� B	�B	�%B	�7B	�DB	�PB	�\B	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�9B	�?B	�FB	�LB	�LB	�LB	�LB	�XB	�jB	�qB	�}B	��B	B	ÖB	ĜB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�B	�)B	�)B	�/B	�/B	�;B	�HB	�TB	�`B	�fB	�fB	�mB	�mB	�mB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
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
	7B

=B

=B

=B
DB
JB
JB
JB
PB
VB
VB
VB
\B
\B
\B
bB
bB
hB
hB
hB
oB
oB
uB
uB
uB
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
 �B
 �B
 �B
 �B
 �B
!�B
!�B
"�B
"�B
#�B
$�B
$�B
$�B
%�B
&�B
&�B
&�B
&�B
&�B
&�B
'�B
(�B
)�B
)�B
+B
+B
,B
,B
,B
-B
-B
-B
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
1'B
1'B
2-B
2-B
33B
33B
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
7LB
8RB
8RB
8RB
8RB
9XB
9XB
:^B
:^B
:^B
;dB
;dB
<jB
<jB
<jB
<jB
<jB
=qB
=qB
>wB
=qB
>wB
>wB
>wB
>wB
>wB
?}B
?}B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
C�B
B�B
C�B
D�B
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
F�B
F�B
F�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
N�B
M�B
N�B
N�B
N�B
O�B
O�B
O�B
P�B
O�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
VB
VB
VB
W
B
W
B
XB
W
B
XB
XB
XB
XB
YB
YB
ZB
ZB
[#B
[#B
[#B
[#B
[#B
[#B
[#B
\)B
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
^5B
_;B
_;B
_;B
_;B
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
bNB
bNB
bNB
cTB
cTB
cTB
dZB
cTB
cTB
dZB
dZB
dZB
e`B
dZB
e`B
dZB
e`B
dZB
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
m�B
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
o�B
o�B
o�B
p�B
p�B
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
r�B
r�B
r�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
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
v�B
v�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
y�B
y�B
z�B
z�B
z�B
z�B
{�B
{�B
{�B
{�B
{�B
{�B
|�B
|�B
|�B
|�B
|�B
}�B
}�B
}�B
}�B
}�B
}�B
~�B
~�B
~�B
~�B
~�B
� B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BdZBdZBdZBdZBdZBdZBdZBdZBdZBdZBdZBcTBcTBcTBcTBcTBdZBcTBcTBbNB`BB[#BW
BL�BVB��B��B��B  B  BBB1B	7BDBPB\B{BuBoB{B�B�B�B�B�B�B�B�B�B�B�B�B�B �B�B�B�B�B�B�B�B�B�B�B�B�BhBhBVBJB
=B+BBBBBB��B��B��B�B�B�mB�BȴB�^B�B��B��B�1Bw�Be`B]/BM�B=qB6FB"�B�BoB�BhB��B�ZB��BÖB�?B��B��B�VBz�Bn�B^5BO�B<jB&�B�B\B
��B
�5B
��B
ƨB
�B
��B
�7B
u�B
gmB
K�B
8RB
#�B
�B
B	��B	�B	�B	��B	��B	��B	ĜB	�B	�bB	u�B	^5B	VB	M�B	@�B	.B	�B		7B��B��B�B�B�B�`B�;B�B��B��B�B��B��B��B�uB�uB�PB�DB�%B�By�Br�Bp�Bm�BiyBffBe`B_;B\)BVBS�BM�BL�BK�BI�BH�BG�BI�BG�BG�BE�BF�BE�BE�BF�BE�BF�BE�BE�BF�BF�BG�BF�BI�BH�BG�BH�BF�BF�BF�BE�BD�BD�BC�BB�BB�BB�BC�BA�BF�BE�BE�BD�BE�BD�BD�BG�BK�BJ�BI�BL�BN�BR�BVBVBW
BXBZB[#B[#B\)B\)B^5B^5BaHBaHBbNBbNBbNBcTBcTBcTBcTBdZBcTBffBgmBffBe`BgmBhsBiyBiyBiyBiyBjBl�Bo�Bp�Bq�Br�Bt�Bv�Bx�Bz�B{�B{�B|�B~�B�B�B�B�B�B�B�B�B�B�B� B}�Bz�Bx�Bx�Bv�Bt�Be`BhsBffBdZB`BB`BBaHBcTBffBl�Bn�Bn�Bn�Bn�Bq�Bq�Br�Bt�Bu�Bw�Bz�B~�B�B�B�+B�DB�PB�\B�uB��B��B��B��B��B�B�B�3B�?B�XB�wB�}B��B��B��BŢB��B��B��B�
B�B�#B�;B�TB�fB�yB�B�B�B��B	B	+B		7B	JB	PB	VB	VB	VB	\B	{B	�B	�B	�B	%�B	&�B	+B	/B	1'B	2-B	2-B	5?B	9XB	:^B	<jB	=qB	?}B	@�B	D�B	L�B	P�B	R�B	S�B	S�B	T�B	W
B	\)B	]/B	^5B	`BB	aHB	aHB	bNB	e`B	gmB	ffB	e`B	e`B	hsB	jB	n�B	s�B	v�B	z�B	|�B	� B	�B	�%B	�7B	�DB	�PB	�\B	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�9B	�?B	�FB	�LB	�LB	�LB	�LB	�XB	�jB	�qB	�}B	��B	B	ÖB	ĜB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�B	�)B	�)B	�/B	�/B	�;B	�HB	�TB	�`B	�fB	�fB	�mB	�mB	�mB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
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
	7B

=B

=B

=B
DB
JB
JB
JB
PB
VB
VB
VB
\B
\B
\B
bB
bB
hB
hB
hB
oB
oB
uB
uB
uB
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
 �B
 �B
 �B
 �B
 �B
!�B
!�B
"�B
"�B
#�B
$�B
$�B
$�B
%�B
&�B
&�B
&�B
&�B
&�B
&�B
'�B
(�B
)�B
)�B
+B
+B
,B
,B
,B
-B
-B
-B
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
1'B
1'B
2-B
2-B
33B
33B
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
7LB
8RB
8RB
8RB
8RB
9XB
9XB
:^B
:^B
:^B
;dB
;dB
<jB
<jB
<jB
<jB
<jB
=qB
=qB
>wB
=qB
>wB
>wB
>wB
>wB
>wB
?}B
?}B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
C�B
B�B
C�B
D�B
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
F�B
F�B
F�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
N�B
M�B
N�B
N�B
N�B
O�B
O�B
O�B
P�B
O�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
VB
VB
VB
W
B
W
B
XB
W
B
XB
XB
XB
XB
YB
YB
ZB
ZB
[#B
[#B
[#B
[#B
[#B
[#B
[#B
\)B
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
^5B
_;B
_;B
_;B
_;B
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
bNB
bNB
bNB
cTB
cTB
cTB
dZB
cTB
cTB
dZB
dZB
dZB
e`B
dZB
e`B
dZB
e`B
dZB
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
m�B
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
o�B
o�B
o�B
p�B
p�B
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
r�B
r�B
r�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
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
v�B
v�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
y�B
y�B
z�B
z�B
z�B
z�B
{�B
{�B
{�B
{�B
{�B
{�B
|�B
|�B
|�B
|�B
|�B
}�B
}�B
}�B
}�B
}�B
}�B
~�B
~�B
~�B
~�B
~�B
� B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.27 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20220718100135                              AO  ARCAADJP                                                                    20220718100135    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20220718100135  QCP$                G�O�G�O�G�O�5F83E           AO  ARGQQCPL                                                                    20220718100135  QCF$                G�O�G�O�G�O�0               