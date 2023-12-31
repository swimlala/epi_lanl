CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:26:21Z creation;2022-06-04T19:26:21Z conversion to V3.1      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604192621  20220610161505  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               YA   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @م"2@y]1   @م"�z@*�&�x���ddZ�1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @33@�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�ffB�  B�ffB�33B�  B���B�  B�  B�  B�  B�  B�33B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�33C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C�C   C"  C#�fC&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ�CL33CM�fCP  CR  CT  CU�fCW�fCZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG�fDH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� DyfDy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�3D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�@u�@�\)@�\)A�A;�A[�A{�A��
A��
A��
A��
A��
A��
A��
A��
B�B�B�B�B&�B.�B6�B>�BF�BN�BV�B^�Bf�Bn�Bv�B~�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B��)B�u�B��)B���B�u�B�]B�u�B�u�B�u�B�u�B�u�BШ�B�B�B�B�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B���B�u�C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C�{C��C!��C#�GC%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI�{CK�CM�GCO��CQ��CS��CU�GCW�GCY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��>C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC�ФC�ФC��qC��qC��qC��qC��>C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qD n�D �Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�D	n�D	�D
n�D
�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�DhRD�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�D n�D �D!n�D!�D"n�D"�D#n�D#�D$n�D$�D%n�D%�D&n�D&�D'n�D'�D(n�D(�D)n�D)�D*n�D*�D+n�D+�D,n�D,�D-n�D-�D.n�D.�D/n�D/�D0n�D0�D1n�D1�D2n�D2�D3n�D3�D4n�D4�D5n�D5�D6n�D6�D7n�D7�D8n�D8�D9n�D9�D:n�D:�D;n�D;�D<n�D<�D=n�D=�D>n�D>�D?n�D?�D@n�D@�DAn�DA�DBn�DB�DCn�DC�DDn�DD�DEn�DE�DFn�DF�DGuDG�DHn�DH�DIn�DI�DJn�DJ�DKn�DK�DLn�DL�DMn�DM�DNn�DN�DOn�DO�DPn�DP�DQn�DQ�DRn�DR�DSn�DS�DTn�DT�DUn�DU�DVn�DV�DWn�DW�DXn�DX�DYn�DY�DZn�DZ�D[n�D[�D\n�D\�D]n�D]�D^n�D^�D_n�D_�D`n�D`�Dan�Da�Dbn�Db�Dcn�Dc�Ddn�Dd�Den�De�Dfn�Df�Dgn�Dg�Dhn�Dh�Din�Di�Djn�Dj�Dkn�Dk�Dln�Dl�Dmn�Dm�Dnn�Dn�Don�Do�Dpn�Dp�Dqn�Dq�Drn�Dr�Dsn�Ds�Dtn�Dt�Dun�Du�Dvn�Dv�Dwn�Dw�Dxn�Dx�Dyn�Dy�Dzn�Dz�D{n�D{�D|n�D|�D}n�D}�D~n�D~�Dn�D�D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�:�D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D·\D��\D�7\D�w\D÷\D��\D�7\D�w\Dķ\D��\D�7\D�w\Dŷ\D��\D�7\D�w\DƷ\D��\D�7\D�w\DǷ\D��\D�7\D�w\Dȷ\D��\D�7\D�w\Dɷ\D��\D�7\D�w\Dʷ\D��\D�7\D�w\D˷\D��\D�7\D�w\D̷\D��\D�7\D�w\Dͷ\D��\D�7\D�w\Dη\D��\D�7\D�w\DϷ\D��\D�7\D�w\Dз\D��\D�7\D�w\Dѷ\D��\D�7\D�w\Dҷ\D��\D�7\D�w\Dӷ\D��\D�7\D�w\DԷ\D��\D�7\D�w\Dշ\D��\D�7\D�w\Dַ\D��\D�7\D�w\D׷\D��\D�7\D�w\Dط\D��\D�7\D�w\Dٷ\D���D�7\D�w\Dڷ\D��\D�7\D�w\D۷\D��\D�7\D�w\Dܷ\D��\D�7\D�w\Dݷ\D��\D�7\D�w\D޷\D��\D�7\D�w\D߷\D��\D�7\D�w\D�\D��\D�7\D�w\D�\D��\D�7\D�w\D�\D��\D�7\D�w\D�\D��\D�7\D�w\D�\D��\D�7\D�w\D�\D��\D�7\D�w\D�\D��\D�7\D�w\D�\D��\D�7\D�w\D�\D��\D�7\D�w\D�\D��\D�7\D�w\D�\D��\D�7\D�w\D�\D��\D�7\D�w\D�\D��\D�7\D�w\D��\D��\D�7\D�w\D�\D��\D�7\D�w\D�\D��\D�7\D�w\D�\D��\D�7\D�w\D�\D��\D�7\D�w\D�\D��\D�7\D�w\D�\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�fA��A�A��A�JA��A�
	A�A�VA�A�MA��A��A�YA��A�+A�7A��A�CA��A�!A��A� �A�A�uA�A�qA��A�~A�kA�1A�!-A�#�A�#�A�$A��A��2A���Aҡ�A�gmA���A��A�+A�5tA�.�A�SA�\A��xA���A�ZA��A���A�:A�O�A���A���A�qAA��RA��A���A�NpA���A��A��MA�A A�t�A�.A��_A�<6A�W�A�-CA��A��A���A���A�4�A�Q�A�iyA�\�A�g8A��BA�CAy�dAv��Atk�Aq��Am��AiT�Ag��AdOAa�A]��AX$�ATxlAR�AQOvAPm]AOE�AK�KAJ{AH�.AGc�AE��AC�bA@jA=�jA<
�A5cA2A1�A1+kA0�,A0n�A/�HA/A.��A-�A-9XA,OA+��A+YKA*�A*#:A)��A)�A(>�A(J#A(RTA(E�A'�A'�FA'4nA'ffA'ƨA'X�A&��A&��A&��A&bA%��A%j�A%%FA$��A$Z�A#�DA#�A#.IA"�A"VmA!��A �)A ��A xA��A�A�;A�Al"A5?A@AѷAe�A��A�VAh
AW?A{JA�jA�6A�XA��AS�A2�A�A��A��A��A�aA��AVA)_Aw�A"�A��ArGA��AH�A�A��Ah
A�A/A��A�fAsAK^A��A�A�MA��AR�A��A6A�ATaAA�FARTAe�A iA�XAQ�A��Ag�A \A
͟A
�A	��A	�6A	r�A	�Ab�A�A��A�hAl�A҉AaA�A�|A�7AH�A�Ag8A��A2aA|�A��Au�A�A �	A e@�ݘ@���@�@���@�y�@��"@�w�@�%@�|�@��[@��S@�{@�G�@�dZ@�I�@�+�@���@���@��a@얼@��3@��@��U@�g�@��K@랄@�`B@��@��@�=@���@�6@�^@�c�@�xl@傪@�Mj@���@�9X@�a@�%F@��P@���@�@�R�@��d@��@�]�@��	@�ѷ@޸R@�S�@�o @ܰ�@�C�@���@���@���@ڇ+@�;�@��a@�u%@װ�@�=�@��@�M@ԑ�@�1�@��K@ҭ�@�#:@��@��@��a@ѱ[@� i@Ї�@��T@��@��o@ͺ^@�zx@�f�@��@��@�q�@�(�@��@ˠ�@�Vm@��@��z@�{J@�W?@� i@ȵ@�tT@�@�x�@ƨ�@��@��N@�%F@��@Į}@�^5@�D�@�	@ü@�zx@�9�@���@�҉@1@��@���@�Q�@��m@��L@�H@��@�w2@�H�@��F@��@���@�@��!@�n�@�7�@��D@��@��`@���@��@�&@��j@�kQ@���@��h@�}V@�M�@�D�@�(�@���@�m]@�q@���@�A�@�O@��A@��6@���@�J�@��@��@�i�@�	�@���@���@���@�\�@��@��@�H@��;@���@���@�{�@�Ft@��@�!�@�֡@���@���@�_�@�{@��@���@�dZ@��@��@�U2@��.@��@���@�a@�ی@�tT@�6@���@��@�X�@�#�@��@��@�l"@�E�@���@�f�@��@�Y@��8@��@���@�u%@�;�@��#@�a@�q@��y@���@�4n@�G@���@��@�Mj@��@�ߤ@�L0@��D@���@�Q�@�S@��|@���@�c @��S@�@@���@�kQ@�C-@�ݘ@���@�6z@�5?@�G@��@���@�U�@��O@�M@��@�n/@��@��@���@�s�@��@���@�4�@��'@�|�@�b@��@�j�@��j@�p;@��@���@���@�@O@��|@��L@��+@�{@���@���@�L�@���@���@���@���@��d@�L�@��@�ȴ@�U2@�3�@��@�Q�@�֡@�u%@��@��W@���@�rG@�X@�E9@�>�@�33@�Y@��@��}@���@�YK@�2�@�{@��Q@���@�@@���@�H�@��@��z@�f�@�&@��s@��@���@���@���@�z�@�D�@���@��t@�?}@���@��u@�g8@�,=@��@��@��+@��)@��@��a@�iD@�+@��@��p@�]d@�0@~ȴ@~YK@~�@}!�@|�@|�@{l�@z�]@zL0@z1�@y�@y��@yIR@xS�@w��@w
=@v��@v^5@u�Z@t��@s�@@r�c@r8�@q�d@q��@q?}@p��@p�@poi@p1'@o��@o33@n��@n�\@n&�@m�@m2a@l�@k��@k��@k�	@ka@k1�@j��@j�@i�#@iT�@h�@h7�@h"h@h  @g�@@gF�@f��@fOv@e�d@d�5@d�@d�[@d��@d7�@c�&@cy�@c�@b��@b�M@b��@b\�@b&�@b�@a��@ac�@aIR@a%@`�@`S�@`H@_��@_��@_33@^�M@^�<@^��@]��@]�@\|�@[�@[�P@[U�@[=@[
=@Zȴ@Z�r@Z#:@Yc@X�P@X�@X�@X<�@W�a@W�@V�y@V�}@V��@Vc @VQ@V+k@Vu@U��@U+�@T�P@T��@Tc�@Tx@Sݘ@S��@Sl�@SK�@S"�@R�@R��@Rz@Q��@Q�^@Q[W@P��@Pj@O�+@Oqv@N��@N�A@N#:@M��@M��@M��@Mzx@M*0@L��@Ll"@L?�@L�@K��@K�W@K��@KC@J�,@J��@I��@I�7@IIR@I#�@H�@H%�@HG@G��@Gn/@G@F��@F@�@F{@E�M@EDg@D�@D�e@DtT@C��@C��@CP�@C�@B��@BC�@B
�@A��@A��@Aa�@@��@@�@@�@?�+@?� @?>�@>�y@>�1@>V@=�t@=�@=zx@=�@<y>@<�@;�}@;��@;�@:�M@:��@:n�@9�T@9�X@9*0@8��@8Ɇ@8��@8�I@8l"@8  @7��@7]�@7@6�@6Q@6;�@5�Z@5�^@5�X@5�M@57L@4�5@4��@4�@4?�@3�+@3��@3n/@31�@2�@2��@2YK@2!�@1�@1�9@1��@1��@1�=@1��@1x�@1F@1%F@0Ĝ@0��@0I�@/�6@/]�@/P�@/W?@/=@/�@.�@.�@.��@.E�@-�o@-@-Dg@,�P@,�O@,�@,c�@,"h@+�A@+�@@+,�@+
=@*�c@*�m@*��@*=q@*&�@)��@)�C@)p�@)G�@)2a@)�@(��@(h�@(S�@(�@'��@&��@%��@%��@%O�@%%@$��@$c�@$?�@$�@#��@#�@#��@#|�@#b�@#W?@#.I@"�@"��@"l�@!�@!�~@!s�@!+@ �|@ �f@ ��@ �P@ ��@ ~(@ z�@ ~(@ �@ m�@ %�@ݘ@o�@4�@��@��@�@�L@�1@V@!�@�@�z@c�@�@�@�f@��@�@z�@PH@x@�&@��@~�@S�@33@�@ں@��@{�@=q@+k@u@��@�@�@o @O�@@	l@�@%@��@�E@��@��@_@U2@C-@�@�m@ƨ@�*@��@j�@$t@�@�1@��@{�@v�@p;@!�@��@w2@G�@<6@+@��@�|@�@�v@��@D�@,=@�+@�6@��@�@��@�B@�!@��@C�@J@�9@��@Y�@0�@�@ѷ@��@��@q@<�@@��@��@�@qv@4�@�@�@�]@�h@��@3�@�@��@�C@�7@m]@5�@	l@�P@�/@�9@�@A�@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�fA��A�A��A�JA��A�
	A�A�VA�A�MA��A��A�YA��A�+A�7A��A�CA��A�!A��A� �A�A�uA�A�qA��A�~A�kA�1A�!-A�#�A�#�A�$A��A��2A���Aҡ�A�gmA���A��A�+A�5tA�.�A�SA�\A��xA���A�ZA��A���A�:A�O�A���A���A�qAA��RA��A���A�NpA���A��A��MA�A A�t�A�.A��_A�<6A�W�A�-CA��A��A���A���A�4�A�Q�A�iyA�\�A�g8A��BA�CAy�dAv��Atk�Aq��Am��AiT�Ag��AdOAa�A]��AX$�ATxlAR�AQOvAPm]AOE�AK�KAJ{AH�.AGc�AE��AC�bA@jA=�jA<
�A5cA2A1�A1+kA0�,A0n�A/�HA/A.��A-�A-9XA,OA+��A+YKA*�A*#:A)��A)�A(>�A(J#A(RTA(E�A'�A'�FA'4nA'ffA'ƨA'X�A&��A&��A&��A&bA%��A%j�A%%FA$��A$Z�A#�DA#�A#.IA"�A"VmA!��A �)A ��A xA��A�A�;A�Al"A5?A@AѷAe�A��A�VAh
AW?A{JA�jA�6A�XA��AS�A2�A�A��A��A��A�aA��AVA)_Aw�A"�A��ArGA��AH�A�A��Ah
A�A/A��A�fAsAK^A��A�A�MA��AR�A��A6A�ATaAA�FARTAe�A iA�XAQ�A��Ag�A \A
͟A
�A	��A	�6A	r�A	�Ab�A�A��A�hAl�A҉AaA�A�|A�7AH�A�Ag8A��A2aA|�A��Au�A�A �	A e@�ݘ@���@�@���@�y�@��"@�w�@�%@�|�@��[@��S@�{@�G�@�dZ@�I�@�+�@���@���@��a@얼@��3@��@��U@�g�@��K@랄@�`B@��@��@�=@���@�6@�^@�c�@�xl@傪@�Mj@���@�9X@�a@�%F@��P@���@�@�R�@��d@��@�]�@��	@�ѷ@޸R@�S�@�o @ܰ�@�C�@���@���@���@ڇ+@�;�@��a@�u%@װ�@�=�@��@�M@ԑ�@�1�@��K@ҭ�@�#:@��@��@��a@ѱ[@� i@Ї�@��T@��@��o@ͺ^@�zx@�f�@��@��@�q�@�(�@��@ˠ�@�Vm@��@��z@�{J@�W?@� i@ȵ@�tT@�@�x�@ƨ�@��@��N@�%F@��@Į}@�^5@�D�@�	@ü@�zx@�9�@���@�҉@1@��@���@�Q�@��m@��L@�H@��@�w2@�H�@��F@��@���@�@��!@�n�@�7�@��D@��@��`@���@��@�&@��j@�kQ@���@��h@�}V@�M�@�D�@�(�@���@�m]@�q@���@�A�@�O@��A@��6@���@�J�@��@��@�i�@�	�@���@���@���@�\�@��@��@�H@��;@���@���@�{�@�Ft@��@�!�@�֡@���@���@�_�@�{@��@���@�dZ@��@��@�U2@��.@��@���@�a@�ی@�tT@�6@���@��@�X�@�#�@��@��@�l"@�E�@���@�f�@��@�Y@��8@��@���@�u%@�;�@��#@�a@�q@��y@���@�4n@�G@���@��@�Mj@��@�ߤ@�L0@��D@���@�Q�@�S@��|@���@�c @��S@�@@���@�kQ@�C-@�ݘ@���@�6z@�5?@�G@��@���@�U�@��O@�M@��@�n/@��@��@���@�s�@��@���@�4�@��'@�|�@�b@��@�j�@��j@�p;@��@���@���@�@O@��|@��L@��+@�{@���@���@�L�@���@���@���@���@��d@�L�@��@�ȴ@�U2@�3�@��@�Q�@�֡@�u%@��@��W@���@�rG@�X@�E9@�>�@�33@�Y@��@��}@���@�YK@�2�@�{@��Q@���@�@@���@�H�@��@��z@�f�@�&@��s@��@���@���@���@�z�@�D�@���@��t@�?}@���@��u@�g8@�,=@��@��@��+@��)@��@��a@�iD@�+@��@��p@�]d@�0@~ȴ@~YK@~�@}!�@|�@|�@{l�@z�]@zL0@z1�@y�@y��@yIR@xS�@w��@w
=@v��@v^5@u�Z@t��@s�@@r�c@r8�@q�d@q��@q?}@p��@p�@poi@p1'@o��@o33@n��@n�\@n&�@m�@m2a@l�@k��@k��@k�	@ka@k1�@j��@j�@i�#@iT�@h�@h7�@h"h@h  @g�@@gF�@f��@fOv@e�d@d�5@d�@d�[@d��@d7�@c�&@cy�@c�@b��@b�M@b��@b\�@b&�@b�@a��@ac�@aIR@a%@`�@`S�@`H@_��@_��@_33@^�M@^�<@^��@]��@]�@\|�@[�@[�P@[U�@[=@[
=@Zȴ@Z�r@Z#:@Yc@X�P@X�@X�@X<�@W�a@W�@V�y@V�}@V��@Vc @VQ@V+k@Vu@U��@U+�@T�P@T��@Tc�@Tx@Sݘ@S��@Sl�@SK�@S"�@R�@R��@Rz@Q��@Q�^@Q[W@P��@Pj@O�+@Oqv@N��@N�A@N#:@M��@M��@M��@Mzx@M*0@L��@Ll"@L?�@L�@K��@K�W@K��@KC@J�,@J��@I��@I�7@IIR@I#�@H�@H%�@HG@G��@Gn/@G@F��@F@�@F{@E�M@EDg@D�@D�e@DtT@C��@C��@CP�@C�@B��@BC�@B
�@A��@A��@Aa�@@��@@�@@�@?�+@?� @?>�@>�y@>�1@>V@=�t@=�@=zx@=�@<y>@<�@;�}@;��@;�@:�M@:��@:n�@9�T@9�X@9*0@8��@8Ɇ@8��@8�I@8l"@8  @7��@7]�@7@6�@6Q@6;�@5�Z@5�^@5�X@5�M@57L@4�5@4��@4�@4?�@3�+@3��@3n/@31�@2�@2��@2YK@2!�@1�@1�9@1��@1��@1�=@1��@1x�@1F@1%F@0Ĝ@0��@0I�@/�6@/]�@/P�@/W?@/=@/�@.�@.�@.��@.E�@-�o@-@-Dg@,�P@,�O@,�@,c�@,"h@+�A@+�@@+,�@+
=@*�c@*�m@*��@*=q@*&�@)��@)�C@)p�@)G�@)2a@)�@(��@(h�@(S�@(�@'��@&��@%��@%��@%O�@%%@$��@$c�@$?�@$�@#��@#�@#��@#|�@#b�@#W?@#.I@"�@"��@"l�@!�@!�~@!s�@!+@ �|@ �f@ ��@ �P@ ��@ ~(@ z�@ ~(@ �@ m�@ %�@ݘ@o�@4�@��@��@�@�L@�1@V@!�@�@�z@c�@�@�@�f@��@�@z�@PH@x@�&@��@~�@S�@33@�@ں@��@{�@=q@+k@u@��@�@�@o @O�@@	l@�@%@��@�E@��@��@_@U2@C-@�@�m@ƨ@�*@��@j�@$t@�@�1@��@{�@v�@p;@!�@��@w2@G�@<6@+@��@�|@�@�v@��@D�@,=@�+@�6@��@�@��@�B@�!@��@C�@J@�9@��@Y�@0�@�@ѷ@��@��@q@<�@@��@��@�@qv@4�@�@�@�]@�h@��@3�@�@��@�C@�7@m]@5�@	l@�P@�/@�9@�@A�@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
 B
4B
B
B
4B
�B
.B
}B
�B
�B
hB
NB
4B
NB
4B
4B
4B
B
4B
hB
hB
4B
4B
}B
�B
B
bB
�B
�B
.B
HB
}B
�B
�B
�B
HB
�B
B

�B
KB
gB
�B
�B	��B
B
=�B
;�B
oB
��B
�/B"�BPbB^OBH1B{�B��B�B��B��B��B��B��Bw�BbB^5B]B\xB^�B^�BqBlB=�B�BuB
� B
�UB
��B
��B
^�B
C{B
/�B
�B	��B	�4B	�,B	ƨB	��B	�hB	��B	�DB	{�B	qvB	[#B	LJB	BB	?�B	<B	5?B	-�B	(�B	$�B	#�B	'8B	BB	RB	e�B	bB	G_B	8lB	G�B	S&B	X+B	Z�B	`vB	iyB	l�B	z*B	�B	�~B	�#B	��B	��B	�*B	��B	��B	�'B	�jB	�9B	یB	�dB	�B	��B	�B
�B
�B
$�B
&�B
(�B
+6B
)�B
)�B
*B
-]B
-�B
6�B
6+B
5ZB
6+B
4nB
1�B
6B
:�B
6�B
*eB
)�B
,�B
,WB
,=B
+6B
*0B
*�B
/�B
-�B
,WB
+�B
./B
2-B
<�B
>�B
=<B
8�B
5�B
6B
6FB
7�B
8�B
=VB
D�B
D�B
B�B
?}B
<�B
=<B
<B
<�B
@�B
CaB
C�B
C�B
BuB
@4B
=�B
=<B
>B
=�B
=VB
;JB
9>B
:xB
;�B
<�B
:�B
8�B
7LB
8�B
7�B
5�B
5B
2-B
1�B
0!B
/ B
1�B
0�B
1�B
1�B
0�B
.�B
-�B
,�B
+6B
*KB
)yB
(�B
(>B
'�B
$�B
$tB
#�B
%�B
$�B
#:B
#�B
"�B
�B
�B
�B
sB
�B
�B
mB
�B
aB
FB
[B
oB
hB
.B
B
dB
	7B
�B
�B
 OB	��B	�B	�iB	�B	�B	�|B	�B	�vB	�AB	��B	��B
 �B
�B
YB
�B
�B
�B
�B
	B
	B
�B
�B
�B
SB
�B
�B
SB
mB
9B
9B
B
�B
B
B
B
UB
 �B
 B
 4B
;B
�B
GB
-B
�B
�B
�B
GB
�B
�B
�B
[B
�B
UB
;B	��B	��B	��B	�DB	�DB	��B	��B	��B	��B	�dB	��B	��B	�<B	�"B	��B	��B	��B	��B	��B
  B
[B
�B
�B
uB
uB
uB
�B
�B
�B
�B
�B
 �B	�}B
 �B
 �B
 �B
B
 B
�B
�B
oB
oB
�B
oB
oB
�B
;B
UB
B
�B
B
�B
AB
AB
'B
�B
�B
;B
�B
 B
;B
 B
B
 �B
 �B
 �B
 �B
 4B
 iB
 �B
UB
oB
�B
'B
[B
�B
{B
B
MB
B
9B
�B
B
B
�B
�B
�B
�B
�B
�B
_B
�B
�B
�B
B
	B
	lB
	B
	B
	RB
	�B

�B

�B

�B
B
�B
�B
�B
�B
�B
�B
�B
B
B
dB
�B
�B
PB
�B
�B
�B
�B
\B
�B
.B
B
�B
�B
�B
 B
�B
�B
4B
�B
�B
�B
�B
�B
�B
�B
�B
:B
B
 B
B
TB
�B
�B
&B
uB
�B
[B
[B
uB
�B
�B
�B
B
2B
2B
gB
�B
SB
�B
�B
�B
�B
�B
$B
�B
EB
+B
�B
�B
1B
KB
eB
�B
�B
WB
=B
WB
)B
�B
�B
~B
�B
B
�B
�B
�B
�B
 BB
 �B
 �B
!�B
!�B
"4B
"4B
"�B
#B
#TB
#�B
#�B
#�B
$�B
$�B
$�B
%FB
%FB
%zB
%�B
%�B
&2B
&�B
'RB
'�B
(�B
(sB
(�B
)DB
)_B
)_B
)_B
)yB
)yB
)�B
)�B
*0B
*KB
*�B
*�B
*�B
+B
+kB
+�B
,�B
,�B
-�B
-�B
.}B
/ B
/ B
/ B
/ B
/B
/5B
/�B
/�B
0B
0�B
1vB
1�B
1�B
2-B
2-B
2GB
2GB
2aB
2GB
2GB
2�B
2�B
2�B
3B
3�B
3�B
4nB
49B
4B
4�B
4�B
4�B
5�B
5ZB
5�B
5�B
5�B
5�B
5�B
6�B
6�B
7LB
7fB
7�B
7�B
8�B
9�B
:*B
:�B
:�B
;0B
;dB
;�B
;�B
;�B
<B
<B
<�B
=B
=�B
=�B
>B
>BB
>wB
>�B
?.B
?.B
?HB
?}B
?�B
@B
@4B
@�B
A B
AoB
AoB
A�B
A�B
BB
BuB
B�B
B�B
C�B
C�B
C�B
C�B
C�B
D3B
D�B
D�B
F?B
G�B
GzB
GEB
GzB
G�B
HKB
H1B
HB
G�B
H�B
IlB
I�B
I�B
J#B
J�B
J�B
J�B
J�B
KxB
K�B
K�B
LdB
L�B
L�B
L�B
L�B
MB
MB
M�B
N"B
N"B
NVB
NVB
N�B
N�B
O\B
OvB
O�B
O�B
O�B
O�B
O�B
O�B
PbB
P}B
P�B
P�B
P�B
QB
Q4B
QNB
QhB
Q�B
Q�B
Q�B
RB
RB
R�B
R�B
R�B
S�B
S�B
S�B
TaB
T�B
T�B
T�B
UgB
UgB
U�B
UgB
U�B
VmB
V�B
V�B
V�B
V�B
V�B
V�B
W?B
WsB
W�B
X+B
X_B
XEB
XB
X�B
XyB
XyB
X�B
X�B
YKB
Y�B
Y�B
Y�B
ZQB
ZB
ZQB
ZQB
ZkB
Z�B
Z�B
Z�B
[#B
[qB
[�B
[�B
\B
[�B
\]B
\�B
\�B
]/B
]B
]B
]�B
]�B
^5B
^OB
^jB
^�B
_B
_;B
`B
`B
`BB
`�B
aHB
a-B
a-B
a�B
b4B
b4B
b�B
b�B
cB
c B
c B
cTB
c�B
c�B
dB
dB
dB
d@B
d@B
eB
ezB
ezB
e`B
ezB
e�B
f2B
f2B
ffB
f�B
f�B
gB
g8B
h
B
h
B
h$B
h$B
hsB
h�B
h�B
h�B
h�B
h�B
h�B
h�B
h�B
i_B
i�B
i�B
i�B
i�B
jB
jB
jB
j�B
j�B
j�B
jeB
j�B
jB
jeB
j�B
j�B
j�B
j�B
j�B
j�B
kB
kB
kkB
kkB
kkB
k�B
k�B
k�B
k�B
k�B
k�B
k�B
l"B
l=B
l=B
lWB
l�B
l�B
l�B
l�B
m�B
ncB
nIB
n�B
n�B
oOB
oiB
o�B
o�B
o�B
o�B
pB
p�B
p�B
p�B
p�B
qB
qAB
q[B
q�B
q�B
q�B
r-B
rGB
r-B
r-B
rB
r|B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s3B
sMB
s�B
s�B
tB
t9B
t9B
t�B
t�B
t�B
t�B
uZB
u�B
u�B
u�B
u�B
vB
v�B
v�B
wB
wLB
w�B
w�B
w�B
w�B
w�B
xB
w�B
x8B
xlB
xlB
x�B
x�B
x�B
x�B
y	B
y$B
yrB
y�B
y�B
y�B
y�B
y�B
z*B
z^B
z�B
zxB
zxB
z�B
z�B
z�B
{0B
{0B
{0B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
|PB
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
}<B
}<B
}�B
}�B
}�B
}�B
}�B
~B
~(B
~(B
~BB
~]B
~wB
~�B
~�B
B
cB
cB
�B
�B
�B
�B
� B
�OB
�iB
��B
��B
��B
�B
�B
�B
�;B
�;B
��B
��B
��B
�B
�'B
�AB
�uB
��B
��B
��B
��B
��B
�aB
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
 B
4B
B
B
4B
�B
.B
}B
�B
�B
hB
NB
4B
NB
4B
4B
4B
B
4B
hB
hB
4B
4B
}B
�B
B
bB
�B
�B
.B
HB
}B
�B
�B
�B
HB
�B
B

�B
KB
gB
�B
�B	��B
B
=�B
;�B
oB
��B
�/B"�BPbB^OBH1B{�B��B�B��B��B��B��B��Bw�BbB^5B]B\xB^�B^�BqBlB=�B�BuB
� B
�UB
��B
��B
^�B
C{B
/�B
�B	��B	�4B	�,B	ƨB	��B	�hB	��B	�DB	{�B	qvB	[#B	LJB	BB	?�B	<B	5?B	-�B	(�B	$�B	#�B	'8B	BB	RB	e�B	bB	G_B	8lB	G�B	S&B	X+B	Z�B	`vB	iyB	l�B	z*B	�B	�~B	�#B	��B	��B	�*B	��B	��B	�'B	�jB	�9B	یB	�dB	�B	��B	�B
�B
�B
$�B
&�B
(�B
+6B
)�B
)�B
*B
-]B
-�B
6�B
6+B
5ZB
6+B
4nB
1�B
6B
:�B
6�B
*eB
)�B
,�B
,WB
,=B
+6B
*0B
*�B
/�B
-�B
,WB
+�B
./B
2-B
<�B
>�B
=<B
8�B
5�B
6B
6FB
7�B
8�B
=VB
D�B
D�B
B�B
?}B
<�B
=<B
<B
<�B
@�B
CaB
C�B
C�B
BuB
@4B
=�B
=<B
>B
=�B
=VB
;JB
9>B
:xB
;�B
<�B
:�B
8�B
7LB
8�B
7�B
5�B
5B
2-B
1�B
0!B
/ B
1�B
0�B
1�B
1�B
0�B
.�B
-�B
,�B
+6B
*KB
)yB
(�B
(>B
'�B
$�B
$tB
#�B
%�B
$�B
#:B
#�B
"�B
�B
�B
�B
sB
�B
�B
mB
�B
aB
FB
[B
oB
hB
.B
B
dB
	7B
�B
�B
 OB	��B	�B	�iB	�B	�B	�|B	�B	�vB	�AB	��B	��B
 �B
�B
YB
�B
�B
�B
�B
	B
	B
�B
�B
�B
SB
�B
�B
SB
mB
9B
9B
B
�B
B
B
B
UB
 �B
 B
 4B
;B
�B
GB
-B
�B
�B
�B
GB
�B
�B
�B
[B
�B
UB
;B	��B	��B	��B	�DB	�DB	��B	��B	��B	��B	�dB	��B	��B	�<B	�"B	��B	��B	��B	��B	��B
  B
[B
�B
�B
uB
uB
uB
�B
�B
�B
�B
�B
 �B	�}B
 �B
 �B
 �B
B
 B
�B
�B
oB
oB
�B
oB
oB
�B
;B
UB
B
�B
B
�B
AB
AB
'B
�B
�B
;B
�B
 B
;B
 B
B
 �B
 �B
 �B
 �B
 4B
 iB
 �B
UB
oB
�B
'B
[B
�B
{B
B
MB
B
9B
�B
B
B
�B
�B
�B
�B
�B
�B
_B
�B
�B
�B
B
	B
	lB
	B
	B
	RB
	�B

�B

�B

�B
B
�B
�B
�B
�B
�B
�B
�B
B
B
dB
�B
�B
PB
�B
�B
�B
�B
\B
�B
.B
B
�B
�B
�B
 B
�B
�B
4B
�B
�B
�B
�B
�B
�B
�B
�B
:B
B
 B
B
TB
�B
�B
&B
uB
�B
[B
[B
uB
�B
�B
�B
B
2B
2B
gB
�B
SB
�B
�B
�B
�B
�B
$B
�B
EB
+B
�B
�B
1B
KB
eB
�B
�B
WB
=B
WB
)B
�B
�B
~B
�B
B
�B
�B
�B
�B
 BB
 �B
 �B
!�B
!�B
"4B
"4B
"�B
#B
#TB
#�B
#�B
#�B
$�B
$�B
$�B
%FB
%FB
%zB
%�B
%�B
&2B
&�B
'RB
'�B
(�B
(sB
(�B
)DB
)_B
)_B
)_B
)yB
)yB
)�B
)�B
*0B
*KB
*�B
*�B
*�B
+B
+kB
+�B
,�B
,�B
-�B
-�B
.}B
/ B
/ B
/ B
/ B
/B
/5B
/�B
/�B
0B
0�B
1vB
1�B
1�B
2-B
2-B
2GB
2GB
2aB
2GB
2GB
2�B
2�B
2�B
3B
3�B
3�B
4nB
49B
4B
4�B
4�B
4�B
5�B
5ZB
5�B
5�B
5�B
5�B
5�B
6�B
6�B
7LB
7fB
7�B
7�B
8�B
9�B
:*B
:�B
:�B
;0B
;dB
;�B
;�B
;�B
<B
<B
<�B
=B
=�B
=�B
>B
>BB
>wB
>�B
?.B
?.B
?HB
?}B
?�B
@B
@4B
@�B
A B
AoB
AoB
A�B
A�B
BB
BuB
B�B
B�B
C�B
C�B
C�B
C�B
C�B
D3B
D�B
D�B
F?B
G�B
GzB
GEB
GzB
G�B
HKB
H1B
HB
G�B
H�B
IlB
I�B
I�B
J#B
J�B
J�B
J�B
J�B
KxB
K�B
K�B
LdB
L�B
L�B
L�B
L�B
MB
MB
M�B
N"B
N"B
NVB
NVB
N�B
N�B
O\B
OvB
O�B
O�B
O�B
O�B
O�B
O�B
PbB
P}B
P�B
P�B
P�B
QB
Q4B
QNB
QhB
Q�B
Q�B
Q�B
RB
RB
R�B
R�B
R�B
S�B
S�B
S�B
TaB
T�B
T�B
T�B
UgB
UgB
U�B
UgB
U�B
VmB
V�B
V�B
V�B
V�B
V�B
V�B
W?B
WsB
W�B
X+B
X_B
XEB
XB
X�B
XyB
XyB
X�B
X�B
YKB
Y�B
Y�B
Y�B
ZQB
ZB
ZQB
ZQB
ZkB
Z�B
Z�B
Z�B
[#B
[qB
[�B
[�B
\B
[�B
\]B
\�B
\�B
]/B
]B
]B
]�B
]�B
^5B
^OB
^jB
^�B
_B
_;B
`B
`B
`BB
`�B
aHB
a-B
a-B
a�B
b4B
b4B
b�B
b�B
cB
c B
c B
cTB
c�B
c�B
dB
dB
dB
d@B
d@B
eB
ezB
ezB
e`B
ezB
e�B
f2B
f2B
ffB
f�B
f�B
gB
g8B
h
B
h
B
h$B
h$B
hsB
h�B
h�B
h�B
h�B
h�B
h�B
h�B
h�B
i_B
i�B
i�B
i�B
i�B
jB
jB
jB
j�B
j�B
j�B
jeB
j�B
jB
jeB
j�B
j�B
j�B
j�B
j�B
j�B
kB
kB
kkB
kkB
kkB
k�B
k�B
k�B
k�B
k�B
k�B
k�B
l"B
l=B
l=B
lWB
l�B
l�B
l�B
l�B
m�B
ncB
nIB
n�B
n�B
oOB
oiB
o�B
o�B
o�B
o�B
pB
p�B
p�B
p�B
p�B
qB
qAB
q[B
q�B
q�B
q�B
r-B
rGB
r-B
r-B
rB
r|B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s3B
sMB
s�B
s�B
tB
t9B
t9B
t�B
t�B
t�B
t�B
uZB
u�B
u�B
u�B
u�B
vB
v�B
v�B
wB
wLB
w�B
w�B
w�B
w�B
w�B
xB
w�B
x8B
xlB
xlB
x�B
x�B
x�B
x�B
y	B
y$B
yrB
y�B
y�B
y�B
y�B
y�B
z*B
z^B
z�B
zxB
zxB
z�B
z�B
z�B
{0B
{0B
{0B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
|PB
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
}<B
}<B
}�B
}�B
}�B
}�B
}�B
~B
~(B
~(B
~BB
~]B
~wB
~�B
~�B
B
cB
cB
�B
�B
�B
�B
� B
�OB
�iB
��B
��B
��B
�B
�B
�B
�;B
�;B
��B
��B
��B
�B
�'B
�AB
�uB
��B
��B
��B
��B
��B
�aB
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105246  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604192621  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604192621  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604192621                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605042629  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605042629  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610161505                      G�O�G�O�G�O�                