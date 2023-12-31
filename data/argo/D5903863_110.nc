CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       d2015-01-15T23:31:51Z creation; 2015-01-15T23:31:51Z updated; 2015-09-28T12:13:18Z converted from 3.0   
references        (http://www.argodatamgt.org/Documentation   comment           user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7    PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7`   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8    PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8$   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8D   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8d   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�E�vQ�   
_FillValue        A.�~       axis      T           8h   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8p   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�E�vQ�   
_FillValue        A.�~            8t   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8|   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I(   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `h   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  o�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  h   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �H   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �(   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �h   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ܘ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ߘ   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �$   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �(   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �8   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �<   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �@   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �DArgo profile    3.1 1.2 19500101000000  20150115233151  20170523133352  5903863 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               nA   AO  4298_0127_110                   2C  D   NAVIS_A                         0127                            120111                          863 @�2ὡ?�1   @�2�r��
@7u\(��b�E����1   GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                      nA   A   A   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*�C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�3D��3D�  D�<�D� D�� D�  D�@ D� D�� D�  D�@ D�|�D�� D���D�@ D� D�� D�  D�@ D� D��D���D�<�D� D�� D�  D�@ D� D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�6f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@z�H@�p�@�p�A�RA6�RAV�RAv�RA�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)B�B�B�B�B%�B-�B5�B=�BE�BM�BU�B]�Be�Bm�Bu�B}�B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
Ck�Ck�Ck�Ck�C	k�Ck�Ck�Ck�Ck�Ck�Ck�Ck�Ck�Ck�Ck�Ck�C!k�C#k�C%k�C'k�C)�C+k�C-k�C/k�C1k�C3k�C5k�C7k�C9k�C;k�C=k�C?k�CAk�CCk�CEk�CGk�CIk�CKk�CMk�COk�CQk�CSk�CUk�CWk�CYk�C[k�C]k�C_k�Cak�Cck�Cek�Cgk�Cik�Ckk�Cmk�Cok�Cqk�Csk�Cuk�Cwk�Cyk�C{k�C}k�Ck�C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���Cµ�Cõ�Cĵ�Cŵ�CƵ�Cǵ�Cȵ�Cɵ�Cʵ�C˵�C̵�C͵�Cε�Cϵ�Cе�Cѵ�Cҵ�Cӵ�CԵ�Cյ�Cֵ�C׵�Cص�Cٵ�Cڵ�C۵�Cܵ�Cݵ�C޵�Cߵ�C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C���C���C���C���C���C���C���C���C���C���C���C���D Z�D ��DZ�D��DZ�D��DZ�D��DZ�D��DZ�D��DZ�D��DZ�D��DZ�D��D	Z�D	��D
Z�D
��DZ�D��DZ�D��DZ�D��DZ�D��DZ�D��DZ�D��DZ�D��DZ�D��DZ�D��DZ�D��DZ�D��DZ�D��DZ�D��DZ�D��DZ�D��DZ�D��DZ�D��DZ�D��DZ�D��DZ�D��DZ�D��D Z�D ��D!Z�D!��D"Z�D"��D#Z�D#��D$Z�D$��D%Z�D%��D&Z�D&��D'Z�D'��D(Z�D(��D)Z�D)��D*Z�D*��D+Z�D+��D,Z�D,��D-Z�D-��D.Z�D.��D/Z�D/��D0Z�D0��D1Z�D1��D2Z�D2��D3Z�D3��D4Z�D4��D5Z�D5��D6Z�D6��D7Z�D7��D8Z�D8��D9Z�D9��D:Z�D:��D;Z�D;��D<Z�D<��D=Z�D=��D>Z�D>��D?Z�D?��D@Z�D@��DAZ�DA��DBZ�DB��DCZ�DC��DDZ�DD��DEZ�DE��DFZ�DF��DGZ�DG��DHZ�DH��DIZ�DI��DJZ�DJ��DKZ�DK��DLZ�DL��DMZ�DM��DNZ�DN��DOZ�DO��DPZ�DP��DQZ�DQ��DRZ�DR��DSZ�DS��DTZ�DT��DUZ�DU��DVZ�DV��DWZ�DW��DXZ�DX��DYZ�DY��DZZ�DZ��D[Z�D[��D\Z�D\��D]Z�D]��D^Z�D^��D_Z�D_��D`Z�D`��DaZ�Da��DbZ�Db��DcZ�Dc��DdZ�Dd��DeZ�De��DfZ�Df��DgZ�Dg��DhZ�Dh��DiZ�Di��DjZ�Dj��DkZ�Dk��DlZ�Dl��DmZ�Dm��DnZ�Dn��DoZ�Do��DpZ�Dp��DqZ�Dq��DrZ�Dr��DsZ�Ds��DtZ�Dt��DuZ�Du��DvZ�Dv��DwZ�Dw��DxZ�Dx��DyZ�Dy��DzZ�Dz��D{Z�D{��D|Z�D|��D}Z�D}��D~Z�D~��DZ�D��D�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD­qD��qD�-qD�mqDíqD��qD�-qD�mqDĭqD��qD�-qD�mqDŭqD��qD�-qD�mqDƭqD��qD�-qD�mqDǭqD��qD�-qD�mqDȭqD��qD�-qD�mqDɭqD��qD�-qD�mqDʭqD��qD�-qD�mqD˭qD��qD�-qD�mqḒqD��qD�-qD�mqDͭqD��qD�-qD�mqDέqD��qD�-qD�mqDϭqD��qD�-qD�mqDЭqD��qD�-qD�mqDѭqD��qD�-qD�mqDҭqD��qD�-qD�mqDӭqD��qD�-qD�mqDԭqD��qD�-qD�mqDխqD��qD�-qD�mqD֭qD��qD�-qD�mqD׭qD��qD�-qD�mqDحqD��qD�-qD�mqD٭qD��qD�-qD�mqDڭqD��qD�-qD�mqDۭqD��qD�-qD�mqDܭqD��qD�-qD�mqDݭqD��qD�-qD�mqDޭqD��qD�-qD�mqD߭qD��qD�-qD�mqD�qD��qD�-qD�j>D�qD��qD�-qD�p�DⰤD��qD�*>D�mqD�qD��qD�-qD�mqD�qD��qD�-qD�j>D�qD��>D�-qD�mqD�qD��qD�-qD�mqD�>D��>D�*>D�mqD�qD��qD�-qD�mqD鰤D��qD�-qD�mqD�qD��qD�-qD�mqD�qD��qD�-qD�mqD�qD��qD�-qD�mqD��qD��qD�-qD�mqD�qD��qD�-qD�mqD�qD��qD�-qD�mqD�qD��qD�-qD�mqD�qD��qD�-qD�mqD�qD��qD�-qD�mqD�qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��D�#�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��hA���A���A��\A��+A�v�A�p�A�p�A�r�A�x�A��A�|�A�v�A�ffA�^5A�XA�O�A�K�A�G�A�C�A�A�A�9XA�$�A��A��A��A� �A��A��A��A��A�bA���A��A��A�|�A���A���A�p�A�bA�33A�/A��DA�S�A���A�-A�JA�I�A��
A��!A���A�"�A�p�A���A���A�E�A��A��#A�z�A���A���A���A�1A�z�A��A�r�A�1A�$�A��TA�`BA���A�x�A�~�A�t�A���A�r�A��\A��A���A���A��hA�A�ĜA�O�A�5?A�VA�\)A�dZA���A��A�M�A��^A�bNA�l�A�ȴA�7LA��#A�n�A���A��9A�E�A}��A{/AxJAu%At  Arn�AqK�ApQ�Am\)AjĜAi�AgVAe|�AdbAa|�A`��A^�+A[��A[�AXffAV�jAVVASG�AO��AO/AN �AL1'AJ��AI��AIAH��AG��AG/AE�-AEhsAD��AD��AC|�AAl�A@1'A>n�A<��A:��A9��A8�RA6��A5��A3�A2��A2�jA2r�A1A/�PA.��A.jA-�A-�wA-S�A,�A,A+t�A*-A(�A'�PA&ffA%�#A%��A$�\A#t�A"�/A!�^A �DA�A~�A+A��A��A�RA�#At�AA�A�jA{A��A`BA5?A��Ax�Av�A�wA
�+A
A	hsA�A�`A9XA�mAhsAffA�FA��A��AA7LA �DA =q@�~�@�=q@�V@� �@��w@��@�dZ@�p�@�D@�P@�$�@�G�@��#@�  @�@�@�V@�^5@���@��@�%@��@ߥ�@�33@޸R@ݙ�@�b@�x�@���@�l�@�"�@ְ!@�hs@�=q@�x�@�7L@��`@�z�@϶F@Ώ\@�/@�Q�@ʇ+@��@ǍP@Ɨ�@�v�@�?}@�z�@�\)@�M�@���@�O�@�Ĝ@�1@�l�@�
=@���@���@�~�@��#@�bN@��w@�S�@��@�E�@���@�hs@��`@�Ĝ@��9@��@�j@��@��w@�|�@���@�ȴ@�~�@��#@��^@���@���@���@��@��@��@�(�@��@��P@�33@���@���@�  @�I�@�(�@���@��
@�;d@��^@�@���@��^@���@�X@���@��@�;d@�ȴ@�^5@��@�@��7@�?}@���@��/@�Z@�o@�;d@��H@�~�@���@���@��@��@�z�@�b@��F@�|�@�@�M�@��@�hs@���@�I�@�r�@�bN@���@�(�@�b@���@���@�\)@�33@��@���@�n�@�E�@�=q@�J@���@��@�r�@�bN@���@���@�\)@�l�@�l�@��@��R@��\@�-@��@���@��@�G�@�%@���@���@�1@��F@�t�@�;d@�+@�33@�
=@�ȴ@�~�@�^5@�{@��@��7@�hs@�O�@�%@��`@�Ĝ@���@�9X@�  @���@��@�"�@���@���@�=q@�@��-@���@��@�`B@�V@��/@��u@�j@�I�@��m@��w@�|�@�\)@�33@��R@�v�@��@��-@�p�@��@���@�r�@�9X@��@��@��m@��;@��
@���@�t�@�C�@��@���@�ff@��@��7@�V@��`@��@��u@�z�@�Z@��@��@��P@���@�+@�5?@��/@��D@��@��@�A�@�  @��F@���@�|�@�dZ@��@��!@�=q@���@�x�@�&�@�/@�?}@�/@���@���@���@��@�(�@�P@~v�@}�h@}V@{o@z=q@y��@y�@y��@y&�@y%@yx�@y��@xA�@w|�@v��@vE�@v$�@v@u�T@u@u�@t�/@tz�@tZ@tI�@t9X@t(�@s��@s��@sdZ@r�!@r-@q��@qX@p��@p��@p�u@pbN@pA�@p1'@p  @o�@ol�@o\)@o\)@o�@o+@o
=@n�+@n$�@m�-@l��@l��@l�@lZ@k�m@kt�@kS�@kC�@k33@k33@k33@k33@k"�@j�@k33@k"�@k"�@ko@ko@j��@j�\@jM�@i��@h1'@gK�@f�R@f��@fV@e��@eO�@eV@d��@d��@d9X@d�@c�m@c�m@dI�@c�m@c33@c@b�!@bn�@a�@a7L@`Ĝ@`Q�@`Q�@_�;@_�w@_��@_|�@_
=@^��@^V@^{@]�-@\�@\(�@[ƨ@Z��@Z~�@Y��@Y�@XbN@W��@W��@WK�@V��@Vȴ@V��@VE�@VV@V5?@V$�@V{@U�@Up�@Tj@S�
@Sƨ@S��@S�@SS�@SC�@S33@So@R�H@Rn�@R�@Q��@Q%@P�`@P��@Pr�@P1'@P �@P  @O�;@O��@N��@Nff@M@M�h@Mp�@M`B@M?}@M/@L��@L�/@L�j@L�@Lj@K��@KdZ@KS�@KdZ@Ko@JM�@I��@I��@I7L@I%@H��@H�9@HA�@H  @G��@F��@F5?@E��@Ep�@EV@D�j@D�D@Dj@DZ@DZ@DI�@DI�@D9X@D9X@D(�@Cƨ@B�@B^5@A�^@A�7@AG�@A%@@�`@@Ĝ@@��@@�u@@r�@@1'@@b@@  @?�w@?��@?|�@?�@>�y@>�@>�R@>v�@>$�@=�T@=�T@=��@=�h@=p�@=?}@<�@<��@;��@;��@;t�@;33@;@:J@9��@9��@9�@9�@9�@9��@97L@9�@8��@8��@8��@8Ĝ@8Ĝ@8�9@8��@8�@8b@7��@7��@7�P@7l�@7K�@7�@6�R@6v�@5�@5�h@5O�@5V@4�j@4�D@4Z@4�@3�
@3��@3dZ@333@3@2��@2^5@1�#@1��@1x�@1X@1&�@1%@0�`@0�9@0�u@0r�@0A�@/�;@/�w@/�@/�P@/K�@/�@.�@.ȴ@.��@.V@.5?@.$�@.{@-��@-�@-?}@-V@,�@,�j@,�D@,I�@+�m@+ƨ@+��@+S�@+33@+@*��@*�!@*��@*~�@*J@)�^@)hs@)X@)7L@)%@(��@(�9@(��@(r�@(Q�@(A�@( �@'�;@'��@'K�@'�@&�@&��@&ff@&5?@&$�@&$�@&{@&@%�T@%�@%?}@$�@$��@$��@$j@$Z@$I�@$1@#�m@#��@#t�@#dZ@#"�@#o@#o@"��@"�\@"-@!�@!�^@!�7@!7L@!%@ �`@ �u@ 1'@ b@   @�;@�@�P@|�@l�@K�@+@
=@ȴ@ff@{@��@�h@O�@V@�@�/@�j@z�@Z@9X@(�@��@��@33@@�@��@��@��@^5@-@�@��@��@��@��@�7@x�@hs@X@&�@�`@�@1'@�w@�@|�@;d@
=@�y@�@��@V@E�@$�@@��@p�@`B@/@V@�j@z�@Z@I�@(�@�
@�@"�@�@��@�!@�\@n�@M�@M�@�@��@J@��@��@�`@�`@�u@1'@  @��@K�@
=@ȴ@ȴ@��@V@V@5?@@�-@�@`B@/@V@�@�j@��@j@(�@�F@�@C�@"�@
�H@
��@
�\@
n�@
=q@
�@	��@	��@	��@	hs@	7L@�`@��@�u@Q�@1'@b@  @�;11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��hA���A���A��\A��+A�v�A�p�A�p�A�r�A�x�A��A�|�A�v�A�ffA�^5A�XA�O�A�K�A�G�A�C�A�A�A�9XA�$�A��A��A��A� �A��A��A��A��A�bA���A��A��A�|�A���A���A�p�A�bA�33A�/A��DA�S�A���A�-A�JA�I�A��
A��!A���A�"�A�p�A���A���A�E�A��A��#A�z�A���A���A���A�1A�z�A��A�r�A�1A�$�A��TA�`BA���A�x�A�~�A�t�A���A�r�A��\A��A���A���A��hA�A�ĜA�O�A�5?A�VA�\)A�dZA���A��A�M�A��^A�bNA�l�A�ȴA�7LA��#A�n�A���A��9A�E�A}��A{/AxJAu%At  Arn�AqK�ApQ�Am\)AjĜAi�AgVAe|�AdbAa|�A`��A^�+A[��A[�AXffAV�jAVVASG�AO��AO/AN �AL1'AJ��AI��AIAH��AG��AG/AE�-AEhsAD��AD��AC|�AAl�A@1'A>n�A<��A:��A9��A8�RA6��A5��A3�A2��A2�jA2r�A1A/�PA.��A.jA-�A-�wA-S�A,�A,A+t�A*-A(�A'�PA&ffA%�#A%��A$�\A#t�A"�/A!�^A �DA�A~�A+A��A��A�RA�#At�AA�A�jA{A��A`BA5?A��Ax�Av�A�wA
�+A
A	hsA�A�`A9XA�mAhsAffA�FA��A��AA7LA �DA =q@�~�@�=q@�V@� �@��w@��@�dZ@�p�@�D@�P@�$�@�G�@��#@�  @�@�@�V@�^5@���@��@�%@��@ߥ�@�33@޸R@ݙ�@�b@�x�@���@�l�@�"�@ְ!@�hs@�=q@�x�@�7L@��`@�z�@϶F@Ώ\@�/@�Q�@ʇ+@��@ǍP@Ɨ�@�v�@�?}@�z�@�\)@�M�@���@�O�@�Ĝ@�1@�l�@�
=@���@���@�~�@��#@�bN@��w@�S�@��@�E�@���@�hs@��`@�Ĝ@��9@��@�j@��@��w@�|�@���@�ȴ@�~�@��#@��^@���@���@���@��@��@��@�(�@��@��P@�33@���@���@�  @�I�@�(�@���@��
@�;d@��^@�@���@��^@���@�X@���@��@�;d@�ȴ@�^5@��@�@��7@�?}@���@��/@�Z@�o@�;d@��H@�~�@���@���@��@��@�z�@�b@��F@�|�@�@�M�@��@�hs@���@�I�@�r�@�bN@���@�(�@�b@���@���@�\)@�33@��@���@�n�@�E�@�=q@�J@���@��@�r�@�bN@���@���@�\)@�l�@�l�@��@��R@��\@�-@��@���@��@�G�@�%@���@���@�1@��F@�t�@�;d@�+@�33@�
=@�ȴ@�~�@�^5@�{@��@��7@�hs@�O�@�%@��`@�Ĝ@���@�9X@�  @���@��@�"�@���@���@�=q@�@��-@���@��@�`B@�V@��/@��u@�j@�I�@��m@��w@�|�@�\)@�33@��R@�v�@��@��-@�p�@��@���@�r�@�9X@��@��@��m@��;@��
@���@�t�@�C�@��@���@�ff@��@��7@�V@��`@��@��u@�z�@�Z@��@��@��P@���@�+@�5?@��/@��D@��@��@�A�@�  @��F@���@�|�@�dZ@��@��!@�=q@���@�x�@�&�@�/@�?}@�/@���@���@���@��@�(�@�P@~v�@}�h@}V@{o@z=q@y��@y�@y��@y&�@y%@yx�@y��@xA�@w|�@v��@vE�@v$�@v@u�T@u@u�@t�/@tz�@tZ@tI�@t9X@t(�@s��@s��@sdZ@r�!@r-@q��@qX@p��@p��@p�u@pbN@pA�@p1'@p  @o�@ol�@o\)@o\)@o�@o+@o
=@n�+@n$�@m�-@l��@l��@l�@lZ@k�m@kt�@kS�@kC�@k33@k33@k33@k33@k"�@j�@k33@k"�@k"�@ko@ko@j��@j�\@jM�@i��@h1'@gK�@f�R@f��@fV@e��@eO�@eV@d��@d��@d9X@d�@c�m@c�m@dI�@c�m@c33@c@b�!@bn�@a�@a7L@`Ĝ@`Q�@`Q�@_�;@_�w@_��@_|�@_
=@^��@^V@^{@]�-@\�@\(�@[ƨ@Z��@Z~�@Y��@Y�@XbN@W��@W��@WK�@V��@Vȴ@V��@VE�@VV@V5?@V$�@V{@U�@Up�@Tj@S�
@Sƨ@S��@S�@SS�@SC�@S33@So@R�H@Rn�@R�@Q��@Q%@P�`@P��@Pr�@P1'@P �@P  @O�;@O��@N��@Nff@M@M�h@Mp�@M`B@M?}@M/@L��@L�/@L�j@L�@Lj@K��@KdZ@KS�@KdZ@Ko@JM�@I��@I��@I7L@I%@H��@H�9@HA�@H  @G��@F��@F5?@E��@Ep�@EV@D�j@D�D@Dj@DZ@DZ@DI�@DI�@D9X@D9X@D(�@Cƨ@B�@B^5@A�^@A�7@AG�@A%@@�`@@Ĝ@@��@@�u@@r�@@1'@@b@@  @?�w@?��@?|�@?�@>�y@>�@>�R@>v�@>$�@=�T@=�T@=��@=�h@=p�@=?}@<�@<��@;��@;��@;t�@;33@;@:J@9��@9��@9�@9�@9�@9��@97L@9�@8��@8��@8��@8Ĝ@8Ĝ@8�9@8��@8�@8b@7��@7��@7�P@7l�@7K�@7�@6�R@6v�@5�@5�h@5O�@5V@4�j@4�D@4Z@4�@3�
@3��@3dZ@333@3@2��@2^5@1�#@1��@1x�@1X@1&�@1%@0�`@0�9@0�u@0r�@0A�@/�;@/�w@/�@/�P@/K�@/�@.�@.ȴ@.��@.V@.5?@.$�@.{@-��@-�@-?}@-V@,�@,�j@,�D@,I�@+�m@+ƨ@+��@+S�@+33@+@*��@*�!@*��@*~�@*J@)�^@)hs@)X@)7L@)%@(��@(�9@(��@(r�@(Q�@(A�@( �@'�;@'��@'K�@'�@&�@&��@&ff@&5?@&$�@&$�@&{@&@%�T@%�@%?}@$�@$��@$��@$j@$Z@$I�@$1@#�m@#��@#t�@#dZ@#"�@#o@#o@"��@"�\@"-@!�@!�^@!�7@!7L@!%@ �`@ �u@ 1'@ b@   @�;@�@�P@|�@l�@K�@+@
=@ȴ@ff@{@��@�h@O�@V@�@�/@�j@z�@Z@9X@(�@��@��@33@@�@��@��@��@^5@-@�@��@��@��@��@�7@x�@hs@X@&�@�`@�@1'@�w@�@|�@;d@
=@�y@�@��@V@E�@$�@@��@p�@`B@/@V@�j@z�@Z@I�@(�@�
@�@"�@�@��@�!@�\@n�@M�@M�@�@��@J@��@��@�`@�`@�u@1'@  @��@K�@
=@ȴ@ȴ@��@V@V@5?@@�-@�@`B@/@V@�@�j@��@j@(�@�F@�@C�@"�@
�H@
��@
�\@
n�@
=q@
�@	��@	��@	��@	hs@	7L@�`@��@�u@Q�@1'@b@  @�;11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBBBBBBBBBBBBBBBBBBBBBBBBB+BJBVB\BhBhBhBhBoBoBoBhBbB\BbBoBoBuBuBuBuB{B�B�B�B!�B&�B2-B?}BC�BC�B8RB-B'�B%�B�B�B�BuB	7BB��B��B��B��B�B�sB�NB��BBB�dB�LB��B��B��B�\B�+BffBH�B1'B&�B�B�B�5B�B��BÖB�9B�B��B�PB� BgmBK�B5?B �B\B
��B
�yB
�
B
�wB
��B
�%B
s�B
iyB
_;B
YB
Q�B
G�B
8RB
0!B
(�B
uB	��B	�ZB	�B	��B	ÖB	�XB	��B	�JB	�B	o�B	aHB	YB	H�B	>wB	1'B	�B	�B	JB	B	B��B�mB�HB�5B�)B�B��B��B��B��BƨB�wB�qB�dB��BǮBǮBŢB�}B�?B�B��B��B��B�{B�DB�7B�hB�\B�7B�B�B�B�B�B~�B|�Bz�Bx�Bt�Bn�Bl�BjBhsBgmBe`BdZBbNB_;B\)BYBT�BQ�BK�BH�BF�BC�BA�B>wB:^B9XB8RB5?B1'B.B(�B%�B$�B$�B"�B�B�B�B�B�B�B�B�B�B�B�B�B{B{BuBuBoBoBhB\BbBbB\BVB\BVBoBuBuBoB\BJBPBPBJBPBJBJBJBPBVBoB�B�B�B�B�B�B"�B#�B'�B)�B49B9XB?}BA�BC�BE�BH�BJ�BK�BP�BQ�BS�BW
BYBYBZB\)B^5B_;B`BB`BB`BBaHBffBgmBiyBjBn�Bq�Bs�Bu�Bu�Bu�Bv�Bv�Bx�Bz�B{�B�B�7B�DB�VB�hB�uB�hB�VB�hB�{B��B��B��B��B��B��B��B�FB�dB�jB�qB�qB�qB�qB��BÖBŢBŢBŢBŢBǮB��B��B��B��B��B�
B�B�B�)B�5B�;B�NB�`B�`B�B�B�B��B��B	B	+B	1B	
=B	PB	\B	uB	�B	�B	�B	!�B	"�B	)�B	,B	.B	0!B	1'B	33B	49B	8RB	:^B	<jB	<jB	>wB	@�B	C�B	G�B	I�B	K�B	M�B	P�B	S�B	W
B	YB	ZB	[#B	_;B	aHB	cTB	dZB	hsB	iyB	iyB	k�B	n�B	o�B	q�B	u�B	w�B	y�B	~�B	�B	�B	�B	�+B	�7B	�DB	�JB	�PB	�bB	�hB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�-B	�-B	�9B	�?B	�FB	�XB	�^B	�dB	�jB	�jB	�qB	�}B	��B	��B	B	ĜB	ŢB	ƨB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�B	��B	�B	�
B	�B	�B	�B	�B	�B	�#B	�#B	�/B	�5B	�5B	�5B	�5B	�/B	�5B	�HB	�HB	�NB	�NB	�ZB	�ZB	�TB	�TB	�NB	�HB	�NB	�BB	�BB	�BB	�BB	�HB	�NB	�TB	�fB	�mB	�fB	�`B	�fB	�mB	�sB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
%B
%B
+B
	7B

=B
JB
JB
PB
VB
VB
VB
VB
JB
DB
DB
DB
DB
DB
VB
VB
VB
\B
\B
\B
bB
hB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
#�B
#�B
#�B
$�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
%�B
&�B
&�B
(�B
(�B
)�B
)�B
+B
+B
+B
,B
,B
,B
-B
-B
-B
.B
/B
/B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
1'B
1'B
2-B
33B
33B
49B
49B
49B
5?B
5?B
5?B
5?B
5?B
5?B
6FB
6FB
6FB
6FB
7LB
7LB
7LB
8RB
8RB
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
<jB
<jB
<jB
>wB
>wB
>wB
>wB
>wB
>wB
?}B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
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
G�B
G�B
H�B
H�B
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
L�B
L�B
L�B
L�B
L�B
L�B
L�B
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
O�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
P�B
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
S�B
S�B
T�B
T�B
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
]/B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
_;B
_;B
`BB
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
aHB
bNB
bNB
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
dZB
e`B
e`B
e`B
e`B
ffB
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
hsB
iyB
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
p�B
p�B
p�B
p�B
p�B
q�B
q�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B9BB?B5B8BDB?B?B:BDBBB?BDBdBqB4B>B`BeB�BbBRBKB9B8B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B#BhB LB"B&�B2�B@ZBE�BG�B=HB4�B1�B-�B&KB$�BhB�B�BBRBOB�kB�GB�[B��B�B��B�B�B�oB��B�YB�YB�]B�cB�.Bm�BP�B55B-0B B��B�BکB�BơB�@B��B�B��B�XBl�BPB9�B#dBB
��B
�B
��B
�xB
�{B
�UB
v�B
l'B
aB
[1B
T�B
L�B
:�B
6 B
/B
�B
�B	��B	��B	�zB	�B	��B	��B	�fB	��B	s1B	dB	^�B	J�B	C1B	7�B	!CB	 �B	2B	yB	/B��B�B�5B��B�BڳB��B�2B��B�,BʨB�|B�"B�QBÞB�<B�7BʂB�/B��B�B��B�=B�]B�B��B�-B��B�oB�HB��B��B��B��B�;B��B~7B|UB{�ByBpBoHBk�BiZBi�BhBe�Be#Bb,B^�B[�BX�BW}BNtBK�BH�BD�BD�BB�B<KB:NB9�B8�B7YB3lB+�B'�B(BB&aB$�B$[BrB�B�BCBVB�B�B�BiB;B�B�BMBB]B�BGB
B|BQB�B!B�B�BPBTB1BB�B�B?B�B�B�B3B*BKBB�BB�BeBNB�B�B �B�B#XB$vB(�B+B5�B;%B@�BC�BE�BG�BJBK9BMyBR!BS�BU�BX/BY�BZB[JB]5B^�B_�B`�B`�BakBcwBg|BhEBjHBk�BoqBr|Bt�Bv4Bv$BvJBw6BwzBy�B{�B|�B��B��B�ZB��B��B��B��B��B�1B��B��B�]B��B��B��B�"B��B�8B��B��B��B��B��B��B��B�B�B�IB��B�>BȠB˱B��B��BҀBӒB׿BعB٘B�KB�ZB�PB�B�DB�iB�^B�B�B�&B��B	�B	�B		IB	�B	2B	�B	�B	�B	�B	(B	"�B	"�B	*qB	,�B	.�B	0�B	1�B	3�B	4�B	8�B	:�B	<�B	<�B	?B	A�B	D�B	HB	J�B	LvB	NtB	QB	TCB	W�B	Y�B	Z�B	[�B	_�B	b+B	c�B	d�B	iB	i�B	jDB	l�B	oKB	p?B	r>B	v#B	xB	z]B	�B	��B	��B	��B	��B	�B	��B	��B	�B	��B	��B	��B	�IB	�&B	�eB	�B	�{B	�AB	��B	��B	��B	�;B	�SB	�^B	�jB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�FB	�B	�7B	�NB	�B	��B	�>B	�B	�)B	�HB	��B	��B	��B	�B	�B	ɍB	�_B	ˏB	˘B	�oB	��B	��B	��B	�yB	ҒB	�dB	�eB	�lB	՚B	��B	�wB	�OB	��B	ٓB	�B	өB	�]B	�XB	ٸB	ٺB	��B	كB	ږB	ۑB	��B	�B	�B	�B	��B	��B	�qB	�lB	�B	��B	��B	�B	�B	�B	�*B	�fB	�5B	��B	��B	�B	�B	��B	�B	�
B	�B	�OB	�B	�B	�:B	�B	�*B	��B	��B	��B	��B	��B	�9B	�B	��B	��B	��B	��B	�	B	�$B	�!B	�}B	�SB	�FB	�QB	�BB	�B	�:B	�7B	�.B	�#B	�<B	�YB	�KB	�)B	�$B	�PB	�"B	�NB	��B	��B	��B	��B	�mB
 gB
 �B
�B
�B
xB
kB
pB
eB
dB
mB
�B
�B
HB
	�B

�B
�B
�B
�B
�B
�B
<B
�B
NB
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
|B
B
8B
�B
B
�B
 B
?B
B
B
�B
'B
�B
�B
 B
5B
6B
B
B
,B
_B
XB
"B
�B
B
4B
wB
UB
7B
�B
B
B
�B
�B
B
�B
�B
�B
�B
B
PB
�B
\B
�B
B
B
B
B
	B
B
"B
SB
?B
EB
�B
 "B
!BB
"@B
#LB
#,B
#4B
#9B
#XB
#�B
#�B
$�B
$LB
$;B
%3B
%AB
%9B
%OB
%BB
&LB
&=B
&dB
&�B
&�B
'AB
'+B
)�B
)�B
*�B
*�B
+�B
+sB
+tB
,rB
,�B
,�B
-�B
-�B
-�B
.�B
/�B
/�B
0�B
0�B
0�B
0{B
0pB
0zB
0pB
0yB
0uB
0�B
0�B
2B
1�B
2�B
3�B
3�B
4�B
4�B
4�B
5�B
5�B
5�B
5�B
5�B
5�B
6�B
6�B
6�B
6�B
7�B
7�B
7�B
8�B
8�B
9�B
9�B
9�B
:�B
:�B
:�B
:�B
:�B
;B
;�B
;�B
<�B
<�B
=SB
>�B
>�B
>�B
>�B
>�B
>�B
@B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
B�B
C�B
D,B
FB
FB
E�B
FB
FB
GB
G6B
G!B
GKB
H>B
H%B
H(B
I5B
I!B
I&B
I,B
I.B
I,B
J&B
J*B
J(B
K9B
KTB
KgB
L5B
L2B
L(B
M<B
M0B
M2B
M@B
M2B
M1B
M<B
N_B
N7B
N*B
N8B
NLB
OHB
OQB
O/B
O@B
O`B
PCB
P8B
P7B
P]B
PhB
QaB
QTB
QIB
QXB
QXB
Q`B
R�B
RPB
R^B
SsB
SWB
SdB
SeB
SWB
TNB
T`B
T�B
T�B
T�B
UVB
UeB
UsB
UpB
UbB
UWB
UqB
ViB
V_B
ViB
V�B
V�B
V�B
W~B
W�B
X�B
X�B
X�B
XeB
X]B
XjB
XiB
X|B
X�B
X�B
X�B
Y}B
Y�B
Y�B
YoB
YsB
Y�B
Y�B
Z�B
Z�B
ZxB
[�B
[}B
[rB
[�B
\�B
\�B
\�B
\�B
\�B
\�B
]�B
]�B
]�B
]�B
^�B
^�B
^�B
^�B
^�B
^�B
^�B
^�B
_�B
_�B
_�B
_�B
_�B
`�B
`�B
`�B
`�B
`�B
`�B
a�B
a�B
a�B
a�B
a�B
bB
a�B
b�B
b�B
c�B
c�B
c�B
c�B
c�B
c�B
d�B
d�B
d�B
d�B
d�B
d�B
d�B
d�B
e�B
d�B
e�B
fB
e�B
fB
f�B
e�B
e�B
f�B
f�B
f�B
f�B
f�B
f�B
f�B
f�B
g	B
g�B
g�B
g�B
g�B
hB
g�B
g�B
g�B
g�B
hB
hB
g
B
f�B
f�B
g�B
g�B
g�B
g�B
g�B
g�B
g�B
g�B
h�B
j'B
iVB
h�B
iB
jB
i�B
jB
jB
j B
i�B
i�B
i�B
j	B
j�B
j�B
j�B
kB
j�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
mB
m$B
nB
n	B
m�B
nB
n�B
oB
n�B
oB
p B
o�B
p	B
o�B
pB
p
B
p"B
pB
p�B
qB
qB
p�B
p�B
r B
q�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<1�><#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<3�Q<#�
<#�
<#�
<#�
<-*N<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - (REPORTED_SURFACE_PRESSURE) for PADJ                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al,2007,JAOT & effects of pressure adjustments                                                                                                                                                            PADJ REPORTED_SURFACE_PRESSURE =0.58 dbar                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CTM alpha = 0.141 & tau = 6.68 s with error equal to the correction                                                                                                                                                                                             Pressures adjusted using reported surface pressure when warranted.                                                                                                                                                                                              none                                                                                                                                                                                                                                                            Salinity corrected for pressure sensor calibration drift and cell thermal mass                                                                                                                                                                                  201603101135332016031011353320160310113533  AO  ARCAADJP                                                                    20150115233151    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150115233151  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20150115233151  QCF$                G�O�G�O�G�O�0                                                                                                                                   G�O�G�O�G�O�                PM  ARSQPADJV1.1                                                                20160310113533  QC  PRES            @�  D�6fG�O�                PM  ARSQCTM V1.1                                                                20160310113533  QC  PSAL            @�  D�6fG�O�                PM  ARSQOWGUV1.0WOD + Argo                                                      20170523133352  IP                  G�O�G�O�G�O�                