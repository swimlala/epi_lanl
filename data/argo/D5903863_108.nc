CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       d2014-12-26T23:31:38Z creation; 2014-12-26T23:31:38Z updated; 2015-09-28T12:13:30Z converted from 3.0   
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
_FillValue                 �  I,   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `x   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  o�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �d   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �P   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �<   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ܤ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �    HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �P   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �`   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �d   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �t   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �x   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �|   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20141226233138  20170523133352  5903863 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               lA   AO  4298_0127_108                   2C  D   NAVIS_A                         0127                            120111                          863 @�-��b 
1   @�-┫�@7�KƧ��c�\)1   GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                      lA   A   A   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B��B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�C3D�vf111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @z�H@�p�@�p�A�RA6�RAV�RAv�RA�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)B�BG�B�B�B%�B-�B5�B=�BE�BM�BU�B]�Be�Bm�Bu�B}�B��
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
Ck�Ck�Ck�Ck�C	k�Ck�Ck�Ck�Ck�Ck�Ck�Ck�Ck�Ck�Ck�Ck�C!k�C#k�C%k�C'k�C)k�C+k�C-k�C/k�C1k�C3k�C5k�C7k�C9k�C;k�C=k�C?k�CAk�CCk�CEk�CGk�CIk�CKk�CMk�COk�CQk�CSk�CUk�CWk�CYk�C[k�C]k�C_k�Cak�Cck�Cek�Cgk�Cik�Ckk�Cmk�Cok�Cqk�Csk�Cuk�Cwk�Cyk�C{k�C}k�Ck�C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���Cµ�Cõ�Cĵ�Cŵ�CƵ�Cǵ�Cȵ�Cɵ�Cʵ�C˵�C̵�C͵�Cε�Cϵ�Cе�Cѵ�Cҵ�Cӵ�CԵ�Cյ�Cֵ�C׵�Cص�Cٵ�Cڵ�C۵�Cܵ�Cݵ�C޵�Cߵ�C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C���C���C���C���C���C���C���C���C���C���C���C���D Z�D ��DZ�D��DZ�D��DZ�D��DZ�D��DZ�D��DZ�D��DZ�D��DZ�D��D	Z�D	��D
Z�D
��DZ�D��DZ�D��DZ�D��DZ�D��DZ�D��DZ�D��DZ�D��DZ�D��DZ�D��DZ�D��DZ�D��DZ�D��DZ�D��DZ�D��DZ�D��DZ�D��DZ�D��DZ�D��DZ�D��DZ�D��DZ�D��D Z�D ��D!Z�D!��D"Z�D"��D#Z�D#��D$Z�D$��D%Z�D%��D&Z�D&��D'Z�D'��D(Z�D(��D)Z�D)��D*Z�D*��D+Z�D+��D,Z�D,��D-Z�D-��D.Z�D.��D/Z�D/��D0Z�D0��D1Z�D1��D2Z�D2��D3Z�D3��D4Z�D4��D5Z�D5��D6Z�D6��D7Z�D7��D8Z�D8��D9Z�D9��D:Z�D:��D;Z�D;��D<Z�D<��D=Z�D=��D>Z�D>��D?Z�D?��D@Z�D@��DAZ�DA��DBZ�DB��DCZ�DC��DDZ�DD��DEZ�DE��DFZ�DF��DGZ�DG��DHZ�DH��DIZ�DI��DJZ�DJ��DKZ�DK��DLZ�DL��DMZ�DM��DNZ�DN��DOZ�DO��DPZ�DP��DQZ�DQ��DRZ�DR��DSZ�DS��DTZ�DT��DUZ�DU��DVZ�DV��DWZ�DW��DXZ�DX��DYZ�DY��DZZ�DZ��D[Z�D[��D\Z�D\��D]Z�D]��D^Z�D^��D_Z�D_��D`Z�D`��DaZ�Da��DbZ�Db��DcZ�Dc��DdZ�Dd��DeZ�De��DfZ�Df��DgZ�Dg��DhZ�Dh��DiZ�Di��DjZ�Dj��DkZ�Dk��DlZ�Dl��DmZ�Dm��DnZ�Dn��DoZ�Do��DpZ�Dp��DqZ�Dq��DrZ�Dr��DsZ�Ds��DtZ�Dt��DuZ�Du��DvZ�Dv��DwZ�Dw��DxZ�Dx��DyZ�Dy��DzZ�Dz��D{Z�D{��D|Z�D|��D}Z�D}��D~Z�D~��DZ�D��D�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD­qD��qD�-qD�mqDíqD��qD�-qD�mqDĭqD��qD�-qD�mqDŭqD��qD�-qD�mqDƭqD��qD�-qD�mqDǭqD��qD�-qD�mqDȭqD��qD�-qD�mqDɭqD��qD�-qD�mqDʭqD��qD�-qD�mqD˭qD��qD�-qD�mqḒqD��qD�-qD�mqDͭqD��qD�-qD�mqDέqD��qD�-qD�mqDϭqD��qD�-qD�mqDЭqD��qD�-qD�mqDѭqD��qD�-qD�mqDҭqD��qD�-qD�mqDӭqD��qD�-qD�mqDԭqD��qD�-qD�mqDխqD��qD�-qD�mqD֭qD��qD�-qD�mqD׭qD��qD�-qD�mqDحqD��qD�-qD�mqD٭qD��qD�-qD�mqDڭqD��qD�-qD�mqDۭqD��qD�-qD�mqDܭqD��qD�-qD�mqDݭqD��qD�-qD�mqDޭqD��qD�-qD�mqD߭qD��qD�-qD�mqD�qD��qD�-qD�mqD�qD��qD�-qD�mqD�qD��qD�-qD�mqD�qD��qD�-qD�mqD�qD��qD�-qD�mqD�qD��qD�-qD�mqD�qD��qD�-qD�mqD�qD��qD�-qD�mqD�qD��qD�-qD�mqD�qD��qD�-qD�mqD�qD��qD�-qD�mqD�qD��qD�-qD�mqD�qD��qD�-qD�mqD��qD��qD�-qD�mqD�qD��qD�-qD�mqD�qD��qD�-qD�mqD�qD��qD�-qD�mqD�qD��qD�-qD�mqD�qD��qD�-qD�mqD�qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��D�0�D�c�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��A��A��#A��#A��#A��A���A���AîAå�AÓuAÙ�Aã�Að!A�ĜA���A��A�G�A�"�A��A���A��A§�A�hsA�1A��!A�$�A���A�A���A�G�A�O�A�A�A�A��jA�ƨA��-A��`A�A�A�~�A���A�C�A�p�A���A��A��-A��A��!A��A��9A�x�A��
A�9XA�bNA�-A��A�  A��-A��A�33A��-A���A��A��A���A�33A��PA�{A�jA�jA��A�VA��;A�ƨA�ffA�l�A�  A�VA��`A�x�A��FA�&�A���A�XA�XA��A�t�A��uA��
A�$�A���A�mA{&�Az1Ay"�Aw�Av(�AtM�As33ArM�Ao�Am;dAlE�Ak�wAi�FAghsAedZAdVAa�7A`9XA_7LA]�
A[p�AX�jAW�AS��AQ�APjAO�mAPbAP9XAOANM�AK��AJ1AHVAE�TAE�hAE;dAD��AC�AB~�A@��A@{A>VA=��A=�PA<�A9�A8-A7�wA6�A5��A5�A5hsA4�A3�A1�#A0��A0VA/��A/A,�`A+O�A)��A(��A(�!A(ffA'�hA&ffA%�FA%�A$�\A$-A"�A!��A!p�A �\A��At�A(�A+A{A��AbNA��A�\A��A&�AI�AA"�A{A\)A1A7LA�9Ax�A�`AjA�A�FA
�jA	��AM�A�A7LA9XA{A�PA��A�At�A�A �+@�=q@�S�@���@��@�S�@��7@�r�@�@��@�-@�1'@�t�@�n�@�J@�hs@���@�Z@�K�@�ȴ@�h@�A�@�K�@�h@�(�@���@�5?@��@���@�+@�J@�7L@�  @�l�@�5?@�(�@�33@�O�@�dZ@�M�@���@��@���@���@�O�@˾w@�~�@�Ĝ@ƸR@�J@�%@�Q�@�j@�(�@���@��@��-@���@��m@��@�5?@�`B@��u@���@���@�"�@��T@�7L@�j@�K�@���@���@���@���@���@�I�@��@���@��7@��D@��@�v�@��T@�/@�  @�
=@���@�@��@��@� �@�ƨ@�|�@���@���@��h@��@�1'@���@�"�@���@�M�@���@�%@���@�Z@��m@���@�|�@�dZ@�K�@�;d@�"�@���@�~�@��@��@���@�?}@���@�Z@�9X@��m@���@��y@��!@�^5@�@��#@��-@�p�@�/@���@�Ĝ@��D@�9X@�  @��@�S�@��@�{@��h@�7L@�Ĝ@�z�@�I�@�1@���@�o@��!@�n�@�n�@�$�@��^@�p�@�?}@�/@��@���@�I�@�9X@��
@�ƨ@��w@��F@�|�@�;d@��H@�ȴ@��!@�ff@�5?@��@�@�hs@��@�Ĝ@��@�(�@�1@��@�t�@�dZ@�C�@��@�@�@���@���@���@�ȴ@���@���@�v�@�E�@��@��^@���@���@��-@��7@�`B@�G�@�p�@�G�@�G�@�%@�%@��@��@��/@���@�Q�@��;@�1@���@���@�+@�o@��y@���@�~�@�V@�-@�@���@�hs@��@��`@���@��@��/@��/@��j@�z�@�r�@�j@�Z@�I�@� �@���@���@��@�@���@�@��@��H@�v�@�E�@��@�@�@��-@���@�O�@���@��D@�I�@�(�@�1@�;@��@�w@��@;d@
=@~��@~@}�T@}�-@}?}@|�/@|Z@{�F@{S�@{"�@{@z��@zM�@zJ@y�@y��@y��@y�@y�7@yG�@yG�@yG�@yX@y&�@x��@x�@xb@wK�@v�@v@uV@t�j@tz�@tI�@t1@s�
@s��@sS�@so@r�@r~�@r-@rJ@q�@q��@qx�@p�`@pĜ@p�9@pbN@p �@o�@o��@o�P@oK�@o
=@nȴ@nv�@m�T@mO�@mV@mV@l��@l��@l��@l�@l(�@k�
@kƨ@kƨ@kƨ@kƨ@kƨ@k��@kt�@k"�@j��@j�@i��@iX@iX@iG�@i�@h�u@hb@g��@g�P@g�P@gK�@g�@g
=@g�@g|�@g|�@gl�@g\)@g|�@gK�@g+@f�+@fV@f�+@fV@e��@e��@ep�@e?}@e�@d9X@cC�@b�!@bn�@`��@_�;@_|�@_��@`bN@_��@^�R@^E�@^V@^ff@]�@]��@]O�@\��@\�/@\�D@\(�@[�m@[��@[t�@Z��@Z~�@Z�\@ZJ@Yhs@Y7L@Y7L@Y%@X��@XQ�@W��@W
=@V��@V�+@V$�@U?}@U�@T�j@T�@TZ@S��@St�@S33@R�@R�!@R�!@R�\@RJ@Q&�@P�`@P�@P  @O��@OK�@O�@N�R@Nff@M�@M��@M��@M`B@MV@L�@Lj@L1@K�F@K��@K��@K��@K�@Kt�@KC�@K"�@J�@J��@J~�@J=q@JJ@I��@IX@I�@H�u@H  @G�w@G
=@FE�@F$�@E�-@Ep�@D�j@Dj@DI�@C��@CC�@B��@B=q@A�@A��@A7L@A�@A%@@�`@@��@@Ĝ@@Ĝ@@�9@@A�@@b@?�;@?�@?|�@?\)@>ȴ@>ff@>V@>5?@>{@>@=�T@=��@=�-@=�@=V@<�/@<��@<9X@;�m@;�m@;�F@;C�@:�H@:n�@:�@9��@9�^@9x�@97L@9%@8��@81'@8  @8  @7�@7�w@7�@7��@7\)@6��@6��@6��@65?@5�T@5p�@4��@4�/@4�j@4�D@4�@3��@3�F@3"�@2�\@2=q@2�@2J@2J@1��@1�^@1�7@1G�@1&�@0��@0�@0bN@0Q�@0 �@/��@/�@/�@/��@/�P@/l�@/
=@.�@.ȴ@.�+@.ff@.E�@.5?@.{@-�@-O�@-?}@,��@,�j@,�D@,z�@,I�@+��@+ƨ@+�F@+�F@+�F@+�F@+�F@+��@+t�@+dZ@+C�@*��@*�\@*~�@*M�@)�@)�@)�#@)�@)�^@)x�@)hs@)&�@(Ĝ@(�9@(��@(�@(bN@(Q�@(1'@'�@'�w@'�@'�@'��@'l�@'K�@'�@&��@&V@%�T@%��@%O�@%�@$��@$z�@$Z@$�@#ƨ@#��@#t�@#dZ@#33@"��@"~�@"n�@!��@!�^@!��@!hs@!G�@!&�@!�@!%@ ��@ �`@ Ĝ@ �9@ �u@ r�@ bN@ A�@ b@�@��@�@|�@;d@�y@ȴ@�R@v�@V@E�@E�@$�@@�@@�-@�@`B@O�@�@�@�@�@�D@j@9X@1@�F@S�@"�@��@�\@~�@M�@��@�7@x�@hs@hs@hs@hs@7L@�`@Ĝ@��@�u@�@r�@ �@  @�@|�@+@�y@ȴ@��@��@�@ȴ@ff@$�@@�T@��@��@�T@�T@��@�h@��@`B@?}@�/@�@�@�D@j@(�@1@�m@ƨ@��@�@t�@33@@�\@^5@M�@�@�^@�7@hs@G�@%@�`@�`@��@�9@�u@�u@r�@bN@A�@��@��@�P@|�@+@
=@
=@��@�@��@ff@V@E�@5?@��@�h@��@�h@�@p�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��A��A��#A��#A��#A��A���A���AîAå�AÓuAÙ�Aã�Að!A�ĜA���A��A�G�A�"�A��A���A��A§�A�hsA�1A��!A�$�A���A�A���A�G�A�O�A�A�A�A��jA�ƨA��-A��`A�A�A�~�A���A�C�A�p�A���A��A��-A��A��!A��A��9A�x�A��
A�9XA�bNA�-A��A�  A��-A��A�33A��-A���A��A��A���A�33A��PA�{A�jA�jA��A�VA��;A�ƨA�ffA�l�A�  A�VA��`A�x�A��FA�&�A���A�XA�XA��A�t�A��uA��
A�$�A���A�mA{&�Az1Ay"�Aw�Av(�AtM�As33ArM�Ao�Am;dAlE�Ak�wAi�FAghsAedZAdVAa�7A`9XA_7LA]�
A[p�AX�jAW�AS��AQ�APjAO�mAPbAP9XAOANM�AK��AJ1AHVAE�TAE�hAE;dAD��AC�AB~�A@��A@{A>VA=��A=�PA<�A9�A8-A7�wA6�A5��A5�A5hsA4�A3�A1�#A0��A0VA/��A/A,�`A+O�A)��A(��A(�!A(ffA'�hA&ffA%�FA%�A$�\A$-A"�A!��A!p�A �\A��At�A(�A+A{A��AbNA��A�\A��A&�AI�AA"�A{A\)A1A7LA�9Ax�A�`AjA�A�FA
�jA	��AM�A�A7LA9XA{A�PA��A�At�A�A �+@�=q@�S�@���@��@�S�@��7@�r�@�@��@�-@�1'@�t�@�n�@�J@�hs@���@�Z@�K�@�ȴ@�h@�A�@�K�@�h@�(�@���@�5?@��@���@�+@�J@�7L@�  @�l�@�5?@�(�@�33@�O�@�dZ@�M�@���@��@���@���@�O�@˾w@�~�@�Ĝ@ƸR@�J@�%@�Q�@�j@�(�@���@��@��-@���@��m@��@�5?@�`B@��u@���@���@�"�@��T@�7L@�j@�K�@���@���@���@���@���@�I�@��@���@��7@��D@��@�v�@��T@�/@�  @�
=@���@�@��@��@� �@�ƨ@�|�@���@���@��h@��@�1'@���@�"�@���@�M�@���@�%@���@�Z@��m@���@�|�@�dZ@�K�@�;d@�"�@���@�~�@��@��@���@�?}@���@�Z@�9X@��m@���@��y@��!@�^5@�@��#@��-@�p�@�/@���@�Ĝ@��D@�9X@�  @��@�S�@��@�{@��h@�7L@�Ĝ@�z�@�I�@�1@���@�o@��!@�n�@�n�@�$�@��^@�p�@�?}@�/@��@���@�I�@�9X@��
@�ƨ@��w@��F@�|�@�;d@��H@�ȴ@��!@�ff@�5?@��@�@�hs@��@�Ĝ@��@�(�@�1@��@�t�@�dZ@�C�@��@�@�@���@���@���@�ȴ@���@���@�v�@�E�@��@��^@���@���@��-@��7@�`B@�G�@�p�@�G�@�G�@�%@�%@��@��@��/@���@�Q�@��;@�1@���@���@�+@�o@��y@���@�~�@�V@�-@�@���@�hs@��@��`@���@��@��/@��/@��j@�z�@�r�@�j@�Z@�I�@� �@���@���@��@�@���@�@��@��H@�v�@�E�@��@�@�@��-@���@�O�@���@��D@�I�@�(�@�1@�;@��@�w@��@;d@
=@~��@~@}�T@}�-@}?}@|�/@|Z@{�F@{S�@{"�@{@z��@zM�@zJ@y�@y��@y��@y�@y�7@yG�@yG�@yG�@yX@y&�@x��@x�@xb@wK�@v�@v@uV@t�j@tz�@tI�@t1@s�
@s��@sS�@so@r�@r~�@r-@rJ@q�@q��@qx�@p�`@pĜ@p�9@pbN@p �@o�@o��@o�P@oK�@o
=@nȴ@nv�@m�T@mO�@mV@mV@l��@l��@l��@l�@l(�@k�
@kƨ@kƨ@kƨ@kƨ@kƨ@k��@kt�@k"�@j��@j�@i��@iX@iX@iG�@i�@h�u@hb@g��@g�P@g�P@gK�@g�@g
=@g�@g|�@g|�@gl�@g\)@g|�@gK�@g+@f�+@fV@f�+@fV@e��@e��@ep�@e?}@e�@d9X@cC�@b�!@bn�@`��@_�;@_|�@_��@`bN@_��@^�R@^E�@^V@^ff@]�@]��@]O�@\��@\�/@\�D@\(�@[�m@[��@[t�@Z��@Z~�@Z�\@ZJ@Yhs@Y7L@Y7L@Y%@X��@XQ�@W��@W
=@V��@V�+@V$�@U?}@U�@T�j@T�@TZ@S��@St�@S33@R�@R�!@R�!@R�\@RJ@Q&�@P�`@P�@P  @O��@OK�@O�@N�R@Nff@M�@M��@M��@M`B@MV@L�@Lj@L1@K�F@K��@K��@K��@K�@Kt�@KC�@K"�@J�@J��@J~�@J=q@JJ@I��@IX@I�@H�u@H  @G�w@G
=@FE�@F$�@E�-@Ep�@D�j@Dj@DI�@C��@CC�@B��@B=q@A�@A��@A7L@A�@A%@@�`@@��@@Ĝ@@Ĝ@@�9@@A�@@b@?�;@?�@?|�@?\)@>ȴ@>ff@>V@>5?@>{@>@=�T@=��@=�-@=�@=V@<�/@<��@<9X@;�m@;�m@;�F@;C�@:�H@:n�@:�@9��@9�^@9x�@97L@9%@8��@81'@8  @8  @7�@7�w@7�@7��@7\)@6��@6��@6��@65?@5�T@5p�@4��@4�/@4�j@4�D@4�@3��@3�F@3"�@2�\@2=q@2�@2J@2J@1��@1�^@1�7@1G�@1&�@0��@0�@0bN@0Q�@0 �@/��@/�@/�@/��@/�P@/l�@/
=@.�@.ȴ@.�+@.ff@.E�@.5?@.{@-�@-O�@-?}@,��@,�j@,�D@,z�@,I�@+��@+ƨ@+�F@+�F@+�F@+�F@+�F@+��@+t�@+dZ@+C�@*��@*�\@*~�@*M�@)�@)�@)�#@)�@)�^@)x�@)hs@)&�@(Ĝ@(�9@(��@(�@(bN@(Q�@(1'@'�@'�w@'�@'�@'��@'l�@'K�@'�@&��@&V@%�T@%��@%O�@%�@$��@$z�@$Z@$�@#ƨ@#��@#t�@#dZ@#33@"��@"~�@"n�@!��@!�^@!��@!hs@!G�@!&�@!�@!%@ ��@ �`@ Ĝ@ �9@ �u@ r�@ bN@ A�@ b@�@��@�@|�@;d@�y@ȴ@�R@v�@V@E�@E�@$�@@�@@�-@�@`B@O�@�@�@�@�@�D@j@9X@1@�F@S�@"�@��@�\@~�@M�@��@�7@x�@hs@hs@hs@hs@7L@�`@Ĝ@��@�u@�@r�@ �@  @�@|�@+@�y@ȴ@��@��@�@ȴ@ff@$�@@�T@��@��@�T@�T@��@�h@��@`B@?}@�/@�@�@�D@j@(�@1@�m@ƨ@��@�@t�@33@@�\@^5@M�@�@�^@�7@hs@G�@%@�`@�`@��@�9@�u@�u@r�@bN@A�@��@��@�P@|�@+@
=@
=@��@�@��@ff@V@E�@5?@��@�h@��@�h@�@p�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�ZB�ZB�ZB�ZB�ZB�ZB�`B�`B�`B�fB�fB�mB�fB�mB�sB�yB�sB�sB�yB�B�B��BPB�B$�B&�B.B6FB>wBK�BR�Bu�B�jBŢBŢB�B��B��B��B��B��B�?B�ZB�B�B�BB�RB��B�\B�BffBH�B6FB&�B\BB��B�B�B��BƨBB�qB�'B��B�uB�PB�B}�Bx�BhsBcTBW
B1'B{BB�B�BĜB��B��B�bB�Bs�B\)BR�BK�B;dB8RB;dB@�B=qB8RB �BB
��B
�B
�HB
��B
�jB
�B
��B
�PB
}�B
_;B
A�B
�B
\B
+B	��B	�B	�TB	�/B	�B	ŢB	�9B	��B	��B	�{B	�B	s�B	jB	\)B	P�B	G�B	9XB	%�B	oB��B�`B�;B�)B�#B�TB�B�B�mB�5B��B��BĜBɺBɺBƨBŢBƨBĜB�}B�RB�FB�?B�LB�-B��B��B��B��B��B��B��B��B��B��B��B��B��B�hB�7B�B}�B}�B�B�B~�B|�By�Bw�Bt�Bp�Bn�Bl�BjBgmBe`BbNB^5B[#BYBVBR�BO�BN�BM�BK�BJ�BH�BE�BB�B<jB9XB:^B49B:^BA�B<jB5?B/B,B/B1'B0!B1'B0!B/B.B-B+B'�B$�B%�B'�B&�B%�B%�B$�B#�B#�B$�B(�B(�B)�B+B+B+B+B)�B)�B)�B+B)�B)�B'�B'�B(�B(�B(�B'�B(�B)�B)�B+B)�B+B.B/B,B,B-B-B/B2-B33B33B6FB5?B33B33B5?B9XB:^B<jB>wB>wB>wBA�BB�BA�BA�BB�BC�BE�BF�BF�BG�BI�BI�BK�BM�BN�BN�BP�BR�BS�BS�BW
BYB\)B_;B`BBcTBdZBffBjBn�Bo�Br�Bv�Bw�Bx�Bx�Bw�Bz�B}�B}�B�B�%B�1B�DB�PB�\B�oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�'B�9B�XB�^B�dB�qB�wB�}B��BBĜBƨBȴB��B��B��B�B�B�)B�5B�BB�TB�mB�sB�B�B�B��B��B��B��B��B��B	  B	  B	B	B	+B	1B	PB	\B	\B	bB	oB	�B	�B	�B	�B	�B	 �B	#�B	&�B	+B	/B	33B	6FB	<jB	=qB	@�B	C�B	F�B	I�B	L�B	M�B	O�B	S�B	W
B	[#B	]/B	^5B	`BB	bNB	cTB	e`B	hsB	m�B	q�B	s�B	s�B	s�B	v�B	x�B	z�B	}�B	�B	�+B	�7B	�7B	�DB	�DB	�VB	�bB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�3B	�?B	�FB	�LB	�RB	�RB	�XB	�^B	�dB	�jB	�qB	�qB	�}B	��B	��B	ÖB	ŢB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�B	�B	�B	�B	�#B	�#B	�)B	�5B	�5B	�;B	�;B	�BB	�HB	�NB	�TB	�TB	�TB	�ZB	�`B	�fB	�fB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
+B
+B
+B
1B
	7B

=B
DB
JB
JB
PB
VB
VB
\B
oB
uB
uB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 �B
!�B
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
&�B
&�B
%�B
&�B
&�B
'�B
'�B
(�B
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
,B
,B
,B
-B
-B
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
/B
/B
/B
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
1'B
2-B
2-B
33B
33B
2-B
33B
33B
33B
33B
33B
33B
49B
5?B
5?B
5?B
6FB
6FB
6FB
6FB
6FB
6FB
7LB
6FB
6FB
7LB
7LB
7LB
7LB
8RB
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
:^B
:^B
;dB
<jB
<jB
<jB
<jB
<jB
<jB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
>wB
>wB
>wB
>wB
>wB
>wB
?}B
?}B
?}B
?}B
?}B
@�B
@�B
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
B�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
E�B
F�B
E�B
F�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
H�B
I�B
I�B
J�B
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
P�B
Q�B
P�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
S�B
T�B
T�B
VB
VB
VB
VB
VB
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
ZB
ZB
ZB
ZB
[#B
[#B
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
\)B
\)B
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
^5B
^5B
_;B
_;B
_;B
_;B
_;B
_;B
`BB
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
cTB
cTB
dZB
dZB
dZB
dZB
dZB
cTB
cTB
cTB
cTB
dZB
e`B
e`B
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
gmB
gmB
gmB
gmB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
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
l�B
m�B
m�B
m�B
m�B
n�B
o�B
o�B
o�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
p�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�}B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B�B�AB�BB$�B&�B.B6)B>oBK�BQ�Br�B��B�B��B��B�B�sB�QB��B�/B��B�%B��B�B�yB��B�\B��B��Bp�BOB<�B18B�B�B�BB�lB�tB�QB�B�EBÅB�!B��B��B�>B�6BRB}KBjCBf�B_|B8+B�BPB�!BܵB��B�B�B��B�By�B_�BUTBQ6B=OB8�B=�BC�B@B?�B)�BB
��B
�DB
��B
�cB
��B
��B
��B
�B
��B
g,B
LYB
iB
�B
	�B
 �B	�uB	�B	�XB	��B	�uB	�xB	�kB	��B	��B	�fB	vfB	p�B	_RB	SiB	KB	>�B	,bB	{B	LB�^B�BݞB� B�<B�B�RB��B�BقB��BťB��B�]B��BɯB�3B��B��B��B�XB�cB��B��B�hB�CB��B��B�BB�XB��B�B�GB��B��B��B�kB��B�B��B~�BB��B�bB�B~�B{�By<Bx�Bs;Bo�BoBBlZBiABiqBeFBa�B^xB[BXBV�BR-BQBP�BM�BL�BK�BG�BF>B>�B:�B=�B5�B;�BD�B?NB7�B1�B/B0�B2{B2�B1�B1�B18B/�B.�B.�B+�B(�B)�B+8B(�B&�B(WB&�B%4B$�B&�B+7B*GB+�B+�B,&B+�B,)B+�B+B, B-5B+�B,�B*5B*B*B*B*5B*�B*�B+jB+�B,B+�B-�B/�B1�B.�B-�B-�B.�B2IB3�B4dB5IB7�B7B5VB4)B6�B:YB:�B<�B?$B?�B@%BB�BC�BCBB�BC�BD�BF�BGpBG�BI�BJ�BKBMsBN�BO`BP=BRxBS�BT�BU<BX�BZ�B]�B`�Bb!BdZBe�Bh?Bl BouBp�BtfBw�Bx�By�By�ByXB|BB~�B~�B��B�:B�?B�B�eB��B��B�'B��B��B�mB�#B�$B�%B�B�5B��B��B��B�~B��B��B��B�B��B��B��B��B��B�!B�8B��B�B�+B�6B�/B�4B�TB�{B�sB�\B�B��B�3B�B��B�B��B��B�	B�NB�B�bB�WB�B��B��B��B�}B	 aB	 lB	�B	�B	�B	�B	�B	�B	�B	�B	B	?B	B	B	QB	BB	!hB	$bB	'�B	+�B	/�B	3�B	7B	<�B	>3B	AB	C�B	G"B	J;B	M>B	N B	P�B	TdB	W5B	[eB	]�B	^�B	`�B	b�B	dB	e�B	h�B	m�B	q�B	t;B	t<B	t B	v�B	yXB	{6B	~�B	�^B	��B	��B	��B	��B	�?B	�?B	�vB	��B	�EB	��B	�B	�HB	��B	�lB	�kB	�lB	�vB	��B	��B	��B	��B	�;B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�8B	�B	�rB	��B	��B	��B	��B	�EB	ȜB	�KB	ʁB	�UB	�B	�&B	�4B	ͥB	��B	͌B	ΌB	�[B	�^B	�VB	�FB	�HB	�WB	ՂB	�uB	כB	ٵB	ڂB	ڇB	گB	۫B	��B	��B	��B	ޡB	ߞB	��B	��B	��B	�B	�B	�B	�B	��B	��B	�B	�B	�B	��B	�	B	�B	�-B	�gB	�,B	�mB	�B	�B	�B	�B	�B	�	B	�B	�!B	�B	�B	�CB	�2B	�B	�B	�B	�?B	�jB	�B	�B	�CB	�>B	�6B	�,B	�AB	�GB	�QB	�PB	�eB	��B	��B	�`B	�6B	�AB	�4B	�6B	�vB	��B	��B	�OB
 NB
SB
YB
\B
zB
�B
�B
�B
�B
�B
�B
|B
�B
�B
�B
	�B

�B
�B
�B
�B
�B
�B
�B
bB
�B
�B
�B
�B
�B
�B
PB
�B
�B
B
NB

B
B
B
B
�B
�B
^B
+B
B
�B
8B
�B
lB
 �B
�B
TB
�B
 B
 dB
 QB
 KB
 EB
 "B
 =B
 DB
 0B
 4B
!2B
!�B
!:B
"B
#pB
#�B
$FB
$#B
$AB
%nB
%aB
%�B
%�B
%6B
&�B
'{B
'�B
&GB
'{B
'GB
(vB
(�B
)�B
)pB
)qB
)mB
)BB
)^B
)�B
*�B
*wB
+�B
+�B
+sB
,�B
,yB
,�B
-�B
-�B
.yB
.�B
.�B
.�B
.|B
.�B
/�B
/�B
/B
/fB
/kB
/tB
/xB
/�B
/�B
/�B
0�B
0�B
0�B
0�B
0�B
0�B
1�B
1�B
1�B
2�B
3	B
4B
3�B
2�B
3�B
4B
3�B
3�B
3�B
4B
5B
5�B
5�B
5�B
6�B
6�B
6�B
6�B
6�B
6�B
7�B
6�B
6�B
7�B
7�B
7�B
7�B
8�B
7�B
8�B
8�B
8�B
9�B
9�B
9�B
9�B
9�B
9�B
9�B
:�B
:�B
;�B
<�B
<�B
<�B
<�B
<�B
=B
=�B
=�B
=�B
=�B
=�B
=�B
>B
?B
>�B
>�B
>�B
>�B
>�B
?�B
?�B
@	B
@ B
?�B
AB
AB
AB
AB
@�B
@�B
@�B
A#B
A�B
B	B
B=B
BBB
BB
@�B
@�B
@�B
@�B
@�B
A�B
B B
A�B
A�B
B+B
B�B
B�B
CB
CB
C�B
C�B
C�B
C�B
C�B
D,B
DB
C�B
DB
C�B
EB
D�B
EB
F`B
GB
E�B
G'B
G)B
GB
GB
GB
G6B
H!B
HB
IB
JB
JB
KB
L!B
L7B
M(B
M8B
MxB
MKB
M(B
NLB
NkB
N"B
N+B
OB
OOB
P`B
P<B
PbB
P~B
Q>B
QBB
QNB
QMB
RCB
QJB
RbB
RXB
RBB
S<B
SJB
S]B
SSB
S_B
S�B
TyB
U�B
UvB
V�B
VnB
V�B
V�B
VjB
W�B
W�B
WnB
X~B
XdB
X|B
X�B
X�B
XjB
Y�B
Y�B
YpB
Y�B
ZB
ZB
ZrB
ZwB
[zB
[xB
[�B
[vB
[�B
[�B
[yB
[�B
[�B
\�B
\�B
\�B
\�B
\�B
\�B
\�B
\�B
\�B
\�B
\�B
\tB
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
_�B
_�B
_�B
_�B
_�B
_�B
`�B
a�B
a�B
a�B
a�B
a�B
b�B
b�B
b�B
b�B
b�B
b�B
b�B
b�B
b�B
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
c�B
c�B
c�B
c�B
dB
e�B
e�B
d�B
d�B
d�B
d�B
d�B
e�B
e�B
e�B
e�B
e�B
e�B
e�B
e�B
e�B
e�B
e�B
e�B
f�B
f�B
f�B
f�B
f�B
f�B
g�B
g�B
g�B
hB
i�B
i�B
jB
i�B
i�B
i�B
i�B
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
m8B
nB
m�B
m�B
n#B
o B
o�B
o�B
pB
o B
oB
n�B
o�B
o�B
p0B
pB
o�B
o�B
p�B
p�B
p�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<6ћ<#�
<#�
<#�
<#�
<7D<#�
<#�
<8)6<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<&+<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<?j]<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - (REPORTED_SURFACE_PRESSURE) for PADJ                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al,2007,JAOT & effects of pressure adjustments                                                                                                                                                            PADJ REPORTED_SURFACE_PRESSURE =0.58 dbar                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CTM alpha = 0.141 & tau = 6.68 s with error equal to the correction                                                                                                                                                                                             Pressures adjusted using reported surface pressure when warranted.                                                                                                                                                                                              none                                                                                                                                                                                                                                                            Salinity corrected for pressure sensor calibration drift and cell thermal mass                                                                                                                                                                                  201603101135322016031011353220160310113532  AO  ARCAADJP                                                                    20141226233138    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20141226233138  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20141226233138  QCF$                G�O�G�O�G�O�0                                                                                                                                   G�O�G�O�G�O�                PM  ARSQPADJV1.1                                                                20160310113532  QC  PRES            @�  D�vfG�O�                PM  ARSQCTM V1.1                                                                20160310113532  QC  PSAL            @�  D�vfG�O�                PM  ARSQOWGUV1.0WOD + Argo                                                      20170523133352  IP                  G�O�G�O�G�O�                