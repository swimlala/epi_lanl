CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       d2014-11-26T23:01:57Z creation; 2014-11-26T23:01:57Z updated; 2015-09-28T12:13:17Z converted from 3.0   
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
resolution        =���   axis      Z        P  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     P  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     P  _�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     P  o@   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ~�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     P  �d   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     P  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     P  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �(   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     P  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �L   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     P  �    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �p   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ڠ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ݠ   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �,   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �0   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �@   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �D   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �H   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �LArgo profile    3.1 1.2 19500101000000  20141126230157  20170523133351  5903863 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               iA   AO  4298_0127_105                   2C  D   NAVIS_A                         0127                            120111                          863 @�&a�n�1   @�&b�@ @8>�Q��c=$�/1   GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                      iA   A   A   @���@���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  BpffBx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� DsfDs� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@tz�@�=q@�p�A�RA6�RAV�RAv�RA�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)B�B�B�B�B%�B-�B5�B=�BE�BM�BU�B]�Be�BnzBu�B}�B��
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
Ck�Ck�Ck�Ck�C	k�Ck�Ck�Ck�Ck�Ck�Ck�Ck�Ck�Ck�Ck�Ck�C!k�C#k�C%k�C'k�C)k�C+k�C-k�C/k�C1k�C3k�C5k�C7k�C9k�C;k�C=k�C?k�CAk�CCk�CEk�CGk�CIk�CKk�CMk�COk�CQk�CSk�CUk�CWk�CYk�C[k�C]k�C_k�Cak�Cck�Cek�Cgk�Cik�Ckk�Cmk�Cok�Cqk�Csk�Cuk�Cwk�Cyk�C{k�C}k�Ck�C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���Cµ�Cõ�Cĵ�Cŵ�CƵ�Cǵ�Cȵ�Cɵ�Cʨ�C˵�C̵�C͵�Cε�Cϵ�Cе�Cѵ�Cҵ�Cӵ�CԵ�Cյ�Cֵ�C׵�Cص�Cٵ�Cڵ�C۵�Cܵ�Cݵ�C޵�Cߵ�C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C���C���C���C���C���C���C���C���C���C���C���C���D Z�D ��DZ�D��DZ�D��DZ�D��DZ�D��DZ�D��DZ�D��DZ�D��DZ�D��D	Z�D	��D
Z�D
��DZ�D��DZ�D��DZ�D��DZ�D��DZ�D��DZ�D��DZ�D��DZ�D��DZ�D��DZ�D��DZ�D��DZ�D��DZ�D��DZ�D��DZ�D��DZ�D��DZ�D��DZ�D��DZ�D��DZ�D��DZ�D��D Z�D ��D!Z�D!��D"Z�D"��D#Z�D#��D$Z�D$��D%Z�D%��D&Z�D&��D'Z�D'��D(Z�D(��D)Z�D)��D*Z�D*��D+Z�D+��D,Z�D,��D-Z�D-��D.Z�D.��D/Z�D/��D0Z�D0��D1Z�D1��D2Z�D2��D3Z�D3��D4Z�D4��D5Z�D5��D6Z�D6��D7Z�D7��D8Z�D8��D9Z�D9��D:Z�D:��D;Z�D;��D<Z�D<��D=Z�D=��D>Z�D>��D?Z�D?��D@Z�D@��DAZ�DA��DBZ�DB��DCZ�DC��DDZ�DD��DEZ�DE��DFZ�DF��DGZ�DG��DHZ�DH��DIZ�DI��DJZ�DJ��DKZ�DK��DLZ�DL��DMZ�DM��DNZ�DN��DOZ�DO��DPZ�DP��DQZ�DQ��DRZ�DR��DSZ�DS��DTZ�DT��DUZ�DU��DVZ�DV��DWZ�DW��DXZ�DX��DYZ�DY��DZZ�DZ��D[Z�D[��D\Z�D\��D]Z�D]��D^Z�D^��D_Z�D_��D`Z�D`��DaZ�Da��DbZ�Db��DcZ�Dc��DdZ�Dd��DeZ�De��DfZ�Df��DgZ�Dg��DhZ�Dh��DiZ�Di��DjZ�Dj��DkZ�Dk��DlZ�Dl��DmZ�Dm��DnZ�Dn��DoZ�Do��DpZ�Dp��DqZ�Dq��DrZ�Dr�GDsZ�Ds��DtZ�Dt��DuZ�Du��DvZ�Dv��DwZ�Dw��DxZ�Dx��DyZ�Dy��DzZ�Dz��D{Z�D{��D|Z�D|��D}Z�D}��D~Z�D~��DZ�D��D�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD��qD��qD�-qD�mqD­qD��qD�-qD�mqDíqD��qD�-qD�mqDĭqD��qD�-qD�mqDŭqD��qD�-qD�mqDƭqD��qD�-qD�mqDǭqD��qD�-qD�mqDȭqD��qD�-qD�mqDɭqD��qD�-qD�mqDʭqD��qD�-qD�mqD˭qD��qD�-qD�mqḒqD��qD�-qD�mqDͭqD��qD�-qD�mqDέqD��qD�-qD�mqDϭqD��qD�-qD�mqDЭqD��qD�-qD�mqDѭqD��qD�-qD�mqDҭqD��qD�-qD�mqDӭqD��qD�-qD�mqDԭqD��qD�-qD�mqDխqD��qD�-qD�mqD֭qD��qD�-qD�mqD׭qD��qD�-qD�mqDحqD��qD�-qD�mqD٭qD��qD�-qD�mqDڭqD��qD�-qD�mqDۭqD��qD�-qD�mqDܭqD��qD�-qD�mqDݭqD��qD�-qD�mqDޭqD��qD�-qD�mqD߭qD��qD�-qD�mqD�qD��qD�-qD�mqD�qD��qD�-qD�mqD�qD��qD�-qD�mqD�qD��qD�-qD�mqD�qD��qD�-qD�mqD�qD��qD�-qD�mqD�qD��qD�-qD�mqD�qD��qD�-qD�mqD�qD��qD�-qD�mqD�qD��qD�-qD�mqD�qD��qD�-qD�mqD�qD��qD�-qD�mqD�qD��qD�-qD�mqD��qD��qD�-qD�mqD�qD��qD�-qD�mqD�qD��qD�-qD�mqD�qD��qD�-qD�mqD�qD��qD�-qD�mqD�qD��qD�-qD�mqD�qD��qD�-qD�mqD��qD��qD�0�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�(�A�(�A��A��A�$�A�1'A�1'A�1'A�-A�5?A�7LA�9XA�9XA�;dA�;dA�=qA�?}A�A�A�?}A�?}A�A�A�C�A�C�A�A�A�A�A�A�A�A�A�?}A�/A��mA�ffA�$�A��wA��A��!A�=qA�p�A��A� �A��#A���A��-A�I�A��+A��A�v�A��A���A�M�A�S�A���A���A�v�A�dZA��-A�ƨA�Q�A�1A��jA�ZA�XA��A�~�A�VA�ƨA�"�A��#A�-A�$�A�v�A�VA�r�A�t�A���A���A�jA��A��\A��uA��A�"�A�t�A�A�A�-A�ƨA�S�A�-A���A�/A��!A��A���A��A��TA33A}�
A{VAxI�Av~�At^5Ap��Ao+Am�Aj~�Ah�AhĜAh�Ah�uAg��Ad�A`��A^VA\E�A[&�AZ�AX�AU��AS�mAQ�APbNAOAM��AL�jAK�AJ=qAI�AH�RAGK�AE��AEXAD~�AC�wAB�RAB�AA�^A@ĜA?ƨA?l�A?\)A>5?A<�9A;hsA9��A8��A7�A6ȴA5�A5+A4��A4ZA3�
A2��A2{A1�PA1"�A0ĜA/�A.�yA.�uA.$�A-C�A,I�A+l�A*VA)��A)�7A(n�A'�A'x�A&��A%\)A#�mA!��A�A&�A�HAx�AI�AS�A/A��A{AdZA��A��A"�A��AXA��A�wA��A�^A��A�A�uAjA�A��A��A(�A
��A��A33A��AbNA�A|�A�A �A�-A��AE�A7LA b@���@��7@�dZ@���@��7@��@��`@��j@�j@��@�p�@�  @�~�@�^5@�1'@�M�@��T@�@�@�bN@�|�@�E�@�V@�w@��@�t�@�hs@���@ۮ@�V@ف@�Z@�33@֧�@ԋD@�V@��@��@�^5@�A�@ʗ�@�v�@��#@ȃ@Ǿw@�dZ@�$�@�Z@�^5@���@�  @�l�@�@�-@�`B@���@�Q�@�l�@�=q@��m@�
=@��@���@��7@���@�j@�Q�@�1'@�|�@�$�@�/@�&�@�+@���@�E�@��@��@�  @��u@�=q@���@��!@���@��u@��P@�ȴ@��@�33@�o@��H@���@�;d@���@�M�@�E�@�M�@�O�@��D@�z�@�t�@���@�-@���@�7L@��D@�1'@��;@��@���@�C�@���@���@�\)@�\)@�t�@�ff@�-@�-@�-@�&�@��@��
@��
@��m@�A�@�  @�C�@�
=@�ff@���@���@��h@��h@�&�@�G�@���@���@��@��7@��^@��-@���@��u@�ƨ@�@���@���@�-@�9X@��@��@�E�@�33@�@�K�@��D@���@�ȴ@�
=@��@�ff@��w@��T@�M�@�S�@��@���@���@���@�V@�ȴ@��P@�t�@�l�@�dZ@�|�@���@��@�"�@��y@��y@��y@��y@���@�^5@�M�@�M�@�E�@���@�1'@�A�@�%@���@��@�j@�I�@��@��@��;@��H@��#@�O�@���@���@�"�@��!@�M�@��@��j@��9@��9@�j@�r�@��@�I�@��
@��w@��@���@��;@��
@���@�t�@�K�@�+@�S�@�(�@���@��@��7@�hs@��/@��P@��#@��7@�E�@��7@�z�@�Z@�1'@��u@�j@��w@�ƨ@���@��F@���@���@�33@�~�@��+@��@���@�x�@�X@�&�@�%@�%@�Ĝ@���@��@�j@���@�bN@�1'@��@�ƨ@�|�@�S�@���@�E�@�$�@�$�@�$�@�=q@��@��@�V@���@�Z@�Q�@�(�@��@�w@�w@l�@~�@~�R@~v�@~�+@~v�@~��@
=@~ff@|(�@{C�@z��@z^5@z=q@y�#@yx�@y7L@xĜ@xr�@xbN@x1'@xb@x  @w�P@w
=@v��@v�R@vv�@v�+@v��@v$�@u�@t��@tZ@sƨ@s��@r��@r^5@q��@q%@q%@p�u@p1'@o�;@o�P@n��@n�@n�+@n{@m��@m�@m�@l1@k�m@k�F@k�@kC�@j��@i7L@hĜ@hb@g�w@g��@g��@g��@g|�@f��@fȴ@f�R@fV@e�h@e�h@e��@e��@e�@e?}@e�@eV@d�@d��@d��@dj@d(�@d1@cƨ@cdZ@co@b�H@b��@b�!@b��@b~�@b~�@b^5@a��@aX@`��@`bN@`A�@`1'@` �@_�@_�@^�+@^E�@]@]�h@]��@]�-@]�-@]��@]��@]p�@]V@\�@\�@\1@[��@[�m@[�F@[��@[33@Z�@Z��@Z��@Z��@Z~�@Zn�@Y��@Y7L@Y�@X��@X��@Xr�@X �@W�;@W��@W��@W;d@V�y@V�y@V�@Vȴ@V��@VV@V$�@U�-@U?}@Tz�@S��@SS�@R�H@R�!@RM�@Q��@QG�@P��@P��@P�9@P��@PA�@Pb@P  @O�;@O��@O�w@O|�@O+@N�@N$�@M��@N{@M��@L�@L�@Kƨ@K�@J�H@J�\@Jn�@J-@I�#@I�#@Ix�@H�9@Hb@H  @G��@G�P@Gl�@F��@F��@Fff@E��@E��@Ep�@E�@D��@D�@D��@D�j@D�j@D�@DZ@C�
@CS�@B��@B=q@BJ@A�7@A�@@��@@��@@A�@@ �@?�w@?��@?l�@?;d@>ȴ@>��@>��@>E�@>@=�-@=�@=�@=`B@=?}@=O�@=/@<�@<j@<9X@;�
@;o@:�@:�H@:��@:��@:�!@:�\@:M�@9�@9�^@9x�@9G�@9�@8Ĝ@8�u@8�u@8�@8bN@8bN@8A�@7�@7\)@7+@7
=@6�y@6ȴ@6��@6$�@5��@5`B@5�@4�j@4z�@41@3��@3C�@3o@2��@2��@2~�@2^5@2�@1x�@1X@17L@1�@0�`@0Ĝ@0r�@0Q�@0A�@01'@0b@0  @/�;@/��@/�@/|�@/�@.v�@.ff@.V@-�T@-`B@,�/@,�j@,�D@,I�@,�@+ƨ@+�@+@*��@*M�@*-@*�@)�^@)��@)hs@)G�@)%@(Ĝ@(��@(�@(Q�@(b@'�w@'�@'�P@'l�@'K�@&�@&��@&�+@&$�@%�@%��@%`B@%V@$��@$j@$Z@$(�@#�m@#�
@#�
@#�F@#��@#�@#t�@#S�@#33@"=q@!�#@!�7@!x�@!&�@ �u@ Q�@ 1'@  �@ b@   @   @�;@�w@|�@\)@K�@+@�@�@
=@��@E�@5?@5?@$�@�@��@�-@�h@�@��@�j@Z@��@�F@�@dZ@33@�@�\@~�@^5@��@��@x�@hs@&�@Ĝ@�@A�@1'@  @  @�@�;@�w@�P@�P@|�@\)@;d@�@
=@�y@�y@ȴ@ff@5?@@�T@��@��@�@`B@�@�@�/@�j@�D@j@I�@1@t�@��@n�@-@��@�#@��@x�@X@7L@&�@�@��@��@bN@�@�P@K�@+@�@�+@ff@��@�R@E�@5?@$�@$�@�T@�h@O�@�/@�@�D@z�@z�@I�@1@��@ƨ@��@�@C�@@
��@
�!@
~�@
^5@
=q@	��@	�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�(�A�(�A��A��A�$�A�1'A�1'A�1'A�-A�5?A�7LA�9XA�9XA�;dA�;dA�=qA�?}A�A�A�?}A�?}A�A�A�C�A�C�A�A�A�A�A�A�A�A�A�?}A�/A��mA�ffA�$�A��wA��A��!A�=qA�p�A��A� �A��#A���A��-A�I�A��+A��A�v�A��A���A�M�A�S�A���A���A�v�A�dZA��-A�ƨA�Q�A�1A��jA�ZA�XA��A�~�A�VA�ƨA�"�A��#A�-A�$�A�v�A�VA�r�A�t�A���A���A�jA��A��\A��uA��A�"�A�t�A�A�A�-A�ƨA�S�A�-A���A�/A��!A��A���A��A��TA33A}�
A{VAxI�Av~�At^5Ap��Ao+Am�Aj~�Ah�AhĜAh�Ah�uAg��Ad�A`��A^VA\E�A[&�AZ�AX�AU��AS�mAQ�APbNAOAM��AL�jAK�AJ=qAI�AH�RAGK�AE��AEXAD~�AC�wAB�RAB�AA�^A@ĜA?ƨA?l�A?\)A>5?A<�9A;hsA9��A8��A7�A6ȴA5�A5+A4��A4ZA3�
A2��A2{A1�PA1"�A0ĜA/�A.�yA.�uA.$�A-C�A,I�A+l�A*VA)��A)�7A(n�A'�A'x�A&��A%\)A#�mA!��A�A&�A�HAx�AI�AS�A/A��A{AdZA��A��A"�A��AXA��A�wA��A�^A��A�A�uAjA�A��A��A(�A
��A��A33A��AbNA�A|�A�A �A�-A��AE�A7LA b@���@��7@�dZ@���@��7@��@��`@��j@�j@��@�p�@�  @�~�@�^5@�1'@�M�@��T@�@�@�bN@�|�@�E�@�V@�w@��@�t�@�hs@���@ۮ@�V@ف@�Z@�33@֧�@ԋD@�V@��@��@�^5@�A�@ʗ�@�v�@��#@ȃ@Ǿw@�dZ@�$�@�Z@�^5@���@�  @�l�@�@�-@�`B@���@�Q�@�l�@�=q@��m@�
=@��@���@��7@���@�j@�Q�@�1'@�|�@�$�@�/@�&�@�+@���@�E�@��@��@�  @��u@�=q@���@��!@���@��u@��P@�ȴ@��@�33@�o@��H@���@�;d@���@�M�@�E�@�M�@�O�@��D@�z�@�t�@���@�-@���@�7L@��D@�1'@��;@��@���@�C�@���@���@�\)@�\)@�t�@�ff@�-@�-@�-@�&�@��@��
@��
@��m@�A�@�  @�C�@�
=@�ff@���@���@��h@��h@�&�@�G�@���@���@��@��7@��^@��-@���@��u@�ƨ@�@���@���@�-@�9X@��@��@�E�@�33@�@�K�@��D@���@�ȴ@�
=@��@�ff@��w@��T@�M�@�S�@��@���@���@���@�V@�ȴ@��P@�t�@�l�@�dZ@�|�@���@��@�"�@��y@��y@��y@��y@���@�^5@�M�@�M�@�E�@���@�1'@�A�@�%@���@��@�j@�I�@��@��@��;@��H@��#@�O�@���@���@�"�@��!@�M�@��@��j@��9@��9@�j@�r�@��@�I�@��
@��w@��@���@��;@��
@���@�t�@�K�@�+@�S�@�(�@���@��@��7@�hs@��/@��P@��#@��7@�E�@��7@�z�@�Z@�1'@��u@�j@��w@�ƨ@���@��F@���@���@�33@�~�@��+@��@���@�x�@�X@�&�@�%@�%@�Ĝ@���@��@�j@���@�bN@�1'@��@�ƨ@�|�@�S�@���@�E�@�$�@�$�@�$�@�=q@��@��@�V@���@�Z@�Q�@�(�@��@�w@�w@l�@~�@~�R@~v�@~�+@~v�@~��@
=@~ff@|(�@{C�@z��@z^5@z=q@y�#@yx�@y7L@xĜ@xr�@xbN@x1'@xb@x  @w�P@w
=@v��@v�R@vv�@v�+@v��@v$�@u�@t��@tZ@sƨ@s��@r��@r^5@q��@q%@q%@p�u@p1'@o�;@o�P@n��@n�@n�+@n{@m��@m�@m�@l1@k�m@k�F@k�@kC�@j��@i7L@hĜ@hb@g�w@g��@g��@g��@g|�@f��@fȴ@f�R@fV@e�h@e�h@e��@e��@e�@e?}@e�@eV@d�@d��@d��@dj@d(�@d1@cƨ@cdZ@co@b�H@b��@b�!@b��@b~�@b~�@b^5@a��@aX@`��@`bN@`A�@`1'@` �@_�@_�@^�+@^E�@]@]�h@]��@]�-@]�-@]��@]��@]p�@]V@\�@\�@\1@[��@[�m@[�F@[��@[33@Z�@Z��@Z��@Z��@Z~�@Zn�@Y��@Y7L@Y�@X��@X��@Xr�@X �@W�;@W��@W��@W;d@V�y@V�y@V�@Vȴ@V��@VV@V$�@U�-@U?}@Tz�@S��@SS�@R�H@R�!@RM�@Q��@QG�@P��@P��@P�9@P��@PA�@Pb@P  @O�;@O��@O�w@O|�@O+@N�@N$�@M��@N{@M��@L�@L�@Kƨ@K�@J�H@J�\@Jn�@J-@I�#@I�#@Ix�@H�9@Hb@H  @G��@G�P@Gl�@F��@F��@Fff@E��@E��@Ep�@E�@D��@D�@D��@D�j@D�j@D�@DZ@C�
@CS�@B��@B=q@BJ@A�7@A�@@��@@��@@A�@@ �@?�w@?��@?l�@?;d@>ȴ@>��@>��@>E�@>@=�-@=�@=�@=`B@=?}@=O�@=/@<�@<j@<9X@;�
@;o@:�@:�H@:��@:��@:�!@:�\@:M�@9�@9�^@9x�@9G�@9�@8Ĝ@8�u@8�u@8�@8bN@8bN@8A�@7�@7\)@7+@7
=@6�y@6ȴ@6��@6$�@5��@5`B@5�@4�j@4z�@41@3��@3C�@3o@2��@2��@2~�@2^5@2�@1x�@1X@17L@1�@0�`@0Ĝ@0r�@0Q�@0A�@01'@0b@0  @/�;@/��@/�@/|�@/�@.v�@.ff@.V@-�T@-`B@,�/@,�j@,�D@,I�@,�@+ƨ@+�@+@*��@*M�@*-@*�@)�^@)��@)hs@)G�@)%@(Ĝ@(��@(�@(Q�@(b@'�w@'�@'�P@'l�@'K�@&�@&��@&�+@&$�@%�@%��@%`B@%V@$��@$j@$Z@$(�@#�m@#�
@#�
@#�F@#��@#�@#t�@#S�@#33@"=q@!�#@!�7@!x�@!&�@ �u@ Q�@ 1'@  �@ b@   @   @�;@�w@|�@\)@K�@+@�@�@
=@��@E�@5?@5?@$�@�@��@�-@�h@�@��@�j@Z@��@�F@�@dZ@33@�@�\@~�@^5@��@��@x�@hs@&�@Ĝ@�@A�@1'@  @  @�@�;@�w@�P@�P@|�@\)@;d@�@
=@�y@�y@ȴ@ff@5?@@�T@��@��@�@`B@�@�@�/@�j@�D@j@I�@1@t�@��@n�@-@��@�#@��@x�@X@7L@&�@�@��@��@bN@�@�P@K�@+@�@�+@ff@��@�R@E�@5?@$�@$�@�T@�h@O�@�/@�@�D@z�@z�@I�@1@��@ƨ@��@�@C�@@
��@
�!@
~�@
^5@
=q@	��@	�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B\B��B�%BaHBYBK�BA�B:^B,B�BuB1B��B�sB�sB�ZB�5B�B��B�}B�-B��B�uB�Bv�Bl�B`BBYBVBO�BK�B<jB5?B,B)�B$�B�B%B��B�mB�B��BȴB�RB��B�{B�DB�+Bq�BYBQ�B;dB�BB
�B
�/B
��B
ĜB
��B
��B
�hB
� B
l�B
bNB
^5B
J�B
;dB
(�B
bB	��B	�B	��B	ŢB	�^B	��B	��B	��B	��B	�{B	�DB	u�B	`BB	S�B	H�B	A�B	:^B	/B	$�B	�B	{B	\B		7B	+B	B��B��B��B��B��B��B��B��B�B�B�B�B�B�mB�`B�`B�`B�;B�B��B��B��BǮBƨBÖBÖBB��B�wB�jB�^B�RB�FB�3B�'B�B�B��B��B��B��B��B��B��B��B�uB�VB�%B|�Br�BjBgmBdZBaHB^5B\)B[#B]/B_;B\)B[#BW
BVBT�BP�BN�BI�BG�BF�BE�BD�BC�BB�BA�B?}B<jB8RB49B/B,B,B,B+B(�B'�B'�B&�B&�B$�B �B�B�B�B�B�B�B�B �B �B�B�B �B%�B&�B%�B'�B(�B'�B&�B&�B&�B(�B(�B'�B%�B$�B%�B)�B(�B(�B'�B'�B(�B(�B'�B&�B#�B#�B$�B&�B,B/B/B/B2-B33B2-B33B6FB9XB<jB<jB=qB=qB>wB>wB@�B@�B@�B?}BC�BE�BE�BF�BH�BJ�BL�BN�BQ�BS�BVBXB]/Bu�Bx�Bx�Bw�Bw�Bu�B}�B�=B�PB�bB�\B�VB�bB��B��B��B��B��B��B�!B�^B�wB�wB��BƨB��B��B��B��B��B��B��B��B��B��B��BȴBƨBɺB��B��B�B�TB�NB�NB�TB�mB�B�yB�sB�B�B�B�B��B��B��B��B��B��B	B	B	1B	PB	�B	)�B	-B	/B	/B	/B	,B	+B	,B	,B	-B	.B	,B	,B	-B	7LB	>wB	>wB	@�B	I�B	I�B	G�B	K�B	K�B	I�B	B�B	?}B	C�B	M�B	Q�B	S�B	S�B	YB	\)B	aHB	iyB	l�B	m�B	p�B	s�B	v�B	{�B	}�B	� B	� B	� B	� B	�B	�%B	�%B	�%B	�%B	�B	�B	�B	�VB	�\B	�\B	�bB	�bB	�oB	�{B	�{B	�oB	�bB	�bB	�VB	�JB	�7B	�1B	�+B	�%B	�1B	�JB	�JB	�PB	�VB	�\B	�\B	�\B	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�3B	�RB	�RB	�FB	�3B	�B	�B	�?B	�9B	�'B	�-B	�?B	�RB	�RB	�FB	�XB	�^B	�dB	�dB	�dB	�^B	�XB	�jB	�jB	�qB	�qB	�qB	�wB	�wB	��B	ÖB	ŢB	ŢB	ƨB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�#B	�)B	�)B	�/B	�;B	�TB	�`B	�`B	�fB	�mB	�mB	�yB	�B	�B	�B	�B	�B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B	��B	��B	��B
B
B
B
B
B
B
+B
%B
%B
%B
%B
%B
B
%B
+B
	7B
DB
JB
JB
JB
PB
VB
\B
\B
\B
\B
bB
bB
bB
hB
hB
hB
hB
hB
hB
hB
oB
uB
uB
uB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
"�B
"�B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
$�B
$�B
$�B
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
%�B
%�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
%�B
&�B
&�B
'�B
'�B
(�B
)�B
)�B
)�B
)�B
)�B
)�B
,B
,B
-B
-B
.B
/B
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
/B
0!B
0!B
0!B
1'B
1'B
1'B
1'B
1'B
1'B
1'B
1'B
1'B
1'B
1'B
2-B
2-B
33B
49B
49B
5?B
5?B
6FB
6FB
6FB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
8RB
8RB
8RB
8RB
8RB
9XB
9XB
:^B
;dB
;dB
;dB
;dB
<jB
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
>wB
@�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
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
F�B
F�B
F�B
F�B
F�B
F�B
G�B
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
I�B
J�B
J�B
K�B
K�B
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
L�B
L�B
L�B
L�B
M�B
M�B
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
S�B
T�B
T�B
T�B
VB
W
B
W
B
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
ZB
ZB
ZB
[#B
[#B
[#B
[#B
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
_;B
_;B
^5B
_;B
_;B
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
dZB
dZB
dZB
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
e`B
e`B
e`B
ffB
ffB
ffB
gmB
gmB
gmB
hsB
jB
jB
iyB
iyB
iyB
jB
jB
jB
jB
jB
jB
jB
k�B
jB
jB
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
m�B
m�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�BB�B�B��B��BcoB^cBPBD	B>�B1RB�BIBeB�B�B�B�B��B�CBԥB��B��B��B��B�`Bz;Bp�Bb�BZ�BW�BQ�BP�B>�B7sB-B,�B(KB#�B	�B��B��B�TB�B͂B�jB�(B��B�B��Bv�B[�BV�BC�B �B	�B
��B
��B
��B
˫B
�VB
��B
��B
��B
oHB
c�B
d�B
N�B
B�B
/�B
�B
�B	��B	�/B	ȟB	��B	�=B	�(B	��B	��B	��B	�CB	},B	e-B	XrB	KYB	D
B	?#B	4�B	*B	 �B	WB	�B	"B		�B	4B	B��B�B�NB�&B��B�"B��B�[B�PB��B�8B�>B�B��B�B�B��B�B��B��B�B�B��B�/BĄB��B�B�JB��B��B�mB��B�B�"B�RB�6B�mB�B��B�|B�BB�yB��B�DB��B�mB��B��BwTBl}BhuBg�Bd:B`�B\�B\BB_'BaB]�B]�BX�BW�BXBR}BQ�BL�BJ1BH�BF�BE BDIBC�BB�BA�BCyB<	B9�B3{B-�B-B-B-B+aB)�B)PB'nB+ B(B$B BcB�B�B �B �B WB!KB!�B"B�B#B(B'�B)1B*�B)�B+�B)�B(�B(\B*�B*�B)�B(�B(nB(�B+B*�B+
B)QB)�B*�B*B*�B*"B%�B%�B'3B)�B.�B/�B0BB12B3rB3�B3�B5�B8�B;kB=]B=RB>.B>�B?�B?jBAhBA�BBEBB�BD�BFBFfBHIBI�BK�BM6BOZBS6BVBW�BXIBZ�Bv�By�By�By�ByPBu&B|B�B��B��B�RB�B��B�`B��B�+B�RB�;B��B�CB�B��B��B�B��B�DB΄B�2BͫB��BιB�BѳBѧBѢB��B�:B�XB�7B�jB�JB�HB�B��B�B�B��B�|B�|B�B�B�B�@B��B�PB��B��B�NB�hB�>B	�B	AB	�B	�B	4B	)�B	-B	/pB	/�B	0�B	-eB	,MB	,�B	,�B	.B	1 B	/RB	-[B	+�B	6YB	? B	>QB	?'B	K7B	K-B	G�B	LB	M(B	M�B	EYB	?2B	B�B	M�B	R\B	U]B	T�B	Y�B	[�B	`~B	i�B	l�B	m�B	p�B	s�B	w=B	|�B	~�B	�LB	�IB	�LB	��B	��B	��B	�qB	��B	�B	��B	�1B	�VB	��B	�B	��B	��B	��B	��B	��B	�/B	�'B	�B	��B	��B	��B	�*B	�B	��B	��B	��B	��B	�B	��B	��B	�B	�LB	��B	��B	��B	��B	�B	�VB	�"B	�1B	�B	��B	��B	�B	��B	�xB	��B	��B	��B	��B	��B	�PB	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�XB	��B	��B	��B	�<B	��B	��B	�B	��B	��B	�=B	�B	�%B	�B	ƯB	�WB	�RB	�kB	�UB	̄B	�bB	��B	��B	�kB	�?B	�?B	�*B	��B	�B	�B	�	B	��B	ߑB	��B	�B	�B	�B	��B	�B	��B	��B	��B	��B	�B	��B	�uB	�oB	�B	�HB	�EB	�B	�AB	�IB	�-B	�PB	�<B	�B	�0B	�&B	�B	�_B	�kB	�"B	�JB	�NB	�B	�B	�{B	��B	��B	��B	��B	�JB	��B	��B	��B	��B	�>B	��B
 �B	��B	��B	��B
lB
�B
�B
�B
�B
�B
@B
�B
�B
�B
�B
�B
�B
�B
B
	�B
�B
�B
�B
�B
�B
�B
�B
�B
 B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
�B
�B
�B
?B
!B
NB
B
 B
�B
�B
B
B
TB
B
JB
B
�B
�B
�B
 B
�B
B
=B
B
4B
yB
KB
�B
!6B
!,B
![B
"GB
"1B
#AB
#B
#4B
#/B
#�B
$oB
$;B
$WB
$KB
%PB
%gB
%YB
%8B
%TB
%sB
%fB
%(B
&:B
&=B
&UB
&bB
&WB
&�B
&�B
%�B
%�B
%iB
%yB
%TB
%sB
%�B
%�B
&iB
'NB
'PB
(KB
(�B
)hB
*VB
*`B
*UB
*VB
*{B
*�B
,�B
,�B
-�B
-2B
.�B
/�B
/B
.�B
.�B
.�B
.�B
.yB
/�B
/�B
/lB
/�B
0B
/�B
/uB
/�B
/tB
/�B
/�B
0�B
0�B
0�B
1�B
1�B
1�B
1�B
1�B
1�B
1�B
1tB
1�B
1�B
1�B
2�B
2�B
3�B
4�B
4�B
5�B
5�B
6�B
6�B
6�B
7�B
7�B
7�B
7�B
7�B
7�B
7�B
7�B
8�B
8�B
8�B
8�B
8�B
9�B
9�B
:�B
<B
;�B
;�B
;�B
=3B
=�B
=�B
=�B
=�B
=�B
=�B
>�B
?B
>�B
>�B
>�B
>�B
>�B
@�B
A�B
A�B
A�B
A�B
A�B
BB
B=B
A�B
A�B
A�B
A�B
A�B
B'B
BB
B+B
BB
BB
BB
B-B
C4B
CB
B�B
CB
C�B
C�B
C�B
DB
D]B
D�B
D�B
D�B
EB
F	B
F+B
FB
E�B
E�B
FB
G B
GB
G B
GB
GB
GBB
HqB
H	B
H
B
HOB
H[B
IcB
IB
I'B
I/B
J(B
JCB
J5B
JhB
KUB
KIB
L,B
LB
LZB
L+B
L8B
L-B
LDB
LBB
M4B
M6B
M;B
MLB
MUB
M(B
M4B
M/B
M4B
NyB
NPB
O4B
OnB
OKB
PGB
PB
PgB
P]B
PvB
QBB
QVB
QeB
RBB
R:B
RNB
SKB
SWB
SLB
SVB
SaB
T B
T�B
T�B
UXB
U�B
U�B
V�B
WpB
WdB
WdB
WaB
WVB
WnB
XtB
X�B
XqB
YlB
YrB
YnB
YaB
YpB
Y�B
Y�B
ZqB
ZlB
ZqB
Z�B
Z|B
Z}B
Z�B
Z�B
[�B
[yB
[�B
[�B
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
_�B
_�B
^�B
_�B
_�B
_�B
_�B
_�B
_�B
`�B
`�B
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
c�B
c�B
c�B
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
eB
e&B
d�B
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
f	B
fB
e�B
f�B
f�B
f�B
g�B
g�B
g�B
h�B
k$B
j�B
i�B
i�B
i�B
kB
kB
k&B
j�B
j�B
j�B
j�B
k�B
kB
j�B
k�B
k�B
k�B
l	B
m	B
l�B
l�B
nB
m�B
m�B
n B
m�B
m�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<,e<���<>��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - (REPORTED_SURFACE_PRESSURE) for PADJ                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al,2007,JAOT & effects of pressure adjustments                                                                                                                                                            PADJ REPORTED_SURFACE_PRESSURE =0.58 dbar                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CTM alpha = 0.141 & tau = 6.68 s with error equal to the correction                                                                                                                                                                                             Pressures adjusted using reported surface pressure when warranted.                                                                                                                                                                                              none                                                                                                                                                                                                                                                            Salinity corrected for pressure sensor calibration drift and cell thermal mass                                                                                                                                                                                  201603101135312016031011353120160310113531  AO  ARCAADJP                                                                    20141126230157    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20141126230157  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20141126230157  QCF$                G�O�G�O�G�O�0                                                                                                                                   G�O�G�O�G�O�                PM  ARSQPADJV1.1                                                                20160310113531  QC  PRES            @���D�C3G�O�                PM  ARSQCTM V1.1                                                                20160310113531  QC  PSAL            @���D�C3G�O�                PM  ARSQOWGUV1.0WOD + Argo                                                      20170523133351  IP                  G�O�G�O�G�O�                